outlb <- Microsoft365R::get_business_outlook()

{
  for(cycle in basename(fs::dir_ls(fs::path(paths$site_drafts_completed), type = "directory", recurse = FALSE))) {
    response_groups <-
      list.dirs(here::here("..", "Data", cycle),
                full.names = FALSE, recursive = FALSE) |>
      stringr::str_subset(pattern = "Barnehageleder|Rektor|Lærer|Yrkesfaglærer|Modul|Barnehagelærer")
    response_groups <- response_groups[!response_groups %in% "Lærer og yrkesfaglærer"]

    for(response_group in response_groups) {
      if(cycle %in% names(sent_to) && response_group %in% sent_to[[cycle]]) next
      params$cycle <- cycle
      params$response_group <- response_group
      source(here::here(paths$r, "003_get_report_cycle_paths.R"))
      cli::cli_inform("Processing {cycle}-{response_group}")

      eposter <-
        saros.base::create_email_credentials(local_basepath = paths$drafts_completed,
                                        rel_path_base_to_parent_of_user_restricted_folder =
                                          fs::path("Rapporter", params$cycle, params$response_group, "Mesos"),
                                        email_data_frame =
                                          readxl::read_excel(paths$mesos_contacts[[params$cycle]][[params$response_group]],
                                                             sheet = params$response_group) |>
                                          dplyr::mutate(dplyr::across(tidyselect::everything(), ~stringr::str_replace_na(.x, ""))),
                                        email_col= "email",
                                        username_col = "username",
                                        ignore_missing_emails = TRUE,
                                        local_main_password_path = paths$local_main_password_path,
                                        email_body = email_body,
                                        email_subject = email_subject)


      if(nrow(eposter)>0) {
        for(i in seq_len(nrow(eposter))) {
          if(isTRUE(send_emails)) {
            ny_epost <-
              outlb$get_drafts()$create_email(body = eposter[i, "body"][[1]],
                                              subject = eposter[i, "subject"][[1]],
                                              to = eposter[i, "to"][[1]])$
              send() # Sett på dollartegnet på linjen ovenfor og send() her så sendes de. Ellers bare utkast
            cat(paste0("Eposter med passord er utsendt til ", params$cycle, "/", params$response_group, " den ", Sys.time()),
                file = here::here("_logs", "_email_credentials_log.txt"), append = TRUE)
          } else {
            ny_epost <-
              outlb$get_drafts()$create_email(body = eposter[i, "body"][[1]],
                                              subject = eposter[i, "subject"][[1]],
                                              to = eposter[i, "to"][[1]])
          }
        }
      } else cli::cli_warn("Ingen eposter genereres for {params$cycle}/{params$response_group}")

      if(isTRUE(send_emails)) {
        if(any(duplicated(c(sent_to[[cycle]], response_group)))) cli::cli_abort("Duplicates!")
        sent_to[[cycle]] <- c(sent_to[[cycle]], response_group)
      }
    }
  }
  if(isTRUE(send_emails)) {
    cli::cli_inform(c("Update your syntax to avoid mishaps in the future:",
                    "sent_to <- "
                    ))
    dput(sent_to)
  }
}


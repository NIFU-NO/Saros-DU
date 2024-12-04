cycles_needing_authentication_setup |>
rlang::set_names() |>
  purrr::map(.f = function(cycle) {

    if(length(survey_data[[cycle]])==0) cli::cli_abort("{cycle} has no data. This part of the scripts currently assumes you have ran the prep_data_*.R script for the cycle you need.")

    survey_data[[cycle]] |>
      names() |>
      rlang::set_names() |>
      purrr::map(.f = function(response_group) {

        if(is.null(survey_data[[cycle]][[response_group]])) {

          cli::cli_warn("Nothing found for {cycle} and {response_group}");
          NULL
        } else {

          if(suppressWarnings(!is.null(survey_data[[cycle]][[response_group]]$mod))) {
            survey_data[[cycle]][[response_group]] |>
              dplyr::distinct(username = .data[[params$mesos_var]], .data[["mod"]]) |>
              dplyr::filter(!is.na(.data$username)) |>
              dplyr::mutate(folder = saros.base:::filename_sanitizer(.data$username, max_chars = 12, make_unique = FALSE),
                            username = stringr::str_replace(.data$username, pattern = "(.+): .+$", replacement="\\1"),
                            parent_folder_path_rel = fs::path(.env$cycle, paste0("Modul ", mod), .env$params$mesos_folder_name),
                            mod = NULL)
          } else {
            survey_data[[cycle]][[response_group]] |>
              dplyr::distinct(username = .data[[params$mesos_var]]) |>
              dplyr::filter(!is.na(.data$username)) |>
              dplyr::mutate(folder = saros.base:::filename_sanitizer(.data$username, max_chars = 12, make_unique = FALSE),
                            username = stringr::str_replace(.data$username, pattern = "(.+): .+$", replacement="\\1"),
                            parent_folder_path_rel = fs::path(.env$cycle, .env$response_group, .env$params$mesos_folder_name))

          }
        }

      }) |>
      dplyr::bind_rows(.id = "response_group")
  }) |>
  dplyr::bind_rows(.id = "cycle") |>
  writexl::write_xlsx(fs::path(paths$saros, "_username_folder_matching_df.xlsx"))


username_folder_matching_df <-
  readxl::read_excel(fs::path(paths$saros, "_username_folder_matching_df.xlsx"))

if(!rlang::is_string(paths$remote_basepath)) cli::cli_abort("{paths$remote_basepath} is empty!")
if(!rlang::is_string(paths$local_basepath)) cli::cli_abort("{paths$local_basepath} is empty!")
if(!rlang::is_string(paths$site_drafts_completed)) cli::cli_abort("{paths$site_drafts_completed} is empty!")

saros.base::setup_access_restrictions(
  remote_basepath = paths$remote_basepath,
  local_basepath = paths$local_basepath,
  rel_path_base_to_parent_of_user_restricted_folder =
    list.dirs(paths$site_drafts_completed, full.names=FALSE, recursive=FALSE) |>
    stringr::str_subset(pattern = "^_", negate = TRUE) |>
    lapply(function(cycle) {
      sub_paths <-
        list.dirs(path = fs::path(paths$site_drafts_completed, cycle), full.names = FALSE, recursive = FALSE)
      sub_paths <-
        stringr::str_subset(sub_paths, pattern = "/_", negate = TRUE)
      fs::path("Rapporter", cycle, sub_paths, params$mesos_folder_name)
    }) |>
    unlist(),
  local_main_password_path = paths$local_main_password_path,
  username_folder_matching_df = username_folder_matching_df,
  universal_usernames = c("admin", "nifu", "udir"))

cli::cli_warn(c(i="Sjekk master-passordfilen vår.",
                i="Dersom det nå er lagt til nye brukernavn, er det riktig?",
                i="Eller eksisterte det allerede en institusjon (med litt annen skrivemåte) som nå har fått enda et passord å håndtere?",
                i="Vi ønsker kun en institusjon, ett passord."))
rstudioapi::documentOpen(paths$local_main_password_path)

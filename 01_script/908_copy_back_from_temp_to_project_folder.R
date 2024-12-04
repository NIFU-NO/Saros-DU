

# If an existing _site exists, make a backup of it and replace it with the new version. If backup on same date already exists, do it manually
cli::cli_h2("Rename the existing folder {.path {(fs::path(paths$saros, '_site'))}}")
cli::cli_h2("to {.path {(fs::path(paths$saros, paste0('_site', '_old_', Sys.Date())))}}?")
cli::cli_h2("Copy {.path {paths$site_drafts_completed}}")
cli::cli_h2("to {.path {(fs::path(paths$site, '_site'))}}?")
choice_move_site <- utils::menu(c("Yes", "No"))
if(choice_move_site == 1 && fs::dir_exists(fs::path(paths$saros, "_site")) && !fs::dir_exists(fs::path(paths$saros, paste0("_site", "_old_", Sys.Date())))) {
  fs::dir_copy(path = fs::path(paths$saros, "_site"),
               new_path = fs::path(paths$saros, paste0("_site", "_old_", Sys.Date())),
               overwrite = FALSE)
  fs::dir_delete(fs::path(paths$saros, "_site"))
}

fs::dir_copy(path = paths$local_basepath,
             new_path = fs::path(paths$saros, "_site"))

# Same with the qmd-files, move them back if these are newer
cli::cli_h2("Copy ({.path {paths$drafts_produced}}) to your project folder ({.path {paths$drafts_completed}})")
cli::cli_h2("(and overwrite if necessary)?")
choice_copy_drafts <- utils::menu(choices = c("Yes", "No"))
if(choice_copy_drafts == 1) {
  fs::dir_delete(path = paths$drafts_completed)
  saros.base::copy_folder_contents_to_dir(from = paths$drafts_produced, to = paths$drafts_completed, overwrite = TRUE)
  cli::cli_h1("Do you wish to remove all *.png and *.xlsx files from ({paths$drafts_completed})? These can hamper your cloud service and are usually unnecessary since they will be recreated upon re-rendering of the site.")
  choice_rm_png_xlsx <- utils::menu(choices = c("Yes", "No"))
  if(choice_rm_png_xlsx == 1) {
    fs::dir_ls(paths$drafts_completed, recurse = TRUE, type = "file", regexp = "\\.png$|\\.xlsx$") |>
      fs::file_delete()
  }
}

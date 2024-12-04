####################################################################
## Post-PISVEEP (rendering of website and report PDF)
####################################################################


### Render with Quarto
withr::with_envvar(new = c(LC_ALL="C"),
                   action = "replace",
                     quarto::quarto_render(
                       input = if(is.null(paths$site)) cli::cli_abort("paths$site is NULL") else paths$site,
                       as_job = TRUE,
                       use_freezer = TRUE,
                       output_format = c("all")
                     ))

# Take a temporary backup in case post-processing fails
fs::dir_create(paste0(paths$local_basepath, "_orig_", Sys.Date()))
saros.base::copy_folder_contents_to_dir(from = paths$local_basepath,
                                        to = paste0(paths$local_basepath, "_orig_", Sys.Date()),
                                        overwrite = TRUE)

## Post-processing: replace sidebar links to docx/pdf-files with html-version
saros.base::remove_entry_from_sidebar(
  path = paths$local_basepath,
  filename_as_regex = c("report\\.pdf", "report\\.docx"))


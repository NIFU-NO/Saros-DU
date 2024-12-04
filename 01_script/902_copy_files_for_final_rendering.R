##################################################################################
#####    Creating/copying/moving supporting files needed for rendering     #######
##################################################################################

# Using a local "temporary" scratch-folder instead of cloud services because there will be
# many files generated and the cloud services tend to create mysterious errors



### Copy files needed for rendering project, from main project folder to scratch folder
new_project_config_files <-
  fs::path(paths$site, c("_quarto.yaml",
                            "_global.yaml",
                         "styles.css",
                         "styles.scss",
                         "bib_style.csl",
                         "general_formatting.R"))
fs::file_copy(path =
                c(file.path(paths$resources, "YAML", c("_quarto.yaml",
                                                       "_global.yaml")),
                  file.path(paths$resources, "CSS", c("styles.css",
                                                      "styles.scss")),
                  file.path(paths$resources, "CitationStyles", "bib_style.csl"),
                  file.path(paths$resources, "Rscripts", "general_formatting.R")
                ),
              new_path = new_project_config_files,
              overwrite = TRUE)

### Should set all of these as hidden to avoid the temptation of modifying these copies rather than the originals
if(Sys.info()[["sysname"]] == "Windows") {
  for(file in new_project_config_files) {
    system(paste("attrib +h", shQuote(file)))
  }
}

### Install extensions in scratch folder
withr::with_dir(new = paths$site,
                code = {
                  quarto::quarto_add_extension(extension = "NIFU-NO/nifutypst", no_prompt = TRUE, quiet = F)
                  quarto::quarto_add_extension(extension = "NIFU-NO/nifudocx", no_prompt = TRUE, quiet = F)
                  quarto::quarto_add_extension(extension = "NIFU-NO/rename_duplicate_labels", no_prompt = TRUE, quiet = F)
                  quarto::quarto_add_extension(extension = "NIFU-NO/remove_empty_headings", no_prompt = TRUE, quiet = F)
                })

### Copy index.qmd to key folders
# Should make these more informative?
folders_needing_index_qmd <-
  c(fs::path(paths$site_drafts_completed),
    fs::path(paths$site_drafts_completed) |>
      fs::dir_ls(recurse = FALSE, type = "directory")
  )
# folders_needing_index_qmd |>
#   purrr::walk(~{
#     index_qmd <-
#     file.path(paths$resources, "_QMD", "index.qmd") |>
#       readLines(warn = FALSE)
#     if(basename(.x) != "Rapporter") {
#       index_qmd <-
#         index_qmd |>
#         stringr::str_replace(pattern = 'title: \"Deltakerundersøkelsene\"',
#                              replacement = paste0('title: ', basename(.x)))
#     }
#     index_qmd
#     writeLines(index_qmd, con = fs::path(.x, "index.qmd"))
#   })


fs::dir_copy(path = file.path(paths$resources, "_images"),
             new_path = file.path(paths$site, "_images"),
             overwrite = TRUE)


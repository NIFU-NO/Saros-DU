here::i_am("01_script/000_initialize_project.R")

remotes::install_github(c("NIFU-NO/saros", "NIFU-NO/saros.base", "NIFU-NO/saros.utils"), quiet = TRUE,
                        upgrade = "always", force = FALSE)


params <- list()
params$mesos_folder_name <- "Tilbydere"
params$mesos_var <- "campus2"
params$reports <- "Rapporter"

paths <- list()
paths$data <- list()
paths$data$population <- list()
paths$data$sample <- list()
paths$data$survey <- list()

paths$r <-
  fs::path("01_script")
paths$resources <-
  here::here("02_resources")
paths$organization_global_draft_report_settings <-
  fs::path(Sys.getenv("USERPROFILE"), "NIFU", "Metode - General", "SAROS-core", "shared resources", "_draft_report_settings.yaml")
paths$organization_global_refine_chapter_overview_settings <-
  fs::path(Sys.getenv("USERPROFILE"), "NIFU", "Metode - General", "SAROS-core", "shared resources", "_refine_chapter_overview_settings.yaml")

paths$drafts_produced <-
  fs::path(Sys.getenv("USERPROFILE"), "Saros", "21149_DU", params$reports)
paths$drafts_completed <-
  fs::path(".", params$reports)


paths$site <- # Should be renamed "local_scratch"
  # "."
  fs::path(Sys.getenv("USERPROFILE"), "Saros", "21149_DU")
paths$site_drafts_completed <- # renamed "local_scratch_reports"
  fs::path(paths$site, params$reports)
paths$site_resources <-
  fs::path(paths$site, "_extensions")
paths$local_basepath <- fs::path(paths$site, "_site")

paths$remote_basepath <- "/home/nifuno/domains/stephan/du.nifu.no/public_html/"

paths$topImagePath <- here::here("02_resources", "PNG", "cover_ovre.png")

paths$saros <-
  here::here()

paths$local_main_password_path <-
  fs::path("..", "..", "Metode - Sensitivt - Sensitivt", ".main_htpasswd_private")


# paths$map$fylker <- fs::path("..", "..", "Metode - General", "SAROS-core", "shared resources", "maps", "fylker2021.json")
# paths$map$kommuner <- fs::path("..", "..", "Metode - General", "SAROS-core", "shared resources", "maps", "kommuner2021.json")



df_labels <- list()
survey_data <- list()
chapter_overview <- list()
chapter_structure <- list()
chunk_templates <- list()
config_macro <- list()
config_mesos <- list()
output_files <- list()
check_labels <- list()

# copy_folder_contents_to_dir(from = system.file("templates", package = "nifutheme"),
#                             to = paths$resources, overwrite = TRUE)

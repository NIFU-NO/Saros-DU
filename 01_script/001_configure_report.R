params$cycle <- "2024V"
params$response_group <- "Modul"

# Kan velge å hoppe over noen målgrupper, men husk å sette tilbake ()
skip_response_groups <-
  vctrs::vec_c(
    #"Barnehageleder",
    #"Rektor",
    # "Barnehagelærer",
    # "Lærer",
    # "Yrkesfaglærer",
    # "Modul",
  )

fs::dir_create("_logs")

for(cycle in vctrs::vec_c(
  # "2022H",
  # "2023V",
  # "2023H",
  "2024V",
  )) {
  response_groups <-
    list.dirs(here::here("..", "Data", cycle),
              full.names = FALSE, recursive = FALSE)
  if(length(response_groups)==0) cli::cli_abort("Nothing found for {cycle}. Check your working directory.")
  response_groups <-
    grep(x = response_groups, pattern = "Modulen|R-skript|test", value = TRUE, invert = TRUE)

  if("Lærer og yrkesfaglærer" %in% response_groups) {

    response_groups <- response_groups[!response_groups %in% "Lærer og yrkesfaglærer"]
    response_groups <- c(response_groups, "Lærer", "Yrkesfaglærer")

  }

  if(length(skip_response_groups)) cli::cli_warn("Skipping response groups for {params$cycle} as per your request: {skip_response_groups}.")

  for(response_group in response_groups) {
    if(response_group %in% skip_response_groups) next
    cli::cli("{cycle}-{response_group}")
    params$cycle <- cycle
    params$response_group <- response_group

    sink(file = fs::path("_logs", paste0("draft_report_", cycle, "_", response_group, "_", stringr::str_replace_all(as.character(Sys.time()), "[: ]", "-"), ".txt")), append = TRUE)

    source(here::here(paths$r, "003_get_report_cycle_paths.R"))
    source(here::here(paths$r, paste0("200_prep_data_for_", params$cycle, "_", params$response_group, ".R")))
    source(here::here(paths$r, "900_draft_reports.R"))
    sink()
  }
}

source(here::here(paths$r, "902_copy_files_for_final_rendering.R"))

##################################################################
### Create _username_folder_matching_df.xlsx #####
# For each institution, generate expected folder names and usernames, and store in _username_folder_matching_df.xlsx
# To be done for each time passwords need to be generated (which is done for all available folders)
# Must contain all years if password authentication files are being created anew
##################################################################
cycles_needing_authentication_setup <-
vctrs::vec_c(
  "2022H",
  "2023V",
  "2023H",
  "2024V",
  )
source(here::here(paths$r, "902_create_username_folder_matching_df.R"))




source(here::here(paths$r, "905_render.R"))
source(here::here(paths$r, "906_setup_access_restrictions.R")) # Must be run after each render
# Upload manually with FileZilla to your paths$remote_basepath server location's public_html
# Test with user login credentials for at least two distinct usernames (not only admin/nifu)


##################################################################################################
## Copy files back from temporary local folder paths$site to project folder                     ##
## Try doing it in the evening so it doesn't lag your project members' OneDrive synchronization ##
## You will be prompted on major changes                                                        ##
##################################################################################################
source(fs::path(paths$r, "908_copy_back_from_temp_to_project_folder.R"))


## Lage eposter, og eventuelt sende dem ut.
## Sett følgende til TRUE for å faktisk sende ut.
## Bør sjekke at de ikke alt har sendt ut tidligere.
## Alle som har fått tidligere legges i sent_to-listen
send_emails <- FALSE
sent_to <- list(`2023V` = c("Lærer", "Yrkesfaglærer", "Barnehagelærer",
                            "Modul"),
                `2022H` = c("Barnehageleder", "Rektor"),
                `2024V` = "Modul")
email_subject <-
  "Innloggingsdetaljer for {username} til Deltakerundersøkelsenes tilbyder-rapporteringer"
email_body <-
  "
Hei {Fornavn}!


Resultater fra Deltakerundersøkelsene for deltakerne som tok videreutdanning ved din institusjon er nå oppdatert på https://du.nifu.no/. Her finnes både overordnede resultater, samt tilbyderspesifikke rapporter under Tilbyderrapporter i menyen på venstre side. Du får tilsendt passordet fordi Udir har oppført deg som kontaktperson. Rapportene er sendt hovedkontaktene ved universitetene og høyskolene, vennligst videresend til aktuelle mottakere i egen organisasjon. Følgende brukernavn og passord er felles for alle ved {username} og du som kontaktperson er ansvarlig for behandling av tilgang internt.

  Brukernavn:   {username}

  Passord:      {password}

NIFU gjennomfører undersøkelsen på oppdrag for Udir.

Begrensninger:

Dersom det er uoverensstemmelser for tall mellom disse sidene og publiserte rapporter på https://www.nifu.no er det sistnevnte som er de offisielle. Vi har dessverre ikke anledning til å tilby ytterligere analyser for din institusjon. Resultatene er kun for de som har svart. Fjorårets resultater legges ut (igjen) i løpet av dagen.

Vennlig hilsen prosjektgruppen i Deltakerundersøkelsene ved

Cathrine Pedersen

Prosjektleder

NIFU Nordisk institutt for studier av innovasjon, forskning og utdanning
"
source(here::here(paths$r, "907_email_credentials.R"))




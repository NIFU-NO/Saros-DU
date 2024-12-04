# Kapitteloversikten i Excel. Kan oftest klargjøres så snart skjemaet er i Qualtrics.
# Må minimum inneholde Kapittelforfatter, Tema, dep, indep og
# avkrysning for hvilke resp som temaet angår (se kap 2):
# Grunnskole	Videregående	Kommune	Fylkeskommune
# Kan også inneholde andre kolonner. Husk at rader for innledning og Beskrivelse av utvalget må med for riktig nummerering.
paths$chapter_overview[[params$cycle]][[params$response_group]] <-
  here::here("..", "Data", params$cycle, params$response_group, "kapitteloversikt",
             paste0(params$cycle, "_", params$response_group, "_struktur_rapporter_studiested.xlsx"))

# Ikke nødvendig lenger. Ble brukt tidligere når dataene var vasket i Stata. Stata kutter lablene om måtte settes på igjen i R basert på de opprinnelige etikettene.
paths$df_labels[[params$cycle]][[params$response_group]] <-
  here::here("..", "Data", params$cycle, params$response_group, "data fra sx", "csv", "variables.csv")

# Her er surveydataene påkoblet registerdata og de som ikke har svart er fjernet. Brukes til de fleste kapitlene (bortsett fra kap 2).
paths$data$survey[[params$cycle]][[params$response_group]] <-
  here::here("..", "Data", params$cycle, params$response_group, "ferdige filer",
           paste0(params$cycle, "_", params$response_group, "_final.dta"))

# Filbanen hvor 200_prep_data legger ferdigbehandlet datasett klar for saros.
paths$data$saros_ready[[params$cycle]][[params$response_group]] <-
  here::here("..", "Data", params$cycle, params$response_group, "Sarosdata",
             paste0(params$cycle, "_", params$response_group, "_saros_main_chapters.dta"))
fs::dir_create(fs::path_dir(paths$data$saros_ready[[params$cycle]][[params$response_group]]))

# Udir må sende oss kontaktinformasjon til de som skal motta brukernavn og passord
# Kolonnene i Excelfila er:
#
paths$mesos_contacts[[params$cycle]][[params$response_group]] <-
  here::here("..", "Data", params$cycle, "kontaktinfo_tilbydere.xlsx")

if(params$response_group %in% c("Lærer", "Yrkesfaglærer")) {

  paths$chapter_overview[[params$cycle]][[params$response_group]] <-
    here::here("..", "Data", params$cycle, "Lærer og yrkesfaglærer", "kapitteloversikt",
               paste0(params$cycle, "_", "Lærer og yrkesfaglærer", "_struktur_rapporter_studiested.xlsx"))

  paths$df_labels[[params$cycle]][[params$response_group]] <-
    here::here("..", "Data", params$cycle, "Lærer og yrkesfaglærer", "data fra sx", "csv", "variables.csv")

  paths$data$survey[[params$cycle]][[params$response_group]] <-
    here::here("..", "Data", params$cycle, "Lærer og yrkesfaglærer", "ferdige filer",
               paste0(params$cycle, "_", params$response_group, "_final.dta"))

  paths$data$saros_ready[[params$cycle]][[params$response_group]] <-
    here::here("..", "Data", params$cycle, "Lærer og yrkesfaglærer", "Sarosdata",
               paste0(params$cycle, "_", "Lærer og yrkesfaglærer", "_saros_main_chapters.dta"))
  fs::dir_create(fs::path_dir(paths$data$saros_ready[[params$cycle]][[params$response_group]]))

}


# Mappen hvor kapittel-utkast skal genereres. Det blir lagd undermapper for ikke-fylke og fylke.
paths$output[[params$cycle]] <-
  fs::path("Rapporter", params$cycle, "Output")

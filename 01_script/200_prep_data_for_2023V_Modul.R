
library(dplyr, warn.conflicts = FALSE, quietly = TRUE)
conflicted::conflicts_prefer(dplyr::filter, dplyr::lag, .quiet=TRUE)

df_labels[[params$cycle]][[params$response_group]] <-
  read.csv(paths$df_labels[[params$cycle]][[params$response_group]], fileEncoding = "UTF-16", sep = "\t")

survey_data[[params$cycle]][[params$response_group]] <-
  haven::read_dta(paths$data$survey[[params$cycle]][[params$response_group]]) %>%
  labelled::unlabelled()

survey_data[[params$cycle]][[params$response_group]] <-
  survey_data[[params$cycle]][[params$response_group]] %>%
  dplyr::mutate(across(where(~is.factor(.x)), ~forcats::fct_relabel(.x, function(x) stringr::str_remove_all(x, ", spesifiser.*|, vennligst .*"))),
         s_86 = forcats::fct_na_level_to_value(s_86, "Ikke-binær/annet"),
         campus2 = forcats::fct_drop(inst, "Tok bare 1 modul"),
         campus2 = case_when(stringr::str_detect(campus2, "Nord") ~ "Nord",
                             .default = campus2),
         # mod = case_when(mod == "Digitalisering og ledelse" ~ "Digi. og ledelse",
         #                 mod == "Ledelse av utviklings- og endringsarbeid" ~ "Ledelse av utv. og endringsarb.",
         #                 mod == "Ledelse av lærings- og læreplanarbeid" ~ "Led. av lærings og læreplanarb.",
         #                 .default = mod),
         # campus2 = forcats::fct_cross(campus2, mod, sep = "- ", keep_empty = FALSE),
         across(where(~is.factor(.x)), ~forcats::fct_relabel(.x, function(x) na_if(x, ""))),
         across(where(~is.character(.x)), ~na_if(.x, "")),
         across(where(~is.character(.x)), ~ifelse(s_2=="Nei, de kan ikke oversendes mitt studiested", NA_character_, .x)),
         across(matches("s_69_"), ~factor(.x, levels= c("1 (Helt uenig)", "2", "3", "4", "5", "6 (Helt enig)")))#,
         # across(matches("s_14_|s_25$|s_19_|s_18$|s_19$|s_20$|s_48_|s_56_27|s_36_|s_40_|s_46_|s_56_|s_69_|s_84$|s_81_|s_21$"), ~ordered(.x))
  ) %>%
  labelled::copy_labels_from(survey_data[[params$cycle]][[params$response_group]], .strict = FALSE) %>%
  saros.utils::replace_stata_labels(df_new_labels = df_labels[[params$cycle]][[params$response_group]],
                              var_name_col = "variableName", var_label_col = "variableDescription") %>%
  labelled::set_variable_labels(s_1 = "Hva er din situasjon nå?",
                                campus2 = "Campus") %>%
  labelled::update_variable_labels_with(.cols = where(~is.factor(.x)),
                                        .fn = ~stringr::str_replace(.x, pattern = ", vennligst spesifiser$|, spesifiser$", ""))

for(v in colnames(survey_data[[params$cycle]][[params$response_group]])) {
  labelled::var_label(survey_data[[params$cycle]][[params$response_group]][[v]]) <-
    stringr::str_replace(string = labelled::var_label(survey_data[[params$cycle]][[params$response_group]][[v]]),
                         pattern = "Lederutdanningen har gitt meg kompetanse til å: - ",
                         replacement = "Lederutdanningen har gitt meg kompetanse til å: ")
}
if(dplyr::n_distinct(survey_data[[params$cycle]][[params$response_group]]$svarprosent, na.rm = TRUE)==2) {
  cli::cli_abort("Datasettet for {params$cycle}, {params$response_group} bør inneholde også respondenter som ikke har svart, for at svarprosent-figuren skal være informativ. Disse respondentene må være NA på alle andre variabler.")
}
qs::qsave(x = survey_data[[params$cycle]][[params$response_group]],
          file = here::here(paths$data$saros_ready[[params$cycle]][[params$response_group]]))


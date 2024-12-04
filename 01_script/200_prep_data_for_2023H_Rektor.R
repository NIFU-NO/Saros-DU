
library(dplyr, warn.conflicts = FALSE, verbose = FALSE)
conflicted::conflicts_prefer(dplyr::filter, dplyr::lag, .quiet=TRUE)


survey_data[[params$cycle]][[params$response_group]] <-
  paths$data$survey[[params$cycle]][[params$response_group]] %>%
  stringr::str_replace(pattern = "\\.dta", replacement=".rds") %>%
  readRDS() %>%
  labelled::unlabelled() %>%
  mutate(across(where(~inherits(.x, "labelled")), ~labelled::to_character(.x)))

survey_data[[params$cycle]][[params$response_group]] <-
  survey_data[[params$cycle]][[params$response_group]] %>%
  mutate(across(where(~is.factor(.x)), ~forcats::fct_relabel(.x, function(x) stringr::str_remove_all(x, ", spesifiser.*|, vennligst .*"))),
         s_43 = forcats::fct_na_level_to_value(s_43, "Ikke binær/annet"),
         across(where(~is.factor(.x)), ~forcats::fct_relabel(.x, function(x) na_if(x, ""))),
         across(where(~is.character(.x)), ~na_if(.x, "")),
         s_7_cat = ggplot2::cut_interval(s_44, length = 10),
         svarprosent = factor("Ja"),
         campus = Institusjon,
         campus2 = dplyr::case_when(campus == "Dronning Mauds Minne Høgskole" ~ "DMMH",
                                    campus == "Handelshøyskolen BI" ~ "BI",
                                    campus == "Norges handelshøyskole og AFF" ~ "NHH og AFF",
                                    campus == "OsloMet-Storbyuniversitetet" ~ "OsloMet",
                                    campus == "UiT Norges arktiske universitet" ~ "UiT",
                                    campus == "Universitetet i Agder" ~ "UiA",
                                    campus == "Universitetet i Sørøst-Norge" ~ "USN",
                                    campus == "Nord universitet" ~ "Nord",
                                    campus == "" ~ NA_character_,
                                    .default = stringr::str_replace_all(campus, "[[:space:]]", "_")
         ),
         s_7_cat = ggplot2::cut_width(s_44, 3)#,
         # across(matches("s_8_|s_17|s_24_3|s_26_|s_32_|s_56_|s_37_|s_39_|s_7_cat", ~ordered(.x)))
  ) %>%
  labelled::set_variable_labels(svarprosent = "Svarprosent",
                                s_7_cat = "Aldersgrupper, inndelt",
                                campus2 = "Campus") %>%
  labelled::update_variable_labels_with(.cols = where(~is.factor(.x)),
                                        .fn = ~stringr::str_replace(.x, pattern = ", vennligst spesifiser$|, spesifiser$", ""))

qs::qsave(x = survey_data[[params$cycle]][[params$response_group]],
          file = here::here(paths$data$saros_ready[[params$cycle]][[params$response_group]]))

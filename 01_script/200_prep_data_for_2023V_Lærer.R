
library(dplyr, warn.conflicts = FALSE, quietly = TRUE)
conflicted::conflicts_prefer(dplyr::filter, dplyr::lag, .quiet=TRUE)

df_labels[[params$cycle]][[params$response_group]] <-
  read.csv(paths$df_labels[[params$cycle]][[params$response_group]], fileEncoding = "UTF-16", sep = "\t")

survey_data[[params$cycle]][[params$response_group]] <-
  haven::read_dta(paths$data$survey[[params$cycle]][[params$response_group]]) %>%
  labelled::unlabelled()

survey_data[[params$cycle]][[params$response_group]] <-
  survey_data[[params$cycle]][[params$response_group]] %>%
  mutate(across(where(~is.factor(.x)), ~forcats::fct_relabel(.x, function(x) stringr::str_remove_all(x, ", spesifiser.*|, vennligst .*"))),
         across(where(~is.factor(.x)), ~forcats::fct_relabel(.x, function(x) na_if(x, ""))),
         across(where(~is.character(.x)), ~na_if(.x, "")),
         s_100 = forcats::fct_na_level_to_value(s_100, extra_levels = "Annet/ikke binær"),
         campus2 = case_when(studiested == "" ~ NA_character_,
                             studiested == "Høgskolen i Innlandet" ~ "HINN",
                             studiested == "Høgskolen i Østfold" ~ "HiOF",
                             studiested == "Høgskulen på Vestlandet" ~ "HVL",
                             studiested == "Høgskulen i Volda" ~ "HiVolda",
                             studiested == "NLA Høgskolen" ~ "NLA",
                             studiested == "Norges miljø- og biovitenskapelige universitet" ~ "NMBU",
                             studiested == "UiT Norges arktiske universitet" ~ "UiT",
                             studiested %in% c("Universitetet i Oslo", "Universitet i Oslo") ~ "UiO",
                             stringr::str_detect(studiested, "Nord") ~ "Nord",
                             studiested == "Universitetet i Agder" ~ "UiA",
                             studiested == "Universitetet i Bergen" ~ "UiB",
                             studiested == "Universitetet i Stavanger" ~ "UiS",
                             studiested == "Universitetet i Sørøst-Norge" ~ "USN",
                             studiested == "Universitetet i Tromsø" ~ "UiT",
                             .default = studiested)#,
         # across(matches("s_12_|s_32$|s_31$|s_23$|s_27$|s_24_|s_125_|s_90_|s_3[0-4]_|s_47_|s_53_|s_66_|s_73_|s_77_|s_86$|s_101$|s_109$"),
                # ~factor(.x, levels=levels(.x), ordered = TRUE))
) %>%
  labelled::copy_labels_from(survey_data[[params$cycle]][[params$response_group]], .strict = FALSE) %>%
  saros.utils::replace_stata_labels(df_new_labels = df_labels[[params$cycle]][[params$response_group]],
                              var_name_col = "variableName", var_label_col = "variableDescription") %>%
  labelled::update_variable_labels_with(.cols = where(~is.factor(.x)),
                                        .fn = ~stringr::str_replace(.x, pattern = ", vennligst spesifiser$|, spesifiser$", "")) |>
  labelled::set_variable_labels(campus2 = "Campus")

qs::qsave(x = survey_data[[params$cycle]][[params$response_group]],
          file = here::here(paths$data$saros_ready[[params$cycle]][[params$response_group]]))


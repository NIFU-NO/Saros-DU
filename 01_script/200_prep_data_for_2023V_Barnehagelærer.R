
if(params$cycle == "2023V" && params$response_group == "Barnehagelærer") { # Kjør denne fila kun for 2023V

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
             s_87 = forcats::fct_na_level_to_value(s_87, "Annet/ikke binær"),
             across(where(~is.factor(.x)), ~forcats::fct_relabel(.x, function(x) na_if(x, ""))),
             across(where(~is.character(.x)), ~na_if(.x, "")),
             campus2 = case_when(uh_samle %in% c("Dronning Mauds Minne Høgskole", "Dronning Mauds Minne Høgskole for barnehagelærerutdanning") ~ "DMMH",
                                 uh_samle %in% c("Handelshøyskolen BI") ~ "BI",
                                 uh_samle %in% c("Høgskolen i Innlandet") ~ "HINN",
                                 uh_samle %in% c("Høgskolen i Østfold") ~ "HiOF",
                                 uh_samle %in% c("Høgskulen i Volda") ~ "HiVolda",
                                 uh_samle %in% c("Høgskulen på Vestlandet") ~ "HVL",
                                 uh_samle %in% c("Høyskolen Kristiania") ~ "Annet",
                                 uh_samle %in% c("NLA høgskolen") ~ "NLA",
                                 uh_samle %in% c("Nord universitet") ~ "Nord",
                                 uh_samle %in% c("NTNU Norges teknisk-naturvitenskapelige universitet", "") ~ "Annet",
                                 uh_samle %in% c("OsloMet-Storbyuniversitetet") ~ "OsloMet",
                                 uh_samle %in% c("Rudolf Steinerhøyskolen", "Steinerhøyskolen") ~ "Steinerhøyskolen",
                                 uh_samle %in% c("UiT Norges arktiske universitet") ~ "UiT",
                                 uh_samle %in% c("Universitetet i Agder") ~ "UiA",
                                 uh_samle %in% c("Universitetet i Stavanger") ~ "UiS",
                                 uh_samle %in% c("Universitetet i Sørøst-Norge") ~ "USN",
                                 .default = uh_samle),
             s_92 = case_when(s_92 == "Jobber kun administrativt / som styrer" ~ "Annet",
                              s_92 == "Kombinerer styrerarbeid og arbeid på avdeling" ~ "Annet",
                              .default = s_92)#,
             # across(matches("s_4_|s_13|s_15$|s_16$|s_17_|s_19$|s_23$|s_20_|s_22_|s_27_|s_41_|s_102_|s_104$|s_60_|s_64_|s_70$|s_74_|s_71_|s_89$|s_90$|s_91$|s_11$"), ~ordered(.x))
             ) %>%
      labelled::copy_labels_from(survey_data[[params$cycle]][[params$response_group]], .strict = FALSE) %>%
      saros.utils::replace_stata_labels(df_new_labels = df_labels[[params$cycle]][[params$response_group]],
                                  var_name_col = "variableName", var_label_col = "variableDescription") %>%
      labelled::set_variable_labels(s_127 = "Hvordan ble tilretteleggingsmidlene brukt?",
                                    campus2 = "Campus") %>%
      labelled::update_variable_labels_with(.cols = where(~is.factor(.x)),
                                            .fn = ~stringr::str_replace(.x, pattern = ", vennligst spesifiser$|, spesifiser$", ""))

    qs::qsave(x = survey_data[[params$cycle]][[params$response_group]],
              file = here::here(paths$data$saros_ready[[params$cycle]][[params$response_group]]))
}


library(dplyr, warn.conflicts = FALSE, quietly = TRUE)
conflicted::conflicts_prefer(dplyr::filter, dplyr::lag, .quiet=TRUE)
library(magrittr)
# df_labels[[params$cycle]][[params$response_group]] <-
#   read.csv(paths$df_labels[[params$cycle]][[params$response_group]], fileEncoding = "UTF-16", sep = "\t")

survey_data[[params$cycle]][[params$response_group]] <-
  paths$data$survey[[params$cycle]][[params$response_group]] |>
  stringi::stri_replace_last_fixed(pattern = ".dta", replacement = ".rds") |>
  rio::import(trust = TRUE) |>
  labelled::unlabelled()

survey_data[[params$cycle]][[params$response_group]] <-
  survey_data[[params$cycle]][[params$response_group]] |>
  dplyr::mutate(
    across(where(~is.factor(.x)), ~forcats::fct_relabel(.x, function(x) stringr::str_remove_all(x, ", spesifiser.*|, vennligst .*"))),
    across(where(~is.factor(.x)), ~forcats::fct_relabel(.x, function(x) stringr::str_trim(x))),
    across(where(~(is.factor(.x) && c("Ikke-binær/annet") %in% levels(.x))), ~forcats::fct_na_level_to_value(.x, "Ikke-binær/annet")),
    across(where(~is.factor(.x)), ~forcats::fct_relabel(.x, function(x) na_if(x, ""))),
    across(where(~is.character(.x)), ~na_if(.x, "")),
    across(where(~is.character(.x)) & -c(svarprosent, moduler, studiested), ~ifelse(Q5.3=="Nei, de kan ikke oversendes mitt studiested", NA_character_, .x)),
    campus2 = studiested,
    mod = moduler,
    mod = case_when(mod == "Barnehage- og skolemiljø og ledelse" ~ "Barneh. og skolemiljø og led.",
                    mod == "Ledelse av prof læringsfelleskap/læreplan" ~ "Led. av prof. lær.fellesskap",
                    mod == "Ledelse av utviklings- og endringsarbeid" ~ "Led. av utv.- og endringsarb.",
                    mod == "Ledelse og digitalisering" ~ "Led. og digit.",
                    .default = mod),
    # campus2 = forcats::fct_cross(campus2, mod, sep = "- ", keep_empty = FALSE),
    across(matches("s_69_"), ~factor(.x, levels= c("1 (Helt uenig)", "2", "3", "4", "5", "6 (Helt enig)"))),
    across(where(~is.factor(.x) && length(stringr::str_subset(levels(.x), "^[0-9]+ år$")) > 2), ~as.integer(forcats::fct_relabel(.x, function(x) stringr::str_replace_all(x, " år", ""))))
    # across(matches("s_14_|s_25$|s_19_|s_18$|s_19$|s_20$|s_48_|s_56_27|s_36_|s_40_|s_46_|s_56_|s_69_|s_84$|s_81_|s_21$"), ~ordered(.x))
  ) |>
  labelled::copy_labels_from(survey_data[[params$cycle]][[params$response_group]], .strict = FALSE) |>
  # saros.utils::replace_stata_labels(df_new_labels = df_labels[[params$cycle]][[params$response_group]],
  #                             var_name_col = "variableName", var_label_col = "variableDescription") %>%
  labelled::set_variable_labels(#s_1 = "Hva er din situasjon nå?",
    svarprosent = "Svarprosent",
    studieprogram = "Studieprogram",
    campus2 = "Campus") |>
  labelled::update_variable_labels_with(.cols = where(~is.factor(.x)), .fn = ~stringr::str_replace_all(.x, ", vennligst spesifiser$|, spesifiser$|, vennligst beskriv:$", ""))|>
  labelled::update_variable_labels_with(.cols = where(~is.character(.x)), .fn = ~stringr::str_replace_all(.x, " - Text$|, vennligst spesifiser|, spesifiser|, vennligst beskriv:", ""))



for(v in colnames(survey_data[[params$cycle]][[params$response_group]])) {
  labelled::var_label(survey_data[[params$cycle]][[params$response_group]][[v]]) <-
    stringr::str_replace(string = labelled::var_label(survey_data[[params$cycle]][[params$response_group]][[v]]),
                         pattern = ": - ",
                         replacement = " - ")
}

if(dplyr::n_distinct(survey_data[[params$cycle]][[params$response_group]]$svarprosent, na.rm = TRUE) < 2) {
  cli::cli_abort("Datasettet for {params$cycle}, {params$response_group} bør inneholde også respondenter som ikke har svart, for at svarprosent-figuren skal være informativ. Disse respondentene må være NA på alle andre variabler.")
}

survey_data[[params$cycle]][[params$response_group]] |>
  labelled::lookfor(details = TRUE) |>
  dplyr::distinct(levels, .keep_all = TRUE) |>
  dplyr::select(variable, levels) %T>%
  tibble::deframe() |>
  View("Unique factor levels")

survey_data[[params$cycle]][[params$response_group]] |>
  labelled::lookfor(details = FALSE) |>
  dplyr::filter(is.na(label)) |>
  dplyr::pull(variable) %>%
  {if(length(.)) cli::cli_warn("Lacking variable label{?s} {.}")}

survey_data[[params$cycle]][[params$response_group]] |>
  labelled::lookfor(details = FALSE) |>
  dplyr::filter(stringr::str_detect(label, pattern = " - .* - ")) %>%
  {if(nrow(.)) cli::cli_warn(c("`label_separator` matches more than one delimiter, your output is likely ugly:",
                               "{(.$variable)}",
                               "{(.$label)}"))}

qs::qsave(x = survey_data[[params$cycle]][[params$response_group]],
          file = here::here(paths$data$saros_ready[[params$cycle]][[params$response_group]]))


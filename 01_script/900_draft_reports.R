library(dplyr)
library(magrittr)

# Read in survey_data from disk - to ease caching
survey_data[[params$cycle]][[params$response_group]] <-
  qs::qread(file = here::here(paths$data$saros_ready[[params$cycle]][[params$response_group]]),
            strict = TRUE, nthreads = 4)

# Chapter overview
chapter_overview[[params$cycle]][[params$response_group]] <-
  paths$chapter_overview[[params$cycle]][[params$response_group]] %>%
  readxl::read_excel() %>%
  dplyr::filter(!is.na(chapter))

############################################################


#### MACRO ######
config_macro[[params$cycle]][[params$response_group]] <-
  saros.base::read_default_draft_report_args(
    path = paths$organization_global_draft_report_settings
    ) |>
  utils::modifyList(val =
                      saros.base::read_default_draft_report_args(
    path = fs::path(paths$resources, "YAML", "_draft_report_settings.yaml")
  ))

config_macro[[params$cycle]][[params$response_group]]$title <- params$response_group

##
chunk_templates$macro <-
  saros.base::get_chunk_template_defaults() |>
  dplyr::filter(.template_name == "cat_plot_html",
                is.na(.template_variable_type_indep))
chunk_templates$macro[1, ".template"] <-
  "::: {{#fig-{.chunk_name}}}

```{{r, fig.height = fig_height_h_barchart(n_y={.n_dep}, n_cats_y={.n_cats_dep}, max_chars_cats_y={.max_chars_dep})}}
{.obj_name} <-
	data_{.chapter_foldername} |>
		makeme(dep = c({.variable_name_dep}),
		type = 'cat_plot_html')
nrange <- stringi::stri_c('N = ', n_range2({.obj_name}[[1]]))
link <- make_link(data = {.obj_name}[[1]]$data)
link_plot <- make_link(data = {.obj_name},
		file_suffix = '.png', link_prefix='[PNG](',
		save_fn = ggsaver)
x <- I(paste0(c(nrange, link, link_plot), collapse=', '))
girafe({.obj_name})
```

_{.variable_label_prefix_dep}_. `{{r}} x`.

:::"

#### MESOS ######
config_mesos[[params$cycle]][[params$response_group]] <- config_macro[[params$cycle]][[params$response_group]]
config_mesos[[params$cycle]][[params$response_group]]$title <- ""
config_mesos[[params$cycle]][[params$response_group]]$auxiliary_variables <- params$mesos_var

##
chunk_templates$mesos <-
  saros.base::get_chunk_template_defaults(3) |>
  dplyr::filter((.template_name == "cat_plot_html" & is.na(.template_variable_type_indep)) |
                  .template_name == "chr_table")
chunk_templates$mesos$.template[1] <-
"::: {{.panel-tabset}}

## Tilbyder

::: {{#fig-{.chunk_name}-target}}

```{{r}}
#| fig-height: !expr saros::fig_height_h_barchart(n_y={.n_dep}, n_cats_y={.n_cats_dep}, max_chars_cats_y={.max_chars_dep})
plot <-
	makeme(data = data_{.chapter_foldername},
	dep = c({.variable_name_dep}),
	type='cat_plot_html',
	crowd='target',
	mesos_var = params$mesos_var,
	mesos_group = params$mesos_group)
nrange <- stringi::stri_c('N = ', n_range2(plot[[1]]))
link <- make_link(data = plot[[1]]$data)
link_plot <- make_link(data = plot, link_prefix='[PNG](', save_fn = ggsaver, file_suffix = '.png')
x <-  I(paste0(c(nrange, link, link_plot), collapse = ', '))
girafe(plot)
```

`{{r}} x`.

:::


## Alle andre

::: {{#fig-{.chunk_name}-others}}

```{{r}}
#| fig-height: !expr saros::fig_height_h_barchart(n_y={.n_dep}, n_cats_y={.n_cats_dep}, max_chars_cats_y={.max_chars_dep})
plot <-
	makeme(data = data_{.chapter_foldername},
	dep = c({.variable_name_dep}),
	type='cat_plot_html',
	crowd='others',
	mesos_var = params$mesos_var,
	mesos_group = params$mesos_group)
nrange <- stringi::stri_c('N = ', n_range2(plot[[1]]))
link <- make_link(data = plot[[1]]$data)
link_plot <- make_link(data = plot, link_prefix='[PNG](', save_fn = ggsaver, file_suffix = '.png')
x <-  I(paste0(c(nrange, link, link_plot), collapse = ', '))
girafe(plot)
```

`{{r}} x`.

:::

_{.variable_label_prefix_dep}_ by _{tolower(.variable_label_prefix_indep)}_

:::"


chunk_templates$mesos$.template[2] <-
"::: {{#tbl-{.chunk_name}-target}}

```{{r}}
table <-
	makeme(data = data_{.chapter_foldername},
		dep = c({.variable_name_dep}),
		type = 'chr_table_html',
		crowd='target',
		mesos_var = params$mesos_var,
		mesos_group = params$mesos_group)
gt(table[[1]])
```

_{.variable_label_prefix_dep}_ for `{{r}} params$mesos_group`.

:::
"

############################################################

# wrapper_create_mesos_qmd_files <- function(mesos_var = params$mesos_var,
#                                            mesos_groups,
#                                            mesos_group_path = fs::path(paths$drafts_produced, params$cycle, response_group_path_pretty, params$mesos_folder_name),
#                                            read_syntax_pattern = "qs::qread\\('",
#                                            read_syntax_replacement = "qs::qread('../",
#                                            files_to_process_regex = "[0-9]+_|index|report") {
#
#   if(!rlang::is_string(mesos_var) || !is.character(mesos_groups) || !rlang::is_string(mesos_group_path) || !fs::dir_exists(mesos_group_path)) {
#     cli::cli_abort("{.arg mesos_var} must be a string, {.arg mesos_groups} must be a character vector, and {mesos_group_path} must be an existing directory.")
#   }
#
#   withr::with_dir(new = mesos_group_path,
#                   {
#                     files_to_be_renamed <-
#                       fs::dir_ls(path = ".", recurse = FALSE, type = "file", regexp = files_to_process_regex) %>%
#                       rlang::set_names(stringr::str_c("_", .), nm=.)
#                     if(!all(files_to_be_renamed == "_")) fs::file_move(path=names(files_to_be_renamed),
#                                                                        new_path = unname(files_to_be_renamed))
#
#                     files_to_be_renamed |>
#                     purrr::walk(~{
#                       x <- brio::read_lines(.x)
#                       x |>
#                         stringr::str_replace_all(read_syntax_pattern, read_syntax_replacement) |>
#                         brio::write_lines(.x)
#                     })
#
#                     saros.utils::create_mesos_qmd_files(
#                       dir_path = ".",
#                       mesos_var = mesos_var,
#                       mesos_groups = mesos_groups)
#                   })
#
# }
### For alt annet enn Modul: Rektor, Barnehage, osv. Erik i Udir ønsker oppdelt etter modulene.
if(params$response_group != "Modul") {

  response_group_path_pretty <- params$response_group

  tryCatch(fs::dir_delete(fs::path(paths$drafts_produced, params$cycle, response_group_path_pretty)), error=function(e) cli::cli_inform(e))

  # Macro
  config_macro[[params$cycle]][[params$response_group]][["title"]] <- params$response_group

  chapter_structure$macro[[params$cycle]][[params$response_group]] <-
    saros.base::refine_chapter_overview(
      data = survey_data[[params$cycle]][[params$response_group]],
      chapter_overview = chapter_overview[[params$cycle]][[params$response_group]],
      chunk_templates = chunk_templates$macro,
      organize_by = config_macro[[params$cycle]][[params$response_group]][["organize_by"]]
    )

  rlang::inject(
    saros.base::draft_report(
      data = survey_data[[params$cycle]][[params$response_group]],
      chapter_structure = chapter_structure$macro[[params$cycle]][[params$response_group]],
      !!!config_macro[[params$cycle]][[params$response_group]][!names(config_macro[[params$cycle]][[params$response_group]]) %in% c("organize_by")],
      path = fs::path(paths$drafts_produced, params$cycle, response_group_path_pretty)
    ))


  # Mesos
  mesos_df <-
    survey_data[[params$cycle]][[params$response_group]] %>%
    tibble::as_tibble() %>%
    dplyr::mutate(campus2 = as.character(campus2)) %>%
    labelled::set_variable_labels(campus2 = "Campus")

  chapter_structure$mesos[[params$cycle]][[params$response_group]] <-
    saros.base::refine_chapter_overview(
      data = mesos_df,
      chapter_overview = chapter_overview[[params$cycle]][[params$response_group]],
      chunk_templates = chunk_templates$mesos,
      organize_by = config_mesos[[params$cycle]][[params$response_group]][["organize_by"]]
    )

  output_files[[params$cycle]][[params$response_group]] <-
    rlang::inject(
      saros.base::draft_report(
        data = mesos_df,
        chapter_structure =
          chapter_structure$mesos[[params$cycle]][[params$response_group]] |>
          dplyr::select(-any_of("author")),
        !!!config_mesos[[params$cycle]][[params$response_group]][!names(config_mesos[[params$cycle]][[params$response_group]]) %in% c("organize_by")],
        path = fs::path(paths$drafts_produced, params$cycle, response_group_path_pretty, params$mesos_folder_name)
      ))

  mesos_df |>
    dplyr::filter(dplyr::n() >= 7, .by = campus2) |>
    _[[params$mesos_var]] |>
    unique() |>
    sort() |>
    saros.base::setup_mesos(
      mesos_groups = _,
      files_to_process = output_files[[params$cycle]][[params$response_group]],
      mesos_var = params$mesos_var)

}
if(params$response_group == "Modul") {
  # Sammenligne BI Digi og ledelse mot alle andre tilbyderes digi og ledelse-modul

  modules <- unique(survey_data[[params$cycle]][[params$response_group]]$mod)
  modules <- modules[!is.na(modules)]

  for(module in modules) {

    response_group_path_pretty <- paste0("Modul ", stringr::str_replace_all(module, pattern = "\\s", replacement = "-"))
    config_macro[[params$cycle]][[params$response_group]][["title"]] <- paste0("Modul ", module)

    tryCatch(fs::dir_delete(fs::path(paths$drafts_produced, params$cycle, response_group_path_pretty)), error=function(e) cli::cli_inform(e))

    # Macro
    chapter_structure$macro[[params$cycle]][[params$response_group]] <-
      saros.base::refine_chapter_overview(
        data =
          survey_data[[params$cycle]][[params$response_group]] %>%
          dplyr::filter(mod == module),
        chapter_overview = chapter_overview[[params$cycle]][[params$response_group]],
        chunk_templates = chunk_templates$macro,
        organize_by = config_macro[[params$cycle]][[params$response_group]][["organize_by"]]
      )

    rlang::inject(
      saros.base::draft_report(
        data =
          survey_data[[params$cycle]][[params$response_group]] %>%
          dplyr::filter(mod == module),
        chapter_structure = chapter_structure$macro[[params$cycle]][[params$response_group]],
        !!!config_macro[[params$cycle]][[params$response_group]][!names(config_macro[[params$cycle]][[params$response_group]]) %in% c("organize_by")],
        path = fs::path(paths$drafts_produced, params$cycle, response_group_path_pretty)
      ))

    # Mesos

    mesos_df <-
      survey_data[[params$cycle]][[params$response_group]] |>
      tibble::as_tibble() |>
      dplyr::filter(mod == module) |>
      dplyr::mutate(campus2 = as.character(campus2)) |>
      labelled::set_variable_labels(campus2 = "Campus")

    chapter_structure$mesos[[params$cycle]][[params$response_group]] <-

      saros.base::refine_chapter_overview(
        data = mesos_df,
        chapter_overview = chapter_overview[[params$cycle]][[params$response_group]],
        chunk_templates = chunk_templates$mesos,
        organize_by = config_mesos[[params$cycle]][[params$response_group]][["organize_by"]]
      )


    output_files[[params$cycle]][[response_group_path_pretty]] <-
      rlang::inject(
        saros.base::draft_report(
          data = mesos_df,
          chapter_structure =
            chapter_structure$mesos[[params$cycle]][[params$response_group]] %>%
            dplyr::select(-any_of("author")),
          !!!config_mesos[[params$cycle]][[params$response_group]][!names(config_mesos[[params$cycle]][[params$response_group]]) %in% c("organize_by")],
          path = fs::path(paths$drafts_produced, params$cycle, response_group_path_pretty, params$mesos_folder_name)
        ))

    mesos_df |>
      dplyr::filter(dplyr::n() >= 7, .by = campus2) |>
      _[[params$mesos_var]] |>
      unique() |>
      sort() |>
      saros.base::setup_mesos(
        mesos_groups = _,
        files_to_process = output_files[[params$cycle]][[response_group_path_pretty]],
        mesos_var = params$mesos_var)

  }
}



### Create general_formatting.R files in subdirectories which only run general_formatting.R in parent folder.
withr::with_dir(new = paths$site_drafts_completed, code = {
  fs::dir_ls(recurse = T, type = "directory", regexp = "_images|Tilbydere/|/[0-9]{1,2}_", invert = TRUE) %>%
    c(.) %>%
    fs::path(., "general_formatting.R") |>
    purrr::walk(.f = ~brio::write_lines(text='source("../general_formatting.R", chdir = TRUE)', path = .x))
}
)


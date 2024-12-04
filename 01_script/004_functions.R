remove_label_parts <- function(data = survey_data[[params$cycle]][[params$response_group]],
                               pattern = NULL) {
  if(rlang::is_string(pattern)) {
    for(var in colnames(data)) {
      if(is.factor(data[[var]])) {
        label <- attr(data[[var]], "label")
        label <- stringr::str_replace(label, pattern = pattern, replacement = "")
        if(length(label)>0)  attr(data[[var]], "label") <- label
      }
    }
  }
}

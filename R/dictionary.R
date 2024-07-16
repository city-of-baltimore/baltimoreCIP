load_cip_dict <- function(sheet = NULL, file = NULL, ...) {
  dict_data <- googlesheets4::read_sheet(
    "https://docs.google.com/spreadsheets/d/1KKIWzLb31gZIyLQqS_W4EaCtcJ4ROXqvypFzYICbQxE/edit?usp=sharing",
    sheet = sheet
  )

  if (is.null(file)) {
    return(invisible(dict_data))
  }

  readr::write_csv(
    dict_data,
    file = file,
    ...
  )
}

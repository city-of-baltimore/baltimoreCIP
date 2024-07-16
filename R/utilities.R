## ---- mutate_trim_squish
#' Modify character columns using `str_squish` and `str_trim`
#'
#' [mutate_trim_squish()] is a convenience function for squishing and removing
#' whitespace from all character columns in a data frame.
#'
mutate_trim_squish <- function(data, side = c("both", "left", "right")) {
  mutate(
    data,
    across(
      where(is.character),
      \(x) {
        str_trim(str_squish(x), side = side)
      }
    )
  )
}

## ---- str_extract_agency_contract_id
str_extract_agency_contract_id <- function(
    string,
    pattern = "(TR |Tr |TR|TR-|SWC|SWC |SWC-|WC|WC-|WC |SDC|SDC |SDC-|SC |SC|SC-|ER |ER-|ER)[:digit:]+",
    extract_all = FALSE,
    ...) {
  fn <- str_extract

  if (extract_all) {
    fn <- str_extract_all
  }

  string <- fn(
    string,
    pattern,
    ...
  )

  remove_pattern <- "[:space:]|[:punct:]"

  if (!is.list(string)) {
    return(str_remove_trim(toupper(string), pattern = remove_pattern))
  }

  lapply(
    string,
    \(x) {
      str_remove_trim(toupper(x), pattern = remove_pattern)
    }
  )
}


## ---- str_remove_trim
#' Remove part of a string matching a pattern and then trim whitespace
#'
#' [str_remove_trim()] is a wrapper for `str_remove` and
#' `str_trim`. The function is used by [format_prj_data()] to
#' remove codes from name columns.
#'
str_remove_trim <- function(string, pattern, side = c("both", "left", "right")) {
  str_trim(str_remove(string, pattern), side = side)
}

# Functions for working with data frames ----

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


#' Get list of sheets and agency names from Agency Summary report
#'
get_cip_agencies <- function(path) {
  sheets <- readxl::excel_sheets(path)

  agencies <- map(
    sheets,
    \(x) {
      data <- suppressMessages(
        readxl::read_xls(
          path,
          sheet = x,
          skip = 0,
          n_max = 2
        )
      )

      data <- data[[1]]

      stringr::str_remove(
        data,
        "Ordinance of Estimates Recommendation for: "
      )
    }
  )

  # agencies
  rlang::set_names(sheets, agencies)
}


#' Read a CIP Budget Report from XLS
#'
read_cip_report <- function(path,
                            sheet = NULL,
                            col_types = "text",
                            report_type = "budget",
                            ...) {
  col_names <- switch(report_type,
    budget = c(
      "drop_units", "key", "value", "drop_title",
      "operating_impact_amt", "drop_fy", "appropriated_amt",
      "agency_amt", "planning_amt", "bof_amt", "drop_spacer_1",
      "drop_spacer_2", "boe_amt", "drop_spacer_3", "budget_amt",
      "drop_spacer_4", "drop_spacer_5", "total_amt"
    ),
    program = c(
      "drop_units", "key", "value", "agency_name",
      "drop_spacer_1", "appropriated_amt", "fy1_amt", "fy2_amt",
      "operating_impact_amt", "fy3_amt", "fy4_amt", "drop_spacer_2",
      "fy5_amt", "drop_spacer_3", "drop_spacer_4", "fy6_amt", "total_amt"
    )
  )

  readxl::read_xls(
    path = here::here(path),
    sheet = sheet,
    col_names = col_names,
    col_types = col_types,
    ...
  ) |>
    dplyr::select(
      !dplyr::starts_with("drop_")
    )
}

#' Format a CIP Budget Report as rectangular data
#'
format_cip_report <- function(data) {
  if (rlang::has_name(data, "agency_name")) {
    data <- data |>
      dplyr::mutate(
        agency_name = stringr::str_remove(
          agency_name,
          "Ordinance of Estimates Recommendation for: "
        )
      ) |>
      tidyr::fill(agency_name)
  }

  data <- data |>
    dplyr::mutate(
      operating_impact_amt = readr::parse_integer(
        stringr::str_extract(
          operating_impact_amt,
          "[:digit:]+$"
        )
      )
    ) |>
    dplyr::mutate(
      cip_number = dplyr::if_else(
        stringr::str_detect(key, "[:digit:]{3}-[:digit:]{3}"),
        key,
        NA_character_
      ),
      key = dplyr::if_else(
        !is.na(operating_impact_amt),
        NA_character_,
        key
      ),
      location = dplyr::if_else(
        key == "Location:",
        value,
        NA_character_
      ),
      project_desc = dplyr::if_else(
        key == "Description:",
        value,
        NA_character_
      ),
      project_title = dplyr::if_else(
        !is.na(cip_number),
        value,
        NA_character_
      ),
      amt_type = dplyr::case_when(
        !is.na(cip_number) ~ cip_number,
        dplyr::lag(key) == "Source of Funds" ~ "Source",
        key == "Total" ~ "Total"
      ),
      .before = operating_impact_amt
    ) |>
    tidyr::fill(
      location, project_desc, project_title, cip_number,
      amt_type, operating_impact_amt
    ) |>
    dplyr::mutate(
      source = dplyr::if_else(
        amt_type == "Source",
        key,
        NA_character_
      )
    ) |>
    dplyr::relocate(
      project_title,
      .after = cip_number
    ) |>
    dplyr::relocate(
      dplyr::starts_with("fy"),
      .after = dplyr::everything()
    ) |>
    dplyr::filter(
      amt_type %in% c("Source", "Total"),
      !is.na(key)
    ) |>
    dplyr::select(
      !c(key, value)
    ) |>
    format_report_amt() |>
    dplyr::mutate(
      source_code = readr::parse_integer(
        stringr::str_extract(
          source,
          "^[:digit:]+"
        )
      ),
      source_name = stringr::str_trim(
        stringr::str_remove(
          source,
          as.character(source_code)
        )
      ),
      bureau_program = stringr::str_sub(
        cip_number,
        end = 3
      )
    )

  if (rlang::has_name(data, "appropriated_amt")) {
    data <- data |>
      dplyr::mutate(
        has_prior_appropriation = dplyr::case_when(
          appropriated_amt > 0 ~ TRUE,
          appropriated_amt == 0 ~ FALSE,
          .default = NA
        )
      )
  }

  data
}

format_report_amt <- function(data) {
  dplyr::mutate(
    data,
    dplyr::across(
      dplyr::ends_with("_amt"),
      \(x) {
        readr::parse_integer(
          stringr::str_remove_all(
            stringr::str_replace(x, "Zero", "0"),
            ","
          )
        ) * 1000
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

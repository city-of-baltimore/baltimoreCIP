#' Read 2014-2024 budget reports from XLS spreadsheets
#'
read_2014_2024_budget_reports <- function(
    budget_years = paste0("FY", 2014:2024)) {
  report_files <- fs::dir_ls("files/budget")
  # report_files <- rlang::set_names(report_files, budget_years)

  fy_df <- tibble::as_tibble(
    list(
      "FY" = budget_years,
      "filename" = report_files
    )
  )

  report_files |>
    rlang::set_names(
      fs::path_file(report_files)
    ) |>
    purrr::map(
      \(x) {
        x |>
          read_cip_report(report_type = "budget") |>
          format_cip_report()
      }
    ) |>
    purrr::list_rbind(names_to = "filename") |>
    dplyr::left_join(
      fy_df
    ) |>
    dplyr::mutate(
      fiscal_year = parse_integer(
        stringr::str_remove(FY, "FY")
      ),
      # FIXME: Parsing contract numbers provides an incomplete record and may
      # include incorrect values. These should be identified and removed and/or
      # the uncertainty of the values should be documented
      contract_number = coalesce(
        str_extract_agency_contract_id(project_title),
        str_extract_agency_contract_id(project_desc)
      )
    )
}


build_bureau_xwalk <- function() {
  fs::dir_ls("files/agency") |>
    set_names(fs::dir_ls("files/agency")) |>
    map(
      \(x) {
        readxl::read_xls(
          x,
          range = "B8:E33",
          col_names = c(
            "bureau_program",
            "drop_1",
            "bureau_name",
            "drop_2"
          )
        ) |>
          select(!starts_with("drop_"))
      }
    ) |>
    list_rbind(names_to = "filename") |>
    filter(!is.na(bureau_name)) |>
    arrange(desc(filename)) |>
    select(!filename) |>
    distinct(bureau_program, .keep_all = TRUE)
}

#' Get list of sheets and agency names from Agency Summary report
#'
get_cip_agencies <- function(path) {
  sheets <- readxl::excel_sheets(path)

  agencies <- map(
    sheets,
    \(x) {
      data <- suppressMessages(
        readxl::read_xls(path, sheet = x, skip = 0, n_max = 2)
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


format_2014_2024_budget_reports <- function(report_data,
                                            budget_data = NULL,
                                            bureau_xwalk = NULL) {
  report_data |>
    dplyr::left_join(
      budget_data,
      relationship = "many-to-one",
      na_matches = "never",
      by = join_by(cip_number)
    ) |>
    dplyr::left_join(
      bureau_xwalk,
      relationship = "many-to-one",
      na_matches = "never",
      by = join_by(bureau_program)
    ) |>
    mutate(
      cip_year_id = paste0(cip_number, "_", fiscal_year),
      has_prior_request = has_prior_appropriation | fiscal_year > request_year_min # ,
      # FIXME: Consider if request_year_min and budget_year_min should be replaced
      # with NA values for 2014 data
      # request_year_min = if_else(
      #   has_prior_appropriation & (fiscal_year == request_year_min),
      #   NA_integer_,
      #   request_year_min
      # ),
      # budget_year_min = if_else(
      #   has_prior_appropriation & budget_year_min == 2014,
      #   NA_integer_,
      #   budget_year_min
      # )
    ) |>
    relocate(
      starts_with("has_prior"),
      .before = request_year_min
    ) |>
    relocate(
      bureau_name,
      .after = bureau_program
    ) |>
    relocate(
      fiscal_year,
      cip_number,
      cip_year_id,
      .before = everything()
    ) |>
    relocate(
      FY,
      .before = amt_type
    ) |>
    arrange(
      bureau_program,
      desc(fiscal_year),
      cip_number,
      source
    )
}


format_budget_data <- function(report_data) {
  budget_funded_source <- report_data |>
    filter(is.na(source)) |>
    dplyr::distinct(fiscal_year, cip_number, .keep_all = TRUE) |>
    mutate_trim_squish()

  budget_funded_source |>
    dplyr::summarise(
      # year_new = min(fiscal_year, na.rm = TRUE),
      project_titles = as.character(knitr::combine_words(unique(project_title))),
      request_year_min = min(fiscal_year, na.rm = TRUE),
      request_year_max = max(fiscal_year, na.rm = TRUE),
      request_year_list = list(unique(fiscal_year)),
      n_request_year = length(unique(fiscal_year)),
      total_budget_amt = sum(budget_amt),
      .by = cip_number
    ) |>
    mutate(
      request_year_range = request_year_max - (request_year_min - 1)
    ) |>
    left_join(
      budget_funded_source |>
        filter(is.na(source), budget_amt > 0) |>
        summarise(
          budget_year_min = min(fiscal_year, na.rm = TRUE),
          budget_year_max = max(fiscal_year, na.rm = TRUE),
          n_budget_year = length(unique(fiscal_year)),
          .by = cip_number
        ),
      by = join_by(cip_number)
    ) |>
    tidyr::replace_na(
      replace = list(
        n_budget_year = 0
      )
    )
}

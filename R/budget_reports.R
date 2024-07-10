library(tidyverse)

budget_years <- paste0("FY", 2014:2024)

source(here::here("data-raw", "functions.R"))

report_files <- fs::dir_ls("inst/extdata/budget")
report_files <- rlang::set_names(report_files, budget_years)

program_files <- fs::dir_ls("inst/extdata/program")
program_files <- rlang::set_names(program_files, budget_years)

budget_reports_source <- report_files |>
  rlang::set_names(budget_years) |>
  purrr::map(
    \(x) {
      x |>
        read_cip_report(report_type = "budget") |>
        format_cip_report()
    }
  ) |>
  purrr::list_rbind(names_to = "FY") |>
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

budget_funded_source <- budget_reports_source |>
  filter(is.na(source)) |>
  dplyr::distinct(fiscal_year, cip_number, .keep_all = TRUE) |>
  mutate_trim_squish()

budget_funded <- budget_funded_source |>
  dplyr::summarise(
    # year_new = min(fiscal_year, na.rm = TRUE),
    project_titles = knitr::combine_words(unique(project_title)),
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

bureau_xwalk <- fs::dir_ls("inst/extdata/agency") |>
  set_names(fs::dir_ls("inst/extdata/agency")) |>
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

budget_reports <- budget_reports_source |>
  dplyr::left_join(
    budget_funded,
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

budget_reports |>
  filter(is.na(source)) |>
  readr::write_csv(
    here::here("files", "FY14-FY24_CIP-Requests_Totals.csv")
  )

budget_reports |>
  readr::write_csv(
    here::here("files", "FY14-FY24_CIP-Requests_Source.csv")
  )

budget_reports <- readr::read_csv(
    here::here("files", "FY14-FY24_CIP-Requests_Source.csv")
  )


if (FALSE) {
  # https://docs.google.com/spreadsheets/d/1KKIWzLb31gZIyLQqS_W4EaCtcJ4ROXqvypFzYICbQxE/edit?usp=sharing
  budget_reports |>
    getdata::make_variable_dictionary(details = "full") |>
    View()
}

if (FALSE) {
  program_reports <- program_files |>
    purrr::map(
      \(path) {
        program_sheets <- purrr::map(
          get_cip_agencies(path),
          \(sheet) {
            read_cip_report(
              path = path,
              report_type = "program",
              sheet = sheet
            ) |>
              format_cip_report()
          }
        )

        purrr::list_rbind(
          program_sheets
        )
      }
    )

  program_reports <- program_reports |>
    list_rbind(names_to = "FY")

  program_report_summary <-  program_reports |>
    filter(is.na(source)) |>
    pivot_longer(
      cols = matches("fy[1-6]"),
      names_to = "request_year",
      names_pattern = "fy(.)_amt",
      values_to = "amt"
      ) |>
    # names()
    mutate(
      program_year = as.numeric(str_remove(FY, "^FY")),
      request_year = as.integer(request_year),
      request_year = program_year + request_year - 1
    ) |>
    filter(amt >= 0) |>
    # filter(program_year > 2014) |>
    summarise(
      min_program_year = min(program_year),
      min_request_year = min(request_year),
      max_request_year = max(request_year),
      .by = cip_number
    ) |>
    left_join(
      budget_reports |>
        filter(is.na(source)) |>
        summarise(
          agency_amt = sum(agency_amt),
          budget_amt = sum(budget_amt),
          .by = cip_number
        ),
    ) |>
    mutate(
      request_in_first_year = min_program_year == min_request_year
    ) #

  program_report_summary |>
    mutate(
      request_in_first_year = if_else(
        request_in_first_year,
        "Request in 1st yr",
        "Request not in 1st yr"
      )
    ) |>
    filter(
      !(cip_number %in%
    )
    summarise(
      budget_amt = sum(budget_amt, na.rm = TRUE),
      agency_amt = sum(agency_amt, na.rm = TRUE),
      .by = request_in_first_year
    ) |>
    View()
    ggplot() +
    geom_col(aes(request_in_first_year, budget_amt))# +
    facet_wrap(~request_in_first_year)



  budget_funded |>
    filter(n_request_year > 9) |>
    View()
  # read_cip_report(
  #   program_files[[1]],
  #   report_type = "program"
  # ) |>
  #   format_budget_report() |>
  #   View()


  capital_budget_summary <- budget_reports |>
    group_by(FY) |>
    summarise(
      across(
        ends_with("_amt"),
        sum
      )
    )

  capital_budget_summary |>
    ggplot() +
    geom_point(aes(FY, budget_amt), color = "red", size = 4) +
    geom_point(aes(FY, agency_amt), color = "blue", size = 4) +
    geom_point(aes(FY, planning_amt), color = "yellow", size = 4) +
    theme_minimal() +
    scale_y_continuous(labels = scales::label_currency())
}

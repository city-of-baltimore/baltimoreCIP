library(targets)
library(tarchetypes)
library(tidyverse)

tar_option_set(
  packages = c("tidyverse", "sf")
)

tar_source()

tar_plan(
  # Get and format data on 2008-2013 projects ----
  budget_reports_text_2008_2013 = read_pre_2014_program_report_text() |>
    format_pre_2014_program_report_text(),
  project_details_2008_2013 = budget_reports_text_2008_2013 |>
    get_pre_2014_project_details(),
  project_info_2008_2013 = project_details_2008_2013 |>
    get_pre_2014_project_info(),
  budget_reports_2008_2013 = project_details_2008_2013 |>
    get_pre_2014_project_funding(project_info_2008_2013),

  # Get locations for 2014-2024 projects ----
  cip_locations_2014_2024_src = read_cip_locations(),
  cip_locations_2014_2024 = cip_locations_2014_2024_src |>
    format_cip_location_data(),
  bureau_xwalk_2014_2024 = build_bureau_xwalk(),
  budget_reports_2014_2024_src = read_2014_2024_budget_reports(),
  budget_reports_2014_2024 = budget_reports_2014_2024_src |>
    format_2014_2024_budget_reports(
      budget_data = format_budget_data(budget_reports_2014_2024_src),
      bureau_xwalk = bureau_xwalk_2014_2024
    ),
  budget_reports_2014_2024_dict = load_cip_dict(
    sheet = "FY14-FY24_CIP-Requests_Source_Dictionary",
    file = here::here(
      "data",
      "FY14-FY24_CIP-Requests_Source_Dictionary.csv"
    )
  ),
  location_corrections = googlesheets4::read_sheet(
    "https://docs.google.com/spreadsheets/d/16b1AAoZEmcjsrteFNkxNu4gYsIs4MFx__DuMCW9p_hk/edit?usp=sharing"
  ) |>
    filter(!is.na(location_corrected)) |>
    select(!n),

  budget_reports_2008_2024 = combine_budget_reports(
    budget_reports_2008_2013,
    budget_reports_2014_2024
  ) |>
    left_join(
      location_corrections
    ) |>
    mutate(
      location = coalesce(
        location_corrected,
        location
      )
    ) |>
    select(!location_corrected),

  # Render README
  tar_quarto(
    readme_qmd,
    path = here::here("README.qmd")
  )
)

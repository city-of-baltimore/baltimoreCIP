library(targets)
library(tarchetypes)
library(tidyverse)

tar_option_set(
  packages = c("tidyverse", "sf")
)

tar_source()

tar_plan(
  # Get and format data on 2008-2013 projects
  pre_2014_report_text = read_pre_2014_program_report_text() |>
    format_pre_2014_program_report_text(),
  pre_2014_project_details = pre_2014_report_text |>
    get_pre_2014_project_details(),
  pre_2014_project_info = pre_2014_project_details |>
    get_pre_2014_project_info(),
  pre_2014_project_funding = pre_2014_project_details |>
    get_pre_2014_project_funding(pre_2014_project_info),

  # Get locations for 2014-2024 projects
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

  # Render README
  tar_quarto(
    readme_qmd,
    path = here::here("README.qmd")
  )
)

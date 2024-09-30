combine_budget_reports <- function(
    data_2008_2013,
    data_2014_2024,
    save = TRUE) {
  data_2014_2024 <- data_2014_2024 |>
    filter(amt_type == "Total") |>
    select(
      !c(
        contains(c("max", "min")),
        any_of(
          c(
            "request_year_list",
            "n_request_year",
            "request_year_range",
            "n_budget_year"
          )
        ),
        starts_with("has_")
      )
    )

  data_2008_2013 <- data_2008_2013 |>
    filter(amt_type == "Total")

  data_2008_2024 <- list_rbind(
    list(
      data_2014_2024,
      data_2008_2013
    )
  ) |>
    # Drop columns that only appear in one of the data frames
    select(
      !c(
        starts_with("source"),
        report_fy,
        offset_fy,
        project_titles,
        FY,
        budget_amt,
        total_amt,
        total_budget_amt
      )
    )

  if (!save) {
    return(data_2008_2024)
  }

  readr::write_csv(
    data_2008_2024,
    fs::path(
      "data",
      "FY08-FY24_CIP-Budgets.csv"
    )
  )
}

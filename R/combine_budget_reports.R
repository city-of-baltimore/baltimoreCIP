combine_budget_reports <- function(
    data_2008_2013,
    data_2014_2024) {
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

  list_rbind(
    list(
      data_2014_2024,
      data_2008_2013
    )
  )
}

#' Download files from DOP website
download_pre_2014_program_reports <- function(...) {
  pre_2014_program_reports <- tibble::tribble(
    ~Link, ~URL, ~Type,
    "2008 - 2013", "https://planning.baltimorecity.gov/sites/default/files/2008cp.pdf", "Report",
    "2008 Fiscal Year Map", "https://planning.baltimorecity.gov/sites/default/files/cpmap2008.pdf", "Map",
    "2009 - 2014", "https://planning.baltimorecity.gov/sites/default/files/2009cp.pdf", "Report",
    "2009 Fiscal Year Map", "https://planning.baltimorecity.gov/sites/default/files/cpmap2009.pdf", "Map",
    "2010 - 2015", "https://planning.baltimorecity.gov/sites/default/files/2010cp.pdf", "Report",
    "2010 Fiscal Year Map", "https://planning.baltimorecity.gov/sites/default/files/cpmap2010.pdf", "Map",
    "2011 - 2016", "https://planning.baltimorecity.gov/sites/default/files/2011cp.pdf", "Report",
    "2011 Fiscal Year Map", "https://www.baltimorecity.gov/sites/default/files/2011%20BOE%20Map.pdf", "Map",
    "2012 - 2017", "https://planning.baltimorecity.gov/sites/default/files/2012cp.pdf", "Report",
    "2012 Fiscal Year Map", "https://planning.baltimorecity.gov/sites/default/files/cpmap2012.pdf", "Map",
    "2013 - 2018", "https://planning.baltimorecity.gov/sites/default/files/2013cp.pdf", "Report",
    "2013 Fiscal Year Map", "https://planning.baltimorecity.gov/sites/default/files/cpmap2013.pdf", "Map"
  )

  pre_2014_program_reports |>
    filter(
      Type == "Report"
    ) |>
    pull(URL, name = Link) |>
    map(
      \(x) {
        download.file(
          url = x,
          destfile = fs::path(
            "files",
            "program",
            basename(x)
          )
        )
      }
    )
}


#' Read data from PDF files in `files/program` directory
#'
read_pre_2014_program_report_text <- function(...) {
  # Read text from PDF files into a list
  report_files <- fs::path("files", "program") |>
    fs::dir_ls(
      glob = "*.pdf"
    )

  report_files |>
    map(
      \(x) {
        # Extra page text to vector
        page_text <- pdftools::pdf_text(x)

        # Build data frame from page text
        tibble::tibble(
          text = page_text,
          page = seq_along(page_text),
          filename = fs::path_file(x),
          year = str_extract(fs::path_file(x), "[:digit:]+")
        )
      }
    ) |>
    list_rbind() |>
    # Separate lines into rows
    separate_longer_delim(
      text,
      delim = "\n"
    ) |>
    mutate(
      text = str_trim(text)
    )
}

#' Add key column based on text from 2008-2013 reports
format_pre_2014_program_report_text <- function(report_text) {
  # Convert list into (mostly) tidy data frame
  report_text |>
    mutate(
      key = case_when(
        str_detect(text, "^Board of Estimates Recommendation for:") ~ "bureau",
        str_detect(text, "^Source of Funds") ~ "source header",
        str_detect(text, "^[:digit:]{3}[:space:]") ~ "source",
        str_detect(text, "^Description:") ~ "description",
        str_detect(text, "^[:digit:]{3}-[:digit:]{3}") ~ "project",
        str_detect(text, "^Location:") ~ "location",
        str_detect(text, "^Total") ~ "total",
        str_detect(text, "Print Date:[:space:]") ~ "page footer",
        str_detect(text, "^City of Baltimore[:space:]+") ~ "page header"
      ),
      # Fill description keys for multi-line descriptions
      key = if_else(
        is.na(key) & lead(key) == "description",
        "description",
        key
      )
    ) |>
    filter(
      # Drop empty columns and label columns
      text != "",
      text != "Description:",
      text != "Amounts in Thousands"
    ) |>
    mutate(
      # Fill description keys for multi-line descriptions (after dropping description label)
      key = if_else(
        is.na(key) & lag(key) == "description",
        "description",
        key
      )
    )
}

get_pre_2014_project_details <- function(report_text) {
  # Pull project details from report text
  report_text |>
    mutate(
      cip_num = if_else(
        key == "project",
        str_extract(text, "^[:digit:]{3}-[:digit:]{3}"),
        NA_character_
      ),
      project_title = if_else(
        key == "project",
        str_remove(text, fixed(cip_num)),
        NA_character_
      ),
      bureau_name = if_else(
        key == "bureau",
        str_trim(
          str_remove(text, "^Board of Estimates Recommendation for:[:space:]")
        ),
        NA_character_
      ),
      location = if_else(
        key == "location",
        str_remove(
          str_extract(text, ".+(?=[:space:]{3,}Impact)"),
          "^Location:[:space:]"
        ),
        NA_character_
      ),
      operating_impact_amt = if_else(
        key == "location",
        str_extract(
          text,
          "(?<=Budget:).+$"
        ),
        NA_character_
      )
    ) |>
    mutate(
      across(
        c(location, project_title),
        str_squish
      )
    ) |>
    fill(
      bureau_name,
      cip_num,
      project_title
    ) |>
    mutate(
      # Drop duplicate column (?)
      flag = if_else(
        !is.na(location) & location == "High Level Sewer Shed" & filename == "2010cp.pdf",
        TRUE,
        FALSE
      )
    ) |>
    filter(!flag) |>
    select(!flag)
}

get_pre_2014_project_info <- function(project_details) {
  # Pull additional project information from project details
  project_details |>
    fill(key) |>
    filter(
      key == "description"
    ) |>
    summarise(
      project_desc = paste0(
        str_remove(text, "^Description:[:space:]"),
        collapse = " "
      ),
      .by = c(filename, bureau_name, cip_num)
    ) |>
    distinct(filename, bureau_name, cip_num, project_desc) |>
    left_join(
      project_details |>
        filter(!is.na(location) | !is.na(operating_impact_amt)) |>
        select(filename, bureau_name, cip_num, project_title, location, operating_impact_amt) |>
        distinct()
    ) |>
    mutate(
      project_desc = str_squish(project_desc),
      fiscal_year = as.integer(str_extract(filename, "^[:digit:]+")),
      cip_year_id = glue::glue("{cip_num}_{fiscal_year}")
    ) |>
    rename(
      cip_number = cip_num
    ) |>
    mutate(
      # FIXME: Parsing contract numbers provides an incomplete record and may
      # include incorrect values. These should be identified and removed and/or
      # the uncertainty of the values should be documented
      contract_number = coalesce(
        str_extract_agency_contract_id(project_title),
        str_extract_agency_contract_id(project_desc)
      )
    )
}

get_pre_2014_project_funding <- function(project_details, project_info) {
  # Pull project funding from project details
  budget_reports <- project_details |>
    filter(
      key %in% c("source header", "source", "total")
    ) |>
    mutate(
      amt_cols = if_else(
        key %in% c("source", "total"),
        str_squish(text),
        NA_character_
      )
    ) |>
    mutate(
      amt_cols = stringr::str_remove_all(amt_cols, ","),
      amt_cols = stringr::str_replace_all(amt_cols, "Zero", "0") # ,
    ) |>
    mutate(
      source_code = readr::parse_integer(stringr::str_extract(amt_cols, "^[:digit:]+")),
      source_name = stringr::str_extract(amt_cols, "[^[:digit:]]+"),
      source_name = stringr::str_remove(source_name, "-$"),
      source_name = stringr::str_trim(source_name),
      amt_cols = if_else(
        !is.na(source_code),
        stringr::str_remove(amt_cols, as.character(source_code)),
        amt_cols
      ),
      amt_cols = str_remove(amt_cols, fixed(source_name)),
      # Make source_name consistent across years (check this)
      source_name = if_else(
        str_detect(
          source_name,
          "^Urban Development Action Grant"
        ),
        "Urban Development Action Grant (UDAG) Repayments",
        source_name
      ),
      amt_cols = stringr::str_squish(amt_cols),
      amt_type = dplyr::if_else(
        source_name == "Total",
        "Total",
        "Source"
      )
    ) |>
    select(
      filename, cip_num, bureau_name, starts_with("source"), amt_type, amt_cols
    ) |>
    mutate(
      report_fy = as.integer(str_extract(filename, "^[:digit:]+"))
    ) |>
    separate_wider_delim(
      cols = amt_cols,
      names = c("Appr. To Date", paste0("fy", 1:7, "_amt")),
      too_few = "align_start",
      # too_many = "drop",
      delim = " "
    ) |>
    pivot_longer(
      starts_with("fy"),
      names_to = "offset_fy",
      names_transform = \(x){
        as.integer(str_extract(x, "[:digit:]+")) - 1
      }
    ) |>
    rename(
      appropriated_amt = `Appr. To Date`,
      boe_amt = value,
      cip_number = cip_num
    ) |>
    mutate(
      source_name = if_else(
        source_name == "Total",
        NA_character_,
        source_name
      ),
      fiscal_year = report_fy + offset_fy,
      cip_year_id = glue::glue("{cip_number}_{fiscal_year}")
    ) |>
    # Filter to just budget year data
    filter(
      fiscal_year == report_fy
    ) |>
    relocate(
      fiscal_year,
      cip_number,
      .before = everything()
    ) |>
    filter(
      !is.na(source_name)
    ) |>
    left_join(
      project_info |>
        select(!c(fiscal_year, cip_number, filename, bureau_name)),
      by = "cip_year_id"
    ) |>
    mutate(
      across(
        # Scale amounts to thousands
        ends_with("_amt"),
        \(x) {
          if_else(
            is.na(x),
            NA_real_,
            as.numeric(x) * 1000
          )
        }
      )
    )

  budget_report_totals <- budget_reports |>
    select(
      !c(
        source_code, source_name,
        amt_type, offset_fy,
        appropriated_amt, boe_amt
      )
    ) |>
    distinct() |>
    left_join(
      budget_reports |>
        summarise(
          across(
            c(
              appropriated_amt,
              boe_amt
            ),
            \(x) {
              sum(x, na.rm = TRUE)
            }
          ),
          amt_type = "Total",
          .by = cip_year_id
        ),
      by = join_by(cip_year_id)
    )

  list(
    budget_reports,
    budget_report_totals
  ) |>
    list_rbind() |>
    arrange(cip_year_id, amt_type)
}


# Export summary data
if (FALSE) {
  project_funding |>
    readr::write_csv(
      "CIP-BOE-Reports_2008-2013.csv"
    )
}

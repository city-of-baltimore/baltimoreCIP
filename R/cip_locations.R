library(tidyverse)

source(here::here("data-raw", "functions.R"))

budget_reports <-  readr::read_csv(
  here::here("files", "FY14-FY24_CIP-Requests_Source.csv"),
  show_col_types = FALSE
) |>
  filter(is.na(source_code)) |>
  select(
    !starts_with("source")
  ) |>
  select(
    !any_of(c("cip_year_id", "amt_type", "request_year_list"))
  )

# Load data for manual corrections to CIP location data
cip_location_updates <- tibble::tribble(
  ~cip_number, ~cip_number_updated, ~fiscal_year, ~notes,
  "520-014", "525-045", 2019L, NA,
  "520-015", "525-046", 2019L, NA,
  "520-016", "525-047", 2019L, NA,
  "520-019", "525-031", 2019L, NA,
  "520-027", "525-044", 2019L, NA,
  "520-028", "525-037", 2019L, NA,
  "520-035", "525-041", 2019L, NA,
  "520-036", "525-042", 2019L, NA,
  "520-040", "525-034", 2019L, NA,
  "520-044", "525-036", 2019L, NA,
  "525-708", "520-046", 2019L, NA,
  "509-402", NA, 2016L, "No match found in CIP budget reports",
  "197-049", NA, 2015L, "No match found in CIP budget reports",
  "509-402", NA, 2015L, "No match found in CIP budget reports",
  "607-011", NA, 2015L, "No match found in CIP budget reports",
  "607-011", NA, 2014L, "No match found in CIP budget reports"
)

# pak::pkg_install("elipousson/esri2sf")

# Get layers from AGOL CIP mapping application
cip_map_info <- esri2sf::esriitem(
  "https://www.arcgis.com/home/item.html?id=657321dbfda24a06bec5cc0d5e69ebfa"
)

# Combine list of URLs and remove duplicates
cip_map_layers <- purrr::list_rbind(
  cip_map_info$operationalLayers$layers
) |>
  distinct(url, .keep_all = TRUE)

# Get layers
cip_data_list <- purrr::map(
  cip_map_layers$url,
  \(x) {
    esri2sf::esri2sf(x, crs = 3857)
  }
)

# Get layer names from metadata
cip_map_layers_meta <- map(
  cip_map_layers$url,
    esri2sf::esrimeta
  )

# Set names for layer list
cip_data_list <- set_names(cip_data_list, map(cip_map_layers_meta, "name"))

# Combine list into single sf object
cip_location_layers_source <- cip_data_list |>
  list_rbind(names_to = "layer_name") |>
  sf::st_as_sf() |>
  rename(geometry = geoms) |>
  mutate(
    geom_type = sf::st_geometry_type(geometry)
  )

cip_location_layers_combined <- cip_location_layers_source |>
  mutate_trim_squish() |>
  naniar::replace_with_na(
    replace = list(
      Unique_Ide = "",
      Unique_Identifier = "",
      Name = "",
      Project_Ti = "",
      Project_Title = "",
      Location = "",
      `CIP__` = "",
      CIPNo = "",
      CIPno = "",
      New_or_Exi = c("", " "),
      New_or_Existing = c("", " ")
    )
  ) |>
  mutate(
    # Combine values across varied column names
    cip_number = coalesce(
      `CIP__`, CIPNo, CIPno
    ),
    project_title_map = coalesce(
      Name, Project_Ti, Project_Title
    ),
    location_map = Location,
    cip_year_id = coalesce(Unique_Ide, Unique_Identifier),
    new_request_flag = coalesce(New_or_Exi, New_or_Existing),
    # NOTE: Funding columns are dropped later in this pipeline
    # FIXME: funding columns are incomplete and use a non-standard grouping
    general_funds = coalesce(
      City_Gener, General_Fu, City_General_Funds
    ),
    bond_funds = coalesce(
      `City_Bond_`, City_Bond_Funds
    ),
    utility_funds = coalesce(Utility_Fu, Utility_Funds),
    county_grant_funds = coalesce(
      County_Gra, County_Grants
    ),
    state_funds = coalesce(
      State_Fund, State_Funds
    ),
    federal_funds = coalesce(
      Federal_Fu, Federal_Funds
    ),
    .before = everything()
  ) |>
  filter(
    # Remove duplicate 2021 locations from FY14-21 layer
    !(str_detect(layer_name, "FY14-21 Capital Improvement Projects") & (FY == 2021))
  )

cip_location_layers <- cip_location_layers_combined |>
  mutate(
    # Standardize new/existing request flag values (variable is dropped)
    new_request_flag = case_match(
      new_request_flag,
      "new?" ~ "New",
      "new" ~ "New",
      .default = new_request_flag
    ),
    # Fill missing fiscale year values
    fiscal_year = case_when(
      !is.na(FY) ~ as.character(FY),
      !is.na(cip_year_id) ~ str_extract(cip_year_id, "[:digit:]{4}$"),
      str_detect(layer_name, "^FY24") ~ "2024",
      str_detect(layer_name, "^FY23") ~ "2023",
      .default = NA_character_
    ),
    fiscal_year = as.integer(fiscal_year),
    cip_year_id = if_else(
      is.na(cip_year_id),
      paste0(cip_number, "_", fiscal_year),
      cip_year_id
    )
  ) |>
  left_join(
    cip_location_updates,
    na_matches = "never",
    by = join_by(fiscal_year, cip_number)
  ) |>
  mutate(
    cip_number = coalesce(
      cip_number_updated,
      cip_number
    )
  ) |>
  # FIXME: Consider excluding locations that can't be matched to a request record
  # filter(!is.na(notes)) |>
  select(
    all_of(
      c(
        "cip_number", "fiscal_year", "project_title_map", "cip_year_id",
        "location_map", "new_request_flag"
      )
    ),
    ends_with("_funds", ignore.case = FALSE),
    layer_name,
    geom_type
  ) |>
  arrange(desc(fiscal_year), cip_number, desc(geom_type))

if (FALSE) {
  # Check number of repreat cip_year_id values and project names
  cip_location_layers_summary <- cip_location_layers |>
    sf::st_drop_geometry() |>
    summarise(
      n_cip_year_id = n(),
      n_project_title = n_distinct(project_title_map),
      .by = cip_year_id
    )

  cip_location_layers <- cip_location_layers |>
    left_join(
      cip_location_layers_summary,
      relationship = "many-to-one",
      by = join_by(cip_year_id)
    )
}

cip_location_layers <- cip_location_layers |>
  select(!ends_with("_funds"))

# Join CIP locations to CIP Budget data
cip_locations <- cip_location_layers |>
  select(!new_request_flag) |>
  left_join(
    budget_reports,
    na_matches = "never",
    relationship = "many-to-one",
    by = join_by(fiscal_year, cip_number)
  ) |>
  relocate(
    fiscal_year,
    cip_number,
    cip_year_id
  )

if (FALSE) {
  # Check for unmatched location data
  cip_location_layers |>
    anti_join(
      budget_reports |>
        filter(is.na(source_code)),
      na_matches = "never",
      by = join_by(fiscal_year, cip_number)
    ) |>
    select(
      cip_number, cip_year_id, project_title_map, location_map
    )# |>
    # View()
    # mapview::mapview()
}

# Save GeoPackage file with CIP locations
cip_locations |>
  sf::write_sf(
    here::here(
      "files",
      "FY14-FY24_CIP-Requests_Locations.gpkg"
    )
    )


if (FALSE) {
  cip_locations <- sf::read_sf("FY14-FY24_CIP-Requests_Locations.gpkg")

  cip_locations |>
    getdata::make_variable_dictionary(details = "full") |>
    View()
}

# Save GeoPackage file with separate layers for different geometry types
dsn <- "FY14FY24_CIP-Requests_Locations-by-Geom.gpkg"

cip_locations |>
  mutate(
    # geometry_type = sf::st_geometry_type(data),
    geom_type = case_match(
      geom_type,
      "MULTIPOLYGON" ~ "Polygon",
      "POINT" ~ "Points",
      "MULTILINESTRING" ~ "Linear"
    )
  ) |>
  nest_by(geom_type) |>
  pwalk(
    \(geom_type, data) {
      sf::st_write(
        obj = data,
        dsn = here::here("files", dsn),
        layer = paste0(fs::path_ext_remove(dsn), " (", geom_type, ")")
      )
    }
  )



<!-- README.md is generated from README.Rmd. Please edit that file -->

# Baltimore City Capital Improvement Program (CIP) Data

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)

<!-- badges: end -->

The goal of baltimoreCIP is to combine legacy reports and spatial data
for the [Baltimore City Capital Improvement
Program](https://planning.baltimorecity.gov/) into a tidy data source
for internal and public use.

> [!TIP]
>
> This project is built using {targets} and {tarchetypes}: two R
> packages designed to support the reproducible analytical pipelines
> (RAPs). For more information on {targets}, see [The {targets} R
> package user manual](https://books.ropensci.org/targets/).

For questions, please contact Eli Pousson, Data Lead with the Baltimore
City Department of Planning, at eli.pousson@baltimorecity.gov{.email}.

## Organization

This project is organized around three main folders:

- `files`: Required files including Microsoft Excel and PDF reports
  downloaded from the Baltimore City Department of Planning website.
  Organized into sub-folder (`agency`, `budget`, and `program`) based on
  report type.
- `R`: Scripts and functions for processing the files into a tidy data
  format using the `{tidyverse}` family of packages, `{pdftools}`,
  `{sf}`, and other packages.
- `data`: Output data including combined CSV files for the 2008-2013
  reports (`FY08-FY13_CIP-Budgets.csv`), 2014-2024 reports
  (`FY14-FY24_CIP-Requests_Source.csv`), and a GeoJSON file
  (`FY14-FY24_CIP-Requests_Locations.geojson`) with the locations
  associated with that same period. As of July 2024, DOP staff are
  working to compile spatial data for the 2008-2013 period. The folder
  also includes a data dictionary for the 2014-2024 data
  (`FY14-FY24_CIP-Requests_Source_Dictionary.csv`).

## Background

The Capital Improvement Program is a six-year plan for funding capital
projects by City agencies. The program is updated and adopted each year
as part of the Baltimore City Budget.

Prior to FY2024, Baltimore City used a custom SQL database that allowed
limited reporting with Excel and PDF exports. These exported reports are
used in this data analysis pipeline to create a flat file of CIP
requests and approved capital budgets for agency research and reporting
needs.

It is important to note that data created based on these reports several
significant limitations:

- The budgeted amount and previously appropriated amounts do *not*
  account for transfers between accounts (transfers are reviewed and
  approved by the Planning Commission outside the CIP process)
- The year funds appear in the capital budget is often not the same year
  funds are spent. Due to constraints on the size of the capital budget,
  agencies have often needed to set aside funding over a multi-year
  period to fully fund a large scale capital project.
- The number and amount of the requested funds and approved capital
  budget is contingent on the availability of funding from local, state,
  and federal sources as well as agency-identified needs and
  commitments.
- Locations are identified by agencies as part of the initial capital
  requests submitted to the Department of Planning. These prospective
  locations may include sidewalks, alleys, or buildings where work may
  or may not have been completed if the budgeted capital funding could
  not cover the project’s full scope and cost.

There are also some differences between the 2008-2013 data and the
2014-2024 data. The earlier period is based on program reports listing
budgeted and programmed funding by source and fiscal year at the point
of Board of Estimates review and approval. The later data includes both
the earlier stages of agency requests and Planning Commission review and
the later stages of Ordinance of Estimates adoption.

Between 2023 and 2024, Baltimore City migrated contract and financial
management systems to a cloud-based application known as
[Workday](https://www.workday.com/). Data on the Capital Improvement
Program is now stored in both Workday and a second integrated
application, known as [Workday Adaptive
Planning](https://www.workday.com/en-us/products/adaptive-planning/overview.html)
(or Adaptive for short), that combines project information entered by
agency staff with financial data.

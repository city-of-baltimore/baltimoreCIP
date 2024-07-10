
<!-- README.md is generated from README.Rmd. Please edit that file -->

# baltimoreCIP

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
for the Baltimore City Capital Improvement Program into a tidy data
source for internal and public use.

This project is organized around three main folders:

- `files`: Required files including Microsoft Excel and PDF reports
  downloaded from the Baltimore City Department of Planning website.
  Organized into sub-folder (`agency`, `budget`, and `program`) based on
  report type.
- `R`: Scripts and functions for processing the files into a tidy data
  format.
- `data`: Output data including a combined CSV file for the 2014-2024
  reports, a GeoJSON file with the locations associated with that same
  period, and a CSV file for the 2008-2013 reports.

For questions, please contact Eli Pousson, Data Lead with the Baltimore
City Department of Planning, at <eli.pousson@baltimorecity.gov>{.email}.
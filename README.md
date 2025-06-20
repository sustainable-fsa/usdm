
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Static
Badge](https://img.shields.io/badge/Repo-sustainable--fsa%2Fusdm-magenta?style=flat)](https://github.com/sustainable-fsa/usdm/)
![Last
Update](https://img.shields.io/github/last-commit/sustainable-fsa/usdm?style=flat)
![Repo
Size](https://img.shields.io/github/repo-size/sustainable-fsa/usdm?style=flat)

This repository provides a reproducible, archival-quality pipeline to
download, validate, transform, and document weekly shapefiles from the
[US Drought Monitor (USDM)](https://droughtmonitor.unl.edu). The archive
is structured using the [BagIt 1.0
specification](https://tools.ietf.org/html/draft-kunze-bagit-14) and
includes:

- Raw weekly shapefiles and text summaries
- Cleaned, validated spatial data in GeoParquet format
- Per-week ISO 19115-1 metadata in XML
- Geometry validation logs
- Weekly updating and manifest tracking

<a href="https://sustainable-fsa.github.io/usdm/usdm.html" target="_blank">📂
View the US Drought Monitor archive listing here.</a>

The goal of this repository is to draft a regulatory-grade archive of
the US Drought Monitor that conforms with the *Foundations for
Evidence-Based Policymaking Act of 2018* (“Evidence Act”, [Public Law
115–435](https://www.congress.gov/115/statute/STATUTE-132/STATUTE-132-Pg5529.pdf)),
the *Geospatial Data Act of 2018* (enacted as part of [Public Law
115–254](https://www.congress.gov/115/statute/STATUTE-132/STATUTE-132-Pg3186.pdf)),
and [Executive Order 14303, *Restoring Gold Standard
Science*](https://www.federalregister.gov/documents/2025/05/29/2025-09802/restoring-gold-standard-science).
Given the regulatory role played by the USDM (e.g., [7 CFR
1416.205](https://www.ecfr.gov/current/title-7/section-1416.205), [7 CFR
759.5](https://www.ecfr.gov/current/title-7/section-759.5)), it is
essential that an authoritative, well-documented, persistent, and
findable archive of the USDM be established by a Federal agency. This
work seeks to create a framework for such an archive.

------------------------------------------------------------------------

## 📈 About the US Drought Monitor (USDM)

The US Drought Monitor is a weekly map-based product that synthesizes
multiple drought indicators into a single national assessment. It is
produced by:

- National Drought Mitigation Center (NDMC)
- US Department of Agriculture (USDA)
- National Oceanic and Atmospheric Administration (NOAA)

Each weekly map represents a combination of data analysis and expert
interpretation.

> **Note**: This archive is maintained by the Montana Climate Office,
> but all analytical authorship of the USDM drought maps belongs to the
> named USDM authors.

------------------------------------------------------------------------

## 🗂 Directory Structure

The resulting BagIt-compliant archive includes:

``` text
repository-root/
  ├── LICENSE                 # License for the repository (MIT)
  ├── README.Rmd              # Repository documentation (RMarkdown)
  ├── README.md               # Repository documentation (this file)
  ├── example-1.png           # Example figure using the data
  ├── usdm.R                  # Code to download, process, and archive USDM data
  ├── usdm.Rproj              # RStudio project file
  ├── usdm.html               # An HTML directory listing of the USDM Archive
  ├── usdm-manifest.json      # The directory listing of the USDM Archive
  └── usdm/                   # BagIt-compliant archive of USDM weekly data
    ├── bagit.txt               # BagIt version declaration
    ├── bag-info.txt            # Metadata about the bag archive
    ├── manifest-sha256.txt     # Checksums for integrity verification
    └── data/
      ├── raw/                    # Downloaded shapefiles (.zip)
      ├── summary/                # Weekly summary XML files
      ├── parquet/                # Cleaned spatial data (.parquet)
      ├── metadata/               # ISO 19115 metadata XML files
      └── quality/
        └── geometry_validation.csv  # Log of geometry validation issues
```

------------------------------------------------------------------------

## 🧪 Analysis Pipeline

This R pipeline ([`usdm.R`](./usdm.R)):

1.  **Downloads** weekly USDM shapefiles and XML summaries.
2.  **Validates** geometries using the [S2 Geometry
    Library](https://s2geometry.io/) via `sf::st_is_valid()`, and logs
    invalid features for review.
3.  **Cleans and repairs** shapefile geometries and converts them to the
    [GeoParquet](https://geoparquet.org) format.
4.  **Writes ISO 19115-1 metadata** for each weekly dataset using the
    `geometa` package.
5.  **Builds a BagIt structure** with SHA-256 checksums to ensure
    archival integrity.

------------------------------------------------------------------------

## 🔁 Weekly Updating

The pipeline automatically determines the most recent USDM date
available and:

- Only downloads new or modified files.
- Uses file checksums to avoid unnecessary re-processing.
- Appends new validation issues to a persistent quality log.

Use the `usdm()` function to process a specific date, or
`usdm_get_dates()` to get all valid weekly dates.

------------------------------------------------------------------------

## 🛠️ Dependencies

Key R packages used:

- `sf`, `terra`, `arrow`, `tidyverse`, `curl`
- `geometa` for ISO metadata
- `digest` for checksum computation

The script installs all required packages using the
[`pak`](https://pak.r-lib.org) package.

------------------------------------------------------------------------

## 📍 Quick Start: Visualize a Weekly USDM Map in R

This snippet shows how to load a weekly GeoParquet file from the archive
and create a simple drought classification map using `sf` and `ggplot2`.

``` r
# Load required libraries
library(arrow)
library(sf)
library(ggplot2) # For plotting
library(tigris)  # For state boundaries
library(rmapshaper) # For innerlines function

## Get latest USDM data
latest <-
  jsonlite::fromJSON(
    "usdm-manifest.json"
    )$path |>
  stringr::str_subset("parquet") |>
  max()
# e.g., [1] "usdm/data/parquet/USDM_2025-05-27.parquet"

# Read a weekly GeoParquet file as an sf object
# Use tigris::shift_geometry to shift and rescale Alaska, Hawaii, and
# Puerto Rico in a US-wide sf object
usdm_sf <- 
  latest |>
  sf::read_sf() |>
  # tigris::shift_geometry only works consistently on POLYGON geometries
  sf::st_cast("POLYGON", warn = FALSE, do_split = TRUE) |> # 
  tigris::shift_geometry()

states <- 
  tigris::states(cb = TRUE, 
                 resolution = "5m",
                 progress_bar = FALSE) |>
  dplyr::filter(
    !(NAME %in% c("Guam", 
                  "American Samoa", 
                  "United States Virgin Islands", 
                  "Commonwealth of the Northern Mariana Islands"))
  ) |>
  sf::st_cast("POLYGON", warn = FALSE, do_split = TRUE) |>
  tigris::shift_geometry()

# Plot the map
ggplot(usdm_sf) +
  geom_sf(data = sf::st_union(states),
          fill = "grey80",
          color = NA) +
  geom_sf(aes(fill = usdm_class), 
          color = "white",
          linewidth = 0.1) +
  geom_sf(data = rmapshaper::ms_innerlines(states),
          fill = NA,
          color = "white",
          linewidth = 0.2) +
  scale_fill_manual(
    values = c("#ffff00",
               "#fcd37f",
               "#ffaa00",
               "#e60000",
               "#730000"),
    drop = FALSE,
    name = "Drought\nClass") +
  labs(title = "US Drought Monitor",
       subtitle = format(usdm_sf$date[[1]], " %B %d, %Y")) +
  theme_void()
```

<img src="./example-1.png" style="display: block; margin: auto;" />

------------------------------------------------------------------------

## 📝 Citation & Attribution

**Citation format** (suggested):

> US Drought Monitor authors. *US Drought Monitor Weekly Maps*. Data
> curated and archived by R. Kyle Bocinsky, Montana Climate Office.
> Accessed via GitHub archive, YYYY.
> <https://sustainable-fsa.github.io/usdm/>

**Acknowledgments**:

- Map content by USDM authors.
- Data curation and archival structure by R. Kyle Bocinsky, Montana
  Climate Office, University of Montana.

------------------------------------------------------------------------

## 📄 License

- **Raw USDM data** (NDMC): Public Domain (17 USC § 105)
- **Processed data & scripts**: © R. Kyle Bocinsky, released under
  [CC0](https://creativecommons.org/publicdomain/zero/1.0/) and [MIT
  License](./LICENSE) as applicable

------------------------------------------------------------------------

## ⚠️ Disclaimer

This dataset is archived for research and educational use only. The
National Drought Mitigation Center hosts the US Drought Monitor. Please
visit <https://droughtmonitor.unl.edu>.

------------------------------------------------------------------------

## 👏 Acknowledgment

This project is part of:

**[*Enhancing Sustainable Disaster Relief in FSA
Programs*](https://www.ars.usda.gov/research/project/?accnNo=444612)**  
Supported by USDA OCE/OEEP and USDA Climate Hubs  
Prepared by the [Montana Climate Office](https://climate.umt.edu)

------------------------------------------------------------------------

## 📬 Contact

**R. Kyle Bocinsky**  
Director of Climate Extension  
Montana Climate Office  
📧 <kyle.bocinsky@umontana.edu>  
🌐 <https://climate.umt.edu>

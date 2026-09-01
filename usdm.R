# update.packages(repos = "https://cran.rstudio.com/",
#                 ask = FALSE)
# 
# install.packages("pak",
#                  repos = "https://mac.r-project.org")
# 
# options("pkg.cran_mirror" = "https://mac.r-project.org")
# 
# # installed.packages() |>
# #   rownames() |>
# #   pak::pkg_install(upgrade = TRUE,
# #                  ask = FALSE)
# 
# pak::pak(
#   c(
#     "arrow?source",
#     "sf?source",
#     "curl",
#     "tidyverse",
#     "digest",
#     "geometa",
#     "fs",
#     "xml2",
#     "jsonlite",
#     "tigris",
#     "rmapshaper",
#     "furrr",
#     "future.mirai"
#   )
# )

library(magrittr)
library(tidyverse)
library(xml2)
library(sf)
library(arrow)
library(geometa)
library(fs)
library(furrr)
library(future.mirai)

sf::sf_use_s2(TRUE)

usdm_get_dates <-
  function(as_of = lubridate::today("America/Denver")){
    as_of %<>%
      lubridate::as_date()
    
    usdm_dates <-
      seq(lubridate::as_date("20000104"), lubridate::today(), "1 week")
    
    usdm_dates <- usdm_dates[(as_of - usdm_dates) >= 2]
    
    return(usdm_dates)
  }

bag_dir = file.path("usdm")
directories <-
  list(
    bag_dir = bag_dir,
    raw_dir = file.path(bag_dir, "data", "raw"),
    raw_unclipped_dir = file.path(bag_dir, "data", "raw_unclipped"),
    summary_dir = file.path(bag_dir, "data", "summary"),
    parquet_dir = file.path(bag_dir, "data", "parquet"),
    parquet_unclipped_dir = file.path(bag_dir, "data", "parquet_unclipped"),
    metadata_dir = file.path(bag_dir, "data", "metadata"),
    metadata_unclipped_dir = file.path(bag_dir, "data", "metadata_unclipped"),
    quality_dir = file.path(bag_dir, "data", "quality")
  )

directories %>%
  purrr::walk(dir.create,
              recursive = TRUE,
              showWarnings = FALSE)

## ---- S3 archive state -------------------------------------------------
## The archive of record is s3://sustainable-fsa/usdm/ (the inner bag maps
## to the prefix root, so keys look like usdm/data/parquet/USDM_*.parquet).
## Membership in the S3 listing replaces the old local-file + bagit-hash
## guards; the local bag dir is only a staging area for NEW files.
source("R/s3-archive.R")
s3_preflight()
s3_bucket_name <- Sys.getenv("S3_BUCKET", unset = "sustainable-fsa")
s3_prefix      <- Sys.getenv("S3_PREFIX", unset = "usdm")

archived_rel <-
  s3_list_keys(s3_bucket_name, s3_prefix)$Key %>%
  stringr::str_remove(paste0("^", s3_prefix, "/"))

## Pull the small stateful archive files that this run appends to
c("manifest-sha256.txt",
  "data/quality/geometry_validation.csv",
  "data/quality/geometry_validation_unclipped.csv",
  "data/quality/raw_unclipped_sources.csv") %>%
  purrr::keep(~ .x %in% archived_rel) %>%
  purrr::walk(\(f){
    s3_run(c("s3", "cp",
             paste0("s3://", s3_bucket_name, "/", s3_prefix, "/", f),
             file.path(bag_dir, f)),
           echo = FALSE)
  })

# read the manifest, if it exists
if(file.exists(file.path(directories$bag_dir, "manifest-sha256.txt"))){
  manifest <-
    readr::read_table(
      file.path(directories$bag_dir, "manifest-sha256.txt"),
      col_names = c("hash", "file")
    )
}

## The unclipped ("shapefiles_r") weekly zips live in two IIS directory
## listings: roughly the trailing year at the shapefiles_r root, and all
## older weeks under Archive/. Scrape both listings once per run to learn
## each week's verbatim filename (the listings mix usdm_/USDM_ case),
## source URL, size, and upstream modification time (listing times are US
## Central); prefer the Archive/ copy when a week appears in both. The
## index doubles as the unclipped freshness gate: a week absent from it
## has not been posted anywhere yet.
usdm_unclipped_index <-
  function(){
    base <- "https://droughtmonitor.unl.edu"

    index <-
      c(root = "/data/shapefiles_r/",
        archive = "/data/shapefiles_r/Archive/") %>%
      purrr::imap(\(path, src){
        listing <- NULL
        for(i in 1:3){
          listing <-
            tryCatch({
              res <- curl::curl_fetch_memory(paste0(base, path))
              stopifnot(res$status_code == 200L)
              rawToChar(res$content)
            },
            error = function(e) NULL)
          if(!is.null(listing)) break
          Sys.sleep(5)
        }

        if(is.null(listing)){
          gate_skip(paste0("Could not read the NDMC unclipped listing at ",
                           base, path,
                           "; unclipped data may be skipped this run."))
          return(NULL)
        }

        listing %>%
          stringr::str_split_1("<br>") %>%
          stringr::str_subset(
            stringr::regex("HREF=\"[^\"]*/usdm_\\d{8}\\.zip\"",
                           ignore_case = TRUE)) %>%
          stringr::str_match(
            stringr::regex(paste0("([0-9/]+\\s+[0-9:]+\\s+[AP]M)\\s+(\\d+)\\s+",
                                  "<A HREF=\"([^\"]+)\">([^<]+)</A>"),
                           ignore_case = TRUE)) %>%
          tibble::as_tibble(.name_repair =
                              ~ c("entry", "modified", "size", "href", "file")) %>%
          dplyr::transmute(
            date = lubridate::as_date(stringr::str_extract(file, "\\d{8}")),
            file,
            url = paste0(base, href),
            upstream_modified =
              lubridate::parse_date_time(modified, orders = "mdY IMp",
                                         tz = "America/Chicago"),
            size = as.numeric(size),
            source = src
          )
      }) %>%
      purrr::compact() %>%
      dplyr::bind_rows()

    if(nrow(index) == 0)
      return(tibble::tibble(date = lubridate::as_date(character(0)),
                            file = character(0),
                            url = character(0),
                            upstream_modified = lubridate::as_datetime(character(0)),
                            size = numeric(0),
                            source = character(0)))

    index %>%
      dplyr::arrange(date, dplyr::desc(source == "archive")) %>%
      dplyr::distinct(date, .keep_all = TRUE)
  }

unclipped_index <- usdm_unclipped_index()

usdm_download_raw <-
  function(x = "2000-01-04",
           clipped = TRUE,
           raw_dir = if(clipped) directories$raw_dir
                     else directories$raw_unclipped_dir,
           src = NULL,
           sources_file = file.path(directories$quality_dir,
                                    "raw_unclipped_sources.csv")){

    if(clipped){
      usdm_file <-
        x %>%
        lubridate::as_date() %>%
        format("%Y%m%d") %>%
        paste0("https://droughtmonitor.unl.edu/data/shapefiles_m/USDM_",.,"_M.zip")

      outfile <-
        file.path(raw_dir,
                  basename(usdm_file))

      if(!file.exists(outfile)){
        out <-
          usdm_file %>%
          curl::multi_download(urls = .,
                               destfiles =
                                 outfile,
                               resume = TRUE)
      }

      return(outfile)
    }

    ## Unclipped zips are archived byte-for-byte under their verbatim
    ## upstream filenames. curl preserves the upstream Last-Modified as the
    ## local file mtime; because S3 does not retain file times, each
    ## download is also recorded in data/quality/raw_unclipped_sources.csv.
    if(is.null(src))
      src <- dplyr::filter(unclipped_index, date == lubridate::as_date(x))
    if(nrow(src) == 0)
      stop("No unclipped source listed for ", x)

    outfile <-
      file.path(raw_dir, src$file)

    ## A killed run can leave a truncated zip that would pass a bare
    ## file.exists check; the listing's size is authoritative, so
    ## re-download whenever the staged size disagrees.
    if(file.exists(outfile) && file.size(outfile) != src$size)
      unlink(outfile)

    if(!file.exists(outfile)){
      out <-
        curl::multi_download(urls = src$url,
                             destfiles = outfile,
                             resume = TRUE)

      if(!isTRUE(out$success) ||
         !file.exists(outfile) ||
         file.size(outfile) != src$size)
        stop("Download of ", src$url, " failed or is incomplete.")

      provenance <-
        tibble::tibble(
          date = lubridate::as_date(x),
          file = src$file,
          source_url = src$url,
          upstream_modified =
            format(dplyr::coalesce(out$modified, src$upstream_modified),
                   "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
          size = file.size(outfile),
          downloaded = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
        )

      if(!file.exists(sources_file)){
        readr::write_excel_csv(provenance, sources_file)
      } else {
        readr::write_excel_csv(provenance, sources_file, append = TRUE)
      }
    }

    return(outfile)

  }

usdm_download_summary <-
  function(x,
           summary_dir = directories$summary_dir){
    usdm_file <-
      x %>%
      lubridate::as_date() %>%
      format("%Y%m%d") %>%
      paste0("https://droughtmonitor.unl.edu/services/data/summary/xml/usdm_summary_",.,".xml")
    
    outfile <-
      file.path(summary_dir, 
                basename(usdm_file))
    
    if(!file.exists(outfile)){
      out <-
        usdm_file %>%
        curl::multi_download(urls = .,
                             destfiles = 
                               outfile,
                             resume = TRUE)
    }
    
    return(outfile)
  }

usdm_log_validity <-
  function(x){
    current_s2 <- sf::sf_use_s2()
    suppressMessages(sf::sf_use_s2(TRUE)) 
    
    validity_log <- 
      x %>%
      dplyr::mutate(
        valid = st_is_valid(x),
        reason = st_is_valid(x, reason = TRUE)
      ) %>%
      dplyr::filter(!valid) %>%
      sf::st_drop_geometry()
    
    suppressMessages(sf::sf_use_s2(current_s2))
    
    return(validity_log)
    
  }

usdm_clean_summary <-
  function(x = paste0(directories$summary_dir, "usdm_summary_20240528.xml")){
    meta <-
      readr::read_file(x) %>%
      str_replace_all("(?<![a-zA-Z])<(?=\\d)", "&lt;") %>%
      str_replace_all("(?<![a-zA-Z])>(?=\\d)", "&gt;") %>%
      str_replace_all(" < ", " &lt; ") %>%
      str_replace_all(" > ", " &gt; ") %>%
      str_replace_all("< ", "&lt; ") %>%
      str_replace_all(" >", " &gt;") %>%
      str_replace_all("&", "&amp;") %>%
      xml2::read_xml() %>%
      xml2::as_list() %$%
      Results %$%
      week
    
    meta$dates <- 
      list(
        inForce = basename(x) %>%
          stringr::str_remove_all("usdm_summary_|.xml") %>%
          lubridate::as_date(),
        released = lubridate::as_date(unlist(meta$date))
      )
    
    meta$intro %<>%
      unlist(use.names = FALSE) %>%
      paste(collapse = "\n")
    
    meta$regions <-
      meta[names(meta) == "region"] %>% 
      magrittr::set_names(., purrr::map_chr(., \(x){attr(x, "name")})) %>%
      purrr::map(
        \(x){
          paste(unlist(x, use.names = FALSE), collapse = "\n")
        })
    meta[names(meta) == "region"] <- NULL
    
    meta$forecast %<>%
      unlist(use.names = FALSE) %>%
      paste(collapse = "\n")
    
    meta$authors <-
      meta[names(meta) == "author"] %>%
      purrr::map(\(x){unlist(x, recursive = FALSE)}) %>%
      magrittr::set_names(NULL)
    meta[names(meta) == "author"] <- NULL
    
    meta %<>%
      magrittr::extract(c("dates", "intro", "regions", "forecast", "disclaimer", "authors"))
    
    return(meta)
  }

usdm_write_metadata <-
  function(parquet = file.path(directories$parquet_dir, "USDM_2000-01-04.parquet"),
           summary = file.path(directories$summary_dir, "usdm_summary_20000104.xml"),
           clipped = TRUE,
           metadata_dir = if(clipped) directories$metadata_dir
                          else directories$metadata_unclipped_dir){

    usdm_date <-
      basename(parquet) %>%
      stringr::str_remove_all("USDM_|.parquet") %>%
      lubridate::as_date()

    ## The outfile basename is shared between products; the directory and
    ## the file identifier carry the product distinction.
    product_id <-
      paste0("USDM_", if(!clipped) "unclipped_", usdm_date)

    outfile <-
      file.path(metadata_dir,
                paste0("USDM_", usdm_date, ".xml"))

    if(!file.exists(outfile)){

      summary %<>%
        usdm_clean_summary()

      parquet_sf <- sf::read_sf(parquet)

      # ---- Write file-specific ISO 19115 XML to metadata/ ----
      # Create the metadata object
      md <- ISOMetadata$new()
      md$setFileIdentifier(product_id)
      md$setLanguage("eng")
      md$setCharacterSet("utf8")
      md$setDateStamp(Sys.Date())
      md$setMetadataStandardName("ISO 19115:2003/19139")
      md$setMetadataStandardVersion("1.0")
      
      # Create the citation
      citation <- ISOCitation$new()
      citation$setTitle(paste0("US Drought Monitor",
                               if(!clipped) " (unclipped)",
                               " — ",
                               lubridate::stamp("March 1, 1999", quiet = TRUE)(usdm_date)))
      citation$addAlternateTitle(product_id)
      citation$addDate(ISODate$new(date = summary$dates$inForce, dateType = "inForce"))
      citation$addDate(ISODate$new(date = summary$dates$released, dateType = "released"))
      
      #data identification
      ident <- ISODataIdentification$new()
      ident$setAbstract(
        paste0(
          c("",
            paste0("# US Drought Monitor",
                   if(!clipped) " (unclipped)",
                   " — ",
                   lubridate::stamp("March 1, 1999", quiet = TRUE)(usdm_date)),
            if(!clipped)
              paste("## Product",
                    paste0("Generalized US Drought Monitor drought-class polygons that have NOT been masked (clipped) to the US coastline or territorial boundaries; polygons extend beyond shorelines as delineated by the USDM authors. ",
                           "This is the unclipped counterpart of the masked ('M') product archived under data/parquet/; the drought classification is identical — only the coastal/boundary masking differs. ",
                           "Source: https://droughtmonitor.unl.edu/data/shapefiles_r/ (recent weeks) and https://droughtmonitor.unl.edu/data/shapefiles_r/Archive/ (older weeks)."),
                    sep = "\n"),
            paste("## Introduction", summary$intro, sep = "\n"),
            paste0(
              paste(
                paste0("## ",names(summary$regions)), 
                summary$regions, 
                sep = "\n"),
              collapse = "\n\n"),
            paste("## Forecast", summary$forecast, sep = "\n"),
            paste("## Disclaimer", summary$disclaimer, sep = "\n"),
            ""
          ),
          collapse = "\n\n"
        )
      )
      ident$addCredit("The US Drought Monitor is produced through a partnership between the National Drought Mitigation Center at the University of Nebraska-Lincoln, the United States Department of Agriculture and the National Oceanic and Atmospheric Administration.")
      ident$addStatus("completed")
      ident$addLanguage("eng")
      ident$addCharacterSet("utf8")
      ident$addTopicCategory("climatologyMeteorologyAtmosphere")
      ident$addTopicCategory("farming")
      ident$addTopicCategory("environment")
      ident$setCitation(citation)
      
      #maintenance information
      mi <- ISOMaintenanceInformation$new()
      mi$setMaintenanceFrequency("weekly")
      ident$addResourceMaintenance(mi)
      
      for(author in summary$authors){
        rp <- ISOResponsibleParty$new()
        rp$setIndividualName(author$name)
        rp$setOrganisationName(author$affiliation)
        rp$setRole("author")
        contact <- ISOContact$new()
        res <- ISOOnlineResource$new()
        res$setLinkage("https://droughtmonitor.unl.edu")
        res$setName("US Drought Monitor website")
        contact$setOnlineResource(res)
        rp$setContactInfo(contact)
        md$addContact(rp)
      }
      
      rp <- ISOResponsibleParty$new()
      rp$setIndividualName("R. Kyle Bocinsky")
      rp$setOrganisationName("Montana Climate Office")
      rp$setPositionName("Director of Climate Extension")
      rp$setRole("distributor")
      contact <- ISOContact$new()
      address <- ISOAddress$new()
      address$setEmail("kyle.bocinsky@umontana.edu")
      contact$setAddress(address)
      res <- ISOOnlineResource$new()
      res$setLinkage("https://climate.umt.edu")
      res$setName("Montana Climate Office website")
      contact$setOnlineResource(res)
      rp$setContactInfo(contact)
      md$addContact(rp)
      
      #ReferenceSystem
      rs <- ISOReferenceSystem$new()
      rsId <- ISOReferenceIdentifier$new(code = "4326", codeSpace = "EPSG")
      rs$setReferenceSystemIdentifier(rsId)
      md$addReferenceSystemInfo(rs)
      
      # Create geographic extent
      bbox <- st_bbox(parquet_sf)
      geo_bb <- ISOGeographicBoundingBox$new(
        minx = bbox["xmin"],
        maxx = bbox["xmax"],
        miny = bbox["ymin"],
        maxy = bbox["ymax"]
      )
      extent <- ISOExtent$new()
      extent$addGeographicElement(geo_bb)
      ident$addExtent(extent)
      
      #spatial representation type
      ident$addSpatialRepresentationType("vector")
      
      md$addIdentificationInfo(ident)
      
      # Create distribution info
      dist <- ISODistribution$new()
      fmt <- ISOFormat$new()
      fmt$setName("GeoParquet")
      fmt$setVersion("1.0")
      dist$addFormat(fmt)
      md$setDistributionInfo(dist)

      if(!clipped){
        # Lineage: how the unclipped product differs from the masked one
        dq <- ISODataQuality$new()
        scope <- ISODataQualityScope$new()
        scope$setLevel("dataset")
        dq$setScope(scope)
        lineage <- ISOLineage$new()
        lineage$setStatement(
          paste0("Weekly unclipped US Drought Monitor shapefiles ",
                 "(Drought_Areas_US_D0-D4 cumulative drought-class layers, ",
                 "not masked to the US coastline or territorial boundaries) ",
                 "were downloaded verbatim from the National Drought Mitigation Center ",
                 "(https://droughtmonitor.unl.edu/data/shapefiles_r/ and ",
                 "https://droughtmonitor.unl.edu/data/shapefiles_r/Archive/), merged, ",
                 "cleaned with mapshaper (overlapping cumulative classes resolved to the ",
                 "highest drought class and dissolved by class), reprojected to WGS84, ",
                 "and written as GeoParquet.")
        )
        dq$setLineage(lineage)
        md$addDataQualityInfo(dq)
      }

      # Encode to XML
      md$save(outfile)
      
    }
    
    return(outfile)
  }

## mapshaper cleans in the plane, so its planar-valid output can still be
## invalid on the sphere, where edges become great circles (hairline loop
## crossings and near-degenerate slivers — first seen in unclipped weeks
## 2005-05-03 and 2000-02-01). Never use s2_rebuild here: ingesting a
## self-crossing loop can invert regions and change areas by >20%.
## Instead, repair only the invalid features with gentle, area-preserving
## steps — densifying along the planar edges (so the great circles hug
## the original lines) and planar snap-rounding — escalating until the
## features are valid under both the spherical and planar interpretations
## (observed area changes < 0.01%). Weeks these steps cannot repair still
## stop the run at the validity assertions in usdm_process_raw.
usdm_repair_geometry <-
  function(geom){

    densify <- function(g, max_len){
      crs <- sf::st_crs(g)
      sf::st_crs(g) <- NA
      g <- sf::st_segmentize(g, max_len)
      sf::st_crs(g) <- crs
      g
    }

    snap <- function(g, precision){
      current_s2 <- sf::sf_use_s2()
      suppressMessages(sf::sf_use_s2(FALSE))
      on.exit(suppressMessages(sf::sf_use_s2(current_s2)), add = TRUE)
      g <- sf::st_make_valid(sf::st_set_precision(g, precision))
      gc <- sf::st_geometry_type(g) == "GEOMETRYCOLLECTION"
      if(any(gc)) g[gc] <- sf::st_collection_extract(g[gc], "POLYGON")
      sf::st_cast(g, "MULTIPOLYGON")
    }

    flag_invalid <- function(g){
      invalid <- !sf::st_is_valid(g)
      invalid[is.na(invalid)] <- TRUE
      invalid
    }

    ## spherical validity (s2 is on), escalating repairs
    strategies <-
      list(\(g) densify(g, 0.05),
           \(g) snap(g, 1e5),
           \(g) densify(g, 0.01),
           \(g) snap(g, 1e4))
    for(strategy in strategies){
      invalid <- flag_invalid(geom)
      if(!any(invalid)) break
      geom[invalid] <- strategy(geom[invalid])
    }

    ## planar validity for the same geometries
    current_s2 <- sf::sf_use_s2()
    suppressMessages(sf::sf_use_s2(FALSE))
    invalid <- flag_invalid(geom)
    if(any(invalid))
      geom[invalid] <- snap(geom[invalid], 1e7)
    suppressMessages(sf::sf_use_s2(current_s2))

    geom
  }

usdm_process_raw <-
  function(x = usdm_download_raw("2017-03-28"),
           clipped = TRUE,
           parquet_dir = if(clipped) directories$parquet_dir
                         else directories$parquet_unclipped_dir,
           quality_file = file.path(directories$quality_dir,
                                    if(clipped) "geometry_validation.csv"
                                    else "geometry_validation_unclipped.csv"),
           force.redo = FALSE){

    usdm_date <-
      basename(x) %>%
      stringr::str_extract("\\d{8}") %>%
      lubridate::as_date()

    outfile <-
      file.path(parquet_dir,
                paste0("USDM_", usdm_date, ".parquet"))

    if(!file.exists(outfile)){
      raw_sf <-
        if(clipped){
          file.path("/vsizip", x) %>%
            sf::read_sf()
        } else {
          ## The unclipped zips carry one shapefile per drought class
          ## (Drought_Areas_US_D0-D4, cumulative: the D0 layer contains D1,
          ## and so on), plus cartographic Drought_Impacts_* layers that are
          ## not data. Merge the class layers; the mapshaper
          ## overlap-rule=max-id step below resolves the cumulative overlaps
          ## to the highest class. OBJECTID is only unique within each
          ## source layer; DM disambiguates in the validity log.
          dsn <- file.path("/vsizip", x)
          class_layers <-
            sf::st_layers(dsn)$name %>%
            stringr::str_subset("^Drought_Areas_US_D[0-4]$")
          if(length(class_layers) == 0)
            stop("No Drought_Areas_US_D* layers found in ", x)
          class_layers %>%
            purrr::map(\(l) sf::read_sf(dsn, layer = l)) %>%
            purrr::keep(\(d) nrow(d) > 0) %>%
            purrr::map(\(d) dplyr::transmute(d,
                                             OBJECTID = as.integer(OBJECTID),
                                             DM = as.integer(DM))) %>%
            dplyr::bind_rows()
        }

      validity_log <-
        usdm_log_validity(raw_sf) %>%
        dplyr::mutate(date = usdm_date,
                      file = as.character(fs::path_rel(x, start = getwd()))) %>%
        dplyr::transmute(date, file, 
                         OBJECTID = as.integer(OBJECTID),
                         DM = as.integer(DM), valid, reason)
      
      # Check if the file exists
      if (!file.exists(quality_file)) {
        # Write with header
        readr::write_excel_csv(validity_log,
                               quality_file)
      } else {
        # Append without writing the header
        readr::write_excel_csv(validity_log, 
                               quality_file, 
                               append = TRUE)
      }
      
      gjson_temp <-
        tempfile(fileext = ".geojson")
      
      raw_sf %>%
        dplyr::transmute(usdm_class = factor(paste0("D", DM),
                                             levels = c("None", paste0("D", 0:4)),
                                             ordered = TRUE)) %>%
        dplyr::arrange(usdm_class) %>%
        sf::write_sf(gjson_temp,
                     delete_dsn = TRUE)
      
      rmapshaper::apply_mapshaper_commands(
        gjson_temp, 
        command = 
          paste(
            "-clean rewind overlap-rule=max-id -rename-layers usdm_class",
            "-dissolve field=usdm_class",
            "-o format=topojson no-quantization id-field='usdm_class' target=*", 
            stringr::str_replace(gjson_temp,  "geojson", "topojson")
          ),
        force_FC = TRUE,
        sys = TRUE,
        quiet = TRUE
      ) %>%
        sf::read_sf(crs = sf::st_crs(raw_sf)) %>%
        dplyr::transmute(usdm_class = factor(usdm_class,
                                             levels = c("None", paste0("D", 0:4)),
                                             ordered = TRUE)) %>%
        dplyr::mutate(date = usdm_date) %>%
        dplyr::select(date, usdm_class) %>%
        dplyr::arrange(date, usdm_class) %>%
        sf::st_transform("WGS84") ->
        cleaned_sf

      ## Repair (only) features that are invalid on the sphere or in the
      ## plane; a no-op for valid weeks. See usdm_repair_geometry.
      sf::st_geometry(cleaned_sf) <-
        usdm_repair_geometry(sf::st_geometry(cleaned_sf))

      cleaned_sf %>%
        sf::write_sf(
          outfile,
          driver = "Parquet",
          layer_options = c("COMPRESSION=ZSTD",
                            "COMPRESSION_LEVEL=13"),
          delete_dsn = TRUE
        )
      
    }
    
    out <- 
      sf::read_sf(outfile)
    
    if(
      any(!sf::st_is_valid(out))
      
    )
      stop(outfile, " is invalid")
    
    suppressMessages(sf_use_s2(FALSE))
    if(
      any(!sf::st_is_valid(out))
      
    )
      stop(outfile, " is invalid")
    suppressMessages(sf_use_s2(TRUE))
    
    return(outfile)
  }

usdm <-
  function(x = "2000-01-04"){
    ## Skip weeks whose artifacts are all in the S3 archive already;
    ## anything else is (re)built locally and uploaded by the publish block.
    ## The clipped and unclipped products are gated independently: NDMC
    ## posts them through separate processes, and a missing week in one
    ## product must never block archiving the other. The shared narrative
    ## summary gates both, since each product's metadata is built from it.
    usdm_date <- lubridate::as_date(x)
    d8 <- format(usdm_date, "%Y%m%d")

    summary_rel <-
      file.path("data", "summary", paste0("usdm_summary_", d8, ".xml"))

    clipped_rel <-
      c(
        file.path("data", "raw", paste0("USDM_", d8, "_M.zip")),
        file.path("data", "parquet", paste0("USDM_", usdm_date, ".parquet")),
        file.path("data", "metadata", paste0("USDM_", usdm_date, ".xml"))
      )

    ## The unclipped raw zip is archived under its verbatim upstream name,
    ## whose case varies (usdm_/USDM_) and could change if NDMC moves a
    ## week from the shapefiles_r root into Archive/ — so match archived
    ## keys case-insensitively and never create a case-variant duplicate.
    archived_raw_unclipped <-
      stringr::str_subset(
        archived_rel,
        stringr::regex(paste0("^data/raw_unclipped/usdm_", d8, "\\.zip$"),
                       ignore_case = TRUE))
    unclipped_rel <-
      c(
        file.path("data", "parquet_unclipped",
                  paste0("USDM_", usdm_date, ".parquet")),
        file.path("data", "metadata_unclipped",
                  paste0("USDM_", usdm_date, ".xml"))
      )

    need_clipped <-
      !all(c(clipped_rel, summary_rel) %in% archived_rel)
    need_unclipped <-
      !(length(archived_raw_unclipped) > 0 &&
          all(c(unclipped_rel, summary_rel) %in% archived_rel))

    if(!need_clipped && !need_unclipped)
      return(NULL)

    ## Freshness gate: NDMC posts Thursdays ~8:30 ET; artifacts must exist
    ## before we download (a 404 body would corrupt the zip/xml reads). The
    ## summary feeds both products' metadata, so it gates the whole week.
    summary_url <-
      paste0("https://droughtmonitor.unl.edu/services/data/summary/xml/usdm_summary_",
             d8, ".xml")
    if(!url_exists(summary_url)){
      gate_skip(paste0("NDMC has not yet posted the USDM summary for ", usdm_date,
                       "; skipping this week."))
      return(NULL)
    }

    summary <-
      usdm_download_summary(x)

    out <- lst(summary)

    if(need_clipped){
      clipped_url <-
        paste0("https://droughtmonitor.unl.edu/data/shapefiles_m/USDM_",
               d8, "_M.zip")
      if(url_exists(clipped_url)){
        raw <- usdm_download_raw(x, clipped = TRUE)
        parquet <- usdm_process_raw(raw, clipped = TRUE)
        metadata <- usdm_write_metadata(parquet = parquet,
                                        summary = summary,
                                        clipped = TRUE)
        out$clipped <- lst(raw, parquet, metadata)
      } else {
        gate_skip(paste0("NDMC has not yet posted the clipped (_M) shapefile for ",
                         usdm_date, "."))
      }
    }

    if(need_unclipped){
      src <- dplyr::filter(unclipped_index, date == usdm_date)
      ## If the raw zip is already archived, reuse its exact archived name
      ## even if the week has since moved between the root and Archive/.
      if(nrow(src) == 1 && length(archived_raw_unclipped) > 0)
        src$file <- basename(archived_raw_unclipped[[1]])
      if(nrow(src) == 1){
        raw <- usdm_download_raw(x, clipped = FALSE, src = src)
        parquet <- usdm_process_raw(raw, clipped = FALSE)
        metadata <- usdm_write_metadata(parquet = parquet,
                                        summary = summary,
                                        clipped = FALSE)
        out$unclipped <- lst(raw, parquet, metadata)
      } else {
        gate_skip(paste0("NDMC has not posted an unclipped shapefile for ",
                         usdm_date,
                         " (checked the shapefiles_r root and Archive listings)."))
      }
    }

    return(out)

  }

plan(mirai_multisession)

output <- 
  usdm_get_dates() %>%
  magrittr::set_names(.,.) %>%
  furrr::future_map(usdm)

plan(sequential)

# ---- Write bagit.txt ----
writeLines(c(
  "BagIt-Version: 0.97",
  "Tag-File-Character-Encoding: UTF-8"
), file.path(bag_dir, "bagit.txt"))

# ---- Write bag-info.txt ----
writeLines(c(
  paste("Bag-Software-Agent:", "R USDM Archive BagIt Pipeline"),
  paste("Bagging-Date:", Sys.Date()),
  "Contact-Name: R. Kyle Bocinsky",
  "Contact-Email: kyle.bocinsky@umontana.edu",
  "Source-Organization: Montana Climate Office, University of Montana",
  "External-Description: Partitioned GeoParquet archive of US Drought Monitor weekly shapefiles (clipped and unclipped products) with ISO metadata"
), file.path(bag_dir, "bag-info.txt"))

# ---- Update manifest-sha256.txt with newly created files ----
# On a fresh (CI) runner, the local bag holds only this run's new files plus
# the pulled quality log; merge their hashes into the pulled manifest.
new_files <-
  list.files(file.path(bag_dir, "data"),
             recursive = TRUE,
             full.names = TRUE)

new_hashes <-
  tibble::tibble(
    file = gsub(paste0(bag_dir, "/"), "", new_files),
    hash = purrr::map_chr(new_files, digest::digest,
                          algo = "sha256", file = TRUE)
  )

manifest_updated <-
  {if (exists("manifest")) dplyr::anti_join(manifest, new_hashes, by = "file")
   else tibble::tibble(hash = character(0), file = character(0))} %>%
  dplyr::bind_rows(new_hashes) %>%
  dplyr::arrange(file)

writeLines(paste0(manifest_updated$hash, "  ", manifest_updated$file),
           file.path(bag_dir, "manifest-sha256.txt"))

## ---- Publish to S3 (append-only: never --delete) ----------------------
s3_push(s3_bucket_name, s3_prefix, bag_dir, delete = FALSE)
s3_verify(s3_bucket_name, s3_prefix, bag_dir,
          allow_extra = character(0),
          expect_exact = FALSE)

# ---- Regenerate usdm-manifest.json from the authoritative S3 listing ----
generate_tree_flat <- function(
    output_file = file.path("usdm-manifest.json")) {

  hashes <-
    manifest_updated %>%
    {magrittr::set_names(as.list(.$hash), .$file)}

  entries <-
    s3_list_keys(s3_bucket_name, s3_prefix) %>%
    dplyr::mutate(path = stringr::str_remove(Key, paste0("^", s3_prefix, "/"))) %>%
    dplyr::filter(!startsWith(path, "_")) %>%
    dplyr::arrange(path) %>%
    purrr::pmap(\(Key, Size, path){
      entry <- list(path = path, size = Size)
      if (!is.null(hashes[[path]])) entry$hash <- hashes[[path]]
      entry
    })

  jsonlite::write_json(entries, output_file, pretty = TRUE, auto_unbox = TRUE)
  message("✅ Wrote ", length(entries), " entries to ", output_file)
}

generate_tree_flat()

s3_put(s3_bucket_name,
       paste0(s3_prefix, "/usdm-manifest.json"),
       "usdm-manifest.json",
       content_type = "application/json",
       cache_control = "max-age=3600")

s3_write_manifest(s3_bucket_name, s3_prefix)

cf_invalidate(c(
  paste0("/", s3_prefix, "/manifest-sha256.txt"),
  paste0("/", s3_prefix, "/bag-info.txt"),
  paste0("/", s3_prefix, "/bagit.txt"),
  paste0("/", s3_prefix, "/usdm-manifest.json"),
  paste0("/", s3_prefix, "/_manifest.txt"),
  paste0("/", s3_prefix, "/data/quality/geometry_validation.csv"),
  paste0("/", s3_prefix, "/data/quality/geometry_validation_unclipped.csv"),
  paste0("/", s3_prefix, "/data/quality/raw_unclipped_sources.csv")
))

# ---- Render the README ----
# Regenerates README.md and the example map from the freshly updated
# archive; the workflow commits these (and only these) back to git.
cf_wait_manifest("https://data.sustainable-fsa.com/usdm/usdm-manifest.json",
                 "usdm-manifest.json")
rmarkdown::render("README.Rmd")

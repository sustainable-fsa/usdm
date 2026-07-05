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
    summary_dir = file.path(bag_dir, "data", "summary"),
    parquet_dir = file.path(bag_dir, "data", "parquet"),
    metadata_dir = file.path(bag_dir, "data", "metadata"),
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
c("manifest-sha256.txt", "data/quality/geometry_validation.csv") %>%
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

usdm_download_raw <-
  function(x = "2000-01-04",
           raw_dir = directories$raw_dir){
    
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
           metadata_dir = directories$metadata_dir){
    
    usdm_date <-
      basename(parquet) %>%
      stringr::str_remove_all("USDM_|.parquet") %>%
      lubridate::as_date()
    
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
      md$setFileIdentifier(paste0("USDM_", usdm_date))
      md$setLanguage("eng")
      md$setCharacterSet("utf8")
      md$setDateStamp(Sys.Date())
      md$setMetadataStandardName("ISO 19115:2003/19139")
      md$setMetadataStandardVersion("1.0")
      
      # Create the citation
      citation <- ISOCitation$new()
      citation$setTitle(paste0("US Drought Monitor — ", lubridate::stamp("March 1, 1999", quiet = TRUE)(usdm_date)))
      citation$addAlternateTitle(paste0("USDM_", usdm_date))
      citation$addDate(ISODate$new(date = summary$dates$inForce, dateType = "inForce"))
      citation$addDate(ISODate$new(date = summary$dates$released, dateType = "released"))
      
      #data identification
      ident <- ISODataIdentification$new()
      ident$setAbstract(
        paste0(
          c("",
            paste0("# US Drought Monitor — ", lubridate::stamp("March 1, 1999", quiet = TRUE)(usdm_date)),
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
      
      # Encode to XML
      md$save(outfile)
      
    }
    
    return(outfile)
  }

usdm_process_raw <-
  function(x = usdm_download_raw("2017-03-28"),
           parquet_dir = directories$parquet_dir,
           quality_file = file.path(directories$quality_dir, "geometry_validation.csv"),
           force.redo = FALSE){
    
    usdm_date <-
      basename(x) %>%
      stringr::str_remove_all("USDM_|_M.zip") %>%
      lubridate::as_date()
    
    outfile <-
      file.path(parquet_dir,
                paste0("USDM_", usdm_date, ".parquet"))
    
    if(!file.exists(outfile)){
      raw_sf <-
        file.path("/vsizip", x) %>%
        sf::read_sf()
      
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
        sf::st_transform("WGS84") %>%
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
    ## Skip weeks whose four artifacts are all in the S3 archive already;
    ## anything else is (re)built locally and uploaded by the publish block.
    usdm_date <- lubridate::as_date(x)
    date_rel <-
      c(
        file.path("data", "raw",
                  paste0("USDM_", format(usdm_date, "%Y%m%d"), "_M.zip")),
        file.path("data", "parquet",
                  paste0("USDM_", usdm_date, ".parquet")),
        file.path("data", "summary",
                  paste0("usdm_summary_", format(usdm_date, "%Y%m%d"), ".xml")),
        file.path("data", "metadata",
                  paste0("USDM_", usdm_date, ".xml"))
      )

    if(all(date_rel %in% archived_rel))
      return(NULL)

    raw <-
      usdm_download_raw(x)
    
    parquet <-
      usdm_process_raw(raw)
    
    summary = 
      usdm_download_summary(x)
    
    metadata <- 
      usdm_write_metadata(parquet = parquet,
                          summary = summary)
    
    return(lst(raw, parquet, summary, metadata))
    
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
  "External-Description: Partitioned GeoParquet archive of US Drought Monitor weekly shapefiles with ISO metadata"
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
  paste0("/", s3_prefix, "/data/quality/geometry_validation.csv")
))

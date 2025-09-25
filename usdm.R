# update.packages(repos = "https://cran.rstudio.com/",
#                 ask = FALSE)

install.packages("pak",
                 repos = "https://cran.rstudio.com/")

# installed.packages() |>
#   rownames() |>
#   pak::pkg_install(upgrade = TRUE,
#                  ask = FALSE)

pak::pak(
  c(
    "arrow?source",
    "sf?source",
    "curl",
    "tidyverse",
    "digest",
    "geometa",
    "fs",
    "xml2",
    "jsonlite",
    "tigris",
    "rmapshaper",
    "furrr",
    "future.mirai"
  )
)

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
  function(as_of = lubridate::today()){
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
    
    if(
      !(exists("manifest") && 
        file.exists(outfile) &&
        identical(digest::digest(outfile, algo = "sha256", file = TRUE), 
                  (
                    manifest %>% 
                    dplyr::filter(file == stringr::str_remove(outfile, "usdm/")) %$% 
                    hash
                  ))) ||
      !file.exists(outfile)
    ){
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
    
    if(
      !(exists("manifest") && 
        file.exists(outfile) &&
        identical(digest::digest(outfile, algo = "sha256", file = TRUE), 
                  (
                    manifest %>% 
                    dplyr::filter(file == stringr::str_remove(outfile, "usdm/")) %$% 
                    hash
                  ))) ||
      !file.exists(outfile)
    ){
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
    
    if(
      !(exists("manifest") && 
        file.exists(outfile) &&
        identical(digest::digest(outfile, algo = "sha256", file = TRUE), 
                  (
                    manifest %>% 
                    dplyr::filter(file == stringr::str_remove(outfile, "usdm/")) %$% 
                    hash
                  )) &&
        identical(digest::digest(parquet, algo = "sha256", file = TRUE), 
                  (
                    manifest %>% 
                    dplyr::filter(file == stringr::str_remove(parquet, "usdm/")) %$% 
                    hash
                  )) &&
        identical(digest::digest(summary, algo = "sha256", file = TRUE), 
                  (
                    manifest %>% 
                    dplyr::filter(file == stringr::str_remove(summary, "usdm/")) %$% 
                    hash
                  ))
      ) ||
      !file.exists(outfile)
    ){
      
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
    
    if(
      !(exists("manifest") && 
        file.exists(outfile) &&
        identical(digest::digest(outfile, algo = "sha256", file = TRUE), 
                  (
                    manifest %>% 
                    dplyr::filter(file == stringr::str_remove(outfile, "usdm/")) %$% 
                    hash
                  ))) ||
      !file.exists(outfile)
    ){
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
        sf::write_sf(outfile,
                     driver = "Parquet",
                     layer_options = c("COMPRESSION=ZSTD"))
      
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

# ---- Write manifest-sha256.txt ----
data_files <- list.files(file.path(bag_dir, "data"), recursive = TRUE, full.names = TRUE)
checksums <- sapply(data_files, function(f) {
  hash <- digest::digest(f, algo = "sha256", file = TRUE)
  rel_path <- gsub(paste0(bag_dir, "/"), "", f)
  paste0(hash, "  ", rel_path)
})
writeLines(checksums, file.path(bag_dir, "manifest-sha256.txt"))

generate_tree_flat <- function(
    bag_path = bag_dir, 
    manifest_file = file.path(bag_dir, "manifest-sha256.txt"), 
    output_file = file.path("usdm-manifest.json")) {
  bag_path <- fs::path_abs(bag_path)
  
  # Read manifest into a named vector: path -> checksum
  hashes <- list()
  if (!is.null(manifest_file)) {
    lines <- readLines(manifest_file)
    for (line in lines) {
      parts <- strsplit(line, " +")[[1]]
      if (length(parts) >= 2) {
        checksum <- parts[1]
        file <- gsub("^\\./", "", parts[length(parts)]) # remove leading ./ if present
        hashes[[file]] <- checksum
      }
    }
  }
  
  all_entries <- 
    fs::dir_ls(bag_path, recurse = TRUE, all = TRUE, type = "file") |>
    stringr::str_subset("(^|/)[.][^/]+", negate = TRUE)
  
  entries <- list()
  
  for (entry in all_entries) {
    rel_path <- fs::path_rel(entry, start = ".")
    info <- fs::file_info(entry)
    is_dir <- fs::is_dir(entry)
    entry_data <- list(
      path = as.character(rel_path),
      size = if (is_dir) "-" else info$size,
      mtime = if (is_dir) "-" else format(info$modification_time, "%Y-%Om-%d %H:%M:%S")
    )
    # Include checksum if available and not a directory
    if (!is_dir && !is.null(hashes[[as.character(rel_path)]])) {
      entry_data$hash <- hashes[[as.character(rel_path)]]
    }
    entries[[length(entries) + 1]] <- entry_data
  }
  
  # Sort by path
  entries <- entries[order(sapply(entries, function(x) x$path))]
  
  jsonlite::write_json(entries, output_file, pretty = TRUE, auto_unbox = TRUE)
  message("✅ Wrote ", length(entries), " entries to ", output_file)
}

# Generate the flat index
generate_tree_flat()

# Knit the readme
rmarkdown::render("README.Rmd")

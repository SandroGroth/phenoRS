getMODIS <- function(gSD_query, out_dir, use_aria=T, do_par=T, cores=NA) {

  logging::loginfo(paste0("Starting download of product(s) '", paste0(gSD_query$displayId, collapse = "', "), "'."))

  if (isTRUE(use_aria)) {
    # get download URLs from query, Source: getSpatialData
    url_files <- apply(gSD_query, MARGIN = 1, function(x) {
      laads_root = 'https://ladsweb.modaps.eosdis.nasa.gov/archive/allData/'
      y <- rbind.data.frame(x, stringsAsFactors = F)
      colnames(y) <- names(x)
      fn <- gsub("Entity ID: ", "", strsplit(y$summary, ", ")[[1]][1])
      ydoy <- gsub("A", "", strsplit(fn, "[.]")[[1]][2])
      paste0(laads_root, toString(as.numeric(strsplit(fn, "[.]")[[1]][4])), "/", strsplit(fn, "[.]")[[1]][1],
             "/", substr(ydoy, 1, 4), "/", substr(ydoy, 5, nchar(ydoy)), "/", fn)
    })

    # Check if aria2c  available
    if (nchar(unname(Sys.which("aria2c"))) == 0) {
      stop("aria2c.exe not found. Make sure it's correctly added to your PATH variable.")
    }
    logging::logdebug(paste0("aria2c found: ", unname(Sys.which("aria2c"))))

    # check if Login Data is available from getSpatialData
    if(is.null(options("gSD_usgs_user")) && is.null(options("gSD_usgs_passw"))) {
      stop("No Login credentials available. Try executing getSpatialData::loginUSGS() first.")
    }

    # Build temporary textfile for aria2c download
    f_path <- file.path(tempdir(), "MOD_URLs.txt")
    f <- file(f_path)
    writeLines(url_files, f)
    close(f)

    # Build aria2c command
    aria_string <- paste0(Sys.which("aria2c"),
                          " -i ", file.path(tempdir(), "MOD_URLs.txt"),
                          " -d ", out_dir,
                          " -x ", 6,
                          " --http-use=", options("gSD.usgs_user"),
                          " --http-passw=", options("gSD.usgs_passw"),
                          " --allow-overwrite",
                          " --retry-wait=", 2)

    # Execute bulk download using aria2c in seperate command window to monitor progress
    try(system(aria_string, intern = Sys.info()["sysname"] == "Windows", wait = FALSE, invisible = FALSE))
  } else {
    if(isTRUE(do_par)) {
      # set up parallel processing
      if (is.na(cores)) cores <- parallel::detectCores() - 1
      c1 <- parallel::makeCluster(cores)
      doParallel::registerDoParallel(c1)
      logging::logdebug(paste0("Set up parallel processing on ", cores, " cores"))

      # execute download
      foreach(i = 1:nrow(gSD_query[]),
              .combine=c,
              .packages='getSpatialData') %dopar% {
                getSpatialData::getMODIS_data(query[i, ], dir_out=out_dir)
              }

      parallel::stopCluster(c1)
    } else {
      getMODIS_data(query, dir_out=out_dir)
    }
  }
  logging::loginfo("Download finished.")
}

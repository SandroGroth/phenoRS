#' @title Download MODIS Data
#'
#' @description \code{downloadMODIS} downloads MODIS Vegetation Index data queried by
#'              \link[getSpatialData]{getMODIS_query}.
#'
#' @param gSD_query records data.frame. A valid query result returned by \link[getSpatialData]{getMODIS_query}.
#' @param out_dir out_dir character. Full path to download output directory.
#' @param use_aria optional logical. If True, aria2c is used for bulk downloading the selected images.
#'                 Requires valid aria2 installation. For help, see \url{https://aria2.github.io/}.
#'                 Default is \code{TRUE}
#' @param do_par optional logical. Whether parallel download should be applied. This parameter is ignored
#'               when \code{use_aria == TRUE}. Default is \code{TRUE}
#' @param cores optional integer. Number of cores used for parallel download. If \code{NA}, available
#'              cores will be detected and used. Ignored when \code{aria2 == TRUE} or
#'              \code{do_par == FALSE}. Default is \code{NA}.
#'
#' @details Selected MODIS tiles are downloaded as .hdf archives from the Level-1 and Atmosphere Archive
#'          & Distribution System (LAADS) of NASA's Distributed Active Archive Center (DAAC) at the Goddard
#'          Space Flight Center in Greenbelt, Maryland (\url{https://ladsweb.modaps.eosdis.nasa.gov/})
#'
#' @author Sandro Groth
#'
#' @references Jakob Schwalb-Willmann (2018). getSpatialData: Get different kinds of freely available
#'             spatial datasets. R package version 0.0.4. \url{http://www.github.com/16eagle/getSpatialData/}
#'
#' @examples
#' ## Import packages
#' \dontrun{
#' library(getSpatialData)
#' library(sf)
#'
#' ## Set query parameters
#' aoi_data <- data("aoi_data")
#' set_aoi(aoi_data[[1]])
#' time_range <- c("2017-01-01", "2017-12-31")
#'
#' ## Login to USGS
#' loginUSGS("Username")
#'
#' ## get available products
#' product_names <- getMODIS_names()
#' product <- grep("MOD13Q1", product_names, value = T)
#'
#' ## Execute query
#' query <- getMODIS_query(time_range = time_range, name = product)
#'
#' ## Download all selected tiles
#' donwloadMODIS(query, "path/to/directory")
#' }
#'
#' @import doSNOW
#' @import parallel
#'
#' @importFrom logging loginfo logdebug
#' @importFrom getSpatialData getMODIS_data
#'
#' @seealso \link{prepMODIS} \link[getSpatialData]{getMODIS_query}
#'
#' @export
#'
downloadMODIS <- function(gSD_query, out_dir, use_aria=TRUE, do_par=TRUE, cores=NA) {

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

    # Check if aria2c available
    if (nchar(unname(Sys.which("aria2c"))) == 0) {
      stop("aria2c.exe not found. Make sure it's correctly added to your PATH variable.")
    }
    logging::logdebug(paste0("aria2c found: ", unname(Sys.which("aria2c"))))

    # check if Login Data is available from getSpatialData
    if(is.null(options("gSD.usgs_user")) || is.null(options("gSD.usgs_pass"))) {
      stop("No Login credentials available. Try executing getSpatialData::loginUSGS() first.")
    }

    # Build temporary textfile for aria2c download
    f_path <- file.path(tempdir(), "MOD_URLs.txt")
    f <- file(f_path)
    writeLines(url_files, f)
    close(f)

    # Build aria2c command
    username <- options("gSD.usgs_user")
    passw <- options("gSD.usgs_pass")
    cmd <- Sys.which("aria2c")
    args <- c(paste0(" -i ", file.path(tempdir(), "MOD_URLs.txt")),
              paste0(" -d ", out_dir),
              paste0(" -x ", 6),
              paste0(" --http-use=", username),
              paste0(" --http-passw=", passw$gSD.usgs_pass),
              paste0(" --retry-wait=", 2),
              paste0(" --continue=", "true"),
              " --allow-overwrite")

    # Execute bulk download using aria2c in seperate command window to monitor progress
    try(system2(command = cmd, args = args, wait = TRUE, stdout = "", invisible = FALSE))
  } else {
    if(isTRUE(do_par)) {
      # set up parallel processing
      if (is.na(cores)) cores <- parallel::detectCores() - 1
      c1 <- parallel::makePSOCKcluster(cores)
      doSNOW::registerDoSNOW(c1)
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

#'
#'
#' @import doParallel
#'
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom foreach foreach
#' @importFrom gdalUtils gdal_translate
#'
.extract_sd <- function(in_files, out_dir, band_id, band_nr, cores=NA, check_existing=T) {

  # setup parallel processing
  if (is.na(cores)) cores <- parallel::detectCores() - 1
  c1 <- parallel::makeCluster(cores)
  doParallel::registerDoParallel(c1)
  message(paste0("Set up parallel processing on ", cores, " cores"))

  message(paste0("Extracting ", band_id, " of ", length(in_files), " files to "), out_dir, " ...")
  foreach::foreach(f = 1:length(in_files), .packages = "gdalUtils") %dopar% {

    out_file <- file.path(out_dir, paste0(strsplit(basename(in_files[f]), ".hdf")[[1]][1], band_id, ".tif"))
    if (isTRUE(check_existing)) {
      if (!file.exists(out_file)) gdalUtils::gdal_translate(in_files[f], out_file, sd_index = band_nr)
    } else gdalUtils::gdal_translate(in_files[f], out_file, sd_index = band_nr)
  }

  parallel::stopCluster(c1)
}

#'
#'
#'
.extract_qa <- function(in_dir, out_dir, cores=NA, check_existing=T) {

  # check if output directory exists, otherwise create it
  if(!dir.exists(out_dir)) {try(dir.create(out_dir))}
  message(paste0("Output directory set: ", out_dir))

  # list all MODIS hdf files in input diretory
  mod_files <- list.files(in_dir, ".*M(O|Y)D.*.hdf", full.names = T, no.. = T)

  # Get Product information in order to extract the right subdataset
  prod_name <- strsplit(basename(mod_files[1]), '\\.')[[1]][1] # e.g. MOD13Q1
  band_id <- "qa"
  band_nr <- get_sdnr(prod_name, band_id)

  # execute the parallelized extraction
  .extract_sd(mod_files, out_dir, band_id, band_nr, cores, check_existing)
  message("All QA datasets sucessfully extracted.")

}

#'
#'
#'
get_sdnr <- function(product_name, sd_type) {
  switch(sd_type,
         qa = switch(product_name,
                     MOD13Q1 = return(12),
                     MYD13Q1 = return(12),
                     MOD13A1 = return(12),
                     MYD13A1 = return(12),
                     MOD13A2 = return(12),
                     MYD13A2 = return(12),
                     MOD13C1 = return(12),
                     MYD13C1 = return(12),
                     MOD13A3 = return(12),
                     MYD13A3 = return(12),
                     MOD13C2 = return(12),
                     MYD13C2 = return(12),
                     stop(paste0("Product ID: ", product_name, " not supported."))),
         ndvi = switch(product_name,
                       MOD13Q1 = return(1),
                       MYD13Q1 = return(1),
                       MOD13A1 = return(1),
                       MYD13A1 = return(1),
                       MOD13A2 = return(1),
                       MYD13A2 = return(1),
                       MOD13C1 = return(1),
                       MYD13C1 = return(1),
                       MOD13A3 = return(1),
                       MYD13A3 = return(1),
                       MOD13C2 = return(1),
                       MYD13C2 = return(1),
                       stop(paste0("Product ID: ", product_name, " not supported."))),
         evi = switch(product_name,
                      MOD13Q1 = return(2),
                      MYD13Q1 = return(2),
                      MOD13A1 = return(2),
                      MYD13A1 = return(2),
                      MOD13A2 = return(2),
                      MYD13A2 = return(2),
                      MOD13C1 = return(2),
                      MYD13C1 = return(2),
                      MOD13A3 = return(2),
                      MYD13A3 = return(2),
                      MOD13C2 = return(2),
                      MYD13C2 = return(2),
                      stop(paste0("Product ID: ", product_name, " not supported."))),
         doy = switch(product_name,
                      MOD13Q1 = return(11),
                      MYD13Q1 = return(11),
                      MOD13A1 = return(11),
                      MYD13A1 = return(11),
                      MOD13A2 = return(11),
                      MYD13A2 = return(11),
                      MOD13C1 = return(11),
                      MYD13C1 = return(11),
                      MOD13A3 = return(11),
                      MYD13A3 = return(11),
                      MOD13C2 = return(11),
                      MYD13C2 = return(11),
                      stop(paste0("Product ID: ", product_name, " not supported."))),
         stop(paste0("sd_type ", sd_type, " not supported."))
  )

}

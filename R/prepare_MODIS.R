#' @title Prepare MODIS
#'
#' @description \code{prepare_MODIS} prepares MODIS .hdf archives obtained by \link[phenoRS]{download_MODIS}
#' for curve fitting.
#'
#' @details TODO
#'
#' @param in_dir TODO
#' @param out_dir TODO
#' @param aoi TODO
#' @param vi TODO
#' @param product_name TODO
#' @param out_proj TODO
#' @param use_DOY TODO
#' @param use_QA TODO
#' @param keep_orig TODO
#' @param dtype TODO
#' @param overwrite TODO
#' @param do_par TODO
#' @param cores TODO
#' @param progress TODO
#'
#' @return TODO
#'
#' @author Sandro Groth
#'
#' @examples \dontrun{TODO}
#'
#' @seealso TODO
#'
#' @import parallel
#' @import doSNOW
#' @importFrom logging loginfo
#' @importFrom sp CRS
#' @importFrom sf st_transform st_write
#'
#' @name prepare_MODIS
#' @export
prepare_MODIS <- function(in_dir, out_dir, aoi, vi = 'NDVI', product_name = NA, out_proj = NA,
                          use_DOY = TRUE, use_QA = TRUE, keep_orig = TRUE, dtype = 'INT2S', overwrite = TRUE,
                          do_par = TRUE, cores = NA, progress = TRUE) {

  # input checks
  if (!(vi %in% getOption('phRS.supported')$VIs)) stop("Specified VI is not supported.")
  if (!dir.exists(in_dir)) stop("Input directory does not exist.")
  if (dir.exists(out_dir) & isFALSE(overwrite)) stop("Can't overwrite output directory when overwrite = TRUE")
  if (!missing(aoi) && !inherits(aoi, c("sf"))) stop("Unsupported aoi type. Use sf object instead.")
  if (missing(aoi) & isTRUE(out_proj == 'AOI')) stop("Can't use crs of AOI when no AOI is specified.")
  #if (!(product_name %in% getOption('phRS.supported')$products$MODIS)) stop("Specified product not supported.")
  if (isTRUE(do_par) & !is.na(cores) & cores > detectCores()) stop("More cores specified than available.")
  if (isTRUE(do_par) & is.na(cores)) cores <- detectCores() - 1

  # CRS
  out_crs <- NULL
  if (!is.na(out_proj)) {
    if (out_proj == 'AOI') {
      out_crs <- CRS(crs(aoi))
    } else {
      if (!inherits(out_proj, c("CRS", "sp"))) {
        tryCatch({
          if (grepl("+proj", out_proj, fixed = T)) out_crs <- CRS(projargs = out_proj)
          if (grepl("epsg:", out_proj, fixed = T)) out_crs <- CRS(SRS_string = out_proj)
          if (grepl("EPSG:", out_proj, fixed = T)) out_crs <- CRS(SRS_string = out_proj) # TODO: Proper grepl
        },
        error = function(cond) stop(paste0("Failed to get an CRS object from out_proj: ", cond)))
      } else {
        out_crs <- out_proj
      }
    }
    if (is.null(out_crs)) stop("Unable to build output CRS.")
  }

  loginfo("Starting processing...")

  # setup data structure
  if (!dir.exists(out_dir)) {
    if (any(isFALSE(dir.create(out_dir)))) stop("Failed to create output directory.")
  } else {
    if (unlink(out_dir, recursive = T) == 1) stop("Failed to remove old version of output directory.")
    if (any(isFALSE(dir.create(out_dir)))) stop("Failed to create output directory.")
  }
  loginfo(paste0("Output directory set: ", out_dir))
  orig_dir <- file.path(out_dir, "orig")
  if(any(isFALSE(dir.create(orig_dir)))) stop("Failed to create orig directory.")
  loginfo(paste0("Original images directory set: ", orig_dir))
  sds <- c(vi)
  if (isTRUE(use_DOY)) sds <- c(sds, 'DOY')
  if (isTRUE(use_QA))  sds <- c(sds, 'QA')

  # save AOI temporarily for gdalwarp
  if (!missing(aoi)) {
    if (!is.null(out_crs)) {
      if (CRS(crs(aoi))@projargs != out_crs@projargs) aoi <- st_transform(aoi, out_crs)
    }
    tmp_aoi <- file.path(tempdir(), 'aoi_mask.shp')
    st_write(aoi, tmp_aoi, overwrite = T, delete_dsn = T, quiet = T)
  }

  # track current preparation status
  prep_status <- NULL

  # ---- SDS Extraction ----

  loginfo("Extracting subdatasets from .hdf files...")

  # search for MODIS files
  hdfs <- list.files(in_dir, pattern = ".*M(O|Y)D.*.hdf", full.names = T, no.. = T)
  if (length(hdfs) < 1) stop("No MODIS files found in in_directory. Make sure that original naming has
                             been retained.")

  if (isTRUE(do_par)) c1 <- .start_SNOW(cores)
  if (isTRUE(progress)) progb <- .setup_pb(1, length(hdfs), do_par)

  if (isTRUE(do_par)) {
    foreach(f = 1:length(hdfs), .packages = c('raster', 'gdalUtils', 'rgdal', 'lubridate'),
            .export = c('.getMODIS_compositeDOY', '.getMODIS_compositeYear', 'extract_hdf', 'correct_doy'),
            .options.snow = if(isTRUE(progress)) progb$opts else NULL) %dopar% {
      for (i in 1:length(sds)) {
        out_file <- file.path(orig_dir, paste0(strsplit(basename(hdfs[f]), ".hdf")[[1]][1],
                                               "_", sds[i], "_extract.tif"))
        r_sd <- extract_hdf(hdfs[f], sds[i], dtype)
        if (sds[i] == 'DOY') {
          r_sd <- correct_doy(r_sd, .getMODIS_compositeYear(hdfs[f]))
        }
        writeRaster(r_sd, filename = out_file, datatype = dtype, format = 'GTiff', overwrite = T)
      }
    }
    if (isTRUE(progress)) close(progb$pb)
    .stop_SNOW(c1)
  } else {
    for (f in 1:length(hdfs)) {
      for (i in 1:length(sds)) {
        out_file <- file.path(orig_dir, paste0(strsplit(basename(hdfs[f]), ".hdf")[[1]][1],
                                              "_", sds[i], "_extract.tif"))
        r_sd <- extract_hdf(hdfs[f], sds[i], dtype)
        if (sds[i] == 'DOY') {
          r_sd <- correct_doy(r_sd, .getMODIS_compositeYear(hdfs[f]))
        }
        writeRaster(r_sd, filename = out_file, datatype = dtype, format = 'GTiff', overwrite = T)
      }
      if (isTRUE(progress)) setTxtProgressBar(progb$pb, f)
    }
    if (isTRUE(progress)) close(progb$pb)
  }
  prep_status <- 'EXTRACT'

  # ---- Tile Mosaicing ----

  logging::loginfo("Mosaicing tiles...")

  for (i in 1:length(sds)) {
    loginfo(paste0("Mosaicing subdatasets: ", sds[i]))
    img_files <- list.files(orig_dir, paste0('.*M(O|Y)D.*', sds[i], '_extract.tif'),
                            full.names = TRUE, no.. = TRUE)

    if (length(.get_unique_tilestr(img_files)) > 1) {
      dates <- unique(do.call("c", lapply(img_files, .getMODIS_date)))

      if (isTRUE(do_par)) c1 <- .start_SNOW(cores)
      if (isTRUE(progress)) progb <- .setup_pb(1, length(dates), do_par)

      if (isTRUE(do_par)) {
        foreach(d = 1:length(dates), .packages = 'gdalUtils', .export = '.dtype_R_to_GDAL',
                .options.snow = if (isTRUE(progress)) progb$opts else NULL) %dopar% {
          date_curr_str <- strftime(dates[d], format = '%Y%j')
          out_file <- file.path(orig_dir, paste0(date_curr_str, '_', sds[i], "_mosaic.tif"))
          date_curr_files <- img_files[grepl(paste0("\\.A", date_curr_str, "\\."), img_files)]
          catch <- mosaic_rasters(date_curr_files, out_file, of = 'GTiff', ot = .dtype_R_to_GDAL(dtype))
        }
        if (isTRUE(progress)) close(progb$pb)
        .stop_SNOW(c1)
      } else {
        for (d in 1:length(dates)) {
          date_curr_str <- strftime(dates[d], format = '%Y%j')
          out_file <- file.path(orig_dir, paste0(date_curr_str, '_', sds[i], "_mosaic.tif"))
          date_curr_files <- img_files[grepl(paste0("\\.A", date_curr_str, "\\."), img_files)]
          catch <- mosaic_rasters(date_curr_files, out_file, of = 'GTiff', ot = .dtype_R_to_GDAL(dtype))
          if (isTRUE(progress)) setTxtProgressBar(progb$pb, d)
        }
        if (isTRUE(progress)) close(progb$pb)
      }
    } else {
      loginfo("Only one unique tile found. Mosaicing skipped.")
      for (f in 1:length(img_files)) {
        date_str <- .getMODIS_datestr(.getMODIS_date(basename(img_files[f])))
        out_file <- file.path(orig_dir, paste0(date_str, "_", sds[i], "_mosaic.tif"))
        file.rename(from = img_files[f], to = out_file)
      }
    }
  }
  prep_status <- 'MOSAIC'

  # cleanup
  do.call(file.remove, list(list.files(orig_dir, ".*M(O|Y)D.*_extract.tif", full.names = TRUE, no.. = TRUE)))

  # ---- Image Reprojection ----

  if (!is.na(out_proj)) {
    logging::loginfo(paste0("Reprojecting images to: ", out_crs@projargs))

    for (i in 1:length(sds)) {
      logging::loginfo(paste0("Processing ", sds[i], " subdatasets..."))
      img_files <- list.files(orig_dir, paste0('_', sds[i], '_mosaic.tif'), full.names = TRUE, no.. = TRUE)

      if (isTRUE(do_par)) c1 <- .start_SNOW(cores)
      if (isTRUE(progress)) progb <- .setup_pb(1, length(img_files), do_par)

      if (isTRUE(do_par)) {
        foreach(f = 1:length(img_files), .packages = 'gdalUtils', .export = c('reproject', '.dtype_R_to_GDAL'),
                .options.snow = if (isTRUE(progress)) progb$opts else NULL) %dopar% {
          out_file <- file.path(orig_dir, paste0(strsplit(basename(img_files[f]), "_")[[1]][1],
                                                 "_", sds[i], "_proj.tif"))
          reproject(img_files[f], out_file, out_crs, dtype = .dtype_R_to_GDAL(dtype))
        }
        if (isTRUE(progress)) close(progb$pb)
        .stop_SNOW(c1)
      } else {
        for (f in 1:length(img_files)) {
          out_file <- file.path(orig_dir, paste0(strsplit(basename(img_files[f]), "_")[[1]][1],
                                                 "_", sds[i], "_proj.tif"))
          reproject(img_files[f], out_file, out_crs, dtype = .dtype_R_to_GDAL(dtype))
          if (isTRUE(progress)) setTxtProgressBar(progb$pb, f)
        }
        if (isTRUE(progress)) close(progb$pb)
      }
    }
    # cleanup
    do.call(file.remove, list(list.files(orig_dir, "_mosaic.tif", full.names = TRUE, no.. = TRUE)))

    prep_status <- 'PROJ'
  } else loginfo("Reprojection skipped.")

  # ---- AOI Cropping ----

  if (!missing(aoi)) {
    logging::loginfo("Cropping images to AOI...")

    for (i in 1:length(sds)) {
      logging::loginfo(paste0("Processing ", sds[i], " subdatasets..."))

      if (!is.na(out_proj)) {
        img_files <- list.files(orig_dir, paste0('_', sds[i], '_proj.tif'), full.names = TRUE, no.. = TRUE)
      } else {
        img_files <- list.files(orig_dir, paste0('_', sds[i], '_mosaic.tif'), full.names = TRUE, no.. = TRUE)
      }

      aoi_ext <- extent(aoi)

      if (isTRUE(do_par)) c1 <- .start_SNOW(cores)
      if (isTRUE(progress)) progb <- .setup_pb(1, length(img_files), do_par)

      if (isTRUE(do_par)) {
        foreach(f = 1:length(img_files), .packages = c('gdalUtils', 'raster'), .export = 'crop_fast',
                .options.snow = if (isTRUE(progress)) progb$opts else NULL) %dopar% {
          out_file <- file.path(orig_dir, paste0(strsplit(basename(img_files[f]), "_")[[1]][1], "_",
                                                 sds[i], "_crop.tif"))
          crop_fast(img_files[f], out_file, aoi_ext, dtype = dtype)
        }
        if (isTRUE(progress)) close(progb$pb)
        .stop_SNOW(c1)
      } else {
        for (f in 1:length(img_files)) {
          out_file <- file.path(orig_dir, paste0(strsplit(basename(img_files[f]), "_")[[1]][1], "_",
                                                 sds[i], "_crop.tif"))
          crop_fast(img_files[f], out_file, aoi_ext, dtype = dtype)
          if (isTRUE(progress)) setTxtProgressBar(progb$pb, f)
        }
        if (isTRUE(progress)) close(progb$pb)
      }
    }

    # cleanup
    if (!is.na(out_proj)) {
      do.call(file.remove, list(list.files(orig_dir, "_proj.tif", full.names = TRUE, no.. = TRUE)))
    } else {
      do.call(file.remove, list(list.files(orig_dir, "_mosaic.tif", full.names = TRUE, no.. = TRUE)))
    }

    prep_status <- 'CROP'

    # ---- AOI Masking ----

    logging::loginfo("Masking images to AOI...")

    for (i in 1:length(sds)) {
      logging::loginfo(paste0("Processing ", sds[i], " subdatasets..."))
      img_files <- list.files(orig_dir, paste0('_', sds[i], '_crop.tif'), full.names = TRUE, no.. = TRUE)

      if (isTRUE(do_par)) c1 <- .start_SNOW(cores)
      if (isTRUE(progress)) progb <- .setup_pb(1, length(img_files), do_par)

      if (isTRUE(do_par)) {
        foreach::foreach(f = 1:length(img_files), .packages = 'gdalUtils',
                         .export = c('mask_fast', '.dtype_R_to_GDAL'),
                         .options.snow = if (isTRUE(progress)) progb$opts else NULL) %dopar% {
          out_file <- file.path(orig_dir, paste0(strsplit(basename(img_files[f]), "_")[[1]][1], "_",
                                                 sds[i], "_prep.tif"))
          mask_fast(img_files[f], out_file, tmp_aoi, dtype = .dtype_R_to_GDAL(dtype))
        }
        if (isTRUE(progress)) close(progb$pb)
        .stop_SNOW(c1)
      } else {
        for (f in 1:length(img_files)) {
          out_file <- file.path(orig_dir, paste0(strsplit(basename(img_files[f]), "_")[[1]][1], "_",
                                                 sds[i], "_prep.tif"))
          mask_fast(img_files[f], out_file, tmp_aoi, dtype = .dtype_R_to_GDAL(dtype))
          if (isTRUE(progress)) setTxtProgressBar(progb$pb, f)
        }
        if (isTRUE(progress)) close(progb$pb)
      }
    }

    # cleanup
    do.call(file.remove, list(list.files(orig_dir, "_crop.tif", full.names = TRUE, no.. = TRUE)))

    prep_status <- 'PREP'
  } else {
    loginfo("AOI cropping skipped.")
    loginfo("AOI masking skipped.")
  }

  # ---- ENVI Conversion ----

  logging::loginfo("Converting prepared images to .envi files...")

  if (prep_status == 'MOSAIC') img_files <- list.files(orig_dir, "_mosaic.tif", full.names = TRUE, no.. = TRUE)
  if (prep_status == 'PROJ') img_files <- list.files(orig_dir, "_proj.tif", full.names = TRUE, no.. = TRUE)
  if (prep_status == 'PREP') img_files <- list.files(orig_dir, "_prep.tif", full.names = TRUE, no.. = TRUE)

  if (isTRUE(do_par)) c1 <- .start_SNOW(cores)
  if (isTRUE(progress)) progb <- .setup_pb(1, length(img_files), do_par)

  if (isTRUE(do_par)) {
    foreach(f = 1:length(img_files), .packages = c("gdalUtils", "tools"), .export = 'to_envi',
            .options.snow = if (isTRUE(progress)) progb$opts else NULL) %dopar% {
      out_file <- file.path(out_dir, paste0(strsplit(basename(img_files[f]), '_')[[1]][1],
                                            "_", strsplit(basename(img_files[f]), '_')[[1]][2],
                                            "_prepbin.envi"))
      to_envi(img_files[f], out_file, dtype = dtype)
    }
    if (isTRUE(progress)) close(progb$pb)
    .stop_SNOW(c1)
  } else {
    for (f in 1:length(img_files)) {
      out_file <- file.path(out_dir, paste0(strsplit(basename(img_files[f]), '_')[[1]][1],
                                            "_", strsplit(basename(img_files[f]), '_')[[1]][2],
                                            "_prepbin.envi"))
      to_envi(img_files[f], out_file, dtype = dtype)
      if (isTRUE(progress)) setTxtProgressBar(progb$pb, f)
    }
    if (isTRUE(progress)) close(progb$pb)
  }

  # cleanup of orig files
  if (isFALSE(keep_orig)) {
    if (unlink(orig_dir, recursive = T) == 1) stop("Failed to remove original prepared files. Preparation was,
                                                   however, sucessfull.")
  }

  logging::loginfo("Preprocessing successful.")
}

#prepare_MODIS('C:\\Projects\\R\\Data\\HDF_DEV', 'C:\\Projects\\R\\Data\\PREP_NEW', aoi = aoi_small, vi = 'NDVI', out_proj = 'AOI', do_par = T)

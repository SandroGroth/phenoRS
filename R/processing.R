#'
#'
#'
process_single <- function(vi, doy, qa, comp_d, settings) {

  # determine initial weights from QA cube
  if (!is.null(qa_col)) weights <- MODIS_summary_qa(qa_col, settings$weights$weight_min,
                                                    settings$weights$weight_med,
                                                    settings$weights$weight_max) else weights <- NULL

  # check input timeseries
  checked <- check_ts(vi, doy, weights, comp_d,
                      settings$general$minimal_valid,
                      settings$value_range$valid_min,
                      settings$value_range$valid_max,
                      settings$weights$weight_min,
                      settings$approx_spikes)

  y <- checked$y
  d <- checked$d
  w <- checked$w

  # spike removal
  w <- switch(config$spike$spike_method,
    Median = spike_median(y, w, settings$general$n_val_per_year,
                                settings$weights$w_min,
                                settings&spike$spike_value),
    STL    = spike_stl(),
    STL_w  = spike_stl_w()
  )

  # divide seasons
  seas <- divide_seasons(y, d, w, settings$seasons$seasons_per_year)

  y <- seas$y
  d <- seas$d
  w <- seas$w
  mins <- seas$mins
  maxs <- seas$maxs

  # FITTING
  fits <- fit_ts(y, d, w, mins, maxs, settings)

  # PHENOPARAMS

}

#'
#'
#'
process_full <- function(in_dir, out_dir, settings, do_par = FALSE, cores = NA, progress = TRUE) {

  # input checks
  # TODO

  # DATA INPUT -----------------------------------------------------------------------------------------------

  # load metadata
  meta_vi_f <- list.files(in_dir, pattern = '.*_(NDVI|EVI)_.*.aux.xml', full.names = T, no.. = T)
  if (length(meta_vi_f) > 1) {
    loginfo(paste0("Found ", length(meta_vi_f), " VI image metadata files."))
  } else stop("No VI image metadata found in directory.")

  meta_doy_f <- NULL
  if (isTRUE(settings$use_real_doy)) {
    meta_doy_f <- list.files(in_dir, pattern = '.*_DOY_.*.aux.xml', full.names = T, no.. = T)
    if (length(meta_doy_f) > 1) {
      loginfo(paste0("Found ", length(meta_doy_f), " DOY image metadata files."))
    } else stop("No DOY image metadata found in directory.")
  }

  meta_qa_f  <- NULL
  if (isTRUE(settings$use_qa)) {
    meta_qa_f <- list.files(in_dir, pattern = '.*_QA_.*.aux.xml', full.names = T, no.. = T)
    if (length(meta_qa_f) > 1) {
      loginfo(paste0("Found ", length(meta_qa_f), " QA image metadata files."))
    } else stop("No QA image metadata found in directory.")
  }

  # load prepared image files
  vi_f <- list.files(in_dir, pattern = '.*_(NDVI|EVI)_.*.envi$', full.names = T, no.. = T)
  if (length(vi_f) > 1) loginfo(paste0("Found ", length(vi_f), " VI images")) else stop("No VI images found.")

  doy_f <- NULL
  if (isTRUE(settings$use_real_doy)) {
    doy_f <- list.files(in_dir, pattern = '.*_DOY_.*.envi$', full.names = T, no.. = T)
    if (length(doy_f) > 1) loginfo(paste0("Found ", length(doy_f), " DOY images")) else stop("No DOY images found.")
  }

  qa_f <- NULL
  if (isTRUE(settings$use_qa)) {
    qa_f <- list.files(in_dir, pattern = '.*_QA_.*.envi$', full.names = T, no.. = T)
    if (length(qa_f) > 1) loginfo(paste0("Found ", length(qa_f), " QA images")) else stop("No QA images found.")
  }

  # check lengths
  if (settings$use_real_doy) {
    if (length(vi_f) != length(doy_f)) stop("Number of subdatasets (VI, DOY) do not match.")
  }

  if (settings$use_qa) {
    if (length(vi_f) != length(qa_f)) stop("Number of subdatasets (VI, QA) do not match.")
  }

  # build datacubes
  vi_dc <- build_cube(vi_f, meta_vi_f, settings)
  if (settings$use_real_doy) doy_dc <- build_cube(doy_f, meta_doy_f, settings) else doy_dc <- NULL
  if (settings$use_qa) qa_dc <- build_cube(qa_f, meta_qa_f, settings) else qa_dc <- NULL

  # check dimensions
  if (settings$use_real_doy) {
    if (nrow(vi_dc) != nrow(doy_dc)) stop(paste("Non matching number of rows in VI, DOY:", nrow(vi_dc),
                                                nrow(doy_dc)))
    if (ncol(vi_dc) != ncol(doy_dc)) stop(paste("Non matching number of cols in VI, DOY:", ncol(vi_dc),
                                                ncol(doy_dc)))
  }
  if (settings$use_qa) {
    if (nrow(vi_dc) != nrow(qa_dc)) stop(paste("Non matching number of rows in VI, QA:", nrow(vi_dc),
                                                nrow(qa_dc)))
    if (ncol(vi_dc) != ncol(qa_dc)) stop(paste("Non matching number of cols in VI, QA:", ncol(vi_dc),
                                                ncol(qa_dc)))
  }

  # Extract the dates from filename, if not use_real_doy
  comp_doys <- .get_prep_doy(doy_f)
  comp_doystr <- .get_prep_comp_str(doy_f)

  # PIXELWISE PROCESSING -------------------------------------------------------------------------------------

  if (isTRUE(progress)) progb <- .setup_pb(1, length(hdfs), do_par)

  # rowwise
  for (i in 1:nrow(vi_dc)) {

    # Extract current row from datacube
    vi_row <- getValues(vi_dc, row = i)
    if(settings$use_real_doy) doy_row <- getValues(doy_dc, row = i) else doy_row <- NULL
    if(settings$use_qa) qa_row <- getValues(qa_dc, row = i) else qa_row <- NULL

    # colwise
    for (j in 1:ncols(vi_dc)) {

      vi_col <- vi_row[j, ]
      if (!is.null(doy_row)) doy_col <- doy_row[j, ] else doy_col <- comp_doys
      if (!is.null(qa_row)) qa_col <- qa_col[j, ] else qa_col <- NULL

      # PROCESSING
      processed <- process_single(vi_col, doy_col, qa_col, comp_doystr, settings)


    }

    # increment progress
    if (isTRUE(progress)) setTxtProgressBar(progb$pb, f)

  }

  if (isTRUE(progress)) close(progb$pb)
}

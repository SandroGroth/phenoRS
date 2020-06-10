#'
#' @param y Numeric vector.
#'
#' Unequally distributed TS:
#' https://dsp.stackexchange.com/questions/1676/savitzky-golay-smoothing-filter-for-not-equally-spaced-data
#'
#' @importFrom phenofit rcpp_wSG wTSM
#'
#' @export
#'
smooth_regWSavGol <- function(y, w, ypts, frame = floor(ypts/7)*2 + 1, d = 2, iters = 2, ...) {

  if(all(is.na(y))) return(y)

  halfwin = floor((frame-1)/2)

  y_iter   <- y
  fits     <- list()
  ws       <- list()

  for (i in 1:iters) {

    ws[[i]] <- w
    z <- rcpp_wSG(y_iter, halfwin, d, w)

    w_new <- phenofit:::rcpp_wTSM(y, z, w, i, ypts)


    # adaption to upper envelope
    I <- which(y_iter < z)
    if (length(I) > 0) y_iter[I] <- z[I]

    fits[[i]] <- z

  }

  fits %<>% set_names(paste0('ziter', 1:iters))
  ws   %<>% set_names(paste0('witer', 1:iters))

  list(fits=fits, ws=ws)

}

smooth_iregWSavGol <- function(y, x, halfwin, d = 2, iter = 2) {

  if(all(is.na(y))) return(y)
}

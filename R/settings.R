#' Create processing settings
#'
#' @param data_type
#' @param use_qa
#' @param valid_min
#' @param valid_max
#' @param weight_min
#' @param weight_med
#' @param weight_max
#' @param amplitude_cutoff
#' @param spike_method
#' @param spike_value
#' @param stl_stiffness
#'
#' @export
#'
create_settings <- function(
  data_type        = 'Int16',    # 'Int16', 'UInt16', 'Int32', 'Float32'
  n_years          = 3,          # positive integer
  n_val_per_year   = 23,         # positive integer
  internal_min     = -999,       # integer
  max_data_gap     = 3,          # positive integer
  minimal_valid    = 0.5,        # 0 - 1
  approx_spikes    = TRUE,       # TRUE, FALSE
  use_qa           = TRUE,       # TRUE, FALSE
  use_real_doy     = FALSE,      # TRUE, FALSE, only modis composite
  valid_min        = 0,          # 0 - 10,000
  valid_max        = 10000,      # 0 - 10,000, greater than valid_min
  weight_min       = 0,          # > 0, lower than w_med and w_max
  weight_med       = 0.5,        # > 0, lower than w_max
  weight_max       = 1,          # > 0
  amplitude_cutoff = 0,          # 0 - 10,000, 0 means all data processed
  spike_method     = "STL_w",    # "STL_w", "STL", "Median", "None"
  spike_value      = 2,          # when spike_method "Median"
  stl_stiffness    = 3,          # 1.0 - 10.0
  seasons_per_year = 1
) {

  # ---- Input Checks ----
  # type checks
  if(!is.character(data_type))      stop("data_type must be character.")
  if(!is.logical(use_qa))           stop("use_qa must be a logical value.")
  if(!is.numeric(valid_min))        stop("valid_min must be numeric.")
  if(!is.numeric(valid_max))        stop("valid_max must be numeric.")
  if(!is.numeric(weight_min))       stop("weight_min must be numeric.")
  if(!is.numeric(weight_med))       stop("weight_min must be numeric.")
  if(!is.numeric(weight_max))       stop("weight_min must be numeric.")
  if(!is.numeric(amplitude_cutoff)) stop("amplitude_cutoff must be numeric.")
  if(!is.character(spike_method))   stop("spike_method must be character.")
  if(!is.numeric(spike_value))      stop("spike_value must be numeric.")
  if(!is.numeric(stl_stiffness))    stop("stl_stiffness must be numeric.")

  # value checks
  if(!(data_type %in% c('INT2S', 'INT2U', 'INT4S', 'INT4U'))) stop("Unsupported data type.")
  if(any(c(weight_min, weight_med, weight_max) < 0)) stop("weights must be 0 or positive.")
  if(weight_min > weight_med) stop("weight_min must be lower than weight_med.")
  if(weight_med > weight_max) stop("weight_med must be lower than weigth_max.")
  if(!(spike_method %in% c("STL_w", "STL", "Median", "None"))) stop("Invalid spike method.")
  if(!dplyr::between(stl_stiffness, 1.0, 10.0)) stop("stl_stiffnessnot in valid range.")

  # ---- Settings Assignment ----
  list(
    images = list(
      data_type = data_type
    ),
    general = list(
      internal_min = internal_min,
      max_data_gap = max_data_gap,
      minimal_valid = minimal_valid,
      approx_spikes = approx_spikes
    ),
    use_qa = use_qa,
    use_real_doy = use_real_doy,
    value_range = list(
      valid_min = valid_min,
      valid_max = valid_max
    ),
    weights = list(
      weight_min = weight_min,
      weight_med = weight_med,
      weight_max = weight_max
    ),
    amplitude_cutoff = amplitude_cutoff,
    spike = list(
      spike_method = spike_method,
      spike_value = spike_value,
      stl_stiffness = stl_stiffness
    ),
    seasons = list(
      seasons_per_year = seasons_per_year
    )
  )
}

load_settings <- function(in_file) {}

save_settings <- function(settings, out_file) {}

check_settings <- function(settings) {}

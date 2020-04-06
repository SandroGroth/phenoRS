.getMODIS_date <- function(file, pos1=10, pos2=16, format="%Y%j") {
  return(as.Date(substr(basename(file), pos1, pos2), format = format))
}

.getMODIS_datestr <- function(date, format='%Y%j') {
  return(as.character(strftime(date, format = format)))
}

.getMODIS_tile <- function(file, pos1=18, pos2=23) {
  tile_info <- substr(basename(file), pos1, pos2)
  h <- as.numeric(substr(tile_info, 2, 3))
  v <- as.numeric(substr(tile_info, 5, 6))

  return(c(h, v))
}

.get_sdnr <- function(product_name, sd_type) {
  switch(sd_type,
         qa = switch(product_name,
                     MOD13Q1 = return(12),
                     MYD13Q1 = return(12),
                     stop(paste0("Product ID: ", product_name, " not supported."))),
         ndvi = switch(product_name,
                       MOD13Q1 = return(1),
                       MYD13Q1 = return(1),
                       stop(paste0("Product ID: ", product_name, " not supported."))),
         evi = switch(product_name,
                      MOD13Q1 = return(2),
                      MYD13Q1 = return(2),
                      stop(paste0("Product ID: ", product_name, " not supported."))),
         doy = switch(product_name,
                      MOD13Q1 = return(11),
                      MYD13Q1 = return(11),
                      stop(paste0("Product ID: ", product_name, " not supported."))),
         stop(paste0("sd_type ", sd_type, " not supported."))
  )
}

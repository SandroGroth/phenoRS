setup_logging <- function(handlers = c("writeToConsole"), log_level='DEBUG', log_file=NA) {

  if ("writeToFile" %in% handlers) {addHandler(writeToFile, file = log_file)}
  if ("writeToConsole" %in% handlers) {addHandler(writeToConsole)}
  setLevel(log_level)
}

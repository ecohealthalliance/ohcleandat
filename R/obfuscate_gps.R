#' Obfuscate GPS
#'
#' This function fuzzes gps points (or any other numeric values) by first adding
#' error then rounding to a certain number of digits.
#'
#' @param x Numeric. Vector of gps points
#' @param precision Integer. Number of digits to keep. See `round` for more details
#' @param fuzz Numeric. Error to introduce to the gps measurements. This is used
#' to generate the random uniform distribution `runif(1,min = -fuzz, max = fuzz)`
#'
#' @return Numeric. A vector of fuzzed and rounded GPS points
#' @export
#'
#' @examples
#'
#' gps_data  <- data.frame(lat = c(1.0001, 10.22223, 4.00588),
#'   lon = c(2.39595, 4.506930, -60.09999901))
#'
#' gps_data |>
#'   # default obfuscation settings correspont to roughly a 27 by 27 km area
#'   dplyr::mutate(fuzzed_lat = obfuscate_gps(lat) %>%
#'   # can be made more or less precise by changing the number of decimal points
#'   # included or modifying the amount of fuzz (error) introduced
#'   dplyr::mutate(fuzzed_lon = obfuscate_gps(lon, precision = 4, fuzz = 0.002))
#'
obfuscate_gps <- function(x, precision = 2, fuzz = 0.125){

  # fuzz point
  gps_error <- stats::runif(1,min = -fuzz, max = fuzz)
  x_fuzz <- x + gps_error

  # round to 2 decimal points
  out <- round(x_fuzz,digits = precision)
  return(out)
}

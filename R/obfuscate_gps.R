#' Obfuscate GPS
#'
#' This function fuzzes gps points by first adding
#' error then rounding to a certain number of digits.
#'
#'
#' @param x Numeric. Vector of gps points
#' @param precision Integer. Number of digits to keep. See `round` for more
#' details
#' @param fuzz Numeric. Positive number indicating how much error to introduce
#' to the gps measurements. This is used to generate the random uniform
#' distribution `runif(1,min = -fuzz, max = fuzz)`
#' @param type Character. One of "lat" or "lon"
#' @param func Function. Function used in `get_precision`
#' @param ... Additional arguments for func.
#'
#' @return Numeric. A vector of fuzzed and rounded GPS points
#' @export
#'
#' @examples
#'
#' # make data
#' gps_data  <- data.frame(lat = c(1.0001, 10.22223, 4.00588),
#'                         lon = c(2.39595, 4.506930, -60.09999901))
#'
#' # Default obfuscation settings correspont to roughly a 27 by 27 km area
#' gps_data$lat |>
#'   obfuscate_gps(type = "lat")
#'
#' # Obfuscation can be made more or less precise by changing the number of
#' # decimal points included or modifying the amount of fuzz (error)
#' # introduced
#' gps_data$lon |>
#'   obfuscate_gps(precision = 4, fuzz = 0.002, type = "lon")
#'
#' ### working at the poles
#' gps_data_poles  <- data.frame(lat = c(89.0001, 89.22223, -89.8881),
#'                               lon = c(2.39595, 4.506930, -60.09999901))
#'
#'
#' gps_data_poles$lat |>
#'   obfuscate_gps(fuzz = 1, type = "lat")
#'
#'
#' ### working at the 180th meridian
#' gps_data_180  <- data.frame(lat = c(2, 3, 4),
#'                             lon = c(179.39595, -179.506930, -178.09999901))
#' gps_data_180$lon |>
#'   obfuscate_gps(fuzz = 1, type = "lon")
#'
#' ### GPS is on the fritz!
#' \dontrun{
#' gps_data_fritz <- data.frame(lat = c(91, -91, 90),
#'                              lon = c(181.0001, -181.9877, -178.09999901))
#' gps_data_fritz$lon |>
#'   obfuscate_gps(fuzz = 1, type = "lon")
#'
#' gps_data_fritz$lat |>
#'   obfuscate_gps(fuzz = 1, type = "lat")
#' }
#'
obfuscate_gps <- function(x, precision = 2, fuzz = 0.125, type = c("lat","lon"),
                          func = min, ...){

  ## max precision in your data
  # find value in x with most decimal points
  data_precision <- get_precision(x,func = func,...)

  msg_data_precision <- sprintf("The data have a max precision of: %s",data_precision)
  message(msg_data_precision)

  ## warning for max shift is fuzz+1e-precision
  max_shift <- fuzz+10^-(precision/2)
  msg_max_shift <- sprintf("The max shift from the combination of precision and fuzz is: %s degrees",max_shift)
  message(msg_max_shift)

  if(max_shift/fuzz > 2){
    message("The majority of the obfuscation is coming from rounding, this
    potentially makes re-identification easier")
  }

  ## check if obfuscation will have an impact on the data



  type <- match.arg(type, c("lat","lon"))

  if(type == "lat"){
    out <- obfuscate_lat(x,precision,fuzz)
  }

  if(type == "lon"){
    out <- obfuscate_lon(x,precision,fuzz)
  }

  return(out)
}

obfuscate_point <- function(x, precision = 2, fuzz = 0.125){

  # fuzz point
  gps_error <- stats::runif(1,min = -fuzz, max = fuzz)
  x_fuzz <- x + gps_error

  # round to 2 decimal points
  out <- round(x_fuzz,digits = precision)
  return(out)
}


#' Obfuscates latitude data
#'
#' @rdname obfuscate_gps
#'
#' @return Numeric vector
#' @export
#'
obfuscate_lat <- function(x, precision = 2, fuzz = 0.125){

  ## check that fuzz doesnt exceed maximum values
  if(fuzz > 90){
    stop("fuzz greater than range of latitude on earth")
  }

  if(any(x > 90 | x < -90)){
    stop("Latitude is outside the range of latitude on earth")
  }

  points <- obfuscate_point(x,precision,fuzz)

  # make sure point is between 90 and -90
  points_in_range <- purrr::map_dbl(points,function(point){
    while(all(point > 90 | point < -90)){
      point <- obfuscate_point(point,precision,fuzz)
    }
    return(point)
  })

  return(points_in_range)
}


#' Obfuscates longitude data
#'
#' @rdname obfuscate_gps
#'
#' @return Numeric vector
#' @export
#'
obfuscate_lon <- function(x, precision = 2, fuzz = 0.125){

  ## check that fuzz doesnt exceed maximum values
  if(fuzz > 180){
    stop("fuzz greater than range of longitude on earth")
  }

  if(any(x > 180 | x < -180)){
    stop("Longitude is outside the range of longitude on earth ")
  }

  points <- obfuscate_point(x,precision,fuzz)

  ### wrap points near the 180th meridian
  points_in_range <- purrr::map_dbl(points,function(point){

    # if point greater than 180, wrap
     if(point > 180){
      difference <- point - 180
      point <- -180 + difference
     }

    # if point less than -180, wrap
    if(point < -180){
      difference <- -180-point
      point <- 180 - difference
    }

    return(point)
  })

  return(points_in_range)
}

#' Get Precision
#'
#' @param x Numeric. Vector of gps points
#' @param func Function. Apply some function to the vector of precisions. Default is c so that
#' all values are returned
#' @param ... Additional arguments to pass to func.
#'
#' @return output of func - likely a vector
#' @export
#' @author Nathan Layman
#'
#' @examples
#'
#' x <- c(1,100,1.11)
#' get_precision(x,func = min)
#'
#'
get_precision <- function(x,func = c,...) {
  # number of characters with the decimal - number of characters without it
  precision <- 10^-(nchar(gsub("\\.", "", as.character(x))) - nchar(as.character(trunc(x))))
   out <- func(precision,...)
   return(out)
}

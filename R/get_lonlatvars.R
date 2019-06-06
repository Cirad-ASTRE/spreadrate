#' Identify variables with geographical coordinates
#'
#' Given a data.frame with two variables with longitudes and latitudes,
#' return the variable names.
#'
#' This function explores the names of the variables for the patterns
#' 'lon' or 'long' and 'lat' ignoring case. If exactly one of each
#' patterns is detected, it verifies that their magnitudes are within
#' -365 and 365 for the longitude and within -90.1 and 90.1 for the
#' latitude.
#'
#' @param x data.frame
#'
#' @return A character vector with the variable names for longitude
#'   and latitude, or an error.
#' @export
#'
#' @examples
#'   d <- data.frame(lon = 1:10, lat = 1:10)
#'   get_lonlatvars(d)
get_lonlatvars <- function(x) {

  regs <- c(longitude = "^[Ll]on(g(itude)?)?$", latitude = "^[Ll]at(itude)?$")
  idxs <- lapply(regs, grep, names(x), ignore.case = TRUE)

  ## Check that one and only one variable has been identified for
  ## each dimension
  if (any(test <- vapply(idxs, length, 1) != 1)) {
    stop(
      "Could not identify variables for ",
      paste(names(which(test)), collapse = ", "), ".\n",
      "Coordinates must be named as 'lon[gitude]' and 'lat[itude]' and be
      uniquely identifiable."
    )
  }

  lon_rg <- min(x[[idxs$longitude]] > -365) & max(x[[idxs$longitude]] < 365)
  lat_rg <- min(x[[idxs$latitude]] > -90.1) & max(x[[idxs$latitude]] < 90.1)

  if (! lon_rg & lat_rg) {
    stop("The values of ",
         paste(names(regs)[c(!lon_rg, !lat_rg)], collapse = " and "),
    " are not in the expected range.\n",
    "Make sure you have geographical coordinates in decimal degrees
    with respect to the datum WGS84."
    )
  }

  return(names(x)[unlist(idxs)])

}

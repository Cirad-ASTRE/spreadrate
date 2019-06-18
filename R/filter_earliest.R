
#' Approximate angle for a distance
#'
#' Compute an approximate angle that correspond to a given distance
#' in both longitude and latitude.
#'
#' The distance corresponding to an angle in longitude and latitude
#' depends on the location in the earth and is typically different
#' for both directions.
#'
#' This function computes the angle that yields approximately the
#' target distance when added to the longitude or the latitude by
#' minimising the squared errors.
#'
#' The average error is returned as an attribute.
#'
#' @param x A \code{sfc_POINT} object in longlat coordinates,
#'   indicating a position in the globe.
#' @param dm A target distance in m.
#'
#' @return A numeric value of angle in decimal degrees, such that a
#'   displacement in longitude and latitude by that magnitude
#'   corresponds to approximately the given distance.
#' @export
#'
#' @examples
#'
#' ## What angle yields 1 km at the equator? (and with which precision?)
#' x0 <- st_sfc(st_point(c(0, 0)), crs = 4326)
#' dist2arc(x0, 1e3)
#'
#' ## Note that this is independent of the longitude
#' x1 <- st_sfc(st_point(c(90, 0)), crs = 4326)
#' dist2arc(x1, 1e3)
#'
#' ## But is strongly dependent of the latitude
#' x2 <- st_sfc(st_point(c(0, 45)), crs = 4326)
#' x3 <- st_sfc(st_point(c(0, 75)), crs = 4326)
#' dist2arc(x2, 1e3)
#' dist2arc(x3, 1e3)
dist2arc <- function(x, dm) {

  stopifnot(
    inherits(x, "sfc_POINT"),
    isTRUE(st_is_longlat(x))
  )

  ## Distance in m, given a displacement in degrees (2-vector)
  dist_disp <- function(disp) {
    as.numeric(st_distance(x, st_set_crs(x + disp, st_crs(x))))
  }

  ## Objective function: quadratic error in both directions
  f_d <- function(arc_d) {
    (dist_disp(c(0, arc_d)) - dm)**2 + (dist_disp(c(arc_d, 0)) - dm)**2
  }

  ## Consider positive displacements to the east and the north
  ang <- optimise(f_d, c(0, min(c(181, 91) - st_coordinates(x)[1, ])))

  ## Relative error of a displacement with respect to the desired
  ## distance in m
  avg_err <- sqrt(ang$objective/2)

  return(structure(ang$minimum, average_error = avg_err))
}

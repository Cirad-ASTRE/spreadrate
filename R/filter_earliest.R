#' Choose representative points
#'
#' Select a subset of points at a minimum tolerance distance from
#' each other.
#'
#' @param x Object of class \code{sf}
#' @param dTolerance Numeric. Distance tolerance in m.
#'
#' @return Object of class \code{sf} with a subset of the original
#' locations in \code{x}.
#'
#' @export
#' @import INLA
#'
#' @examples
#'
#' ## 50 points in the unit square
#' x <- st_sfc(st_multipoint(matrix(runif(100), ncol = 2)), crs = 3857)
#' representative_points(x, .5)
representative_points <- function(x, dTolerance) {


  # bnd <- inla.nonconvex.hull(
  #   coord_x,
  #   convex = 0,
  #   concave = -0.15,
  #   resolution = 40
  # )

  ## If x is in geographical coordinates, consider the distance in
  ## decimal degrees, as the mean arc-distance in latitude and longitude
  ## in the centroid of the dataset.
  ## If the approximation is too rough, issue a warning.
  gx <- st_geometry(x)
  coord_x <- st_coordinates(gx)[, 1:2]

  if (isTRUE(st_is_longlat(gx))) {
    ## st_centroid warns about imprecise results with geographical
    ## coordinates. We just want a central point. No need for precision.
    xc <- suppressWarnings(st_centroid(st_union(gx)))
    arcdist <- dist2arc(xc, dTolerance)

    if ( (rel_err <- attr(arcdist, "average_error") / dTolerance) > 2/10 ) {
      warning(
        paste0(
          "Approximating distances with a relative error of ",
          round(rel_err * 100), "%. ",
          "Consider projecting the sr_obs object with st_transform()."
        )
      )
    }

    dTolerance <- as.numeric(arcdist)
  }


  mesh <- inla.mesh.create(
    loc = coord_x,
    boundary = inla.mesh.segment(coord_x[rev(chull(coord_x)),]),
    extend = FALSE,
    refine = FALSE,
    cutoff = dTolerance
  )
  # x %>% ggplot2::ggplot() + inlabru::gg(mesh) + ggplot2::geom_sf()

  ans <-
    mesh$loc[, 1:2] %>%
    st_multipoint() %>%
    st_sfc(crs = st_crs(x)) %>%
    st_cast("POINT") %>%
    st_sf()

  return(ans)
}


#' Neighbourhood Tolerance Parameter
#'
#' Get the value or interval of the Neighbourhood Tolerance Parameter
#' of a \code{sr_obs} object.
#'
#' This function retrieves the values set during creation with
#' \code{sr_obs} in absolute terms, performing the convertion from
#' values relative to the diameter of the dataset if necessary. See
#' \code{\link{sr_uq}}.
#'
#' @param x A \code{sr_obs} object.
#'
#' @return A numeric vector of size 1 or 2. Either a constant value
#' for this parameter or a interval with distance values in m.
#' @export
#'
#' @examples
#'   d <- data.frame(lon = 1:3, lat = 1:3, date = 1:3)
#'
#'   ## Here, the neighbourhood tolerance parameter is set between
#'   ## 2.5 % of the dataset diameter and 1 km.
#'   ## Note that the resulting distances are in m, even if the
#'   ## original coordinates are geographic.
#'   obs <- sr_obs(d, "date", uq = sr_uq(neigh_tol = c(-2.5, 1e4)))
#'   neigh_tol(obs)
neigh_tol <- function(x) {

  stopifnot(inherits(x, "sr_obs"))

  neigh_tol <- attr(x, "uq")$neigh_tol

  ## Handle negative neighbourhood parameters (relative to diameter)
  if (any(idx <- neigh_tol < 0)) {
    idx <- which(idx)
    diameter <- as.numeric(max(st_distance(x)))
    neigh_tol[idx] <- - diameter * neigh_tol[idx] / 100
  }

  return(neigh_tol)
}


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

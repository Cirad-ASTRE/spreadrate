#' First-date of invasion
#'
#' Estimate a surface of first-date of invasion, given a sparse set
#' of observation locations and times.
#'
#' This function first takes the earliest observed case in a neighbourhood,
#' as defined by the parameter \code{neigh_tol} in the definition of
#' \code{x}. Next, it interpolates the earliest observations with a
#' smooth surface, using a Thin-Plate splines model.
#'
#' @param x A \code{sr_obs} object defined with \code{\link{sr_obs}}.
#' @param estimation_mask A \code{raster} template for results.
#'
#' @return A RasterLayer with estimated dates of invasion at each
#'   pixel.
#' @export
#' @import raster
#' @import lwgeom
#'
#' @examples
#' d <- data.frame(lon = runif(30), lat = runif(30))
#' d <- transform(d, date = round(10 * sqrt(lon**2 + lat**2)))
#' sro <- sr_obs(d, "date")
#' r <- raster::raster(sf::st_sf(sro), vals = 1)
#' fdoi <- sr_fdoi(sro, r)
#' raster::plot(fdoi, col = hcl.colors(12))
#' points(d)
sr_fdoi <- function(x, estimation_mask) {

  ## Filter earliest observations by neighbourhood
  x_earliest <- filter_earliest_neigh(x)

  ## Fit interpolation model to earliest observations
  fm <- fit_surface(x_earliest)

  ans <- setNames(interpolate(estimation_mask, fm), attr(x, "timevar"))
  ans[is.na(estimation_mask)] <- NA

  ## Perform the same operations for the Monte Carlo replicas (if any)
  mc <- attr(x_earliest, "mc")

  if (!is.null(mc)) {
    fm_mc <- future_map(mc, fit_surface)

    ans_mc <- future_map(
      fm_mc,
      ~setNames(interpolate(estimation_mask, .), attr(x, "timevar"))
    )

    attr(ans, "mc") <- brick(ans_mc)
  }


  return(ans)
}

#' Fit a Thin-plate splines model to a set of observations
#'
#' @param x A \code{sr_obs} object defined with \code{\link{sr_obs}}.
#'
#' @return An object of class \code{Krig} and \code{Tps} which includes
#' the fitted values and the model residuals. See
#' \code{\link[fields]{Tps}}. It also allows interpolation using
#' \code{\link[raster]{interpolate}}.
#' @export
#' @import fields
#'
#' @examples
#' d <- data.frame(lon = runif(30), lat = runif(30))
#' d <- transform(d, date = round(10 * sqrt(lon**2 + lat**2)))
#' sro <- sr_obs(d, "date")
#' fm <- fit_surface(sro)
#' summary(fm)
#' plot(fm)    # Some model diagnostics plots
#' fields::surface(fm)  # Quick image/contour plot of predicted surface.
fit_surface <- function(x) {
  Tps(
    st_coordinates(x),
    x[[attr(x, "timevar")]],
    give.warning = TRUE
  )
}

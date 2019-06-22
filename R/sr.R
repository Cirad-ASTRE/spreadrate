#' Spread rate
#'
#' Estimate the local spread rate of an epidemiological invasion.
#'
#' This function will compute spread-rate estimates for all of the
#' Monte Carlo samples in the dataset. It uses internally the function
#' \code{\link[furrr]{future_map}} which will take advantage of
#' multiple processors or cluster access if you set a proper
#' \code{\link[future]{plan}} beforehand. See examples.
#'
#' @param x A \code{sr_obs} object defined with \code{\link{sr_obs}}.
#' @param r A \code{raster} template for results.
#'
#' @export
#' @import furrr
#'
#' @examples
#'
#' require(sf)
#' ## Randomly sample 50 point in the unit square
#' x_mat <- matrix(runif(100), ncol = 2)
#'
#' ## Make it spatial point in a projected CRS
#' x_sfc <- st_cast(st_sfc(st_multipoint(x_mat), crs = 3857), "POINT")
#'
#' ## Assign dates proportional to the squared distance to the origin
#' ## So that spread-rate decreases linearly
#' sro <- sr_obs(st_sf(x_sfc, date = sqrt(rowSums(x_mat**2))), "date")
#' sro <- sr_obs(st_sf(x_sfc, date = rowSums(x_mat**2)), "date",
#' uq = sr_uq(6, .2, .2, .3))
#'
#' ## Estimate local spread-rate
#' ## in the units of the coordinates divided by the units of time
#' sre <- sr(sro)
#' plot(sre, col = hcl.colors(12))
#' points(st_coordinates(sro))
#'
sr <- function(
  x,
  r = estimation_mask(
    x,
    buffer_size = max(st_distance(x)) /10
  )
) {

  stopifnot(
    inherits(x, "sr_obs"),
    inherits(r, "RasterLayer")
  )

  timevar <- attr(x, "timevar")

  ## Average spread-rate: half-diameter of the dataset (in m)
  ## divided by total elapsed time in timevar units
  half_diameter <- as.numeric(max(st_distance(x))) / 2
  period_mn <- as.numeric(diff(range(x[[timevar]])))
  avg_sr <- half_diameter / period_mn

  ## Prior spread-rate support (in m/timevar units)
  ## 20 times less or more than average
  sr_bnd <- signif(avg_sr, 2) * c(1/20, 20)

  ## Predicted first-date of invasion from fitted model
  fdoi <- sr_fdoi(x, estimation_mask = r)

  ## Derive spread-rate local estimates from predicted fdoi
  sr <- invslope(fdoi, rmtop = 1, bnd = sr_bnd)

  ## Include the first-date-of-invasion surface as an attribute
  attr(sr, "fdoi") <- fdoi

  ## Perform the same operation for the Monte Carlo
  ## replicas (if any, this is in a RasterBrick)
  fdoi_mc <- attr(fdoi, "mc")
  if (!is.null(fdoi_mc)) {
    attr(sr, "mc") <- invslope(fdoi_mc)
  }

  return(sr)
}

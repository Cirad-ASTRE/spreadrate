#' Compute the inverse slope of the input surface
#'
#' This function is used to derive local spread rate from a
#' first-date-of-invasion surface
#'
#' To control for spurious numerical divergences, the user can remove
#' the largest \code{rmtop} % of the values, and all those
#' values beyond a reasonable interval \code{bnd}.
#'
#' @param x \code{Raster*} with estimated first-arrival date. Needs to be
#'   projected.
#' @param rmtop Numeric, between 0 and 100. Discard the \code{rmtop} top percent
#'   of values.
#' @param bnd Numeric vector of length 2. Expected range of values. Everything
#' beyond this range is filtered.
#'
#' @export
#' @import raster
#' @import furrr
#'
#' @return An object of the same class \code{Raster*} as the input.
#' @examples
#'
#' ## A flat surface with constant slope of 1
#' r_values <- replicate(100, seq(0.01, 1, length = 100))
#' r <- raster::raster(r_values, crs = sp::CRS("+init=epsg:3857"))
#' raster::plot(r)
#' invslope(r)
invslope <- function(x, rmtop, bnd) {

  ## spread-rate-neighbouring
  ## need to use some projection so it understands that the coordinates
  ## are in meters (not in degrees)
  if (isLonLat(x)) {
    stop("Spread-rate computation requires projected coordinates. ",
         "Use st_transform() on your observations, for instance.")
  }
  nl <- nlayers(x)

  x_slope <-
    if (nl > 1 ) {
      brick(
        future_map(
          seq.int(nl),
          ~terrain(x[[.]], unit = "tangent")
        )
      )
    } else {
      terrain(x, unit = "tangent")
    }

  sr_layer_names <- paste("est_fasr", seq.int(nl), sep = ".")
  sr_est <- setNames(1/x_slope, sr_layer_names)

  ## Filter-out the x highest values of spread-rate
  # min_sr <- quantile(values(sr_est), rmtop/100, na.rm = TRUE)
  if (!missing(rmtop)) {
    max_sr <- if (nl > 1 ) {
      apply(values(sr_est), 2, quantile, 1 - rmtop/100, na.rm = TRUE)
    } else {
      quantile(sr_est, 1-rmtop/100, na.rm = TRUE)
    }
  } else {
    max_sr <- Inf
  }

  if (!missing(bnd)) {
    max_sr <- vapply(max_sr, min, 1, bnd[2])
    sr_est[sr_est < bnd[1]] <- bnd[1]
  }

  sr_est[sr_est > max_sr] <- max_sr

  return(sr_est)
}


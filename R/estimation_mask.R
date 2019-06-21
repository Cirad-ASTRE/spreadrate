#' Raster termplate for estimation
#'
#' Given a set of observations, define a estimation template.
#'
#' @param x An object of class \code{sfg}, \code{sfc} or \code{sf},
#' including particularly objects of class \code{sr_obs}. Geographical
#' objects that determine the estimation region.
#' @param buffer_size Numeric. Expansion of the extent around the
#' convex-hull of \code{x}. In the units of the data (e.g. m, or
#' arc-degrees).
#' @param res Numeric vector of length 1 or 2. Resolution of the raster
#' template. Size(s) of the pixel, in metres.
#'
#' @export
#' @import sf
#' @import raster
#'
#' @examples
#'
#' ## Some random points in the unit square
#' pp <- sf::st_multipoint(matrix(runif(30), ncol = 2))
#' (em <- estimation_mask(pp, buffer_size = .3, res = .1))
#' raster::plot(em)
#' points(sf::st_coordinates(pp))
estimation_mask <- function(x, buffer_size, res = buffer_size/8) {

  estimation_region <-
    st_sf(
      st_buffer(
        st_convex_hull(
          st_combine(x)  # many points to 1 multipoint
        ),
        dist = buffer_size
      )
    )

  # estimation_region %>%
  #   ggplot() +
  #   geom_sf() +
  #   geom_sf(data = x)

  ## RasterLayer with values of 1 within the estimation_region
  ## and NA elsewhere
  # estimation_mask <-
  #   fasterize(
  #     estimation_region,
  #     raster(estimation_region, res = as.numeric(buffer_size/2))
  #   )

  estimation_mask <-
    rasterize(
      as(estimation_region, "Spatial"),
      raster(
        as(estimation_region, "Spatial"),
        res = as.numeric(res)
      )
    )

  return(estimation_mask)
}

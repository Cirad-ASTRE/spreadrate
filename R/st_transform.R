#' Transform coordinates of a sr_obs object
#'
#' Transform the underlying \code{sf} object and all the MC replicas,
#' if any.
#'
#' @param x A \code{sr_obs} object created with \code{\link{sr_obs}}.
#' @param crs Coordinate reference system: inteer with the EPSG code
#'   or character with proj4string
#' @param ... Ignored
#'
#' @return A \code{sr_obs} object in the target projection.
#' @export
#' @import sf
#'
#' @examples
#' ## Some observed data and MC replicates in longitude/latitude
#' x <- sr_obs(data.frame(lat = 1, lon = 1, day = 1), "day", uq = sr_uq(2, 1, 1))
#' st_crs(x)
#' lapply(attr(x, "mc"), st_crs)
#'
#' xt <- st_transform(x, 3857)
#' st_crs(xt)
#' lapply(attr(xt, "mc"), st_crs)
st_transform.sr_obs <- function(x, crs, ...) {

  # ans <- sf::st_transform.sf(x, crs, ...)
  ans <- NextMethod()
  x_mc <- attr(x, "mc")

  if (!is.null(x_mc)) {
    attr(ans, "mc") <- lapply(x_mc, st_transform, crs, ...)
  }

  return(ans)
}

#' Default neighbourhood tolerance value
#'
#' Set as 2.5 % of the data diameter.
#'
#' @param x Object of class sr_obs, or more generally sf, sfc or sfg.
#'
#' @return A number in the units of the data.
#' @export
#' @importFrom sf st_distance
#' @examples
#'   p <- st_sfc(st_point(c(0, 0)), st_point(c(0, 100)))
#'   default_neigh_tol(p)
default_neigh_tol <- function(x) {
  diameter <- max(st_distance(x))
  return(diameter / 40)
}


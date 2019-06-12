
#' Define a observations dataset
#'
#' Provide a dataset with the necessary meta-data for spread-rate
#' estimation.
#'
#' If your dataset is projected (no lon/lat variables) then first
#' build the \code{sf} object yourself using the corresponding
#' Coordinate Reference System. See example.
#'
#' This function will produce the Monte Carlo samples from the
#' dataset and the \code{uq} object, if necessary. It uses internally
#' the function \code{\link[furrr]{future_map}} which will take advantage
#' of multiple processors or cluster access if you set a proper
#' \code{\link[future]{plan}} beforehand. See examples.
#'
#' @param x \code{data frame} with geographical coordinates or
#'   \code{sf} object of type POINT.
#' @param timevar Character. Variable name with observation times or
#'   dates.
#' @param uq Object created with \code{sr_uq()} with parameters of
#'   Uncertainty Quantification.
#'
#' @return Object of class \code{sr_obs}, which is a \code{sf} of type
#'   POINT with complementary meta-data.
#' @export
#' @import furrr
#'
#' @examples
#'   d <- data.frame(lon = 1, lat = 1, date = 1)
#'   sr_obs(d, "date")
#'
#'   ## Projected coordinates
#'   d <- data.frame(x = 1, y = 1, date = 1)
#'   d.sf <- sf::st_as_sf(d, coords = c("x", "y"), crs = 27561)
#'   sr_obs(d.sf, "date")
#'
#' \dontrun{
#'   ## Perform Monte Carlo samples in parallel
#'   library(furrr)
#'   plan(multiprocess)
#'   uq <- sr_uq(nsim = 3, space = 1, time = 1)
#'   sr_obs(d, "date", uq = uq)
#' }
#'
sr_obs <- function(x, timevar, uq) UseMethod("sr_obs")

#' @import sf
sr_obs.data.frame <- function(x, timevar, uq = sr_uq()) {

  ## Argument assumptions
  ## Assume there are variables named [Ll]at, [Ll]on[g]
  ## with geographical coordinates wrt datum WGS84
  lonlatvars <- get_lonlatvars(x)  # stops if fails

  xf <- sf::st_as_sf(x, coords = lonlatvars, crs = 4326)

  sr_obs.sf(xf, timevar, uq)

}

#' @import sf
sr_obs.sf <- function(x, timevar, uq = sr_uq()) {

  ## Argument assumptions
  stopifnot(
    !missing(timevar),
    is.character(timevar),
    timevar %in% names(x),
    inherits(sf::st_geometry(x), "sfc_POINT"),
    is.numeric(x[[timevar]]) | inherits(x[[timevar]], "Date"),
    inherits(uq, "sr_uq")
  )

  ## Set timevar and uq attributes and class
  attr(x, "timevar") <- timevar
  attr(x, "uq") <- uq
  class(x) <- c("sr_obs", class(x))

  ## Perform replicates if necessary
  if (uq$nsim > 1) {
    attr(x, "mc") <- mc_sample(x)
  }

  return(x)
}

#' Monte Carlo samples from a dataset
#'
#' Shuffle observations in space and time.
#'
#' Add an attribute \code{mc} with a list of datasets with some noise
#' added to the original coordinates and dates. The number of Monte
#' Carlo samples and the variablity of the noise is given by the
#' \code{sr_uq} attribute in the object.
#'
#' @param x An object of class \code{sr_obs} from function
#'   \code{\link{sr_obs}}
#'
#' @import furrr
#' @import sf
#'
#' @examples
#'   d <- data.frame(lon = 1, lat = 1, date = 1)
#'   sr_obs(d, "date", uq = sr_uq(nsim = 3, space = 1, time = 1))
mc_sample <- function(x) {
  stopifnot(inherits(x, "sr_obs"))

  uq <- attr(x, "uq")

  future_map(seq.int(uq$nsim), ~spacetime_jitter(x, uq))
}


#' Variation in data resolution
spacetime_jitter <- function(x, uq) {
  ans <- st_jitter(x, uq$space)
  ans[[attr(x, "timevar")]] <- time_jitter(x[[attr(x, "timevar")]], uq$time)
  return(ans)
}

#' Modify a vector of dates by a random quantity in [-e, e]
#' The distribution decreases exponentially, with rate 1/2
time_jitter <- function(x, e) {

  prob_p <- 1/2^seq(0, e)
  probs <- c(rev(prob_p), tail(prob_p, -1))
  # probs <- probs/sum(probs)  # unnecessary

  x + sample(seq(-e, e), size = length(x), prob = probs, replace = TRUE)

}

print.sr_obs <- function(x) {
  ## TODO: Provide a summary of the time variable and the UQ
  ## parameters in addition to the regular print of the sf object
  cat("Spread-rate observations\n")
  NextMethod()
}

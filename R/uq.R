#' Parameters for Uncertainty Quantification
#'
#' Define the parameters for quantifying the uncertainty in the
#' estimation of the spread-rate.
#'
#' The actual observations will be The number of Monte Carlo replicates
#' The \code{space} and \code{time} arguments are expressed in the
#' data spatial and temporal units.
#'
#' For each one of the \code{nsim} replicates, the observations will
#' be randomly shifted in space within a circle of radious
#' \code{space}, while the dates will be shifted within the interval
#' +- \code{time} with a discrete approximation to a Gaussian
#' curve.
#'
#' The {neighbouring-tolerance} parameter \code{neigh_tol} is used to
#' filter the earliest observed cases in a \emph{neighbourhood}. If
#' two points are closer than this value, they are considered roughly
#' in the same neighbourhood. The date of invasion of a location is
#' the earliest observed case in the neighbourhood. It can be defined
#' as a numeric value in the units of the coordinates, or as a range
#' (i.e. a numeric vector of length 2). Negative values will be
#' interpreted as a percentage of the diameter of the dataset.
#'
#' @param nsim Integer > 0. Number of Monte Carlo replicates.
#' @param space Numeric, non-negative. Uncertainty of spatial coordinates.
#' @param time Numeric, non-negative. Uncertainty of temporal values.
#' @param neigh_tol Number or interval (numeric vector of length 2).
#'   Neighouring-tolerance parameter in the units of the dataset
#'   coordinates if positive, or relative to the dataset diameter if
#'   negative.
#'
#' @return  An object of class \code{sr_uq} which is a list with the
#'   given or default values, after some sanity checks.
#' @export
#'
#' @examples
#'   ## Use the default values: work with current observations
#'   ## without quantifying uncertainty
#'   uq_def <- sr_uq()
#'
#'   ## Set the neighbouring-tolerance parameter only
#'   uq1 <- sr_uq(neigh_tol = 800)
#'
#'   ## 10 MC replicates, shift locations within a circle of radious
#'   ## 1 km, shift dates by zero/one day up or down, consider points
#'   ## closer than 5% of the diameter of the dataset as in the same
#'   ## neighbourhood
#'   uq <- sr_uq(nsim = 10, space = 1e3, time = 1, neigh_tol = -5)
sr_uq <- function(
  nsim = 1L,
  space = 0,
  time = 0,
  neigh_tol = -4.5
) {

  stopifnot(
    is.numeric(nsim) && nsim >= 1,
    is.numeric(space) && space >= 0,
    is.numeric(time) && time >= 0,
    is.numeric(neigh_tol) && length(neigh_tol) %in% 1:2
  )

  if (nsim > 1 &&
      isTRUE(all.equal(space, 0)) &&
      isTRUE(all.equal(time, 0))
  ) {
    ## Trivial replicates
    nsim <- 1L
    warning("Since both the uncertainties of space and time are 0,
            all the replicated datasets will be identical.\n",
            "Setting nsim = 1. ")
  }

  ## nsim should be Integer
  nsim_int <- as.integer(nsim)
  if (!isTRUE(all.equal(nsim, nsim_int))) {
    warning(
      "nsim must be a non-negative integer. Taking the integer part."
    )
  }

  ## neigh_tol, if negative must be bounded between 0 and -100
  if (any(neg.idx <- neigh_tol < 0)) {
    if (any(neigh_tol[neg.idx] < -100)) {
      stop(
        "If negative, neigh_tol must be between -100 and 0\n",
        "since it is interpreted as a percentage."
      )
    }
  }

  ans <- structure(
    list(
      nsim = nsim_int,
      space = space,
      time = time,
      neigh_tol = neigh_tol
    ),
    class = c("sr_uq", "list")
  )

  return(ans)
}


print.sr_uq <- function(x) {
  ## TODO: Method to print a user-friendly explanation of the values
  ## and their interpretation
  NextMethod(print, x)
}

sr_uq_interactive <- function() {
  ## TODO: Method for interactively asking the user for
  ## uq argument values, while giving explanations and
  ## interpretations.
  message("Functionality not yet implemented. Falling back to sr_uq().")
  sr_uq()
}

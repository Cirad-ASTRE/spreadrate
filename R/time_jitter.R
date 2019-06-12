#' Jitter a vector of 1D values
#'
#' Modify a vector of dates by a random quantity in [-e, e].
#' The distribution decreases exponentially, with rate 1/2
#'
#' @param x Numeric or Date vector.
#' @param e Numeric. Semi-width of the jittering interval.
#'
#' @return Vector of the same size and type as \code{x} with jittered
#' values.
#'
#' @examples
#' time_jitter(1:10, 1)  # Numeric input
#' time_jitter(Sys.Date() + 1:10, 1)  # Date input
#'
#' ## Distribution of jittering
#' x <- seq.int(1e3)
#' xj <- time_jitter(x, 5)
#' barplot(table(x - xj))
time_jitter <- function(x, e) {

  prob_p <- 1/2^seq(0, e)
  probs <- c(rev(prob_p), tail(prob_p, -1))
  # probs <- probs/sum(probs)  # unnecessary

  x + sample(seq(-e, e), size = length(x), prob = probs, replace = TRUE)

}

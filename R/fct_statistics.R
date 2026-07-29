#' Robust Modified Z-Score
#'
#' Median-based analogue of the standard Z-score, more resistant to the
#' influence of extreme values than a mean/SD-based Z-score
#' (@iglewiczHowDetectHandle1993; @leysDetectingOutliersNot2013).
#'
#' @param x A numeric vector.
#' @return A numeric vector of the same length as `x`.
robust_modified_z_score <- function(x) {
  x_median <- median(x, na.rm = TRUE)
  # mad() in R already includes the 1/0.6745 consistency factor
  x_mad <- mad(x, na.rm = TRUE)
  (x - x_median) / x_mad
}

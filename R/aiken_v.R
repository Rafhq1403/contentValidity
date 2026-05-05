#' Aiken's V coefficient of content validity
#'
#' Computes Aiken's V (Aiken, 1985), an index of content validity that uses
#' the full rating scale rather than dichotomizing responses as in I-CVI.
#' Aiken's V ranges from 0 to 1, where 1 indicates all experts gave the
#' maximum rating and 0 indicates all gave the minimum.
#'
#' @param ratings A numeric matrix or data frame of expert ratings (rows =
#'   experts, columns = items). A numeric vector is also accepted, treated
#'   as a single item.
#' @param lo Numeric. Minimum possible rating on the scale. Default 1.
#' @param hi Numeric. Maximum possible rating on the scale. Default 4.
#' @param na.rm Logical. If `TRUE`, missing ratings are excluded.
#'   Defaults to `FALSE`.
#'
#' @return A named numeric vector of V values, one per item. If `ratings`
#'   is a vector, returns a single numeric value.
#'
#' @details
#' Aiken's V is calculated as:
#'
#' \deqn{V = (\bar{X} - lo) / (hi - lo)}
#'
#' where \eqn{\bar{X}} is the mean expert rating across raters, and `lo` and
#' `hi` are the minimum and maximum possible scale values, respectively.
#'
#' A common cutoff is V >= 0.70 for adequate content validity, though
#' stricter thresholds are sometimes applied depending on panel size and
#' research context. Unlike I-CVI, Aiken's V uses the full rating scale, so
#' a rating of 4 contributes more than a rating of 3 (rather than both being
#' counted equally as "relevant").
#'
#' @references
#' Aiken, L. R. (1985). Three coefficients for analyzing the reliability and
#' validity of ratings. *Educational and Psychological Measurement*, 45(1),
#' 131-142. \doi{10.1177/0013164485451012}
#'
#' @examples
#' ratings <- matrix(
#'   c(4, 4, 3, 4, 4,
#'     3, 4, 4, 4, 3,
#'     2, 3, 3, 4, 3,
#'     1, 2, 3, 2, 3),
#'   nrow = 5,
#'   dimnames = list(NULL, paste0("item", 1:4))
#' )
#' aiken_v(ratings)
#'
#' # 5-point scale
#' aiken_v(c(5, 4, 5, 5, 4), lo = 1, hi = 5)
#'
#' @seealso [icvi()]
#' @export
aiken_v <- function(ratings, lo = 1, hi = 4, na.rm = FALSE) {

  if (missing(ratings)) {
    stop("`ratings` is required.", call. = FALSE)
  }

  if (is.data.frame(ratings)) {
    ratings <- as.matrix(ratings)
  }

  if (!is.numeric(ratings)) {
    stop("`ratings` must be numeric.", call. = FALSE)
  }

  if (!is.numeric(lo) || length(lo) != 1) {
    stop("`lo` must be a single number.", call. = FALSE)
  }

  if (!is.numeric(hi) || length(hi) != 1) {
    stop("`hi` must be a single number.", call. = FALSE)
  }

  if (hi <= lo) {
    stop("`hi` must be greater than `lo`.", call. = FALSE)
  }

  if (!is.logical(na.rm) || length(na.rm) != 1) {
    stop("`na.rm` must be TRUE or FALSE.", call. = FALSE)
  }

  # Range check on the supplied data
  if (any(ratings < lo | ratings > hi, na.rm = TRUE)) {
    stop("Some ratings are outside the [lo, hi] range.", call. = FALSE)
  }

  one_item <- function(item) {
    if (na.rm) {
      item <- item[!is.na(item)]
    }
    if (anyNA(item) || length(item) == 0) return(NA_real_)
    (mean(item) - lo) / (hi - lo)
  }

  if (is.null(dim(ratings))) {
    return(one_item(ratings))
  }

  apply(ratings, 2, one_item)
}

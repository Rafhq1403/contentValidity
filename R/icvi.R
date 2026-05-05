#' Item-level Content Validity Index (I-CVI)
#'
#' Computes the Item-level Content Validity Index (I-CVI) for one or more items
#' rated by a panel of experts on a relevance scale. Following Lynn (1986) and
#' Polit & Beck (2006), I-CVI is calculated as the proportion of experts who
#' rate an item as 3 (relevant) or 4 (highly relevant) on a 4-point relevance
#' scale.
#'
#' @param ratings A numeric matrix or data frame of expert ratings, where rows
#'   represent experts and columns represent items. Values are typically on a
#'   1-4 relevance scale. A numeric vector is also accepted, treated as a
#'   single item.
#' @param relevant_threshold Integer. The minimum rating considered "relevant".
#'   Defaults to 3 (i.e., ratings of 3 or 4 count as relevant on a 4-point
#'   scale).
#' @param na.rm Logical. If `TRUE`, missing ratings are excluded from the
#'   calculation. Defaults to `FALSE`, in which case any `NA` produces `NA`
#'   for the affected item.
#'
#' @return A named numeric vector of I-CVI values, one per item. If `ratings`
#'   is a vector, returns a single numeric value.
#'
#' @details
#' Common interpretation guidelines (Polit & Beck, 2006):
#'
#' - I-CVI >= 0.78: excellent content validity (with 6 or more experts).
#' - I-CVI 0.70-0.78: acceptable, item may need revision.
#' - I-CVI < 0.70: item should be revised or eliminated.
#'
#' With fewer than six experts, Lynn (1986) recommends a stricter cutoff of
#' I-CVI = 1.00 for unanimous agreement.
#'
#' @references
#' Lynn, M. R. (1986). Determination and quantification of content validity.
#' *Nursing Research*, 35(6), 382-385.
#' \doi{10.1097/00006199-198611000-00017}
#'
#' Polit, D. F., & Beck, C. T. (2006). The content validity index: Are you
#' sure you know what's being reported? Critique and recommendations.
#' *Research in Nursing & Health*, 29(5), 489-497.
#' \doi{10.1002/nur.20147}
#'
#' @examples
#' # Five experts rating four items on a 1-4 relevance scale
#' ratings <- matrix(
#'   c(4, 4, 3, 4, 4,    # Item 1
#'     3, 4, 4, 4, 3,    # Item 2
#'     2, 3, 3, 4, 3,    # Item 3
#'     1, 2, 3, 2, 3),   # Item 4
#'   nrow = 5,
#'   dimnames = list(NULL, paste0("item", 1:4))
#' )
#' icvi(ratings)
#'
#' # Single item supplied as a vector
#' icvi(c(4, 4, 3, 3, 4))
#'
#' # Stricter threshold (only highest rating counts as relevant)
#' icvi(ratings, relevant_threshold = 4)
#'
#' @export
icvi <- function(ratings, relevant_threshold = 3, na.rm = FALSE) {

  if (missing(ratings)) {
    stop("`ratings` is required.", call. = FALSE)
  }

  if (is.data.frame(ratings)) {
    ratings <- as.matrix(ratings)
  }

  if (!is.numeric(ratings)) {
    stop("`ratings` must be numeric.", call. = FALSE)
  }

  if (!is.numeric(relevant_threshold) || length(relevant_threshold) != 1) {
    stop("`relevant_threshold` must be a single number.", call. = FALSE)
  }

  if (!is.logical(na.rm) || length(na.rm) != 1) {
    stop("`na.rm` must be TRUE or FALSE.", call. = FALSE)
  }

  # Vector input — treat as a single item
  if (is.null(dim(ratings))) {
    relevant <- ratings >= relevant_threshold
    return(mean(relevant, na.rm = na.rm))
  }

  # Matrix input — compute per column (item)
  apply(ratings, 2, function(item) {
    relevant <- item >= relevant_threshold
    mean(relevant, na.rm = na.rm)
  })
}

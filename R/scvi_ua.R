#' Scale-level Content Validity Index, Universal Agreement method (S-CVI/UA)
#'
#' Computes the Scale-level Content Validity Index using the universal
#' agreement method, defined as the proportion of items where all experts
#' rate the item as relevant.
#'
#' @param ratings A numeric matrix or data frame of expert ratings (rows =
#'   experts, columns = items) on a relevance scale.
#' @param relevant_threshold Integer. Minimum rating considered "relevant".
#'   Defaults to 3.
#' @param na.rm Logical. If `TRUE`, missing ratings are ignored when checking
#'   universal agreement. Defaults to `FALSE`.
#'
#' @return A single numeric value: the proportion of items with universal
#'   agreement.
#'
#' @details
#' S-CVI/UA is a stricter criterion than S-CVI/Ave and tends to produce lower
#' values, especially with larger expert panels. Polit and Beck (2006)
#' recommend reporting both indices together. With small panels of 3-5
#' experts, S-CVI/UA >= 0.80 is often considered acceptable.
#'
#' @references
#' Polit, D. F., & Beck, C. T. (2006). The content validity index: Are you
#' sure you know what's being reported? Critique and recommendations.
#' *Research in Nursing & Health*, 29(5), 489-497.
#' \doi{10.1002/nur.20147}
#'
#' @examples
#' ratings <- matrix(
#'   c(4, 4, 3, 4, 4,
#'     3, 4, 4, 4, 3,
#'     2, 3, 3, 4, 3,
#'     1, 2, 3, 2, 3),
#'   nrow = 5
#' )
#' scvi_ua(ratings)
#'
#' @seealso [icvi()], [scvi_ave()]
#' @export
scvi_ua <- function(ratings, relevant_threshold = 3, na.rm = FALSE) {

  if (missing(ratings)) {
    stop("`ratings` is required.", call. = FALSE)
  }

  if (is.data.frame(ratings)) {
    ratings <- as.matrix(ratings)
  }

  if (!is.numeric(ratings)) {
    stop("`ratings` must be numeric.", call. = FALSE)
  }

  if (is.null(dim(ratings))) {
    stop("`ratings` must be a matrix or data frame with multiple items. ",
         "S-CVI is undefined for a single item.", call. = FALSE)
  }

  if (!is.numeric(relevant_threshold) || length(relevant_threshold) != 1) {
    stop("`relevant_threshold` must be a single number.", call. = FALSE)
  }

  if (!is.logical(na.rm) || length(na.rm) != 1) {
    stop("`na.rm` must be TRUE or FALSE.", call. = FALSE)
  }

  # For each item (column), TRUE if every expert rated it relevant
  unanimous <- apply(ratings, 2, function(item) {
    all(item >= relevant_threshold, na.rm = na.rm)
  })

  mean(unanimous, na.rm = na.rm)
}

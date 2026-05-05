#' Scale-level Content Validity Index, Average method (S-CVI/Ave)
#'
#' Computes the Scale-level Content Validity Index using the averaging method,
#' defined as the mean of the Item-level Content Validity Indices (I-CVI)
#' across all items in the instrument.
#'
#' @param ratings A numeric matrix or data frame of expert ratings (rows =
#'   experts, columns = items) on a relevance scale.
#' @param relevant_threshold Integer. Minimum rating considered "relevant".
#'   Defaults to 3.
#' @param na.rm Logical. Passed through to [icvi()]. Defaults to `FALSE`.
#'
#' @return A single numeric value: the average I-CVI across items.
#'
#' @details
#' S-CVI/Ave >= 0.90 is generally considered excellent content validity at the
#' scale level (Polit & Beck, 2006). Note that S-CVI is undefined for a single
#' item; supply a matrix or data frame with two or more item columns.
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
#' scvi_ave(ratings)
#'
#' @seealso [icvi()]
#' @export
scvi_ave <- function(ratings, relevant_threshold = 3, na.rm = FALSE) {
  
  # Reject vector input — S-CVI only makes sense for a scale (multiple items)
  if (is.null(dim(ratings))) {
    stop("`ratings` must be a matrix or data frame with multiple items. ",
         "S-CVI is undefined for a single item.", call. = FALSE)
  }
  
  # Compute the per-item I-CVIs by reusing icvi()
  item_cvis <- icvi(ratings,
                    relevant_threshold = relevant_threshold,
                    na.rm = na.rm)
  
  # Return the mean across items
  mean(item_cvis, na.rm = na.rm)
  
}
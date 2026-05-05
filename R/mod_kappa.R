#' Modified kappa - I-CVI adjusted for chance agreement
#'
#' Computes modified kappa for each item, as proposed by Polit, Beck,
#' and Owen (2007). Modified kappa adjusts the Item-level Content Validity
#' Index (I-CVI) for chance agreement under the assumption that each expert
#' independently rates an item as relevant with probability 0.5.
#'
#' @param ratings A numeric matrix or data frame of expert ratings (rows =
#'   experts, columns = items). A numeric vector is also accepted, treated
#'   as a single item.
#' @param relevant_threshold Integer. Minimum rating considered "relevant".
#'   Defaults to 3.
#' @param na.rm Logical. If `TRUE`, missing ratings are excluded when
#'   counting experts and agreements. Defaults to `FALSE`.
#'
#' @return A named numeric vector of modified-kappa values, one per item. If `ratings`
#'   is a vector, returns a single numeric value.
#'
#' @details
#' The formula is:
#'
#' \deqn{\kappa^* = (\mathrm{I\text{-}CVI} - P_c) / (1 - P_c)}
#'
#' where the chance agreement probability is
#'
#' \deqn{P_c = \binom{N}{A} \times 0.5^N}
#'
#' with N = number of experts and A = number of experts rating the item as
#' relevant.
#'
#' Common interpretation cutoffs (Cicchetti and Sparrow, 1981; adopted by
#' Polit et al., 2007):
#'
#' - kappa* < 0.40: poor
#' - kappa* 0.40-0.59: fair
#' - kappa* 0.60-0.74: good
#' - kappa* > 0.74: excellent
#'
#' @references
#' Cicchetti, D. V., & Sparrow, S. A. (1981). Developing criteria for
#' establishing interrater reliability of specific items: Applications to
#' assessment of adaptive behavior. *American Journal of Mental Deficiency*,
#' 86(2), 127-137.
#'
#' Polit, D. F., Beck, C. T., & Owen, S. V. (2007). Is the CVI an acceptable
#' indicator of content validity? Appraisal and recommendations.
#' *Research in Nursing & Health*, 30(4), 459-467.
#' \doi{10.1002/nur.20199}
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
#' mod_kappa(ratings)
#'
#' @seealso [icvi()]
#' @export
mod_kappa <- function(ratings, relevant_threshold = 3, na.rm = FALSE) {

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

  # Helper: compute modified kappa for a single item (vector of expert ratings)
  one_item <- function(item) {
    if (na.rm) {
      item <- item[!is.na(item)]
    }
    if (anyNA(item)) return(NA_real_)
    N <- length(item)
    if (N == 0) return(NA_real_)
    A <- sum(item >= relevant_threshold)
    icvi_val <- A / N
    Pc <- choose(N, A) * 0.5^N
    if (Pc >= 1) return(NA_real_)
    (icvi_val - Pc) / (1 - Pc)
  }

  if (is.null(dim(ratings))) {
    return(one_item(ratings))
  }

  apply(ratings, 2, one_item)
}

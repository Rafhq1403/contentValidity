#' Example expert ratings for content validity analysis
#'
#' A simulated dataset illustrating typical expert ratings during the content
#' validation of a 10-item depression screening instrument. Six expert
#' clinicians rate each item's relevance on a 4-point scale.
#'
#' The pattern of ratings is realistic: some items achieve universal
#' agreement, most show strong but imperfect agreement, and a couple of
#' items would be flagged for revision based on standard CVI cutoffs
#' (e.g., items 5 and 9 in this example).
#'
#' @format A 6 by 10 numeric matrix with rows representing expert raters
#'   (`expert1` through `expert6`) and columns representing candidate items
#'   (`item1` through `item10`). Values are on a 4-point relevance scale:
#'
#' - 1: not relevant
#' - 2: somewhat relevant (item needs major revision)
#' - 3: quite relevant (item needs minor revision)
#' - 4: highly relevant
#'
#' @source Simulated for demonstration; not based on real expert ratings.
#'
#' @examples
#' data(cvi_example)
#' icvi(cvi_example)
#' content_validity(cvi_example)
"cvi_example"

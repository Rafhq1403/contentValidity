#' contentValidity: Content Validity Indices for Instrument Development
#'
#' The `contentValidity` package provides functions for computing content
#' validity indices used in questionnaire and instrument development,
#' including the Item-level and Scale-level Content Validity Indices,
#' modified kappa, Aiken's V, and Lawshe's CVR with corrected critical
#' values.
#'
#' @section Main functions:
#' - [icvi()]: Item-level Content Validity Index
#' - [scvi_ave()]: Scale-level CVI, average method
#' - [scvi_ua()]: Scale-level CVI, universal agreement method
#' - [mod_kappa()]: Modified kappa adjusted for chance agreement
#' - [aiken_v()]: Aiken's V coefficient
#' - [cvr()]: Lawshe's Content Validity Ratio
#' - [cvr_critical()]: Critical CVR values (Wilson, Pan & Schumsky 2012)
#'
#' @keywords internal
#' @importFrom stats pbinom
"_PACKAGE"

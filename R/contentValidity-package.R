#' contentValidity: Content Validity Indices for Instrument Development
#'
#' The `contentValidity` package provides functions for computing content
#' validity indices used in questionnaire and instrument development,
#' along with bootstrap confidence intervals, sample-size planning, and
#' publication-ready reporting tools. Methods follow Lynn (1986), Polit
#' and Beck (2006), Polit, Beck, and Owen (2007), Aiken (1985), Lawshe
#' (1975) with the corrected critical values of Wilson, Pan, and
#' Schumsky (2012), and Gwet (2008, 2014).
#'
#' @section Item-level indices:
#' - [icvi()]: Item-level Content Validity Index
#' - [mod_kappa()]: Modified kappa adjusted for chance agreement
#' - [aiken_v()]: Aiken's V coefficient
#' - [gwet_ac1()]: Gwet's AC1 chance-corrected agreement (binary)
#' - [gwet_ac2()]: Gwet's AC2 weighted chance-corrected agreement (ordinal)
#'
#' @section Scale-level indices:
#' - [scvi_ave()]: Scale-level CVI, average method
#' - [scvi_ua()]: Scale-level CVI, universal agreement method
#'
#' @section Lawshe's Content Validity Ratio:
#' - [cvr()]: Lawshe's CVR
#' - [cvr_critical()]: Critical CVR values (Wilson, Pan & Schumsky 2012)
#'
#' @section Inference and planning:
#' - All indices above support optional bootstrap confidence intervals via
#'   `ci = TRUE`.
#' - [cv_sample_size_icvi()]: minimum number of expert raters for a
#'   target I-CVI confidence-interval half-width.
#'
#' @section Reporting and visualization:
#' - [content_validity()]: one-call wrapper returning all indices,
#'   supporting multi-dimensional / subscale analysis.
#' - [apa_table()]: publication-ready APA-style tables with per-index
#'   interpretation.
#' - [plot.content_validity()]: I-CVI vs. agreement-index scatter with
#'   configurable flagging logic.
#'
#' @keywords internal
#' @importFrom stats pbinom qnorm quantile uniroot setNames pnorm
#' @importFrom graphics abline legend text
#' @importFrom grDevices dev.off
"_PACKAGE"

#' APA-style content validity table
#'
#' Generates a publication-ready content validity table following APA
#' conventions, suitable for inclusion in journal manuscripts, theses, and
#' technical reports. Returns a clean data frame by default, with optional
#' rendering to markdown, HTML, or LaTeX via [knitr::kable()].
#'
#' @param x An object to format. Currently supports objects of class
#'   `"content_validity"` returned by [content_validity()].
#' @param ... Further arguments passed to methods.
#'
#' @return A data frame (when `format = "data.frame"`) or a character
#'   string suitable for inclusion in an R Markdown document (other formats).
#'
#' @export
apa_table <- function(x, ...) {
  UseMethod("apa_table")
}

#' @rdname apa_table
#'
#' @param format Output format. One of `"data.frame"` (default),
#'   `"markdown"`, `"html"`, `"latex"`, or `"pipe"`. All formats other than
#'   `"data.frame"` require the `knitr` package.
#' @param digits Integer. Number of decimal places for numeric values.
#'   Default 2 (APA convention for proportions and correlations).
#' @param interpretation Logical. Whether to include an Interpretation
#'   column based on modified-kappa cutoffs (Cicchetti & Sparrow, 1981).
#'   Default `TRUE`.
#' @param caption Optional character string. The caption to use when format
#'   is not `"data.frame"`. If `NULL` (default), a standard caption is
#'   generated that reports the scale-level indices.
#'
#' @details
#' Item-level interpretation labels follow the modified-kappa cutoffs of
#' Cicchetti and Sparrow (1981), as adopted by Polit, Beck, and Owen (2007):
#'
#' - Excellent: κ* > 0.74
#' - Good: κ* 0.60 to 0.74
#' - Fair: κ* 0.40 to 0.59
#' - Poor: κ* < 0.40
#'
#' Scale-level indices are reported in the caption rather than the table
#' body, matching the typical layout used in nursing, education, and
#' health-sciences journals.
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
#' data(cvi_example)
#' result <- content_validity(cvi_example)
#'
#' # Default: a clean data frame
#' apa_table(result)
#'
#' # Markdown for R Markdown documents
#' if (requireNamespace("knitr", quietly = TRUE)) {
#'   apa_table(result, format = "markdown")
#' }
#'
#' @export
apa_table.content_validity <- function(x,
                                        format = c("data.frame", "markdown",
                                                   "html", "latex", "pipe"),
                                        digits = 2,
                                        interpretation = TRUE,
                                        caption = NULL,
                                        ...) {

  format <- match.arg(format)

  classify <- function(k) {
    if (is.na(k)) {
      "-"
    } else if (k > 0.74) {
      "Excellent"
    } else if (k >= 0.60) {
      "Good"
    } else if (k >= 0.40) {
      "Fair"
    } else {
      "Poor"
    }
  }

  df <- data.frame(
    Item                 = x$items$item,
    `I-CVI`              = round(x$items$icvi, digits),
    `Modified Kappa`     = round(x$items$mod_kappa, digits),
    `Aiken's V`          = round(x$items$aiken_v, digits),
    check.names          = FALSE,
    stringsAsFactors     = FALSE
  )

  if (interpretation) {
    df$Interpretation <- vapply(x$items$mod_kappa, classify, character(1))
  }

  if (format == "data.frame") {
    return(df)
  }

  if (!requireNamespace("knitr", quietly = TRUE)) {
    stop("The `knitr` package is required for format = '", format, "'. ",
         "Install it with install.packages('knitr').", call. = FALSE)
  }

  if (is.null(caption)) {
    caption <- sprintf(
      "Content validity indices (N = %d experts, %d items; S-CVI/Ave = %.2f, S-CVI/UA = %.2f).",
      x$n_experts,
      x$n_items,
      x$scale[["scvi_ave"]],
      x$scale[["scvi_ua"]]
    )
  }

  kable_format <- if (format == "markdown") "pipe" else format

  knitr::kable(
    df,
    format  = kable_format,
    caption = caption,
    digits  = digits,
    align   = c("l", rep("r", ncol(df) - 1L - as.integer(interpretation)),
                if (interpretation) "l" else NULL),
    ...
  )
}

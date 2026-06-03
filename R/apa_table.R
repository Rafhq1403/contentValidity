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
#' @param interpretation Logical. Whether to include an interpretation
#'   column. Default `TRUE`. The cutoffs depend on
#'   `interpretation_index`.
#' @param interpretation_index Character. Which index drives the
#'   interpretation column. One of `"mod_kappa"` (default;
#'   Cicchetti & Sparrow, 1981; Polit, Beck, & Owen, 2007),
#'   `"gwet_ac1"` (Altman, 1991), `"gwet_ac2"` (Altman, 1991), or
#'   `"icvi"` (Polit & Beck, 2006). The resulting column is named
#'   accordingly (e.g., "Kappa Interpretation",
#'   "AC1 Interpretation") so that the labels are not confused
#'   with the other columns in the table.
#' @param caption Optional character string. The caption to use when format
#'   is not `"data.frame"`. If `NULL` (default), a standard caption is
#'   generated that reports the scale-level indices.
#'
#' @details
#' Item-level interpretation labels follow the modified-kappa cutoffs of
#' Cicchetti and Sparrow (1981), as adopted by Polit, Beck, and Owen (2007):
#'
#' - Excellent: kappa* > 0.74
#' - Good: kappa* 0.60 to 0.74
#' - Fair: kappa* 0.40 to 0.59
#' - Poor: kappa* < 0.40
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
                                        interpretation_index = c("mod_kappa",
                                                                  "gwet_ac1",
                                                                  "gwet_ac2",
                                                                  "icvi"),
                                        caption = NULL,
                                        ...) {

  format <- match.arg(format)
  interpretation_index <- match.arg(interpretation_index)

  # Kappa and AC cutoffs follow Cicchetti & Sparrow (1981) /
  # Altman (1991); I-CVI cutoffs follow Polit & Beck (2006).
  classify_kappa <- function(k) {
    if (is.na(k))       "-"
    else if (k > 0.74)  "Excellent"
    else if (k >= 0.60) "Good"
    else if (k >= 0.40) "Fair"
    else                "Poor"
  }

  classify_ac <- function(a) {
    if (is.na(a))       "-"
    else if (a > 0.80)  "Very good"
    else if (a >= 0.60) "Good"
    else if (a >= 0.40) "Moderate"
    else if (a >= 0.20) "Fair"
    else                "Poor"
  }

  classify_icvi <- function(p) {
    if (is.na(p))       "-"
    else if (p >= 0.78) "Excellent"
    else if (p >= 0.70) "Acceptable"
    else                "Revise/discard"
  }

  df <- data.frame(
    Item                 = x$items$item,
    `I-CVI`              = round(x$items$icvi, digits),
    `Modified Kappa`     = round(x$items$mod_kappa, digits),
    `Aiken's V`          = round(x$items$aiken_v, digits),
    check.names          = FALSE,
    stringsAsFactors     = FALSE
  )

  # AC1 and AC2 columns were added in v0.2.0. They are only included in
  # the table if the underlying content_validity object carries them
  # (i.e. v0.2.0 wrapper output), preserving compatibility with any
  # downstream code that constructed content_validity objects manually
  # under v0.1.0.
  if (!is.null(x$items$gwet_ac1)) {
    df$`Gwet's AC1` <- round(x$items$gwet_ac1, digits)
  }
  if (!is.null(x$items$gwet_ac2)) {
    df$`Gwet's AC2` <- round(x$items$gwet_ac2, digits)
  }

  if (interpretation) {
    # Pick the right classifier, column header, and source-column
    # display name for the requested interpretation index. Fall back
    # gracefully if the requested column is missing (e.g. v0.1.0
    # content_validity objects with no AC1/AC2).
    classifier <- switch(
      interpretation_index,
      mod_kappa = classify_kappa,
      gwet_ac1  = classify_ac,
      gwet_ac2  = classify_ac,
      icvi      = classify_icvi
    )
    column_name <- switch(
      interpretation_index,
      mod_kappa = "Kappa Interpretation",
      gwet_ac1  = "AC1 Interpretation",
      gwet_ac2  = "AC2 Interpretation",
      icvi      = "I-CVI Interpretation"
    )
    source_display <- switch(
      interpretation_index,
      mod_kappa = "Modified Kappa",
      gwet_ac1  = "Gwet's AC1",
      gwet_ac2  = "Gwet's AC2",
      icvi      = "I-CVI"
    )
    source_values <- x$items[[interpretation_index]]
    if (is.null(source_values)) {
      stop("Interpretation requested for `", interpretation_index,
           "` but the content_validity object does not carry that ",
           "column. Either supply a v0.2.0 content_validity object or ",
           "choose a different `interpretation_index`.", call. = FALSE)
    }
    interp_values <- vapply(source_values, classifier, character(1))

    # Insert the interpretation column immediately after its source
    # index column, not at the end of the table. This avoids the
    # misleading layout where a generic "Interpretation" column sat
    # next to an unrelated numeric column.
    source_idx <- match(source_display, names(df))
    if (is.na(source_idx)) {
      # Shouldn't happen for v0.2.0 objects, but fall back to appending
      df[[column_name]] <- interp_values
    } else {
      interp_df <- stats::setNames(
        data.frame(interp_values, stringsAsFactors = FALSE),
        column_name
      )
      df <- cbind(
        df[, seq_len(source_idx), drop = FALSE],
        interp_df,
        df[, -seq_len(source_idx), drop = FALSE],
        stringsAsFactors = FALSE
      )
    }
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

  # Build a column-type-aware alignment spec: left-align character
  # columns (Item, Interpretation), right-align numeric columns
  # (proportions and coefficients).
  align_spec <- vapply(df, function(col) {
    if (is.numeric(col)) "r" else "l"
  }, character(1))

  knitr::kable(
    df,
    format  = kable_format,
    caption = caption,
    digits  = digits,
    align   = unname(align_spec),
    ...
  )
}

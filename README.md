# contentValidity

<!-- badges: start -->
[![R-CMD-check](https://github.com/Rafhq1403/contentValidity/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Rafhq1403/contentValidity/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/contentValidity)](https://CRAN.R-project.org/package=contentValidity)
<!-- badges: end -->

`contentValidity` provides functions for computing content validity indices
used in questionnaire and instrument development. It is intended for
researchers in education, psychology, nursing, health sciences, and other
fields where new measurement instruments must be evaluated by expert panels.

## Features

- **I-CVI** -- Item-level Content Validity Index (Lynn, 1986)
- **S-CVI/Ave** -- Scale-level CVI, average method (Polit & Beck, 2006)
- **S-CVI/UA** -- Scale-level CVI, universal agreement (Polit & Beck, 2006)
- **Modified kappa** -- I-CVI adjusted for chance agreement (Polit, Beck, & Owen, 2007)
- **Aiken's V** -- coefficient using the full rating scale (Aiken, 1985)
- **Lawshe's CVR** -- Content Validity Ratio (Lawshe, 1975) with corrected
  critical values from Wilson, Pan, and Schumsky (2012)
- **One-call summary** via `content_validity()` returning all indices in
  a tidy structure
- **Publication-ready APA tables** via `apa_table()`, supporting data
  frame, markdown, HTML, and LaTeX output

## Installation

You can install the development version from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("Rafhq1403/contentValidity")
```

## Quick example

``` r
library(contentValidity)

data(cvi_example)
result <- content_validity(cvi_example)
result
#> Content Validity Analysis
#> -------------------------
#> Experts: 6
#> Items:   10
#>
#> Item-level indices:
#>    item   icvi mod_kappa aiken_v
#>   item1 1.0000    1.0000  1.0000
#>   item2 1.0000    1.0000  0.8889
#>   ...
#>
#> Scale-level indices:
#>   scvi_ave    scvi_ua mean_kappa
#>     0.8833     0.6000     0.8470

# Publication-ready table for journal manuscripts
apa_table(result, format = "markdown")
```

## Citation

After installing the package, run `citation("contentValidity")` for a
copy-paste citation in BibTeX or plain-text form.

## License

MIT (c) 2026 Rashed Alqahtani. See the
[LICENSE](https://github.com/Rafhq1403/contentValidity/blob/master/LICENSE.md)
file for details.

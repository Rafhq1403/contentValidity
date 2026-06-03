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

### Content validity indices

- **I-CVI** -- Item-level Content Validity Index (Lynn, 1986)
- **S-CVI/Ave** -- Scale-level CVI, average method (Polit & Beck, 2006)
- **S-CVI/UA** -- Scale-level CVI, universal agreement (Polit & Beck, 2006)
- **Modified kappa** -- I-CVI adjusted for chance agreement (Polit, Beck, & Owen, 2007)
- **Aiken's V** -- coefficient using the full rating scale (Aiken, 1985)
- **Lawshe's CVR** -- Content Validity Ratio (Lawshe, 1975) with corrected
  critical values from Wilson, Pan, and Schumsky (2012)
- **Gwet's AC1** -- binary chance-corrected agreement (Gwet, 2008) — new in v0.2.0
- **Gwet's AC2** -- ordinal weighted chance-corrected agreement, matching
  `irrCAC::gwet.ac1.raw()` (Gwet, 2008, 2014) — new in v0.2.0

### Inference and planning

- **Bootstrap confidence intervals** for every index, percentile and BCa
  methods, expert-level resampling — new in v0.2.0
- **Sample-size planning** for I-CVI estimation via Wald and Wilson
  methods (`cv_sample_size_icvi()`) — new in v0.2.0

### Convenience

- **One-call summary** via `content_validity()` returning all eight indices
  in a tidy structure
- **Multi-dimensional / subscale support** via the `subscale` argument
  for instruments structured into domains — new in v0.2.0
- **Publication-ready APA tables** via `apa_table()`, supporting data
  frame, markdown, HTML, and LaTeX output, with selectable
  per-index interpretation columns
- **S3 plot method** for the difficulty-discrimination-style scatter
  diagram, with selectable y-axis index and automatic flagging
  of items outside the adequacy region — new in v0.2.0

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
#>    item   icvi mod_kappa aiken_v gwet_ac1 gwet_ac2
#>   item1 1.0000    1.0000  1.0000   1.0000   1.0000
#>   item2 1.0000    1.0000  0.8889   1.0000   0.8964
#>   ...
#>
#> Scale-level indices (overall):
#>   scvi_ave    scvi_ua mean_kappa   mean_ac1   mean_ac2
#>     0.8833     0.6000     0.8470     0.6917     0.8864

# Publication-ready table for journal manuscripts
apa_table(result, format = "markdown")

# Visualize the item map
plot(result, y_index = "gwet_ac2")

# Sample-size planning
cv_sample_size_icvi(expected = 0.85, half_width = 0.10)
#> [1] 49

# Bootstrap confidence intervals
icvi(cvi_example, ci = TRUE, n_boot = 1000, seed = 1)
```

## Citation

After installing the package, run `citation("contentValidity")` for a
copy-paste citation in BibTeX or plain-text form.

## License

MIT (c) 2026 Rashed Alqahtani. See the
[LICENSE](https://github.com/Rafhq1403/contentValidity/blob/master/LICENSE.md)
file for details.

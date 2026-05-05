# contentValidity

<!-- badges: start -->
<!-- badges: end -->

`contentValidity` is an R package for computing content validity indices
used in questionnaire and instrument development. It is intended for
researchers in education, psychology, nursing, health sciences, and other
fields where new measurement instruments must be evaluated by expert panels.

## Planned scope

- **I-CVI** — Item-level Content Validity Index *(implemented)*
- **S-CVI/Ave** — Scale-level CVI, average method *(planned)*
- **S-CVI/UA** — Scale-level CVI, universal agreement *(planned)*
- **Modified kappa (κ\*)** — I-CVI adjusted for chance agreement *(planned)*
- **Aiken's V** *(planned)*
- **Lawshe's CVR** with updated critical values *(planned)*

## Installation

The package is in early development. You can install the development
version from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("Rafhq1403/contentValidity")
```

## Quick example

``` r
library(contentValidity)

# Five experts rate four items on a 1-4 relevance scale
ratings <- matrix(
  c(4, 4, 3, 4, 4,
    3, 4, 4, 4, 3,
    2, 3, 3, 4, 3,
    1, 2, 3, 2, 3),
  nrow = 5,
  dimnames = list(NULL, paste0("item", 1:4))
)

icvi(ratings)
#> item1 item2 item3 item4
#>   1.0   1.0   0.8   0.4
```

## Citation

If you use this package, please cite it (a citation entry will be added on
first CRAN release).

## License

MIT (c) 2026 Rashed Alqahtani. See [LICENSE](LICENSE.md).

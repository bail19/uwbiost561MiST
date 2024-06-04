---
output: github_document
---

# Purpose

This is a demo of the MiST package for the UW BIOST 561 Final Project. A formal package update will be released soon.

- The URL to the GitHub repository is: https://github.com/bail19/uwbiost561MiST
- The URL to the Pkgdown webpage is: https://bail19.github.io/uwbiost561MiST/

# How to install
This package is called `uwbiost561MiST`. To install, run the following code (in R):

```R
library(devtools)
devtools::install_github("bail19/uwbiost561MiST")
```

Upon completion, you can run the following code (in R):

```R
library(uwbiost561MiST)
```

# Dependencies

The package depends on the following packages: `CompQuadForm`(note: built under R version 3.3.2) and `Parallel`.

# Session info

This package was developed in the following environment

```R
> devtools::session_info()
─ Session info ──────────────────────────────────────────────────────────────────────────────────────────────────────
 setting  value
 version  R version 4.4.0 (2024-04-24)
 os       macOS Ventura 13.6
 system   aarch64, darwin20
 ui       RStudio
 language (EN)
 collate  en_US.UTF-8
 ctype    en_US.UTF-8
 tz       America/Los_Angeles
 date     2024-05-30
 rstudio  2023.09.0+463 Desert Sunflower (desktop)
 pandoc   3.1.1 @ /Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/ (via rmarkdown)

─ Packages ──────────────────────────────────────────────────────────────────────────────────────────────────────────
 package      * version date (UTC) lib source
 cachem         1.1.0   2024-05-16 [1] CRAN (R 4.4.0)
 cli            3.6.2   2023-12-11 [1] CRAN (R 4.4.0)
 CompQuadForm   1.4.3   2017-04-12 [1] CRAN (R 4.4.0)
 devtools       2.4.5   2022-10-11 [1] CRAN (R 4.4.0)
 digest         0.6.35  2024-03-11 [1] CRAN (R 4.4.0)
 ellipsis       0.3.2   2021-04-29 [1] CRAN (R 4.4.0)
 evaluate       0.23    2023-11-01 [1] CRAN (R 4.4.0)
 fastmap        1.2.0   2024-05-15 [1] CRAN (R 4.4.0)
 fs             1.6.4   2024-04-25 [1] CRAN (R 4.4.0)
 glue           1.7.0   2024-01-09 [1] CRAN (R 4.4.0)
 htmltools      0.5.8.1 2024-04-04 [1] CRAN (R 4.4.0)
 htmlwidgets    1.6.4   2023-12-06 [1] CRAN (R 4.4.0)
 httpuv         1.6.15  2024-03-26 [1] CRAN (R 4.4.0)
 knitr          1.46    2024-04-06 [1] CRAN (R 4.4.0)
 later          1.3.2   2023-12-06 [1] CRAN (R 4.4.0)
 lifecycle      1.0.4   2023-11-07 [1] CRAN (R 4.4.0)
 magrittr       2.0.3   2022-03-30 [1] CRAN (R 4.4.0)
 memoise        2.0.1   2021-11-26 [1] CRAN (R 4.4.0)
 mime           0.12    2021-09-28 [1] CRAN (R 4.4.0)
 miniUI         0.1.1.1 2018-05-18 [1] CRAN (R 4.4.0)
 pkgbuild       1.4.4   2024-03-17 [1] CRAN (R 4.4.0)
 pkgload        1.3.4   2024-01-16 [1] CRAN (R 4.4.0)
 profvis        0.3.8   2023-05-02 [1] CRAN (R 4.4.0)
 promises       1.3.0   2024-04-05 [1] CRAN (R 4.4.0)
 purrr          1.0.2   2023-08-10 [1] CRAN (R 4.4.0)
 R6             2.5.1   2021-08-19 [1] CRAN (R 4.4.0)
 Rcpp           1.0.12  2024-01-09 [1] CRAN (R 4.4.0)
 remotes        2.5.0   2024-03-17 [1] CRAN (R 4.4.0)
 rlang          1.1.3   2024-01-10 [1] CRAN (R 4.4.0)
 rmarkdown      2.27    2024-05-17 [1] CRAN (R 4.4.0)
 rstudioapi     0.16.0  2024-03-24 [1] CRAN (R 4.4.0)
 sessioninfo    1.2.2   2021-12-06 [1] CRAN (R 4.4.0)
 shiny          1.8.1.1 2024-04-02 [1] CRAN (R 4.4.0)
 stringi        1.8.4   2024-05-06 [1] CRAN (R 4.4.0)
 stringr        1.5.1   2023-11-14 [1] CRAN (R 4.4.0)
 urlchecker     1.0.1   2021-11-30 [1] CRAN (R 4.4.0)
 usethis        2.2.3   2024-02-19 [1] CRAN (R 4.4.0)
 vctrs          0.6.5   2023-12-01 [1] CRAN (R 4.4.0)
 xfun           0.44    2024-05-15 [1] CRAN (R 4.4.0)
 xtable         1.8-4   2019-04-21 [1] CRAN (R 4.4.0)
 yaml           2.3.8   2023-12-11 [1] CRAN (R 4.4.0)

 [1] /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/library

─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────

```

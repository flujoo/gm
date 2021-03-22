
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gm <img src="man/figures/logo.png" align="right" alt="logo" width="120"/>

<!-- badges: start -->
<!-- badges: end -->

Generate musical scores and audio files easily, and show them in R
Markdown documents, R Jupyter Notebooks and RStudio.

## Hello! World

``` r
library(gm)

m <- 
  # initialize a Music object
  Music() +
  # add a 4/4 time signature
  Meter(4, 4) +
  # add a musical line of four quarter notes
  Line(list("C5", "D5", "E5", "F5"), list(1, 1, 1, 1))
  
show(m)
```

![](man/figures/readme.png)

## Installation

Install gm:

``` r
# install.packages("devtools")
devtools::install_github("flujoo/gm")
```

MuseScore, an open source and free notation software, is required. Get
it from <https://musescore.org/>.

## More

See `vignette("gm", package = "gm")` for a complete guide to gm.

## Donation

<https://ko-fi.com/flujoo>

or Alipay (支付宝):

<img src="man/figures/alipay.jpeg" alt="alipay" width="250">

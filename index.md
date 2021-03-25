
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gm <img src="man/figures/logo.png" align="right" alt="logo" width="120"/>

<!-- badges: start -->
<!-- badges: end -->

Create music easily, and show musical scores and audio files in R
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
  
show(m, to = c("score", "audio"))
```

![](man/figures/readme.png)

<audio controls>
  <source src="reference/figures/readme_audio.mp3" type="audio/mpeg">
</audio>

## Installation

Install gm:

``` r
install.packages("gm")
```

Or the development version from GitHub:

```r
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

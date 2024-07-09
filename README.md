
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gm <img src="man/figures/logo.png" align="right" alt="logo" width="120"/>

<!-- badges: start -->
<!-- badges: end -->

Create music with R.

## Example

``` r
library(gm)

music <- 
  Music() +
  Meter(4, 4) +
  Line(c("C5", "D5", "E5", "F5"))
  
show(music)
```

![](man/figures/readme.png)

![](man/figures/readme_audio.png)

## Installation

Install gm:

``` r
install.packages("gm")

# Or install the development version from GitHub
pak::pak("flujoo/gm")
```

Install [MuseScore](https://musescore.org/). MuseScore is open source
and free notation software.

## More

See [`vignette("gm")`](https://flujoo.github.io/gm/articles/gm.html) for
a complete guide to gm.

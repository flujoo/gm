
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gm - Create Music With R <img src="man/figures/logo.png" align="right" alt="logo" width="120"/>

<!-- badges: start -->
<!-- badges: end -->

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

<audio controls>
  <source src="reference/figures/readme_audio.mp3" type="audio/mpeg">
</audio>

## Installation

Install gm:

``` r
install.packages("gm")
```

Install [MuseScore](https://musescore.org/). MuseScore is open source
and free notation software.

## More

See `vignette("gm")` for a complete guide to gm.

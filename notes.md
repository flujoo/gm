When CRAN checks this package, they builds vignettes, run tests and examples
but without MuseScore installed. To prevent errors in this situation:

1. Add `eval = identical(Sys.getenv("gm_vignette"), "true")` in
`knitr::opts_chunk$set()` in vignettes that need MuseScore.

2. Add `gm_vignette=true` in .Renviron before calling `devtools::build()` or
`R CMD build` to build these vignettes locally.

3. Comment `gm_vignette=true` out before calling `R CMD check --as-cran`
to simulate the environment where CRAN tests this package. These vignettes
are not built again. 

4. Also set `MUSESCORE_PATH=<wrong path>` in .Renviron to make MuseScore
unavailable when `R CMD check --as-cran` runs tests, for the same reason.


Can't add audio file in Github README, so let pkgdown use `index.md` 
rather than `README.md` to generate the home page. The only difference is:

```html
<audio controls>
  <source src="reference/figures/readme_audio.mp3" type="audio/mpeg">
</audio>
```

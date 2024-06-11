#' Infer Context Where Package Is Running
#'
#' @description
#' Possible contexts:
#'
#' 1. R Markdown: HTML or other output
#' 2. Shiny
#' 3. RStudio
#' 4. R Jupyter Notebook
#' 5. Console
#'
#' @details
#' Check if context is R Markdown before checking RStudio, because
#' if R Markdown is running in RStudio, both clauses will be `TRUE`.
#'
#' @seealso
#' - [How to tell if code is executed within a knitr/rmarkdown context?](
#' https://stackoverflow.com/questions/33107908/)
#'
#' - [Check if R is running in RStudio](
#' https://stackoverflow.com/questions/12389158/)
#'
#' @noRd
infer_context <- function() {
  in_rmd <-
    requireNamespace("knitr", quietly = TRUE) &&
    isTRUE(getOption("knitr.in.progress"))

  in_shiny <-
    requireNamespace("shiny", quietly = TRUE) &&
    shiny::isRunning()

  in_jupyter <- isTRUE(getOption("jupyter.in_kernel"))

  if (in_rmd) {
    if (knitr::is_html_output()) "rmd" else "rmd_other"

  } else if (in_shiny) {
    "shiny"

  } else if (Sys.getenv("RSTUDIO") == "1") {
    "rstudio"

  } else if (in_jupyter) {
    "jupyter"

  } else {
    "other"
  }
}

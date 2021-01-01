# import functions --------------------------------------------------------

#' @importFrom magrittr %>% %T>% %<>%
NULL



# globals -----------------------------------------------------------------

globals <- new.env()
globals$error_messages <- character(0)
globals$env <- NULL
globals$width <- 75



# generics ----------------------------------------------------------------

#' @keywords internal
#' @export
to_value <- function(x, ...) {
  UseMethod("to_value")
}


#' @export
show <- function(x, ...) {
  UseMethod("show")
}



# utils -------------------------------------------------------------------

coordinate <- function(nouns, conjunction = "or") {
  l <- length(nouns)

  if (l == 1) {
    return(nouns)
  }

  paste(
    paste(nouns[-l], collapse = ", "),
    conjunction,
    nouns[l]
  )
}


shorten_string <- function(string, width) {
  l <- nchar(string)

  if (l > width) {
    string <- string %>%
      substr(1, width) %>%
      paste("...")
  }

  string
}


generate_string <- function(general, specifics, env) {
  specifics %>%
    sapply(function(s) paste("*", s)) %>%
    paste(collapse = "\n") %>%
    {ifelse(. == "", "", paste0("\n\n", .))} %>%
    glue::glue(general, ., .envir = env)
}

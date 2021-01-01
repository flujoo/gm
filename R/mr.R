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

# connect `words` with `conjunction`
coordinate <- function(words, conjunction = "or") {
  l <- length(words)

  if (l == 1) {
    return(words)
  }

  paste(
    paste(words[-l], collapse = ", "),
    conjunction,
    words[l]
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

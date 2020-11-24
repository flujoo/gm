vowel_types <- c(
  "integer", "environment", "S4", "any", "expression", "externalptr"
)


join_words <- function(items, conjunction = NULL) {
  l <- length(items)

  if (l <= 1) {
    return(items)
  }

  if (is.null(conjunction)) {
    return(paste(items, collapse = ", "))
  }

  paste(
    paste(items[-l], collapse = ", "),
    conjunction,
    items[l]
  )
}


abort_type <- function(what, valid, invalid) {
  article_valid <- ifelse(valid[[1]] %in% vowel_types, "an", "a")
  article_invalid <- ifelse(invalid %in% vowel_types, "an", "a")

  valid <- join_words(valid, "or")

  m <- glue::glue(
    "`{what}` must be {article_valid} {valid}.\n",
    "* You've supplied {article_invalid} {invalid}."
  )

  rlang::abort(m)
}


show_errors <- function(messages, general) {
  l <- length(messages)

  if (l == 0) {
    return(invisible(NULL))
  }

  if (l <= 5) {
    more <- NULL
    i <- l
  } else {
    i <- 5

    if (l == 6) {
      more <- "... and 1 more problem."
    } else {
      more <- paste("... and", l - 5, "more problems.")
    }

    more <- more %>%
      paste0("\n", ., " See full report with `mr::inspect_errors()`.")
  }

  messages %>%
    # add all error messages to `globals`
    c(general, .) %T>%
    assign("error_messages", ., globals) %>%
    # show no more than 5 (excluding the general message)
    .[1:(i + 1)] %>%
    c(more) %>%
    paste(collapse = "\n") %>%
    rlang::abort()
}


#' @export
inspect_errors <- function() {
  globals$error_messages %>%
    paste(collapse = "\n") %>%
    cat("\n")
}

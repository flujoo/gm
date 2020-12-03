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


get_article <- function(type) {
  ifelse(type[1] %in% vowel_types, "an", "a")
}


check_type <- function(type = NULL, method = typeof, supplied, valid,
                       specific = NULL, general = NULL, name, ...) {
  # how to memorize these parameters:
  # check `type`, if not supplied, apply `method` to `supplied` to get it,
  # then check if it is `valid`, report with `specific` and `general` messages,
  # if not supplied, generate them with `name` and `...`

  if (is.null(type)) {
    type <- method(supplied)
  }

  if (!(type %in% valid)) {
    # article before type
    a_t <- get_article(type)
    # article before valid
    a_v <- get_article(valid)

    # even if `specific` and `general` are supplied, `glue()` may need
    # `a_t` and `a_v` to complete them, so these two variables are outside
    # of the following statements

    if (is.null(specific)) {
      specific <- "* You've supplied {a_t} {type}."
    }

    if (is.null(general)) {
      valid <- join_words(valid, "or")
      general <- "`{name}` must be {a_v} {valid}."
    }

    glue::glue(
      general, "\n\n", specific,
      .envir = list2env(list(...))
    ) %>% rlang::abort()
  }
}


check_length <- function(l = NULL, supplied, valid, specific = NULL,
                         general = NULL, type = NULL, name,
                         valid_phrase = NULL, ...) {
  if (is.null(l)) {
    l <- length(supplied)
  }

  if (is.character(valid)) {
    # e.g. "l > 2"
    con <- parse(text = valid) %>% eval()
  } else if (is.function(valid)) {
    con <- valid(l)
  } else {
    con <- l %in% valid
  }

  if (!con) {
    if (is.null(type)) {
      article <- "an"
      type <- "object"
    } else {
      article <- get_article(type)
    }

    if (is.null(valid_phrase)) {
      valid_phrase <- join_words(valid, "or")
    }

    if (is.null(specific)) {
      specific <- "* You've supplied {article} {type} of length {l}."
    }

    if (is.null(general)) {
      general <- "`{name}` must be of length {valid_phrase}."
    }

    glue::glue(
      general, "\n\n", specific,
      .envir = list2env(list(...))
    ) %>% rlang::abort()
  }
}


check_content <- function(supplied, valid, specific = NULL, general = NULL,
                          name, ...) {
  if (is.function(valid)) {
    con <- valid(supplied)
  } else if (is.expression(valid)) {
    con <- eval(valid)
  } else {
    con <- supplied %in% valid
  }

  if (!con) {
    if (is.character(supplied)) {
      supplied <- paste0('"', supplied, '"')
    }

    if (is.character(valid)) {
      valid <- sapply(valid, function(x) paste0('"', x, '"'))
    }
    valid <- join_words(valid, "or")

    if (is.null(specific)) {
      specific <- "* You've supplied {supplied}."
    }

    if (is.null(general)) {
      general <- "`{name}` must be {valid}."
    }

    glue::glue(
      general, "\n\n", specific,
      .envir = list2env(list(...))
    ) %>% rlang::abort()
  }
}


check_positive_integer <- function(supplied, name, general = NULL) {
  valid <- expression(
    as.integer(supplied) == supplied && supplied > 0
  )

  if (is.null(general)) {
    general <- name %>%
      paste0("`", ., "` must be a positive integer.")
  }

  check_content(supplied = supplied, valid = valid, general = general)
}

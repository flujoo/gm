vowel_types <- c(
  "integer", "environment", "S4", "any", "expression", "externalptr"
)


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


show_errors <- function(general, specific, supplement = NULL) {
  l <- length(specific)

  # return if `specific` is empty
  if (l == 0) {
    return(invisible(NULL))
  }

  # display at most 5 specific error messages
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

  # add a enter between blocks
  general <- paste0(general, "\n")

  if (!is.null(supplement)) {
    supplement <- paste0("\n", supplement)
  }

  # add "*" to each specific error messages
  specific <- specific %>%
    sapply(function(m) paste("*", m))

  # add all error messages to `globals$error_messages`
  c(general, specific, supplement) %>%
    assign("error_messages", ., globals)

  # display at most 5 specific error messages
  c(general, specific[1:i], more, supplement) %>%
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


check_content <- function(supplied, valid, specific = NULL, general = NULL,
                          name, ...) {
  if (is.function(valid)) {
    con <- valid(supplied)
  } else if (is.expression(valid)) {
    con <- eval(valid)
  } else {
    con <- all(supplied %in% valid)
  }

  if (!con) {
    if (is.character(supplied) && !is.na(supplied)) {
      supplied <- paste0('"', supplied, '"')
    }

    if (is.character(valid)) {
      valid <- sapply(valid, function(x) paste0('"', x, '"'))
    }
    valid <- coordinate(valid, "or")

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
    !is.na(supplied) &&
      as.integer(supplied) == supplied &&
      supplied > 0
  )

  if (is.null(general)) {
    general <- name %>%
      paste0("`", ., "` must be a positive integer.")
  }

  check_content(supplied = supplied, valid = valid, general = general)
}


check_na <- function(supplied, name, general = NULL) {
  valid <- expression(
    !is.na(supplied)
  )

  if (is.null(general)) {
    general <- name %>%
      paste0("`", ., "` must not be NA.")
  }

  check_content(supplied = supplied, valid = valid, general = general)
}


# will be deprecated
check_n <- function(n, name) {
  check_type(supplied = n, valid = c("double", "integer"), name = name)
  check_length(supplied = n, valid = 1, name = name, type = "numeric")
  check_positive_integer(supplied = n, name = name)
}


check_op_classes <- function(class_left = NULL, class_right = NULL,
                             method = class, left, right,
                             valid_left, valid_right,
                             general = NULL, specific = NULL, ...) {
  if (is.null(class_left)) {
    class_left <- method(left)[1]
  }

  if (is.null(class_right)) {
    class_right <- method(right)[1]
  }

  con <-
    class_left %in% valid_left && class_right %in% valid_right ||
    class_left %in% valid_right && class_right %in% valid_left

  if (!con) {
    a_valid_left <- get_article(valid_left)
    a_valid_right <- get_article(valid_right)

    a_left <- get_article(class_left)
    a_right <- get_article(class_right)

    if (is.null(general)) {
      general <- paste(
        "One side of `+` must be {a_valid_left}",
        "{coordinate(valid_left, 'or')},",
        "the other side must be {a_valid_right}",
        "{coordinate(valid_right, 'or')}."
      )
    }

    if (is.null(specific)) {
      specific <- paste(
        "* The left side is {a_left} {class_left},",
        "the right side is {a_right} {class_right}."
      )
    }

    glue::glue(
      general, "\n\n", specific,
      .envir = list2env(list(...))
    ) %>% rlang::abort()
  }
}



# basic validators --------------------------------------------------------

# usually used to check an argument's type or class
check_type <- function(x, valid, name = NULL, general = NULL,
                       specific = NULL, method = "type", type = NULL, ...) {
  # get the argument's name if not supplied directly
  if (is.null(name)) {
    # can't use %>% here
    name <- deparse(substitute(x))
  }

  # get `type` if not supplied directly
  if (is.null(type)) {
    if (method == "type") {
      type <- typeof(x)
    } else if (method == "class") {
      # remember that object can have more than one class
      type <- class(x)[1]
    }
  }

  # abort if `type` is not in `valid`
  if (!(type %in% valid)) {
    if (is.null(general)) {
      general <- "`{name}` must be of {method} {coordinate(valid)}."
    }

    if (is.null(specific)) {
      specific <- "What you've supplied is of {method} {type}."
    }

    specific <- paste("*", specific)

    glue::glue(
      general, "\n\n", specific,
      # you can pass variables in `...`
      .envir = list2env(list(...))
    ) %>% rlang::abort()
  }
}


check_length <- function(x, valid, name = NULL, general = NULL,
                         specific = NULL, l = NULL, ...) {
  if (is.null(name)) {
    # can't use %>% here
    name <- deparse(substitute(x))
  }

  if (is.null(l)) {
    l <- length(x)
  }

  # analyze `valid`
  # when `valid` is `Inf`
  if (length(valid) == 1 && is.infinite(valid)) {
    con <- l > 0
    phrase <- "larger than 0"
  # when treat `valid` as set
  } else if (is.numeric(valid)) {
    con <- l %in% valid
    phrase <- "{coordinate(valid)}"
  }

  if (!con) {
    if (is.null(general)) {
      general <- paste0("`{name}` must be of length ", phrase, ".")
    }

    if (is.null(specific)) {
      specific <- "What you've supplied is of length {l}."
    }

    specific <- paste("*", specific)

    glue::glue(
      general, "\n\n", specific,
      .envir = list2env(list(...))
    ) %>% rlang::abort()
  }
}

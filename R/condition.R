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
    valid <- coordinate(valid)

    if (is.null(general)) {
      general <- "`{name}` must have {method} {valid}."
    }

    if (is.null(specific)) {
      specific <- "`{name}` has {method} {type}."
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
      general <- paste0("`{name}` must have length ", phrase, ".")
    }

    if (is.null(specific)) {
      specific <- "`{name}` has length {l}."
    }

    specific <- paste("*", specific)

    glue::glue(
      general, "\n\n", specific,
      .envir = list2env(list(...))
    ) %>% rlang::abort()
  }
}


check_content <- function(x, valid, name = NULL, general = NULL,
                          specific = NULL, ...) {
  if (is.null(name)) {
    # can't use %>% here
    name <- deparse(substitute(x))
  }

  # analyze `valid`
  if (is.function(valid)) {
    con <- valid(x)
  } else if (is.expression(valid)) {
    con <- eval(valid)
  } else {
    con <- all(x %in% valid)
  }

  if (!con) {
    # quote characters
    if (is.character(x) && !is.na(x)) {
      x <- paste0('"', x, '"')
    }

    if (is.character(valid)) {
      valid <- sapply(valid, function(v) paste0('"', v, '"'))
    }

    valid <- coordinate(valid)

    if (is.null(general)) {
      general <- "`{name}` must be {valid}."
    }

    if (is.null(specific)) {
      specific <- "`{name}` is {x}."
    }

    specific <- paste("*", specific)

    glue::glue(
      general, "\n\n", specific,
      .envir = list2env(list(...))
    ) %>% rlang::abort()
  }
}



# shortcut validators -----------------------------------------------------

is_positive_integer <- function(x) {
  # `x` is checked to be a single numeric
  !is.na(x) & as.integer(x) == x & x > 0
}


check_positive_integer <- function(x, name = NULL) {
  if (is.null(name)) {
    name <- deparse(substitute(x))
  }

  check_type(x, c("double", "integer"), name)
  check_length(x, 1, name)

  valid <- is_positive_integer
  general <- "`{name}` must be a positive integer."
  check_content(x, valid, name, general)
}


check_name <- function(x, name = NULL) {
  if (is.null(name)) {
    name <- deparse(substitute(x))
  }

  check_type(x, "character", name)
  check_length(x, 1, name)

  general <- "`{name}` must not be NA."
  check_content(x, expression(!is.na(x)), name, general)
}


# check left and right classes for a binary operator
# usually, these classes are already given
check_binary_classes <- function(x, y, valid_x, valid_y, general = NULL,
                                 specific = NULL, operator = "+") {
  # left and right arguments may be not in the default order
  con <- `||`(
    (x %in% valid_x) && (y %in% valid_y),
    (y %in% valid_x) && (x %in% valid_y)
  )

  if (!con) {
    valid_x <- coordinate(valid_x)
    valid_y <- coordinate(valid_y)

    if (is.null(general)) {
      general <- paste(
        "One side of `{operator}` must have class {valid_x},",
        "the other side {valid_y}."
      )
    }

    if (is.null(specific)) {
      specific <- paste(
        "Left side has class {x},",
        "right side {y}."
      )
    }

    specific <- paste("*", specific)

    glue::glue(general, "\n\n", specific) %>% rlang::abort()
  }
}


check_same_length <- function(x, y, name_x = NULL, name_y = NULL) {
  l_x <- length(x)
  l_y <- length(y)

  if (l_x != l_y) {
    if (is.null(name_x)) {
      name_x <- deparse(substitute(x))
    }

    if (is.null(name_y)) {
      name_y <- deparse(substitute(y))
    }

    general <- "`{name_x}` and `{name_y}` must have same length."
    specific <- "* `{name_x}` is of length {l_x}, `{name_y}` {l_y}."

    glue::glue(general, "\n\n", specific) %>% rlang::abort()
  }
}



# show many error messages ------------------------------------------------

show_errors <- function(general, specifics, supplement = NULL,
                        env = NULL, class = NULL) {
  l <- length(specifics)

  # return if `specifics` is empty
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
  specifics <- specifics %>%
    sapply(function(m) paste("*", m))

  # add all error messages to `globals$error_messages`
  c(general, specifics, supplement) %>%
    assign("error_messages", ., globals)
  assign("env", env, globals)

  # display at most 5 specific error messages
  c(general, specifics[1:i], more, supplement) %>%
    paste(collapse = "\n") %>%
    glue::glue(.envir = env) %>%
    rlang::abort(class = class)
}


#' @export
inspect_errors <- function() {
  globals$error_messages %>%
    paste(collapse = "\n") %>%
    glue::glue(.envir = globals$env)
}



# item validators ---------------------------------------------------------

check_item_type <- function(x, valid, name = NULL, general = NULL,
                            specific = NULL, method = "type", l = NULL, ...) {
  if (is.null(name)) {
    name <- deparse(substitute(x))
  }

  if (is.null(l)) {
    l <- length(x)
  }

  if (l == 0) {
    return(invisible(NULL))
  }

  s_valid <- coordinate(valid)

  if (is.null(general)) {
    general <- "Each item of `{name}` must have {method} {s_valid}."
  }

  if (is.null(specific)) {
    specific <- "`{name}[[{i}]]` has {method} {t}."
  }

  ms <- character(0)

  for (i in 1:l) {
    x_i <- x[[i]]

    if (method == "type") {
      t <- typeof(x_i)
    } else if (method == "class") {
      t <- class(x_i)[1]
    }

    if (!(t %in% valid)) {
      ms[[length(ms) + 1]] <- specific %>%
        glue::glue() %>%
        unclass()
    }
  }

  show_errors(general, ms, env = environment())
}


check_item_length <- function(x, valid, name = NULL, phrase = NULL,
                              general = NULL, specific = NULL, ...) {
  l <- length(x)

  # return if `x` is empty
  if (l == 0) {
    return()
  }

  # analyze `valid`
  # if `valid` is `Inf`, it means the length must be larger than 0
  if (length(valid) == 1 && is.infinite(valid)) {
    con <- expression(l_i > 0)

    if (is.null(phrase)) {
      phrase <- "larger than 0"
    }
  # else treat `valid` as set
  } else if (is.numeric(valid)) {
    con <- expression(l_i %in% valid)

    if (is.null(phrase)) {
      phrase <- coordinate(valid)
    }
  }

  if (is.null(name)) {
    name <- deparse(substitute(x))
  }

  if (is.null(general)) {
    general <- "Each item of `{name}` must have length {phrase}."
  }

  if (is.null(specific)) {
    specific <- "`{name}[[{i}]]` has length {l_i}."
  }

  specifics <- character(0)

  for (i in 1:l) {
    x_i <- x[[i]]
    l_i <- length(x_i)

    if (!eval(con)) {
      specifics[[length(specifics) + 1]] <-
        specific %>%
        glue::glue() %>%
        unclass()
    }
  }

  show_errors(general, specifics, env = environment())
}

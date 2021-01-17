# introduction ------------------------------------------------------------

# when a Key is added at a bar, when a Tie is added at an index,
# they are added at a "position"
# so position is an abstraction of bar, index, etc.


# there are three types of positions: note, chord, segment

# type "note" is a single positive integer structurally
# bar and one-level index are two examples

# type "chord" contains one or two positive integers,
# of which the second one refers to a sub-position
# for example, `c(10, 2)` can refer to the 2nd pitch of the 10th chord
# this type is related to tie

# type "segment" contains two positive integers,
# which refer to the start and end positions of a segment
# this type is related to pedal, slur, etc.


# for convenience, argument `positions` can be a single position,
# which will be normalized to list



# check `positions` length ------------------------------------------------

#' @keywords internal
#' @export
check_positions_length <- function(positions, type, name = NULL) {
  UseMethod("check_positions_length")
}


#' @keywords internal
#' @export
check_positions_length.numeric <- function(positions, type, name = NULL) {
  if (is.null(name)) {
    name <- deparse(substitute(positions))
  }

  valid <- switch(type, "note" = 1, "chord" = c(1, 2), "segment" = 2)
  phrase <- coordinate(valid)
  general <- "If `{name}` is a numeric, it must have length {phrase}."
  check_length(positions, valid, name, general, phrase = phrase)
}


#' @keywords internal
#' @export
check_positions_length.list <- function(positions, type, name = NULL) {
  if (is.null(name)) {
    name <- deparse(substitute(positions))
  }

  valid <- Inf
  general <- "If `{name}` is a list, it must have length larger than 0."
  check_length(positions, valid, name, general)
}



# check `positions` content -----------------------------------------------

#' @keywords internal
#' @export
check_positions_content <- function(positions, type, name = NULL) {
  UseMethod("check_positions_content")
}


#' @keywords internal
#' @export
check_positions_content.numeric <- function(positions, type, name = NULL) {
  if (is.null(name)) {
    name <- deparse(substitute(positions))
  }

  phrase <- switch(
    type,
    "note" = "be a positive integer",
    "chord" = "contain one or two positive integers",
    "segment" = "contain two different positive integers"
  )

  general <- "If `{name}` is a numeric, it must {phrase}."

  l <- length(positions)

  # single positive integer
  if (l == 1) {
    if (!is_positive_integer(positions)) {
      specifics <- "`{name}` is {positions}."
      show_errors(general, specifics, env = environment())
    }
  }

  # two positive integers
  if (l > 1) {
    specifics <- character(0)
    specific <- "`{name}[{i}]` is {p}."

    for (i in 1:l) {
      p <- positions[i]

      if (!is_positive_integer(p)) {
        specifics[length(specifics) + 1] <-
          specific %>%
          glue::glue() %>%
          unclass()
      }
    }

    show_errors(general, specifics, env = environment())

    # type "segment" must contain two different integers
    if (type == "segment") {
      p1 <- positions[1]
      p2 <- positions[2]

      if (p1 == p2) {
        specifics[length(specifics) + 1] <-
          "`{name}` contains two {p1}'s." %>%
          glue::glue() %>%
          unclass()
      }
    }

    show_errors(general, specifics, env = environment())
  }
}


#' @keywords internal
#' @export
check_positions_content.list <- function(positions, type, name = NULL) {
  if (is.null(name)) {
    name <- deparse(substitute(positions))
  }

  phrase <- switch(
    type,
    "note" = "be a positive integer",
    "chord" = "contain one or two positive integers",
    "segment" = "contain two different positive integers"
  )

  specifics <- character(0)
  general <- "If `{name}` is a list, each item of it must {phrase}."

  for (i in 1:length(positions)) {
    p <- positions[[i]]
    t <- typeof(p)
    l <- length(p)

    # check type
    if (!is.numeric(p)) {
      specifics[length(specifics) + 1] <-
        "`{name}[[{i}]]` has type {t}." %>%
        glue::glue() %>%
        unclass()

      next
    }

    # check length
    con <- any(
      type == "note" && l != 1,
      type == "chord" && !(l %in% 1:2),
      type == "segment" && l != 2
    )

    if (con) {
      specifics[length(specifics) + 1] <-
        "`{name}[[{i}]]` has length {l}." %>%
        glue::glue() %>%
        unclass()

      next
    }

    # check if is positive integer
    if (!all(is_positive_integer(p))) {
      for (j in 1:l) {
        p_j <- p[j]

        if (!is_positive_integer(p_j)) {
          if (l == 1) {
            specific <- "`{name}[[{i}]]` is {p_j}."
          } else {
            specific <- "`{name}[[{i}]][{j}]` is {p_j}."
          }

          specifics[length(specifics) + 1] <-
            specific %>%
            glue::glue() %>%
            unclass()
        }
      }

      next
    }

    # check if has two different items
    if (type == "segment") {
      p1 <- p[1]
      p2 <- p[2]

      if (p1 == p2) {
        specifics[length(specifics) + 1] <-
          "`{name}[[{i}]]` contains two {p1}'s." %>%
          glue::glue() %>%
          unclass()
      }
    }
  }

  show_errors(general, specifics, env = environment())
}



# check `positions` -------------------------------------------------------

# all in one validator
check_positions <- function(positions, type, name = NULL) {
  if (is.null(name)) {
    name <- deparse(substitute(positions))
  }

  check_type(positions, c("double", "integer", "list"), name)
  check_positions_length(positions, type, name)
  check_positions_content(positions, type, name)
}



# PositionLine ------------------------------------------------------------

# just a constructor
# validator, normalizer and constructor are separated
PositionLine <- function(positions, type) {
  list(positions = positions, type = type) %>% `class<-`("PositionLine")
}



# print PositionLine ------------------------------------------------------

#' @keywords internal
#' @export
print.PositionLine <- function(x, silent = FALSE, ...) {
  positions <- x$positions

  # normalize numeric `x$positions` to list
  if (is.numeric(positions)) {
    positions %<>% list()
  }

  ss <- character(0)

  for (p in positions) {
    if (length(p) == 2) {
      p %<>%
        paste(collapse = ", ") %>%
        paste0("(", ., ")")
    }

    ss %<>% c(p)
  }

  s <- paste(ss, collapse = ", ")

  if (silent) {
    s
  } else {
    cat(s, "\n")
  }
}



# normalize `positions` ---------------------------------------------------

# only apply to list `positions`
remove_duplicate <- function(positions, warn = TRUE) {
  positions %<>% lapply(as.integer)
  # `unique` differentiates between double and integer

  . <- unique(positions)

  # warn if there is any change
  if (warn && !identical(., positions)) {
    "Any duplicate in `positions` is removed." %>% rlang::warn()
  }

  .
}


# only apply to list `positions`
sort_positions <- function(positions) {
  # get all first elements
  p1s <- sapply(positions, function(position) position[1])
  # sort `positions` by first element
  positions <- positions[order(p1s)]

  # re-assign `p1s`
  p1s <- sapply(positions, function(position) position[1])
  # sort `positions` by second element
  for (p1 in unique(p1s)) {
    is <- which(p1s == p1)

    if (length(is) > 1) {
      ps <- positions[is]
      p2s <- sapply(ps, function(position) position[2])
      positions[is] <- ps[order(p2s)]
    }
  }

  positions
}



normalize_positions <- function(positions, type) {
  # sort "segment" --------------------------------------------------------
  if (type == "segment") {
    positions %<>% lapply(sort)
  }


  # remove duplicates -----------------------------------------------------

  positions %<>% lapply(as.integer)
  # `unique` differentiates between double and integer

  positions %<>% unique()
  # for `type` segment`, this must be done after `positions` is sorted
  # this is because for example,
  # `c(1, 2)` and `c(2, 1)` are duplicates,
  # only after `c(2, 1)` is sorted to `c(1, 2)`


  # sort `positions` ------------------------------------------------------

  # get all first elements
  p1s <- sapply(positions, function(position) position[1])
  # sort `positions` by first element
  positions <- positions[order(p1s)]

  # re-assign `p1s`
  p1s <- sapply(positions, function(position) position[1])
  # sort `positions` by second element
  for (p1 in unique(p1s)) {
    is <- which(p1s == p1)

    if (length(is) > 1) {
      ps <- positions[is]
      p2s <- sapply(ps, function(position) position[2])
      positions[is] <- ps[order(p2s)]
    }
  }


  positions
}

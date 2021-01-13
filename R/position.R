PositionLine <- function(positions, type) {
  # check `positions`
  check_type(positions, "list")
  check_length(positions, Inf)
  check_positions(positions, type)

  # normalize `positions`
  positions %<>% normalize_positions(type)

  # create PositionLine
  list(positions = positions) %>% `class<-`("PositionLine")
}


check_positions <- function(positions, type) {
  phrase <- switch(type,
    "note" = "be a positive integer",
    "chord" = "contain one or two positive integers",
    "segment" = "contain two different positive integers"
  )

  general <- "Each item of `positions` must {phrase}."
  specifics <- character(0)

  for (i in 1:length(positions)) {
    p <- positions[[i]]
    t <- typeof(p)
    l <- length(p)

    # check type
    if (!is.numeric(p)) {
      specifics[[length(specifics) + 1]] <-
        "`positions[[{i}]]` has type {t}." %>%
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
      specifics[[length(specifics) + 1]] <-
        "`positions[[{i}]]` has length {l}." %>%
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
            specific <- "`positions[[{i}]]` is {p_j}."
          } else {
            specific <- "`positions[[{i}]][{j}]` is {p_j}."
          }

          specifics[[length(specifics) + 1]] <-
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
        specifics[[length(specifics) + 1]] <-
          "`positions[[{i}]]` contains two {p1}'s." %>%
          glue::glue() %>%
          unclass()
      }
    }
  }

  show_errors(general, specifics, env = environment())
}


normalize_positions <- function(positions, type) {
  # sort each item of `positions` if `type` is "segment"
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


#' @keywords internal
#' @export
print.PositionLine <- function(x, silent = FALSE, ...) {
  ss <- character(0)

  for (p in x$positions) {
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

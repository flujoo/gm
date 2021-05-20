# check if tuplets in `durations` form complete groups
check_tuplet_group <- function(durations) {
  general <- "Tuplets in `durations` must form complete groups."
  specifics <- c(
    "The tuplet group containing `durations[[{i}]]` is incomplete.",
    i = "`durations` is recycled, if it's shorter than `pitches`."
  )

  # "working memory" to store temporarily undecided tuplets
  wm <- list()
  l <- length(durations)

  for (i in 1:l) {
    d <- durations[[i]]
    l_wm <- length(wm)

    # skip non-tuplet `d` if `wm` is empty
    if (l_wm == 0 && !is_tuplet(d)) {
      next
    }

    # if `wm` is not empty,
    # and `d` is non-tuplet or incompatible with the last tuplet in `wm`,
    # then the group containing the last tuplet is incomplete,
    # trigger "incompatible" error
    if (l_wm != 0) {
      last <- wm[[l_wm]]

      if (!is_tuplet(d) || (is_tuplet(d) && !is_compatible(d, last))) {
        erify::throw(
          general, specifics, list(i = i - 1), class = "incompatible")
      }
    }

    # store `d` in `wm`
    wm %<>% c(list(d))

    # try to reduce `wm`
    tryCatch(
      {wm %<>% reduce_tuplets()},
      # trigger "over-complete" error
      error = function(e) erify::throw(
          general, specifics, list(i = i - 1), class = "over-complete")
    )

    # if `wm` is not totally reduced,
    # but the loop has already reached the end, trigger "incomplete" error,
    if (length(wm) != 0 && i == l) {
      erify::throw(general, specifics, list(i = i), class = "incomplete")
    }
  }
}


is_tuplet <- function(duration) {
  duration$tuplers %>%
    length() %>%
    as.logical()
}


# extra components may be added to a Duration for convenience, remove them
clear_duration <- function(duration) {
  ns <- names(duration)
  extra <- ns[!(ns %in% c("type", "dot", "tuplers"))]
  duration[extra] <- NULL
  duration
}


# being compatible means two tuplets share a common ancestor, meanwhile,
# the depth of `tuplet` is the same with or larger than `tuplet_0`'s
is_compatible <- function(tuplet, tuplet_0) {
  tuplet %<>% clear_duration()
  tuplet_0 %<>% clear_duration()

  # get depths of these two tuplets
  depth <- tuplet$tuplers %>% length()
  depth_0 <- tuplet_0$tuplers %>% length()

  # the depth of `tuplet` must not be less than `tuplet_0`'s
  if (depth < depth_0) {
    return(FALSE)
  }

  # remove the Tuplers beyond `depth_0` in `tuplet`
  if (depth > depth_0) {
    tuplet$tuplers[(depth_0 + 1):depth] <- NULL
  }

  # set `take` to `NULL` at the last level in both tuplets
  tuplet$tuplers[[depth_0]]$take <- NULL
  tuplet_0$tuplers[[depth_0]]$take <- NULL

  # now compare these two tuplets
  identical(tuplet, tuplet_0)
}


# if the tuplets at the deepest level form a group at that level,
# reduce that level, repeat this process until no tuplet is left in `tuplets`
reduce_tuplets <- function(tuplets) {
  repeat {
    # get depths of `tuplets`
    depths <- sapply(tuplets, function(tuplet) length(tuplet$tuplers))

    # get the largest depth
    depth_max <- max(depths)

    # reset `tuplets` if no tuplet left in `tuplets`,
    if (depth_max == 0) {
      return(list())
    }
    # which means `tuplet` forms a tuplet group,
    # and is reduced to a non-tuplet

    # get the indices of the tuplets of `depth_max`
    ks <- which(depths == depth_max)

    # sum up the last Tuplers of these tuplets
    total <-
      tuplets[ks] %>%
      sapply(function(tuplet) quantify(tuplet$tuplers[[depth_max]])) %>%
      sum()

    # if `total` is 1, then the group is complete at level `depth_max`,
    # reduce that level:
    if (total == 1) {
      # keep the first tuple at that level, remove its last Tupler,
      tuplets[[ks[1]]]$tuplers[[depth_max]] <- NULL
      # remove other tuplets
      tuplets[ks[-1]] <- NULL
      # go to next loop
      next
    }

    # if `total` is less than 1, the reducing process will stop
    if (total < 1) {
      return(tuplets)
    }

    # if `total` is larger than 1,
    # it means the group at current level has not been complete yet,
    # but the next tuplet is already in `tuplets`,
    # then the group containing the last tuplet is incomplete
    if (total > 1) {
      stop()
    }
  }
}

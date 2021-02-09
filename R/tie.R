normalize_line_tie <- function(tie, pitches) {
  # skip if `tie` is `NULL`
  if (is.null(tie)) {
    return()
  }

  # primary validation
  check_positions(tie, "chord")

  # remove any duplicate or redundant position in list `tie`
  if (is.list(tie)) {
    tie %<>% lapply(as.integer)
    # this is for `unique`,
    # because `unique` differentiates between double and integer

    # the rationale behind the following few steps is,
    # for example, remove `c(1, 1)` if `1` is in `tie`
    # get length 1 positions in `tie`
    ps <- Filter(function(p) length(p) == 1, tie)
    # remove redundant length 2 positions
    for (i in 1:length(tie)) {
      p <- tie[[i]]
      p1 <- p[1]

      if (p1 %in% ps && length(p) == 2) {
        tie[[i]] <- p1
      }
    }

    # remove any duplicate
    tie %<>% unique()

  # put numeric `tie` into list
  } else {
    tie %<>%
      as.integer() %>%
      list()
  }

  # check if `tie` corresponds to `pitches`
  check_tie(tie, pitches)

  # reduce or expand certain positions
  tie %<>% normalize_tie(pitches)

  # normalize `tie` to PositionLine
  PositionLine(tie, "chord")
}


check_tie <- function(positions, pitches) {
  general <- paste(
    "In the Line,",
    "there must be a note at each position specified in `tie`,",
    "and also a note with equivalent pitch after that note."
  )

  specifics <- character(0)
  l <- length(pitches)
  vs <- to_values.pitches(pitches)

  for (i in 1:length(positions)) {
    # unpack each position
    po <- positions[[i]]
    po1 <- po[1]
    po2 <- po[2]
    # `po2` may not exist, i.e. is `NA`

    # convert current position to string
    if (is.na(po2)) {
      s_po <- po1
    } else {
      s_po <- paste0("(", po1, ", ", po2, ")")
    }

    # current position is beyond Line length
    if (po1 > l) {
      specifics[length(specifics) + 1] <-
        paste(
          "Can't find note at position {s_po},",
          "the Line length is only {l}."
        ) %>%
        glue::glue() %>%
        unclass()

      class_ <- "position_beyond_line_length"
      # this is for testing

      next
    }

    # get pitch(es) at `po1`
    ps <- pitches[[po1]]
    l_ps <- length(ps)

    # rest at current position
    if (l_ps == 1 && is.na(ps)) {
      specifics[length(specifics) + 1] <-
        paste(
          "Can't find note at position {s_po},",
          "only a rest at position {po1}."
        ) %>%
        glue::glue() %>%
        unclass()

      class_ <- "rest_at_position"
      next
    }

    # `po2` is beyond chord length
    if (!is.na(po2) && po2 > l_ps) {
      specifics[length(specifics) + 1] <-
        paste(
          "Can't find note at position {s_po},",
          "only {l_ps}",
          ifelse(l_ps == 1, "note", "notes"),
          "at position {po1}."
        ) %>%
        glue::glue() %>%
        unclass()

      class_ <- "position_beyond_chord_length"
      next
    }

    # get the position after `po`
    po_after <- po1 + 1

    # no note after `po`
    if (po_after > l) {
      specifics[length(specifics) + 1] <-
        paste(
          "Can't find note after position {s_po},",
          "the Line length is only {l}."
        ) %>%
        glue::glue() %>%
        unclass()

      class_ <- "next_position_beyond_line_length"
      next
    }

    # get pitch(es) at `po_after`
    ps_after <- pitches[[po_after]]
    l_after <- length(ps_after)

    # rest at `po_after`
    if (l_after == 1 && is.na(ps_after)) {
      specifics[length(specifics) + 1] <-
        paste(
          "Can't find note after position {s_po},",
          "only a rest at position {po_after}."
        ) %>%
        glue::glue() %>%
        unclass()

      class_ <- "rest_at_next_position"
      next
    }

    # complicated part ...

    # get the value(s) of the pitch at `po`
    v <- vs[[po]]
    v_after <- vs[[po_after]]

    # when it's a chord at `po` but `po` has length 1,
    # or it's a note at `po`,
    # any note after `po` can be tied only once,
    # so this is not tricky, compared to the next clause
    if (((l_ps > 1 && is.na(po2)) || l_ps == 1) && !any(v %in% v_after)) {
      specifics[length(specifics) + 1] <-
        paste(
          "Can't find note with equivalent pitch after position {s_po}."
        ) %>%
        glue::glue() %>%
        unclass()

      class_ <- "no_equivalent_pitch"
      next
    }

    # it is tricky when it's a chord at `po` and `po` has length 2
    if (l_ps > 1 && !is.na(po2)) {
      # find a equivalent pitch, if any
      k <- Position(function(x) x == v, v_after)

      if (is.na(k)) {
        specifics[length(specifics) + 1] <-
          paste(
            "Can't find note with equivalent pitch after position {s_po}."
          ) %>%
          glue::glue() %>%
          unclass()

        class_ <- "no_equivalent_pitch_complicated"

      } else {
        # the tricky part is, if find a equivalent pitch,
        # other positions in `tie` may also depend on it,
        # so set that value to -1
        vs[[po_after]][k] <- -1
      }
    }
  }

  show_errors(general, specifics, env = environment(), class = class_)
}


normalize_tie <- function(positions, pitches) {
  vs <- to_values.pitches(pitches)
  # for expanding positions in the second clause in below

  for (i in 1:length(positions)) {
    po <- positions[[i]]
    po1 <- po[1]
    po2 <- po[2]

    p <- pitches[[po]]
    l_p <- length(p)
    p_after <- pitches[[po1 + 1]]

    # reduce current position to length 1,
    # if it's a note at this position
    if (!is.na(po2) && l_p == 1) {
      positions[[i]] <- po[1]
      next
    }

    # expand current position if its length is 1,
    # but it's a chord at this position
    if (is.na(po2) && l_p > 1) {
      # get values
      v <- vs[[po]]
      v_after <- vs[[po1 + 1]]

      # store resulted positions
      pos <- list()

      for (j in 1:l_p) {
        v_j <- v[j]

        # find a equivalent pitch
        k <- Position(function(x) x == v_j, v_after)

        # see `check_tie` for the rationale behind this part
        if (!is.na(k)) {
          pos[[length(pos) + 1]] <- c(po1, j)
          # set `v_after` rather than `vs[[po1 + 1]]`
          v_after[k] <- -1
        }
      }

      # replace current position
      positions[[i]] <- pos
    }
  }

  # flatten `positions` (and keep the original position order)
  pos <- list()
  for (po in positions) {
    if (is.list(po)) {
      pos %<>% c(po)
    } else {
      pos %<>% c(list(po))
    }
  }

  pos
}


to_values.pitches <- function(pitches) {
  for (i in 1:length(pitches)) {
    p <- pitches[[i]]
    c_ <- class(p)

    if (c_ == "PitchValue") {
      pitches[[i]] <- unclass(p)

    } else if (c_ == "PitchNotation") {
      pitches[[i]] <- p %>%
        to_Pitch() %>%
        to_value()

    } else if (c_ == "PitchRest") {
      pitches[[i]] <- 0

    } else if (c_ == "Pitch") {
      pitches[[i]] <- to_value(p)

    } else if (c_ == "PitchChord") {
      pitches[[i]] <- p %>%
        unclass() %>%
        to_values.pitches() %>%
        unlist()
    }
  }

  pitches
}


# leave marks in tied Pitches
mark_tie <- function(pitches, positions) {
  for (po in positions) {
    # mark the start Pitch
    pitches[[po]]$tie_start <- TRUE

    # mark the end Pitch
    po2 <- po[1] + 1
    p2 <- pitches[[po2]]
    c_ <- class(p2)

    if (c_ == "Pitch") {
      pitches[[po2]]$tie_stop <- TRUE

    } else if (c_ == "PitchChord") {
      v1 <- pitches[[po]] %>% to_value()
      vs <- sapply(p2, to_value)

      # note that there could be more than one Pitch same to `v1` at `po2`
      # mark the Pitch that is not marked
      ks <- which(vs == v1)
      i <- 1

      repeat {
        k <- ks[i]
        mark <- pitches[[c(po2, k)]]$tie_stop

        if (is.null(mark)) {
          pitches[[c(po2, k)]]$tie_stop <- TRUE
          break
        }

        i <- i + 1
      }
    }
  }

  pitches
}

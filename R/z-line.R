#' @export
Line <- function(pitches = NULL, durations = NULL, tie = NULL, name = NULL,
                 as = NULL, to = NULL, after = NULL, bar = NULL,
                 offset = NULL) {
  # check `pitches` and `durations`
  check_pitches(pitches)
  check_durations(durations)
  check_pitches_durations(pitches, durations)

  # check other arguments
  if (!is.null(name)) {
    erify::check_string(name)
  }

  if (!is.null(as)) {
    erify::check_content(as, c("part", "staff", "voice", "segment"))
  }

  check_to(to)

  if (!is.null(after)) {
    erify::check_bool(after)
  }

  check_bar(bar)

  if (!is.null(offset)) {
    erify::check_positive(offset)
  }

  # deprecate `tie`
  deprecate_tie(tie)

  # normalize `pitches` and `durations`
  . <- normalize_pitches_durations(pitches, durations)
  pitches <- .$pitches
  durations <- .$durations

  # check and mark tuplets
  check_tuplet_group(durations)
  mark_tuplets(durations)

  # convert `pitches` and `durations` to tibbles
  pitches %<>% frame_pitches()
  durations %<>% frame_durations()

  # create Line object
  list(
    pitches = pitches,
    durations = durations,
    name = name,
    as = as,
    to = to,
    after = after,
    bar = bar,
    offset = offset
  ) %>% `class<-`("Line")
}


# check if `pitches` and `durations` are both empty
check_pitches_durations <- function(pitches, durations) {
  pass <- length(pitches) > 0 || length(durations) > 0

  if (pass) {
    return(invisible())
  }

  general <- "`pitches` and `durations` must not both be empty."
  specifics <- c(
    "`pitches` is { erify::back_quote(pitches) }.",
    "`durations` is { erify::back_quote(durations) }."
  )

  erify::throw(general, specifics, environment())
}


deprecate_tie <- function(tie) {
  if (is.null(tie)) {
    return(invisible())
  }

  warning(
    "`tie` is deprecated, use `Tie()` instead.\n",
    call. = FALSE,
    immediate. = TRUE
  )
}


# initialize, normalize or recycle `pitches` and `durations`
normalize_pitches_durations <- function(pitches, durations) {
  l_ps <- len(pitches)
  l_ds <- len(durations)

  # initialize `pitches` or `durations` if empty
  if (l_ps == 0) {
    pitches <- NA
    l_ps <- 1L
  }

  if (l_ds == 0) {
    durations <- 1
    l_ds <- 1L
  }

  # normalize `pitches` and `durations`
  pitches %<>% normalize_pitches()
  durations %<>% normalize_durations()

  # recycle the short one
  l_max <- max(l_ps, l_ds)

  if (l_ps < l_max) {
    pitches %<>% rep_len(l_max)
  }

  if (l_ds < l_max) {
    durations %<>% rep_len(l_max)
  }

  list(pitches = pitches, durations = durations)
}


frame_pitches <- function(pitches) {
  # initialize a tibble
  tb <- tibble::tibble(
    i = integer(),
    j = integer(),
    pitch = list(),
    notation = character(),
    value = integer()
  )

  # add cases
  for (i in seq_along(pitches)) {
    pitch <- pitches[[i]]
    l <- len(pitch)

    if (l <= 1) {
      # note that `pitch` can be `NULL`
      j <- 1L
      pitch %<>% list()

    } else {
      j <- 1:l
    }

    tb %<>% tibble::add_case(
      i = i,
      j = j,
      pitch = pitch,
      notation = signify(pitch),
      value = quantify(pitch)
    )
  }

  tb
}


frame_durations <- function(durations) {
  tibble::tibble(
    i = seq_along(durations),
    duration = durations,
    notation = signify(duration),
    value = quantify(duration)
  )
}

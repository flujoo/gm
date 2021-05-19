#' @export
Line <- function(pitches = NULL, durations = NULL, name = NULL, as = NULL,
                 to = NULL, after = NULL, bar = NULL, offset = NULL) {
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

  if (!is.null(to)) {
    check_to(to)
  }

  if (!is.null(after)) {
    erify::check_bool(after)
  }

  if (!is.null(bar)) {
    erify::check_n(bar)
  }

  check_offset(offset)

  # normalize `pitches` and `durations`
  . <- normalize_pitches_durations(pitches, durations)
  pitches <- .$pitches
  durations <- .$durations

  # check and mark tuplets
  check_tuplet_group(durations)
  mark_tuplets(durations)

  # combine `pitches` and `durations`
  notes <- combine_pitches_durations(pitches, durations)
  notes %<>% describe_notes()

  # create Line object
  list(
    notes = notes,
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


# initialize, normalize or recycle `pitches` and `durations`
normalize_pitches_durations <- function(pitches, durations) {
  l_ps <- length(pitches)
  l_ds <- length(durations)

  # initialize `pitches` or `durations` if empty
  if (l_ps == 0) {
    pitches <- NA
    l_ps <- 1
  }

  if (l_ds == 0) {
    durations <- 1
    l_ds <- 1
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


combine_pitches_durations <- function(pitches, durations) {
  # initialize a tibble
  notes <- tibble::tibble(
    i = integer(),
    j = integer(),
    pitch = list(),
    duration = list()
  )

  # add cases
  for (i in seq_along(pitches)) {
    # unpack
    pitch <- pitches[[i]]
    duration <- durations[[i]]
    l <- length(pitch)

    if (l == 1) {
      notes %<>% tibble::add_case(
        i = i,
        j = NA_integer_,
        pitch = list(pitch),
        duration = list(duration)
      )

    } else if (l > 1) {
      # expand pitch chords
      notes %<>% tibble::add_case(
        i = rep(i, l),
        j = 1:l,
        pitch = pitch,
        duration = rep(list(duration), l)
      )
    }
  }

  notes
}


# add pitch/duration notation/value to `notes`
describe_notes <- function(notes) {
  notes$pn <- signify(notes$pitch)
  notes$pv <- quantify(notes$pitch)
  notes$dn <- signify(notes$duration, short = getOption("gm.shorten_dn"))
  notes$dv <- quantify(notes$duration)
  dplyr::select(notes, i, j, pitch, pn, pv, duration, dn, dv)
}

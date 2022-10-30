#' Add Case to `lines` in Music
#' @noRd
add_line <- function(lines, object) {
  if (is.null(lines)) {
    location <- initialize_line_location()

  } else {
    to <- object$to
    as <- object$as
    after <- object$after

    # normalization
    if (is.null(as)) as <- "part"
    if (is.null(after)) after <- TRUE

    target <- locate_to(to, lines)
    check_voice_limit(as, lines, target, to)
    location <- infer_line_location(target, as, after)
    lines <- update_lines(lines, target, as, after)
  }

  line <- to_case(object, location)
  lines <- rbind(lines, line)
  lines
}


#' Initialize Line's Location
#'
#' Initialize the incoming Line's location when there is no Line in the Music.
#'
#' A **location** is a Line's part, staff, voice, and segment in a score.
#'
#' @noRd
initialize_line_location <- function() {
  data.frame(
    part = 1L,
    staff = 1L,
    voice = 1L,
    segment = 1L
  )
}


#' Get Line's Location in Score
#'
#' Get a Line's location in a score. The Line is referred to by `to`.
#'
#' @noRd
locate_to <- function(to, lines) {
  if (is.numeric(to)) {
    line <- lines[to, ]

  } else if (is.character(to)) {
    line <- lines[!is.na(lines$name) & lines$name == to, ]

  } else {
    # get the last Line in the score
    # rather than the last case in `lines`
    lines <- lines[lines$part == max(lines$part), ]
    lines <- lines[lines$staff == max(lines$staff), ]
    lines <- lines[lines$voice == max(lines$voice), ]
    line <- lines[lines$segment == max(lines$segment), ]
  }

  line[, c("part", "staff", "voice", "segment")]
}


#' Infer Line's Location
#'
#' Infer the location of the incoming Line from the location of the Line
#' that `to` refers to.
#'
#' @noRd
infer_line_location <- function(target, as, after) {
  if (as == "segment") {
    if (after) target$segment <- target$segment + 1L

  } else if (as == "voice") {
    if (after) target$voice <- target$voice + 1L
    target$segment <- 1L

  } else if (as == "staff") {
    if (after) target$staff <- target$staff + 1L
    target$voice <- 1L
    target$segment <- 1L

  } else if (as == "part") {
    if (after) target$part <- target$part + 1L
    target$staff <- 1L
    target$voice <- 1L
    target$segment <- 1L
  }

  target
}


#' Update `lines` in Music
#' @noRd
update_lines <- function(lines, target, as, after) {
  d <- if (after) 1L else 0L

  if (as == "part") {
    filter <- lines$part >= target$part + d

  } else if (as == "staff") {
    filter <- (lines$part == target$part) &
      (lines$staff >= target$staff + d)

  } else if (as == "voice") {
    filter <- (lines$part == target$part) &
      (lines$staff == target$staff) &
      (lines$voice >= target$voice + d)

  } else if (as == "segment") {
    filter <- (lines$part == target$part) &
      (lines$staff == target$staff) &
      (lines$voice == target$voice) &
      (lines$segment >= target$segment + d)
  }

  lines[filter, as] <- lines[filter, as] + 1L
  lines
}


#' @keywords internal
#' @export
to_case.Line <- function(object, location, ...) {
  name <- object$name
  bar <- object$bar
  offset <- object$offset

  # normalization
  if (is.null(name)) name <- NA_character_
  if (is.null(bar)) bar <- NA_integer_
  if (is.null(offset)) offset <- NA_real_

  line <- cbind(name = name, location, bar = bar, offset = offset)

  if (requireNamespace("tibble", quietly = TRUE)) {
    line <- tibble::as_tibble(line)
  }

  line
}

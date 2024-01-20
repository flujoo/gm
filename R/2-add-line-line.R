#' Add Case to `lines` in Music
#' @noRd
add_line <- function(lines, object) {
  if (is.null(lines)) {
    # Initialization
    location <- data.frame(part = 1L, staff = 1L, voice = 1L, segment = 1L)

  } else {
    to <- object$to
    as <- object$as
    after <- object$after

    # Normalization
    if (is.null(as)) as <- "part"
    if (is.null(after)) after <- TRUE

    target <- locate_to(to, lines)
    check_voice_limit(as, lines, target, to)
    location <- infer_line_location(target, as, after)
    lines <- update_lines(lines, target, as, after)
  }

  line <- normalize_line(object, location)
  lines <- rbind(lines, line)
  lines
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


normalize_line <- function(object, location) {
  bar <- object$bar
  offset <- object$offset
  name <- object$name

  if (is.null(bar)) bar <- 1L
  if (is.null(offset)) offset <- 0
  if (is.null(name)) name <- NA_character_

  line <- cbind(location, bar = bar, offset = offset, name = name)

  if (requireNamespace("tibble", quietly = TRUE)) {
    line <- tibble::as_tibble(line)
  }

  line
}

#' @keywords internal
#' @export
add.Line <- function(object, music) {
  name <- object$name
  lines <- music$lines

  # check if the Line's `$name` is used by any Line in the Music
  check_line_name(name, lines)

  to <- object$to

  # check if the Line's `$to` refers to a Line in the Music
  check_to_exist(to, lines, "Line")

  l <- nrow(lines)

  # append the Line's `$notes` to the Music's
  music$notes <- object$notes %>%
    tibble::add_column(line = l + 1L, .before = 1L) %>%
    tibble::add_case(music$notes, .)

  as <- object$as
  after <- object$after

  # normalize `as`
  if (is.null(as)) {
    as <- "part"
  }

  # normalize `after`
  if (is.null(after)) {
    after <- TRUE
  }

  # generate `$part`, `$staff`, ... for the Line
  if (l == 0) {
    number <- tibble::tibble(part = 1L, staff = 1L, voice = 1L, segment = 1L)

  } else {
    # get `$part`, `$staff`, ... from the case which `to` refers to
    number_to <- get_to_number(to, lines)
    number <- generate_number(number_to, as, after, to, lines)
  }

  # add `$name`, `$bar` and `$offset` to `number`
  line <- generate_line(number, name, object$bar, object$offset)

  # update `$part`, `$staff`, ... of the Music's `$lines`
  if (l != 0) {
    lines %<>% update_lines(number_to, as, after)
  }

  # add `line` to the Music `$lines`
  music$lines <- tibble::add_case(lines, line)

  music
}


# check if the Line's `$name` is used by any Line in the Music
check_line_name <- function(name, lines) {
  pass <- is.null(name) || !(name %in% lines$name)

  if (pass) {
    return(invisible())
  }

  general <- "Each Line in a Music must have a unique name or no name."
  specific <- 'Name `"{name}"` has been used.'
  erify::throw(general, specific, list(name = name))
}


# from the Music's `$lines`,
# get `$part`, `$staff`, ... from the case which `to` refers to
get_to_number <- function(to, lines) {
  if (is.numeric(to)) {
    case <- lines[to, ]

  } else if (is.character(to)) {
    case <- lines[lines$name == to, ]

  } else if (is.null(to)) {
    # get the last line in the score
    # (not the last case in the Music's `$lines`)
    case <- lines %>%
      {.[.$part == max(.$part), ]} %>%
      {.[.$staff == max(.$staff), ]} %>%
      {.[.$voice == max(.$voice), ]} %>%
      {.[.$segment == max(.$segment), ]}
  }

  case[, c("part", "staff", "voice", "segment")]
}


# generate `$part`, `$staff`, ... for the Line
generate_number <- function(number_to, as, after, to, lines) {
  if (as == "part") {
    if (after) {
      number_to$part <- number_to$part + 1L
    }

    number_to$staff <- 1L
    number_to$voice <- 1L
    number_to$segment <- 1L

  } else if (as == "staff") {
    if (after) {
      number_to$staff <- number_to$staff + 1L
    }

    number_to$voice <- 1L
    number_to$segment <- 1L

  } else if (as == "voice") {
    # check if the staff the Line is added to already has four voices
    check_voice_limit(lines, number_to, to)

    if (after) {
      number_to$voice <- number_to$voice + 1L
    }

    number_to$segment <- 1L

  } else if (as == "segment") {
    number_to$segment <- number_to$segment + 1L
  }

  number_to
}


# check if the staff the Line is added to already has four voices
check_voice_limit <- function(lines, number_to, to) {
  # count the number of the voices in the staff indicated by `number_to`
  con <- lines$part == number_to$part & lines$staff == number_to$staff
  n <- lines[con, ] %>% nrow()

  # pass
  if (n <= 4) {
    return(invisible())
  }

  general <- "Any staff in a Music can contain at most four voices."

  if (is.null(to)) {
    s_to <- "the Line is added to"
  } else if (is.character(to)) {
    s_to <- 'containing Line "{to}"'
  } else if (is.numeric(to)) {
    s_to <- "containing Line {to}"
  }

  specific <- paste("The staff", s_to, "already has four voices.")
  erify::throw(general, specific, environment())
}


# add `$name`, `$bar` and `$offset` to `number`
generate_line <- function(number, name, bar, offset) {
  # normalize arguments
  if (is.null(name)) {
    name <- NA_character_
  }

  bar %<>% normalize_bar()

  if (is.null(offset)) {
    offset <- 0
  }

  # add arguments
  number %>%
    tibble::add_column(name = name, .before = 1L) %>%
    tibble::add_column(bar = bar, offset = offset)
}


# update `$part`, `$staff`, ... of the Music's `$lines`
update_lines <- function(lines, number_to, as, after) {
  d <- ifelse(after, 1L, 0L)

  if (as == "part") {
    con <- lines$part >= number_to$part + d
    lines[con, ]$part %<>% {. + 1L}

  } else if (as == "staff") {
    con <- (lines$part == number_to$part) &
      (lines$staff >= number_to$staff + d)

    lines[con, ]$staff %<>% {. + 1L}

  } else if (as == "voice") {
    con <- (lines$part == number_to$part) &
      (lines$staff == number_to$staff) &
      (lines$voice >= number_to$voice + d)

    lines[con, ]$voice %<>% {. + 1L}

  } else if (as == "segment") {
    con <- (lines$part == number_to$part) &
      (lines$staff == number_to$staff) &
      (lines$voice == number_to$voice) &
      (lines$segment >= number_to$segment + d)

    lines[con, ]$segment %<>% {. + 1L}
  }

  lines
}

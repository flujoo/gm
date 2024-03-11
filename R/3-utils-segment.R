check_segments <- function(lines) {
  general <- "Segments must not overlap."
  specific <- "Line %s and %s overlap."
  specifics <- character()

  for (i in seq_len(NROW(lines))) {
    line_i <- lines[i, ]
    if (line_i[["segment"]] == 1) next

    js <- find_bottom_segments(line_i, lines)

    for (j in js) {
      line_j <- lines[j, ]
      if (is_separate(line_i, line_j)) next
      specifics <- c(specifics, sprintf(specific, i, j))
    }
  }

  erify::throw(general, specifics)
}


#' Check If Top Segment Is Separate From Bottom Segment
#' @noRd
is_separate <- function(top, bottom) {
  any(
    top$start_bar > bottom$end_bar,
    top$start_bar == bottom$end_bar && top$start_offset >= bottom$end_offset,
    top$end_bar < bottom$start_bar,
    top$end_bar == bottom$start_bar && top$end_offset <= bottom$start_offset
  )
}


find_bottom_segments <- function(line, lines) {
  which(
    lines$part == line$part &
    lines$staff == line$staff &
    lines$voice == line$voice &
    lines$segment < line$segment
  )
}

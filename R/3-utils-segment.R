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


find_bottom_segments <- function(lines, line) {
  which(
    lines$part == line$part &
    lines$staff == line$staff &
    lines$voice == line$voice &
    lines$segment < line$segment
  )
}

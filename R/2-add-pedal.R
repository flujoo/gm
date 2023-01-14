#' @keywords internal
#' @export
add.Pedal <- function(object, music) {
  to <- object$to
  i <- object$i
  j <- object$j
  lines <- music$lines
  notes <- music$notes

  # validation
  check_to_exist(to, lines)
  line <- get_line_row(to, lines)
  check_i(i, line, notes)
  check_i(j, line, notes)

  # normalization
  names(object)[names(object) == "to"] <- "line"
  object$line <- line

  . <- sort(c(i, j))
  object$i <- .[1]
  object$j <- .[2]

  # construction
  music$pedals <- update_pedals(music$pedals, object)
  music
}


update_pedals <- function(pedals, pedal) {
  line <- pedal$line
  i <- pedal$i
  j <- pedal$j

  ks <- NULL

  for (k in seq_len(NROW(pedals))) {
    pedal_k <- pedals[k, ]

    if (pedal_k$line != line) next

    i_k <- pedal_k$i
    j_k <- pedal_k$j

    if (j_k < i || i_k > j) next
    ks <- c(ks, k)

    # replace the Pedal in the Music that is the same as the incoming Pedal
    if (i == i_k && j == j_k) break

    # drop the incoming Pedal if it is "covered" by any Pedal in the Music
    if (i >= i_k && j <= j_k) return(pedals)

    # merge the Pedals in the Music that "touch" the incoming Pedal
    if (i >= i_k && i <= j_k) {
      i <- i_k
      pedal$i <- i

    } else if (j >= i_k && j <= j_k) {
      j <- j_k
      pedal$j <- j
    }
  }

  if (!is.null(ks)) pedals <- pedals[-ks, ]
  rbind(pedals, to_case(pedal))
}

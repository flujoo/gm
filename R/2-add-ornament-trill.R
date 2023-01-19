#' @keywords internal
#' @export
add.Trill <- function(object, music) {
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

  if (!is.na(j)) {
    . <- sort(c(i, j))
    object$i <- .[1]
    object$j <- .[2]
  }

  # construction
  if (is.na(j) || j == i) music <- remove_ornaments(music, object)
  music$trills <- update_trills(music$trills, object)
  music
}


update_trills <- function(trills, trill) {
  line <- trill$line
  i <- trill$i
  j <- trill$j

  # for convenience
  if (is.na(j)) j <- i

  # Trills to remove
  ks <- NULL

  for (k in seq_len(NROW(trills))) {
    trill_k <- trills[k, ]

    if (trill_k$line != line) next

    i_k <- trill_k$i
    j_k <- trill_k$j

    # for convenience
    if (is.na(j_k)) j_k <- i_k

    if (j_k < i || i_k > j) next

    ks <- c(ks, k)

    # replace the Trill in the Music that is the same as the incoming Trill
    if (i == i_k && j == j_k) break

    # drop the incoming Trill if it is "covered" by any Trill in the Music
    if (i >= i_k && j <= j_k) return(trills)

    # merge the Trills in the Music that "touch" the incoming Trill
    if (i >= i_k && i <= j_k) {
      i <- i_k
      trill$i <- i

    } else if (j >= i_k && j <= j_k) {
      j <- j_k
      trill$j <- j
    }
  }

  if (!is.null(ks)) trills <- trills[-ks, ]
  rbind(trills, to_case(trill))
}

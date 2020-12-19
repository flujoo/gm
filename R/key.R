# Key ---------------------------------------------------------------------

#' @export
Key <- function(key, bar = 1, to = NULL, to_part = NULL) {
  check_key(key)
  check_n(bar, "bar")
  check_line_to(to)
  check_key_to_part(to_part, to)

  to_part <- normalize_key_to_part(to_part, to)

  list(key = key, bar = bar, to = to, to_part = to_part) %>%
    `class<-`(c("Key", "BarAddOn", "Printable"))
}


check_key <- function(key) {
  check_type(key, c("double", "integer"))
  check_length(supplied = key, valid = 1, name = "key", type = "numeric")

  check_content(
    supplied = key,
    valid = -7:7,
    general = "`key` must be any integer between -7 and 7."
  )
}


# will be deprecated
check_key_to_part <- function(to_part, to) {
  if (is.null(to) || is.null(to_part)) {
    return(invisible(NULL))
  }

  check_type(supplied = to_part, valid = "logical", name = "to_part")
  check_length(
    supplied = to_part, valid = 1, name = "to_part", type = "logical"
  )
  check_na(supplied = to_part, name = "to_part")
}


normalize_key_to_part <- function(to_part, to) {
  if (is.null(to)) {
    return(NULL)

  } else {
    if (is.null(to_part)) {
      return(TRUE)

    } else {
      return(to_part)
    }
  }
}


#' @keywords internal
#' @export
to_string.Key <- function(x, ...) {
  x$key
}



# + Key -------------------------------------------------------------------

#' @keywords internal
#' @export
add.Key <- function(term, music) {
  to <- term$to

  if (is.null(to)) {
    return(add.BarAddOn(term, music))
  }

  line_names <- music$line_names
  check_add_line_to(to, line_names)

  parts <- music$parts
  l <- length(parts)

  for (i in 1:l) {
    part <- parts[[i]]
    staffs <- part$staffs
    m <- length(staffs)

    for (j in 1:m) {
      staff <- staffs[[j]]
      voices <- staff$voices
      n <- length(voices)

      for (k in 1:n) {
        voice <- voices[[k]]
        voice_name <- voice$name

        if (to == voice_name) {
          if (term$to_part == TRUE) {
            kl <- part$key_line

            if (is.null(kl)) {
              kl <- BarAddOnLine(list(), "Key")
            }

            music$parts[[i]]$key_line <- kl + term

          } else {
            kl <- staff$key_line

            if (is.null(kl)) {
              kl <- BarAddOnLine(list(), "Key")
            }

            music$parts[[i]]$staffs[[j]]$key_line <- kl + term
          }
        }
      }
    }
  }

  music
}

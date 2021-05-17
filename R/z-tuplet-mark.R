# add `$tuplet_start/stop` to mark the first/last tuplet of a group
# the value gives of which levels the tuplet is the start/stop
mark_tuplets <- function(durations) {
  wm <- list()

  for (i in seq_along(durations)) {
    d <- durations[[i]]

    # skip non-tuplets
    if (!is_tuplet(d)) {
      next
    }

    # get the depth of `d`
    depth <- length(d$tuplers)

    l <- length(wm)

    # add `$tuplet_start` to current tuplet
    if (l == 0) {
      durations[[i]]$tuplet_start <- 1:depth

    } else {
      last <- wm[[l]]
      depth_last <- length(last$tuplers)

      if (depth_last < depth) {
        durations[[i]]$tuplet_start <- (depth_last + 1):depth
      }
    }

    # add `d` to `wm`
    wm %<>% c(list(d))

    # reduce tuplets in `wm`
    wm %<>% reduce_tuplets()

    # get the length of `wm` again
    l <- length(wm)

    # add `$tuplet_stop` to current tuplet
    if (l == 0) {
      durations[[i]]$tuplet_stop <- 1:depth

    } else {
      last <- wm[[l]]
      depth_last <- length(last$tuplers)

      if (depth_last < depth) {
        durations[[i]]$tuplet_stop <- (depth_last + 1):depth
      }
    }
  }

  durations
}

gm_options <- list(
  # shorten `$notes$dn`
  gm.shorten_dn = FALSE
)


# gm options that are already set will not be changed
.onLoad <- function(libname, pkgname) {
  to_set <- !(names(gm_options) %in% names(options()))

  if (any(to_set)) {
    options(gm_options[to_set])
  }

  invisible()
}


# gm options that are different from the default
# are assumed set by user, and thus will not be unset
.onUnload <- function(libpath) {
  # names of gm options to unset
  names_to_unset <- NULL

  for (name in names(gm_options)) {
    # get the value of `name` from `gm_options`
    default <- gm_options[[name]]

    # get the value of `name` from `options()`
    set <- getOption(name)

    # unset `name` only if `set` and `default` are identical
    if (identical(default, set)) {
      names_to_unset %<>% c(name)
    }
  }

  # unset default gm options
  if (!is.null(names_to_unset)) {
    to_unset <- vector("list", length(names_to_unset))
    names(to_unset) <- names_to_unset
    options(to_unset)
  }

  invisible()
}

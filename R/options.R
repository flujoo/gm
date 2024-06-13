.onLoad <- function(libname, pkgname) {
  settings <- list(
    gm.musescore_path = NULL,
    gm.context = infer_context(),
    gm.show_to = c("score", "audio")
  )

  to_set <- !names(settings) %in% names(options())
  if (any(to_set)) options(settings[to_set])
  invisible()
}


.onUnload <- function(libpath) {
  settings <- list(
    gm.musescore_path = NULL,
    gm.context = NULL,
    gm.show_to = NULL
  )

  options(settings)
  invisible()
}

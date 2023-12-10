# cran.r-project.org/doc/manuals/r-devel/R-exts.html#Package-subdirectories

# Note that all user-level objects in a package should be documented;
# if a package pkg contains user-level objects which are for “internal” use
# only, it should provide a file pkg-internal.Rd which documents all such
# objects, and clearly states that these are not meant to be called by the
# user. See e.g. the sources for package grid in the R distribution.

#' Internal gm Functions
#'
#' These are not to be called by the user.
#'
#' @aliases
#' add
#' add.Accidental
#'
#' locate
#' locate.Accidental
#'
#' @keywords internal
#' @name gm-internal
NULL

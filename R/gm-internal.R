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
#' add.Articulation
#' add.Clef
#' add.Dynamic
#' add.Fermata
#' add.Grace
#' add.Hairpin
#' add.Instrument
#' add.Key
#' add.Line
#' add.Lyric
#' add.Meter
#' add.Mordent
#' add.Schleifer
#' add.Tremolo
#' add.Turn
#' add.Notehead
#' add.Velocity
#' add.Tie
#' add.Breath
#' add.Pedal
#' add.Slur
#' add.Stem
#' add.Tempo
#'
#' locate
#' locate.Accidental
#' locate.Articulation
#' locate.Clef
#' locate.Dynamic
#' locate.Fermata
#' locate.Grace
#' locate.Hairpin
#' locate.Instrument
#' locate.Key
#' locate.Meter
#' locate.Notehead
#' locate.Velocity
#' locate.Tie
#' locate.Breath
#' locate.Slur
#' locate.Stem
#' locate.Tempo
#'
#' check_add_to
#' check_add_to.default
#' check_add_to.character
#' check_add_to.numeric
#'
#' to_string
#' to_string.Clef
#' to_string.Key
#' to_string.Pitch
#' to_string.Duration
#' to_string.Meter
#' to_string.MusicXML
#'
#' to_value
#' to_value.Pitch
#' to_value.Duration
#' to_value.Meter
#'
#' to_Pitch
#' to_Pitch.character
#' to_Pitch.numeric
#'
#' to_Duration
#' to_Duration.character
#'
#' to_MusicXML
#' to_MusicXML.Pitch
#' to_MusicXML.Breath
#' to_MusicXML.Duration
#' to_MusicXML.Note
#' to_MusicXML.Tempo
#'
#' to_fraction
#' to_fraction.Meter
#' to_fraction.Duration
#'
#' print.Pitch
#' print.Duration
#' print.MusicXML
#'
#' insert
#'
#' @keywords internal
#' @name gm-internal
NULL

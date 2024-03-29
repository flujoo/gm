% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/music.R
\name{Music}
\alias{Music}
\title{Initialize \code{Music} Object}
\usage{
Music()
}
\value{
A list with class \code{Music}.
}
\description{
Initialize a \code{Music} object.

\code{Music} objects represent whole music pieces.
}
\details{
A typical workflow with \code{Music} objects:
\enumerate{
\item Initialize an empty \code{Music} object with \code{\link[=Music]{Music()}}.
\item Add components to it with \code{\link[=+.Music]{+.Music()}}.
\item Print it, or display it as musical score or audio file with
\code{\link[=show]{show()}}, to check its structure.
\item Keep adding components and checking it until you get what you want.
\item Sometimes you may want to export the final \code{Music} object with
\code{\link[=export]{export()}}.
}
}
\examples{
# initialize a Music object
Music()

# print a Music object to check its structure
m <- Music() + Meter(4, 4) + Line(list("C4"), list(4))
m
}
\seealso{
\code{\link[=+.Music]{+.Music()}} for adding components to a \code{Music} object.

\code{\link[=show]{show()}} for displaying a \code{Music} object as musical score and
audio file.

\code{\link[=export]{export()}} for exporting a \code{Music} object to various file formats.
}

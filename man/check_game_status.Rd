% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_game_status.R
\name{check_game_status}
\alias{check_game_status}
\title{Check Hoot Owl Hoot! game}
\usage{
check_game_status(game)
}
\arguments{
\item{game}{A game object}
}
\value{
A character string with values win, loss, or in progress.
}
\description{
Game can be a win, loss, or in progress.
}
\examples{
game <- setup()
check(game)

}
\seealso{
\code{\link{setup}}
}


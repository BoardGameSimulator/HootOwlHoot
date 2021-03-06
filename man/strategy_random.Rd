% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/strategy_random.R
\name{strategy_random}
\alias{strategy_random}
\title{Play a random strategy}
\usage{
strategy_random(game)
}
\arguments{
\item{game}{A current game state}
}
\value{
list with these elements:
  \code{card} string indicating what card to play
  \code{owl} an integer indicating the owl to move
}
\description{
Play a sun if you have a sun, otherwise play a random card
on a random owl.
}
\examples{
game <- setup_game()
strategy_random(game)
}
\seealso{
\code{\link{play_card}}
}


#' Play a last-owl-farthest strategy
#'
#' Play a sun if you have a sun, otherwise move the last owl using a random
#' card.
#'
#' @param game A current game state
#' @return list with these elements:
#'   \code{card} string indicating what card to play
#'   \code{owl} an integer indicating the owl to move
#' @export
#' @seealso \code{\link{play_card}}
#' @examples
#' game <- setup_game()
#' strategy_last_owl_random(game)
strategy_last_owl_random <- function(game) {
  play <- list()

  # choose sun, if no sun a random card
  player_cards <- game$player[,game$turn]
  if ("sun" %in% player_cards) {
    play$card <- "sun"
  } else {
    play$card <- sample(player_cards,1)
  }

  # if sun, owl is NULL otherwise move last owl
  if (play$card != "sun") {
    n_owls <- sum(game$board$occupied)
    play$owl <- n_owls
  }

  return(play)
}

#' Play a random strategy
#'
#' Play a sun if you have a sun, otherwise play a random card
#' on a random owl.
#'
#' @param game
#' @return list with these elements:
#'   \code{card} string indicating what card to play
#'   \code{owl} an integer indicating the owl to move
#' @export
#' @seealso \link{\code{play_card}}
#' @examples
#' game <- setup_game()
#' strategy_random(game)
strategy_random <- function(game) {
  play <- list()

  # choose sun, if no sun a random card
  player_cards <- game$player[,game$turn]
  if ("sun" %in% player_cards) {
    play$card <- "sun"
  } else {
    play$card <- sample(player_cards,1)
  }

  # if sun, owl is NULL otherwise pick random owl
  if (play$card != "sun") {
    n_owls <- sum(game$board$occupied)
    play$owl <- sample(n_owls, 1)
  }

  return(play)
}

#' Updates the game state to the next player
#'
#' @param game A current game state
#' @return An updated game
#' @seealso \code{\link{play}}
#'
next_player <- function(game) {
  n_players <- ncol(game$player)
  game$turn <- game$turn + 1

  if (game$turn > n_players)
    game$turn <- 1

  return(game)
}

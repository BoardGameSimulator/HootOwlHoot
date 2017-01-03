#' Simulate a Hoot Owl Hoot! game
#'
#' @param n_players An integer specifying the number of players
#' @param n_owls An integer specifying the number of owls
#' @param strategy A function specifying a strategy for all players to use or a
#'   list of length \code{n_players} where element i specifies a strategy for
#'   player i. See details for more information.
#' @param n_cards_per_player An integer specifying the number of cards for each
#'   player.
#' @param verbosity An integer specifying how much detail to print out (0=none).
#' @return A game end state
#' @details A strategy is a function taking a \code{game} and returning a list
#'   with elements \code{card} and \code{owl} for the next play. See
#'   \link{\code{strategy_random}} for an example.
#' @seealso \link{\code{strategy_random}}
#' @export
#' @examples
#' simulate_game(2,2, strategy_random)
#'
simulate_game <- function(n_players, n_owls, strategy, n_cards_per_player=3,
                          verbose = 0) {
  game <- setup_game(n_players = n_players,
                     n_owls    = n_owls,
                     n_cards_per_player = n_cards_per_player)

  if (is.function(strategy)) {
    tmp <- strategy
    strategy <- list()
    for (i in 1:n_players)
      strategy[[i]] <- tmp
    names(strategy) <- paste("Strategy for player ", 1:n_players)
  }

  stopifnot(length(strategy) == n_players)

  round <- 0
  if (verbose) cat("Round:")
  while(check_game_status(game) == "in progress") {
    if (game$turn == 1 & verbose) {
      round <- round+1
      cat(round," ",sep="")
    }

    play <- strategy[[game$turn]](game)
    game <- play_card(card = play$card,
                      game = game,
                      owl  = play$owl)
  }

  return(game)
}

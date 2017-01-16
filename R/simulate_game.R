#' Simulate a Hoot Owl Hoot! game
#'
#' @param n_players An integer specifying the number of players
#' @param n_owls An integer specifying the number of owls
#' @param strategy A function specifying a strategy for all players to use or a
#'   list of length \code{n_players} where element i specifies a strategy for
#'   player i. See details for more information. This can also be a character
#'   of length 1 indicating one of the built-in strategies.
#' @param n_cards_per_player An integer specifying the number of cards for each
#'   player.
#' @param verbosity An integer specifying how much detail to print out (0=none).
#' @return A game end state
#' @details A strategy is a function taking a \code{game} and returning a list
#'   with elements \code{card} and \code{owl} for the next play. See
#'   \code{\link{strategy_random}} for an example.
#' @seealso \code{\link{strategy_random}}
#' @export
#' @examples
#' simulate_game(2,2, strategy_random)
#'
simulate_game <- function(n_players, n_owls, strategy,
                          n_cards_per_player = 3,
                          verbosity = 0) {
  game <- setup_game(n_players = n_players,
                     n_owls    = n_owls,
                     n_cards_per_player = n_cards_per_player)

  # If strategy is a string, then we need to call up the appropriate functions.
  if (is.character(strategy)) {
    # Check to make sure string is in list of known strategies
    stopifnot(all(strategy %in% c("random","last_owl_farthest","last_owl_random"))) 
    
    if (length(strategy) > 1) 
      stop("Strategy character vectors are not implemented yet.")
    
    strategy <- switch(strategy,
                       random            = strategy_random,
                       last_owl_farthest = strategy_last_owl_farthest,
                       last_owl_random   = strategy_last_owl_random)
  }
  
  if (is.function(strategy)) {
    tmp <- strategy
    strategy <- list()
    for (i in 1:n_players)
      strategy[[i]] <- tmp
    names(strategy) <- paste("Strategy for player ", 1:n_players)
  }

  stopifnot(length(strategy) == n_players)

  round <- 0
  if (verbosity) cat("Round:")
  while(check_game_status(game) == "in progress") {
    if (game$turn == 1 & verbosity) {
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

#' Run a Hoot Owl Hoot! experiment
#'
#' Simulate a Hoot Owl Hoot! game and record whether the game was
#' a win or loss, how many cards were played, and how many suns were played.
#'
#' @param n_players An integer specifying the number of players
#' @param n_owls An integer specifying the number of owls
#' @param strategy A function specifying a strategy for all players to use or a
#'   list of length \code{n_players} where element i specifies a strategy for
#'   player i. See details for more information.
#' @param n_cards_per_player An integer specifying the number of cards for each
#'   player.
#' @param verbosity An integer specifying how much detail to print out (0=none).
#' @return A data.frame with end game statistics
#' @seealso \code{\link{simulate_game}}
#' @export
#' @examples
#' run_experiment(n_players = 2, n_owls = 2, strategy = strategy_random)
#'
run_experiment <- function(n_players, n_owls, strategy,
                           n_cards_per_player, verbosity) {
  game <- simulate_game(n_players = n_players,
                        n_owls = n_owls,
                        strategy = strategy,
                        n_cards_per_player = n_cards_per_player,
                        verbosity)

  data.frame(win = check_game_status(game) == "win",
             n_cards_played = length(game$discard),
             n_suns_played = 13 - game$sun)
}



#' Run a series of Hoot Owl Hoot! experiments
#'
#' This function will simulate a bunch of Hoot Owl Hoot! games with a set
#' strategy and record the end game status of those games.
#'
#' @param n_reps An integer indicating how many reps of each combination to run.
#' @param n_players A vector of integers specifying the number of players
#' @param n_owls A vector of integers specifying the number of owls
#' @param strategy A function specifying a strategy for all players to use or a
#'   list of length \code{n_players} where element i specifies a strategy for
#'   player i. The same strategy will be used for all games.
#' @param n_cards_per_player A vector of integers specifying the number of cards
#' for each player.
#' @param verbosity An integer specifying how much detail to print out (0=none).
#' @seealso \code{\link{run_experiment}}
#' @export
#' @import dplyr
#' @examples
#' run_experiments(n_reps = 2, n_players = 2:3, n_owls = 1:2,
#'                 strategy = strategy_random,
#'                 n_cards_per_player = 2:3,
#'                 verbosity = 0)
#'
run_experiments <- function(n_reps, n_players, n_owls,
                            strategy = strategy_random,
                            n_cards_per_player = 3,
                            verbosity = 0) {

  experiments <- expand.grid(rep = 1:n_reps,
                             n_players = n_players,
                             n_owls = n_owls,
                             n_cards_per_player = n_cards_per_player)

  run_experiment_wrap <- function(d, .strategy, .verbosity) {
    run_experiment(n_players = d$n_players,
                   n_owls = d$n_owls,
                   strategy = .strategy,
                   n_cards_per_player = d$n_cards_per_player,
                   verbosity = .verbosity)
  }

  results <- experiments %>%
    rowwise() %>%
    do(run_experiment_wrap(.,.strategy = strategy, .verbosity = verbosity))

  cbind(experiments, results)
}

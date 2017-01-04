#' Play a last-owl-farthest strategy
#'
#' Play a sun if you have a sun, otherwise move the last owl to the location
#' that moves them the farthest.
#'
#' @param game A current game state
#' @return list with these elements:
#'   \code{card} string indicating what card to play
#'   \code{owl} an integer indicating the owl to move
#' @export
#' @seealso \link{\code{strategy_random}}
#' @examples
#' game <- setup_game()
#' strategy_last_owl_farthest(game)
strategy_last_owl_farthest <- function(game) {
  play <- list()

  # choose sun, if no sun choose the farthest move for last owl
  player_cards <- game$player[,game$turn]
  if ("sun" %in% player_cards) {
    play$card <- "sun"
  } else {
    # Find board locations available in front of last owl
    board_colors <- game$board$color[1:(max(which(game$board$occupied))-1)]
    board_colors <- board_colors[!game$board$occupied[1:length(board_colors)]]

    # Find which card will move owl the farthest by reversing the board colors
    rev_colors <- rev(board_colors)
    n_spaces_moved <- numeric(length(player_cards))
    for (i in seq_along(player_cards)) {
      n_spaces_moved[i] <- match(player_cards[i],rev_colors)
    }

    # Play the card that moved the maximum number of spaces
    play$card <- player_cards[which.max(n_spaces_moved)]
    if (length(play$card) == 0) # owl can move into nest with any card
      play$card <- sample(player_cards, 1)
  }

  # if sun, owl is NULL otherwise move last owl
  if (play$card != "sun") {
    n_owls <- sum(game$board$occupied)
    play$owl <- n_owls
  }

  return(play)
}

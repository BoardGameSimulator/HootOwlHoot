#' Discard a card and draw a new one
#'
#' @param card A string indicating the card
#' @param game A current game state
#' @return An updated game state
#' @seealso \link{\code{play_card}}
#'
discard_and_draw <- function(card, game) {
  # check_card(card, game) # should we check here again?

  player_cards <- game$player[,game$turn]
  player_cards <- player_cards[-match(card, player_cards)]
  player_cards <- c(player_cards, game$deck[1])
  game$player[,game$turn] <- player_cards

  game$discard <- c(game$discard, card)
  game$deck    <- game$deck[-1]

  return(game)
}

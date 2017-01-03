#' Play a card
#'
#' @param card A character string indicating a card
#' @param owl An integer specifying which owl to move. Owl #1 is closest to the
#'   end.
#' @param game A game state
#' @return An updated game state
#' @export
#' @seealso \link{\code{setup_game}}
#' @examples
#' game <- setup_game()
#' game <- play_card(game$player$player1[1])
#'
play_card <- function(card, game, owl=NULL) {

  check_card(card, game)

  if (card == "sun") {
    game$sun <- game$sun-1
    if (!is.null(owl))
      warning("`owl` argument was ignored")
  } else {
    game <- move_owl(card, owl, game)
  }

  game <- discard_and_draw(card, game)
  game <- next_player(game)

  return(game)
}

#' Check the card the player tries to play
#'
#' @param card A string indicating the card a player is trying to play
#' @param game A current game state
#' @return Invisibly TRUE
#'
check_card <- function(card, game) {

  player_cards <- unique(game$player[,game$turn])

  if (!isTRUE(card %in% player_cards))
    stop(paste("Invalid card! Choices:", paste(player_cards, collapse = " ")))

  if (card != "sun" & "sun" %in% player_cards) {
    stop("You must play a sun.")
  }

  return(TRUE)
}

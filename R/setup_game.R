#' Setup Hoot Owl Hoot! Game
#'
#' @param n_players An integer (2-4) specifying the number of players
#' @param n_owls An integer (1-6) specifying the number of owls
#' @param n_cards_per_player An integer specifying the number of cards per
#'   player.
#' @return A list the following items:
#'   \code{deck} contains the shuffled cards
#'   \code{player} is a n_cards_per_player x n_players matrix with the in hand
#'     cards
#'   \code{board} a \code{data.frame} containing the spaces on the board and
#'     whether those spaces are occupied
#'   \code{sun} an integer specifying where the sun is (0 is a loss)
#'   \code{discard} the cards in the discard pile (initially NULL)
#'   \code{turn} an integer specifying whose turn it is
#' @export
#' @examples
#' board <- setup_game(n_players = 3, n_owls = 4)
#'
setup_game <- function(n_players = 2, n_owls = 3, n_cards_per_player = 3) {
  game <- list()

  cards <- rep(c("sun",
                     "yellow","blue","green",
                     "red","purple","orange"),
                   times = c(14, rep(6,6)))

  game$deck <- shuffle(cards)

  # Deal cards
  card_indices_to_deal <- 1:(n_players*n_cards_per_player)
  game$player <- matrix(game$deck[card_indices_to_deal],
                        nrow = n_cards_per_player,
                        ncol = n_players)
  colnames(game$player) <- paste0("player", 1:n_players)
  game$deck <- game$deck[-card_indices_to_deal]

  # Place owls
  board_spaces <- c("purple","red","orange",
                    "blue","green","yellow",
                    "purple","red","orange",
                    "blue","green","yellow",
                    "red","purple","blue",
                    "orange","yellow","green",
                    "red","purple","blue",
                    "orange","green","yellow",
                    "purple","red","orange",
                    "blue","green","yellow",
                    "red","purple","blue",
                    "red","purple","blue",
                    "orange","green","yellow")
  game$board <- data.frame(space = board_spaces,
                           occupied = FALSE)
  board_length <- length(board_spaces)
  occupied_spaces <- (board_length-5):(board_length-5+n_owls-1)
  game$board$occupied[occupied_spaces] <- TRUE

  # set sun
  game$sun <- 13 # 0 is loss

  game$discard <- NULL

  game$turn <- 1

  return(game)
}

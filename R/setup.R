#' Setup Hoot Owl Hoot! Game
#'
#' @param n_players An integer (2-4) specifying the number of players
#' @param n_owls An integer (1-6) specifying the number of owls
#' @export
#' @examples
#' board <- setup(n_players = 3, n_owls = 4)
#'
setup <- function(n_players = 2, n_owls = 3, n_cards_per_player = 3) {
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

  return(game)
}

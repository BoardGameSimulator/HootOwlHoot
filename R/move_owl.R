#' Move an owl to specified location
#'
#' @param card A string specifying a card
#' @param owl An integer specifying an owl
#' @param game A current game state
#' @return An updated game state
#'
move_owl <- function(card, owl, game) {

  check_owl(owl, game)

  owl_location <- which(game$board$occupied)[owl]
  game$board$occupied[owl_location] <- FALSE

  # Find a valid space and move it there
  valid_space <- which(game$board$color[1:(owl_location-1)] == card &
                         !game$board$occupied[1:(owl_location-1)])

  if (length(valid_space) > 0) {
    game$board$occupied[max(valid_space)] <- TRUE
  }

  # If no spaces are valid, then the owl "leaves" the board.

  return(game)
}


#' Check to make sure owl is valid
#'
#' @param owl An integer specifying an owl
#' @param game A current game state
#' @return Invisibly TRUE
#'
check_owl <- function(owl, game) {
  n_owls_in_game <- sum(game$board$occupied)
  if (owl > n_owls_in_game)
    stop(paste("`owl` must be less than",n_owls_in_game))

  return(invisible(TRUE))
}

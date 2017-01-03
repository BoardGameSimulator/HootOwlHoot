#' Check Hoot Owl Hoot! game
#'
#' Game can be a win, loss, or in progress.
#'
#' @param game A game object
#' @return A character string with values win, loss, or in progress.
#'
check <- function(game) {
  if (game$sun == 0) return("loss")
  if (all(!game$board$occupied)) return("win")
  return("in progress")
}

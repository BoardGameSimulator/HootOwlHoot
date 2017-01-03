#' Shuffle cards
#'
#' @param cards A character vector of cards
#' @export
#' @examples
#' discard = c("this","that")
#' shuffle(discard)
#'
shuffle <- function(cards) {
  sample(cards)
}

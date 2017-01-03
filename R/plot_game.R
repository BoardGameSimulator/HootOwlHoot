#' Plot a current game state
#'
#' @param game A current game state
#' @import dplyr
#' @examples
#' game <- setup_game()
#' plot_game(game)
#'
plot_game <- function(game) {
  requireNamespace("dplyr", quietly = TRUE)

  cp <- as.character(unique(game$board$color))
  names(cp) <- cp

  g <- ggplot(game$board, aes(x=space, color=color)) +
    geom_point(aes(y=1), size=2)

  if (any(game$board$occupied))
    g <- g + geom_text(data = game$board %>% filter(occupied),
                       aes(x=space, y=1), label="X", color="black") +

  g <- g +
    scale_color_manual(values = cp) +
    theme_minimal()
}

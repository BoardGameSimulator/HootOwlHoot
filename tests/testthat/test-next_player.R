context("test-next_player")

test_that("next_player gives errors", {
  expect_error(next_player())
  expect_error(next_player(list(player=1)))
  expect_error(next_player(list(turn=1)))
})

test_that("next_player gives correct answers", {
  game <- list(player = matrix(NA, ncol=2, nrow=2),
               turn   = 1)

  expect_equal(next_player(game),
               list(player = game$player,
                    turn   = game$turn+1))

  game$turn = 2

  expect_equal(next_player(game),
               list(player = game$player,
                    turn   = 1))

})


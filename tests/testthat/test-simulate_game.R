context("test-simulate_game")

test_that("simulate_game runs", {
  expect_error(simulate_game(2,2,strategy_random), NA)
  expect_error(simulate_game(2,2,"random"), NA)
})




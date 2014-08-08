context("Random generation function")

test_that("generating from a single binomial works", {
  expect_that(rsumbin(1, size=c(5), prob=c(0.1)), not(throws_error()))
})

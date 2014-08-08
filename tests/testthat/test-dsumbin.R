context("Density function")

test_that("adding two binomials works", {
  expect_that(dsumbin(10, size=c(5,5), prob=c(1, 1)), equals(1))
})


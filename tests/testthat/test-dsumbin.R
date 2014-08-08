context("Density function")

test_that("adding two binomials works", {
  expect_that(dsumbin(10, size=c(5,5), prob=c(1, 1)), equals(1))
})

test_that("one binomial works", {
  expect_that(dsumbin(3, size=c(5), prob=(.3)), not(throws_error())) 
})
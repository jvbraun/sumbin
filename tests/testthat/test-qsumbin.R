context("Quantile function")

test_that("error checking the prob parameter works", {
  expect_that(qsumbin(0.5, size=c(5,5), prob=c(0.10, 2.5)), throws_error())
  expect_that(qsumbin(0.5, size=c(5,5), prob=c("0.1", "0.2")), throws_error())
})

test_that("values make sense", {
  expect_that(qsumbin(0, size=c(5,5), prob=c(0.10, 0.3)), equals(0))
  expect_that(qsumbin(1, size=c(5,5), prob=c("0.1", "0.2")), throws_error())
})

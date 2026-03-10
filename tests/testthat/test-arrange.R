test_that("arrange() reorders frames", {
  images <- make_film(n = 3L, labels = c("A", "B", "C"))
  result <- suppressMessages(arrange(images, order = c(3L, 1L, 2L)))
  expect_equal(length(result), 3L)
  expect_equal(attr(result, "labels"), c("C", "A", "B"))
})

test_that("arrange() with identity order is a no-op on labels", {
  images <- make_film(n = 3L, labels = c("A", "B", "C"))
  result <- suppressMessages(arrange(images, order = c(1L, 2L, 3L)))
  expect_equal(attr(result, "labels"), c("A", "B", "C"))
})

test_that("arrange() errors when order has wrong length", {
  images <- make_film(n = 3L)
  expect_error(arrange(images, order = c(1L, 2L)))
})

test_that("arrange() errors when order contains out-of-bounds index", {
  images <- make_film(n = 3L)
  expect_error(arrange(images, order = c(1L, 2L, 4L)))
})

test_that("arrange() errors when order contains index below 1", {
  images <- make_film(n = 3L)
  expect_error(arrange(images, order = c(0L, 1L, 2L)))
})

test_that("arrange() errors when order has duplicate indices", {
  images <- make_film(n = 3L)
  expect_error(arrange(images, order = c(1L, 1L, 2L)))
})

test_that("arrange() messages the frame sequence", {
  images <- make_film(n = 2L)
  withr::with_options(list(stopmotion.verbose = TRUE), {
    expect_message(arrange(images, order = c(2L, 1L)), regexp = "Frame sequence after arrange")
  })
})

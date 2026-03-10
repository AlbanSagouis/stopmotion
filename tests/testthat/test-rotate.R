test_that("rotate() does not change frame count", {
  images <- make_film(n = 3L)
  result <- suppressMessages(rotate(images = images, degrees = 90))
  expect_equal(length(result), 3L)
})

test_that("rotate() labels all frames when frames = NULL", {
  images <- make_film(n = 3L, labels = c("A", "B", "C"))
  result <- suppressMessages(rotate(images = images, degrees = 90))
  expect_equal(attr(result, "labels"), c("A [rotated]", "B [rotated]", "C [rotated]"))
})

test_that("rotate() only labels the selected frames", {
  images <- make_film(n = 3L, labels = c("A", "B", "C"))
  result <- suppressMessages(rotate(images = images, degrees = 90, frames = 2L))
  expect_equal(attr(result, "labels"), c("A", "B [rotated]", "C"))
})

test_that("rotate() handles first frame", {
  images <- make_film(n = 3L, labels = c("A", "B", "C"))
  result <- suppressMessages(rotate(images = images, degrees = 45, frames = 1L))
  expect_equal(length(result), 3L)
  expect_equal(attr(result, "labels")[[1]], "A [rotated]")
})

test_that("rotate() handles last frame", {
  images <- make_film(n = 3L, labels = c("A", "B", "C"))
  result <- suppressMessages(rotate(images = images, degrees = 45, frames = 3L))
  expect_equal(length(result), 3L)
  expect_equal(attr(result, "labels")[[3]], "C [rotated]")
})

test_that("rotate() errors on degrees outside [-360, 360]", {
  images <- make_film(n = 3L)
  expect_error(rotate(images = images, degrees = 400))
  expect_error(rotate(images = images, degrees = -400))
})

test_that("rotate() messages the frame sequence", {
  images <- make_film(n = 2L)
  withr::with_options(list(stopmotion.verbose = TRUE), {
    expect_message(
      rotate(images = images, degrees = 45),
      regexp = "Frame sequence after rotate"
    )
  })
})

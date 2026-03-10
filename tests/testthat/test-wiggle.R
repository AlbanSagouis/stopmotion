test_that("wiggle() inserts 2 extra frames per selected frame", {
  images <- make_film(n = 3L)
  result <- suppressMessages(wiggle(images = images, degrees = 3, frames = 2L))
  expect_equal(length(result), 5L)
})

test_that("wiggle() labels include +/- degree annotations", {
  images <- make_film(n = 3L, labels = c("A", "B", "C"))
  result <- suppressMessages(wiggle(images = images, degrees = 5, frames = 2L))
  expect_equal(
    attr(result, "labels"),
    c("A", "B", "B [wiggled +5\u00b0]", "B [wiggled -5\u00b0]", "C")
  )
})

test_that("wiggle() handles multiple selected frames with correct offset", {
  images <- make_film(n = 3L)
  result <- suppressMessages(wiggle(images = images, degrees = 3, frames = c(1L, 2L)))
  expect_equal(length(result), 7L)
})

test_that("wiggle() frames = NULL applies to all frames", {
  images <- make_film(n = 2L)
  result <- suppressMessages(wiggle(images = images, degrees = 3))
  expect_equal(length(result), 6L)
})

test_that("wiggle() handles first frame", {
  images <- make_film(n = 3L)
  result <- suppressMessages(wiggle(images = images, degrees = 3, frames = 1L))
  expect_equal(length(result), 5L)
})

test_that("wiggle() handles last frame", {
  images <- make_film(n = 3L)
  result <- suppressMessages(wiggle(images = images, degrees = 3, frames = 3L))
  expect_equal(length(result), 5L)
})

test_that("wiggle() errors on negative degrees", {
  images <- make_film(n = 3L)
  expect_error(wiggle(images = images, degrees = -3))
})

test_that("wiggle() messages the frame sequence", {
  images <- make_film(n = 2L)
  withr::with_options(list(stopmotion.verbose = TRUE), {
    expect_message(
      wiggle(images = images, degrees = 3),
      regexp = "Frame sequence after wiggle"
    )
  })
})

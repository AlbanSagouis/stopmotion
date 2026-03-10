test_that("preview() returns a magick-image", {
  images <- make_film(n = 3L)
  result <- preview(images, fps = 10L)
  expect_s3_class(result, "magick-image")
})

test_that("preview() with label = FALSE returns a magick-image", {
  images <- make_film(n = 3L)
  result <- preview(images, fps = 10L, label = FALSE)
  expect_s3_class(result, "magick-image")
})

test_that("preview() frames subset returns the correct number of frames", {
  images <- make_film(n = 5L)
  result <- preview(images, fps = 10L, frames = 2:4, label = FALSE)
  expect_equal(length(result), 3L)
})

test_that("preview() accepts all valid fps divisors of 100", {
  images <- make_film(n = 2L)
  for (fps in c(1L, 2L, 4L, 5L, 10L, 20L, 25L, 50L, 100L)) {
    expect_no_error(preview(images, fps = fps, label = FALSE))
  }
})

test_that("preview() errors when fps is not a divisor of 100", {
  images <- make_film(n = 2L)
  expect_error(preview(images, fps = 7L),  regexp = "divisor")
  expect_error(preview(images, fps = 15L), regexp = "divisor")
  expect_error(preview(images, fps = 33L), regexp = "divisor")
})

test_that("preview() errors when fps is below 1", {
  images <- make_film(n = 2L)
  expect_error(preview(images, fps = 0L))
})

test_that("preview() errors on frames below 1", {
  images <- make_film(n = 3L)
  expect_error(preview(images, fps = 10L, frames = 0L))
})

test_that("preview() errors on frames exceeding film length", {
  images <- make_film(n = 3L)
  expect_error(preview(images, fps = 10L, frames = 5L))
})

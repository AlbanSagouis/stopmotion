test_that("duplicate() linear increases frame count by length(frames)", {
  images <- make_film(n = 3L)
  result <- suppressMessages(duplicate(images = images, style = "linear", frames = 2L))
  expect_equal(length(result), 4L)
})

test_that("duplicate() linear inserts [duplicated] label before the original", {
  images <- make_film(n = 3L, labels = c("A", "B", "C"))
  result <- suppressMessages(duplicate(images = images, style = "linear", frames = 2L))
  expect_equal(attr(result, "labels"), c("A", "B [duplicated]", "B", "C"))
})

test_that("duplicate() looped increases frame count by length(frames)", {
  images <- make_film(n = 4L)
  result <- suppressMessages(duplicate(images = images, style = "looped", frames = 2:3))
  expect_equal(length(result), 6L)
})

test_that("duplicate() looped inserts [duplicated] labels after max(frames)", {
  images <- make_film(n = 4L, labels = c("A", "B", "C", "D"))
  result <- suppressMessages(duplicate(images = images, style = "looped", frames = 2:3))
  expect_equal(attr(result, "labels"), c("A", "B", "C", "B [duplicated]", "C [duplicated]", "D"))
})

test_that("duplicate() looped handles frames reaching the last frame", {
  images <- make_film(n = 3L, labels = c("A", "B", "C"))
  result <- suppressMessages(duplicate(images = images, style = "looped", frames = 2:3))
  expect_equal(length(result), 5L)
  expect_equal(attr(result, "labels"), c("A", "B", "C", "B [duplicated]", "C [duplicated]"))
})

test_that("duplicate() shuffle produces correct frame count", {
  images <- make_film(n = 4L)
  result <- suppressMessages(duplicate(images = images, style = "shuffle", frames = 2:3))
  expect_equal(length(result), 6L)
})

test_that("duplicate() shuffle labels include [shuffled]", {
  images <- make_film(n = 3L, labels = c("A", "B", "C"))
  result <- suppressMessages(duplicate(images = images, style = "shuffle", frames = 2:3))
  labels <- attr(result, "labels")
  expect_true(all(grepl("\\[shuffled\\]", labels[2:5])))
})

test_that("duplicate() frames = NULL defaults to all frames", {
  images <- make_film(n = 2L)
  result <- suppressMessages(duplicate(images = images, style = "looped"))
  expect_equal(length(result), 4L)
})

test_that("duplicate() errors when frames exceed image length", {
  images <- make_film(n = 3L)
  expect_error(duplicate(images = images, style = "linear", frames = 5L))
})

test_that("duplicate() linear with multiple frames targets the correct originals", {
  images <- make_film(n = 4L, labels = c("A", "B", "C", "D"))
  result <- suppressMessages(duplicate(images = images, style = "linear", frames = c(2L, 3L)))
  # Each duplication inserts one copy before the original, offset-adjusted for prior insertions.
  # Expected sequence: A | B[dup] B | C[dup] C | D
  expect_equal(length(result), 6L)
  expect_equal(
    attr(result, "labels"),
    c("A", "B [duplicated]", "B", "C [duplicated]", "C", "D")
  )
})

test_that("duplicate() looped errors on non-consecutive frames", {
  images <- make_film(n = 5L)
  expect_error(
    duplicate(images = images, style = "looped", frames = c(1L, 3L)),
    regexp = "consecutive"
  )
})

test_that("duplicate() shuffle errors on non-consecutive frames", {
  images <- make_film(n = 5L)
  expect_error(
    duplicate(images = images, style = "shuffle", frames = c(1L, 3L)),
    regexp = "consecutive"
  )
})

test_that("duplicate() errors on frames below 1", {
  images <- make_film(n = 3L)
  expect_error(duplicate(images = images, style = "linear", frames = 0L))
})

test_that("duplicate() messages the frame sequence", {
  images <- make_film(n = 2L, labels = c("A", "B"))
  withr::with_options(list(stopmotion.verbose = TRUE), {
    expect_message(
      duplicate(images = images, style = "linear", frames = 1L),
      regexp = "Frame sequence after duplicate"
    )
  })
})

test_that("splice() increases frame count by length(insert)", {
  images <- make_film(n = 3L)
  insert <- make_film(n = 1L)
  result <- suppressMessages(splice(images = images, insert = insert, after = 2L))
  expect_equal(length(result), 4L)
})

test_that("splice() inserts [spliced] label at the correct position", {
  images <- make_film(n = 3L, labels = c("A", "B", "C"))
  insert <- make_film(n = 1L, labels = "X")
  result <- suppressMessages(splice(images = images, insert = insert, after = 2L))
  expect_equal(attr(result, "labels"), c("A", "B", "X [spliced]", "C"))
})

test_that("splice() can insert multiple frames at once", {
  images <- make_film(n = 3L)
  insert <- make_film(n = 2L)
  result <- suppressMessages(splice(images = images, insert = insert, after = 1L))
  expect_equal(length(result), 5L)
})

test_that("splice() with multiple after values inserts at the correct positions", {
  images <- make_film(n = 4L, labels = c("A", "B", "C", "D"))
  insert <- make_film(n = 1L, labels = "X")
  # Insert after frame 1 and after frame 3 (original indices).
  # After first insertion: A X[s] B C D (length 5); second after original 3 → adjusted 4.
  result <- suppressMessages(splice(images = images, insert = insert, after = c(1L, 3L)))
  expect_equal(length(result), 6L)
  expect_equal(
    attr(result, "labels"),
    c("A", "X [spliced]", "B", "C", "X [spliced]", "D")
  )
})

test_that("splice() errors when after exceeds film length", {
  images <- make_film(n = 3L)
  insert <- make_film(n = 1L)
  expect_error(splice(images = images, insert = insert, after = 5L))
})

test_that("splice() errors when after is below 1", {
  images <- make_film(n = 3L)
  insert <- make_film(n = 1L)
  expect_error(splice(images = images, insert = insert, after = 0L))
})

test_that("splice() messages the frame sequence", {
  images <- make_film(n = 2L)
  insert <- make_film(n = 1L)
  withr::with_options(list(stopmotion.verbose = TRUE), {
    expect_message(
      splice(images = images, insert = insert, after = 1L),
      regexp = "Frame sequence after splice"
    )
  })
})

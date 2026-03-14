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

# ── regression: canvas size and fill colour ───────────────────────────────────

test_that("wiggle() preserves the original canvas size for all frames", {
  # Regression: image_rotate() expands the canvas; wiggle() must crop it back.
  # Before the fix, wiggled frames were ~11x11 instead of 10x10.
  images <- make_film(n = 3L)
  result <- suppressMessages(wiggle(images = images, degrees = 5, frames = 1:2))
  info <- magick::image_info(result)
  expect_true(all(info$width  == 10L))
  expect_true(all(info$height == 10L))
})

test_that("wiggle() fills rotation corners with the frame's own background colour", {
  # Regression: image_rotate() was using a stale ImageMagick fill colour
  # instead of the frame's background, producing visible artefacts.
  bg_color <- "#aabbcc"
  frame  <- magick::image_blank(20L, 20L, color = bg_color)
  images <- Reduce(c, replicate(3L, frame, simplify = FALSE))
  result <- suppressMessages(wiggle(images = images, degrees = 5, frames = 2L))
  # Wiggled frames sit at positions 3 (+5°) and 4 (-5°) after the fix.
  expected_rgb <- c(0xaaL, 0xbbL, 0xccL)
  for (pos in c(3L, 4L)) {
    corner <- as.vector(as.integer(magick::image_data(
      magick::image_crop(result[pos], "1x1+0+0"), channels = "rgb"
    )))
    expect_equal(corner, expected_rgb,
                 label = sprintf("corner of wiggled frame %d", pos))
  }
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

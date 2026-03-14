test_that("flip() does not change frame count", {
  images <- make_film(n = 3L)
  result <- suppressMessages(flip(images = images))
  expect_equal(length(result), 3L)
})

test_that("flip() labels selected frames as [flipped]", {
  images <- make_film(n = 3L, labels = c("A", "B", "C"))
  result <- suppressMessages(flip(images = images, frames = 2L))
  expect_equal(attr(result, "labels"), c("A", "B [flipped]", "C"))
})

test_that("flip() labels all frames when frames = NULL", {
  images <- make_film(n = 3L, labels = c("A", "B", "C"))
  result <- suppressMessages(flip(images = images))
  expect_true(all(grepl("\\[flipped\\]", attr(result, "labels"))))
})

test_that("scale() does not change frame count", {
  images <- make_film(n = 3L)
  result <- suppressMessages(scale(images = images, geometry = "50%"))
  expect_equal(length(result), 3L)
})

test_that("scale() labels selected frames as [scaled]", {
  images <- make_film(n = 3L, labels = c("A", "B", "C"))
  result <- suppressMessages(scale(
    images = images,
    geometry = "50%",
    frames = 2L
  ))
  expect_equal(attr(result, "labels"), c("A", "B [scaled]", "C"))
})

test_that("background() does not change frame count", {
  images <- make_film(n = 3L)
  result <- suppressMessages(background(images = images, color = "white"))
  expect_equal(length(result), 3L)
})

test_that("background() labels selected frames as [background]", {
  images <- make_film(n = 3L, labels = c("A", "B", "C"))
  result <- suppressMessages(background(
    images = images,
    color = "white",
    frames = 2L
  ))
  expect_equal(attr(result, "labels"), c("A", "B [background]", "C"))
})

test_that("trim() does not change frame count", {
  images <- make_trimmable_film(n = 3L)
  result <- suppressMessages(trim(images = images))
  expect_equal(length(result), 3L)
})

test_that("trim() labels selected frames as [trimmed]", {
  images <- make_trimmable_film(n = 3L, labels = c("A", "B", "C"))
  result <- suppressMessages(trim(images = images, frames = 2L))
  expect_equal(attr(result, "labels"), c("A", "B [trimmed]", "C"))
})

test_that("trim() labels all frames when frames = NULL", {
  images <- make_trimmable_film(n = 3L, labels = c("A", "B", "C"))
  result <- suppressMessages(trim(images = images))
  expect_true(all(grepl("\\[trimmed\\]", attr(result, "labels"))))
})

test_that("trim() errors on fuzz outside [0, 100]", {
  images <- make_film(n = 3L)
  expect_error(trim(images = images, fuzz = 150))
})

test_that("crop() does not change frame count", {
  images <- make_film(n = 3L)
  result <- suppressMessages(crop(images = images, geometry = "5x5"))
  expect_equal(length(result), 3L)
})

test_that("crop() labels selected frames as [cropped]", {
  images <- make_film(n = 3L, labels = c("A", "B", "C"))
  result <- suppressMessages(crop(
    images = images,
    geometry = "5x5",
    frames = 2L
  ))
  expect_equal(attr(result, "labels"), c("A", "B [cropped]", "C"))
})

test_that("crop() labels all frames when frames = NULL", {
  images <- make_film(n = 3L, labels = c("A", "B", "C"))
  result <- suppressMessages(crop(images = images, geometry = "5x5"))
  expect_true(all(grepl("\\[cropped\\]", attr(result, "labels"))))
})

test_that("blur() does not change frame count", {
  images <- make_film(n = 3L)
  result <- suppressMessages(blur(images = images, radius = 1, sigma = 0.5))
  expect_equal(length(result), 3L)
})

test_that("blur() labels selected frames as [blurred]", {
  images <- make_film(n = 3L, labels = c("A", "B", "C"))
  result <- suppressMessages(blur(
    images = images,
    radius = 1,
    sigma = 0.5,
    frames = 2L
  ))
  expect_equal(attr(result, "labels"), c("A", "B [blurred]", "C"))
})

test_that("blur() labels all frames when frames = NULL", {
  images <- make_film(n = 3L, labels = c("A", "B", "C"))
  result <- suppressMessages(blur(images = images))
  expect_true(all(grepl("\\[blurred\\]", attr(result, "labels"))))
})

test_that("blur() errors on negative radius", {
  images <- make_film(n = 3L)
  expect_error(blur(images = images, radius = -1))
})

test_that("flop() does not change frame count", {
  images <- make_film(n = 3L)
  result <- suppressMessages(flop(images = images))
  expect_equal(length(result), 3L)
})

test_that("flop() labels selected frames as [flopped]", {
  images <- make_film(n = 3L, labels = c("A", "B", "C"))
  result <- suppressMessages(flop(images = images, frames = 2L))
  expect_equal(attr(result, "labels"), c("A", "B [flopped]", "C"))
})

test_that("flop() labels all frames when frames = NULL", {
  images <- make_film(n = 3L, labels = c("A", "B", "C"))
  result <- suppressMessages(flop(images = images))
  expect_true(all(grepl("\\[flopped\\]", attr(result, "labels"))))
})

test_that("border() does not change frame count", {
  images <- make_film(n = 3L)
  result <- suppressMessages(border(
    images = images,
    color = "black",
    geometry = "2x2"
  ))
  expect_equal(length(result), 3L)
})

test_that("border() labels selected frames as [bordered]", {
  images <- make_film(n = 3L, labels = c("A", "B", "C"))
  result <- suppressMessages(border(
    images = images,
    color = "black",
    geometry = "2x2",
    frames = 2L
  ))
  expect_equal(attr(result, "labels"), c("A", "B [bordered]", "C"))
})

test_that("border() labels all frames when frames = NULL", {
  images <- make_film(n = 3L, labels = c("A", "B", "C"))
  result <- suppressMessages(border(
    images = images,
    color = "black",
    geometry = "2x2"
  ))
  expect_true(all(grepl("\\[bordered\\]", attr(result, "labels"))))
})

# ── flip() additional coverage ───────────────────────────────────────────────

test_that("flip() handles first frame", {
  images <- make_film(n = 3L)
  result <- suppressMessages(flip(images = images, frames = 1L))
  expect_equal(length(result), 3L)
})

test_that("flip() handles last frame", {
  images <- make_film(n = 3L)
  result <- suppressMessages(flip(images = images, frames = 3L))
  expect_equal(length(result), 3L)
})

test_that("flip() errors on frames below 1", {
  images <- make_film(n = 3L)
  expect_error(flip(images = images, frames = 0L))
})

test_that("flip() actually inverts pixel data", {
  film <- make_trimmable_film(n = 1L)
  result <- suppressMessages(flip(images = film))
  before <- as.integer(magick::image_data(film[1L], channels = "rgba"))
  after <- as.integer(magick::image_data(result[1L], channels = "rgba"))
  expect_false(identical(before, after))
})

test_that("flip() messages the frame sequence", {
  images <- make_film(n = 2L)
  withr::with_options(list(stopmotion.verbose = TRUE), {
    expect_message(flip(images = images), regexp = "Frame sequence after flip")
  })
})

# ── flop() additional coverage ───────────────────────────────────────────────

test_that("flop() handles first frame", {
  images <- make_film(n = 3L)
  result <- suppressMessages(flop(images = images, frames = 1L))
  expect_equal(length(result), 3L)
})

test_that("flop() handles last frame", {
  images <- make_film(n = 3L)
  result <- suppressMessages(flop(images = images, frames = 3L))
  expect_equal(length(result), 3L)
})

test_that("flop() errors on frames below 1", {
  images <- make_film(n = 3L)
  expect_error(flop(images = images, frames = 0L))
})

test_that("flop() actually mirrors pixel data", {
  film <- make_trimmable_film(n = 1L)
  result <- suppressMessages(flop(images = film))
  before <- as.integer(magick::image_data(film[1L], channels = "rgba"))
  after <- as.integer(magick::image_data(result[1L], channels = "rgba"))
  expect_false(identical(before, after))
})

test_that("flop() messages the frame sequence", {
  images <- make_film(n = 2L)
  withr::with_options(list(stopmotion.verbose = TRUE), {
    expect_message(flop(images = images), regexp = "Frame sequence after flop")
  })
})

# ── scale() additional coverage ──────────────────────────────────────────────

test_that("scale() labels all frames when frames = NULL", {
  images <- make_film(n = 3L, labels = c("A", "B", "C"))
  result <- suppressMessages(scale(images = images, geometry = "50%"))
  expect_true(all(grepl("\\[scaled\\]", attr(result, "labels"))))
})

test_that("scale() errors on frames below 1", {
  images <- make_film(n = 3L)
  expect_error(scale(images = images, geometry = "50%", frames = 0L))
})

test_that("scale() messages the frame sequence", {
  images <- make_film(n = 2L)
  withr::with_options(list(stopmotion.verbose = TRUE), {
    expect_message(
      scale(images = images, geometry = "50%"),
      regexp = "Frame sequence after scale"
    )
  })
})

# ── background() additional coverage ─────────────────────────────────────────

test_that("background() labels all frames when frames = NULL", {
  images <- make_film(n = 3L, labels = c("A", "B", "C"))
  result <- suppressMessages(background(images = images, color = "white"))
  expect_true(all(grepl("\\[background\\]", attr(result, "labels"))))
})

test_that("background() default color is 'white'", {
  images <- make_film(n = 2L)
  expect_no_error(suppressMessages(background(images = images)))
})

test_that("background() errors on frames below 1", {
  images <- make_film(n = 3L)
  expect_error(background(images = images, color = "white", frames = 0L))
})

test_that("background() messages the frame sequence", {
  images <- make_film(n = 2L)
  withr::with_options(list(stopmotion.verbose = TRUE), {
    expect_message(
      background(images = images, color = "white"),
      regexp = "Frame sequence after background"
    )
  })
})

# ── blur() additional coverage ───────────────────────────────────────────────

test_that("blur() errors on frames below 1", {
  images <- make_film(n = 3L)
  expect_error(blur(images = images, frames = 0L))
})

test_that("blur() messages the frame sequence", {
  images <- make_film(n = 2L)
  withr::with_options(list(stopmotion.verbose = TRUE), {
    expect_message(blur(images = images), regexp = "Frame sequence after blur")
  })
})

# ── crop() additional coverage ───────────────────────────────────────────────

test_that("crop() with gravity does not change frame count", {
  images <- make_film(n = 3L)
  result <- suppressMessages(
    crop(images = images, geometry = "5x5", gravity = "Center")
  )
  expect_equal(length(result), 3L)
})

test_that("crop() with repage = FALSE does not change frame count", {
  images <- make_film(n = 3L)
  result <- suppressMessages(crop(
    images = images,
    geometry = "5x5",
    repage = FALSE
  ))
  expect_equal(length(result), 3L)
})

test_that("crop() errors on frames below 1", {
  images <- make_film(n = 3L)
  expect_error(crop(images = images, geometry = "5x5", frames = 0L))
})

test_that("crop() messages the frame sequence", {
  images <- make_film(n = 2L)
  withr::with_options(list(stopmotion.verbose = TRUE), {
    expect_message(
      crop(images = images, geometry = "5x5"),
      regexp = "Frame sequence after crop"
    )
  })
})

# ── trim() additional coverage ───────────────────────────────────────────────

test_that("trim() errors on frames below 1", {
  images <- make_trimmable_film(n = 3L)
  expect_error(trim(images = images, frames = 0L))
})

test_that("trim() messages the frame sequence", {
  images <- make_trimmable_film(n = 2L)
  withr::with_options(list(stopmotion.verbose = TRUE), {
    expect_message(trim(images = images), regexp = "Frame sequence after trim")
  })
})

# ── border() regression: virtual-canvas offset on rotated frames ──────────────

test_that("border() preserves canvas size when applied to a rotated frame", {
  # Regression: image_rotate() leaves a virtual-canvas offset that caused
  # image_crop() to return one pixel short on each axis, so the bordered frame
  # shrank by 1px. image_flatten() in border() fixes this.
  frame   <- magick::image_blank(10L, 10L, color = "white")
  rotated <- magick::image_rotate(frame, degrees = 5)
  rot_w   <- magick::image_info(rotated)$width
  rot_h   <- magick::image_info(rotated)$height
  images  <- c(frame, rotated)
  result  <- suppressMessages(
    border(images = images, color = "red", geometry = "2x2", frames = 2L)
  )
  info <- magick::image_info(result)
  expect_equal(info$width[2L],  rot_w)
  expect_equal(info$height[2L], rot_h)
})

# ── border() additional coverage ─────────────────────────────────────────────

test_that("border() errors on frames below 1", {
  images <- make_film(n = 3L)
  expect_error(border(images = images, frames = 0L))
})

test_that("border() messages the frame sequence", {
  images <- make_film(n = 2L)
  withr::with_options(list(stopmotion.verbose = TRUE), {
    expect_message(
      border(images = images, color = "black", geometry = "2x2"),
      regexp = "Frame sequence after border"
    )
  })
})

# ── montage() ────────────────────────────────────────────────────────────────

test_that("montage() returns a magick-image", {
  images <- make_film(n = 4L)
  result <- suppressWarnings(montage(images))
  expect_s3_class(result, "magick-image")
})

test_that("montage() with frames subset returns a magick-image", {
  images <- make_film(n = 6L)
  result <- suppressWarnings(montage(images, frames = 1:3))
  expect_s3_class(result, "magick-image")
})

test_that("montage() errors on frames below 1", {
  images <- make_film(n = 3L)
  expect_error(montage(images, frames = 0L))
})

test_that("montage() errors on frames exceeding film length", {
  images <- make_film(n = 3L)
  expect_error(montage(images, frames = 5L))
})

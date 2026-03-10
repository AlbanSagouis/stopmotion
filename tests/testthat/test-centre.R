make_points <- function(frames, x1, y1, x2, y2) {
  data.frame(
    frame = rep(as.integer(frames), each = 2L),
    x = as.numeric(rbind(x1, x2)),
    y = as.numeric(rbind(y1, y2))
  )
}

test_that("centre() does not change frame count", {
  images <- make_film(n = 3L)
  pts <- make_points(
    frames = 1:3,
    x1 = c(2, 2, 2),
    y1 = c(2, 2, 2),
    x2 = c(8, 8, 8),
    y2 = c(8, 8, 8)
  )
  result <- suppressMessages(centre(
    images = images,
    points = pts,
    reference = 1L
  ))
  expect_equal(length(result), 3L)
})

test_that("centre() labels transformed frames as [centred]", {
  images <- make_film(n = 3L, labels = c("A", "B", "C"))
  pts <- make_points(
    frames = 1:3,
    x1 = c(2, 3, 2),
    y1 = c(2, 3, 2),
    x2 = c(8, 7, 8),
    y2 = c(8, 7, 8)
  )
  result <- suppressMessages(centre(
    images = images,
    points = pts,
    reference = 1L
  ))
  labels <- attr(result, "labels")
  expect_equal(labels[[1L]], "A")
  expect_equal(labels[[2L]], "B [centred]")
  expect_equal(labels[[3L]], "C [centred]")
})

test_that("centre() does not label or transform the reference frame", {
  images <- make_film(n = 3L, labels = c("A", "B", "C"))
  pts <- make_points(
    frames = 1:3,
    x1 = c(2, 3, 2),
    y1 = c(2, 3, 2),
    x2 = c(8, 7, 8),
    y2 = c(8, 7, 8)
  )
  result <- suppressMessages(centre(
    images = images,
    points = pts,
    reference = 2L
  ))
  labels <- attr(result, "labels")
  expect_equal(labels[[1L]], "A [centred]")
  expect_equal(labels[[2L]], "B")
  expect_equal(labels[[3L]], "C [centred]")
})

test_that("centre() frames = NULL processes all non-reference frames", {
  images <- make_film(n = 4L, labels = c("A", "B", "C", "D"))
  pts <- make_points(
    frames = 1:4,
    x1 = c(2, 3, 2, 3),
    y1 = c(2, 3, 2, 3),
    x2 = c(8, 7, 8, 7),
    y2 = c(8, 7, 8, 7)
  )
  result <- suppressMessages(centre(
    images = images,
    points = pts,
    reference = 1L
  ))
  labels <- attr(result, "labels")
  expect_equal(labels[1L], "A")
  expect_true(all(grepl("\\[centred\\]", labels[2:4])))
})

test_that("centre() with identical points leaves frame unchanged (identity warp)", {
  images <- make_film(n = 2L, labels = c("A", "B"))
  pts <- make_points(
    frames = 1:2,
    x1 = c(2, 2),
    y1 = c(2, 2),
    x2 = c(8, 8),
    y2 = c(8, 8)
  )
  result <- suppressMessages(centre(
    images = images,
    points = pts,
    reference = 1L
  ))
  expect_equal(length(result), 2L)
  expect_equal(attr(result, "labels")[[2L]], "B [centred]")
})

test_that("centre() errors when reference frame has wrong number of points", {
  images <- make_film(n = 2L)
  pts <- data.frame(frame = c(1L, 2L, 2L), x = c(2, 3, 8), y = c(2, 3, 8))
  expect_error(centre(images = images, points = pts, reference = 1L))
})

test_that("centre() errors when a non-reference frame has wrong number of points", {
  images <- make_film(n = 2L)
  pts <- data.frame(frame = c(1L, 1L, 2L), x = c(2, 8, 3), y = c(2, 8, 3))
  expect_error(centre(images = images, points = pts, reference = 1L))
})

test_that("centre() errors when points is missing required columns", {
  images <- make_film(n = 2L)
  pts <- data.frame(frame = c(1L, 1L), px = c(2, 8), py = c(2, 8))
  expect_error(centre(images = images, points = pts, reference = 1L))
})

test_that("centre() with explicit frames subset only transforms selected frames", {
  images <- make_film(n = 4L, labels = c("A", "B", "C", "D"))
  # Points provided for frames 1, 2, 4; frame 3 is intentionally excluded.
  pts <- data.frame(
    frame = c(1L, 1L, 2L, 2L, 4L, 4L),
    x = as.numeric(c(2, 2, 3, 3, 2, 2)),
    y = as.numeric(c(2, 8, 3, 7, 2, 8))
  )
  result <- suppressMessages(
    centre(
      images = images,
      points = pts,
      reference = 1L,
      frames = c(1L, 2L, 4L)
    )
  )
  labels <- attr(result, "labels")
  expect_equal(labels[[1L]], "A") # reference: unchanged
  expect_equal(labels[[2L]], "B [centred]") # transformed
  expect_equal(labels[[3L]], "C") # not in frames: untouched
  expect_equal(labels[[4L]], "D [centred]") # transformed
})

test_that("centre() messages the frame sequence", {
  images <- make_film(n = 2L)
  pts <- make_points(
    frames = 1:2,
    x1 = c(2, 3),
    y1 = c(2, 3),
    x2 = c(8, 7),
    y2 = c(8, 7)
  )
  withr::with_options(list(stopmotion.verbose = TRUE), {
    expect_message(
      centre(images = images, points = pts, reference = 1L),
      regexp = "Frame sequence after centre"
    )
  })
})

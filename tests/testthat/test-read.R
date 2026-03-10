test_that("read() returns a magick-image of the correct length", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  magick::image_write(magick::image_blank(width = 10L, height = 10L), file.path(tmp, "frame_1.png"))
  magick::image_write(magick::image_blank(width = 10L, height = 10L), file.path(tmp, "frame_2.png"))

  images <- read(dir = tmp)

  expect_s3_class(images, "magick-image")
  expect_equal(length(images), 2L)
})

test_that("read() initialises labels from filenames without extension", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  magick::image_write(magick::image_blank(width = 10L, height = 10L), file.path(tmp, "photo_001.png"))
  magick::image_write(magick::image_blank(width = 10L, height = 10L), file.path(tmp, "photo_002.png"))

  images <- read(dir = tmp)

  expect_equal(attr(images, "labels"), c("photo_001", "photo_002"))
})

test_that("read() pattern filters files", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  magick::image_write(magick::image_blank(width = 10L, height = 10L), file.path(tmp, "frame_1.png"))
  magick::image_write(magick::image_blank(width = 10L, height = 10L), file.path(tmp, "frame_2.png"))
  magick::image_write(magick::image_blank(width = 10L, height = 10L), file.path(tmp, "other.png"))

  images <- read(dir = tmp, pattern = "^frame")

  expect_equal(length(images), 2L)
})

test_that("read() errors on a non-existent directory", {
  expect_error(read(dir = tempfile()))
})

test_that("read() returns zero-length magick-image for empty directory", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  images <- read(dir = tmp)

  expect_s3_class(images, "magick-image")
  expect_equal(length(images), 0L)
})

test_that("read() label order matches list.files() sort order", {
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  # Names chosen to sort unambiguously the same way on all filesystems.
  magick::image_write(magick::image_blank(10L, 10L), file.path(tmp, "aaa.png"))
  magick::image_write(magick::image_blank(10L, 10L), file.path(tmp, "bbb.png"))
  magick::image_write(magick::image_blank(10L, 10L), file.path(tmp, "ccc.png"))

  images <- read(dir = tmp)
  expected <- tools::file_path_sans_ext(sort(c("aaa.png", "bbb.png", "ccc.png")))
  expect_equal(attr(images, "labels"), expected)
})

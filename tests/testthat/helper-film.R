# Shared test helper: build a small in-memory magick-image film.
# Each frame is a plain 10x10 white pixel image, fast to create and process.
make_film <- function(n, labels = NULL) {
  images <- Reduce(
    f = c,
    x = lapply(seq_len(n), function(i) magick::image_blank(width = 10L, height = 10L))
  )
  if (!is.null(labels)) attr(images, "labels") <- labels
  images
}

# Shared test helper: build a film suitable for image_trim tests.
# Each frame is a 20x20 white image with a small red rectangle at the centre,
# giving image_trim a non-uniform bounding box to detect.
make_trimmable_film <- function(n, labels = NULL) {
  frame <- magick::image_composite(
    image = magick::image_blank(width = 20L, height = 20L, color = "white"),
    composite_image = magick::image_blank(width = 4L, height = 4L, color = "red"),
    offset = "+4+4"
  )
  images <- Reduce(f = c, x = replicate(n = n, expr = frame, simplify = FALSE))
  if (!is.null(labels)) attr(images, "labels") <- labels
  images
}

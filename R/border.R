#' Add a border to images
#'
#' Adds a coloured border around selected frames. Wraps
#' \code{magick::image_border}.
#'
#' @param color a character string specifying the border colour,
#'   e.g. \code{"black"} or \code{"#FF0000"}.
#' @param geometry a geometry string specifying border width and height,
#'   e.g. \code{"10x10"} for a 10-pixel border on all sides.
#' @inheritSection duplicate Verbosity
#' @inheritParams duplicate
#' @importFrom magick image_border image_crop image_info
#' @returns a \code{magick-image} object
#' @examples \donttest{
#'   dino_dir <- system.file("extdata", package = "stopmotion")
#'   images <- read(dir = dino_dir)
#'   border(images = images, color = "black", geometry = "5x5")
#'   border(images = images, color = "white", geometry = "10x10", frames = 1:3)
#' }
#' @export

border <- function(
  images,
  color = "lightgray",
  geometry = "10x10",
  frames = NULL
) {
  checkmate::assert_class(images, "magick-image")
  checkmate::assert_string(color)
  checkmate::assert_string(geometry)
  if (is.null(frames)) {
    frames <- seq_along(images)
  }
  checkmate::assert_integerish(frames, lower = 1L, upper = length(images))

  labels <- get_labels(images)

  # Parse border dimensions from geometry (e.g. "8x8" → bw = 8, bh = 8)
  m <- regmatches(geometry, regexpr("^[0-9]+x[0-9]+", geometry))
  if (length(m) > 0L) {
    parts <- as.integer(strsplit(m, "x")[[1L]])
    bw <- parts[1L]
    bh <- parts[2L]
  } else {
    bw <- bh <- 0L
  }

  for (i in frames) {
    img_i <- images[i]
    # Crop by the border width first so the canvas size is preserved after
    # image_border() expands it back out.
    if (bw > 0L || bh > 0L) {
      # Materialise the full canvas before cropping: image_rotate() and similar
      # operations can leave a virtual-canvas offset that causes image_crop() to
      # return one pixel short on each axis, shrinking the frame.
      img_i <- magick::image_flatten(img_i)
      info_i <- image_info(img_i)
      crop_geom <- sprintf(
        "%dx%d+%d+%d",
        info_i$width[1L]  - 2L * bw,
        info_i$height[1L] - 2L * bh,
        bw, bh
      )
      img_i <- image_crop(img_i, geometry = crop_geom, repage = TRUE)
    }
    after_seq <- if (i < length(images)) {
      seq(i + 1L, length(images))
    } else {
      integer(0)
    }
    images <- c(
      images[seq_len(i - 1L)],
      image_border(img_i, color = color, geometry = geometry),
      images[after_seq]
    )
    labels[i] <- paste0(labels[i], " [bordered]")
  }

  images <- set_labels(images, labels = labels)
  print_frames(images, fn_name = "border")
  images
}

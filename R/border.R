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
#' @importFrom magick image_border
#' @returns a \code{magick-image} object
#' @examples \dontrun{
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

  for (i in frames) {
    after_seq <- if (i < length(images)) {
      seq(i + 1L, length(images))
    } else {
      integer(0)
    }
    images <- c(
      images[seq_len(i - 1L)],
      image_border(images[i], color = color, geometry = geometry),
      images[after_seq]
    )
    labels[i] <- paste0(labels[i], " [bordered]")
  }

  images <- set_labels(images, labels = labels)
  print_frames(images, fn_name = "border")
  images
}

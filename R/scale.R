#' Scale images
#'
#' @param geometry a character string specifying the target geometry,
#'   e.g. \code{"50\%"} or \code{"800x600"}.
#' @inheritSection duplicate Verbosity
#' @inheritParams duplicate
#' @importFrom magick image_scale
#' @returns a \code{magick-image} object
#' @examples \donttest{
#'   dino_dir <- system.file("extdata", package = "stopmotion")
#'   images <- read(dir = dino_dir)
#'   scale(images = images, geometry = "50%")
#'   scale(images = images, geometry = "50%", frames = 2:3)
#' }
#' @export

scale <- function(images, geometry, frames = NULL) {
  checkmate::assert_class(images, "magick-image")
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
      image_scale(images[i], geometry = geometry),
      images[after_seq]
    )
    labels[i] <- paste0(labels[i], " [scaled]")
  }

  images <- set_labels(images, labels = labels)
  print_frames(images, fn_name = "scale")
  images
}

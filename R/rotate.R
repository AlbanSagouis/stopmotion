#' Rotate images
#'
#' Replaces selected frames with a rotated version in place. For large-angle
#' transformations. For a small-angle hand-held rock effect, see
#' \code{\link{wiggle}}.
#'
#' @param degrees a number in \code{[-360, 360]} specifying the rotation angle.
#' @inheritSection duplicate Verbosity
#' @inheritParams duplicate
#' @importFrom magick image_rotate
#' @returns a \code{magick-image} object
#' @examples \donttest{
#'   dino_dir <- system.file("extdata", package = "stopmotion")
#'   images <- read(dir = dino_dir)
#'   rotate(images = images, degrees = 90, frames = 2L)
#' }
#' @export

rotate <- function(images, degrees, frames = NULL) {
  checkmate::assert_class(images, "magick-image")
  if (is.null(frames)) {
    frames <- seq_along(images)
  }
  checkmate::assert_integerish(frames, lower = 1L, upper = length(images))
  checkmate::assert_numeric(degrees, lower = -360, upper = 360, len = 1L)

  labels <- get_labels(images)

  for (i in frames) {
    after_seq <- if (i < length(images)) {
      seq(i + 1L, length(images))
    } else {
      integer(0)
    }
    images <- c(
      images[seq_len(i - 1L)],
      image_rotate(image = images[i], degrees = degrees),
      images[after_seq]
    )
    labels[i] <- paste0(labels[i], " [rotated]")
  }

  images <- set_labels(images, labels = labels)
  print_frames(images, fn_name = "rotate")
  images
}

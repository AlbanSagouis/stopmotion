#' Blur images
#'
#' Applies a Gaussian blur to selected frames. Wraps \code{magick::image_blur}.
#'
#' @param radius a non-negative number specifying the blur radius in pixels.
#' @param sigma a non-negative number specifying the standard deviation of the
#'   Gaussian, controlling blur strength. Use \code{0} for no blur.
#' @inheritSection duplicate Verbosity
#' @inheritParams duplicate
#' @importFrom magick image_blur
#' @returns a \code{magick-image} object
#' @examples \donttest{
#'   dino_dir <- system.file("extdata", package = "stopmotion")
#'   images <- read(dir = dino_dir)
#'   blur(images = images, radius = 2, sigma = 1)
#'   blur(images = images, radius = 2, sigma = 1, frames = 1:3)
#' }
#' @export

blur <- function(images, radius = 1, sigma = 0.5, frames = NULL) {
  checkmate::assert_class(images, "magick-image")
  checkmate::assert_numeric(radius, lower = 0, len = 1L)
  checkmate::assert_numeric(sigma, lower = 0, len = 1L)
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
      image_blur(images[i], radius = radius, sigma = sigma),
      images[after_seq]
    )
    labels[i] <- paste0(labels[i], " [blurred]")
  }

  images <- set_labels(images, labels = labels)
  print_frames(images, fn_name = "blur")
  images
}

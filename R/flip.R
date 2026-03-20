#' Flip images vertically
#'
#' @inheritSection duplicate Verbosity
#' @inheritParams duplicate
#' @importFrom magick image_flip
#' @returns a \code{magick-image} object
#' @examples \donttest{
#'   dino_dir <- system.file("extdata", package = "stopmotion")
#'   images <- read(dir = dino_dir)
#'   flip(images = images, frames = 2:3)
#' }
#' @export

flip <- function(images, frames = NULL) {
  checkmate::assert_class(images, "magick-image")
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
      image_flip(images[i]),
      images[after_seq]
    )
    labels[i] <- paste0(labels[i], " [flipped]")
  }

  images <- set_labels(images, labels = labels)
  print_frames(images, fn_name = "flip")
  images
}

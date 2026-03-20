#' Set the background colour of images
#'
#' @param color a character string specifying a colour,
#'   e.g. \code{"white"} or \code{"#FF0000"}.
#' @inheritSection duplicate Verbosity
#' @inheritParams duplicate
#' @importFrom magick image_background
#' @returns a \code{magick-image} object
#' @examples \donttest{
#'   dino_dir <- system.file("extdata", package = "stopmotion")
#'   images <- read(dir = dino_dir)
#'   background(images = images, color = "white")
#'   background(images = images, color = "white", frames = 1)
#' }
#' @export

background <- function(images, color = "white", frames = NULL) {
  checkmate::assert_class(images, "magick-image")
  checkmate::assert_string(color)
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
      image_background(images[i], color = color),
      images[after_seq]
    )
    labels[i] <- paste0(labels[i], " [background]")
  }

  images <- set_labels(images, labels = labels)
  print_frames(images, fn_name = "background")
  images
}

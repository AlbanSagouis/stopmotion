#' Flop images horizontally
#'
#' Mirrors selected frames along the vertical axis (left-right reflection).
#' For a vertical flip (top-bottom), see \code{\link{flip}}.
#' Wraps \code{magick::image_flop}.
#'
#' @inheritSection duplicate Verbosity
#' @inheritParams duplicate
#' @importFrom magick image_flop
#' @returns a \code{magick-image} object
#' @examples \dontrun{
#'   dino_dir <- system.file("extdata", package = "stopmotion")
#'   images <- read(dir = dino_dir)
#'   flop(images = images)
#'   flop(images = images, frames = 2:3)
#' }
#' @export

flop <- function(images, frames = NULL) {
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
      image_flop(images[i]),
      images[after_seq]
    )
    labels[i] <- paste0(labels[i], " [flopped]")
  }

  images <- set_labels(images, labels = labels)
  print_frames(images, fn_name = "flop")
  images
}

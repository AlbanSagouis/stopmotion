#' Preview an animation
#'
#' Converts a stack of frames into an animated GIF for display, with each
#' frame's index and label overlaid as text.  In an interactive session the
#' animation opens in the system viewer; in a knitr/Quarto document it is
#' embedded as an inline animated GIF.  Wraps \code{magick::image_animate}.
#'
#' @param fps playback speed in frames per second.  Must be a positive integer
#'   divisor of 100, because GIF delay is stored in hundredths of a second
#'   (\code{delay = 100 / fps}).  Valid values: 1, 2, 4, 5, 10, 20, 25, 50,
#'   100.  Defaults to \code{10}.
#' @param loop a non-negative integer giving the number of times the animation
#'   loops.  \code{0} (the default) means loop forever.
#' @param label logical. Whether to overlay the frame index and label on each
#'   frame.  Defaults to \code{TRUE}.
#' @inheritParams duplicate
#' @importFrom magick image_animate image_info image_annotate
#' @returns a \code{magick-image} object containing the animated sequence.
#' @examples \donttest{
#'   dino_dir <- system.file("extdata", package = "stopmotion")
#'   images <- read(dir = dino_dir)
#'   preview(images)
#'   preview(images, fps = 5)
#'   preview(images, label = FALSE)
#' }
#' @export

preview <- function(images, fps = 10, loop = 0, frames = NULL, label = TRUE) {
  checkmate::assert_class(images, "magick-image")
  checkmate::assert_integerish(fps, lower = 1L, len = 1L)
  if (100L %% fps != 0L) {
    stop(
      "'fps' must be a divisor of 100. Valid values: 1, 2, 4, 5, 10, 20, 25, 50, 100."
    )
  }
  checkmate::assert_integerish(loop, lower = 0L, len = 1L)
  checkmate::assert_flag(label)
  if (is.null(frames)) {
    frames <- seq_along(images)
  }
  checkmate::assert_integerish(frames, lower = 1L, upper = length(images))

  selected <- images[frames]

  if (label) {
    labels <- get_labels(images)[frames]
    n_total <- length(images)
    h <- image_info(selected[1L])$height
    font_size <- max(12L, as.integer(h / 20L))
    px_offset <- max(4L, as.integer(h / 60L))
    location <- sprintf("+%d+%d", px_offset, px_offset)

    for (i in seq_along(frames)) {
      text <- sprintf("%d / %d  %s", frames[i], n_total, labels[i])
      selected[i] <- image_annotate(
        selected[i],
        text = text,
        size = font_size,
        gravity = "SouthWest",
        location = location,
        color = "white",
        boxcolor = "black",
        font = "monospace"
      )
    }
  }

  image_animate(selected, fps = fps, loop = loop)
}

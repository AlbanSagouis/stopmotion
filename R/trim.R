#' Trim edges from images
#'
#' Removes border pixels from selected frames by detecting the background
#' colour and trimming uniform edges. Wraps \code{magick::image_trim}.
#'
#' @param fuzz a number in \code{[0, 100]} controlling colour tolerance when
#'   detecting the background. Higher values trim more aggressively.
#' @inheritSection duplicate Verbosity
#' @inheritParams duplicate
#' @importFrom magick image_trim
#' @returns a \code{magick-image} object
#' @examples \dontrun{
#'   dino_dir <- system.file("extdata", package = "stopmotion")
#'   images <- read(dir = dino_dir)
#'   trim(images = images)
#'   trim(images = images, fuzz = 10, frames = 1:3)
#' }
#' @export

trim <- function(images, fuzz = 0, frames = NULL) {
  checkmate::assert_class(images, "magick-image")
  checkmate::assert_numeric(fuzz, lower = 0, upper = 100, len = 1L)
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
      image_trim(images[i], fuzz = fuzz),
      images[after_seq]
    )
    labels[i] <- paste0(labels[i], " [trimmed]")
  }

  images <- set_labels(images, labels = labels)
  print_frames(images, fn_name = "trim")
  images
}

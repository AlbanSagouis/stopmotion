#' Add a wiggle effect to frames
#'
#' Inserts two rotated copies after each selected frame — one tilted
#' \code{+degrees} and one tilted \code{-degrees} — creating a hand-held
#' stop-motion rock effect. For large-angle permanent rotations, see
#' \code{\link{rotate}}.
#'
#' @param degrees a positive number specifying the tilt angle in degrees.
#'   Both \code{+degrees} and \code{-degrees} are applied automatically.
#' @inheritSection duplicate Verbosity
#' @inheritParams duplicate
#' @importFrom magick image_rotate
#' @returns a \code{magick-image} object with 2 extra frames per selected frame.
#' @examples \dontrun{
#'   dino_dir <- system.file("extdata", package = "stopmotion")
#'   images <- read(dir = dino_dir)
#'   wiggle(images = images, degrees = 3, frames = 1:2)
#' }
#' @export

wiggle <- function(images, degrees = 3, frames = NULL) {
  checkmate::assert_class(images, "magick-image")
  checkmate::assert_numeric(degrees, lower = 0, upper = 360, len = 1L)
  if (is.null(frames)) frames <- seq_along(images)
  checkmate::assert_integerish(frames, lower = 1L, upper = length(images))

  labels <- get_labels(images)

  offset <- 0L
  for (i in frames) {
    j <- i + offset
    after_seq <- if (j < length(images)) seq(j + 1L, length(images)) else integer(0)
    images <- c(
      images[seq_len(j - 1L)],
      images[j],
      image_rotate(image = images[j], degrees = degrees),
      image_rotate(image = images[j], degrees = -degrees),
      images[after_seq]
    )
    labels <- c(
      labels[seq_len(j - 1L)],
      labels[j],
      paste0(labels[j], sprintf(" [wiggled +%g\u00b0]", degrees)),
      paste0(labels[j], sprintf(" [wiggled -%g\u00b0]", degrees)),
      labels[after_seq]
    )
    offset <- offset + 2L
  }

  images <- set_labels(images, labels = labels)
  print_frames(images, fn_name = "wiggle")
  images
}

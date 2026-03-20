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
#' @importFrom magick image_rotate image_info image_background image_crop
#' @returns a \code{magick-image} object with 2 extra frames per selected frame.
#' @examples \donttest{
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
    info_j    <- image_info(images[j])
    orig_geom <- sprintf("%dx%d", info_j$width[1L], info_j$height[1L])
    # Set the fill for rotation to the frame's own background colour (sampled
    # from the top-left corner) so the rotation corners blend in seamlessly.
    bg_raw  <- as.integer(magick::image_data(image_crop(images[j], "1x1+0+0"), channels = "rgba"))
    bg_hex  <- sprintf("#%02x%02x%02x", bg_raw[1L], bg_raw[2L], bg_raw[3L])
    img_j   <- image_background(images[j], bg_hex)
    after_seq <- if (j < length(images)) seq(j + 1L, length(images)) else integer(0)
    images <- c(
      images[seq_len(j - 1L)],
      images[j],
      image_crop(image_rotate(image = img_j, degrees =  degrees), orig_geom, repage = TRUE),
      image_crop(image_rotate(image = img_j, degrees = -degrees), orig_geom, repage = TRUE),
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

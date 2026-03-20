#' Crop images
#'
#' Crops selected frames to a given geometry. Wraps \code{magick::image_crop}.
#'
#' @param geometry a geometry string specifying the cropped region,
#'   e.g. \code{"100x100+10+10"} (widthxheight+x_offset+y_offset).
#' @param gravity anchor point for the crop: one of \code{"NorthWest"},
#'   \code{"North"}, \code{"NorthEast"}, \code{"West"}, \code{"Center"},
#'   \code{"East"}, \code{"SouthWest"}, \code{"South"}, \code{"SouthEast"}.
#'   Defaults to \code{NULL} (top-left).
#' @param repage logical. If \code{TRUE} (default), resets the virtual canvas
#'   after cropping.
#' @inheritSection duplicate Verbosity
#' @inheritParams duplicate
#' @importFrom magick image_crop
#' @returns a \code{magick-image} object
#' @examples \donttest{
#'   dino_dir <- system.file("extdata", package = "stopmotion")
#'   images <- read(dir = dino_dir)
#'   crop(images = images, geometry = "200x200+50+50")
#'   crop(images = images, geometry = "200x200", gravity = "Center", frames = 1:3)
#' }
#' @export

crop <- function(
  images,
  geometry,
  gravity = NULL,
  repage = TRUE,
  frames = NULL
) {
  checkmate::assert_class(images, "magick-image")
  checkmate::assert_string(geometry)
  checkmate::assert_logical(repage, len = 1L)
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
      image_crop(
        images[i],
        geometry = geometry,
        gravity = gravity,
        repage = repage
      ),
      images[after_seq]
    )
    labels[i] <- paste0(labels[i], " [cropped]")
  }

  images <- set_labels(images, labels = labels)
  print_frames(images, fn_name = "crop")
  images
}

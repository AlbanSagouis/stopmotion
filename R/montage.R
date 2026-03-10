#' Display frames as a montage
#'
#' Arranges selected frames into a single composite image.
#' Wraps \code{magick::image_montage}.
#'
#' @param geometry a geometry string controlling the size and spacing of each
#'   tile, e.g. \code{"64x64+2+2"}.
#' @param tile a string specifying the grid layout, e.g. \code{"5x2"}.
#'   Defaults to \code{NULL}, which lets ImageMagick choose.
#' @param gravity anchor point for each tile's label and content: one of
#'   \code{"Center"}, \code{"North"}, \code{"South"}, etc.
#'   Defaults to \code{"Center"}.
#' @param bg background colour string, e.g. \code{"white"}.
#' @param shadow logical. Whether to add a drop-shadow under each tile.
#' @inheritParams duplicate
#' @importFrom magick image_montage image_convert
#' @returns a \code{magick-image} object containing a single composite frame.
#' @examples \dontrun{
#'   dino_dir <- system.file("extdata", package = "stopmotion")
#'   images <- read(dir = dino_dir)
#'   montage(images)
#'   montage(images, frames = 1:4, tile = "4x1", geometry = "128x128+4+4")
#' }
#' @export

montage <- function(
  images,
  geometry = NULL,
  tile     = NULL,
  gravity  = "Center",
  bg       = "white",
  shadow   = FALSE,
  frames   = NULL
) {
  checkmate::assert_class(images, "magick-image")
  if (is.null(frames)) frames <- seq_along(images)
  checkmate::assert_integerish(frames, lower = 1L, upper = length(images))

  image_convert(
    image_montage(
      images[frames],
      geometry = geometry,
      tile     = tile,
      gravity  = gravity,
      bg       = bg,
      shadow   = shadow
    ),
    depth = 8L
  )
}

#' Read images into a stop-motion film
#'
#' Reads all image files from \code{dir} (optionally filtered by
#' \code{pattern}) and returns them as a \code{magick-image} object.
#'
#' @section Frame order:
#'   Frames are loaded in the order returned by \code{\link[base]{list.files}},
#'   which sorts filenames lexicographically.  This means the filesystem
#'   filename order determines the stop-motion frame order.  Name your files
#'   accordingly (e.g. \code{frame_001.png}, \code{frame_002.png}, \ldots) to
#'   guarantee the intended sequence.  If you need to reorder frames after
#'   loading, use \code{\link{arrange}}.
#'
#' @param dir path to directory containing the images relative to working
#'    directory.
#' @inheritParams base::list.files
#' @returns an object of class \code{magick-image}
#'
#' @importFrom magick image_read
#'
#' @export
#'
#' @examples  \dontrun{
#'    dino_dir <- system.file("extdata", package = "stopmotion")
#'    images <- read(dir = dino_dir)
#' }

read <- function(dir, pattern = "") {
  checkmate::assert_directory(dir, access = "r")
  checkmate::assert_string(pattern, null.ok = FALSE)

  files <- list.files(path = dir, pattern = pattern, full.names = TRUE)
  images <- image_read(files)
  set_labels(images, labels = tools::file_path_sans_ext(basename(files)))
}

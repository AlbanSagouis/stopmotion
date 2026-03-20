#' Splice frames into a film
#' @param insert an object of class \code{magick-image} containing 1 or more
#'   images to be inserted.
#' @param after integer scalar (or vector of scalars) giving the frame
#'   number(s) after which \code{insert} will be inserted.  When a vector is
#'   supplied the insertions are applied left-to-right, each offset by the
#'   cumulative growth of the film from prior insertions.
#'
#' @inheritSection duplicate Verbosity
#' @inheritParams duplicate
#' @returns a \code{magick-image} object
#' @examples \donttest{
#'   dino_dir <- system.file("extdata", package = "stopmotion")
#'   images <- read(dir = dino_dir)
#'   splice(images = images, insert = magick::wizard, after = 1)
#' }
#' @export

splice <- function(
  images,
  insert,
  after
) {
  checkmate::assert_class(images, "magick-image")
  checkmate::assert_class(insert, "magick-image")
  checkmate::assert_integerish(after, lower = 1L, upper = length(images))

  labels <- get_labels(images)
  insert_labels <- paste0(get_labels(insert), " [spliced]")

  offset <- 0L
  for (i in after) {
    j <- i + offset
    after_seq <- if (j < length(images)) {
      seq(j + 1L, length(images))
    } else {
      integer(0)
    }
    images <- c(
      images[seq_len(j)],
      insert,
      images[after_seq]
    )
    labels <- c(
      labels[seq_len(j)],
      insert_labels,
      labels[after_seq]
    )
    offset <- offset + length(insert)
  }

  images <- set_labels(images, labels = labels)
  print_frames(images, fn_name = "splice")
  images
}

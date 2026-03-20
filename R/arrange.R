#' Reorder frames in a stop-motion film
#'
#' Returns a new \code{magick-image} object with frames placed in the order
#' given by \code{order}.  Use this after \code{\link{read}} when the
#' lexicographic filename sort does not match the intended frame sequence.
#'
#' @param images an object of class \code{magick-image} to reorder.
#' @param order integer vector of frame indices giving the desired order.
#'   Must be a permutation of \code{1:length(images)} (every frame index
#'   appearing exactly once).
#' @returns a \code{magick-image} object with frames in the requested order.
#' @inheritSection duplicate Verbosity
#' @examples \donttest{
#'   dino_dir <- system.file("extdata", package = "stopmotion")
#'   images <- read(dir = dino_dir)
#'   # Swap the first and second frame.
#'   images <- arrange(images, order = c(2L, 1L, seq(3L, 10L)))
#' }
#' @export

arrange <- function(images, order) {
  checkmate::assert_class(images, "magick-image")
  n <- length(images)
  checkmate::assert_integerish(order, lower = 1L, upper = n, len = n,
                                any.missing = FALSE)
  if (!identical(sort(as.integer(order)), seq_len(n))) {
    stop(sprintf("'order' must be a permutation of 1:%d (each frame index used exactly once).", n),
         call. = FALSE)
  }

  labels <- get_labels(images)
  images <- images[order]
  images <- set_labels(images, labels = labels[order])
  print_frames(images, fn_name = "arrange")
  images
}

#' Align frames to a common set of reference points
#'
#' Transforms selected frames so that two user-supplied reference points (e.g.
#' left and right eye positions) map onto the same pixel locations across all
#' frames. The transformation is a full affine warp — rotation, scaling, and
#' translation are applied simultaneously — computed from the two point
#' correspondences via \code{magick::image_distort}.
#'
#' @param points a \code{data.frame} with columns \code{frame} (integer frame
#'   index), \code{x} (numeric, pixels from the left edge), and \code{y}
#'   (numeric, pixels from the \emph{bottom} edge, as returned by
#'   \code{locator()} after \code{plot(as.raster(images[[i]]))}). Exactly two
#'   rows per frame are
#'   required. Within each frame, the first row is reference point 1 and the
#'   second is reference point 2; the pairing must be consistent across frames
#'   (e.g. always left-eye first, right-eye second).
#' @param reference integer. The frame whose reference points define the target
#'   alignment. All other selected frames are warped to match it. The reference
#'   frame itself is left unchanged. Defaults to \code{1L}.
#' @inheritSection duplicate Verbosity
#' @inheritParams duplicate
#' @importFrom magick image_distort
#' @returns a \code{magick-image} object of the same length as \code{images}.
#' @examples \dontrun{
#'   dino_dir <- system.file("extdata", package = "stopmotion")
#'   images <- read(dir = dino_dir)
#'
#'   # Manually record eye positions for each frame (e.g. using locator())
#'   points <- data.frame(
#'     frame = c(1L, 1L, 2L, 2L, 3L, 3L),
#'     x     = c(210, 390, 215, 388, 208, 392),
#'     y     = c(180, 182, 176, 179, 183, 181)
#'   )
#'
#'   centre(images = images, points = points, reference = 1L)
#' }
#' @export

centre <- function(images, points, reference = 1L, frames = NULL) {
  checkmate::assert_class(images, "magick-image")
  checkmate::assert_data_frame(points, min.rows = 2L)
  checkmate::assert_names(names(points), must.include = c("frame", "x", "y"))
  checkmate::assert_integerish(points$frame)
  checkmate::assert_numeric(points$x)
  checkmate::assert_numeric(points$y)
  checkmate::assert_integerish(reference, len = 1L)

  if (is.null(frames)) {
    frames <- seq_along(images)
  }
  checkmate::assert_integerish(frames, lower = 1L, upper = length(images))

  img_height <- magick::image_info(images[1L])$height

  ref_pts <- points[points$frame == reference, c("x", "y")]
  if (nrow(ref_pts) != 2L) {
    stop(sprintf(
      "The reference frame (%d) must have exactly 2 reference points; %d found.",
      reference,
      nrow(ref_pts)
    ))
  }

  labels <- get_labels(images)

  for (i in frames) {
    if (i == reference) {
      next
    }

    src_pts <- points[points$frame == i, c("x", "y")]
    if (nrow(src_pts) != 2L) {
      stop(sprintf(
        "Frame %d must have exactly 2 reference points; %d found.",
        i,
        nrow(src_pts)
      ))
    }

    control_pts <- c(
      src_pts$x[[1L]],
      img_height - src_pts$y[[1L]],
      ref_pts$x[[1L]],
      img_height - ref_pts$y[[1L]],
      src_pts$x[[2L]],
      img_height - src_pts$y[[2L]],
      ref_pts$x[[2L]],
      img_height - ref_pts$y[[2L]]
    )

    after_seq <- if (i < length(images)) {
      seq(i + 1L, length(images))
    } else {
      integer(0)
    }
    images <- c(
      images[seq_len(i - 1L)],
      image_distort(
        image = images[i],
        distortion = "Affine",
        coordinates = control_pts,
        bestfit = FALSE
      ),
      images[after_seq]
    )
    labels[i] <- paste0(labels[i], " [centred]")
  }

  images <- set_labels(images, labels = labels)
  print_frames(images, fn_name = "centre")
  images
}

#' @rdname centre
#' @export
center <- centre

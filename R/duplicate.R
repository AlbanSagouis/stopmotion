#' Duplicate frames
#' @param images an object of class \code{magick-image} to modify
#' @param style one of \code{"linear"}, \code{"looped"}, or \code{"shuffle"},
#'   controlling how duplicates are inserted:
#'   \describe{
#'     \item{\code{"linear"}}{inserts one copy immediately before each selected
#'       frame, in order. The original frame follows its duplicate.}
#'     \item{\code{"looped"}}{appends one copy of each selected frame (in order)
#'       after \code{max(frames)}, creating a loop-back effect. Requires
#'       \code{frames} to be a consecutive sequence.}
#'     \item{\code{"shuffle"}}{randomly reorders both the originals and their
#'       copies within the selected range, replacing those positions. Requires
#'       \code{frames} to be a consecutive sequence.}
#'   }
#' @param frames integer vector of frame indices to duplicate. Defaults to
#'   \code{NULL}, which duplicates all frames.
#' @returns a \code{magick-image} object with duplicate frames inserted.
#' @section Verbosity:
#'   After each operation a message listing the updated frame sequence is
#'   printed in interactive sessions.  Use
#'   \code{stopmotion_verbosity(FALSE)} to suppress these messages, or set
#'   \code{options(stopmotion.verbose = FALSE)} in your script or
#'   \file{.Rprofile}.
#' @examples \donttest{
#'   dino_dir <- system.file("extdata", package = "stopmotion")
#'   images <- read(dir = dino_dir)
#'   duplicate(images = images, style = "shuffle", frames = 1:2)
#' }
#' @export

duplicate <- function(
  images,
  style = c("linear", "looped", "shuffle"),
  frames = NULL
) {
  checkmate::assert_class(images, "magick-image")
  if (is.null(frames)) {
    frames <- seq_along(images)
  }
  checkmate::assert_integerish(frames, lower = 1L, upper = length(images))
  style <- match.arg(style)

  if (is.element(style, c("shuffle", "looped"))) {
    if (!all(diff(frames) == 1L)) {
      stop(
        'frames must be consecutive integers when style = "looped" or "shuffle".'
      )
    }
  }

  labels <- get_labels(images)

  switch(
    style,
    linear = {
      offset <- 0L
      for (i in frames) {
        j <- i + offset
        images <- c(
          images[seq_len(j - 1L)],
          images[j],
          images[seq(j, length(images))]
        )
        labels <- c(
          labels[seq_len(j - 1L)],
          paste0(labels[j], " [duplicated]"),
          labels[seq(j, length(labels))]
        )
        offset <- offset + 1L
      }
    },
    looped = {
      tail_seq <- if (max(frames) < length(images)) {
        seq(max(frames) + 1L, length(images))
      } else {
        integer(0)
      }
      images <- c(
        images[seq_len(max(frames))],
        images[frames],
        images[tail_seq]
      )
      labels <- c(
        labels[seq_len(max(frames))],
        paste0(labels[frames], " [duplicated]"),
        labels[tail_seq]
      )
    },
    shuffle = {
      shuffled_idx <- sample(c(frames, frames), replace = FALSE)
      tail_seq <- if (max(frames) < length(images)) {
        seq(max(frames) + 1L, length(images))
      } else {
        integer(0)
      }
      images <- c(
        images[seq_len(min(frames) - 1L)],
        images[shuffled_idx],
        images[tail_seq]
      )
      labels <- c(
        labels[seq_len(min(frames) - 1L)],
        paste0(labels[shuffled_idx], " [shuffled]"),
        labels[tail_seq]
      )
    }
  )

  images <- set_labels(images, labels = labels)
  print_frames(images, fn_name = "duplicate")
  images
}

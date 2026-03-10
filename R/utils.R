# Internal: retrieve frame labels, initialising from indices if absent.
get_labels <- function(images) {
  labels <- attr(images, "labels")
  if (is.null(labels)) as.character(seq_along(images)) else labels
}

# Internal: attach a labels vector to a magick-image object.
set_labels <- function(images, labels) {
  attr(images, "labels") <- labels
  images
}

#' Control stopmotion verbosity
#'
#' Convenience wrapper around \code{options(stopmotion.verbose = )} for
#' enabling or disabling the frame-sequence messages printed after each
#' operation.  By default messages are shown in interactive sessions and
#' suppressed in non-interactive contexts (e.g. knitr/Quarto rendering).
#'
#' @param verbose \code{TRUE} to enable messages, \code{FALSE} to suppress
#'   them.
#' @returns the previous value of the option, invisibly.
#' @examples
#' old <- stopmotion_verbosity(FALSE)
#' on.exit(stopmotion_verbosity(old))
#' @export
stopmotion_verbosity <- function(verbose) {
  checkmate::assert_flag(verbose)
  old <- getOption("stopmotion.verbose", default = interactive())
  options(stopmotion.verbose = verbose)
  invisible(old)
}

# Internal: message the current frame sequence after an operation.
print_frames <- function(images, fn_name) {
  if (!isTRUE(getOption("stopmotion.verbose", default = interactive()))) {
    return(invisible(NULL))
  }
  labels <- get_labels(images)
  n <- length(labels)
  message(sprintf("Frame sequence after %s() \u2014 %d frames:", fn_name, n))
  message(paste(
    sprintf(paste0(" %", nchar(n), "d  %s"), seq_len(n), labels),
    collapse = "\n"
  ))
  invisible(NULL)
}

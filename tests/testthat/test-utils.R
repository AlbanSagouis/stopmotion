test_that("stopmotion_verbosity() returns the previous value invisibly", {
  withr::with_options(list(stopmotion.verbose = TRUE), {
    old <- stopmotion_verbosity(FALSE)
    expect_identical(old, TRUE)
  })
  withr::with_options(list(stopmotion.verbose = FALSE), {
    old <- stopmotion_verbosity(TRUE)
    expect_identical(old, FALSE)
  })
})

test_that("stopmotion_verbosity() sets the option", {
  withr::with_options(list(stopmotion.verbose = NULL), {
    stopmotion_verbosity(TRUE)
    expect_identical(getOption("stopmotion.verbose"), TRUE)
    stopmotion_verbosity(FALSE)
    expect_identical(getOption("stopmotion.verbose"), FALSE)
  })
})

test_that("stopmotion_verbosity() errors on non-flag input", {
  expect_error(stopmotion_verbosity(1L))
  expect_error(stopmotion_verbosity("yes"))
  expect_error(stopmotion_verbosity(NA))
})

test_that("get_labels() initialises from indices when no labels attribute is set", {
  images <- make_film(n = 3L)
  # No labels set — get_labels() should return character indices.
  labels <- get_labels(images)
  expect_equal(labels, c("1", "2", "3"))
})

test_that("get_labels() returns the stored labels when present", {
  images <- make_film(n = 3L, labels = c("A", "B", "C"))
  labels <- get_labels(images)
  expect_equal(labels, c("A", "B", "C"))
})

test_that("set_labels() attaches labels as an attribute", {
  images <- make_film(n = 2L)
  images <- set_labels(images, labels = c("X", "Y"))
  expect_equal(attr(images, "labels"), c("X", "Y"))
})

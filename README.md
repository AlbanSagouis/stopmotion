
<!-- README.md is generated from README.Rmd. Please edit that file -->

# stopmotion

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check](https://github.com/AlbanSagouis/stopmotion/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/AlbanSagouis/stopmotion/actions/workflows/R-CMD-check.yaml)
![Code Coverage:
99%](https://img.shields.io/badge/code_coverage-99%25-brightgreen)

A pipeline-friendly toolkit for assembling and editing stop motion
animations from sequences of still images. **stopmotion** wraps the
[magick](https://docs.ropensci.org/magick/) package and adds frame-level
control that plain **magick** pipelines lack: every function accepts a
`frames` argument so that any operation can target a precise subset of
frames.

## Installation

Install the released version from CRAN:

``` r
install.packages("stopmotion")
```

Or install the development version from GitHub:

``` r
remotes::install_github(
    repo = "AlbanSagouis/stopmotion@dev",
    build_manual = TRUE,
    build_vignettes = TRUE
)
```

## Overview

| Family | Functions |
|----|----|
| Load | `read()` |
| Restructure | `duplicate()`, `splice()`, `arrange()` |
| Transform | `rotate()`, `wiggle()`, `flip()`, `flop()`, `blur()`, `scale()`, `crop()`, `trim()`, `border()`, `background()`, `centre()` |
| Display | `montage()`, `preview()` |

## Quick start

``` r
library(stopmotion)

# Load all PNG frames from a folder (sorted lexicographically — name files
# frame_001.png, frame_002.png, … to guarantee frame order).
images <- read(dir = "path/to/frames/")

# Inspect all frames side-by-side.
montage(images)

# Duplicate frames 3–4 to slow down a moment.
duplicate(images, frames = 3:4, style = "looped") |>

    # Rotate every frame 2° to the right.
    rotate(degrees = 2) |>

    # Flip only frame 5 vertically.
    flip(frames = 5) |>

    # Export as an animated GIF at 10 fps.
    preview(fps = 10)
```

After each operation **stopmotion** prints a message listing the updated
frame sequence in interactive sessions. Use
`stopmotion_verbosity(FALSE)` to silence these messages, or add
`options(stopmotion.verbose = FALSE)` to your `.Rprofile`.

## License

MIT © Alban Sagouis

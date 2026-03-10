library("testthat")

test_check("stopmotion")

# ── stopmotion function dependency map ────────────────────────────────────────
#
#  INTERNAL HELPERS (utils.R)
#  ──────────────────────────
#
#  get_labels()  ──────────────────────────────────────────────────────────────┐
#  set_labels()  ──────────────────────────────────────────────────────────────┤
#  print_frames()  (calls get_labels(); gated by stopmotion_verbosity()) ──────┤
#                                                                              │
#  USER-FACING FUNCTIONS                          calls internal helpers?      │
#  ─────────────────────────────────────────────  ───────────────────────      │
#                                                                              │
#  [Load]                                                                      │
#    read()            ── set_labels() ◄─────────────────────────────────────◄─┤
#                                                                              │
#  [Restructure]                                                               │
#    arrange()         ── get_labels, set_labels, print_frames ◄───────────────┤
#    duplicate()       ── get_labels, set_labels, print_frames ◄───────────────┤
#    splice()          ── get_labels, set_labels, print_frames ◄───────────────┤
#                                                                              │
#  [Transform]                                                                 │
#    background()      ── get_labels, set_labels, print_frames ◄───────────────┤
#    blur()            ── get_labels, set_labels, print_frames ◄───────────────┤
#    border()          ── get_labels, set_labels, print_frames ◄───────────────┤
#    centre()          ── get_labels, set_labels, print_frames ◄───────────────┤
#    crop()            ── get_labels, set_labels, print_frames ◄───────────────┤
#    flip()             ── get_labels, set_labels, print_frames ◄───────────────┤
#    flop()             ── get_labels, set_labels, print_frames ◄───────────────┤
#    rotate()          ── get_labels, set_labels, print_frames ◄───────────────┤
#    scale()           ── get_labels, set_labels, print_frames ◄───────────────┤
#    trim()            ── get_labels, set_labels, print_frames ◄───────────────┤
#    wiggle()          ── get_labels, set_labels, print_frames ◄───────────────┤
#               └── also applies image_rotate() twice (same primitive as       │
#                   rotate(), but always uses +degrees and -degrees pair)      │
#                                                                              │
#  [Display]                                                                   │
#    montage()         ── (no internal helpers; wraps magick directly)         │
#    preview()         ── get_labels ◄─────────────────────────────────────────┘
#
#  ── Typical pipeline flow ─────────────────────────────────────────────────
#
#  read()  ──►  arrange()?  ──►  [any transforms]  ──►  preview() / montage()
#
#  [option]  stopmotion_verbosity()  controls whether print_frames() emits
#            messages; defaults to interactive() if the option is unset.

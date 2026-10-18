test_that("bundled anatomical SVG template is available", {
  path <- itch_anatomical_template()
  expect_true(file.exists(path))
  svg <- readLines(path, warn = FALSE)
  expect_true(any(grepl("itch-head_scalp", svg, fixed = TRUE)))
  expect_true(any(grepl("itch-clothing_contact", svg, fixed = TRUE)))
})

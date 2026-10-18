test_that("SVG prevalence map writes a customized vector file", {
  ids <- names(itch_anatomy_regions())
  d <- as.data.frame(matrix(0, nrow = 2, ncol = length(ids)))
  names(d) <- ids
  d[2, ] <- 1
  out <- tempfile(fileext = ".svg")
  result <- write_itch_bodymap_svg(d, output_file = out)
  expect_true(file.exists(result))
  svg <- paste(readLines(result, warn = FALSE), collapse = "\n")
  expect_true(grepl('id="itch-head_scalp"', svg, fixed = TRUE))
  expect_true(grepl('id="itch-clothing_contact"', svg, fixed = TRUE))
})

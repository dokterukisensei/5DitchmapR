test_that("available denominator excludes missing responses", {
  r <- itch_regions()
  d <- as.data.frame(matrix(0, 4, length(r)))
  names(d) <- r
  d[[r[1]]] <- c(1, 1, 0, NA)
  z <- itch_prevalence(d, r)
  first <- z[z$region == r[1], ]
  expect_equal(first$affected, 2)
  expect_equal(first$denominator, 3)
  expect_equal(first$percent, 200 / 3)
})

test_that("validation catches missing columns", {
  expect_warning(z <- validate_itch_data(data.frame(x = c(0, 1))), "Missing columns")
  expect_false(z$valid)
})

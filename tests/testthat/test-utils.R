test_that("parse_range works", {

  x <- purrr::map(
    c("[0, 5]", "(0, 5]", "[0, 5)", "(0, 5)"),
    ~ parse_range(.))

  expect_identical(x[[1]]$lims, c(0, 5))
  expect_identical(x[[2]]$lims, c(0, 5))
  expect_identical(x[[3]]$lims, c(0, 5))
  expect_identical(x[[4]]$lims, c(0, 5))

  expect_identical(x[[1]]$lower_lim, `>=`)
  expect_identical(x[[2]]$lower_lim, `>`)
  expect_identical(x[[3]]$lower_lim, `>=`)
  expect_identical(x[[4]]$lower_lim, `>`)

  expect_identical(x[[1]]$upper_lim, `<=`)
  expect_identical(x[[2]]$upper_lim, `<=`)
  expect_identical(x[[3]]$upper_lim, `<`)
  expect_identical(x[[4]]$upper_lim, `<`)

  x <- purrr::map(
    c("[Inf, Inf]", "(-inf, inf]", "[0, Inf)", "(Inf, 5)"),
    ~ parse_range(.))

  expect_identical(x[[1]]$lims, c(-Inf, Inf))
  expect_identical(x[[2]]$lims, c(-Inf, Inf))
  expect_identical(x[[3]]$lims, c(0, Inf))
  expect_identical(x[[4]]$lims, c(-Inf, 5))

  expect_identical(x[[1]]$lower_lim, `>=`)
  expect_identical(x[[2]]$lower_lim, `>`)
  expect_identical(x[[3]]$lower_lim, `>=`)
  expect_identical(x[[4]]$lower_lim, `>`)

  expect_identical(x[[1]]$upper_lim, `<=`)
  expect_identical(x[[2]]$upper_lim, `<=`)
  expect_identical(x[[3]]$upper_lim, `<`)
  expect_identical(x[[4]]$upper_lim, `<`)

  expect_error(parse_range(c(3, 5)))
  expect_error(parse_range("{3, 5}"))

})

test_that("define_blocks validates member codes and scope names", {
  blocks <- define_blocks(list(ea = c("de", "fr", "it", "es")))
  expect_named(blocks, "ea")
  expect_error(define_blocks(list(ea = c("DE", "fr"))))
  expect_error(define_blocks(list(notascope = c("de", "fr"))))
})

test_that("stage3_block_identity renders a valid koma identity equation", {
  eq <- stage3_block_identity("ea", "gdp", c("de", "fr"), c(de = 0.6, fr = 0.4))
  expect_equal(eq, "ea_gdp == 0.6*de_gdp + 0.4*fr_gdp")
})

test_that("aggregate_blocks produces one series per block per concept", {
  x <- koma::as_ets(stats::ts(rep(1, 8), start = c(2020, 1), frequency = 4),
                    series_type = "rate", method = "none")
  panel <- list(de_gdp = x, fr_gdp = x)
  blocks <- list(ea = c("de", "fr"))
  out <- aggregate_blocks(panel, blocks, "gdp", list(ea = c(de = 0.6, fr = 0.4)))
  expect_named(out, "ea_gdp")
})

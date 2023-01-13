test_that("onValidate() caches query", {
  pool <- local_pool()
  # reset cache from initial creation + validation
  pool$state$validateQuery <- NULL

  con <- poolCheckout(pool)
  withr::defer(poolReturn(con))
  onValidate(con)
  expect_equal(pool$state$validateQuery, "SELECT 1")
})

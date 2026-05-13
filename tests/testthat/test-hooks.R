test_that("onValidate() caches query", {
  pool <- local_db_pool()
  # reset cache from initial creation + validation
  pool$state$validateQuery <- NULL

  con <- localCheckout(pool)
  onValidate(con)
  expect_equal(pool$state$validateQuery, "SELECT 1")
})

test_that("onValidate() chains underlying error when no query works", {
  pool <- local_db_pool()
  con <- localCheckout(pool)

  pool$state$validateQuery <- c(
    "SELECT * FROM nonexistent_table",
    "SELECT * FROM another_nonexistent_table"
  )
  expect_snapshot(onValidate(con), error = TRUE)

  pool$state$validateQuery <- c("SELECT * FROM nonexistent_table", "SELECT 1")
  expect_no_error(onValidate(con))
})

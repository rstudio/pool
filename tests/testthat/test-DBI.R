test_that("can set options on creation", {
  pool <- dbPool(RSQLite::SQLite(), onCreate = function(con) {
    DBI::dbExecute(con, "PRAGMA journal_mode = TRUNCATE")
  })
  withr::defer(poolClose(pool))

  res <- DBI::dbGetQuery(pool, "PRAGMA journal_mode")[[1]]
  expect_equal(res, "truncate")
})

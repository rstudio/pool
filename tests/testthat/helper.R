local_pool <- function(env = parent.frame()) {
  pool <- dbPool(RSQLite::SQLite())
  dbWriteTable(pool, "mtcars", mtcars)

  withr::defer(poolClose(pool), envir = env)
  pool
}

checkCounts <- function(pool, free, taken) {
  expect_equal(pool$counters$free, free)
  expect_equal(pool$counters$taken, taken)
}

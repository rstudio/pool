local_db_pool <- function(env = parent.frame()) {
  pool <- dbPool(RSQLite::SQLite())
  defer(poolClose(pool), envir = env)
  pool
}

checkCounts <- function(pool, free, taken) {
  expect_equal(pool$counters$free, free)
  expect_equal(pool$counters$taken, taken)
}

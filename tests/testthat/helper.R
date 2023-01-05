local_pool <- function(env = parent.frame()) {
  pool <- dbPool(RSQLite::SQLite())
  dbWriteTable(pool, "mtcars", mtcars)

  withr::defer(poolClose(pool), envir = env)
  pool
}

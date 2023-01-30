test_that("localCheckout works", {
  pool <- poolCreate(function(x) 1)
  withr::defer(poolClose(pool))

  f <- function() {
    localCheckout(pool)
    checkCounts(pool, free = 0, taken = 1)
  }

  f()
  checkCounts(pool, free = 1, taken = 0)
})

test_that("createObject throws if `factory` throws or returns NULL", {
  expect_snapshot(error = TRUE, {
    poolCreate(MockPooledObj)
    poolCreate(function(x) NULL)
  })
})

test_that("useful warning if onDestroy fails", {
  pool <- poolCreate(MockPooledObj$new, idleTimeout = 0)

  checkCounts(pool, free = 1, taken = 0)
  failOnDestroy <<- TRUE

  a <- poolCheckout(pool)
  b <- poolCheckout(pool)

  # since we're over minSize, returning `b` destroy it
  expect_snapshot({
    poolReturn(b)
    later::run_now()
  })

  checkCounts(pool, free = 0, taken = 1)
  failOnDestroy <<- FALSE

  poolReturn(a)
  poolClose(pool)
  gc()
})

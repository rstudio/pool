test_that("max size is enforced", {
  pool <- poolCreate(MockPooledObj$new, maxSize = 2)
  withr::defer(poolClose(pool))

  obj1 <- poolCheckout(pool)
  obj2 <- poolCheckout(pool)

  expect_snapshot(poolCheckout(pool), error = TRUE)

  poolReturn(obj1)
  poolReturn(obj2)
})

test_that("idle objects are reaped", {
  pool <- poolCreate(MockPooledObj$new, idleTimeout = 0)
  withr::defer(poolClose(pool))

  obj1 <- poolCheckout(pool)
  obj2 <- poolCheckout(pool)
  poolReturn(obj1)
  poolReturn(obj2)

  checkCounts(pool, free = 2, taken = 0)
  later::run_now() # force scheduler to run NOW
  checkCounts(pool, free = 1, taken = 0)
})

test_that("can't return the same object twice", {
  pool <- poolCreate(MockPooledObj$new)
  withr::defer(poolClose(pool))

  obj <- poolCheckout(pool)
  poolReturn(obj)
  expect_snapshot(poolReturn(obj), error = TRUE)
})

test_that("warns if onPassivate fails", {
  pool <- poolCreate(MockPooledObj$new)
  withr::defer(poolClose(pool))

  obj <- poolCheckout(pool)
  failOnPassivate <<- TRUE
  expect_snapshot(poolReturn(obj), error = TRUE)
  failOnPassivate <<- FALSE
})

test_that("poolClose() warns about taken objects, but they can still be returned", {
  pool <- poolCreate(MockPooledObj$new)

  obj <- poolCheckout(pool)
  expect_snapshot(poolClose(pool))

  poolReturn(obj)
})

test_that("warns if object can't be returned", {
  pr_expect_snapshot({
    pool <- poolCreate(function() 1)
    obj <- poolCheckout(pool)
    rm(obj)
    . <- gc()
    poolClose(pool)
  })
})

test_that("poolReturn() errors if object is not valid", {
  expect_snapshot(poolReturn("x"), error = TRUE)
})

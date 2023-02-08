test_that("can be created and closed", {
  pool <- poolCreate(function() 1)
  checkCounts(pool, free = 1, taken = 0)
  expect_true(pool$valid)

  poolClose(pool)
  checkCounts(pool, free = 0, taken = 0)
  expect_false(pool$valid)
})

test_that("it requires a valid factory", {
  expect_snapshot(error = TRUE, {
    poolCreate(1)
    poolCreate(function(x) NULL)
  })
})

test_that("pool can't be closed twice", {
  pool <- poolCreate(function() 1)
  poolClose(pool)

  expect_snapshot(poolCheckout(pool), error = TRUE)
})

test_that("can fetch and release", {
  pool <- poolCreate(function() 1)
  withr::defer(poolClose(pool))

  obj <- poolCheckout(pool)
  expect_equal(obj, 1, ignore_attr = TRUE)
  checkCounts(pool, free = 0, taken = 1)

  poolReturn(obj)
  checkCounts(pool, free = 1, taken = 0)
})

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

test_that("validates (only) when needed", {
  pool <- poolCreate(MockPooledObj$new, validationInterval = 0.1)
  withr::defer(poolClose(pool))

  last_validated <- function(pool) {
    obj <- localCheckout(pool)
    pool_metadata(obj)$lastValidated
  }

  # Capture initial validation time
  last_validated_0 <- last_validated(pool)

  # After waiting less than validationInterval, validation time shouldn't change
  Sys.sleep(pool$validationInterval / 2)
  last_validated_1 <- last_validated(pool)
  expect_equal(last_validated_0, last_validated_1)

  # After waiting more than validationInterval, validation time should change
  Sys.sleep(pool$validationInterval)
  last_validated_2 <- last_validated(pool)
  expect_lt(last_validated_0, last_validated_2)
})

test_that("warns if validation fails once, creates new object and tries again", {
  pool <- poolCreate(MockPooledObj$new, validationInterval = 0.1)
  withr::defer(poolClose(pool))

  check_valid_object <- function(x) {
    # Sneak into private methods
    pool[['.__enclos_env__']]$private$checkValid(x)
  }

  # create object that will fail to validate
  badObject <- poolCheckout(pool)
  attr(badObject, "bad") <- TRUE
  Sys.sleep(pool$validationInterval + .1)

  # can't validate, so should create a new object
  expect_snapshot(obj <- check_valid_object(badObject))

  Sys.sleep(pool$validationInterval + .1)
  expect_identical(obj, check_valid_object(obj))
  # this implicitly returns badOjbect
  checkCounts(pool, free = 1, taken = 0)

  # now force all validations to fail so we get an error
  failOnValidate <<- TRUE
  withr::defer(failOnValidate <<- FALSE)

  Sys.sleep(pool$validationInterval + .1)
  expect_snapshot(check_valid_object(obj), error = TRUE)

  # and since all objects have been destroyed the pool is empty
  checkCounts(pool, free = 0, taken = 0)

})

test_that("can't return the same object twice", {
  pool <- poolCreate(MockPooledObj$new)
  withr::defer(poolClose(pool))

  obj <- poolCheckout(pool)
  poolReturn(obj)
  expect_snapshot(poolReturn(obj), error = TRUE)
})

test_that("poolClose() warns about taken objects, but they can still be returned", {
  pool <- poolCreate(MockPooledObj$new)

  obj <- poolCheckout(pool)
  expect_snapshot(poolClose(pool))

  poolReturn(obj)
})

test_that("warns if object can't be returned", {
  expect_snapshot({
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

test_that("pool has useful print method", {
  pool <- poolCreate(function() 10)
  withr::defer(poolClose(pool))

  expect_snapshot({
    pool

    x1 <- poolCheckout(pool)
    x2 <- poolCheckout(pool)
    pool

    poolReturn(x1)
    pool

    poolReturn(x2)
  })
})

test_that("empty pool has useful print method", {
  pool <- poolCreate(function() 10, minSize = 0)
  withr::defer(poolClose(pool))

  expect_snapshot({
    pool
  })
})

# Failure modes -----------------------------------------------------------

test_that("useful warning if onDestroy fails", {
  pool <- poolCreate(MockPooledObj$new, idleTimeout = 0)
  withr::defer(poolClose(pool))

  checkCounts(pool, free = 1, taken = 0)
  failOnDestroy <<- TRUE
  withr::defer(failOnDestroy <<- FALSE)

  a <- poolCheckout(pool)
  b <- poolCheckout(pool)

  # since we're over minSize, returning `b` destroys it
  expect_snapshot({
    poolReturn(b)
    later::run_now()
  })

  checkCounts(pool, free = 0, taken = 1)
  poolReturn(a)
})

test_that("throws if onPassivate fails", {
  pool <- poolCreate(MockPooledObj$new)
  withr::defer(poolClose(pool))

  obj <- poolCheckout(pool)
  failOnPassivate <<- TRUE
  withr::defer(failOnPassivate <<- FALSE)

  expect_snapshot(poolReturn(obj), error = TRUE)
})

test_that("throws if onActivate fails", {
  pool <- poolCreate(MockPooledObj$new)
  withr::defer(poolClose(pool))

  failOnActivate <<- TRUE
  withr::defer(failOnActivate <<- FALSE)

  expect_snapshot(poolCheckout(pool), error = TRUE)
  checkCounts(pool, free = 0, taken = 0)
})

test_that("throws if onValidate fails", {
  pool <- poolCreate(MockPooledObj$new)
  withr::defer(poolClose(pool))

  failOnValidate <<- TRUE
  withr::defer(failOnValidate <<- FALSE)
  expect_snapshot(poolCheckout(pool), error = TRUE)
  checkCounts(pool, free = 0, taken = 0)
})


test_that("warns if validation fails once, creates new object and tries again", {
  pool <- poolCreate(MockPooledObj$new, validationInterval = 0.1)
  withr::defer(poolClose(pool))

  ## create function to get to an R6 object's private methods
  ## (gets private environment from an R6 object)
  get_private <- function(x) {
    x[['.__enclos_env__']]$private
  }

  ## cannot validate bad object, so creates new one and tries again
  ## new object's activation and validation succeeds
  badObject <- poolCheckout(pool)

  attr(badObject, "bad") <- TRUE
  Sys.sleep(pool$validationInterval + .1)
  expect_snapshot(obj <- get_private(pool)$checkValid(badObject))

  Sys.sleep(pool$validationInterval + .1)
  ## check that the new object is valid
  expect_identical(obj, get_private(pool)$checkValid(obj))

  ## back to having one free, valid object
  checkCounts(pool, free = 1, taken = 0)

  Sys.sleep(pool$validationInterval + .1)
  ## cannot validate bad object, so creates new one and tries again
  ## new object's activation and validation also fails: throw
  failOnValidate <<- TRUE

  expect_snapshot(get_private(pool)$checkValid(obj), error = TRUE)
  failOnValidate <<- FALSE

  ## since we couldn't validate the object the first or the second
  ## time around, it was destroyed, and there are now no objects
  ## in the pool
  checkCounts(pool, free = 0, taken = 0)
})

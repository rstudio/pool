source("utils.R")

context("Pool's fetch method")

describe("fetch", {

  pool <- poolCreate(MockPooledObj$new, minSize = 1, maxSize = 3,
    validationInterval = 1)

  it("throws if onActivate fails", {
    checkCounts(pool, free = 1, taken = 0)

    failOnActivate <<- TRUE
    expect_error(
      expect_warning(obj <- poolCheckout(pool),
        paste("It wasn't possible to activate and/or validate",
          "the object. Trying again with a new object.")),
      "Object does not appear to be valid.")
    checkCounts(pool, free = 0, taken = 0)
    failOnActivate <<- FALSE
  })

  it("throws if onValidate fails", {
    checkCounts(pool, free = 0, taken = 0)
    failOnValidate <<- TRUE
    expect_error(
      expect_warning(poolCheckout(pool),
        paste("It wasn't possible to activate and/or validate",
          "the object. Trying again with a new object.")),
      "Object does not appear to be valid.")
    checkCounts(pool, free = 0, taken = 0)
    failOnValidate <<- FALSE
  })

  it("only validates after validationInterval", {
    obj <- poolCheckout(pool)
    t0 <- Sys.time()
    pool_metadata <- attr(obj, "pool_metadata", exact = TRUE)
    lastValidated_t0 <- pool_metadata$lastValidated

    poolReturn(obj)

    obj <- poolCheckout(pool)
    t1 <- Sys.time()
    pool_metadata <- attr(obj, "pool_metadata", exact = TRUE)
    lastValidated_t1 <- pool_metadata$lastValidated

    if (difftime(t1, t0, units = "secs") < pool$validationInterval) {
      ## because validationInterval hasn't passed yet
      expect_identical(lastValidated_t0, lastValidated_t1)
    }

    checkCounts(pool, free = 0, taken = 1)
    poolReturn(obj)
    checkCounts(pool, free = 1, taken = 0)

    obj <- poolCheckout(pool)
    t2 <- Sys.time()
    pool_metadata <- attr(obj, "pool_metadata", exact = TRUE)
    lastValidated_t2 <- pool_metadata$lastValidated

    if (difftime(t2, t0, units = "secs") < pool$validationInterval) {
      ## because validationInterval hasn't passed yet
      expect_identical(lastValidated_t0, lastValidated_t2)
    }

    checkCounts(pool, free = 0, taken = 1)
    poolReturn(obj)
    checkCounts(pool, free = 1, taken = 0)

    Sys.sleep(pool$validationInterval + 1)

    obj <- poolCheckout(pool)
    t3 <- Sys.time()
    pool_metadata <- attr(obj, "pool_metadata", exact = TRUE)
    lastValidated_t3 <- pool_metadata$lastValidated

    if (difftime(t3, t0, units = "secs") > pool$validationInterval) {
      ## because validationInterval HAS passed at this point
      expect_false(identical(lastValidated_t0, lastValidated_t3))
    }

    checkCounts(pool, free = 0, taken = 1)
    poolReturn(obj)
    checkCounts(pool, free = 1, taken = 0)

    obj <- poolCheckout(pool)
    t4 <- Sys.time()
    pool_metadata <- attr(obj, "pool_metadata", exact = TRUE)
    lastValidated_t4 <- pool_metadata$lastValidated

    if (difftime(t4, t3, units = "secs") < pool$validationInterval) {
      ## because validationInterval hasn't passed yet
      expect_identical(lastValidated_t3, lastValidated_t4)
    }

    checkCounts(pool, free = 0, taken = 1)
    poolReturn(obj)
    checkCounts(pool, free = 1, taken = 0)
  })

  it("warns if validation fails once, creates new object and tries again", {
    checkCounts(pool, free = 1, taken = 0)

    ## create function to get to an R6 object's private methods
    ## (gets private environment from an R6 object)
    get_private <- function(x) {
      x[['.__enclos_env__']]$private
    }

    ## cannot validate bad object, so creates new one and tries again
    ## new object's activation and validation succeeds
    badObject <- poolCheckout(pool)
    checkCounts(pool, free = 0, taken = 1)

    Sys.sleep(pool$validationInterval + 1)
    attr(badObject, "bad") <- TRUE
    expect_warning(obj <- get_private(pool)$checkValid(badObject),
      paste("It wasn't possible to activate and/or validate",
        "the object. Trying again with a new object."))

    Sys.sleep(pool$validationInterval + 1)
    ## check that the new object is valid
    expect_identical(obj, get_private(pool)$checkValid(obj))

    ## back to having one free, valid object
    checkCounts(pool, free = 1, taken = 0)

    Sys.sleep(pool$validationInterval + 1)
    ## cannot validate bad object, so creates new one and tries again
    ## new object's activation and validation also fails: throw
    failOnValidate <<- TRUE
    expect_error(
      expect_warning(get_private(pool)$checkValid(obj),
        paste("It wasn't possible to activate and/or validate",
          "the object. Trying again with a new object.")),
      "Object does not appear to be valid.")
    failOnValidate <<- FALSE

    ## since we couldn't validate the object the first or the second
    ## time around, it was destroyed, and there are now no objects
    ## in the pool
    checkCounts(pool, free = 0, taken = 0)
  })

  it("throws if the pool was closed", {
    checkCounts(pool, free = 0, taken = 0)
    obj <- poolCheckout(pool)
    poolReturn(obj)

    checkCounts(pool, free = 1, taken = 0)
    poolClose(pool)
    checkCounts(pool, free = 0, taken = 0)
    expect_error(poolCheckout(pool),
      "This pool is no longer valid. Cannot fetch new objects.")
  })
})

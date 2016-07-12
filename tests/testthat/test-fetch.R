source("utils.R")

context("Pool's fetch method")

describe("fetch", {

  pool <- poolCreate(MockPooledObj$new,
    closed = FALSE, valid = TRUE,
    minSize = 1, maxSize = 3)

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

  it("only validates after validateTimeout", {
    naiveScheduler$protect({
      obj <- poolCheckout(pool)
      t0 <- Sys.time()
      ..metadata <- attr(obj, "..metadata", exact = TRUE)
      lastValidated_t0 <- ..metadata$..lastValidated

      poolReturn(obj)

      obj <- poolCheckout(pool)
      t1 <- Sys.time()
      ..metadata <- attr(obj, "..metadata", exact = TRUE)
      lastValidated_t1 <- ..metadata$..lastValidated

      if ((t1 - t0)*1000 < pool$validateTimeout) {
        ## because validateTimeout hasn't passed yet
        expect_identical(lastValidated_t0, lastValidated_t1)
      }

      checkCounts(pool, free = 0, taken = 1)
      poolReturn(obj)
      checkCounts(pool, free = 1, taken = 0)

      obj <- poolCheckout(pool)
      t2 <- Sys.time()
      ..metadata <- attr(obj, "..metadata", exact = TRUE)
      lastValidated_t2 <- ..metadata$..lastValidated

      if ((t2 - t0)*1000 < pool$validateTimeout) {
        ## because validateTimeout hasn't passed yet
        expect_identical(lastValidated_t0, lastValidated_t2)
      }

      checkCounts(pool, free = 0, taken = 1)
      poolReturn(obj)
      checkCounts(pool, free = 1, taken = 0)

      Sys.sleep((pool$validateTimeout + 100)/1000)

      obj <- poolCheckout(pool)
      t3 <- Sys.time()
      ..metadata <- attr(obj, "..metadata", exact = TRUE)
      lastValidated_t3 <- ..metadata$..lastValidated

      if ((t3 - t0)*1000 > pool$validateTimeout) {
        ## because validateTimeout HAS passed at this point
        expect_false(identical(lastValidated_t0, lastValidated_t3))
      }

      checkCounts(pool, free = 0, taken = 1)
      poolReturn(obj)
      checkCounts(pool, free = 1, taken = 0)

      obj <- poolCheckout(pool)
      t4 <- Sys.time()
      ..metadata <- attr(obj, "..metadata", exact = TRUE)
      lastValidated_t4 <- ..metadata$..lastValidated

      if ((t4 - t3)*1000 < pool$validateTimeout) {
        ## because validateTimeout hasn't passed yet
        expect_identical(lastValidated_t3, lastValidated_t4)
      }

      checkCounts(pool, free = 0, taken = 1)
      poolReturn(obj)
      checkCounts(pool, free = 1, taken = 0)
    })
  })

  it("throws if the pool was closed", {
    checkCounts(pool, free = 1, taken = 0)
    poolClose(pool)
    checkCounts(pool, free = 0, taken = 0)
    expect_error(poolCheckout(pool),
      "This pool is no longer valid. Cannot fetch new objects.")
  })
})




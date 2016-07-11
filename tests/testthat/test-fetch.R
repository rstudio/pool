source("utils.R")

context("Pool's fetch method")

describe("fetch", {

  pool <- poolCreate(MockPooledObj$new,
    closed = FALSE, valid = TRUE,
    minSize = 1, maxSize = 3, idleTimeout = 1000,
    validateTimeout = 0) ## to force immediate validation for testing purposes

  it("throws if onActivate fails", {
    checkCounts(pool, free = 1, taken = 0)

    failOnActivate <<- TRUE
    expect_error(obj <- poolCheckout(pool),
      "Object does not appear to be valid.")
    checkCounts(pool, free = 0, taken = 0)
    failOnActivate <<- FALSE
  })

  it("throws if onValidate fails", {
    checkCounts(pool, free = 0, taken = 0)
    failOnValidate <<- TRUE
    expect_error(poolCheckout(pool),
     "Object does not appear to be valid.")
    checkCounts(pool, free = 0, taken = 0)
    failOnValidate <<- FALSE
  })

  it("throws if the pool was closed", {
    poolClose(pool)
    expect_error(poolCheckout(pool),
     "This pool is no longer valid. Cannot fetch new objects.")
  })
})




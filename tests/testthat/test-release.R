source("utils.R")

context("Pool's release method")

describe("release", {

  pool <- poolCreate(MockPooledObj$new,
    closed = FALSE, valid = TRUE,
    minSize = 1, maxSize = 3, idleTimeout = 1000)

  it("throws if object was already released", {
    checkCounts(pool, free = 1, taken = 0)
    obj <- poolCheckout(pool)
    poolReturn(obj)
    expect_error(poolReturn(obj),
      "This object was already returned to the pool.")
    checkCounts(pool, free = 1, taken = 0)
  })

  it("throws if object is not valid", {
    obj <- "a"
    expect_error(poolReturn(obj), "Invalid object.")
  })

  it("warns if onPassivate fails", {
    checkCounts(pool, free = 1, taken = 0)
    obj <- poolCheckout(pool)
    failOnPassivate <<- TRUE
    expect_error(poolReturn(obj),
      "Object could not be returned back to the pool. ",
      "It was destroyed instead.")
    failOnPassivate <<- FALSE
    checkCounts(pool, free = 0, taken = 0)
  })

  it("is allowed after the pool is closed", {
    checkCounts(pool, free = 0, taken = 0)
    obj <- poolCheckout(pool)
    checkCounts(pool, free = 0, taken = 1)
    expect_warning(poolClose(pool),
      "You still have checked out objects.")
    checkCounts(pool, free = 0, taken = 1)
    poolReturn(obj)
    checkCounts(pool, free = 0, taken = 0)
    expect_error(poolClose(pool),
      "The pool was already closed.")
  })
})




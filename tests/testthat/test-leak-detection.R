source("utils.R")

context("Pool leak detection")

describe("pool", {

  pool <- poolCreate(MockPooledObj$new,
    closed = FALSE, valid = TRUE,
    minSize = 1, maxSize = 3, idleTimeout = 1000)

  it("checks for leaks (anonymous)", {
    checkCounts(pool, free = 1, taken = 0)
    poolCheckout(pool)
    expect_warning(gc(), "You have a leaked pooled object. Destroying it.")
    checkCounts(pool, free = 0, taken = 0)
  })

  it("checks for leaks (named)", {
    obj <- poolCheckout(pool)
    checkCounts(pool, free = 0, taken = 1)
    rm(obj)
    expect_warning(gc(), "You have a leaked pooled object. Destroying it.")
    checkCounts(pool, free = 0, taken = 0)
  })

  poolClose(pool)
})

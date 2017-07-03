source("utils.R")

context("Pool leak detection")

describe("pool", {

  pool <- poolCreate(MockPooledObj$new,
    minSize = 1, maxSize = 3, idleTimeout = 0)

  it("checks for leaks (anonymous)", {
    checkCounts(pool, free = 1, taken = 0)
    poolCheckout(pool)
    gc()
    expect_warning(gc(), "You have a leaked pooled object.")
    checkCounts(pool, free = 0, taken = 1)
  })

  it("checks for leaks (named)", {
    obj <- poolCheckout(pool)
    checkCounts(pool, free = 0, taken = 2)
    rm(obj)
    expect_warning(gc(), "You have a leaked pooled object.")
    checkCounts(pool, free = 0, taken = 2)
  })

  it("warns if it's closed with 1+ checked out objects", {
    expect_warning(poolClose(pool),
      "You still have checked out objects")
  })
})

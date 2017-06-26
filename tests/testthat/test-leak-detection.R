source("utils.R")

context("Pool leak detection")

describe("pool", {

  pool1 <- poolCreate(MockPooledObj$new,
    minSize = 1, maxSize = 3, idleTimeout = 1)

  it("checks for leaks (anonymous)", {
    checkCounts(pool1, free = 1, taken = 0)
    poolCheckout(pool1)

    later::later(function() {
      expect_warning(gc(), "You have a leaked pooled object.")
      checkCounts(pool1, free = 0, taken = 0)
      poolClose(pool1)
    }, 1.5)
  })

  pool2 <- poolCreate(MockPooledObj$new,
    minSize = 1, maxSize = 3, idleTimeout = 1)

  it("checks for leaks (named)", {
    obj <- poolCheckout(pool2)
    checkCounts(pool2, free = 0, taken = 1)
    rm(obj)

    later::later(function() {
      expect_warning(gc(), "You have a leaked pooled object.")
      checkCounts(pool2, free = 0, taken = 0)
    }, 1.5)
  })

  poolClose(pool2)
})

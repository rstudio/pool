source("utils.R")

context("Pool's createObject and destroyObject methods")

describe("createObject", {

  it("throws if `factory` throws or returns NULL", {
    expect_error(poolCreate(MockPooledObj),
      "attempt to apply non-function")
    expect_error(poolCreate(function(x) NULL),
      "Object creation was not successful.")
  })
})

describe("destroyObject", {

  pool <- poolCreate(MockPooledObj$new,
    minSize = 1, maxSize = 3, idleTimeout = 0)

  it("throws if onDestroy fails", {
    checkCounts(pool, free = 1, taken = 0)
    failOnDestroy <<- TRUE

    a <- poolCheckout(pool)
    b <- poolCheckout(pool)
    checkCounts(pool, free = 0, taken = 2)

    poolReturn(b)

    ## since we're over the minSize, once we return `b` to
    ## the pool, it will be destroyed immediately (since
    ## we set `idleTimeout = 0`)
    expect_warning(gc(),
      "Object of class MockPooledObj could not be ",
      "destroyed properly, but was successfully removed ",
      "from pool.")

    checkCounts(pool, free = 0, taken = 1)
    failOnDestroy <<- FALSE

    ## cleanup: return `a`
    poolReturn(a)
  })

  poolClose(pool)
  gc()
})


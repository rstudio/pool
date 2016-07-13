source("utils.R")

context("Pool's createObject and destroyObject methods")

describe("createObject", {

  it("throws if `factory` throws or returns NULL", {
    expect_error(poolCreate(MockPooledObj),
      "could not find function \"factory\"")
    expect_error(poolCreate(function(x) NULL),
      "Object creation was not successful.")
  })
})

describe("destroyObject", {

  pool <- poolCreate(MockPooledObj$new,
    closed = FALSE, valid = TRUE,
    minSize = 1, maxSize = 3, idleTimeout = 1000)

  it("throws if onDestroy fails", {
    checkCounts(pool, free = 1, taken = 0)
    failOnDestroy <<- TRUE

    a <- poolCheckout(pool)
    b <- poolCheckout(pool)
    checkCounts(pool, free = 0, taken = 2)

    ## since we're over the minSize, once we return `b` to
    ## the pool, it will be destroyed immediately (since
    ## we're using the default eager scheduler)
    expect_warning(poolReturn(b),
      "Object of class MockPooledObj could not be ",
      "destroyed properly, but was successfully removed ",
      "from pool.")
    checkCounts(pool, free = 0, taken = 1)
    failOnDestroy <<- FALSE

    ## cleanup: return `a`
    poolReturn(a)
    checkCounts(pool, free = 1, taken = 0)
  })

  poolClose(pool)
  gc()
})


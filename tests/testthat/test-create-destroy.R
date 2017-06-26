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
    minSize = 1, maxSize = 3, idleTimeout = 1)

  it("throws if onDestroy fails", {
    checkCounts(pool, free = 1, taken = 0)
    failOnDestroy <<- TRUE

    a <- poolCheckout(pool)
    b <- poolCheckout(pool)
    checkCounts(pool, free = 0, taken = 2)

    ## since we're over the minSize, once we return `b` to
    ## the pool, it will be destroyed after 1 second (since
    ## that's what we set for `idleTimeout`)
    later::later(function() {
      expect_warning(poolReturn(b),
        "Object of class MockPooledObj could not be ",
        "destroyed properly, but was successfully removed ",
        "from pool.")
      checkCounts(pool, free = 0, taken = 1)
      failOnDestroy <<- FALSE
    }, 1.5)

    ## cleanup: return `a`
    poolReturn(a)
  })

  poolClose(pool)
  gc()
})


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

    ## since we're over the minSize, once we return `b` to
    ## the pool, it will be destroyed immediately (since
    ## we set `idleTimeout = 0`)
    #
    # Previously, the expect_error() below was a expect_warning(), but a
    # change in later 1.0.0 altered the way that warnings are handled; they no
    # longer pass up through run_now(). A future version of later may change
    # that back to the original behavior. The workaround is to convert the
    # warning to an error and look for the error.
    op <- options(warn = 2)
    on.exit(options(op), add = TRUE)
    expect_error({
        poolReturn(b)
        later::run_now() # this is needed so that the scheduler runs NOW
      },
      regexp = paste0(
        "Object of class MockPooledObj could not be ",
        "destroyed properly, but was successfully removed ",
        "from pool."
      ),
      # The class seems redundant, but is necessary for this test to not throw
      # an unnecessary error with later<1.0.0. It can be removed in the future
      # after later 1.0.0 has been released.
      class = "error"
    )

    checkCounts(pool, free = 0, taken = 1)
    failOnDestroy <<- FALSE

    ## cleanup: return `a`
    poolReturn(a)
    checkCounts(pool, free = 1, taken = 0)
  })

  poolClose(pool)
  gc()
})

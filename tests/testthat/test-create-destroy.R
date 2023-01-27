source("utils.R")

test_that("createObject throws if `factory` throws or returns NULL", {
  expect_snapshot(error = TRUE, {
    poolCreate(MockPooledObj)
    poolCreate(function(x) NULL)
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

    expect_snapshot({
      poolReturn(b)
      later::run_now()
    })

    checkCounts(pool, free = 0, taken = 1)
    failOnDestroy <<- FALSE

    ## cleanup: return `a`
    poolReturn(a)
    checkCounts(pool, free = 1, taken = 0)
  })

  poolClose(pool)
  gc()
})

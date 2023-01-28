source("utils.R")

describe("release", {
  local_reproducible_output()

  pool <- poolCreate(MockPooledObj$new,
    minSize = 1, maxSize = 3, idleTimeout = 0)

  it("returns the object back to the pool, and it can be recycled", {
    checkCounts(pool, free = 1, taken = 0)
    obj1 <- poolCheckout(pool)
    obj2 <- poolCheckout(pool)
    obj3 <- poolCheckout(pool)
    checkCounts(pool, free = 0, taken = 3)

    expect_snapshot(poolCheckout(pool), error = TRUE)

    checkCounts(pool, free = 0, taken = 3)
    poolReturn(obj3)
    obj4 <- poolCheckout(pool)
    poolReturn(obj1)
    poolReturn(obj2)
    poolReturn(obj4)

    later::run_now() # this is needed so that the scheduler runs NOW
    checkCounts(pool, free = 1, taken = 0)
  })

  it("throws if object was already released", {
    checkCounts(pool, free = 1, taken = 0)
    obj <- poolCheckout(pool)
    poolReturn(obj)
    expect_snapshot(poolReturn(obj), error = TRUE)
    checkCounts(pool, free = 1, taken = 0)
  })

  it("warns if onPassivate fails", {
    checkCounts(pool, free = 1, taken = 0)
    obj <- poolCheckout(pool)
    failOnPassivate <<- TRUE
    expect_snapshot(poolReturn(obj), error = TRUE)
    failOnPassivate <<- FALSE
    checkCounts(pool, free = 0, taken = 0)
  })

  it("is allowed after the pool is closed", {
    checkCounts(pool, free = 0, taken = 0)
    obj <- poolCheckout(pool)
    checkCounts(pool, free = 0, taken = 1)
    expect_snapshot(poolClose(pool))
    checkCounts(pool, free = 0, taken = 1)
    poolReturn(obj)
    checkCounts(pool, free = 0, taken = 0)
    expect_snapshot(poolClose(pool), error = TRUE)
  })

  it("warns if object can't be returned", {
    expect_snapshot({
      pool <- poolCreate(function() 1)
      obj <- poolCheckout(pool)
      rm(obj)
      . <- gc()
      poolClose(pool)
    })
  })

})
test_that("poolReturn() errors if object is not valid", {
  expect_snapshot(poolReturn("x"), error = TRUE)
})

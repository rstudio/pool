source("utils.R")

context("Pool basics")

describe("pool", {

  describe("basic mechanics", {
    pool <- poolCreate(MockPooledObj$new,
      minSize = 1, maxSize = 3, idleTimeout = 1000)

    it("can be created", {
      expect_is(pool, "Pool")
      expect_is(pool, "R6")
    })

    it("respects validity", {
      expect_true(pool$valid)
      poolClose(pool)
      expect_false(pool$valid)
    })

    it("destroys all free objects when closed", {
      checkCounts(pool, free = 0)
    })

    it("finalizer runs", {
      pool <- poolCreate(MockPooledObj$new,
        minSize = 1, maxSize = 3, idleTimeout = 1000)
      rm(pool)
    })
  })

  describe("object operations", {
    pool <- poolCreate(MockPooledObj$new,
      minSize = 1, maxSize = 3, idleTimeout = 1000)

    it("supports generic fetch/release", {
      checkCounts(pool, free = 1, taken = 0)

      obj <- poolCheckout(pool)
      checkCounts(pool, free = 0, taken = 1)
      expect_is(obj, "MockPooledObj")

      poolReturn(obj)
      checkCounts(pool, free = 1, taken = 0)
      expect_false(obj$closed)
    })

    it("enforces maxSize", {
      a <- poolCheckout(pool)
      b <- poolCheckout(pool)
      c <- poolCheckout(pool)
      expect_error(poolCheckout(pool),
        "Maximum number of objects in pool has been reached")
      objs <- list(a, b, c)
      lapply(objs, poolReturn)
    })

    poolClose(pool)
  })
})



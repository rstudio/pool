source("utils.R")

context("Pool basics")

describe("pool", {

  describe("basic mechanics", {
    pool <- poolCreate(MockPooledObj$new,
      closed = FALSE, valid = TRUE,
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

    it("destroys all objects when closed", {
      checkCounts(pool, free = 0, taken = 0)
    })
  })

  describe("object operations", {
    pool <- poolCreate(MockPooledObj$new,
      closed = FALSE, valid = TRUE,
      minSize = 1, maxSize = 3, idleTimeout = 1000)

    it("supports generic fetch/release", {
      fetched <- poolCheckout(pool)
      checkCounts(pool, free = 0, taken = 1)
      expect_is(fetched, "MockPooledObj")

      poolReturn(fetched)
      checkCounts(pool, free = 1, taken = 0)
      expect_false(fetched$closed)
    })

    it("enforces maxSize", {
      a <- poolCheckout(pool)
      b <- poolCheckout(pool)
      c <- poolCheckout(pool)
      expect_error(poolCheckout(pool))
    })
  })
})



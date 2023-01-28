source("utils.R")

describe("pool", {
  local_reproducible_output()

  describe("basic mechanics", {
    pool <- poolCreate(MockPooledObj$new,
      minSize = 1, maxSize = 3, idleTimeout = 1)

    it("can be created", {
      expect_s3_class(pool, "Pool")
      expect_s3_class(pool, "R6")
    })

    it("respects validity", {
      expect_true(pool$valid)
      poolClose(pool)
      expect_false(pool$valid)
    })

    it("destroys all free objects when closed", {
      checkCounts(pool, free = 0)
    })

    # it("finalizer runs", {
    #   pool <- poolCreate(MockPooledObj$new,
    #     minSize = 1, maxSize = 3, idleTimeout = 1)
    #   rm(pool)
    # })
  })

  describe("object operations", {
    pool <- poolCreate(MockPooledObj$new,
      minSize = 1, maxSize = 3, idleTimeout = 1)

    it("supports generic fetch/release", {
      checkCounts(pool, free = 1, taken = 0)

      obj <- poolCheckout(pool)
      checkCounts(pool, free = 0, taken = 1)
      expect_s3_class(obj, "MockPooledObj")

      poolReturn(obj)
      checkCounts(pool, free = 1, taken = 0)
      expect_false(obj$closed)
    })

    it("enforces maxSize", {
      pool <- poolCreate(function() 1, maxSize = 3)

      a <- poolCheckout(pool)
      b <- poolCheckout(pool)
      c <- poolCheckout(pool)
      expect_snapshot(poolCheckout(pool), error = TRUE)
      objs <- list(a, b, c)
      lapply(objs, poolReturn)

      poolClose(pool)
    })

    poolClose(pool)
  })
})

test_that("pool has useful print method", {
  pool <- poolCreate(function() 10)
  on.exit(poolClose(pool))

  expect_snapshot({
    pool

    x1 <- poolCheckout(pool)
    x2 <- poolCheckout(pool)
    pool

    poolReturn(x1)
    pool

    poolReturn(x2)
  })
})

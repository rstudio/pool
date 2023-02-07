test_that("can be created and closed", {
  pool <- poolCreate(function() 1)
  checkCounts(pool, free = 1, taken = 0)
  expect_true(pool$valid)

  poolClose(pool)
  checkCounts(pool, free = 0, taken = 0)
  expect_false(pool$valid)
})

test_that("it can fetch and release", {
  pool <- poolCreate(function() 1)
  withr::defer(poolClose(pool))

  obj <- poolCheckout(pool)
  expect_equal(obj, 1, ignore_attr = TRUE)
  checkCounts(pool, free = 0, taken = 1)

  poolReturn(obj)
  checkCounts(pool, free = 1, taken = 0)
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

test_that("empty pool has useful print method", {
  pool <- poolCreate(function() 10, minSize = 0)
  on.exit(poolClose(pool))

  expect_snapshot({
    pool
  })
})

source("utils.R")

context("Pool scheduling")

describe("pool scheduling", {
  it("schedules things in the right order", {
    results <- integer()
    protectDefaultScheduler({
      scheduleTask(1000, function() {
        results <<- c(results, 3L)
      })
      scheduleTask(100, function() {
        results <<- c(results, 2L)
      })
      scheduleTask(10, function() {
        results <<- c(results, 1L)
      })
    })
    expect_identical(results, 1:3)
  })

  pool <- Pool$new(MockPooledObj$new, 2, 10, 10000)

  it("basic scenarios work", {
    # protectDefaultScheduler is necessary here in order
    # to make sure all the scheduled tasks are executed
    # at the end of the test, not immediately.
    protectDefaultScheduler({
      checkCounts(pool, 2, 0)

      conn1 <- poolCheckout(pool)
      conn2 <- poolCheckout(pool)
      conn3 <- poolCheckout(pool)

      checkCounts(pool, 0, 3)

      poolReturn(conn3)

      checkCounts(pool, 1, 2)

      scheduleTask(9000, function() {
        checkCounts(pool, 1, 2)
      })
      scheduleTask(11000, function() {
        checkCounts(pool, 0, 2)
      })
    })
  })
})

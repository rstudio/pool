source("utils.R")

context("Pool scheduling")

describe("pool scheduler", {

  it("schedules things in the right order", {
    results <- integer()

    # naiveScheduler$protect is necessary here in order
    # to make sure all the scheduled tasks are executed
    # at the end of the test, not immediately.
    naiveScheduler$protect({
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

  pool <- poolCreate(MockPooledObj$new,
    closed = FALSE, valid = TRUE,
    minSize = 2, maxSize = 10, idleTimeout = 10000)

  it("works with pool", {
    naiveScheduler$protect({
      checkCounts(pool, free = 2, taken = 0)

      a <- poolCheckout(pool)
      b <- poolCheckout(pool)
      c <- poolCheckout(pool)

      checkCounts(pool, free = 0, taken = 3)

      ## under the hood, this sets a task for 10000 millis,
      ## so `c` will be destroyed in between the two tasks
      ## below
      poolReturn(c)

      checkCounts(pool, free = 1, taken = 2)

      scheduleTask(9000, function() {
        checkCounts(pool, free = 1, taken = 2)
      })
      scheduleTask(11000, function() {
        checkCounts(pool, free = 0, taken = 2)
      })
    })
    poolReturn(a)
    poolReturn(b)
  })

  poolClose(pool)
})

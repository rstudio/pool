library(R6)

MockPooledObj <- R6Class(
  "MockPooledObj",
  public = list(
    closed = FALSE
  )
)

# Make R6 class available to S4
setClass("MockPooledObj")

setMethod("onDestroy", "MockPooledObj", function(object) {
  if (object$closed)
    stop("onDestroy called twice on the same object")
  object$closed <- TRUE
})

checkCounts <- function(pool, free, taken) {
  if (!missing(free)) {
    expect_identical(pool$counters$free, free)
  }
  if (!missing(taken)) {
    expect_identical(pool$counters$taken, taken)
  }
}

describe("pool", {
  pool <- Pool$new(MockPooledObj$new, 1, 3, 1000)

  it("returns new objects on fetch", {
    fetched <- pool$fetch()
    checkCounts(pool, free = 0, taken = 1)
    expect_is(fetched, "MockPooledObj")

    pool$release(attr(fetched, "id", exact = TRUE), fetched)
    checkCounts(pool, free = 1, taken = 0)
    expect_false(fetched$closed)
  })

  it("supports generic fetch/release", {
    fetched <- poolCheckout(pool)
    checkCounts(pool, free = 0, taken = 1)
    expect_is(fetched, "MockPooledObj")

    poolReturn(fetched)
    checkCounts(pool, free = 1, taken = 0)
    expect_false(fetched$closed)
  })

  it("checks for leaks", {
    poolCheckout(pool)
    ## see lines 146-156 of pool.R to justify both of these changes
    # expect_warning(gc())
    # checkCounts(pool, free = 0, taken = 0)
    print("Without this line, this doesn't work. Isn't it odd?")
    gc()
    checkCounts(pool, free = 1, taken = 0)
  })

  it("enforces maxSize", {
    a <- poolCheckout(pool)
    b <- poolCheckout(pool)
    c <- poolCheckout(pool)
    expect_error(poolCheckout(pool))
  })
})

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

  pool <- Pool$new(MockPooledObj$new, 2, 10,
                   idleTimeout = 10000)

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

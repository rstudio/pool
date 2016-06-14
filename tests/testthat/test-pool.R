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
  object$closed <- TRUE
})

describe("pool", {
  pool <- Pool$new(MockPooledObj$new, 1, 10)

  checkCounts <- function(free, taken, leaked) {
    if (!missing(free)) {
      expect_identical(pool$counters$free, free)
    }
    if (!missing(taken)) {
      expect_identical(pool$counters$taken, taken)
    }
    if (!missing(leaked)) {
      expect_identical(pool$counters$leaked, leaked)
    }
  }

  it("returns new objects on fetch", {
    fetched <- pool$fetch()
    checkCounts(free = 0, taken = 1)
    expect_is(fetched, "MockPooledObj")

    pool$release(attr(fetched, "id", exact = TRUE), fetched)
    checkCounts(free = 1, taken = 0)
    expect_false(fetched$closed)
  })

  it("supports generic fetch/release", {
    fetched <- poolCheckout(pool)
    checkCounts(free = 0, taken = 1)
    expect_is(fetched, "MockPooledObj")

    poolReturn(fetched)
    checkCounts(free = 1, taken = 0)
    expect_false(fetched$closed)
  })

  it("checks for leaks", {
    poolCheckout(pool)
    expect_warning(gc())
    checkCounts(free = 0, taken = 0, leaked = 1)
  })
})

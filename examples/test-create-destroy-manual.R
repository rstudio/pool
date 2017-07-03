library(pool)
library(testthat)

# boilerplate
checkCounters <- function(pool) {
  expect_gte(pool$counters$free, 0)
  expect_gte(pool$counters$taken, 0)
}

checkCounts <- function(pool, free, taken) {
  checkCounters(pool)
  if (!missing(free)) {
    expect_identical(pool$counters$free, free)
  }
  if (!missing(taken)) {
    expect_identical(pool$counters$taken, taken)
  }
}

MockPooledObj <- R6::R6Class("MockPooledObj",
  public = list(
    closed = NULL,
    valid = NULL,
    initialize = function(closed = FALSE, valid = TRUE) {
      self$closed <- closed
      self$valid <- valid
    },
    invalidate = function() self$valid <- FALSE
  )
)

failOnActivate <- FALSE
failOnPassivate <- FALSE
failOnDestroy <- FALSE
failOnValidate <- FALSE

setClass("MockPooledObj")
setMethod("onActivate", "MockPooledObj", function(object) {
  if (failOnActivate) stop("Activation failed...")
})
setMethod("onPassivate", "MockPooledObj", function(object) {
  if (failOnPassivate) stop("Passivation failed...")
})
setMethod("onDestroy", "MockPooledObj", function(object) {
  if (failOnDestroy) stop("Destruction failed...")
  if (object$closed)
    stop("onDestroy called twice on the same object")
  object$closed <- TRUE
})
setMethod("onValidate", "MockPooledObj", function(object, query) {
  if (failOnValidate) stop("Validation failed...")
  if (isTRUE(attr(object, "bad", exact = TRUE))) {
    stop("Bad object")
  }
})

# actual test
pool <- poolCreate(MockPooledObj$new, minSize = 1, maxSize = 3, idleTimeout = 0)

checkCounts(pool, free = 1, taken = 0)

failOnDestroy <- TRUE

a <- poolCheckout(pool)
b <- poolCheckout(pool)

checkCounts(pool, free = 0, taken = 2)

poolReturn(b) # THIS MUST THROW A WARNING

failOnDestroy <- FALSE

checkCounts(pool, free = 0, taken = 1)

poolReturn(a)

checkCounts(pool, free = 1, taken = 0)

poolClose(pool)

library(R6)


#********************************************************************#
#********** Create Mock Pooled object for testing purposes **********#
#********************************************************************#
MockPooledObj <- R6Class("MockPooledObj",
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

# Make R6 class available to S4, and set a few MockPooledObj methods
setClass("MockPooledObj")

setMethod("onDestroy", "MockPooledObj", function(object) {
  if (object$closed)
    stop("onDestroy called twice on the same object")
  object$closed <- TRUE
})

setMethod("onValidate", "MockPooledObj", function(object) {
  object$valid
})



#********************************************************************#
#************************* Utility functions ************************#
#********************************************************************#
checkCounts <- function(pool, free, taken) {
  if (!missing(free)) {
    expect_identical(pool$counters$free, free)
  }
  if (!missing(taken)) {
    expect_identical(pool$counters$taken, taken)
  }
}

checkCounters <- function(pool) {
  expect_gte(pool$counters$free, 0)
  expect_gte(pool$counters$taken, 0)
}

checkShiny <- function() {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    skip("Did not test integration with shiny package")
  }
}

foo <- function (x, ...) UseMethod("foo", x)
foo.default <- function(x, ...) {}

#********************************************************************#
#********** Create Mock Pooled object for testing purposes **********#
#********************************************************************#
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

# Make R6 class available to S4, and set a few MockPooledObj methods
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

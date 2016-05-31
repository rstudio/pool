#' @include pool.R
NULL

#*********************************************************#
#******** set S4 generics for consistency with DBI *******#
#*********************************************************#
#' Releases an object back to the pool.
#' @export
setGeneric("release", function(object) {
  standardGeneric("release")
})

#***************** for internal use only *****************#
#********* should not be called by the end user **********#
setGeneric("onActivate", function(object) {
  standardGeneric("onActivate")
})

setGeneric("onPassivate", function(object) {
  standardGeneric("onPassivate")
})

setGeneric("onDestroy", function(object, envir) {
  standardGeneric("onDestroy")
})

setGeneric("onValidate", function(object) {
  standardGeneric("onValidate")
})

#*********************************************************#
#****************** set method defaults ******************#
#*********************************************************#
#' @export
setMethod("release", "ANY", function(object) {
  id <- attr(object, "id", exact = TRUE)
  pool <- attr(object, "pool", exact = TRUE)
  pool$release(id)
})

#' @export
setMethod("onActivate", "ANY", function(object) {NULL})

#' @export
setMethod("onPassivate", "ANY", function(object) {NULL})

#' @export
setMethod("onDestroy", "ANY", function(object, envir) {NULL})

#' @export
setMethod("onValidate", "ANY", function(object) {NULL})

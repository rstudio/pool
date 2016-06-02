#' @include pool.R
NULL

#' Releases an object back to the pool.
#'
#' Should be called by the end user if they previously fetched an
#' object directly using \code{obj <- pool$fetch()} and are now
#' done with said object.
#'
#' @export
setGeneric("release", function(object) {
  standardGeneric("release")
})

#' @export
setMethod("release", "ANY", function(object) {
  id <- attr(object, "id", exact = TRUE)
  pool <- attr(object, "pool", exact = TRUE)
  pool$release(id)
})

#' Pooled object methods.
#'
#' For backend authors only. Authors should implement all of these,
#' which are then called in the within the Pool class methods. These
#' should not be called directly either by backend authors or by the
#' end users.
#'
#' @name object
NULL

#' @rdname object
setGeneric("onActivate", function(object) {
  standardGeneric("onActivate")
})

#' @rdname object
setGeneric("onPassivate", function(object) {
  standardGeneric("onPassivate")
})

#' @rdname object
setGeneric("onDestroy", function(object, envir) {
  standardGeneric("onDestroy")
})

#' @rdname object
setGeneric("onValidate", function(object) {
  standardGeneric("onValidate")
})

#' @export
setMethod("onActivate", "ANY", function(object) {
  invisible()
})

#' @export
setMethod("onPassivate", "ANY", function(object) {
  invisible()
})

#' @export
setMethod("onDestroy", "ANY", function(object, envir) {
  invisible()
})

#' @export
setMethod("onValidate", "ANY", function(object) {
  invisible()
})

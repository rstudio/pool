#' @include pool.R
NULL

#' Releases an object back to the pool.
#'
#' Should be called by the end user if they previously fetched an
#' object directly using \code{obj <- pool$fetch()} and are now
#' done with said object.
#'
#' @param object A pooled object.
#'
#' @aliases release,ANY-method
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
#' @param object A pooled object.
#'
#' @name object
NULL

#' @aliases onActivate,ANY-method
#' @export
#' @rdname object
setGeneric("onActivate", function(object) {
  standardGeneric("onActivate")
})

#' @aliases onPassivate,ANY-method
#' @export
#' @rdname object
setGeneric("onPassivate", function(object) {
  standardGeneric("onPassivate")
})

#' @aliases onDestroy,ANY-method
#' @export
#' @rdname object
setGeneric("onDestroy", function(object) {
  standardGeneric("onDestroy")
})

#' @aliases onValidate,ANY-method
#' @export
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
setMethod("onDestroy", "ANY", function(object) {
  invisible()
})

#' @export
setMethod("onValidate", "ANY", function(object) {
  invisible()
})

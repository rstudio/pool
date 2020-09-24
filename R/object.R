#' @include pool-methods.R
NULL

#' Pooled object methods.
#'
#' For backend authors only. Authors should implement all of these,
#' which are then called by the Pool class methods. These should
#' not be called directly either by backend authors or by the end
#' users.
#'
#' @keywords internal
#' @param object A pooled object.
#' @name object
NULL

#' @export
#' @rdname object
setGeneric("onActivate", function(object) {
  standardGeneric("onActivate")
})

#' @export
#' @rdname object
setGeneric("onPassivate", function(object) {
  standardGeneric("onPassivate")
})

#' @export
#' @rdname object
setGeneric("onDestroy", function(object) {
  standardGeneric("onDestroy")
})

#' @param query A simple query that can be used to verify that
#' the `object` functions as expected.
#' @export
#' @rdname object
setGeneric("onValidate", function(object, query) {
  standardGeneric("onValidate")
})

#************************ Set defaults ************************#

#' @export
#' @rdname object
setMethod("onActivate", "ANY", function(object) {
  invisible()
})

#' @export
#' @rdname object
setMethod("onPassivate", "ANY", function(object) {
  invisible()
})

#' @export
#' @rdname object
setMethod("onDestroy", "ANY", function(object) {
  invisible()
})

#' @export
#' @rdname object
setMethod("onValidate", "ANY", function(object, query) {
  invisible()
})

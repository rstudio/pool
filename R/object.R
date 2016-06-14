#' @include pool.R
NULL

#' Pooled object methods.
#'
#' For backend authors only. Authors should implement all of these,
#' which are then called by the Pool class methods. These should
#' not be called directly either by backend authors or by the end
#' users.
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

#************************ Set defaults ************************#

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

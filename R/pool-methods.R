#' @include pool.R
NULL

#' Create a pool of reusable objects
#'
#' @description
#' A generic pool class that holds objects. These can be fetched
#' from the pool and released back to it at will, with very
#' little computational cost. The pool should be created only once
#' and closed when it is no longer needed, to prevent leaks.
#'
#' See [dbPool() for an example of object pooling applied to DBI database
#' connections.
#'
#' @export
#' @aliases Pool
setClass("Pool")

#' @export
#' @rdname Pool-class
#' @param factory A factory function used to create responsible for the generation of
#'   the objects that the pool will hold (ex: for DBI database connections,
#'   this function is `dbConnect`). It must take no arguments.
#' @param minSize,maxSize The minimum and maximum number of objects in the pool.
#' @param idleTimeout The number of seconds that an idle
#'   object will be kept in the pool before it is destroyed (only
#'   applies if the number of objects is over the `minSize`).
#'   Use `Inf` if you want created objects never to be destroyed
#'   (there isn't a great reason for this usually).
#' @param validationInterval The minimum number of seconds that
#'  `pool` will wait before running a validation check on the
#'  next checked out object. By not necessarily validating every
#'  checked out object, there can be substantial performance gains
#'  (especially if the interval between checking out new objects is
#'  very small).
#' @param  state A `pool` public variable to be used by
#'  backend authors as necessary.
poolCreate <- function(factory,
                       minSize = 1,
                       maxSize = Inf,
                       idleTimeout = 60,
                       validationInterval = 600,
                       state = NULL) {
  Pool$new(
    factory,
    minSize,
    maxSize,
    idleTimeout,
    validationInterval,
    state
  )
}

#' Checks out an object from the pool.
#'
#' Should be called by the end user if they need a persistent
#' object, that is not returned to the pool automatically.
#' When you don't longer need the object, be sure to return it
#' to the pool using `poolReturn(object)`.
#'
#' @param pool The pool to get the object from.
#' @export
setGeneric("poolCheckout", function(pool) {
  standardGeneric("poolCheckout")
})

#' @rdname poolCheckout
#' @export
setMethod("poolCheckout", "Pool", function(pool) {
  pool$fetch()
})

#' @export
#' @rdname Pool-class
#' @param pool A Pool object previously created with `poolCreate`
setGeneric("poolClose", function(pool) {
  standardGeneric("poolClose")
})

#' @export
#' @rdname Pool-class
setMethod("poolClose", "Pool", function(pool) {
  pool$close()
})

#' Return an object back to the pool
#'
#' Should be called by the end user if they previously fetched
#' an object directly using `object <- poolCheckout(pool)`
#' and are now done with said object.
#'
#' @param object A pooled object.
#' @export
setGeneric("poolReturn", function(object) {
  standardGeneric("poolReturn")
})

#' @export
#' @rdname poolReturn
setMethod("poolReturn", "ANY", function(object) {
  pool_metadata <- attr(object, "pool_metadata", exact = TRUE)
  if (is.null(pool_metadata) || !pool_metadata$valid) {
    stop("Invalid object.")
  }
  pool <- pool_metadata$pool
  pool$release(object)
})

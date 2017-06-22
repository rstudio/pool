#' @include pool.R
NULL

#' S4 class for compatibility with DBI methods
#' @export
setClass("Pool")

## documented manually, with the Pool object
#' @export
poolCreate <- function(factory, minSize = 1, maxSize = Inf,
                       idleTimeout = 60000, validationInterval = 2000,
                       state = NULL) {
  Pool$new(factory, minSize, maxSize,
    idleTimeout, validationInterval, state)
}

#' Checks out an object from the pool.
#'
#' Should be called by the end user if they need a persistent
#' object, that is not returned to the pool automatically.
#' When you don't longer need the object, be sure to return it
#' to the pool using \code{poolReturn(object)}.
#'
#' @param pool The pool to get the object from.
#'
#' @aliases poolCheckout,Pool-method
#' @export
setGeneric("poolCheckout", function(pool) {
  standardGeneric("poolCheckout")
})

#' @export
setMethod("poolCheckout", "Pool", function(pool) {
  pool$fetch()
})

#' Returns an object back to the pool.
#'
#' Should be called by the end user if they previously fetched
#' an object directly using \code{object <- poolCheckout(pool)}
#' and are now done with said object.
#'
#' @param object A pooled object.
#'
#' @aliases poolReturn,ANY-method
#' @export
setGeneric("poolReturn", function(object) {
  standardGeneric("poolReturn")
})

#' @export
setMethod("poolReturn", "ANY", function(object) {
  pool_metadata <- attr(object, "pool_metadata", exact = TRUE)
  if (is.null(pool_metadata) || !pool_metadata$valid) {
    stop("Invalid object.")
  }
  pool <- pool_metadata$pool
  pool$release(object)
})

## documented manually, with the Pool object
#' @export
setGeneric("poolClose", function(pool) {
  standardGeneric("poolClose")
})

#' @export
setMethod("poolClose", "Pool", function(pool) {
  pool$close()
})

#' Show method
#' @param object A Pool object.
#' @export
setMethod("show", "Pool", function(object) {
  pooledObj <- poolCheckout(object)
  on.exit(poolReturn(pooledObj))
  cat("<Pool>\n", "  pooled object class: ",
      is(pooledObj)[1], sep = "")
})

#' Create a pool of reusable objects
#'
#' @description
#' A generic pool class that holds objects. These can be fetched
#' from the pool and released back to it at will, with very
#' little computational cost. The pool should be created only once
#' and closed when it is no longer needed, to prevent leaks.
#'
#' Every usage of `poolCreate()` should always be paired with a call to
#' `poolClose()` to avoid "leaking" resources. In shiny app, you should
#' create the pool outside of the server function and close it on stop,
#' i.e. `onStop(function() pool::poolClose(pool))`.
#'
#' See [dbPool()] for an example of object pooling applied to DBI database
#' connections.
#'
#' @export
#' @aliases Pool
setClass("Pool")

#' @export
#' @rdname Pool-class
#' @param factory A zero-argument function called to create the objects that
#'   the pool will hold (e.g. for DBI database connections, [dbPool()] uses
#'   a wrapper around `DBI::dbConnect()`).
#' @param minSize,maxSize The minimum and maximum number of objects in the pool.
#' @param idleTimeout Number of seconds to wait before destroying idle objects
#'   (i.e. objects available for checkout over and above `minSize`).
#' @param validationInterval Number of seconds to wait between validating
#'   objects that are available for checkout. These objects are validated
#'   in the background to keep them alive.
#'
#'   To force objects to be validated on every checkout, set
#'   `validationInterval = 0`.
#' @param  state A `pool` public variable to be used by backend authors.
poolCreate <- function(factory,
                       minSize = 1,
                       maxSize = Inf,
                       idleTimeout = 60,
                       validationInterval = 60,
                       state = NULL) {
  Pool$new(
    factory,
    minSize,
    maxSize,
    idleTimeout,
    validationInterval,
    state,
    error_call = current_env()
  )
}

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

#' Check out and return object from the pool
#'
#' @description
#' Use `poolCheckout()` to check out an object from the pool and
#' `poolReturn()` to return it. You will receive a warning if all objects
#' aren't returned before the pool is closed.
#'
#' `localCheckout()` is a convenience function that can be used inside
#' functions (and other function-scoped operations like `shiny::reactive()`
#' and `local()`). It checks out an object and automatically returns it when
#' the function exits
#'
#' Note that validation is only performed when the object is checked out,
#' so you generally want to keep the checked out around for as little time as
#' possible.
#' 
#' When pooling DBI database connections, you normally would not use
#' `poolCheckout()`. Instead, for single-shot queries, treat the pool object
#' itself as the DBI connection object and it will perform checkout/return for
#' you. And for transactions, use [poolWithTransaction()]. See [dbPool()] for
#' an example.
#'
#' @param pool The pool to get the object from.
#' @export
#' @examples
#' pool <- dbPool(RSQLite::SQLite())
#' # For illustration only. You normally would not explicitly use
#' # poolCheckout with a DBI connection pool (see Description).
#' con <- poolCheckout(pool)
#' con
#' poolReturn(con)
#'
#' f <- function() {
#'   con <- localCheckout(pool)
#'   # do something ...
#' }
#' f()
#'
#' poolClose(pool)
setGeneric("poolCheckout", function(pool) {
  standardGeneric("poolCheckout")
})

#' @rdname poolCheckout
#' @export
setMethod("poolCheckout", "Pool", function(pool) {
  pool$fetch()
})

#' @rdname poolCheckout
#' @param object Object to return
#' @export
setGeneric("poolReturn", function(object) {
  standardGeneric("poolReturn")
})

#' @export
#' @rdname poolCheckout
setMethod("poolReturn", "ANY", function(object) {
  pool <- pool_metadata(object)$pool
  pool$release(object)
})

#' @export
#' @rdname poolCheckout
#' @param env Environment corresponding to the execution frame. For expert
#'   use only.
localCheckout <- function(pool, env = parent.frame()) {
  obj <- poolCheckout(pool)
  defer(poolReturn(obj), envir = env)
  obj
}

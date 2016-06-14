#' @include DBI.R
NULL

#' Fetch a connection from the pool
#'
#' Fetch a DBIConnection object from the pool (a wrapper around
#' \code{poolCheckout(pool)}). This method should be used with
#' care, since this puts the onus of releasing the connection
#' on you (the user). While this means that you still enjoy the
#' performance benefits associated with having a pool, you lose
#' the benefits of automatic connection management, since you
#' are now yourself responsible for releasing the connection
#' (using \code{poolReturn(conn)} or \code{dbDisconnect(conn)})
#' at the appropriate time.
#'
#' @param drv The pool object to fetch the connection from.
#' @param ... Not in use.
#'
#' @export
setMethod("dbConnect", "Pool", function(drv, ...) {
  drv$fetch()
})

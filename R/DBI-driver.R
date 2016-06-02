#' @include DBI.R
NULL

#' Fetch a connection from the pool
#'
#' Fetch a DBIConnection object from the pool. This method should
#' be used with care, since this puts the onus of releasing the
#' connection on you (the user). While this means that you still
#' enjoy the performance benefits associated with having a pool,
#' you lose the benefits of automatic connection management, since
#' you are now yourself responsible for releasing the connection
#' (using \code{release(conn)}) at the appropriate time.
#'
#' @export
setMethod("dbConnect", "Pool", function(drv, ...) {
  drv$fetch()
})

#' List currently open connections.
#'
#' Pool object wrapper around the original DBI method for
#' DBIDriver objects. See \code{\link[DBI]{dbListConnections}} for
#' the original documentation.
#'
#' @export
setMethod("dbListConnections", "Pool", function(drv, ...) {
  list()  #### CHANGE THIS! (once public Pool counters exist)
})

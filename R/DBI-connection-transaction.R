#' @include DBI.R
NULL

#' DBIConnection transaction methods are not supported for
#' Pool objects.
#'
#' You cannot perfrom SQL transaction using a Pool object directly
#' (because that would imply keeping a connection open and not
#' knowing when to return it back to the pool).
#'
#' If you must use these methods, fetch an actual connection first
#' with \code{conn <- poolCheckout(pool)} -- then call the appropriate
#' DBI method on \code{conn}. Since you're fetching a connection
#' from the pool yourself, you must also remember to return it
#' back to the pool when you're done: \code{poolReturn(conn)}
#' (otherwise, you have a leaked connection).
#'
#' For simple transactions, consider using
#' \code{\link[DBI]{dbWithTransaction}} instead, which is safer
#' since it does not require you to fetch and release the
#' connection yourself.
#'
#' See \code{\link[DBI]{transactions}} for the original
#' documentation.
#'
#' @param conn,...,code See \code{\link[DBI]{transactions}}.
#'
#' @name DBI-connection-transaction
NULL

#' @export
#' @rdname DBI-connection-transaction
setMethod("dbBegin", "Pool", function(conn, ...) {
  stop("Must use `conn <- poolCheckout(pool); dbBegin(conn, ...)` ",
       "instead. Remember to `poolReturn(conn)` when `conn` is ",
       "no longer necessary. Consider using `dbWithTransaction(...)` ",
       "for simple transactions.")
})

#' @export
#' @rdname DBI-connection-transaction
setMethod("dbCommit", "Pool", function(conn, ...) {
  stop("Must use `conn <- poolCheckout(pool); dbCommit(conn, ...)` ",
       "instead. Remember to `poolReturn(conn)` when `conn` is ",
       "no longer necessary. Consider using `dbWithTransaction(...)` ",
       "for simple transactions.")
})

#' @export
#' @rdname DBI-connection-transaction
setMethod("dbRollback", "Pool", function(conn, ...) {
  stop("Must use `conn <- poolCheckout(pool); dbRollback(conn, ...)` ",
       "instead. Remember to `poolReturn(conn)` when `conn` is ",
       "no longer necessary. Consider using `dbWithTransaction(...)` ",
       "for simple transactions.")
})

#' @export
#' @rdname DBI-connection-transaction
setMethod("dbWithTransaction", "Pool", function(conn, code) {
  stop("Must use `conn <- poolCheckout(pool); dbRollback(conn, ...)` ",
       "instead. Remember to `poolReturn(conn)` when `conn` is ",
       "no longer necessary. Consider using `dbWithTransaction(...)` ",
       "for simple transactions.")
})

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
#' with \code{conn <- pool$fetch()} -- then call the appropriate
#' DBI method on \code{conn}. Since you're fetching a connection
#' from the pool yourself, you must also remember to release it
#' back to the pool when you're done: \code{release(conn)}
#' (otherwise, you have a leaked connection).
#'
#' For simple transactions, consider using \code{withTransaction}
#' instead, which is safer since it does not require you to fetch
#' and release the connection yourself.
#'
#' See \code{\link[DBI]{transactions}} for the original
#' documentation.
#'
#' @param conn,... See \code{\link[DBI]{transactions}}.
#'
#' @name DBI-connection-transaction
NULL

#' @export
#' @rdname DBI-connection-transaction
setMethod("dbBegin", "Pool", function(conn, ...) {
  stop("Must use `conn <- pool$fetch(); dbBegin(conn, ...)` ",
       "instead. Remember to `release(conn)` when `conn` is ",
       "no longer necessary. Consider using `withTransaction(...)` ",
       "for simple transactions.")
})

#' @export
#' @rdname DBI-connection-transaction
setMethod("dbCommit", "Pool", function(conn, ...) {
  stop("Must use `conn <- pool$fetch(); dbCommit(conn, ...)` ",
       "instead. Remember to `release(conn)` when `conn` is ",
       "no longer necessary. Consider using `withTransaction(...)` ",
       "for simple transactions.")
})

#' @export
#' @rdname DBI-connection-transaction
setMethod("dbRollback", "Pool", function(conn, ...) {
  stop("Must use `conn <- pool$fetch(); dbRollback(conn, ...)` ",
       "instead. Remember to `release(conn)` when `conn` is ",
       "no longer necessary. Consider using `withTransaction(...)` ",
       "for simple transactions.")
})

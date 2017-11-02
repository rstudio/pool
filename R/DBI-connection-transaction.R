#' @include DBI.R
NULL

#' DBIConnection transaction methods are not supported for
#' Pool objects.
#'
#' You cannot perform SQL transaction using a Pool object directly
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
#' \code{\link{poolWithTransaction}} instead,
#' which is safer since it does not require you to fetch and
#' release the connection yourself.
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
       "no longer necessary. Consider using `poolWithTransaction(...)` ",
       "for simple transactions.")
})

#' @export
#' @rdname DBI-connection-transaction
setMethod("dbCommit", "Pool", function(conn, ...) {
  stop("Must use `conn <- poolCheckout(pool); dbCommit(conn, ...)` ",
       "instead. Remember to `poolReturn(conn)` when `conn` is ",
       "no longer necessary. Consider using `poolWithTransaction(...)` ",
       "for simple transactions.")
})

#' @export
#' @rdname DBI-connection-transaction
setMethod("dbRollback", "Pool", function(conn, ...) {
  stop("Must use `conn <- poolCheckout(pool); dbRollback(conn, ...)` ",
       "instead. Remember to `poolReturn(conn)` when `conn` is ",
       "no longer necessary. Consider using `poolWithTransaction(...)` ",
       "for simple transactions.")
})


#' @export
#' @rdname DBI-connection-transaction
setMethod("dbWithTransaction", "Pool", function(conn, code) {
  stop("Must use `conn <- poolCheckout(pool); dbWithTransaction(conn, ...)` ",
       "instead. Remember to `poolReturn(conn)` when `conn` is ",
       "no longer necessary. Consider using `poolWithTransaction(...)` ",
       "for simple transactions.")
})


#' Self-contained database transactions using pool
#'
#' This function allows you to use a pool object directly to execute a
#' transaction on a database connection, without ever having to actually
#' check out a connection from the pool and then return it. Using this
#' function instead of the direct transaction methods will guarantee that
#' you don't leak connections or forget to commit/rollback a transaction.
#'
#' This function is similar to \code{\link[DBI]{dbWithTransaction}}, but
#' its arguments work a little differently. First, it takes in a pool
#' object, instead of a connection. Second, instead of taking an arbitrary
#' chunk of code to execute as a transaction (i.e. either run all the
#' commands successfully or not run any of them), it takes in a function.
#' This function (the \code{func} argument) gives you an argument to use
#' in its body, a database connection. So, you can use connection methods
#' without ever having to check out a connection. But you can also use
#' arbitrary R code inside the \code{func}'s body. This function will be
#' called once we fetch a connection from the pool. Once the function
#' returns, we release the connection back to the pool.
#'
#' Like its DBI sister \code{\link[DBI]{dbWithTransaction}}, this function
#' calls \code{dbBegin()} before executing the code, and \code{dbCommit()}
#' after successful completion, or \code{dbRollback()} in case of an error.
#' This means that calling \code{poolWithTransaction} always has side
#' effects, namely to commit or roll back the code executed when \code{func}
#' is called. In addition, if you modify the local R environment from within
#' \code{func} (e.g. setting global variables, writing to disk), these
#' changes will persist after the function has returned.
#'
#' Also, like \code{\link[DBI]{dbWithTransaction}}, there is also a special
#' function called \code{dbBreak()} that allows for an early, silent exit
#' with rollback. It can be called only from inside \code{poolWithTransaction}.
#'
#' @param pool The pool object to fetch the connection from.
#' @param func A function that has one argument, \code{conn} (a database
#'   connection checked out from \code{pool}).
#'
#' @return \code{func}'s return value.
#' @export
#' @examples
#' pool <- dbPool(RSQLite::SQLite(), dbname = ":memory:")
#'
#' dbWriteTable(pool, "cars", head(cars, 3))
#' dbReadTable(pool, "cars")   # there are 3 rows
#'
#' ## successful transaction
#' poolWithTransaction(pool, function(conn) {
#'   dbExecute(conn, "INSERT INTO cars (speed, dist) VALUES (1, 1);")
#'   dbExecute(conn, "INSERT INTO cars (speed, dist) VALUES (2, 2);")
#'   dbExecute(conn, "INSERT INTO cars (speed, dist) VALUES (3, 3);")
#' })
#' dbReadTable(pool, "cars")   # there are now 6 rows
#'
#' ## failed transaction -- note the missing comma
#' tryCatch(
#'   poolWithTransaction(pool, function(conn) {
#'     dbExecute(conn, "INSERT INTO cars (speed, dist) VALUES (1, 1);")
#'     dbExecute(conn, "INSERT INTO cars (speed dist) VALUES (2, 2);")
#'     dbExecute(conn, "INSERT INTO cars (speed, dist) VALUES (3, 3);")
#'   }),
#'   error = identity
#' )
#' dbReadTable(pool, "cars")   # still 6 rows
#'
#' ## early exit, silently
#' poolWithTransaction(pool, function(conn) {
#'   dbExecute(conn, "INSERT INTO cars (speed, dist) VALUES (1, 1);")
#'   dbExecute(conn, "INSERT INTO cars (speed, dist) VALUES (2, 2);")
#'   if (nrow(dbReadTable(conn, "cars")) > 7) dbBreak()
#'   dbExecute(conn, "INSERT INTO cars (speed, dist) VALUES (3, 3);")
#' })
#' dbReadTable(pool, "cars")   # still 6 rows
#'
#' poolClose(pool)
poolWithTransaction <- function(pool, func) {
  conn <- poolCheckout(pool)
  on.exit(poolReturn(conn))
  DBI::dbWithTransaction(conn, func(conn))
}


#' @export
#' @rdname poolWithTransaction
dbBreak <- DBI::dbBreak

#' Self-contained database transactions using pool
#'
#' This function allows you to use a pool object directly to execute a
#' transaction on a database connection, without ever having to actually
#' check out a connection from the pool and then return it. Using this
#' function instead of the direct transaction methods will guarantee that
#' you don't leak connections or forget to commit/rollback a transaction.
#'
#' This function is similar to [DBI::dbWithTransaction()], but
#' its arguments work a little differently. First, it takes in a pool
#' object, instead of a connection. Second, instead of taking an arbitrary
#' chunk of code to execute as a transaction (i.e. either run all the
#' commands successfully or not run any of them), it takes in a function.
#' This function (the `func` argument) gives you an argument to use
#' in its body, a database connection. So, you can use connection methods
#' without ever having to check out a connection. But you can also use
#' arbitrary R code inside the `func`'s body. This function will be
#' called once we fetch a connection from the pool. Once the function
#' returns, we release the connection back to the pool.
#'
#' Like its DBI sister [DBI::dbWithTransaction()], this function
#' calls `dbBegin()` before executing the code, and `dbCommit()`
#' after successful completion, or `dbRollback()` in case of an error.
#' This means that calling `poolWithTransaction` always has side
#' effects, namely to commit or roll back the code executed when `func`
#' is called. In addition, if you modify the local R environment from within
#' `func` (e.g. setting global variables, writing to disk), these
#' changes will persist after the function has returned.
#'
#' Also, like [DBI::dbWithTransaction()], there is also a special
#' function called `dbBreak()` that allows for an early, silent exit
#' with rollback. It can be called only from inside `poolWithTransaction`.
#'
#' @param pool The pool object to fetch the connection from.
#' @param func A function that has one argument, `conn` (a database
#'   connection checked out from `pool`).
#'
#' @return `func`'s return value.
#' @export
#' @examples
#' if (requireNamespace("RSQLite", quietly = TRUE)) {
#'   pool <- dbPool(RSQLite::SQLite(), dbname = ":memory:")
#'
#'   dbWriteTable(pool, "cars", head(cars, 3))
#'   dbReadTable(pool, "cars")   # there are 3 rows
#'
#'   ## successful transaction
#'   poolWithTransaction(pool, function(conn) {
#'     dbExecute(conn, "INSERT INTO cars (speed, dist) VALUES (1, 1);")
#'     dbExecute(conn, "INSERT INTO cars (speed, dist) VALUES (2, 2);")
#'     dbExecute(conn, "INSERT INTO cars (speed, dist) VALUES (3, 3);")
#'   })
#'   dbReadTable(pool, "cars")   # there are now 6 rows
#'
#'   ## failed transaction -- note the missing comma
#'   tryCatch(
#'     poolWithTransaction(pool, function(conn) {
#'       dbExecute(conn, "INSERT INTO cars (speed, dist) VALUES (1, 1);")
#'       dbExecute(conn, "INSERT INTO cars (speed dist) VALUES (2, 2);")
#'       dbExecute(conn, "INSERT INTO cars (speed, dist) VALUES (3, 3);")
#'     }),
#'     error = identity
#'   )
#'   dbReadTable(pool, "cars")   # still 6 rows
#'
#'   ## early exit, silently
#'   poolWithTransaction(pool, function(conn) {
#'     dbExecute(conn, "INSERT INTO cars (speed, dist) VALUES (1, 1);")
#'     dbExecute(conn, "INSERT INTO cars (speed, dist) VALUES (2, 2);")
#'     if (nrow(dbReadTable(conn, "cars")) > 7) dbBreak()
#'     dbExecute(conn, "INSERT INTO cars (speed, dist) VALUES (3, 3);")
#'   })
#'   dbReadTable(pool, "cars")   # still 6 rows
#'
#'   poolClose(pool)
#'
#' } else {
#'   message("Please install the 'RSQLite' package to run this example")
#' }
poolWithTransaction <- function(pool, func) {
  conn <- localCheckout(pool)
  DBI::dbWithTransaction(conn, func(conn))
}

#' @export
DBI::dbBreak

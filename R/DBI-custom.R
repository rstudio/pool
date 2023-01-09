#' DBI methods
#'
#' @description
#' Pool methods for DBI generics generally check out a connection
#' (with [poolCheckout()]), perform the operation, and the return the connection
#' to the pool (with [poolReturn()]). This page describes the exceptions.
#'
#' Pool cannot implement the [DBI::dbSendQuery()] and [DBI::dbSendStatement()]
#' methods because they both return result sets that are inextricably bound
#' to a specific connection, and there's no guarantee that connection will
#' still be alive once you retrieve the results. Instead use [DBI::dbGetQuery()]
#' and [DBI::dbExecute()].
#'
#' Similarly, pool cannot implement the [DBI::dbBegin()], [DBI::dbRollback()],
#' or [DBI::dbCommit()] generics because there's no guarantee that you'll get
#' the same connection from call to call. Instead use [poolWithTransaction()].
#'
#' @param conn,dbObj A Pool object, as returned from [dbPool()].
#' @param statement,code,... See DBI documentation.
#' @name DBI-custom
#' @examples
#' if (requireNamespace("RSQLite", quietly = TRUE)) {
#'   mtcars1 <- mtcars[ c(1:16), ] # first half of the mtcars dataset
#'   mtcars2 <- mtcars[-c(1:16), ] # second half of the mtcars dataset
#'
#'   pool <- dbPool(RSQLite::SQLite(), dbname = ":memory:")
#'
#'   # write the mtcars1 table into the database
#'   dbWriteTable(pool, "mtcars", mtcars1, row.names = TRUE)
#'
#'   # list the current tables in the database
#'   dbListTables(pool)
#'
#'   # read the "mtcars" table from the database (only 16 rows)
#'   dbReadTable(pool, "mtcars")
#'
#'   # append mtcars2 to the "mtcars" table already in the database
#'   dbWriteTable(pool, "mtcars", mtcars2, row.names = TRUE, append = TRUE)
#'
#'   # read the "mtcars" table from the database (all 32 rows)
#'   dbReadTable(pool, "mtcars")
#'
#'   # get the names of the columns in the databases's table
#'   dbListFields(pool, "mtcars")
#'
#'   # use dbExecute to change the "mpg" and "cyl" values of the 1st row
#'   dbExecute(pool,
#'     paste(
#'       "UPDATE mtcars",
#'       "SET mpg = '22.0', cyl = '10'",
#'       "WHERE row_names = 'Mazda RX4'"
#'     )
#'   )
#'
#'   # read the 1st row of "mtcars" table to confirm the previous change
#'   dbGetQuery(pool, "SELECT * FROM mtcars WHERE row_names = 'Mazda RX4'")
#'
#'   # drop the "mtcars" table from the database
#'   dbRemoveTable(pool, "mtcars")
#'
#'   # list the current tables in the database
#'   dbListTables(pool)
#'
#'   poolClose(pool)
#'
#' } else {
#'   message("Please install the 'RSQLite' package to run this example")
#' }
NULL

setClass("Pool")

## Throw error here since this would require keeping a connection
## open and never releasing it back to the pool.
#' @export
#' @rdname DBI-custom
setMethod("dbSendQuery", "Pool", function(conn, statement, ...) {
  stop("Pool does not implement the dbSendQuery() method. Either use dbGetQuery(), or `conn <- poolCheckout(pool); dbSendQuery(conn, ...)` (and call `poolReturn(conn)` when finished).")
})

#' @export
#' @rdname DBI-custom
setMethod("dbSendStatement", "Pool", function(conn, statement, ...) {
  stop("Pool does not implement the dbSendStatement() method. Use dbExecute() instead.")
})

#' @export
#' @rdname DBI-custom
setMethod("dbGetInfo", "Pool", function(dbObj, ...) {
  pooledObj <- poolCheckout(dbObj)
  on.exit(poolReturn(pooledObj))
  list(class = is(dbObj),
       valid = dbObj$valid,
       minSize = dbObj$minSize,
       maxSize = dbObj$maxSize,
       idleTimeout = dbObj$idleTimeout,
       pooledObjectClass = is(pooledObj)[1],
       numberFreeObjects = dbObj$counters$free,
       numberTakenObjects = dbObj$counters$taken)
})


#' @export
#' @rdname DBI-custom
setMethod("dbBegin", "Pool", function(conn, ...) {
  stop("Must use `conn <- poolCheckout(pool); dbBegin(conn, ...)` ",
       "instead. Remember to `poolReturn(conn)` when `conn` is ",
       "no longer necessary. Consider using `poolWithTransaction(...)` ",
       "for simple transactions.")
})

#' @export
#' @rdname DBI-custom
setMethod("dbCommit", "Pool", function(conn, ...) {
  stop("Must use `conn <- poolCheckout(pool); dbCommit(conn, ...)` ",
       "instead. Remember to `poolReturn(conn)` when `conn` is ",
       "no longer necessary. Consider using `poolWithTransaction(...)` ",
       "for simple transactions.")
})

#' @export
#' @rdname DBI-custom
setMethod("dbRollback", "Pool", function(conn, ...) {
  stop("Must use `conn <- poolCheckout(pool); dbRollback(conn, ...)` ",
       "instead. Remember to `poolReturn(conn)` when `conn` is ",
       "no longer necessary. Consider using `poolWithTransaction(...)` ",
       "for simple transactions.")
})


#' @export
#' @rdname DBI-custom
setMethod("dbWithTransaction", "Pool", function(conn, code) {
  stop("Must use `conn <- poolCheckout(pool); dbWithTransaction(conn, ...)` ",
       "instead. Remember to `poolReturn(conn)` when `conn` is ",
       "no longer necessary. Consider using `poolWithTransaction(...)` ",
       "for simple transactions.")
})

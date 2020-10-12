#' @include DBI.R
NULL

#' DBIConnection methods.
#'
#' As a convenience, Pool implements DBIConnection methods; calling any implemented
#' DBI method directly on a Pool object will result in a connection being checked
#' out (with [poolCheckout()]), the operation being performed on that connection,
#' and the connection being returned to the pool (with [poolReturn()]).
#'
#' Pool cannot implement the [DBI::dbSendQuery()] and [DBI::dbSendStatement()]
#' methods because they both return live ResultSet objects. This is incompatible
#' with the Pool model, because once a connection is returned to the pool, using
#' an existing ResultSet object could give erroneous results, throw an error, or
#' even crash the entire R process. In most cases, [DBI::dbGetQuery()] and
#' [DBI::dbExecute()] can be used instead. If you really need the control that
#' `dbSendQuery` gives you (for example, to process a large table in chunks)
#' then use `poolCheckout()` to get a real connection object (and don't forget
#' to return it to the pool using `poolReturn()` afterwards).
#'
#' @param conn,dbObj A Pool object, as returned from [dbPool()].
#' @param statement,name,value,prefix,fields,row.names,temporary,... See DBI
#'   documentation.
#'
#' @name DBI-connection
#' @seealso For the original documentation, see:
#'   * [DBI::dbSendQuery()] (not implemented by Pool)
#'   * [DBI::dbSendStatement()] (not implemented by Pool)
#'   * [DBI::dbAppendTable()]
#'   * [DBI::dbCreateTable()]
#'   * [DBI::dbGetQuery()]
#'   * [DBI::dbExecute()]
#'   * [DBI::dbIsReadOnly()]
#'   * [DBI::dbListFields()]
#'   * [DBI::dbListObjects()]
#'   * [DBI::dbListResults()]
#'   * [DBI::dbListTables()]
#'   * [DBI::dbReadTable()]
#'   * [DBI::dbWriteTable()]
#'   * [DBI::dbExistsTable()]
#'   * [DBI::dbRemoveTable()]
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

## Throw error here since this would require keeping a connection
## open and never releasing it back to the pool.
#' @export
#' @rdname DBI-connection
setMethod("dbSendQuery", "Pool", function(conn, statement, ...) {
  stop("Pool does not implement the dbSendQuery() method. Either use dbGetQuery(), or `conn <- poolCheckout(pool); dbSendQuery(conn, ...)` (and call `poolReturn(conn)` when finished).")
})

#' @export
#' @rdname DBI-connection
setMethod("dbSendStatement", "Pool", function(conn, statement, ...) {
  stop("Pool does not implement the dbSendStatement() method. Use dbExecute() instead.")
})

## Always use this, except if dealing with transactions that
## cannot be dealt with using dbWithTransaction(...)
#' @export
#' @rdname DBI-connection
setMethod("dbGetQuery", signature("Pool", "character"), function(conn, statement, ...) {
  connection <- poolCheckout(conn)
  on.exit(poolReturn(connection))
  DBI::dbGetQuery(connection, statement, ...)
})

## Analogous to dbGetQuery(), but for non-SELECT statements
#' @export
#' @rdname DBI-connection
setMethod("dbExecute", signature("Pool", "character"), function(conn, statement, ...) {
  connection <- poolCheckout(conn)
  on.exit(poolReturn(connection))
  DBI::dbExecute(connection, statement, ...)
})

#' @export
#' @rdname DBI-connection
setMethod("dbListResults", "Pool", function(conn, ...) {
  list()
})

#' @export
#' @rdname DBI-connection
setMethod("dbListFields", signature("Pool", "character"), function(conn, name, ...) {
  connection <- poolCheckout(conn)
  on.exit(poolReturn(connection))
  DBI::dbListFields(connection, name, ...)
})

#' @export
#' @rdname DBI-connection
setMethod("dbListTables", "Pool", function(conn, ...) {
  connection <- poolCheckout(conn)
  on.exit(poolReturn(connection))
  DBI::dbListTables(connection)
})

#' @export
#' @rdname DBI-connection
setMethod("dbListObjects", "Pool", function(conn, prefix = NULL, ...) {
  connection <- poolCheckout(conn)
  on.exit(poolReturn(connection))
  DBI::dbListObjects(connection, prefix = prefix, ...)
})

#' @export
#' @rdname DBI-connection
setMethod("dbReadTable", signature("Pool", "character"), function(conn, name, ...) {
  connection <- poolCheckout(conn)
  on.exit(poolReturn(connection))
  DBI::dbReadTable(connection, name, ...)
})

#' @export
#' @rdname DBI-connection
setMethod("dbWriteTable", "Pool", function(conn, name, value, ...) {
  connection <- poolCheckout(conn)
  on.exit(poolReturn(connection))
  DBI::dbWriteTable(connection, name, value, ...)
})

#' @export
#' @rdname DBI-connection
setMethod("dbCreateTable", "Pool", function(conn, name, fields, ..., row.names = NULL, temporary = FALSE) {
  connection <- poolCheckout(conn)
  on.exit(poolReturn(connection))
  DBI::dbCreateTable(connection, name, fields, ..., row.names = row.names, temporary = temporary)
})

#' @export
#' @rdname DBI-connection
setMethod("dbAppendTable", "Pool", function(conn, name, value, ..., row.names = NULL) {
  connection <- poolCheckout(conn)
  on.exit(poolReturn(connection))
  DBI::dbAppendTable(connection, name, value, ..., row.names = row.names)
})

#' @export
#' @rdname DBI-connection
setMethod("dbExistsTable", "Pool", function(conn, name, ...) {
  connection <- poolCheckout(conn)
  on.exit(poolReturn(connection))
  DBI::dbExistsTable(connection, name, ...)
})

#' @export
#' @rdname DBI-connection
setMethod("dbRemoveTable", "Pool", function(conn, name, ...) {
  connection <- poolCheckout(conn)
  on.exit(poolReturn(connection))
  DBI::dbRemoveTable(connection, name, ...)
})

#' @export
#' @rdname DBI-connection
setMethod("dbIsReadOnly", "Pool", function(dbObj, ...) {
  connection <- poolCheckout(dbObj)
  on.exit(poolReturn(connection))
  DBI::dbIsReadOnly(connection, ...)
})

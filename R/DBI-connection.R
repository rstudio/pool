#' @include DBI.R
NULL

#' DBIConnection methods.
#'
#' Pool object wrappers around DBIConnection methods. For the original
#' documentation, see:
#' \itemize{
#'  \item [DBI::dbSendQuery()]
#'  \item [DBI::dbGetQuery()]
#'  \item [DBI::dbExecute()]
#'  \item [DBI::dbListResults()]
#'  \item [DBI::dbListFields()]
#'  \item [DBI::dbListTables()]
#'  \item [DBI::dbReadTable()]
#'  \item [DBI::dbWriteTable()]
#'  \item [DBI::dbExistsTable()]
#'  \item [DBI::dbRemoveTable()]
#' }
#'
#' @name DBI-connection
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
#' @param conn,statement,... See [DBI::dbSendQuery()].
#' @export
#' @rdname DBI-connection
setMethod("dbSendQuery", "Pool", function(conn, statement, ...) {
  stop("Must use `conn <- poolCheckout(pool); dbSendQuery(conn, statement, ...)` ",
       "instead. Remember to `poolReturn(conn)` when `conn` is no longer ",
       "necessary.")
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
setMethod("dbReadTable", signature("Pool", "character"), function(conn, name, ...) {
  connection <- poolCheckout(conn)
  on.exit(poolReturn(connection))
  DBI::dbReadTable(connection, name, ...)
})

#' @param name,value See [DBI::dbWriteTable()].
#' @export
#' @rdname DBI-connection
setMethod("dbWriteTable", "Pool", function(conn, name, value, ...) {
  connection <- poolCheckout(conn)
  on.exit(poolReturn(connection))
  DBI::dbWriteTable(connection, name, value, ...)
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

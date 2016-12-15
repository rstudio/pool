#' @include DBI.R
NULL

#' DBIConnection methods.
#'
#' Pool object wrappers around DBIConnection methods. For the original
#' documentation, see:
#' \itemize{
#'  \item \code{\link[DBI]{dbSendQuery}}
#'  \item \code{\link[DBI]{dbGetQuery}}
#'  \item \code{\link[DBI]{dbListResults}}
#'  \item \code{\link[DBI]{dbListFields}}
#'  \item \code{\link[DBI]{dbListTables}}
#'  \item \code{\link[DBI]{dbReadTable}}
#'  \item \code{\link[DBI]{dbWriteTable}}
#'  \item \code{\link[DBI]{dbExistsTable}}
#'  \item \code{\link[DBI]{dbRemoveTable}}
#' }
#'
#' @name DBI-connection
NULL

## Throw error here since this would require keeping a connection
## open and never releasing it back to the pool.
#' @param conn,statement,... See \code{\link[DBI]{dbSendQuery}}.
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
setMethod("dbGetQuery", "Pool", function(conn, statement, ...) {
  connection <- poolCheckout(conn)
  on.exit(poolReturn(connection))
  DBI::dbGetQuery(connection, statement, ...)
})

## Analogous to dbGetQuery(), but for non-SELECT statements
#' @export
#' @rdname DBI-connection
setMethod("dbExecute", "Pool", function(conn, statement, ...) {
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
setMethod("dbListFields", "Pool", function(conn, name, ...) {
  connection <- poolCheckout(conn)
  on.exit(poolReturn(connection))
  DBI::dbListFields(connection, name)
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
setMethod("dbReadTable", "Pool", function(conn, name, ...) {
  connection <- poolCheckout(conn)
  on.exit(poolReturn(connection))
  DBI::dbReadTable(connection, name, ...)
})

#' @param name,value See \code{\link[DBI]{dbWriteTable}}.
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

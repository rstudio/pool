#' @include DBI.R
NULL

#' Creating and manipulating SQL tables.
#'
#' Pool object wrappers around DBIConnection methods that deal
#' with the creation and manipulation of SQL tables. See
#' \code{\link[DBI]{sqlData}}, \code{\link[DBI]{sqlCreateTable}}
#' and \code{\link[DBI]{sqlAppendTable}} for the original
#' documentation.
#'
#' @name DBI-connection-sql
NULL

#' @export
#' @rdname DBI-connection-sql
setMethod("sqlData", "Pool", function(con, value, row.names = NA, ...) {
  connection <- con$fetch()
  on.exit(release(connection))
  DBI::sqlData(connection, value, row.names = NA, ...)
})

#' @export
#' @rdname DBI-connection-sql
setMethod("sqlCreateTable", "Pool",
  function(con, table, fields, row.names = NA, temporary = FALSE, ...) {
    connection <- con$fetch()
    on.exit(release(connection))
    DBI::sqlCreateTable(connection, table, fields,
                        row.names, temporary, ...)
  }
)

#' @export
#' @rdname DBI-connection-sql
setMethod("sqlAppendTable", "Pool",
  function(con, table, values, row.names = NA, ...) {
    connection <- con$fetch()
    on.exit(release(connection))
    DBI::sqlAppendTable(connection, table, values, row.names, ...)
  }
)

#' @export
#' @rdname DBI-connection-sql
sqlAppendTableTemplate <- function(obj, table, values,
                                   row.names = NA, prefix = "?", ...) {
  if (inherits(obj, "DBIConnection")) {
    DBI::sqlAppendTableTemplate(obj, table, values,
                                row.names, prefix, ...)
  }
  else if (inherits(obj, "Pool")) {
    connection <- obj$fetch()
    on.exit(release(connection))
    DBI::sqlAppendTableTemplate(connection, table, values,
                                row.names, prefix, ...)
  } else {
    stop("The class of `obj` must be either 'DBIConnection'",
         "or 'Pool'.")
  }
}

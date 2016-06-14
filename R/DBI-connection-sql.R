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

#' @param con,value,row.names,... See \code{\link[DBI]{sqlData}}.
#' @export
#' @rdname DBI-connection-sql
setMethod("sqlData", "Pool", function(con, value, row.names = NA, ...) {
  connection <- con$fetch()
  on.exit(poolReturn(connection))
  DBI::sqlData(connection, value, row.names = NA, ...)
})

#' @param table,fields,temporary See \code{\link[DBI]{sqlCreateTable}}.
#' @export
#' @rdname DBI-connection-sql
setMethod("sqlCreateTable", "Pool",
  function(con, table, fields, row.names = NA, temporary = FALSE, ...) {
    connection <- con$fetch()
    on.exit(poolReturn(connection))
    DBI::sqlCreateTable(connection, table, fields,
                        row.names, temporary, ...)
  }
)

#' @param values See \code{\link[DBI]{sqlAppendTable}}.
#' @export
#' @rdname DBI-connection-sql
setMethod("sqlAppendTable", "Pool",
  function(con, table, values, row.names = NA, ...) {
    connection <- con$fetch()
    on.exit(poolReturn(connection))
    DBI::sqlAppendTable(connection, table, values, row.names, ...)
  }
)

#' @include DBI.R
NULL

#' Safely interpolate values into an SQL string and parse them back.
#'
#' Pool object wrappers around DBIConnection methods that deal
#' with the safe interpolation of values into an SQL string and
#' the reverse -- parsing interpolated variables from SQL. See
#' \code{\link[DBI]{sqlInterpolate}} and
#' \code{\link[DBI]{sqlParseVariables}} for the original
#' documentation.
#'
#' @name DBI-connection-interpolate
NULL

#' @export
#' @rdname DBI-connection-interpolate
setMethod("sqlInterpolate", "Pool", function(`_con`, `_sql`, ...) {
  `_connection` <- `_con`$fetch()
  on.exit(release(`_connection`))
  DBI::sqlInterpolate(`_connection`, `_sql`, ...)
})

#' @export
#' @rdname DBI-connection-interpolate
setMethod("sqlParseVariables", "Pool", function(con, sql, ...) {
  connection <- con$fetch()
  on.exit(release(connection))
  DBI::sqlParseVariables(connection, sql, ...)
})

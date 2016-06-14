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

#' @param _con,`_sql`,... See \code{\link[DBI]{sqlInterpolate}}.
#' @export
#' @rdname DBI-connection-interpolate
setMethod("sqlInterpolate", "Pool", function(`_con`, `_sql`, ...) {
  `_connection` <- poolCheckout(`_con`)
  on.exit(poolReturn(`_connection`))
  DBI::sqlInterpolate(`_connection`, `_sql`, ...)
})

#' @param con,sql See \code{\link[DBI]{sqlParseVariables}}.
#' @export
#' @rdname DBI-connection-interpolate
setMethod("sqlParseVariables", "Pool", function(con, sql, ...) {
  connection <- poolCheckout(con)
  on.exit(poolReturn(connection))
  DBI::sqlParseVariables(connection, sql, ...)
})

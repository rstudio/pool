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

#' @param conn,sql,...,.dots See \code{\link[DBI]{sqlInterpolate}}.
#' @export
#' @rdname DBI-connection-interpolate
setMethod("sqlInterpolate", "Pool", function(conn, sql, ..., .dots = list()) {
  connection <- poolCheckout(conn)
  on.exit(poolReturn(connection))
  if (identical(list(), .dots)) {
    DBI::sqlInterpolate(connection, sql, ...)
  } else {
    DBI::sqlInterpolate(connection, sql, .dots = .dots)
  }
})

#' @export
#' @rdname DBI-connection-interpolate
setMethod("sqlParseVariables", "Pool", function(conn, sql, ...) {
  connection <- poolCheckout(conn)
  on.exit(poolReturn(connection))
  DBI::sqlParseVariables(connection, sql, ...)
})

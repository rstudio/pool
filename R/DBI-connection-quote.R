#' @include DBI.R
NULL

#' SQL quoting.
#'
#' Pool object wrappers around DBIConnection methods that deal
#' with SQL escaping needs. See \code{\link[DBI]{SQL}} for the
#' original documentation.
#'
#' @param conn,x,... See \code{\link[DBI]{SQL}}.
#'
#' @name DBI-connection-quote
NULL

#' @export
#' @rdname DBI-connection-quote
setMethod("dbQuoteIdentifier", c("Pool", "ANY"),
  function(conn, x, ...) {
    connection <- poolCheckout(conn)
    on.exit(poolReturn(connection))
    DBI::dbQuoteIdentifier(connection, x, ...)
  }
)

#' @export
#' @rdname DBI-connection-quote
setMethod("dbQuoteLiteral", c("Pool", "ANY"),
  function(conn, x, ...) {
    connection <- poolCheckout(conn)
    on.exit(poolReturn(connection))
    DBI::dbQuoteLiteral(connection, x, ...)
  }
)

#' @export
#' @rdname DBI-connection-quote
setMethod("dbQuoteString", c("Pool", "ANY"),
  function(conn, x, ...) {
    connection <- poolCheckout(conn)
    on.exit(poolReturn(connection))
    DBI::dbQuoteString(connection, x, ...)
  }
)

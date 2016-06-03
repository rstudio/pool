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
setMethod("dbQuoteIdentifier", c("Pool", "character"),
  function(conn, x, ...) {
    connection <- conn$fetch()
    on.exit(release(connection))
    DBI::dbQuoteIdentifier(connection, x, ...)
  }
)

#' @export
#' @rdname DBI-connection-quote
setMethod("dbQuoteIdentifier", c("Pool", "SQL"),
  function(conn, x, ...) {
    connection <- conn$fetch()
    on.exit(release(connection))
    DBI::dbQuoteIdentifier(connection, x, ...)
  }
)

#' @export
#' @rdname DBI-connection-quote
setMethod("dbQuoteIdentifier", c("Pool", "Table"),
  function(conn, x, ...) {
    connection <- conn$fetch()
    on.exit(release(connection))
    DBI::dbQuoteIdentifier(connection, x, ...)
  }
)

#' @export
#' @rdname DBI-connection-quote
setMethod("dbQuoteString", c("Pool", "character"),
  function(conn, x, ...) {
    connection <- conn$fetch()
    on.exit(release(connection))
    DBI::dbQuoteString(connection, x, ...)
  }
)

#' @export
#' @rdname DBI-connection-quote
setMethod("dbQuoteString", c("Pool", "SQL"),
  function(conn, x, ...) {
    connection <- conn$fetch()
    on.exit(release(connection))
    DBI::dbQuoteString(connection, x, ...)
  }
)

#' @include DBI.R
NULL

#' Release a connection back to the pool
#'
#' A wrapper around \code{poolReturn(conn)}. Unlike its namesake in DBI,
#' in the context of the \code{pool} package, \code{dbDisconnect} does
#' not close the connection and free the associated resources. Rather,
#' it simply returns the connection back to pool, which then decides
#' whether to keep it around or actually destroy it.
#'
#' @param conn A DBIConnection object previously fetched from the pool.
#' @param ... Not in use.
#'
#' @export
setMethod("dbDisconnect", "DBIConnection", function(conn, ...) {
  poolReturn(conn)
})

#' @export
#' @rdname object
setMethod("onPassivate", "DBIConnection", function(object) {
  rs <- dbListResults(object)
  lapply(rs, dbClearResult)
  dbRollback(object)
})

#' @export
#' @rdname object
setMethod("onDestroy", "DBIConnection", function(object) {
  invisible()
})

#' @export
#' @rdname object
setMethod("onValidate", "DBIConnection", function(object) {
  invisible()
})

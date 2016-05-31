#' @include DBI.R
NULL

#****************** General object methods ******************#
## Include only methods that deviate from the defaults set in
## object.R

#' @export
setMethod("onPassivate", "DBIConnection", function(object) {
  rs <- dbListResults(object)
  lapply(rs, dbRollback)
  lapply(rs, dbClearResult)
})

#' @export
setMethod("onDestroy", "DBIConnection", function(object) {NULL})

#' @export
setMethod("onValidate", "DBIConnection", function(object) {NULL})


#******************* DBI specific methods *******************#
## Wrap DBI methods using the pool object directly, rather
## than the fetched connection.

## To be used with care, since this puts the onus of releasing
## the connection on you (the user)
#' @export
setMethod("dbConnect", "Pool", function(drv, ...) {
  # fetch a connection
  drv$fetch()
})

## Throw error here since this would require keeping a connection
## open and never releasing it back to the pool.
#' @export
setMethod("dbSendQuery", "Pool", function(conn, statement, ...) {
  stop("Must use `conn <- pool$fetch(); dbSendQuery(conn, statement, ...)`",
       "instead. Remember to `release(conn)` when `conn` is no longer",
       "necessary.")
})

## Always use this, except if dealing with transactions that
## cannot be dealt with using withTransaction(...)
#' @export
setMethod("dbGetQuery", "Pool", function(conn, statement, ...) {
  connection <- conn$fetch()
  on.exit(release(connection))
  DBI::dbGetQuery(connection, statement, ...)
})

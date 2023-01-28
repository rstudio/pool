#' DBI methods
#'
#' @description
#' Most pool methods for DBI generics check out a connection, perform the
#' operation, and the return the connection to the pool, as described in
#' [DBI-wrap].
#'
#' This page describes the exceptions:
#'
#' * [DBI::dbSendQuery()] and [DBI::dbSendStatement()] can't work with pool
#'   because they return result sets that are bound to a specific connection.
#'   Instead use [DBI::dbGetQuery()] and [DBI::dbExecute()].
#'
#' * [DBI::dbBegin()], [DBI::dbRollback()], [DBI::dbCommit()], and
#'   [DBI::dbWithTransaction()] can't work with pool because transactions are
#'   bound to a connection. Instead use [poolWithTransaction()].
#'
#' * [DBI::dbDisconnect()] can't work because pool handles disconnection.
#'
#' * [DBI::dbGetInfo()] returns information about the pool, not the database
#'   connection.
#'
#' * [DBI::dbIsValid()] returns whether or not the entire pool is valid (i.e.
#'   not closed).
#'
#' @param conn,dbObj A Pool object, as returned from [dbPool()].
#' @param statement,code,... See DBI documentation.
#' @name DBI-custom
NULL

setClass("Pool")

#' @export
#' @rdname DBI-custom
setMethod("dbSendQuery", "Pool", function(conn, statement, ...) {
  abort(c(
    "Not supported for pool objects",
    i = "Please use `dbGetQuery()` instead"
  ))
})

#' @export
#' @rdname DBI-custom
setMethod("dbSendStatement", "Pool", function(conn, statement, ...) {
  abort(c(
    "Not supported for pool objects",
    i = "Please use `dbExecute()` instead"
  ))
})

#' @export
#' @rdname DBI-custom
setMethod("dbDisconnect", "Pool", function(conn, ...) {
  abort(c(
    "Not supported for pool objects"
  ))
})

#' @export
#' @rdname DBI-custom
setMethod("dbGetInfo", "Pool", function(dbObj, ...) {
  pooledObj <- poolCheckout(dbObj)
  on.exit(poolReturn(pooledObj))
  list(
    class = is(dbObj),
    valid = dbObj$valid,
    minSize = dbObj$minSize,
    maxSize = dbObj$maxSize,
    idleTimeout = dbObj$idleTimeout,
    pooledObjectClass = is(pooledObj)[1],
    numberFreeObjects = dbObj$counters$free,
    numberTakenObjects = dbObj$counters$taken
  )
})

#' @export
#' @rdname DBI-custom
setMethod("dbIsValid", "Pool", function(dbObj, ...) {
  dbObj$valid
})

#' @export
#' @rdname DBI-custom
setMethod("dbBegin", "Pool", function(conn, ...) {
  abort(c(
    "Not supported for pool objects",
    i = "Please use `poolWithTransaction()` instead"
  ))
})

#' @export
#' @rdname DBI-custom
setMethod("dbCommit", "Pool", function(conn, ...) {
  abort(c(
    "Not supported for pool objects",
    i = "Please use `poolWithTransaction()` instead"
  ))
})

#' @export
#' @rdname DBI-custom
setMethod("dbRollback", "Pool", function(conn, ...) {
  abort(c(
    "Not supported for pool objects",
    i = "Please use `poolWithTransaction()` instead"
  ))
})


#' @export
#' @rdname DBI-custom
setMethod("dbWithTransaction", "Pool", function(conn, code) {
  abort(c(
    "Not supported for pool objects",
    i = "Please use `poolWithTransaction()` instead"
  ))
})

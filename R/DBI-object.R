#' @include DBI.R
NULL

#' DBIObject methods.
#'
#' Pool object wrappers around DBIObject methods. See
#' [DBI::dbDataType()], [DBI::dbGetInfo()]
#' and [DBI::dbIsValid()] for the original
#' documentation.
#'
#' @name DBI-object
#' @examples
#' if (requireNamespace("RSQLite", quietly = TRUE)) {
#'   pool <- dbPool(RSQLite::SQLite(), dbname = ":memory:")
#'
#'   dbGetInfo(pool)
#'   dbIsValid(pool)
#'
#'   dbDataType(pool, 1:5)
#'   dbDataType(pool, 1)
#'   dbDataType(pool, TRUE)
#'   dbDataType(pool, Sys.Date())
#'   dbDataType(pool, Sys.time())
#'   dbDataType(pool, Sys.time() - as.POSIXct(Sys.Date()))
#'   dbDataType(pool, c("x", "abc"))
#'   dbDataType(pool, list(raw(10), raw(20)))
#'   dbDataType(pool, I(3))
#'   dbDataType(pool, iris)
#'
#'   poolClose(pool)
#'
#'   dbIsValid(pool)
#'
#' } else {
#'   message("Please install the 'RSQLite' package to run this example")
#' }
NULL

#' @param dbObj,obj,... See [DBI::dbDataType()].
#' @export
#' @rdname DBI-object
setMethod("dbDataType", "Pool", function(dbObj, obj, ...) {
  connection <- poolCheckout(dbObj)
  on.exit(poolReturn(connection))
  DBI::dbDataType(connection, obj, ...)
})

#' @export
#' @rdname DBI-object
setMethod("dbGetInfo", "Pool", function(dbObj, ...) {
  pooledObj <- poolCheckout(dbObj)
  on.exit(poolReturn(pooledObj))
  list(class = is(dbObj),
       valid = dbObj$valid,
       minSize = dbObj$minSize,
       maxSize = dbObj$maxSize,
       idleTimeout = dbObj$idleTimeout,
       pooledObjectClass = is(pooledObj)[1],
       numberFreeObjects = dbObj$counters$free,
       numberTakenObjects = dbObj$counters$taken)
})

#' @export
#' @rdname DBI-object
setMethod("dbIsValid", "Pool", function(dbObj, obj, ...) {
  dbObj$valid
})

#' @include DBI.R
NULL

#' DBIObject methods.
#'
#' Pool object wrappers around DBIObject methods. See
#' \code{\link[DBI]{dbDataType}}, \code{\link[DBI]{dbGetInfo}}
#' and \code{\link[DBI]{dbIsValid}} for the original
#' documentation.
#'
#' @name DBI-object
NULL

#' @param dbObj,obj,... See \code{\link[DBI]{dbDataType}}.
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
       minSize = dbObj$minSize,
       maxSize = dbObj$maxSize,
       pooledObjectClass = is(pooledObj)[1],
       numberFreeObjects = dbObj$freeObjCounter,
       numberTakenObjects = dbObj$takenObjCounter,
       numberLeakedObjects = dbObj$leakedObjCounter)
})

#' @export
#' @rdname DBI-object
setMethod("dbIsValid", "Pool", function(dbObj, obj, ...) {
  dbObj$valid
})

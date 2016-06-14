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
  connection <- dbObj$fetch()
  on.exit(poolReturn(connection))
  DBI::dbDataType(connection, obj, ...)
})

#' @export
#' @rdname DBI-object
setMethod("dbGetInfo", "Pool", function(dbObj, ...) {
  list()  #### CHANGE THIS!!! (once public Pool counters exist)
})

#' @export
#' @rdname DBI-object
setMethod("dbIsValid", "Pool", function(dbObj, obj, ...) {
  dbObj$valid
})

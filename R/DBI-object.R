#' @include DBI.R
NULL

#' DBIObject methods.
#'
#' Pool object wrappers around DBIObject methods. See
#' \code{\link[DBI]{show}}, \code{\link[DBI]{dbDataType}},
#' \code{\link[DBI]{dbGetInfo}} and \code{\link[DBI]{dbIsValid}}
#' for the original documentation.
#'
#' @name DBI-object
NULL

#' @export
#' @rdname DBI-object
setMethod("show", "Pool", function(object) {
  connection <- object$fetch()
  on.exit(release(connection))
  DBI::show(connection)
})

#' @export
#' @rdname DBI-object
setMethod("dbDataType", "Pool", function(dbObj, obj, ...) {
  DBI:::dbiDataType(obj)
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

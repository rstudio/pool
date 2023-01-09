#' @include pool-methods.R
NULL

#' Make R identifiers into legal SQL identifiers.
#'
#' Pool object wrappers around DBIObject methods. See
#' [DBI::make.db.names()] for the original documentation.
#' Note that these methods are DEPRECATED. Please use
#' `dbQuoteIdentifier` (or possibly `dbQuoteString`)
#' instead, as documented in [DBI-connection-quote()].
#'
#' @name DBI-object-deprecated
NULL

#' @param dbObj,snames,keywords,unique,allow.keywords,name,case,...
#'   see [DBI::make.db.names()]
#' @export
#' @rdname DBI-object-deprecated
setMethod("make.db.names", signature(dbObj="Pool", snames="character"),
  definition = function(dbObj, snames, keywords, unique, allow.keywords, ...) {
    connection <- poolCheckout(dbObj)
    on.exit(poolReturn(connection))
    DBI::make.db.names(connection, snames, keywords, unique, allow.keywords, ...)
  }
)

#' @export
#' @rdname DBI-object-deprecated
setMethod("isSQLKeyword", signature(dbObj="Pool", name="character"),
  definition = function(dbObj, name, keywords, case, ...) {
    connection <- poolCheckout(dbObj)
    on.exit(poolReturn(connection))
    DBI::isSQLKeyword(connection, name, keywords, case, ...)
  }
)

#' @export
#' @rdname DBI-object-deprecated
setMethod("SQLKeywords", "Pool", function(dbObj, ...) {
  connection <- poolCheckout(dbObj)
  on.exit(poolReturn(connection))
  DBI::SQLKeywords(connection, ...)
})

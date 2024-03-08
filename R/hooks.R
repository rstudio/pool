#' Pooled object methods.
#'
#' For backend authors only. Authors should implement all of these,
#' which are then called by the Pool class methods. These should
#' not be called directly either by backend authors or by the end
#' users.
#'
#' @keywords internal
#' @param object A pooled object.
#' @name hooks
NULL

#' @export
#' @rdname hooks
setGeneric("onActivate", function(object) {
  standardGeneric("onActivate")
})

#' @export
#' @rdname hooks
setGeneric("onPassivate", function(object) {
  standardGeneric("onPassivate")
})

#' @export
#' @rdname hooks
setGeneric("onDestroy", function(object) {
  standardGeneric("onDestroy")
})

#' @param query A simple query that can be used to verify that
#' the `object` functions as expected.
#' @export
#' @rdname hooks
setGeneric("onValidate", function(object, query) {
  standardGeneric("onValidate")
})


# Defaults ----------------------------------------------------------------

#' @export
#' @rdname hooks
setMethod("onActivate", "ANY", function(object) {
  invisible()
})

#' @export
#' @rdname hooks
setMethod("onPassivate", "ANY", function(object) {
  invisible()
})

#' @export
#' @rdname hooks
setMethod("onDestroy", "ANY", function(object) {
  invisible()
})

#' @export
#' @rdname hooks
setMethod("onValidate", "ANY", function(object, query) {
  invisible()
})

# DBI ---------------------------------------------------------------------

## This method used to attempt to clean up for you if it
## detected open results sets. However, different database
## drivers implement different DBI methods and this quickly
## became unwieldy. From now on, pool will not reset a
## connection's state back to a clean slate (which is not
## something to worry about unless users are explicitly
## checking out connections -- only necessary for pretty
## complicated transactions).
#' @export
#' @rdname hooks
setMethod("onPassivate", "DBIConnection", function(object) {
  invisible()
})

#' @export
#' @rdname hooks
setMethod("onDestroy", "DBIConnection", function(object) {
  DBI::dbDisconnect(object)
})

#' @export
#' @rdname hooks
setMethod("onValidate", "DBIConnection", function(object) {
  pool <- pool_metadata(object)$pool
  query <- pool$state$validateQuery

  if (!is.null(query)) {
    error <- try({
      dbGetQuery(object, query)
      return()
    }, silent = TRUE)
  } else {

    ## options mostly gathered from here:
    ## http://stackoverflow.com/a/3670000/6174455
    options <- c(
      # Most modern databases
      "SELECT 1",
      # Oracle: https://en.wikipedia.org/wiki/DUAL_table
      "SELECT 1 FROM DUAL",
      # HSQLDB
      "SELECT 1 FROM INFORMATION_SCHEMA.SYSTEM_USERS WHERE 0=1",
      "SELECT 1 FROM INFORMATION_SCHEMA.TABLES WHERE 0=1",
      # DB2: https://www.ibm.com/docs/en/db2-for-zos/12?topic=tables-sysdummy1
      "SELECT 1 FROM SYSIBM.SYSDUMMY1",
      # informix
      "SELECT 1 FROM systables WHERE 0=1",
      # SAP HANA
      "SELECT 1 FROM SYS_TABLES WHERE 0=1",
      "SELECT 1 FROM SYS.TABLES WHERE 0=1",
      # Firebird
      # https://firebirdsql.org/file/documentation/chunk/en/refdocs/fblangref30/fblangref30-appx04-systables.html
      "SELECT 1 FROM RDB$DATABASE WHERE 0=1"
    )

    ## Iterates through the possible validation queries:
    ## the first one that succeeds get stored in the `state`
    ## of pool (a public variable) and we return.
    ## If none succeed, validation is not possible and we
    ## throw an error.
    for (opt in options) {
      error <- try({
        dbGetQuery(object, opt)
        pool$state$validateQuery <- opt
        return()
      }, silent = TRUE)
    }
  }
  cond <- attr(error, "condition", exact = TRUE)
  stop(simpleError(
    paste("Validation not successful --", conditionMessage(cond)),
    conditionCall(cond)
  ))
})


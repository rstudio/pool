#' @include object.R
NULL

## This method used to attempt to clean up for you if it
## detected open results sets. However, different database
## drivers implement different DBI methods and this quickly
## became unwieldy. From now on, pool will not reset a
## connection's state back to a clean slate (which is not
## something to worry about unless users are explicitly
## checking out connections -- only necessary for pretty
## complicated transactions).
#' @export
#' @rdname object
setMethod("onPassivate", "DBIConnection", function(object) {
  invisible()
})

#' @export
#' @rdname object
setMethod("onDestroy", "DBIConnection", function(object) {
  DBI::dbDisconnect(object)
})

#' @export
#' @rdname object
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
      "SELECT 1",
      "SELECT 1 FROM DUAL",
      "SELECT 1 FROM INFORMATION_SCHEMA.SYSTEM_USERS",
      "SELECT 1 FROM INFORMATION_SCHEMA.TABLES",
      "VALUES 1",
      "SELECT 1 FROM SYSIBM.SYSDUMMY1",
      "SELECT 1 FROM systables",
      "SELECT 1 FROM SYS_TABLES",
      "SELECT 1 FROM SYS.TABLES"
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

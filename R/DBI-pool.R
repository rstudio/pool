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

## Ideally this would also reset the connection more fully (ex: reset
## all user-defined variables set with `SET` back to their default values).
## Currently, there isn't a handy dbResetConnection() generic in DBI, so
## this functionality is not yet here. Until then, our reccomendation is
## that, if you absolutely need to set variables, or somehow modify the
## connection's defaults, you might not want to use the pool package. It
## won't stop you, but it does mean that the next time you fetch a connection,
## you'll never be sure if this is a brand new connection with the default
## settings OR a connection that you have previsouly returned to pool
## AFTER you changed the defaults...
#' @export
#' @rdname object
setMethod("onPassivate", "DBIConnection", function(object) {
  if (inherits(object, "JDBCConnection")){
    return(NULL)
  }
  rs <- dbListResults(object)
  lapply(rs, function(x) {
    if (dbIsValid(x)) {
      dbClearResult(x)
      dbRollback(object)
    }
  })
})

#' @export
#' @rdname object
setMethod("onDestroy", "DBIConnection", function(object) {
  DBI::dbDisconnect(object)
})

#' @export
#' @rdname object
setMethod("onValidate", "DBIConnection", function(object) {
  pool <- attr(object, "..metadata", exact = TRUE)$pool
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
      "SELECT * FROM INFORMATION_SCHEMA.TABLES",
      "VALUES 1",
      "SELECT 1 FROM SYSIBM.SYSDUMMY1",
      "select count(*) from systables"
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

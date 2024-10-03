#' Create a pool of database connections
#'
#' `dbPool()` is a drop-in replacement for [DBI::dbConnect()] that
#' provides a shared pool of connections that can automatically reconnect
#' to the database if needed.
#' See [DBI-wrap] for methods to use with pool objects,
#' and [DBI-custom] for unsupported methods and the "pool" way of using them.
#'
#' A new connection is created transparently
#'
#' - if the pool is empty
#' - if the currently checked out connection is invalid
#'   (checked at most once every `validationInterval` seconds)
#' - if the pool is not full and the connections are all in use
#'
#' Use [poolClose()] to close the pool and all connections in it.
#' See [poolCraete()] for details on the internal workings of the pool.
#'
#' @param drv A [DBI Driver][DBI::DBIDriver-class], e.g. `RSQLite::SQLite()`,
#'   `RPostgres::Postgres()`, `odbc::odbc()` etc.
#' @param ... Arguments passed on to [DBI::dbConnect()]. These are used to
#'   identify the database and provide needed authentication.
#' @param onCreate A function that takes a single argument, a connection,
#'   and is called when the connection is created. Use this with
#'   [DBI::dbExecute()] to set default options on every connection created
#'   by the pool.
#' @inheritParams poolCreate
#' @param validateQuery A simple query that can be used to verify that the
#'   connetction is valid. If not provided, `dbPool()` will try a few common
#'   options, but these don't work for all databases.
#'
#' @export
#' @examples
#' # You use a dbPool in the same way as a standard DBI connection
#' pool <- dbPool(RSQLite::SQLite(), dbname = demoDb())
#' pool
#'
#' dbGetQuery(pool, "SELECT * FROM mtcars LIMIT 4")
#'
#' # Always close a pool when you're done using it
#' poolClose(pool)
dbPool <- function(drv,
                   ...,
                   minSize = 1,
                   maxSize = Inf,
                   onCreate = NULL,
                   idleTimeout = 60,
                   validationInterval = 60,
                   validateQuery = NULL) {

  # Force dots
  dots <- list(...)
  if (length(dots) > 0 && !is_named(dots)) {
    abort("All arguments to `dbPool` must be named")
  }

  state <- new.env(parent = emptyenv())
  state$validateQuery <- validateQuery

  # Force other arguments needed for
  list(drv, onCreate)

  poolCreate(
    factory = function() {
      con <- dbConnect(drv, ...)
      if (!is.null(onCreate)) {
        onCreate(con)
      }
      con
    },
    state = state,
    minSize = minSize,
    maxSize = maxSize,
    idleTimeout = idleTimeout,
    validationInterval = validationInterval
  )
}

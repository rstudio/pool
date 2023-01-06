#' @include object.R
NULL

#' Create a pool of database connections
#'
#' `dbPool()` is a drop-in replacement for [DBI::dbConnect()] that
#' provides a shared pool of connections that can automatically reconnect
#' to the database if needed.
#'
#' @param drv A [DBI Driver][DBI::DBIDriver-class], e.g. `RSQLite::SQLite()`,
#'   `RPostgres::Postgres()`, `odbc::odbc()` etc.
#' @param ... Arguments passed on to [DBI::dbConnect()]. These are used to
#'   identify the database and provide needed authentication.
#' @inheritParams poolCreate
#' @param validateQuery A simple query that can be used to verify that the
#'   connetction is valid. If not provided, `dbPool()` will try a few common
#'   options, but these don't work for all databases.
#'
#' @export
#' @examples
#' # You use a dbPool in the same way as a standard DBI connection
#' pool <- dbPool(RSQLite::SQLite())
#' pool
#'
#' DBI::dbWriteTable(pool, "mtcars", mtcars)
#' dbGetQuery(pool, "SELECT * FROM mtcars LIMIT 4")
#'
#' # Using the RMySQL package
#' if (requireNamespace("RMySQL", quietly = TRUE)) {
#'   pool <- dbPool(
#'     drv = RMySQL::MySQL(),
#'     dbname = "shinydemo",
#'     host = "shiny-demo.csa7qlmguqrf.us-east-1.rds.amazonaws.com",
#'     username = "guest",
#'     password = "guest"
#'   )
#'
#'   dbGetQuery(pool, "SELECT * from City LIMIT 5;")
#'
#'   poolClose(pool)
#' }
dbPool <- function(drv,
                   ...,
                   minSize = 1,
                   maxSize = Inf,
                   idleTimeout = 60,
                   validationInterval = 600,
                   validateQuery = NULL) {

  # Force dots
  dots <- list(...)
  if (length(dots) > 0 && !is_named(dots)) {
    stop("All arguments to `dbPool` must be named")
  }

  state <- new.env(parent = emptyenv())
  state$validateQuery <- validateQuery

  poolCreate(
    factory = function() dbConnect(drv, ...),
    state = state,
    minSize = minSize,
    maxSize = maxSize,
    idleTimeout = idleTimeout,
    validationInterval = validationInterval
  )
}

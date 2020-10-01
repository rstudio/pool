#' @include object.R
NULL

#' Create a DBI Database Connection Pool.
#'
#' A wrapper around `poolCreate` to simplify the creation of a DBI
#' database connection pool. Check the documentation of [poolCreate()]
#' for a generic overview of the parent function and the Pool object. The
#' main thing to point out is that, for `dbPool`, you always need to
#' provide a DBI driver (i.e. of class [DBI::DBIDriver-class()]),
#' and it should always be accompanied by the required authorization
#' arguments (see the example below).
#'
#' @param drv An object that inherits from [DBI::DBIDriver-class()],
#'   or an existing [DBI::DBIConnection-class()] object (in order to
#'   clone an existing connection).
#'
#' @param ... Arguments needed for both [DBI::dbConnect()]
#' (mandatory if required by the `DBIDriver` you're using) and
#' [poolCreate()] (optional) - all arguments should be named:
#' \enumerate{
#'   \item The required authorization arguments needed by the DBMS
#'   instance; these typically include `user`, `password`,
#'   `dbname`, `host`, `port`, etc. For details check
#'   the appropriate `DBIDriver`'s documentation.
#'   \item Optionally, override the `poolCreate` defaults:
#'   `minSize` (minimum number of connections that the pool should
#'   have at all times), `maxSize` (maximum number of connections
#'   that the pool may have at any time), `idleTimeout` (number
#'   of seconds to wait before closing a connection, if the number
#'   of connection is above `minSize`), and `validationInterval`
#'   (number of seconds to wait before validating the connection again).
#' }
#' @param validateQuery The query to run to verify that the connection
#' is valid (it should be as simple as possible). If this is not
#' provided, `dbPool` will try a few possibilities, but these are
#' not exhaustive.
#'
#' @export
#' @examples
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
#'   #>   ID           Name CountryCode      District Population
#'   #> 1  1          Kabul         AFG         Kabol    1780000
#'   #> 2  2       Qandahar         AFG      Qandahar     237500
#'   #> 3  3          Herat         AFG         Herat     186800
#'   #> 4  4 Mazar-e-Sharif         AFG         Balkh     127800
#'   #> 5  5      Amsterdam         NLD Noord-Holland     731200
#'
#'   poolClose(pool)
#'
#' } else {
#'   message("Please install the 'RMySQL' package to run this example")
#' }
#'
#' # Using a DSN with an ODBC driver
#' \dontrun{
#' pool <- dbPool(odbc::odbc(), dsn = "Data Source Name")
#' }
dbPool <- function(drv, ..., validateQuery = NULL) {
  state <- new.env(parent = emptyenv())
  state$validateQuery <- validateQuery

  dots <- list(...)

  if (length(names(dots)) != length(dots)) {
    stop("All arguments to `dbPool` must be named")
  }

  poolCreate_argnames <- setdiff(names(formals(poolCreate)), c("factory", "state"))
  poolCreate_args <- dots[intersect(names(dots), poolCreate_argnames)]
  dbConnect_args <- dots[setdiff(names(dots), poolCreate_argnames)]

  factory <- function()  do.call(dbConnect, c(list(drv), dbConnect_args))
  do.call(poolCreate, c(list(factory, state = state), poolCreate_args))
}

#' @include object.R
NULL

#' Create a DBI Database Connection Pool.
#'
#' A wrapper around \code{poolCreate} to simplify the creation of a DBI
#' database connection pool. Check the documentation of \code{\link{poolCreate}}
#' for a generic overview of the parent function and the Pool object. The
#' main thing to point out is that, for \code{dbPool}, you always need to
#' provide a DBI driver (i.e. of class \code{\link[DBI]{DBIDriver-class}}),
#' and it should always be accompanied by the required authorization
#' arguments (see the example below).
#'
#' @param drv An object that inherits from \code{\link[DBI]{DBIDriver-class}},
#'   or an existing \code{\link[DBI]{DBIConnection-class}} object (in order to
#'   clone an existing connection).
#'
#' @param ... Arguments needed for both \code{\link[DBI]{dbConnect}}
#' (mandatory if required by the \code{DBIDriver} you're using) and
#' \code{\link{poolCreate}} (optional) - all arguments should be named:
#' \enumerate{
#'   \item The required authorization arguments needed by the DBMS
#'   instance; these typically include \code{user}, \code{password},
#'   \code{dbname}, \code{host}, \code{port}, etc. For details check
#'   the appropriate \code{DBIDriver}'s documentation.
#'   \item Optionally, override the \code{poolCreate} defaults:
#'   \code{minSize} (minimum number of connections that the pool should
#'   have at all times), \code{maxSize} (maximum number of connections
#'   that the pool may have at any time), \code{idleTimeout} (number
#'   of seconds to wait before closing a connection, if the number
#'   of connection is above \code{minSize}), and \code{validationInterval}
#'   (number of seconds to wait before validating the connection again).
#' }
#' @param validateQuery The query to run to verify that the connection
#' is valid (it should be as simple as possible). If this is not
#' provided, \code{dbPool} will try a few possibilities, but these are
#' not exhaustive.
#'
#' @export
#' @examples
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

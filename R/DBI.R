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
#' or a character string specifying the name of DBMS driver, e.g.,
#' "SQLite", "MySQL", "PostgreSQL", or an existing
#' \code{\link[DBI]{DBIConnection-class}} object (in order to clone an
#' existing connection).
#'
#' @param ... Arguments needed for both \code{\link[DBI]{dbConnect}}
#' (mandatory if required by the \code{DBIDriver} you're using) and
#' \code{\link{poolCreate}} (optional) - all arguments should be named:
#' \enumerate{
#'   \item The required authorization arguments needed by the DBMS
#'   instance; these typically include \code{user}, \code{password},
#'   \code{dbname}, \code{host}, \code{port}, etc. For details check
#'   the appropriate \code{DBIDriver}'s documentayion.
#'   \item Optionally, override the \code{poolCreate} defaults:
#'   \code{minSize} (minimum number of connections that the pool should
#'   have at all times), \code{maxSize} (maximum number of connections
#'   that the pool may have at any time), \code{idleTimeout} (number
#'   of milliseconds to wait before closing a connection, if the number
#'   of connection is above \code{minSize}), and \code{validateTimeout}
#'   (number of milliseconds to wait before validating the connection
#'   again).
#' }
#' @param validateQuery The query to run to verify that the connection
#' is valid (it should be as simple as possible). If this is not
#' provided, \code{dbPool} will try a few possibilities, but these are
#' not exhaustive.
#'
#' @export
#' @examples
#' \dontrun{
#' pool <- dbPool(
#'   drv = RMySQL::MySQL(),
#'   dbname = "shinydemo",
#'   host = "shiny-demo.csa7qlmguqrf.us-east-1.rds.amazonaws.com",
#'   username = "guest",
#'   password = "guest"
#' )
#'
#' dbGetQuery(pool, "SELECT * from City LIMIT 5;")
#' #>   ID           Name CountryCode      District Population
#' #> 1  1          Kabul         AFG         Kabol    1780000
#' #> 2  2       Qandahar         AFG      Qandahar     237500
#' #> 3  3          Herat         AFG         Herat     186800
#' #> 4  4 Mazar-e-Sharif         AFG         Balkh     127800
#' #> 5  5      Amsterdam         NLD Noord-Holland     731200
#'
#' poolClose(pool)
#' }
dbPool <- function(drv, ..., validateQuery = NULL) {
  if (is.character(drv)) {
    if (drv == "SQLite") {
      if (!requireNamespace("RSQLite", quietly = TRUE)) {
        stop("RSQLite package required", call. = FALSE)
      }
      drv = RSQLite::SQLite()
    }
    else if (drv == "MySQL") {
      if (!requireNamespace("RMySQL", quietly = TRUE)) {
        stop("RMySQL package required", call. = FALSE)
      }
      drv = RMySQL::MySQL()
    }
    else if (drv == "PostgreSQL") {
      if (!requireNamespace("RPostgreSQL", quietly = TRUE)) {
        stop("RPostgreSQL package required", call. = FALSE)
      }
      drv = RPostgreSQL::PostgreSQL()
    }
  }
  stateEnv <- new.env(parent = emptyenv())
  stateEnv$validateQuery <- validateQuery
  poolCreate(dbConnect, drv, ..., stateEnv = stateEnv)
}

#' Wrap DBI Database Connection Pool for dplyr use.
#'
#' This functions opens a \code{dplyr}-compatible connection. If you're
#' used to using \code{dplyr} to query databases, then you're familiar
#' with the \code{src_*} functions (e.g. \code{\link[dplyr]{src_mysql}}).
#' In those functions, you typically have to specify the \code{drv} and
#' all the authorization arguments required to connect to the database.
#' If you're using \code{pool}, however, you've already did all of this
#' when you created the Pool object with \code{dbPool}. So for
#' \code{src_pool}, you only need to pass in that same Pool object. Then,
#' you can use the resulting object just like in \code{dplyr} (see the
#' examples below).
#'
#' @param pool A pool object
#'
#' @section Comparison to \code{dplyr} code:
#' First, let's show how you'd connect and query a MySQL database
#' using only \code{dplyr}:
#'
#' \preformatted{
#' my_db <- src_mysql(
#'   dbname = "shinydemo",
#'   host = "shiny-demo.csa7qlmguqrf.us-east-1.rds.amazonaws.com",
#'   user = "guest",
#'   password = "guest"
#' )
#' # get the first 5 rows:
#' my_db \%>\% tbl("City") \%>\% head(5)
#' }
#'
#' Now, let's do the same thing using a Pool object:
#'
#' \preformatted{pool <- dbPool(
#'   drv = RMySQL::MySQL(),
#'   dbname = "shinydemo",
#'   host = "shiny-demo.csa7qlmguqrf.us-east-1.rds.amazonaws.com",
#'   username = "guest",
#'   password = "guest"
#' )
#' # get the first 5 rows:
#' src_pool(pool) \%>\% tbl("City") \%>\% head(5)
#' }
#'
#' @section Performance:
#' What's the advantage of using \code{pool} with \code{dplyr}, rather
#' than just using \code{dplyr} to query a database? As usual with
#' \code{pool}, the answer is performance (less emphasis on connection
#' management this time since \code{dplyr} already did a good job on
#' that). However, this only applies for some situations. Basically,
#' when you use any of the \code{src_*} functions (\code{src_pool}
#' included), you are creating connections that will only be closed
#' on garbage collection. If you are querying a database a lot and
#' using \code{src_*} each time, you are always fetching and closing
#' connections. The difference is that, without \code{pool}, you're
#' always fetching connections from the database itself (potentially
#' pretty computationally expensive) and closing them for good. But
#' with \code{pool}, you're just fetching and returning connections
#' to and from the pool, which is essentially free.
#'
#' (You might not need to use get a new connection with
#' \code{src_*} for each query, but if you have a potentially unbounded
#' number of users, as in a hosted Shiny app, you certainly should --
#' or you'll risk runnign out of connections. See
#' \href{http://rstudio.com}{this article} to learn more about
#' using \code{dplyr} together with \code{pool} in a Shiny app.)
#'
#' @export
#' @examples
#' \dontrun{
#' pool <- dbPool(
#'   drv = RMySQL::MySQL(),
#'   dbname = "shinydemo",
#'   host = "shiny-demo.csa7qlmguqrf.us-east-1.rds.amazonaws.com",
#'   username = "guest",
#'   password = "guest"
#' )
#'
#' # equivalent to: dbGetQuery(pool, "SELECT * from City LIMIT 5;")
#' src_pool(pool) %>% tbl("City") %>% head(5)
#' #>   ID           Name CountryCode      District Population
#' #> 1  1          Kabul         AFG         Kabol    1780000
#' #> 2  2       Qandahar         AFG      Qandahar     237500
#' #> 3  3          Herat         AFG         Herat     186800
#' #> 4  4 Mazar-e-Sharif         AFG         Balkh     127800
#' #> 5  5      Amsterdam         NLD Noord-Holland     731200
#'
#' poolClose(pool)
#' }
src_pool <- function(pool) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr package required", call. = FALSE)
  }
  drv <- attr(pool, "drv", exact = TRUE)
  conn <- poolCheckout(pool)
  info <- dbGetInfo(conn)
  dplyr::src_sql(drv, conn, info = info, disco = NULL)
}

#' @include object.R
NULL

DBIConnectionFactory <- R6Class("DBIConnectionFactory",
  public = list(
    drv = NULL,
    initialize = function(drv) {
      self$drv <- drv
    },
    generator = function(...) {
      function() {
        DBI::dbConnect(self$drv, ...)
      }
    }
  )
)

setClass("DBIConnectionFactory")

#' Create a DBI Database Connection Pool.
#'
#' Creates a DBI database connection pool. Check the documentation
#' of \code{\link{poolCreate}} for a generic overview of the this
#' function and the Pool object. The main thing to point out is
#' that, for DBI database connection pooling, the \code{src}
#' argument should always be a \code{\link[DBI]{DBIDriver-class}},
#' and it should always be accompanied by the required authorization
#' arguments (see the example below).
#'
#' @param src An object that inherits from \code{\link[DBI]{DBIDriver-class}},
#' or a character string specifying the name of DBMS driver, e.g.,
#' "RSQLite", "RMySQL", "RPostgreSQL", or an existing
#' \code{\linkS4class{DBIConnection}} object (in order to clone an
#' existing connection).
#'
#' @param minSize An optional number specifying the minimum
#' number of objects that the pool should have at all times.
#'
#' @param maxSize An optional number specifying the maximum
#' number of objects that the pool may have at any time.
#'
#' @param ... Authorization arguments needed by the DBMS instance;
#' these typically include \code{user}, \code{password}, \code{dbname},
#' \code{host}, \code{port}, etc.  For details see the appropriate
#' \code{DBIDriver}.
#'
#' @export
#' @examples
#' \dontrun{
#' pool <- poolCreate(
#'   drv = MySQL(),
#'   dbname = "shinydemo",
#'   host = "shiny-demo.csa7qlmguqrf.us-east-1.rds.amazonaws.com",
#'   username = "guest",
#'   password = "guest"
#' )
#'
#' dbGetQuery(pool, "SELECT * from City WHERE ID < 5")
#' #>   ID           Name CountryCode      District Population
#' #> 1  1          Kabul         AFG         Kabol    1780000
#' #> 2  2       Qandahar         AFG      Qandahar     237500
#' #> 3  3          Herat         AFG         Herat     186800
#' #> 4  4 Mazar-e-Sharif         AFG         Balkh     127800
#'
#' pool$close()
#' }
setMethod("poolCreate", "DBIDriver",
  function(src, minSize, maxSize, ...) {
    poolCreate(DBIConnectionFactory$new(src),
               minSize, maxSize, ...)
  }
)

#' Given a DBIConnectionFactory object, creates a pool
#'
#' @param src DBIConnectionFactory object.
#'
#' @param minSize An optional number specifying the minimum
#' number of objects that the pool should have at all times.
#'
#' @param maxSize An optional number specifying the maximum
#' number of objects that the pool may have at any time.
#'
#' @param ... Authorization arguments needed by the DBMS instance;
#' these typically include \code{user}, \code{password}, \code{dbname},
#' \code{host}, \code{port}, etc.  For details see the appropriate
#' \code{DBIDriver}.
setMethod("poolCreate", "DBIConnectionFactory",
  function(src, minSize, maxSize, ...) {
    Pool$new(src$generator(...), minSize, maxSize)
  }
)

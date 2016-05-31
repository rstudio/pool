#' @include object.R
NULL

#' @export
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

#' @export
setClass("DBIConnectionFactory")

#' @export
setMethod("createPool", "DBIDriver",
  function(drv, minSize, maxSize, ...) {
    createPool(DBIConnectionFactory$new(drv),
               minSize, maxSize, ...)
  }
)

#' @export
setMethod("createPool", "DBIConnectionFactory",
  function(drv, minSize, maxSize, ...) {
    Pool$new(drv$generator(...), minSize, maxSize)
  }
)

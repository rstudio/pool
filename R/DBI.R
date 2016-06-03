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

#' @rdname Pool
#' @export
setMethod("createPool", "DBIDriver",
  function(drv, minSize, maxSize, ...) {
    createPool(DBIConnectionFactory$new(drv),
               minSize, maxSize, ...)
  }
)

setMethod("createPool", "DBIConnectionFactory",
  function(drv, minSize, maxSize, ...) {
    Pool$new(drv$generator(...), minSize, maxSize)
  }
)

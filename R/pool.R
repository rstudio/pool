#' @include utils.R
NULL

#' Object Pooling in R.
#'
#' Creates objects pools for various types of objects in R to
#' make it less computationally expensive to get a fetch and
#' release them.
#'
#' @name pool
#' @docType package
#' @import DBI R6
NULL

#' @import methods
NULL

#' Pool Class.
#'
#' A generic pool class that holds objects. These can be fetched
#' from the pool and released back to it at will, with very
#' little computaional cost. The pool should be created only once
#' and closed when it is no longer needed, to prevent leaks.
#'
#' @usage createPool(drv, ...)
#'
#' @format A pool instance with the following public methods:
#' \itemize{
#'  \item \strong{\code{pool$fetch()}} Fetches and returns an
#'  object from the pool.
#'  \item \strong{\code{pool$release(id)}} Releases the object
#'  with \code{id} back to the pool.
#'  \item \strong{\code{pool$close()}} Closes the pool,
#'  terminating any active objects.
#' }
#'
#' @export
#' @examples
#' \dontrun{
#' pool <- createPool(
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
Pool <- R6Class("Pool",
  public = list(
    ## initialize the pool with min number of objects
    initialize = function(factory, minSize, maxSize) {
      private$factory <- factory
      private$minSize <- minSize
      private$maxSize <- maxSize
      private$freeObjects <- new.env()
      private$takenObjects <- new.env()
      for (i in seq_len(private$minSize)) {
        private$createObject()
      }
      reg.finalizer(self,
        function(self) {
          freeEnv <- private$freeObjects
          takenEnv <- private$takenObjects
          if (length(freeEnv) > 0 || length(takenEnv) > 0) { ### NEED TO RETHINK THIS
            warning("Closing leaked connections")
            self$close()
          }
        },
        onexit = TRUE)
    },
    ## calls activate and returns an object
    fetch = function() {
      freeEnv <- private$freeObjects
      takenEnv <- private$takenObjects
      ## see if there's any free objects
      if (length(freeEnv > 0)) {
        ## get first free object we find
        id <- ls(freeEnv)[[1]]
        object <- freeEnv[[id]]
      } else {
        ## if we get here, there are no free objects
        ## and we must create a new one
        object <- private$createObject()
        id <- attr(object, "id", exact = TRUE)
      }
      ## activate object and return it
      private$toggleObjectStatus(id, freeEnv, takenEnv)
      onActivate(object)
      return(object)
    },
    ## passivates the object and returns it back to
    ## the pool (possibly destroys the object if the
    ## number of total object exceeds the maximum)
    release = function(id) {
      freeEnv <- private$freeObjects
      takenEnv <- private$takenObjects
      object <- takenEnv[[id]]
      onPassivate(object)
      private$toggleObjectStatus(id, takenEnv, freeEnv)
      if (length(freeEnv) > private$maxSize) {
        onDestroy(object, envir = freeEnv)
        rm(id, envir = freeEnv)
      }
    },
    ## cleaning up and closing the pool
    close = function() {
      freeEnv <- private$freeObjects
      takenEnv <- private$takenObjects
      ## destroy all objects
      eapply(freeEnv, onDestroy)
      eapply(takenEnv, onDestroy)
      ## empty the objects' environments
      rm(list = ls(freeEnv), envir = freeEnv)
      rm(list = ls(takenEnv), envir = takenEnv)
    }
  ),
  private = list(
    freeObjects = NULL,
    takenObjects = NULL,
    factory = NULL,
    minSize = NULL,
    maxSize = NULL,
    ## creates an object, assigns it to the
    ## free environment and returns it
    createObject = function() {
      ## always create an object in the free envir
      ## to guarantee that ids are unique
      envir <- private$freeObjects
      id <- as.character(length(envir) + 1)
      object <- private$factory()
      assign(id, object, envir = envir)
      attr(object, "id") <- id
      attr(object, "pool") <- self
      return(object)
    },
    ## change the objects's environment when a
    ## free object gets taken and vice versa
    toggleConnectionStatus = function(id, from, to) {
      object <- from[[id]]
      rm(id, envir = from)
      assign(id, object, envir = to)
    }
  )
)

#*********************************************************#
#*** set S4 class and generic for consistency with DBI ***#
#*********************************************************#
#' @rdname Pool
#' @export
setClass("Pool")

#' @usage createPool(drv, minSize = 3, maxSize = Inf, ...)
#'
#' @param drv The driver used to connect to the generator of
#' the objects that the pool will hold.
#' @param minSize An optional number specifying the minimum
#' number of objects that the pool should have at all times.
#' @param maxSize An optional number specifying the maximum
#' number of objects that the pool should have at all times.
#' @param ... Authorization arguments needed by the object
#' instance generated by the \code{drv}.
#'
#' @seealso \link[DBI]{dbConnect}
#'
#' @rdname Pool
#' @export
setGeneric("createPool",
  function(drv, minSize = 3, maxSize = Inf, ...) {
    standardGeneric("createPool")
  }
)


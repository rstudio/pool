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

#' @export
Pool <- R6Class("Pool",
  public = list(
    valid = NULL,
    ## initialize the pool with min number of objects
    initialize = function(factory, minSize, maxSize) {
      self$valid <- TRUE
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
    fetch = function() {                       #### WHAT TO DO IF THIS THROWS AN ERROR???
      freeEnv <- private$freeObjects
      takenEnv <- private$takenObjects
      ## see if there's any free objects
      if (length(freeEnv) > 0) {
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
    release = function(id) {                      #### WHAT TO DO IF THIS THROWS AN ERROR???
      freeEnv <- private$freeObjects
      takenEnv <- private$takenObjects
      object <- takenEnv[[id]]
      onPassivate(object)
      private$toggleObjectStatus(id, takenEnv, freeEnv)
      if (length(freeEnv) > private$maxSize) {
        onDestroy(object) #, envir = freeEnv)
        rm(id, envir = freeEnv)
      }
    },
    ## cleaning up and closing the pool
    close = function() {                      #### WHAT TO DO IF THIS THROWS AN ERROR???
      self$valid <- FALSE
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
      attr(object, "id") <- id
      attr(object, "pool") <- self
      assign(id, object, envir = envir)
      return(object)
    },
    ## change the objects's environment when a
    ## free object gets taken and vice versa
    toggleObjectStatus = function(id, from, to) {
      object <- from[[id]]
      rm(list = id, envir = from)
      assign(id, object, envir = to)
    }
  )
)


#' S4 class for compatibility with DBI methods
#' @export
setClass("Pool")

#' @export
setGeneric("createPool",
  function(drv, minSize = 3, maxSize = Inf, ...) {
    standardGeneric("createPool")
  }
)

#' @export
setGeneric("closePool", function(pool) {
  standardGeneric("closePool")
})

#' @export
setMethod("closePool", "Pool", function(pool) {
  pool$close()
})

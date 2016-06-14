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
    freeObjCounter = NULL,
    takenObjCounter = NULL,
    leakedObjCounter = NULL, ### Does this make sense??
    ## initialize the pool with min number of objects
    initialize = function(factory, minSize, maxSize) {
      self$valid <- TRUE
      self$freeObjCounter <- 0
      self$takenObjCounter <- 0
      self$leakedObjCounter <- 0
      private$factory <- factory
      private$minSize <- minSize
      private$maxSize <- maxSize
      private$freeObjects <- new.env(parent = emptyenv())
      #private$takenObjects <- new.env(parent = emptyenv())
      private$leakedObjects <- new.env(parent = emptyenv())
      for (i in seq_len(private$minSize)) {
        private$createObject()
      }
      reg.finalizer(self,
        function(self) {
          freeEnv <- private$freeObjects
          #takenEnv <- private$takenObjects
          #if (length(freeEnv) > 0 || length(takenEnv) > 0) { ### NEED TO RETHINK THIS
          if (length(freeEnv) > 0) {
            warning("Closing leaked connections")
            self$close()
          }
        },
        onexit = TRUE)
    },
    ## calls activate and returns an object
    fetch = function() {                       #### WHAT TO DO IF THIS THROWS AN ERROR???
      freeEnv <- private$freeObjects
      #takenEnv <- private$takenObjects
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
      private$toggleObjectStatus(id, object, "free", "taken")
      onActivate(object)

      canary <- new.env(parent = emptyenv())
      attr(object, "canary") <- canary
      reg.finalizer(canary, function(e) {
        if (length(self$takenObjCounter) > 0) {
          warning("You have leaked connections. Closing them...")
          assign(id, object, envir = private$leakedObjects)
          self$takenObjCounter <- self$takenObjCounter - 1
          self$leakedObjCounter <- self$leakedObjCounter + 1
        }
        # Warn if not closed properly
      })

      return(object)
    },
    ## passivates the object and returns it back to
    ## the pool (possibly destroys the object if the
    ## number of total object exceeds the maximum)
    release = function(id, object) {                      #### WHAT TO DO IF THIS THROWS AN ERROR???
      freeEnv <- private$freeObjects
      #takenEnv <- private$takenObjects
      #object <- takenEnv[[id]]
      onPassivate(object)
      private$toggleObjectStatus(id, object, "taken", "free")
      if (length(freeEnv) > private$maxSize) {
        onDestroy(object) #, envir = freeEnv)
        rm(id, envir = freeEnv)
      }
    },
    ## cleaning up and closing the pool
    close = function() {                      #### WHAT TO DO IF THIS THROWS AN ERROR???
      self$valid <- FALSE
      freeEnv <- private$freeObjects
      #takenEnv <- private$takenObjects
      ## destroy all objects
      eapply(freeEnv, onDestroy)
      #eapply(takenEnv, onDestroy)
      ## empty the objects' environments
      rm(list = ls(freeEnv), envir = freeEnv)
      #rm(list = ls(takenEnv), envir = takenEnv)
      ## TODO: deal with leaked connections, since there's no longer a takenEnv to check
    }
  ),
  private = list(
    freeObjects = NULL,
    #takenObjects = NULL,
    leakedObjects = NULL,
    factory = NULL,
    minSize = NULL,
    maxSize = NULL,
    ## creates an object, assigns it to the
    ## free environment and returns it
    createObject = function() {
      ## always create an object in the free envir
      ## to guarantee that ids are unique
      freeEnv <- private$freeObjects
      id <- as.character(length(freeEnv) + 1)
      object <- private$factory()
      attr(object, "id") <- id
      attr(object, "pool") <- self
      assign(id, object, envir = freeEnv)
      self$freeObjCounter <- self$freeObjCounter + 1
      return(object)
    },
    ## change the objects's environment when a
    ## free object gets taken and vice versa
    toggleObjectStatus = function(id, object, from, to) {
      freeEnv <- private$freeObjects
      if (from == "free") {
        #object <- freeEnv[[id]]
        rm(list = id, envir = freeEnv)
        #assign(id, object, envir = to)
        self$freeObjCounter <- self$freeObjCounter - 1
        self$takenObjCounter <- self$takenObjCounter + 1
      } else if (from == "taken") {
        assign(id, object, envir = freeEnv)
        self$freeObjCounter <- self$freeObjCounter + 1
        self$takenObjCounter <- self$takenObjCounter - 1
      } else {
        stop("`from` must be wither 'free' or 'taken'")
      }
    }
  )
)


#' S4 class for compatibility with DBI methods
#' @export
setClass("Pool")

## documented manually, with the Pool object
#' @export
setGeneric("poolCreate",
  function(src, minSize = 3, maxSize = Inf, ...) {
    standardGeneric("poolCreate")
  }
)

#' Checks out an object from the pool.
#'
#' Should be called by the end user if they need a persistent
#' object, that is not returned to the pool automatically.
#' When you don't longer need the object, be sure to return it
#' to the pool using \code{poolReturn(object)}.
#'
#' @param pool The pool to get the object from.
#'
#' @aliases poolCheckout,Pool-method
#' @export
setGeneric("poolCheckout", function(pool) {
  standardGeneric("poolCheckout")
})

#' @export
setMethod("poolCheckout", "Pool", function(pool) {
  pool$fetch()
})

#' Returns an object back to the pool.
#'
#' Should be called by the end user if they previously fetched
#' an object directly using \code{object <- poolCheckout(pool)}
#' and are now done with said object.
#'
#' @param object A pooled object.
#'
#' @aliases poolReturn,ANY-method
#' @export
setGeneric("poolReturn", function(object) {
  standardGeneric("poolReturn")
})

#' @export
setMethod("poolReturn", "ANY", function(object) {
  id <- attr(object, "id", exact = TRUE)
  pool <- attr(object, "pool", exact = TRUE)
  pool$release(id, object)
})

## documented manually, with the Pool object
#' @export
setGeneric("poolClose", function(pool) {
  standardGeneric("poolClose")
})

#' @export
setMethod("poolClose", "Pool", function(pool) {
  pool$close()
})

#' Show method
#' @param object A Pool object.
#' @export
setMethod("show", "Pool", function(object) {
  pooledObj <- poolCheckout(object)
  on.exit(poolReturn(pooledObj))
  cat("<Pool>\n", "  pooled object class: ",
      is(pooledObj)[1], sep = "")
})

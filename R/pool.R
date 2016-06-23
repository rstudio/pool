#' @include utils.R
#' @include scheduler.R
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
    counters = NULL,
    minSize = NULL,
    maxSize = NULL,
    idleTimeout = NULL,
    ## initialize the pool with min number of objects
    initialize = function(factory, minSize, maxSize,
                          idleTimeout) {
      self$valid <- TRUE

      self$counters <- new.env(parent = emptyenv())
      self$counters$free <- 0
      self$counters$taken <- 0

      private$factory <- factory
      self$minSize <- minSize
      self$maxSize <- maxSize
      self$idleTimeout <- idleTimeout

      private$freeObjects <- new.env(parent = emptyenv())

      for (i in seq_len(self$minSize)) {
        private$createObject()
      }
      reg.finalizer(self,
        function(self) {
          protectDefaultScheduler({
            freeEnv <- private$freeObjects
            if (length(freeEnv) > 0) {
              self$close()
            }
          })
        },
        onexit = TRUE)
    },
    ## calls activate and returns an object
    fetch = function() {                       #### WHAT TO DO IF THIS THROWS AN ERROR???
      protectDefaultScheduler({
        if (self$counters$free + self$counters$taken >= self$maxSize) {
          stop("Maximum number of objects in pool has been reached")
        }

        freeEnv <- private$freeObjects
        ## see if there's any free objects
        if (length(freeEnv) > 0) {
          ## get first free object we find
          id <- ls(freeEnv)[[1]]
          object <- freeEnv[[id]]

          taskHandle <- attr(object, "reapTaskHandle", exact = TRUE)
          if (!is.null(taskHandle)) {
            attr(object, "reapTaskHandle") <- NULL
            # Cancel the reap task.
            taskHandle()
          }

        } else {
          ## if we get here, there are no free objects
          ## and we must create a new one
          object <- private$createObject()
          id <- attr(object, "id", exact = TRUE)
        }
        ## activate object and return it
        private$changeObjectStatus(id, object, "free", "taken")
        onActivate(object)
        if (!onValidate(object)) {
          stop("Object activation was not successful")
        }

        return(object)
      })
    },
    ## passivates the object and returns it back to
    ## the pool (possibly destroys the object if the
    ## number of total object exceeds the maximum)
    release = function(id, object) {                      #### WHAT TO DO IF THIS THROWS AN ERROR???
      protectDefaultScheduler({
        freeEnv <- private$freeObjects
        onPassivate(object)
        private$changeObjectStatus(id, object, "taken", "free")

        taskHandle <- scheduleTask(self$idleTimeout, function() {
          if (self$counters$free + self$counters$taken > self$minSize) {
            private$changeObjectStatus(id, object, "free", NULL)
            private$destroyObject(object)
          }
        })
        attr(object, "reapTaskHandle") <- taskHandle
      })
    },
    ## cleaning up and closing the pool
    close = function() {                      #### WHAT TO DO IF THIS THROWS AN ERROR???
      self$valid <- FALSE
      freeEnv <- private$freeObjects
      ## destroy all objects
      eapply(freeEnv, private$destroyObject)
      ## empty the objects' environments
      rm(list = ls(freeEnv), envir = freeEnv)
      ## TODO: deal with leaked connections, since there's no longer a takenEnv to check
    }
  ),
  private = list(
    freeObjects = NULL,
    factory = NULL,
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

      # Leak detection logic
      canary <- new.env(parent = emptyenv())
      canary$closed <- FALSE
      attr(object, "canary") <- canary
      ## maybe calling gc() ins't necessary (see issue #3 in Github)
      gc()
      reg.finalizer(canary, function(e) {
        protectDefaultScheduler({
          if (!canary$closed) {
            ## for dplyr use (as of right now), leaking is inevitable
            ## and it will annoy the heck out of users to be getting
            ## warnings when they're doing exactly as we tell them to
            ## so commenting this out for now...
            # warning("You have leaked pooled objects. Closing them.")
            scheduleTask(1, function() {
              ## changed because in issue #4 in Github, I really think
              ## this makes more sense...
              self$release(id, object)
              #message("Connection finalizer ran")

              # private$changeObjectStatus(id, object, "taken", NULL)
              # private$destroyObject(object)
            })
          }
        })
      })

      private$changeObjectStatus(id, object, NULL, "free")
      return(object)
    },
    destroyObject = function(object) {
      canary <- attr(object, "canary", exact = TRUE)
      if (canary$closed) {
        warning("Object was destroyed twice")
      }
      canary$closed <- TRUE
      onDestroy(object)
    },
    ## change the objects's environment when a
    ## free object gets taken and vice versa.
    ## Valid values for `from` and `to` are:
    ## NULL, "free", "taken"
    changeObjectStatus = function(id, object, from, to) {
      # Remove from environment if necessary, and
      # decrement counter
      if (!is.null(from)) {
        removeFrom <- switch(from,
          free = private$freeObjects,
          NULL
        )
        if (!is.null(removeFrom)) {
          rm(list = id, envir = removeFrom)
        }
        self$counters[[from]] <- self$counters[[from]] - 1
      }

      if (!is.null(to)) {
        # Add to environment if necessary, and increment counter
        addTo <- switch(to,
          free = private$freeObjects,
          NULL
        )
        if (!is.null(addTo)) {
          assign(id, object, envir = addTo)
        }
        self$counters[[to]] <- self$counters[[to]] + 1
      }
    }
  )
)


#' S4 class for compatibility with DBI methods
#' @export
setClass("Pool")

## documented manually, with the Pool object
#' @export
poolCreate <- function(factory, ...,
  minSize = 1, maxSize = Inf, idleTimeout = 60000) {
  pool <- Pool$new(
    function() {factory(...)},
    minSize, maxSize, idleTimeout
  )
  ## if a createPool is used with a DBIDriver, make a note
  ## (to ease dplyr compatibility later on)
  checkDriver(pool, ...)
  pool
}

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

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
    validateTimeout = NULL,
    validateQuery = NULL,

    ## initialize the pool with min number of objects
    initialize = function(factory, minSize, maxSize,
                          idleTimeout, validateTimeout,
                          validateQuery) {
      self$valid <- TRUE

      self$counters <- new.env(parent = emptyenv())
      self$counters$free <- 0
      self$counters$taken <- 0
      private$idCounter <- 1

      private$factory <- factory
      self$minSize <- minSize
      self$maxSize <- maxSize
      self$idleTimeout <- idleTimeout
      self$validateTimeout <- validateTimeout
      self$validateQuery <- validateQuery
      private$validateTimer <- Sys.time()

      private$freeObjects <- new.env(parent = emptyenv())

      for (i in seq_len(self$minSize)) {
        private$createObject()
      }
      reg.finalizer(self,
        function(self) {
          if (self$valid) {
            warning("Closing leaked pool.")
            self$close()
          }
        },
        onexit = TRUE)
    },

    ## calls activate and returns an object
    fetch = function() {
      if (!self$valid) {
        stop("This pool is no longer valid. Cannot fetch new objects.")
      }
      naiveScheduler$protect({
        if (self$counters$free + self$counters$taken >= self$maxSize) {
          stop("Maximum number of objects in pool has been reached")
        }

        ## see if there's any free objects
        freeEnv <- private$freeObjects
        if (length(freeEnv) > 0) {
          id <- ls(freeEnv)[[1]]  ## get first free object we find
          object <- freeEnv[[id]]
          private$cancelReapTask(object)  ## cancel reap task if it exists

        } else {
          ## if we get here, there are no free objects
          ## and we must create a new one
          object <- private$createObject()
          id <- attr(object, "..metadata", exact = TRUE)$..id
        }

        private$changeObjectStatus(id, object, "free", "taken")

        ## activate and validate
        tryCatch({
          onActivate(object)
          private$validate(object)
        }, error = function(e) {

          tryCatch({
            ## cleanup
            state <- attr(object, "..metadata", exact = TRUE)$..state
            private$changeObjectStatus(id, object, state, NULL)

            ## try again
            object <- private$createObject()
            id <- attr(object, "..metadata", exact = TRUE)$..id
            private$changeObjectStatus(id, object, "free", "taken")
            onActivate(object)
            onValidate(object, self$validateQuery)  ## skip timeout here
          },
          error = function(e) {
            ## cleanup
            state <- attr(object, "..metadata", exact = TRUE)$..state
            private$changeObjectStatus(id, object, state, NULL)

            stop("Object does not appear to be valid. ",
                 "Error message: ", conditionMessage(e),
                 call. = FALSE)
          })
        })
        return(object)
      })
    },

    ## passivates the object and returns it back to the pool
    ## (sets up task to destroy the object if the number of
    ## total objects exceeds the minimum)
    release = function(id, object) {
      ..metadata <- attr(object, "..metadata", exact = TRUE)
      if (..metadata$..state == "free") {
        stop("This object was already returned to the pool.")
      }
      if (is.null(..metadata) || !..metadata$..valid) {
        stop("Invalid object.")
      }

      naiveScheduler$protect({
        tryCatch({
          onPassivate(object)

          ## immediately destroy object if pool has already been closed
          if (!self$valid) {
            private$changeObjectStatus(id, object, "taken", NULL)
          } else {
            taskHandle <- scheduleTask(
              self$idleTimeout, function() {
                if (self$counters$free + self$counters$taken > self$minSize) {
                  private$changeObjectStatus(id, object, "free", NULL)
                }
              }
            )
            attr(object, "..metadata")$..reapTaskHandle <- taskHandle
            private$changeObjectStatus(id, object, "taken", "free")
          }
        }, error = function(e) {
          state <- attr(object, "..metadata", exact = TRUE)$..state
          private$changeObjectStatus(id, object, state, NULL)
          warning("Object could not be returned back to the pool. ",
                  "It was destroyed instead. Error message: ",
                  conditionMessage(e))
        })
      })
    },

    ## cleaning up and closing the pool -- after the pool
    ## is closed, objects that were previously checked out
    ## can still be returned to the pool (which will
    ## immediately destroy them). Objects can no longer be
    ## checked out from the pool.
    close = function() {
      naiveScheduler$protect({
        if (!self$valid) stop("The pool was already closed.")

        self$valid <- FALSE
        freeEnv <- private$freeObjects
        freeObs <- ls(freeEnv)

        ## destroy all free objects
        for (id in freeObs) {
          obj <- freeEnv[[id]]
          private$cancelReapTask(obj)
          private$changeObjectStatus(id, obj, "free", NULL)
        }

        # check if there are taken objects
        if (self$counters$taken > 0) {
          warning("You still have checked out objects. Return ",
                  "them to the pool so they can de destroyed. ",
                  "(If these are leaked objects - no reference ",
                  "- they will be destroyed the next time the ",
                  "garbage collector runs).")
        }
      })
    }
  ),
  private = list(

    freeObjects = NULL,
    factory = NULL,
    idCounter = NULL,
    validateTimer = NULL,

    validate = function(object) {
      now <- Sys.time()
      interval <- (now - private$validateTimer) * 1000
      if (interval >= self$validateTimeout) {
        onValidate(object, self$validateQuery)
        private$validateTimer <- now
      }
    },

    ## creates an object, assigns it to the
    ## free environment and returns it
    createObject = function() {
      object <- private$factory()
      if (is.null(object)) {
        stop("Object creation was not successful. The `factory` ",
             "argument must be a function that creates and ",
             "returns the object to be pooled.")
      }

      ## attach metadata about the object
      ..metadata <- new.env(parent = emptyenv())
      attr(object, "..metadata") <- ..metadata

      id <- as.character(private$idCounter)
      private$idCounter <- private$idCounter + 1
      ..metadata$..id <- id
      ..metadata$..pool <- self
      ..metadata$..valid <- TRUE
      ..metadata$..state <- "free"

      ## detect leaked connections
      reg.finalizer(..metadata, function(e) {
        naiveScheduler$protect({
          if (..metadata$..valid) {
            warning("You have a leaked pooled object. Destroying it.")
            scheduleTask(1, function() {
              state <- attr(object, "..metadata", exact = TRUE)$..state
              private$changeObjectStatus(id, object, state, NULL)
            })
          }
        })
      })

      private$changeObjectStatus(id, object, NULL, "free")
      return(object)
    },

    destroyObject = function(object) {
      tryCatch({
        ..metadata <- attr(object, "..metadata", exact = TRUE)
        if (!..metadata$..valid) {
          warning("Object was destroyed twice.")
          return()
        }
        ..metadata$..valid <- FALSE
        onDestroy(object)
      }, error = function(e) {
        warning("Object of class ", is(object)[1],
                " could not be destroyed properly, ",
                "but was successfully removed from pool. ",
                "Error message: ", conditionMessage(e))

      })
    },

    ## change the objects's environment when a free object
    ## gets taken and vice versa. Valid values for `from`
    ## and `to` are: NULL, "free", "taken"
    changeObjectStatus = function(id, object, from, to) {
      # Remove from environment if necessary, and
      # decrement counter
      if (!is.null(from)) {
        removeFrom <- switch(from,
          free = private$freeObjects,
          NULL
        )
        if (!is.null(removeFrom)) {
          if (exists(id, envir = removeFrom)) {
            rm(list = id, envir = removeFrom)
          } else {
            stop("The object could not be found.")
          }
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
        ..metadata <- attr(object, "..metadata", exact = TRUE)
        ..metadata$..state <- to
      } else {
        ## if `to` == NULL, it means destroy the object
        private$destroyObject(object)
      }
    },

    cancelReapTask = function(object) {
      ..metadata <- attr(object, "..metadata", exact = TRUE)
      taskHandle <- ..metadata$..reapTaskHandle
      if (!is.null(taskHandle)) {
        ..metadata$..reapTaskHandle <- NULL
        taskHandle()   ## cancel the previous reap task
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
  minSize = 1, maxSize = Inf,
  idleTimeout = 60000, validateTimeout = 1000,
  validateQuery = NULL) {
  pool <- Pool$new(
    function() {factory(...)},
    minSize, maxSize,
    idleTimeout, validateTimeout, validateQuery
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
  ..metadata <- attr(object, "..metadata", exact = TRUE)
  if (is.null(..metadata) || !..metadata$..valid) {
    stop("Invalid object.")
  }
  id <- ..metadata$..id
  pool <- ..metadata$..pool
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

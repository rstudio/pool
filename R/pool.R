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
    validationInterval = NULL,
    state = NULL,

    ## initialize the pool with min number of objects
    initialize = function(factory, minSize, maxSize,
                          idleTimeout, validationInterval,
                          state) {
      naiveScheduler$protect({
        self$valid <- TRUE

        self$counters <- new.env(parent = emptyenv())
        self$counters$free <- 0
        self$counters$taken <- 0
        private$idCounter <- 1

        private$factory <- factory
        self$minSize <- minSize
        self$maxSize <- maxSize

        self$idleTimeout <- idleTimeout
        self$validationInterval <- validationInterval
        self$state <- state

        private$freeObjects <- new.env(parent = emptyenv())

        for (i in seq_len(self$minSize)) {
          private$createObject()
        }
        reg.finalizer(self,
          function(self) {if (self$valid) self$close()},
          onexit = TRUE)
      })
    },

    ## calls activate and returns an object
    fetch = function() {
      naiveScheduler$protect({
        if (!self$valid) {
          stop("This pool is no longer valid. Cannot fetch new objects.")
        }
        if (self$counters$free + self$counters$taken >= self$maxSize) {
          stop("Maximum number of objects in pool has been reached")
        }

        ## see if there's any free objects
        freeEnv <- private$freeObjects
        if (length(freeEnv) > 0) {
          id <- ls(freeEnv)[[1]]  ## get first free object we find
          object <- freeEnv[[id]]
          ## cancel reap task if it exists
          private$cancelScheduledTask(object, "destroyHandle")

        } else {
          ## if we get here, there are no free objects
          ## and we must create a new one
          object <- private$createObject()
        }

        private$cancelScheduledTask(object, "validateHandle")
        ## call onActivate, onValidate and change object status
        object <- private$checkValid(object)
        private$changeObjectStatus(object, "taken")

      })
      return(object)
    },

    ## passivates the object and returns it back to the pool
    ## (sets up task to destroy the object if the number of
    ## total objects exceeds the minimum)
    release = function(object) {
      naiveScheduler$protect({
        pool_metadata <- attr(object, "pool_metadata", exact = TRUE)
        if (pool_metadata$state == "free") {
          stop("This object was already returned to the pool.")
        }
        if (is.null(pool_metadata) || !pool_metadata$valid) {
          stop("Invalid object.")
        }
        ## immediately destroy object if pool has already been closed
        if (!self$valid) {
          private$changeObjectStatus(object, NULL)
          return()
        }

        ## passivate object (or if that fails, destroy it and throw)
        tryCatch({
          onPassivate(object)
        }, error = function(e) {
          private$changeObjectStatus(object, NULL)
          stop("Object could not be returned back to the pool. ",
               "It was destroyed instead. Error message: ",
               conditionMessage(e))
        })

        ## set up a task to destroy the object after `idleTimeout`
        ## millis, if we're over the minimum number of objects
        taskHandle <- scheduleTask(
          self$idleTimeout, function() {
            if (self$counters$free + self$counters$taken > self$minSize) {
              private$changeObjectStatus(object, NULL)
            }
          }
        )
        pool_metadata$destroyHandle <- taskHandle
        private$changeObjectStatus(object, "free")

        ## set up recurring validation every `validationInterval` millis
        ## so we can catch if an idle connection gets broken somehow
        ## (but only if we have a proper scheduler)
        if (!(is.null(getOption("pool.scheduler", NULL)))) {
          pool_metadata$validateHandle <-
            scheduleTaskRecurring(self$validationInterval, function() {
              object <- private$checkValid(object)
              ## if we got here, the object was successfully
              ## activated and validated; now needs to be passivated
              onPassivate(object)
            })
        }
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
          private$changeObjectStatus(freeEnv[[id]], NULL)
        }

        # check if there are taken objects
        if (self$counters$taken > 0) {
          warning("You still have checked out objects. Return ",
                  "them to the pool so they can be destroyed. ",
                  "(If these are leaked objects - no reference ",
                  "- they will be destroyed the next time the ",
                  "garbage collector runs).", call. = FALSE)
        }
      })
    }
  ),

  private = list(

    freeObjects = NULL,
    factory = NULL,
    idCounter = NULL,

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
      pool_metadata <- new.env(parent = emptyenv())
      attr(object, "pool_metadata") <- pool_metadata

      id <- as.character(private$idCounter)
      private$idCounter <- private$idCounter + 1
      pool_metadata$id <- id
      pool_metadata$pool <- self
      pool_metadata$valid <- TRUE
      pool_metadata$state <- NULL
      pool_metadata$lastValidated <- NULL

      ## detect leaked connections and destroy them
      reg.finalizer(pool_metadata, function(e) {
        naiveScheduler$protect({
          if (pool_metadata$valid) {
            warning("You have a leaked pooled object. Destroying it.")
            naiveScheduler$schedule(1, function() {
              private$changeObjectStatus(object, NULL)
            })
          }
        })
      },
      onexit = TRUE)

      private$changeObjectStatus(object, "free")
      return(object)
    },

    ## tries to run onDestroy
    destroyObject = function(object) {
      tryCatch({
        pool_metadata <- attr(object, "pool_metadata", exact = TRUE)
        if (!pool_metadata$valid) {
          warning("Object was destroyed twice.")
          return()
        }
        pool_metadata$valid <- FALSE
        private$cancelScheduledTask(object, "validateHandle")
        private$cancelScheduledTask(object, "destroyHandle")
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
    changeObjectStatus = function(object, to) {
      pool_metadata <- attr(object, "pool_metadata", exact = TRUE)
      id <- pool_metadata$id
      from <- pool_metadata$state

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
        pool_metadata$state <- to
      } else {
        ## if `to` == NULL, it means destroy the object
        private$destroyObject(object)
      }
    },

    cancelScheduledTask = function(object, task) {
      pool_metadata <- attr(object, "pool_metadata", exact = TRUE)
      taskHandle <- pool_metadata[[task]]
      if (!is.null(taskHandle)) {
        pool_metadata[[task]] <- NULL
        taskHandle()   ## cancel the previous task
      }
    },

    ## try to validate + activate an object; if that fails,
    ## destroy the object and run whatever more cleanup is
    ## necessary (provided through `errorFun`)
    checkValidTemplate = function(object, errorFun) {
      tryCatch({
        onActivate(object)
        private$validate(object)
        return(object)

      }, error = function(e) {
        private$changeObjectStatus(object, NULL)
        errorFun(e)
      })
    },

    ## tries to validate + activate the object; if that fails,
    ## the first time around, warn, destroy that object and try
    ## again with a new object; **returns** the object
    ## if both tries fail, throw an error
    checkValid = function(object) {
      object <- private$checkValidTemplate(object,
        function(e) {
          warning("It wasn't possible to activate and/or validate ",
                  "the object. Trying again with a new object.",
                  call. = FALSE)

          private$checkValidTemplate(private$createObject(),
            function(e) {
              stop("Object does not appear to be valid. ",
                   "Error message: ", conditionMessage(e),
                   call. = FALSE)
          })
      })
      return(object)
    },

    ## run onValidate on the object only if over `validationInterval`
    ## millis have passed since the last validation (this allows
    ## us some performance gains)
    validate = function(object) {
      pool_metadata <- attr(object, "pool_metadata", exact = TRUE)
      lastValidated <- pool_metadata$lastValidated
      ## if the object has never been validated, set `lastValidated`
      ## to guarantee that it will be validated now
      if (is.null(lastValidated)) {
        lastValidated <- Sys.time() - self$validationInterval - 1
      }
      interval <- (Sys.time() - lastValidated) * 1000
      if (interval >= self$validationInterval) {
        onValidate(object)
        pool_metadata$lastValidated <- Sys.time()
      }
    }
  )
)

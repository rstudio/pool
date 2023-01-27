#' @include scheduler.R
NULL

#' @export
Pool <- R6::R6Class("Pool",
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
      idleTimeout, validationInterval, state, error_call = caller_env()) {
        self$valid <- TRUE

        self$counters <- new.env(parent = emptyenv())
        self$counters$free <- 0
        self$counters$taken <- 0
        private$idCounter <- 1

        if (!is.function(factory)) {
          abort("`factory` must be a function.", call = error_call)
        }
        private$factory <- factory
        self$minSize <- minSize
        self$maxSize <- maxSize

        self$idleTimeout <- idleTimeout
        self$validationInterval <- validationInterval
        self$state <- state

        private$freeObjects <- new.env(parent = emptyenv())

        for (i in seq_len(self$minSize)) {
          private$createObject(error_call = error_call)
        }
    },

    ## calls activate and returns an object
    fetch = function(error_call = caller_env()) {
      if (!self$valid) {
        abort(
          "This pool is no longer valid. Cannot fetch new objects.",
          call = error_call
        )
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
      object <- private$checkValid(object, error_call = error_call)
      private$changeObjectStatus(object, "taken")

      return(object)
    },

    ## passivates the object and returns it back to the pool
    ## (sets up task to destroy the object if the number of
    ## total objects exceeds the minimum)
    release = function(object) {
      pool_metadata <- attr(object, "pool_metadata", exact = TRUE)
      if (pool_metadata$state == "free") {
        abort("This object was already returned to the pool.")
      }
      if (is.null(pool_metadata) || !pool_metadata$valid) {
        abort("Invalid object.")
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
        abort(
          c(
            "Object could not be returned back to the pool.",
            "It was destroyed instead"
          ),
          parent = e
        )
      })

      ## set up a task to destroy the object after `idleTimeout`
      ## secs, if we're over the minimum number of objects
      taskHandle <- later::later(
        function() {
          if (self$counters$free + self$counters$taken > self$minSize) {
            private$changeObjectStatus(object, NULL)
          }
        },
        self$idleTimeout
      )
      pool_metadata$destroyHandle <- taskHandle
      private$changeObjectStatus(object, "free")

      ## set up recurring validation every `validationInterval` secs
      ## so we can catch if an idle connection gets broken somehow
      pool_metadata$validateHandle <- scheduleTaskRecurring(function() {
          object <- private$checkValid(object)
          ## if we got here, the object was successfully
          ## activated and validated; now needs to be passivated
          onPassivate(object)
        }, self$validationInterval)
    },

    ## cleaning up and closing the pool -- after the pool
    ## is closed, objects that were previously checked out
    ## can still be returned to the pool (which will
    ## immediately destroy them). Objects can no longer be
    ## checked out from the pool.
    close = function() {
      if (!self$valid) abort("The pool was already closed.")

      self$valid <- FALSE
      freeEnv <- private$freeObjects
      freeObs <- ls(freeEnv)

      ## destroy all free objects
      for (id in freeObs) {
        private$changeObjectStatus(freeEnv[[id]], NULL)
      }

      # check if there are taken objects
      if (self$counters$taken > 0) {
        pool_warn(c(
          "You still have checked out objects.",
          "Use `poolReturn()` them to the pool so they can be destroyed."
        ))
      }
    }
  ),

  private = list(

    freeObjects = NULL,
    factory = NULL,
    idCounter = NULL,

    ## creates an object, assigns it to the
    ## free environment and returns it
    createObject = function(error_call = parent.frame()) {
      if (self$counters$free + self$counters$taken >= self$maxSize) {
        abort("Maximum number of objects in pool has been reached", call = error_call)
      }

      object <- private$factory()
      if (is.null(object)) {
        abort(
          c(
            "Object creation failed.",
            "The `factory` must not return `NULL`"
          ),
          call = error_call
        )
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
        if (pool_metadata$valid) {
          pool_warn(c(
            "Checked-out object deleted before being returned.",
            "Make sure to `poolReturn()` all objects retrieved with `poolCheckout().`"
          ))
          self$release(object)
        }
      }, onexit = TRUE)

      private$changeObjectStatus(object, "free")
      return(object)
    },

    ## tries to run onDestroy
    destroyObject = function(object) {
      tryCatch({
        pool_metadata <- attr(object, "pool_metadata", exact = TRUE)
        if (!pool_metadata$valid) {
          pool_warn("object was destroyed twice.")
          return()
        }
        pool_metadata$valid <- FALSE
        private$cancelScheduledTask(object, "validateHandle")
        private$cancelScheduledTask(object, "destroyHandle")
        onDestroy(object)
      }, error = function(e) {
        pool_warn(c(
          "Object could not be destroyed, but was removed from the pool.",
          "Error message:",
          prefix(conditionMessage(e), "  ")
        ))
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
            abort("Object could not be found.")
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
    checkValid = function(object, error_call = caller_env()) {
      object <- private$checkValidTemplate(object,
        function(e) {
          pool_warn(c(
            "Failed to activate and/or validate existing object.",
            "Trying again with a new object"
          ))

          private$checkValidTemplate(private$createObject(),
            function(e) {
              abort(
                "Object does not appear to be valid.",
                call = error_call,
                parent = e
              )
            })
        })
      return(object)
    },

    ## run onValidate on the object only if over `validationInterval`
    ## secs have passed since the last validation (this allows
    ## us some performance gains)
    validate = function(object) {
      pool_metadata <- attr(object, "pool_metadata", exact = TRUE)
      lastValidated <- pool_metadata$lastValidated
      ## if the object has never been validated, set `lastValidated`
      ## to guarantee that it will be validated now
      if (is.null(lastValidated)) {
        lastValidated <- Sys.time() - self$validationInterval
      }
      interval <- difftime(Sys.time(), lastValidated, units = "secs")

      if (interval >= self$validationInterval) {
        onValidate(object)
        pool_metadata$lastValidated <- Sys.time()
      }
    }
  )
)


pool_warn <- function(messages) {
  file <- if (is_testing()) stdout() else stderr()

  out <- paste0(messages, "\n", collapse = "")
  cat(prefix(out, "<pool> "), file = file)
}
prefix <- function(x, prefix) {
  gsub("(?m)^", prefix, x, perl = TRUE)
}
is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
}

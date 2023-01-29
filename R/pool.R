#' @export
Pool <- R6::R6Class("Pool",
  public = list(

    objClass = NULL,
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

        self$objClass <- NULL

        private$freeObjects <- new.env(parent = emptyenv())

        for (i in seq_len(self$minSize)) {
          private$createObject(error_call = error_call)
        }
    },

    ## calls activate and returns an object
    fetch = function(error_call = caller_env()) {
      private$checkValid(error_call)

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
        object <- private$createObject(error_call = error_call)
      }

      private$cancelScheduledTask(object, "validateHandle")
      ## call onActivate, onValidate and change object status
      object <- private$checkObjectValid(object, error_call = error_call)
      private$changeObjectStatus(object, "taken")

      return(object)
    },

    ## passivates the object and returns it back to the pool
    ## (sets up task to destroy the object if the number of
    ## total objects exceeds the minimum)
    release = function(object, error_call = caller_env()) {
      pool_metadata <- pool_metadata(object, error_call = error_call())
      if (pool_metadata$state == "free") {
        abort("This object was already returned to the pool.", call = error_call)
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
          call = error_call,
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
    },

    ## cleaning up and closing the pool -- after the pool
    ## is closed, objects that were previously checked out
    ## can still be returned to the pool (which will
    ## immediately destroy them). Objects can no longer be
    ## checked out from the pool.
    close = function(error_call = parent.frame()) {
      private$checkValid(error_call)

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
    },

    print = function(...) {
      cat("<Pool> of ", self$objClass %||% "unknown", " objects\n", sep = "")
      cat("  Objects checked out: ", self$counters$taken, "\n", sep = "")
      cat("  Available in pool: ", self$counters$free, "\n", sep = "")
      cat("  Max size: ", self$maxSize, "\n", sep = "")
      cat("  Valid: ", self$valid, "\n", sep = "")
      invisible(self)
    }
  ),

  private = list(

    freeObjects = NULL,
    factory = NULL,
    idCounter = NULL,

    checkValid = function(error_call = parent.frame()) {
      if (!self$valid) {
        abort("The pool has been closed.", call = error_call)
      }
    },

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

      if (is.null(self$objClass)) {
        self$objClass <- class(object)
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
      # force validation to happen immediately to surface any issues
      pool_metadata$lastValidated <- Sys.time() - self$validationInterval - 1

      ## detect leaked connections and destroy them
      reg.finalizer(pool_metadata, function(e) {
        if (e$valid) {
          pool_warn(c(
            "Checked-out object deleted before being returned.",
            "Make sure to `poolReturn()` all objects retrieved with `poolCheckout().`"
          ))
        }
      }, onexit = TRUE)

      private$changeObjectStatus(object, "free")
      return(object)
    },

    ## tries to run onDestroy
    destroyObject = function(object) {
      pool_metadata <- pool_metadata(object, check_valid = FALSE)
      if (!pool_metadata$valid) {
        pool_warn("Object was destroyed twice.")
        return()
      }

      pool_metadata$valid <- FALSE
      private$cancelScheduledTask(object, "validateHandle")
      private$cancelScheduledTask(object, "destroyHandle")

      tryCatch({
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
      pool_metadata <- pool_metadata(object)
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
      pool_metadata <- pool_metadata(object, check_valid = FALSE)
      taskHandle <- pool_metadata[[task]]
      if (!is.null(taskHandle)) {
        pool_metadata[[task]] <- NULL
        taskHandle()   ## cancel the previous task
      }
    },

    ## tries to validate + activate the object; if that fails,
    ## warn, destroy that object and try once more
    ## if second attempt fails, throw an error
    checkObjectValid = function(object, error_call = caller_env()) {
      tryCatch(
        {
          private$activateAndValidate(object)
          return(object)
        },
        error = function(e) {}
      )

      pool_warn(c(
        "Failed to activate and/or validate existing object.",
        "Trying again with a new object."
      ))
      private$changeObjectStatus(object, NULL)
      object <- private$createObject()

      withCallingHandlers(
        private$activateAndValidate(object),
        error = function(e) {
          private$changeObjectStatus(object, NULL)
          abort(
            "Freshly created object does not appear to be valid.",
            call = error_call,
            parent = e
          )
        }
      )
      object
    },

    activateAndValidate = function(object) {
      onActivate(object)
      private$validate(object)
    },

    ## run onValidate on the object only if over `validationInterval`
    ## secs have passed since the last validation (this allows
    ## us some performance gains)
    validate = function(object) {
      pool_metadata <- pool_metadata(object)
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

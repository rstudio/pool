#' @include utils.R
NULL

NaiveScheduler <- R6Class("NaiveScheduler",
  public = list(

    initialize = function() {
      private$scheduledTasks <- new.env(parent = emptyenv())
      private$seq <- 0
      private$refCount <- RefCount$new(private$executeTasks)
    },

    schedule = function(millis, callback) {
      private$seq <- private$seq + 1
      timestamp <- paste0(
        format(Sys.time() + millis/1000, "%Y%m%d-%H%M%OS6-"),
        private$seq
      )
      private$scheduledTasks[[timestamp]] <- callback
      return(function() {
        if (exists(timestamp, envir = private$scheduledTasks)) {
          rm(list = timestamp, envir = private$scheduledTasks)
        }
      })
    },

    protect = function(expr) {
      private$refCount$acquire()
      on.exit(private$refCount$release())
      force(expr)
    }
  ),
  private = list(

    scheduledTasks = NULL,
    seq = NULL,
    refCount = NULL,

    executeTasks = function() {
      while (TRUE) {
        tasks <- sort(ls(private$scheduledTasks))
        if (length(tasks) == 0) break
        task <- private$scheduledTasks[[tasks[[1]]]]
        rm(list = tasks[[1]], envir = private$scheduledTasks)
        task()
      }
    }
  )
)


RefCount <- R6Class("RefCount",
  public = list(

    initialize = function(callback) {
      private$count <- 0
      private$callback <- callback
    },

    acquire = function() {
      private$count <- private$count + 1
    },

    release = function() {
      private$count <- private$count - 1
      if (private$count == 0) {
        private$callback()
      }
    }
  ),
  private = list(
    count = NULL,
    callback = NULL
  )
)


Scheduler <- R6Class("Scheduler",
  public = list(
    naiveScheduler = NULL,

    initialize = function() {
      self$naiveScheduler <- NaiveScheduler$new()
      private$allowRecurring <- FALSE
    },

    scheduleTask = function(millis, callback) {
      scheduler <- private$getScheduler()
      scheduler(millis, callback)
    },

    scheduleRecurringTask = function(millis, callback) {
      ## to set private$allowRecurring
      private$getScheduler()
      if (private$allowRecurring) {
        self$scheduleTask(millis, callback)
        return(self$reschedule(millis, callback))
      } else {
        NULL
      }
    },

    reschedule = function(millis, callback) {
      self$scheduleTask(millis, function() {
        return(self$scheduleRecurringTask(millis, callback))
      })
    }
  ),
  private = list(
    allowRecurring = NULL,

    # "pool.scheduler" should be a function that attempts to
    # run the given callback at, preferably, the given number
    # of millis in the future. The return value should be a
    # function that cancels the task.
    getScheduler = function() {
      scheduler <- getOption("pool.scheduler", NULL)
      if (is.null(scheduler)) {
        scheduler <- self$naiveScheduler$schedule
        private$allowRecurring <- FALSE
      } else {
        private$allowRecurring <- TRUE
      }
      scheduler
    }
  )
)


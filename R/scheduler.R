
Scheduler <- R6Class("Scheduler",
  public = list(
    initialize = function() {
      if (exists("setSchedulerOption")) {
        setSchedulerOption({
          scheduler <- getOption("pool.scheduler", NULL)
          private$allowRecurring = TRUE
        })
      } else {
        scheduler <- naiveScheduleTask  ## eager task scheduling
        private$allowRecurring = FALSE
      }
      private$scheduler <- scheduler
      private$continue <- TRUE
    },

    # pool.scheduler option should be a function that attempts
    # to run the given callback at, preferably, the given number
    # of millis in the future. The return value should be a
    # function that cancels the task.
    scheduleTask = function(millis, callback) {
      private$scheduler(millis, callback)
    },

    scheduleRecurringTask = function(millis, callback) {
      if (private$allowRecurring) {
        self$scheduleTask(millis, callback)
        private$reschedule(millis, callback, private$continue)
      }
    },

    cancelRecurringTasks = function() {
      private$continue <- FALSE
    }

  ),
  private = list(
    scheduler = NULL,
    continue = NULL,
    allowRecurring = NULL,

    reschedule = function(millis, callback, continue) {
      if (continue) {
        self$scheduleTask(millis, function() {
          self$scheduleRecurringTask(millis, callback)
        })
      }
    }
  )
)




## Make this into an R6 class??
scheduledTasks <- new.env(parent = emptyenv())

naiveScheduleTask <- local({
  seq <- 0

  function(millis, callback) {
    seq <<- seq + 1
    timestamp <- paste0(
      format(Sys.time() + millis/1000, "%Y%m%d-%H%M%OS6-"),
      seq
    )
    scheduledTasks[[timestamp]] <- callback
    function() {
      if (exists(timestamp, envir = scheduledTasks)) {
        rm(list = timestamp, envir = scheduledTasks)
      }
    }
  }
})

executeTasks <- function() {
  while (TRUE) {
    tasks <- sort(ls(scheduledTasks))
    if (length(tasks) == 0) {
      break
    }

    task <- scheduledTasks[[tasks[[1]]]]
    rm(list = tasks[[1]], envir = scheduledTasks)

    task()
  }
}

protectDefaultScheduler <- function(expr) {
  defaultSchedulerRefCount$acquire()
  on.exit(defaultSchedulerRefCount$release())

  force(expr)
}

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

defaultSchedulerRefCount <- RefCount$new(executeTasks)

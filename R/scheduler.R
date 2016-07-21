#' @include utils.R
NULL

## A naive scheduler that can keep track of tasks and execute
## them in the right order (ex: a task set for 10 "millis" in
## the future will run after a task set for 5 "millis" in the
## future) -- provided that these are declared in a "protected"
## scope (see `protect` method). However, this scheduler has
## no notion of time and the `millis` parameter defines only
## the order of execution of the tasks, not *when* they are
## actually executed. This behaves eagerly: the tasks are
## executed as soon as possible (either immediately if
## `schedule` is called in an unprotected scope, or right
## after the execution of the `protect` function finishes).
NaiveScheduler <- R6Class("NaiveScheduler",
  public = list(

    initialize = function() {
      private$scheduledTasks <- new.env(parent = emptyenv())
      private$seq <- 0
      private$refCount <- RefCount$new(private$executeTasks)
    },

    schedule = function(millis, callback) {
      if (private$refCount$getCount() == 0) {
        warning("Scheduling a task while executeTasks() ",
                "is still executing")
      }

      ## create a unique id for each task, such that tasks for
      ## further into the future are given ids that place them
      ## after tasks set for earlier (when `sort(ls(scheduledTasks))`
      ## is run)
      private$seq <- private$seq + 1
      timestamp <- paste0(
        format(Sys.time() + millis/1000, "%Y%m%d-%H%M%OS6-"),
        private$seq
      )
      ## add task to the environment of scheduled tasks
      private$scheduledTasks[[timestamp]] <- callback

      ## return value is a function that removes the task from
      ## the scheduled tasks environments (therefore preventing
      ## its future execution)
      return(function() {
        if (exists(timestamp, envir = private$scheduledTasks)) {
          rm(list = timestamp, envir = private$scheduledTasks)
        }
      })
    },

    ## the tasks set up inside `protect` (using the `schedule`
    ## function above) are only executed after the call to
    ## `protect` is finished; therefore, this provides a
    ## "protected" scope in which to set up tasks, such that
    ## we can guarantee the right order of execution
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

    ## the actual mechanism of executing the scheduled tasks:
    ## look into the scheduledTasks environment, sort it, and
    ## remove + execute each task as adequate; stop only when
    ## there are no more tasks in the environment
    executeTasks = function() {
      while (TRUE) {
        tasks <- sort(ls(private$scheduledTasks))
        if (length(tasks) == 0) break
        task <- private$scheduledTasks[[tasks[[1]]]]
        rm(list = tasks[[1]], envir = private$scheduledTasks)
        self$protect({
          task()
        })
      }
    })
)

## A helper class to NaiveScheduler that implements a simple
## reference counter. In our case, we initialize the reference
## counter by providing the NaiveScheduler's `executeTasks`
## function as the callback. Then, each time that
## `NaiveScheduler$protect` is called, we increment the
## RefCount's `count` variable by one. When we exit each
## `NaiveScheduler$protect` function call, we decrement
## the `count` variable by one. The callback (which is
## actually executing the scheduled tasks) is only called
## when the `count` vairable is 0 (i.e. when we're outside
## any call to `NaiveScheduler$protect`). This ensures that
## `NaiveScheduler$protect` functions as desired (see the
## comments in NaiveScheduler for more on that).
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
    },

    getCount = function() {
      private$count
    }
  ),
  private = list(
    count = NULL,
    callback = NULL
  )
)

## Used in the Pool class: chooses the best available scheduler
## to schedule the gives task (callback)
scheduleTask = function(millis, callback) {

  ## "pool.scheduler" should be a function that attempts to
  ## run the given callback at, preferably, the given number
  ## of millis in the future. The return value should be a
  ## function that cancels the task.
  scheduler <- getOption("pool.scheduler", NULL)

  ## if no such option is provided, use an instance of the
  ## NaiveScheduler (defined in the global environment, at
  ## the end of this file)
  if (is.null(scheduler)) {
    scheduler <- naiveScheduler$schedule
  }
  scheduler(millis, callback)
}


## Used in the Pool class (but only when
## `getOption("pool.scheduler", NULL)` does *not* return
## NULL): builds on top of `scheduleTask` to schedule
## recurring tasks. It uses the same mechanics: the
## return value is a function that cancels the
## scheduling of future tasks.
scheduleTaskRecurring <- function(millis, callback) {
  cancelled <- FALSE
  callback2 <- function() {
    callback()
    if (!cancelled)
      handle <<- scheduleTask(millis, callback2)
  }
  handle <- scheduleTask(millis, callback2)

  function() {
    cancelled <<- TRUE
    handle()
  }
}

naiveScheduler <- NaiveScheduler$new()

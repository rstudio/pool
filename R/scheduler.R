#' @include utils.R
NULL

## Used in the Pool class to schedule and cancel tasks (based on `later`)
scheduleTask <- function(func, delay) {
  force(func)
  later::later(function() {
    # Make sure warn is at least 1 so that warnings are emitted immediately.
    # (warn=2 is also OK, for use in debugging.)
    if (getOption("warn") < 1) {
      op <- options(warn = 1)
      on.exit(options(op))
    }
    if (!is.null(func))
      func()
  }, delay)

  ## return value is a function that cancel the task, so the user can
  ## cancel the task by calling the return value of `scheduleTask`. E.g:
  ##    > cancel <- scheduleTaskRecurring(function() print("hello"), 1)
  ##    [1] "hello"
  ##    [1] "hello"
  ##    > cancel()
  function() {
    func <<- NULL
  }
}

## Used in the Pool class. This function builds on top of `scheduleTask`
## to schedule recurring tasks. It uses the same mechanics: the return
## value is a function that cancels the scheduling of future tasks.
scheduleTaskRecurring <- function(func, delay) {
  force(func)
  cancelled <- FALSE
  func2 <- function() {
    func()
    if (!cancelled)
      handle <<- scheduleTask(func2, delay)
  }
  handle <- scheduleTask(func2, delay)

  function() {
    cancelled <<- TRUE
    handle()
  }
}

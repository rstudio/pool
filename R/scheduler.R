## Used in the Pool class. This function builds on top of `scheduleTask`
## to schedule recurring tasks. It uses the same mechanics: the return
## value is a function that cancels the scheduling of future tasks.
scheduleTaskRecurring <- function(func, delay) {
  force(func)
  cancelled <- FALSE
  func2 <- function() {
    func()
    if (!cancelled)
      handle <<- later::later(func2, delay)
  }
  handle <- later::later(func2, delay)

  function() {
    cancelled <<- TRUE
    handle()
  }
}

pool_metadata <- function(x,
                          check_valid = TRUE,
                          error_call = caller_env(),
                          error_arg = caller_arg(x)) {
  meta <- attr(x, "pool_metadata", exact = TRUE)

  if (is.null(meta)) {
    abort(
      paste0("`", error_arg, "` is not an pooled object."),
      call = error_call
    )
  }
  if (check_valid && !meta$valid) {
    abort(
      paste0("`", error_arg, "` is no longer valid."),
      call = error_call
    )
  }

  meta
}

#********************************************************************#
#********** Create Mock Pooled object for testing purposes **********#
#********************************************************************#
MockPooledObj <- R6::R6Class("MockPooledObj",
  public = list(

    closed = NULL,
    valid = NULL,

    initialize = function(closed = FALSE, valid = TRUE) {
      self$closed <- closed
      self$valid <- valid
    },

    invalidate = function() self$valid <- FALSE
  )
)

failOnActivate <- FALSE
failOnPassivate <- FALSE
failOnDestroy <- FALSE
failOnValidate <- FALSE

# Make R6 class available to S4, and set a few MockPooledObj methods
setClass("MockPooledObj")

setMethod("onActivate", "MockPooledObj", function(object) {
  if (failOnActivate) stop("Activation failed...")
})

setMethod("onPassivate", "MockPooledObj", function(object) {
  if (failOnPassivate) stop("Passivation failed...")
})

setMethod("onDestroy", "MockPooledObj", function(object) {
  if (failOnDestroy) stop("Destruction failed...")
  if (object$closed)
    stop("onDestroy called twice on the same object")
  object$closed <- TRUE
})

setMethod("onValidate", "MockPooledObj", function(object, query) {
  if (failOnValidate) stop("Validation failed...")
  if (isTRUE(attr(object, "bad", exact = TRUE))) {
    stop("Bad object")
  }
})



#********************************************************************#
#************************* Utility functions ************************#
#********************************************************************#

checkCounters <- function(pool) {
  expect_gte(pool$counters$free, 0)
  expect_gte(pool$counters$taken, 0)
}

checkCounts <- function(pool, free, taken) {
  checkCounters(pool)
  if (!missing(free)) {
    expect_identical(pool$counters$free, free)
  }
  if (!missing(taken)) {
    expect_identical(pool$counters$taken, taken)
  }
}

#********************************************************************#
#**************************** Sample data ***************************#
#********************************************************************#

# first 10 rows of nycflights13::flights (except `time_hour` col)
flights <- tibble::tibble(
  year = c(2013L, 2013L, 2013L, 2013L, 2013L, 2013L, 2013L, 2013L, 2013L, 2013L),
  month = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L),
  day = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L),
  dep_time = c(517L, 533L, 542L, 544L, 554L, 554L, 555L, 557L, 557L, 558L),
  sched_dep_time = c(515L, 529L, 540L, 545L, 600L, 558L, 600L, 600L, 600L, 600L),
  dep_delay = c(2, 4, 2, -1, -6, -4, -5, -3, -3, -2),
  arr_time = c(830L, 850L, 923L, 1004L, 812L, 740L, 913L, 709L, 838L, 753L),
  sched_arr_time = c(819L, 830L, 850L, 1022L, 837L, 728L, 854L, 723L, 846L, 745L),
  arr_delay = c(11, 20, 33, -18, -25, 12, 19, -14, -8, 8),
  carrier = c("UA", "UA", "AA", "B6", "DL", "UA", "B6", "EV", "B6", "AA"),
  flight = c(1545L, 1714L, 1141L, 725L, 461L, 1696L, 507L, 5708L, 79L, 301L),
  tailnum = c("N14228", "N24211", "N619AA", "N804JB", "N668DN", "N39463", "N516JB", "N829AS", "N593JB", "N3ALAA"),
  origin = c("EWR", "LGA", "JFK", "JFK", "LGA", "EWR", "EWR", "LGA", "JFK", "LGA"),
  dest = c("IAH", "IAH", "MIA", "BQN", "ATL", "ORD", "FLL", "IAD", "MCO", "ORD"),
  air_time = c(227, 227, 160, 183, 116, 150, 158, 53, 140, 138),
  distance = c(1400, 1416, 1089, 1576, 762, 719, 1065, 229, 944, 733),
  hour = c(5, 5, 5, 5, 6, 5, 6, 6, 6, 6),
  minute = c(15, 29, 40, 45, 0, 58, 0, 0, 0, 0)
)


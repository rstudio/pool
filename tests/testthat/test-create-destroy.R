
# MANUAL TEST REQUIRED
# see examples/test-create-destroy-manual.R

# Unfortunately this test is not possible right now, because the warnings
# that are expected come after a `later::later()` call. There is a shortcoming
# with `later` that causes this to behave differently than we'd expect:
#   expect_warning(scheduleTask(function() {warning("foo")}, 0))
# It is only after the next command that the warning is printed to the console.
# We managed to get the desired behavior by changing the "warn" option in our
# `scheduleTask()` function (to 1). However, this is not captured by
# `expect_warning()`, so this test must be run manually/interactively.

# source("utils.R")
#
# context("Pool's createObject and destroyObject methods")
#
# describe("createObject", {
#
#   it("throws if `factory` throws or returns NULL", {
#     expect_error(poolCreate(MockPooledObj),
#       "attempt to apply non-function")
#     expect_error(poolCreate(function(x) NULL),
#       "Object creation was not successful.")
#   })
# })
#
# describe("destroyObject", {
#
#   pool <- poolCreate(MockPooledObj$new,
#     minSize = 1, maxSize = 3, idleTimeout = 0)
#
#   it("throws if onDestroy fails", {
#     checkCounts(pool, free = 1, taken = 0)
#     failOnDestroy <<- TRUE
#
#     a <- poolCheckout(pool)
#     b <- poolCheckout(pool)
#     checkCounts(pool, free = 0, taken = 2)
#
#     ## since we're over the minSize, once we return `b` to
#     ## the pool, it will be destroyed immediately (since
#     ## we set `idleTimeout = 0`)
#     # expect_warning(poolReturn(b),
#     #   "Object of class MockPooledObj could not be ",
#     #   "destroyed properly, but was successfully removed ",
#     #   "from pool.")
#     poolReturn(b)
#
#     checkCounts(pool, free = 0, taken = 1)
#     failOnDestroy <<- FALSE
#
#     ## cleanup: return `a`
#     poolReturn(a)
#     checkCounts(pool, free = 1, taken = 0)
#   })
#
#   poolClose(pool)
#   gc()
# })

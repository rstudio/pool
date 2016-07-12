source("utils.R")

context("Pool's close method")

describe("close", {

  pool <- poolCreate(MockPooledObj$new,
    closed = FALSE, valid = TRUE,
    minSize = 1, maxSize = 3, idleTimeout = 1000)

  it("...", {
    return()
  })

  poolClose(pool)
})




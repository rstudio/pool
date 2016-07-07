source("utils.R")

context("Pool leak deatection")

describe("pool", {
  pool <- poolCreate(MockPooledObj$new,
    closed = FALSE, valid = TRUE,
    minSize = 1, maxSize = 3, idleTimeout = 1000)

  it("checks for leaks (anonymous)", {
    checkCounts(pool, free = 1, taken = 0)
    poolCheckout(pool)

    ## Without some kind of S3 method call, this doesn't work. Isn't it odd?
    #foo(0)
    expect_warning(gc(), "You have leaked pooled objects. Closing them.")
    checkCounts(pool, free = 0, taken = 0)
  })

  it("checks for leaks (named)", {
    fetched <- poolCheckout(pool)
    checkCounts(pool, free = 0, taken = 1)
    rm(fetched)
    expect_warning(gc(), "You have leaked pooled objects. Closing them.")
    checkCounts(pool, free = 0, taken = 0)
  })
})

source("utils.R")

context("Pool leak deatection")

describe("pool", {
  pool <- Pool$new(MockPooledObj$new, 1, 3, 1000)

  it("checks for leaks (anonymous)", {
    checkCounts(pool, free = 1, taken = 0)
    poolCheckout(pool)

    ## see lines 146-156 of pool.R to justify both of these changes
    # expect_warning(gc())
    # checkCounts(pool, free = 0, taken = 0)
    # print("Without this line, this doesn't work. Isn't it odd?")
    # gc()
    # checkCounts(pool, free = 1, taken = 0)

    ## temporalily changed back tests, just like the code
    ## Without this print statement, this doesn't work. Isn't it odd?
    print("")
    gc()
    checkCounts(pool, free = 0, taken = 0)
  })

  it("checks for leaks (named)", {
    fetched <- poolCheckout(pool)
    checkCounts(pool, free = 0, taken = 1)
    rm(fetched)
    gc()
    checkCounts(pool, free = 0, taken = 0)
  })
})

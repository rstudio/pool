source("utils.R")

context("Shiny scheduler")

describe("pool", {

  describe("respects idleTimeout", {

    pool <- poolCreate(MockPooledObj$new,
      closed = FALSE, valid = TRUE,
      minSize = 1, maxSize = 3, idleTimeout = 1000)

    it("can be created", {
      checkShiny()
    })

    it("respects validity", {
      checkShiny()
    })
  })

  describe("...!", {
  })
})



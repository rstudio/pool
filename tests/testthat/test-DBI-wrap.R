test_that("wrapper functions look good", {
  # Skip in test coverage runs because covr instrumentation is injected
  # into function body
  skip_if_not(is.null(getOption("covr.flags")))

  expect_snapshot({
    DBI_wrap("dbExecute")
  })
})

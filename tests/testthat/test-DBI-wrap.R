test_that("wrapper functions look good", {
  expect_snapshot({
    DBI_wrap("dbExecute")
  })
})

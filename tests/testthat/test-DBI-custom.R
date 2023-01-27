test_that("unsupported functions give useful errors", {
  pool <- local_pool()

  expect_snapshot(error = TRUE, {
    dbSendQuery(pool)
    dbSendStatement(pool)
    dbBegin(pool)
    dbCommit(pool)
    dbRollback(pool)
  })
})

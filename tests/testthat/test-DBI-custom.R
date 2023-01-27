test_that("unsupported functions give useful errors", {
  pool <- local_pool()

  expect_snapshot(error = TRUE, {
    dbSendQuery(pool)
    dbSendStatement(pool)
    dbBegin(pool)
    dbCommit(pool)
    dbRollback(pool)
    dbDisconnect(pool)
  })
})

test_that("All DBI Connection methods have a wrapper", {
  env <- ns_env("DBI")
  dbi_objs <- ls(env)
  dbi_generics <- dbi_objs[sapply(dbi_objs, isGeneric, where = env)]
  dbi_con_generics <- dbi_generics[sapply(dbi_generics, function(x) {
    args <- formals(get(x, env))
    any(c("con", "conn", "dbObj") %in% names(args))
  })]
  # Remove deprecated
  dbi_con_generics <- setdiff(dbi_con_generics, c(
    "dbCallProc",
    "dbGetException",
    "SQLKeywords",
    "isSQLKeyword",
    "make.db.names",
    "dbListResults"
  ))

  pool_methods <- attr(methods(class = "Pool"), "info")
  provided <- pool_methods$generic[pool_methods$from == "pool"]

  expect_equal(setdiff(dbi_con_generics, provided), character())
})

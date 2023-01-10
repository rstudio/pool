library(DBI)

test_that("dbSendQuery", {
  pool <- local_pool()
  expect_error(dbSendQuery(pool, "SELECT * FROM mtcars"))
})

test_that("dbSendStatement", {
  pool <- local_pool()
  expect_error(dbSendStatement(pool, "DELETE FROM mtcars"))
})

test_that("dbGetQuery", {
  pool <- local_pool()
  expect_equal(dbGetQuery(pool, "SELECT * FROM mtcars"), `row.names<-`(mtcars, NULL))
})

test_that("dbReadTable", {
  pool <- local_pool()
  expect_equal(dbReadTable(pool, "mtcars"), `row.names<-`(mtcars, NULL))
})

test_that("dbExecute", {
  pool <- local_pool()
  expect_equal(dbExecute(pool, "DELETE FROM mtcars WHERE cyl = 6"), 7L)
  expect_equal(dbGetQuery(pool, "SELECT COUNT(*) FROM mtcars")[[1]], 32L - 7L)
})

test_that("dbListResults", {
  pool <- local_pool()
  # Always list()
  expect_identical(dbListResults(pool), list())
  dbGetQuery(pool, "SELECT * FROM mtcars")
  expect_identical(dbListResults(pool), list())
})

test_that("dbListFields", {
  pool <- local_pool()
  expect_identical(dbListFields(pool, "mtcars"), colnames(mtcars))
})

test_that("dbListTables", {
  pool <- local_pool()
  expect_identical(dbListTables(pool), "mtcars")
})

test_that("dbListObjects", {
  pool <- local_pool()
  expect_identical(
    dbListObjects(pool),
    {
      conn <- poolCheckout(pool)
      withr::defer(poolReturn(conn))
      dbListObjects(conn)
    }
  )
})

test_that("dbWriteTable", {
  pool <- local_pool()
  dbWriteTable(pool, "cars", cars)
  expect_identical(dbGetQuery(pool, "SELECT * FROM cars"), cars)

  tmp <- tempfile("pressure", fileext = ".csv")
  withr::defer(unlink(tmp))
  write.csv(pressure, tmp, row.names = FALSE)
  dbWriteTable(pool, "pressure", tmp)
  expect_identical(dbGetQuery(pool, "SELECT * FROM pressure"), read.csv(tmp))
})

test_that("dbCreateTable/dbExistsTable/dbRemoveTable", {
  pool <- local_pool()

  expect_false(dbExistsTable(pool, "df"))

  df <- data.frame(x = 1:10)
  dbCreateTable(pool, "df", df)
  expect_equal(dbGetQuery(pool, "SELECT * FROM df"), df[0, , drop = FALSE])
  expect_true(dbExistsTable(pool, "df"))

  dbRemoveTable(pool, "df")
  expect_false(dbExistsTable(pool, "df"))
})

test_that("dbAppendTable", {
  pool <- local_pool()
  df <- data.frame(x = 1:20)

  dbCreateTable(pool, "df", df)
  dbAppendTable(pool, "df", df)
  dbAppendTable(pool, "df", df)
  expect_equal(dbGetQuery(pool, "SELECT COUNT(*) FROM df")[[1]], 40)
})

test_that("dbIsReadOnly", {
  pool <- local_pool()
  expect_false(dbIsReadOnly(pool))
})

test_that("dbUnquoteIdentifier", {
  pool <- local_pool()
  expect_equal(
    dbUnquoteIdentifier(pool, dbQuoteIdentifier(pool, "Hello"))[[1]]@name,
    c(table = "Hello")
  )
})

# jcheng 2020-10-12: This doesn't work, exports and imports are both included in pool_funcs
# test_that("All methods implemented", {
#   funcs <- ls("package:DBI")
#   funcs <- funcs[sapply(funcs, isGeneric, where = "package:DBI")]
#
#   pool_funcs <- getNamespaceExports("pool")
#
#   resultset <- c("dbBind", "dbClearResult", "dbColumnInfo", "dbFetch", "fetch",
#     "dbGetRowCount", "dbGetRowsAffected", "dbGetStatement", "dbHasCompleted",
#     "dbSetDataMappings")
#   driver <- c("dbCanConnect", "dbConnect")
#   deprecated <- c("dbCallProc", "dbDriver", "dbGetException",
#     "dbListConnections", "dbUnloadDriver")
#   not_applicable <- c("dbDisconnect", "dbGetConnectArgs")
#   stateful <- c("dbGetException")
#   total <- c(pool_funcs, resultset, driver, deprecated, not_applicable, stateful)
#
#   expect_true(all(funcs %in% total))
# })

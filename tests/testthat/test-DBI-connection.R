library(DBI)

pool <- NULL

setup({
  pool <<- dbPool(RSQLite::SQLite())
  dbWriteTable(pool, "mtcars", mtcars)
})

teardown({
  poolClose(pool)
  pool <<- NULL
})

test_that("dbSendQuery", {
  expect_error(dbSendQuery(pool, "SELECT * FROM mtcars"))
})

test_that("dbSendStatement", {
  expect_error(dbSendStatement(pool, "DELETE FROM mtcars"))
})

test_that("dbGetQuery", {
  expect_equal(dbGetQuery(pool, "SELECT * FROM mtcars"), `row.names<-`(mtcars, NULL))
})

test_that("dbReadTable", {
  expect_equal(dbReadTable(pool, "mtcars"), `row.names<-`(mtcars, NULL))
})

test_that("dbExecute", {
  expect_equal(dbExecute(pool, "DELETE FROM mtcars WHERE cyl = 6"), 7L)
  expect_equal(dbGetQuery(pool, "SELECT COUNT(*) FROM mtcars")[[1]], 32L - 7L)
})

test_that("dbListResults", {
  # Always list()
  expect_identical(dbListResults(pool), list())
  dbGetQuery(pool, "SELECT * FROM mtcars")
  expect_identical(dbListResults(pool), list())
})

test_that("dbListFields", {
  expect_identical(dbListFields(pool, "mtcars"), colnames(mtcars))
})

test_that("dbListTables", {
  expect_identical(dbListTables(pool), "mtcars")
})

test_that("dbListObjects", {
  expect_identical(
    dbListObjects(pool),
    {
      conn <- poolCheckout(pool)
      on.exit(poolReturn(conn))
      dbListObjects(conn)
    }
  )
})

test_that("dbWriteTable", {
  dbWriteTable(pool, "cars", cars)
  expect_identical(dbGetQuery(pool, "SELECT * FROM cars"), cars)

  tmp <- tempfile("pressure", fileext = ".csv")
  on.exit(unlink(tmp))
  write.csv(pressure, tmp, row.names = FALSE)
  dbWriteTable(pool, "pressure", tmp)
  expect_identical(dbGetQuery(pool, "SELECT * FROM pressure"), read.csv(tmp))
})

test_that("dbCreateTable", {
  dbCreateTable(pool, "iris", iris)
  iris1 <- iris
  iris1$Species <- as.character(iris$Species)
  expect_identical(dbGetQuery(pool, "SELECT * FROM iris"), iris1[FALSE,])
})

test_that("dbAppendTable", {
  iris1 <- iris
  iris1$Species <- as.character(iris$Species)
  dbAppendTable(pool, "iris", iris1)
  dbAppendTable(pool, "iris", iris1)
  expect_identical(dbGetQuery(pool, "SELECT COUNT(*) FROM iris")[[1]], nrow(iris) * 2L)
})

test_that("dbExistsTable", {
  expect_true(dbExistsTable(pool, "iris"))
  expect_false(dbExistsTable(pool, "not_iris"))
})

test_that("dbRemoveTable", {
  expect_true("iris" %in% dbListTables(pool))
  dbRemoveTable(pool, "iris")
  expect_false("iris" %in% dbListTables(pool))
})

test_that("dbIsReadOnly", {
  expect_false(dbIsReadOnly(pool))
})

test_that("dbUnquoteIdentifier", {
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

test_that("wrapper functions look good", {
  # Skip in test coverage runs because covr instrumentation is injected
  # into function body
  skip_if_not(is.null(getOption("covr.flags")))

  expect_snapshot({
    DBI_wrap("dbExecute")
  })
})

test_that("dbSendQuery", {
  pool <- local_pool()
  expect_error(DBI::dbSendQuery(pool, "SELECT * FROM mtcars"))
})

test_that("dbSendStatement", {
  pool <- local_pool()
  expect_error(DBI::dbSendStatement(pool, "DELETE FROM mtcars"))
})

test_that("dbGetQuery", {
  pool <- local_pool()
  expect_equal(DBI::dbGetQuery(pool, "SELECT * FROM mtcars"), `row.names<-`(mtcars, NULL))
})

test_that("dbReadTable", {
  pool <- local_pool()
  expect_equal(DBI::dbReadTable(pool, "mtcars"), `row.names<-`(mtcars, NULL))
})

test_that("dbExecute", {
  pool <- local_pool()
  expect_equal(DBI::dbExecute(pool, "DELETE FROM mtcars WHERE cyl = 6"), 7L)
  expect_equal(DBI::dbGetQuery(pool, "SELECT COUNT(*) FROM mtcars")[[1]], 32L - 7L)
})

test_that("dbListFields", {
  pool <- local_pool()
  expect_identical(DBI::dbListFields(pool, "mtcars"), colnames(mtcars))
})

test_that("dbListTables", {
  pool <- local_pool()
  expect_identical(DBI::dbListTables(pool), "mtcars")
})

test_that("dbListObjects", {
  pool <- local_pool()
  expect_identical(
    DBI::dbListObjects(pool),
    {
      conn <- poolCheckout(pool)
      withr::defer(poolReturn(conn))
      DBI::dbListObjects(conn)
    }
  )
})

test_that("dbWriteTable", {
  pool <- local_pool()
  DBI::dbWriteTable(pool, "cars", cars)
  expect_identical(DBI::dbGetQuery(pool, "SELECT * FROM cars"), cars)

  tmp <- tempfile("pressure", fileext = ".csv")
  withr::defer(unlink(tmp))
  write.csv(pressure, tmp, row.names = FALSE)
  DBI::dbWriteTable(pool, "pressure", tmp)
  expect_identical(DBI::dbGetQuery(pool, "SELECT * FROM pressure"), read.csv(tmp))
})

test_that("dbCreateTable/dbExistsTable/dbRemoveTable", {
  pool <- local_pool()

  expect_false(DBI::dbExistsTable(pool, "df"))

  df <- data.frame(x = 1:10)
  DBI::dbCreateTable(pool, "df", df)
  expect_equal(DBI::dbGetQuery(pool, "SELECT * FROM df"), df[0, , drop = FALSE])
  expect_true(DBI::dbExistsTable(pool, "df"))

  dbRemoveTable(pool, "df")
  expect_false(DBI::dbExistsTable(pool, "df"))
})

test_that("dbAppendTable", {
  pool <- local_pool()
  df <- data.frame(x = 1:20)

  DBI::dbCreateTable(pool, "df", df)
  DBI::dbAppendTable(pool, "df", df)
  DBI::dbAppendTable(pool, "df", df)
  expect_equal(dbGetQuery(pool, "SELECT COUNT(*) FROM df")[[1]], 40)
})

test_that("dbIsReadOnly", {
  pool <- local_pool()
  expect_false(DBI::dbIsReadOnly(pool))
})

test_that("dbUnquoteIdentifier", {
  pool <- local_pool()
  expect_equal(
    DBI::dbUnquoteIdentifier(pool, dbQuoteIdentifier(pool, "Hello"))[[1]]@name,
    c(table = "Hello")
  )
})


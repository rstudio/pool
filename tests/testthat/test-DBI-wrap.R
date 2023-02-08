test_that("wrapper functions look good", {
  # Skip in test coverage runs because covr instrumentation is injected
  # into function body
  skip_if_not(is.null(getOption("covr.flags")))

  expect_snapshot({
    DBI_wrap("dbExecute")
  })
})

test_that("dbWriteTable/dbGetQuery/dbReadTable", {
  pool <- local_db_pool()

  df <- data.frame(x = 1:10)
  dbWriteTable(pool, "df", df)
  expect_equal(dbGetQuery(pool, "SELECT * FROM df"), df)
  expect_equal(dbReadTable(pool, "df"), df)
})

test_that("dbExecute", {
  pool <- local_db_pool()

  df <- data.frame(x = 1:5)
  dbWriteTable(pool, "df", df)

  expect_equal(dbExecute(pool, "DELETE FROM df WHERE x > 3"), 2)
  expect_equal(dbReadTable(pool, "df"), data.frame(x = 1:3))
})

test_that("dbListFields", {
  pool <- local_db_pool()
  df <- data.frame(x = 1, y = 2, z = 3)
  dbWriteTable(pool, "df", df)

  expect_equal(dbListFields(pool, "df"), c("x", "y", "z"))
})

test_that("dbListTables", {
  pool <- local_db_pool()
  dbWriteTable(pool, "df", data.frame(x = 1))
  expect_identical(dbListTables(pool), "df")
})

test_that("dbListObjects", {
  pool <- local_db_pool()
  con <- localCheckout(pool)

  expect_equal(dbListObjects(pool), dbListObjects(con))
})

test_that("dbCreateTable/dbExistsTable/dbRemoveTable", {
  pool <- local_db_pool()

  expect_false(dbExistsTable(pool, "df"))

  df <- data.frame(x = 1:10)
  dbCreateTable(pool, "df", df)
  expect_true(dbExistsTable(pool, "df"))
  expect_equal(dbGetQuery(pool, "SELECT * FROM df"), df[0, , drop = FALSE])

  dbRemoveTable(pool, "df")
  expect_false(dbExistsTable(pool, "df"))
})

test_that("dbAppendTable", {
  pool <- local_db_pool()
  df <- data.frame(x = 1:20)

  dbCreateTable(pool, "df", df)
  dbAppendTable(pool, "df", df)
  dbAppendTable(pool, "df", df)
  expect_equal(dbGetQuery(pool, "SELECT COUNT(*) FROM df")[[1]], 40)
})

test_that("dbIsReadOnly", {
  pool <- local_db_pool()
  expect_false(dbIsReadOnly(pool))
})

test_that("dbUnquoteIdentifier", {
  pool <- local_db_pool()
  expect_equal(
    dbUnquoteIdentifier(pool, SQL("`Hello`")),
    list(Id(table = "Hello"))
  )
})

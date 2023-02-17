test_that("can copy and collect", {
  pool <- local_db_pool()

  df <- tibble::tibble(x = 1:10, y = 1:10)
  db <- dplyr::copy_to(
    pool,
    df,
    temporary = FALSE,
    indexes = list(c("x", "y"), "y")
  )
  expect_s3_class(dbplyr::remote_con(db), "Pool")
  expect_equal(dplyr::collect(db), df)

  # But copy_to must not be temporary
  expect_snapshot(dplyr::copy_to(pool, df), error = TRUE)
})

test_that("can use one-table verbs", {
  pool <- local_db_pool()

  df <- tibble::tibble(letters = letters[1:3], x = 1:3, z = 3:1)
  db <- dplyr::copy_to(pool, df, temporary = FALSE)

  out <- df %>%
    dplyr::select(-z) %>%
    dplyr::filter(x < 2) %>%
    dplyr::group_by(letters) %>%
    dplyr::summarise(n = dplyr::n())
  expect_equal(dplyr::collect(out), tibble::tibble(letters = "a", n = 1))
})

test_that("dplyr verbs throw error when `temporary = TRUE`", {
  pool <- local_db_pool()
  DBI::dbWriteTable(pool, "mtcars", mtcars)

  expect_snapshot(error = TRUE, {
    dplyr::copy_to(pool, data.frame(x = 1), "df")
    dplyr::compute(dplyr::tbl(pool, "mtcars"))
  })
})

test_that("joins, semi_joins, and set ops work", {
  pool <- local_db_pool()

  db1 <- dplyr::copy_to(pool, data.frame(x = 1), temporary = FALSE)
  db2 <- dplyr::copy_to(pool, data.frame(x = 2), temporary = FALSE)

  expect_no_error(dplyr::collect(dplyr::left_join(db1, db1, by = "x")))
  expect_no_error(dplyr::collect(dplyr::semi_join(db1, db2, by = "x")))
  expect_no_error(dplyr::collect(dplyr::union(db1, db2)))
})

test_that("can use schemas with pool", {
  pool <- local_db_pool()

  df <- tibble::tibble(x = 1:5)

  dplyr::copy_to(pool, df,
    dbplyr::in_schema("main", "df"),
    temporary = FALSE,
    overwrite = TRUE
  )

  tbl <- dplyr::tbl(pool, dbplyr::in_schema("main", "df"))
  expect_equal(dplyr::collect(tbl), df)
})

test_that("wrapper looks good", {
  # Skip in test coverage runs because covr instrumentation is injected
   # into function body
   skip_if_not(is.null(getOption("covr.flags")))

   expect_snapshot({
    dbplyr_wrap("db_collect")
    "with temporary argument"
    dbplyr_wrap("db_compute")
  })
})

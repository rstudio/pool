source("utils.R")

context("dplyr compatibility")

describe("pool package", {

  if (requireNamespace("RSQLite", quietly = TRUE)) {

    db <- tempfile()
    pool <- dbPool(RSQLite::SQLite(), dbname = db)

    it("can create local SQLite pool", {
      expect_equal(class(pool), c("Pool", "R6"))
      info <- list(
        class = "Pool",
        valid = TRUE,
        minSize = 1,
        maxSize = Inf,
        idleTimeout = 60,
        pooledObjectClass = "SQLiteConnection",
        numberFreeObjects = 0,
        numberTakenObjects = 1
      )
      expect_equal(dbGetInfo(pool), info)
      checkCounts(pool, free = 1, taken = 0)
    })

    it("can use dplyr syntax to copy table to DB", {
      checkCounts(pool, free = 1, taken = 0)
      copy_to(pool, flights, "flights",
        temporary = FALSE,
        indexes = list(
          c("year", "month", "day"),
          "carrier",
          "tailnum",
          "dest"
        )
      )
      checkCounts(pool, free = 1, taken = 0)
      expect_true(dbExistsTable(pool, "flights"))
    })

    it("can use dplyr syntax to get a table from DB", {
      checkCounts(pool, free = 1, taken = 0)
      flights_db <- tbl(pool, "flights")
      expect_s3_class(flights_db$src$con, "Pool")
      checkCounts(pool, free = 1, taken = 0)
      expect_s3_class(flights_db, "tbl_Pool")
    })

    it("can use dplyr syntax to select", {
      checkCounts(pool, free = 1, taken = 0)
      flights_db <- tbl(pool, "flights")
      s <- dplyr::select(flights_db, year:day, dep_delay, arr_delay)
      expect_equal(tibble::as_tibble(s),
        tibble::tibble(
          year = rep(2013L, 10),
          month = rep(1L, 10),
          day = rep(1L, 10),
          dep_delay = c(2, 4, 2, -1, -6, -4, -5, -3, -3, -2),
          arr_delay = c(11, 20, 33, -18, -25, 12, 19, -14, -8, 8)
        )
      )
      checkCounts(pool, free = 1, taken = 0)
    })

    it("can use dplyr syntax to filter", {
      checkCounts(pool, free = 1, taken = 0)
      flights_db <- tbl(pool, "flights")
      f <- dplyr::filter(flights_db, dep_delay > 0)
      ft <- tibble::as_tibble(f)
      expect_equal(ft$dep_time, c(517, 533, 542))
      expect_equal(ft$arr_time, c(830, 850, 923))
      checkCounts(pool, free = 1, taken = 0)
    })

    it("can use dplyr syntax to `collect`", {
      checkCounts(pool, free = 1, taken = 0)
      flights_db <- tbl(pool, "flights")
      c <- dplyr::collect(flights_db)
      expect_equal(c, flights)
      expect_equal(nrow(c), 10)
      checkCounts(pool, free = 1, taken = 0)
    })

    it("throws error when `temporary = TRUE`", {
      expect_error(copy_to(pool, flights, "temp"),
        "You cannot use `temporary = TRUE`"
      )
    })

    poolClose(pool)
  }
})

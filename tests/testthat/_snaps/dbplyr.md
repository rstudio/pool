# can copy and collect

    Code
      dplyr::copy_to(pool, df)
    Condition
      Error:
      ! Can't use temporary tables with Pool objects
      x Temporary tables are local to a connection
      i Either use `temporary = FALSE`, or
      i Check out a local connection with `localCheckout()`

# dplyr verbs throw error when `temporary = TRUE`

    Code
      dplyr::copy_to(pool, data.frame(x = 1), "df")
    Condition
      Error:
      ! Can't use temporary tables with Pool objects
      x Temporary tables are local to a connection
      i Either use `temporary = FALSE`, or
      i Check out a local connection with `localCheckout()`
    Code
      dplyr::compute(dplyr::tbl(pool, "mtcars"))
    Condition
      Error:
      ! Can't use temporary tables with Pool objects
      x Temporary tables are local to a connection
      i Either use `temporary = FALSE`, or
      i Check out a local connection with `localCheckout()`

# wrapper looks good

    Code
      dbplyr_wrap("db_collect")
    Output
      function (con, sql, n = -1, warn_incomplete = TRUE, ...) 
      {
          db_con <- localCheckout(con)
          dbplyr::db_collect(con = db_con, sql = sql, n = n, warn_incomplete = warn_incomplete, 
              ... = ...)
      }
      <environment: namespace:pool>
    Code
      # with temporary argument
      dbplyr_wrap("db_compute")
    Output
      function (con, table, sql, ..., overwrite = FALSE, temporary = TRUE, 
          unique_indexes = list(), indexes = list(), analyze = TRUE, 
          in_transaction = TRUE) 
      {
          stop_if_temporary(temporary)
          db_con <- localCheckout(con)
          dbplyr::db_compute(con = db_con, table = table, sql = sql, 
              ... = ..., overwrite = overwrite, temporary = temporary, 
              unique_indexes = unique_indexes, indexes = indexes, analyze = analyze, 
              in_transaction = in_transaction)
      }
      <environment: namespace:pool>

# warns if dbplyr is old

    Code
      check_dbplyr()
    Message
      ! Pool works best with dbplyr 2.4.0 or greater.
      i You have dbplyr 1.0.0.
      i Please consider upgrading.


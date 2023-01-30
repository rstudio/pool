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
          db_con <- poolCheckout(con)
          on.exit(poolReturn(db_con))
          dbplyr::db_collect(con = db_con, sql = sql, n = n, warn_incomplete = warn_incomplete, 
              ... = ...)
      }
      <environment: namespace:pool>
    Code
      # with temporary argument
      dbplyr_wrap("db_compute")
    Output
      function (con, table, sql, temporary = TRUE, unique_indexes = list(), 
          indexes = list(), analyze = TRUE, ...) 
      {
          stop_if_temporary(temporary)
          db_con <- poolCheckout(con)
          on.exit(poolReturn(db_con))
          dbplyr::db_compute(con = db_con, table = table, sql = sql, 
              temporary = temporary, unique_indexes = unique_indexes, 
              indexes = indexes, analyze = analyze, ... = ...)
      }
      <environment: namespace:pool>


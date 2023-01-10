# can copy and collect

    Code
      dplyr::copy_to(pool, df)
    Condition
      Error:
      ! You cannot use `temporary = TRUE` when using a Pool object, since temporary tables are local to a connection, and there's no guarantee you'll get the same connection back next time. You must either create a permanent table, or checkout a connection from `pool` directly with `con <- poolCheckout(pool)`, and then release the connection back to the pool when you're finished (`poolReturn(con)`).

# dplyr verbs throw error when `temporary = TRUE`

    Code
      dplyr::copy_to(pool, data.frame(x = 1), "df")
    Condition
      Error:
      ! You cannot use `temporary = TRUE` when using a Pool object, since temporary tables are local to a connection, and there's no guarantee you'll get the same connection back next time. You must either create a permanent table, or checkout a connection from `pool` directly with `con <- poolCheckout(pool)`, and then release the connection back to the pool when you're finished (`poolReturn(con)`).
    Code
      dplyr::compute(dplyr::tbl(pool, "mtcars"))
    Condition
      Error:
      ! You cannot use `temporary = TRUE` when using a Pool object, since temporary tables are local to a connection, and there's no guarantee you'll get the same connection back next time. You must either create a permanent table, or checkout a connection from `pool` directly with `con <- poolCheckout(pool)`, and then release the connection back to the pool when you're finished (`poolReturn(con)`).

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


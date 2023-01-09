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


# pool package: throws error when `temporary = TRUE`

    Code
      dplyr::copy_to(pool, data.frame(x = 1), "df")
    Error <simpleError>
      You cannot use `temporary = TRUE` when using a Pool object, since temporary tables are local to a connection, and there's no guarantee you'll get the same connection back next time. You must either create a permanent table, or checkout a connection from `pool` directly with `con <- poolCheckout(pool)`, and then release the connection back to the pool when you're finished (`poolReturn(con)`).
    Code
      dplyr::compute(filter(tbl(pool, "mtcars"), cyl == 4))
    Error <simpleError>
      You cannot use `temporary = TRUE` when using a Pool object, since temporary tables are local to a connection, and there's no guarantee you'll get the same connection back next time. You must either create a permanent table, or checkout a connection from `pool` directly with `con <- poolCheckout(pool)`, and then release the connection back to the pool when you're finished (`poolReturn(con)`).


# wrapper functions look good

    Code
      DBI_wrap("dbExecute")
    Output
      function (conn, statement, ...) 
      {
          db_con <- localCheckout(conn)
          DBI::dbExecute(conn = db_con, statement = statement, ... = ...)
      }
      <environment: namespace:pool>


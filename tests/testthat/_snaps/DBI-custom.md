# unsupported functions give useful errors

    Code
      dbSendQuery(pool)
    Condition
      Error in `dbSendQuery()`:
      ! Not supported for pool objects
      i Please use `dbGetQuery()` instead
    Code
      dbSendStatement(pool)
    Condition
      Error in `dbSendStatement()`:
      ! Not supported for pool objects
      i Please use `dbExecute()` instead
    Code
      dbBegin(pool)
    Condition
      Error in `dbBegin()`:
      ! Not supported for pool objects
      i Please use `poolWithTransaction()` instead
    Code
      dbCommit(pool)
    Condition
      Error in `dbCommit()`:
      ! Not supported for pool objects
      i Please use `poolWithTransaction()` instead
    Code
      dbRollback(pool)
    Condition
      Error in `dbRollback()`:
      ! Not supported for pool objects
      i Please use `poolWithTransaction()` instead


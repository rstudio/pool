# Unsupported DBI methods

Most pool methods for DBI generics check out a connection, perform the
operation, and the return the connection to the pool, as described in
[DBI-wrap](http://rstudio.github.io/pool/reference/DBI-wrap.md).

This page describes the exceptions:

- [`DBI::dbSendQuery()`](https://dbi.r-dbi.org/reference/dbSendQuery.html)
  and
  [`DBI::dbSendStatement()`](https://dbi.r-dbi.org/reference/dbSendStatement.html)
  can't work with pool because they return result sets that are bound to
  a specific connection. Instead use
  [`DBI::dbGetQuery()`](https://dbi.r-dbi.org/reference/dbGetQuery.html),
  [`DBI::dbExecute()`](https://dbi.r-dbi.org/reference/dbExecute.html),
  or
  [`localCheckout()`](http://rstudio.github.io/pool/reference/poolCheckout.md).

- [`DBI::dbBegin()`](https://dbi.r-dbi.org/reference/transactions.html),
  [`DBI::dbRollback()`](https://dbi.r-dbi.org/reference/transactions.html),
  [`DBI::dbCommit()`](https://dbi.r-dbi.org/reference/transactions.html),
  and
  [`DBI::dbWithTransaction()`](https://dbi.r-dbi.org/reference/dbWithTransaction.html)
  can't work with pool because transactions are bound to a connection.
  Instead use
  [`poolWithTransaction()`](http://rstudio.github.io/pool/reference/poolWithTransaction.md).

- [`DBI::dbDisconnect()`](https://dbi.r-dbi.org/reference/dbDisconnect.html)
  can't work because pool handles disconnection. Use
  [`poolClose()`](http://rstudio.github.io/pool/reference/Pool-class.md)
  instead.

- [`DBI::dbGetInfo()`](https://dbi.r-dbi.org/reference/dbGetInfo.html)
  returns information about the pool, not the database connection.

- [`DBI::dbIsValid()`](https://dbi.r-dbi.org/reference/dbIsValid.html)
  returns whether or not the entire pool is valid (i.e. not closed).

## Usage

``` r
# S4 method for class 'Pool'
dbSendQuery(conn, statement, ...)

# S4 method for class 'Pool,ANY'
dbSendStatement(conn, statement, ...)

# S4 method for class 'Pool'
dbDisconnect(conn, ...)

# S4 method for class 'Pool'
dbGetInfo(dbObj, ...)

# S4 method for class 'Pool'
dbIsValid(dbObj, ...)

# S4 method for class 'Pool'
dbBegin(conn, ...)

# S4 method for class 'Pool'
dbCommit(conn, ...)

# S4 method for class 'Pool'
dbRollback(conn, ...)

# S4 method for class 'Pool'
dbWithTransaction(conn, code)
```

## Arguments

- conn, dbObj:

  A Pool object, as returned from
  [`dbPool()`](http://rstudio.github.io/pool/reference/dbPool.md).

- statement, code, ...:

  See DBI documentation.

pool
======

##### *Object Pooling in R*

[![Travis-CI Build Status](https://travis-ci.org/rstudio/pool.svg?branch=master)](https://travis-ci.org/rstudio/pool)

This package enables the creation of object pools for various types of objects in R, to make it less computationally expensive to fetch one. Currently the only supported pooled objects are DBI connections (see the [`DBI` package](https://github.com/rstats-db/DBI) for more info). However, the `Pool` class is general enough to allow for pooling of any R objects, provided that someone implements the backend appropriately (creating the object factory class and all the required methods). 

For more information on database connection pooling or instruction on the implementation of a object pool backend, see this website.

## Motivation

In the case of connecting to databases using DBI, consider the following code:

```r
system.time({
  conn <- dbConnect(
    drv = RMySQL::MySQL(), 
    dbname = "shinydemo",
    host = "shiny-demo.csa7qlmguqrf.us-east-1.rds.amazonaws.com",
    username = "guest",
    password = "guest")
  rs <- dbSendQuery(conn, "SELECT * FROM City LIMIT 5;")
  dbClearResult(rs)
  dbDisconnect(conn)
})
```

This takes on the order of 450 to 600ms to run. Now, if we connect to the database before we start our timer, we get a significant performance boost:

```r
conn <- dbConnect(
  drv = RMySQL::MySQL(), 
  dbname = "shinydemo",
  host = "shiny-demo.csa7qlmguqrf.us-east-1.rds.amazonaws.com",
  username = "guest",
  password = "guest")
system.time({
  rs <- dbSendQuery(conn, "SELECT * FROM City LIMIT 5;")
  dbClearResult(rs)
})
dbDisconnect(conn)
```

The timed code only takes about 110ms to run. This makes it clear that getting a connection from the database is the performance bottleneck; the actual queries are much faster. Having a connection pool takes care of this problem by fetching a minimum number of connections and keeping them live, to be checked out of the pool as needed. Instead of using pool, another possible solution to this would be to get a connection at the beginning of your session, use it whenever you want, and then close it at the end of your session. While this would work, there are two main problems with this approach:

1.
2.

Maybe also mention it's useful in long-running processes when you don't want to leave a connection to the database open (or you want to re-connect automatically if it gets dropped).

## Database connection pooling for Shiny apps

This is particularly useful when writing shiny apps because...

## Sample Usage

### Installation and Loading

To install the most up-to-date dvelopment versions of DBI and pool, use:

```r
devtools::install_github("rstats-db/DBI")
devtools::install_github("bborgesr/pool")
```

Then, at the start of your R session, make sure to load the packages:

```r
library(DBI)
library(pool)
```

### Using the pool to query a database

The pool package is at its most useful when you use it directly, rather than fetching an actual object from it. In the case of databases, this means that you use the Pool object directly to query the database, rather than first fetching a connection and then using that to query the database.

```r
pool <- poolCreate(
  src = RMySQL::MySQL(), 
  dbname = "shinydemo",
  host = "shiny-demo.csa7qlmguqrf.us-east-1.rds.amazonaws.com",
  username = "guest",
  password = "guest"
)

dbGetQuery(pool, "SELECT * FROM City LIMIT 5;")
#>   ID           Name CountryCode      District Population
#> 1  1          Kabul         AFG         Kabol    1780000
#> 2  2       Qandahar         AFG      Qandahar     237500
#> 3  3          Herat         AFG         Herat     186800
#> 4  4 Mazar-e-Sharif         AFG         Balkh     127800
#> 5  5      Amsterdam         NLD Noord-Holland     731200
```

### Using a connection to query a database

You can also fetch an actual DBIConnection object from the pool. However, this approach should be used with care, since this puts the onus of releasing the connection on you (the user). While this means that you still enjoy the performance benefits associated with having a pool, you lose the benefits of automatic connection management, since you are now yourself responsible for releasing the connection (using `release(conn)`) at the appropriate time (otherwise, you get a leaked connection). This approach should really only be necessary when you want to perform a non-trivial SQL transaction. You cannot perfrom SQL transaction using a Pool object directly (because that would imply keeping a connection open and not knowing when to return it back to the pool). For simple transactions, consider using `withTransaction` instead, which is safer since it does not require you to fetch and release the connection yourself.

```r
conn <- poolCheckout(pool)
rs <- dbSendQuery(conn, "SELECT * FROM City LIMIT 5;")
if (dbGetInfo(rs, what = "rowCount") > 5) {
  warning("dubious result -- rolling back transaction")
  dbRollback(conn)
}
poolReturn(conn)  ## alternatively, use: dbDisconnect(conn)

poolClose(pool)
```

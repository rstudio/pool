pool
======
##### *Object Pooling in R*

*Travis:* [![Travis-CI Build Status](https://travis-ci.org/rstudio/pool.svg?branch=master)](https://travis-ci.org/rstudio/pool)

*AppVeyor:* [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/rstudio/pool?branch=master&svg=true)](https://ci.appveyor.com/project/rstudio/pool)

This package enables the creation of object pools for various types of objects in R, to make it less computationally expensive to fetch one. Currently the only supported pooled objects are `DBI` connections (see the [`DBI` package](https://github.com/rstats-db/DBI) for more info), which can be used to query a database either directly through `DBI` or through `dplyr`.
However, the `Pool` class is general enough to allow for pooling of any R objects, provided that someone implements the backend appropriately (creating the object factory class and all the required methods) -- a vignette with instructions on how to do so will be coming soon.

What follows is the main motivation behind the creation of this package and some fairly simple examples.

### For a more comprehensive guide of how to integrate a database in a Shiny app see [the series of articles under the Databases section](http://shiny.rstudio.com/articles/) on Shiny's official website.

#### In particular, [this article](http://shiny.rstudio.com/articles/pool-basics.html) serves as a good intro to database connection pooling in Shiny, with a complete app example.

## Motivation

In the case of connecting to a database using `DBI`, consider the following code:

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

On my machine, this takes on the order of 450 to 600ms to run. Now, if we connect to the database before we start our timer, we get a significant performance boost:

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

The timed code only takes about 110ms to run. This makes it clear that getting a connection from the database is the performance bottleneck; the actual queries are much faster. Having a connection pool takes care of this problem by fetching a minimum number of connections and keeping them live, to be checked out of the pool as needed. Instead of using a pool, another possible solution to this would be to get a connection at the beginning of your session, use it whenever you want, and then close it at the end of your session. This is almost always a bad idea because:

- since there is only one connection, it cannot handle simultaneous requests (this is especially an issue if you have a complicated app or if you have more than one session open at any time -- i.e. more than one user at the same time);
- if the connection breaks at some point (maybe the database server crashed), you won't get a new connection (you have to exit the app and re-run it);
- even if you're not making any queries at the moment (or if you leave your app running while your gone), you're gonna have an idle connection sitting around for no reason;
- finally, even if you use use more than one connection per app (but fewer than one connection per query), it can be difficult to keep track of all your connections, since you'll be opening and closing them in potentially very different places.

## Database connection pooling for Shiny apps

`pool` to the rescue! The goal of `pool` is to let you write Shiny apps that connect to databases, without having to worry about connection management or performance. The `pool` package adds a new level of abstraction when connecting to a database: instead of directly fetching a connection from the database, you will create an object (called a pool) with a reference to that database. The pool holds a number of connections to the database. Some of these may be currently in-use and some of these may be idle, waiting for a query to request them. Each time you make a query, you are querying the pool, rather than the database. Under the hood, the pool will either give you an idle connection that it previously fetched from the database or, if it has no free connections, fetch one and give it to you. You never have to create or close connections directly: the pool knows when it should grow, shrink or keep steady.

## Sample Usage

### Installation and Loading

To install the most up-to-date development versions of `DBI`, `pool` and `shiny`, use:

```r
devtools::install_github("rstats-db/DBI")
devtools::install_github("rstudio/pool")
devtools::install_github("rstudio/shiny")
```

Then, at the start of your R session or your Shiny app, make sure to load the packages:

```r
library(DBI)
library(pool)
library(shiny)
```

### Using the pool to query a database

The pool package is at its most useful when you use it directly, rather than fetching an actual object from it. In the case of databases, this means that you use the Pool object directly to query the database, rather than first fetching a connection and then using that to query the database.

```r
pool <- dbPool(
  drv = RMySQL::MySQL(),
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

If this was intended as a Shiny app, you should be creating a pool at the start of the app (if you're not using a single-file app, you could put this at the top of `server.R` or in `global.R`). Then, reference that pool each time you make a query (usually at the reactive/function level). By default, on creation, the pool fetches and keeps around one idle connection. When you make a query to the pool, it will always use that connection, unless it happens to already be busy in another query (this becomes more likely if you have several sessions going on at the same time). If that's the case, the pool will fetch a second connection for the current query; once that's finished, the pool with hold on to it for a minute (by default). If that second connection is requested again in that period of time, the countdown resets. Otherwise, the pool disconnects it. So basically, the pool "knows" when it should have more connections and how to manage them (including disconnecting them when necessary).

### Using a connection to query a database

However, you can also fetch an actual DBIConnection object from the pool. However, this approach should be used with care, since this puts the onus of releasing the connection on you (the user). While this means that you still enjoy the performance benefits associated with having a pool, you lose the benefits of automatic connection management, since you are now yourself responsible for returning the connection to the pool (using `poolReturn(conn)`) at the appropriate time (otherwise, you get a leaked connection). This approach should really only be necessary when you want to perform a non-trivial SQL transaction. You cannot perform SQL transactions using a Pool object directly (because that would imply keeping a connection open and not knowing when to return it back to the pool). Here's an example how to handle transactions with `pool`:

<!--
For simple transactions, consider using `withTransaction` instead, which is safer since it does not require you to fetch and release the connection yourself.
-->

```r
conn <- poolCheckout(pool)
rs <- dbSendQuery(conn, "SELECT * FROM City LIMIT 5;")
if (dbGetInfo(rs, what = "rowCount") > 5) {
  warning("dubious result -- rolling back transaction")
  dbRollback(conn)
}
poolReturn(conn)  ## alternatively, use: dbDisconnect(conn)
```

=========

### Issue and PR tracking (via [waffle.io](https://waffle.io/))

[![Stories in Backlog](https://badge.waffle.io/rstudio/pool.svg?label=Backlog&title=Backlog)](http://waffle.io/rstudio/pool)
[![Stories in Ready](https://badge.waffle.io/rstudio/pool.svg?label=Ready&title=Ready)](http://waffle.io/rstudio/pool)
[![Stories in In Progress](https://badge.waffle.io/rstudio/pool.svg?label=In%20Progress&title=In%20Progress)](http://waffle.io/rstudio/pool)
[![Stories in Done](https://badge.waffle.io/rstudio/pool.svg?label=Done&title=Done)](http://waffle.io/rstudio/pool)

[![Throughput Graph](https://graphs.waffle.io/rstudio/pool/throughput.svg)](https://waffle.io/rstudio/pool/metrics/throughput)

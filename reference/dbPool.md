# Create a pool of database connections

`dbPool()` is a drop-in replacement for
[`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)
that provides a shared pool of connections that can automatically
reconnect to the database if needed. See
[DBI-wrap](http://rstudio.github.io/pool/reference/DBI-wrap.md) for
methods to use with pool objects, and
[DBI-custom](http://rstudio.github.io/pool/reference/DBI-custom.md) for
unsupported methods and the "pool" way of using them.

## Usage

``` r
dbPool(
  drv,
  ...,
  minSize = 1,
  maxSize = Inf,
  onCreate = NULL,
  idleTimeout = 60,
  validationInterval = 60,
  validateQuery = NULL
)
```

## Arguments

- drv:

  A [DBI Driver](https://dbi.r-dbi.org/reference/DBIDriver-class.html),
  e.g.
  [`RSQLite::SQLite()`](https://rsqlite.r-dbi.org/reference/SQLite.html),
  `RPostgres::Postgres()`, `odbc::odbc()` etc.

- ...:

  Arguments passed on to
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html).
  These are used to identify the database and provide needed
  authentication.

- minSize, maxSize:

  The minimum and maximum number of objects in the pool.

- onCreate:

  A function that takes a single argument, a connection, and is called
  when the connection is created. Use this with
  [`DBI::dbExecute()`](https://dbi.r-dbi.org/reference/dbExecute.html)
  to set default options on every connection created by the pool.

- idleTimeout:

  Number of seconds to wait before destroying idle objects (i.e. objects
  available for checkout over and above `minSize`).

- validationInterval:

  Number of seconds to wait between validating objects that are
  available for checkout. These objects are validated in the background
  to keep them alive.

  To force objects to be validated on every checkout, set
  `validationInterval = 0`.

- validateQuery:

  A simple query that can be used to verify that the connetction is
  valid. If not provided, `dbPool()` will try a few common options, but
  these don't work for all databases.

## Details

A new connection is created transparently

- if the pool is empty

- if the currently checked out connection is invalid (checked at most
  once every `validationInterval` seconds)

- if the pool is not full and the connections are all in use

Use
[`poolClose()`](http://rstudio.github.io/pool/reference/Pool-class.md)
to close the pool and all connections in it. See
[`poolCreate()`](http://rstudio.github.io/pool/reference/Pool-class.md)
for details on the internal workings of the pool.

## Examples

``` r
# You use a dbPool in the same way as a standard DBI connection
pool <- dbPool(RSQLite::SQLite(), dbname = demoDb())
pool
#> <Pool> of SQLiteConnection objects
#>   Objects checked out: 0
#>   Available in pool: 1
#>   Max size: Inf
#>   Valid: TRUE

dbGetQuery(pool, "SELECT * FROM mtcars LIMIT 4")
#>            model  mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> 1      Mazda RX4 21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
#> 2  Mazda RX4 Wag 21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
#> 3     Datsun 710 22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
#> 4 Hornet 4 Drive 21.4   6  258 110 3.08 3.215 19.44  1  0    3    1

# Always close a pool when you're done using it
poolClose(pool)
```

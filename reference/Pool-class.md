# Create a pool of reusable objects

A generic pool class that holds objects. These can be fetched from the
pool and released back to it at will, with very little computational
cost. The pool should be created only once and closed when it is no
longer needed, to prevent leaks.

Every usage of `poolCreate()` should always be paired with a call to
`poolClose()` to avoid "leaking" resources. In shiny app, you should
create the pool outside of the server function and close it on stop,
i.e. `onStop(function() pool::poolClose(pool))`.

See [`dbPool()`](http://rstudio.github.io/pool/reference/dbPool.md) for
an example of object pooling applied to DBI database connections.

## Usage

``` r
poolCreate(
  factory,
  minSize = 1,
  maxSize = Inf,
  idleTimeout = 60,
  validationInterval = 60,
  state = NULL
)

poolClose(pool)

# S4 method for class 'Pool'
poolClose(pool)
```

## Arguments

- factory:

  A zero-argument function called to create the objects that the pool
  will hold (e.g. for DBI database connections,
  [`dbPool()`](http://rstudio.github.io/pool/reference/dbPool.md) uses a
  wrapper around
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)).

- minSize, maxSize:

  The minimum and maximum number of objects in the pool.

- idleTimeout:

  Number of seconds to wait before destroying idle objects (i.e. objects
  available for checkout over and above `minSize`).

- validationInterval:

  Number of seconds to wait between validating objects that are
  available for checkout. These objects are validated in the background
  to keep them alive.

  To force objects to be validated on every checkout, set
  `validationInterval = 0`.

- state:

  A `pool` public variable to be used by backend authors.

- pool:

  A Pool object previously created with `poolCreate`

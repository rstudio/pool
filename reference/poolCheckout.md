# Check out and return object from the pool

Use `poolCheckout()` to check out an object from the pool and
`poolReturn()` to return it. You will receive a warning if all objects
aren't returned before the pool is closed.

`localCheckout()` is a convenience function that can be used inside
functions (and other function-scoped operations like
[`shiny::reactive()`](https://rdrr.io/pkg/shiny/man/reactive.html) and
[`local()`](https://rdrr.io/r/base/eval.html)). It checks out an object
and automatically returns it when the function exits

Note that validation is only performed when the object is checked out,
so you generally want to keep the checked out around for as little time
as possible.

When pooling DBI database connections, you normally would not use
`poolCheckout()`. Instead, for single-shot queries, treat the pool
object itself as the DBI connection object and it will perform
checkout/return for you. And for transactions, use
[`poolWithTransaction()`](http://rstudio.github.io/pool/reference/poolWithTransaction.md).
See [`dbPool()`](http://rstudio.github.io/pool/reference/dbPool.md) for
an example.

## Usage

``` r
poolCheckout(pool)

# S4 method for class 'Pool'
poolCheckout(pool)

poolReturn(object)

# S4 method for class 'ANY'
poolReturn(object)

localCheckout(pool, env = parent.frame())
```

## Arguments

- pool:

  The pool to get the object from.

- object:

  Object to return

- env:

  Environment corresponding to the execution frame. For expert use only.

## Examples

``` r
pool <- dbPool(RSQLite::SQLite())
# For illustration only. You normally would not explicitly use
# poolCheckout with a DBI connection pool (see Description).
con <- poolCheckout(pool)
con
#> <SQLiteConnection>
#>   Path: 
#>   Extensions: TRUE
poolReturn(con)

f <- function() {
  con <- localCheckout(pool)
  # do something ...
}
f()

poolClose(pool)
```

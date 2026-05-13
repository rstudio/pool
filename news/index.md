# Changelog

## pool (development version)

- Added compatibility for dbplyr’s upcoming dialect-based dispatch.

## pool 1.0.4

CRAN release: 2024-10-07

- Switched from hosted MySQL database to local SQLite database in
  examples.

## pool 1.0.3

CRAN release: 2024-02-14

- Now explicitly requires DBI 1.2.0
  ([\#178](https://github.com/rstudio/pool/issues/178)) and messages if
  you’re using an old dbplyr
  ([\#179](https://github.com/rstudio/pool/issues/179)).

## pool 1.0.2

CRAN release: 2024-01-18

- No longer depends on the withr package, by instead requiring R 3.6.

- Add wrappers for dbplyr generics `db_col_types()`
  ([\#171](https://github.com/rstudio/pool/issues/171)) and
  `db_copy_to()` ([\#172](https://github.com/rstudio/pool/issues/172)).

- Pool no longer generates spurious messages about needing to use
  `in_schema()` or avoiding the use of `ident_q()`.

- Add support for new DBI generics that return Arrow objects.

## pool 1.0.1

CRAN release: 2023-02-21

- [`copy_to()`](https://dplyr.tidyverse.org/reference/copy_to.html) now
  returns a tbl that uses the Pool.

- Added missing methods for `sql_join_suffix()`
  ([\#165](https://github.com/rstudio/pool/issues/165)) and
  `sql_query_explain()`
  ([\#167](https://github.com/rstudio/pool/issues/167)).

## pool 1.0.0

CRAN release: 2023-02-11

### New features

- Pool has been re-licensed to MIT
  ([\#158](https://github.com/rstudio/pool/issues/158)).

- [`dbPool()`](http://rstudio.github.io/pool/reference/dbPool.md) gains
  an `onCreate` parameter that allows you do something to every
  connection that pool creates. This is useful for setting options that
  you want to apply to every connection
  ([\#98](https://github.com/rstudio/pool/issues/98)).

- New
  [`localCheckout()`](http://rstudio.github.io/pool/reference/poolCheckout.md)
  checkouts and then automatically returns an object. It only works in
  function scope.

### Minor improvements and bug fixes

- Pools now get a useful print method
  ([\#140](https://github.com/rstudio/pool/issues/140)).

- pool now implements the dbplyr 2.0.0 interface, eliminating warnings
  when using pool with dplyr
  ([\#132](https://github.com/rstudio/pool/issues/132)).

- Pool errors and warnings have been reviewed with an eye to making them
  more immediately actionable
  ([\#145](https://github.com/rstudio/pool/issues/145)).

- Objects are now validated once on first checkout to ensure that the
  object and validation strategy are both ok.

- Added support for SAP HANA databases
  ([@marcosci](https://github.com/marcosci),
  [\#103](https://github.com/rstudio/pool/issues/103)).

- [`dbPool()`](http://rstudio.github.io/pool/reference/dbPool.md) and
  [`poolCreate()`](http://rstudio.github.io/pool/reference/Pool-class.md)
  now default to validating every 60s, rather than every 600s. This
  makes pools a little more robust to shorter connection timeouts
  ([\#149](https://github.com/rstudio/pool/issues/149)).

- [`dbPool()`](http://rstudio.github.io/pool/reference/dbPool.md)’s
  `validateQuery` is now actually used
  ([\#153](https://github.com/rstudio/pool/issues/153)).

- DBI methods should dispatch correctly in more cases; in particular
  [`dbReadTable()`](https://dbi.r-dbi.org/reference/dbReadTable.html)
  and friends will now work correctly when used with
  [`DBI::Id()`](https://dbi.r-dbi.org/reference/Id.html)
  ([\#120](https://github.com/rstudio/pool/issues/120)).

## pool 0.1.6

CRAN release: 2021-01-14

- [`left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
  and friends once again work with pool objects
  ([\#111](https://github.com/rstudio/pool/issues/111)).

- [`dbPool()`](http://rstudio.github.io/pool/reference/dbPool.md)
  objects previously could leak memory.
  ([\#115](https://github.com/rstudio/pool/issues/115))

## pool 0.1.5

CRAN release: 2020-11-03

- dplyr and dbplyr are now Suggests instead of Imports. Thanks,
  [@AkhilGNair](https://github.com/AkhilGNair)!
  ([\#106](https://github.com/rstudio/pool/issues/106))

- When used with dbplyr, `tbl`s now store a copy of the pool, not a
  checked out connection.
  ([\#107](https://github.com/rstudio/pool/issues/107))

- [`dbListObjects()`](https://dbi.r-dbi.org/reference/dbListObjects.html),
  [`dbCreateTable()`](https://dbi.r-dbi.org/reference/dbCreateTable.html),
  [`dbAppendTable()`](https://dbi.r-dbi.org/reference/dbAppendTable.html),
  [`dbIsReadOnly()`](https://dbi.r-dbi.org/reference/dbIsReadOnly.html),
  [`dbQuoteLiteral()`](https://dbi.r-dbi.org/reference/dbQuoteLiteral.html),
  and
  [`dbUnquoteIdentifier()`](https://dbi.r-dbi.org/reference/dbUnquoteIdentifier.html)
  methods are now implemented for pool objects.
  ([\#100](https://github.com/rstudio/pool/issues/100),
  [\#109](https://github.com/rstudio/pool/issues/109))

## pool 0.1.4.3

CRAN release: 2019-10-03

- Previously, pool would always set `options(warn=1)` when running
  tasks. It now ensures that the value of `warn` can be 1 or greater.
  This can be useful in debugging, so that `options(warn=2)` can be
  used. ([\#90](https://github.com/rstudio/pool/issues/90))

## pool 0.1.4.2

CRAN release: 2019-01-07

- Update unit test for compatibility with future dbplyr.
  ([\#82](https://github.com/rstudio/pool/issues/82))

## pool 0.1.4.1

CRAN release: 2018-06-29

- Change package maintainer

## pool 0.1.4

CRAN release: 2018-03-10

- Changed the methods
  [`dbExistsTable()`](https://dbi.r-dbi.org/reference/dbExistsTable.html),
  [`dbRemoveTable()`](https://dbi.r-dbi.org/reference/dbRemoveTable.html),
  [`dbWriteTable()`](https://dbi.r-dbi.org/reference/dbWriteTable.html),
  [`dbGetQuery()`](https://dbi.r-dbi.org/reference/dbGetQuery.html),
  [`dbExecute()`](https://dbi.r-dbi.org/reference/dbExecute.html),
  [`dbListFields()`](https://dbi.r-dbi.org/reference/dbListFields.html)
  and
  [`dbReadTable()`](https://dbi.r-dbi.org/reference/dbReadTable.html) to
  dispatch over the first two arguments, as per the default definition
  in DBI. ([\#57](https://github.com/rstudio/pool/issues/57))

## pool 0.1.3

CRAN release: 2017-11-03

- Use
  [`requireNamespace("pkg", quietly = TRUE)`](https://rdrr.io/r/base/ns-load.html)
  for `RMySQL` and `RSQLite` in the examples and tests since these are
  “Suggests” packages (i.e. not “Depends”). (commit 4205feb)

## pool 0.1.2

CRAN release: 2017-11-03

#### Minor new features and improvements

- Included more examples in the documentation.
  ([\#50](https://github.com/rstudio/pool/issues/50))

- Fixed the “test-create-destroy.R” test. Previously, this test had to
  be run manually because it uses `later` and its async nature could not
  be captured by `testthat`. However, using
  [`later::run_now()`](https://later.r-lib.org/reference/run_now.html)
  immediately after the relevant code snippet (i.e. still inside the
  first argument of `expect_*`) solves the issue.
  ([\#50](https://github.com/rstudio/pool/issues/50))

- Use `difftime(t1, t0, units = "secs")` when calculating a time
  interval. Unlike the simpler `t1 - t0` method, this guarantees that
  the result will always be consistently the number of *seconds*.
  However, there’s no change when calculating a new time (*not* a time
  interval) using `t2 <- t1 - interval`, since we want `t2` to be a
  time, rather than a time interval (which is always what is returned by
  `difftime`). ([\#50](https://github.com/rstudio/pool/issues/50) and
  [\#48](https://github.com/rstudio/pool/issues/48), thank you
  [@caewok](https://github.com/caewok)!)

#### Bug fixes

- Fix all dbplyr wrapper functions that weren’t passing in additional
  arguments because the call to the original `dbplyr` function included
  `... = ...` instead of `...`. Also, pass `temporary = temporary` in
  `copy_to.Pool`, so that we don’t defeat the whole purpose of that
  wrapper. ([\#50](https://github.com/rstudio/pool/issues/50))

- Change the place where the check for the maximum number of objects is
  made. Previously, this chunk of code was misplaced and it would result
  in buggy behavior: namely, once the maximum number of objects was
  reached, no more objects could be checked out (**even if you returned
  any/all objects back to the pool**). The only reason this wasn’t
  spotted earlier is because the default `maxSize` is `Inf` (and there’s
  usually not a good reason to change it).
  ([\#50](https://github.com/rstudio/pool/issues/50))

## pool 0.1.1

CRAN release: 2017-09-23

#### Breaking changes

- Fix [\#39](https://github.com/rstudio/pool/issues/39): Moved `dplyr`
  support in `pool` from `dplyr` 0.5.0 to `dplyr` 0.7.0, which includes
  a lot of breaking changes including the addition of a brand new
  package called `dbplyr`.
  ([\#42](https://github.com/rstudio/pool/issues/42))

  For `pool` users, the main change is that all the `src_*` functions
  are now gone (from `dplyr` and `pool`). Therefore, if you had
  something like:

  ``` r

  data <- src_pool(pool) %>% tbl("test")
  ```

  You can just change it to the simpler:

  ``` r

  data <- pool %>% tbl("test")
  ```

  If you’re still on an old version of `dplyr` and want to use `pool` as
  well, please install the package using the tag created for that
  purpose:

  ``` r

  devtools::install_github("rstudio/pool@dplyr-pre-0.7.0-compat")
  ```

- Changed all time arguments to accept number of ***seconds***, instead
  of milliseconds. This is because this is what the `later` package uses
  and there was no reason for `pool` to be different, except backward
  compatibility. Since both time arguments to `dbPool` (`idleTimeout`
  and `validationInterval`) have default values, we’re hoping this
  change won’t even be noticed by most users. If you were setting either
  of those directly, however, you will need to update your app if you
  update the `pool` package.
  ([\#44](https://github.com/rstudio/pool/issues/44))

- Dropped the Pool methods around `dbConnect` and `dbDisconnect`,
  because these made it easier to lose track of whether you’re operating
  on a Pool object or on a database connection directly. From now on,
  only these allow you to get a connection from the pool and return it
  back, respectively:
  ([\#44](https://github.com/rstudio/pool/issues/44))

  ``` r

  con <- poolCheckout(pool)
  poolReturn(con)
  ```

#### New features

- Use `later` package for scheduling tasks
  ([\#44](https://github.com/rstudio/pool/issues/44)). This also has the
  side effect of fixing
  [\#40](https://github.com/rstudio/pool/issues/40) and
  [\#43](https://github.com/rstudio/pool/issues/43) since `later` allows
  us to get rid of the `naiveScheduler` completely.

#### Library updates

- Roxygen 5.0.1 to 6.0.1. (commit
  [\#9952000](https://github.com/rstudio/pool/issues/9952000))

## pool 0.1.0

- Initial release!

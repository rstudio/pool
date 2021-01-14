# pool 0.1.6

pool 0.1.5.9000
================

* `left_join()` and friends once again work with pool objects (#111).

* `dbPool()` objects previously could leak memory. (#115)

pool 0.1.5
================

* dplyr and dbplyr are now Suggests instead of Imports. Thanks, @AkhilGNair! (#106)

* When used with dbplyr, `tbl`s now store a copy of the pool, not a checked
  out connection. (#107)

* `dbListObjects()`, `dbCreateTable()`, `dbAppendTable()`, `dbIsReadOnly()`, `dbQuoteLiteral()`, and `dbUnquoteIdentifier()` methods are now implemented for pool objects. (#100, #109)

pool 0.1.4.3
================

* Previously, pool would always set `options(warn=1)` when running tasks. It now ensures that the value of `warn` can be 1 or greater. This can be useful in debugging, so that `options(warn=2)` can be used. (#90)

pool 0.1.4.2
================

* Update unit test for compatibility with future dbplyr. (#82)

pool 0.1.4.1
================

* Change package maintainer

pool 0.1.4
================

* Changed the methods `dbExistsTable()`, `dbRemoveTable()`, `dbWriteTable()`, `dbGetQuery()`, `dbExecute()`, `dbListFields()` and `dbReadTable()` to dispatch over the first two arguments, as per the [default definition in DBI](https://github.com/r-dbi/DBI/blob/master/R/DBConnection.R). (#57)

pool 0.1.3
================

* Use `requireNamespace("pkg", quietly = TRUE)` for `RMySQL` and `RSQLite` in the examples and tests since these are "Suggests" packages (i.e. not "Depends"). (commit 4205feb)

pool 0.1.2
================

### Minor new features and improvements

* Included more examples in the documentation. (#50)

* Fixed the "test-create-destroy.R" test. Previously, this test had to be run manually because it uses `later` and its async nature could not be captured by `testthat`. However, using `later::run_now()` immediately after the relevant code snippet (i.e. still inside the first argument of `expect_*`) solves the issue. (#50)

* Use `difftime(t1, t0, units = "secs")` when calculating a time interval. Unlike the simpler `t1 - t0` method, this guarantees that the result will always be consistently the number of _seconds_. However, there's no change when calculating a new time (_not_ a time interval) using `t2 <- t1 - interval`, since we want `t2` to be a time, rather than a time interval (which is always what is returned by `difftime`). (#50 and #48, thank you [@caewok](https://github.com/caewok)!)

### Bug fixes

* Fix all dbplyr wrapper functions that weren't passing in additional arguments because the call to the original `dbplyr` function included `... = ...` instead of `...`. Also, pass `temporary = temporary` in `copy_to.Pool`, so that we don't defeat the whole purpose of that wrapper. (#50)

* Change the place where the check for the maximum number of objects is made. Previously, this chunk of code was misplaced and it would result in buggy behavior: namely, once the maximum number of objects was reached, no more objects could be checked out (**even if you returned any/all objects back to the pool**). The only reason this wasn't spotted earlier is because the default `maxSize` is `Inf` (and there's usually not a good reason to change it). (#50)


pool 0.1.1
================

### Breaking changes
* Fix #39: Moved `dplyr` support in `pool` from `dplyr` 0.5.0 to `dplyr` 0.7.0, which includes a lot of breaking changes including the addition of a brand new package called `dbplyr`. (#42)

    For `pool` users, the main change is that all the `src_*` functions are now gone (from `dplyr` and `pool`). Therefore, if you had something like:

    ```r
    data <- src_pool(pool) %>% tbl("test")
    ```

    You can just change it to the simpler:

    ```r
    data <- pool %>% tbl("test")
    ```

    If you're still on an old version of `dplyr` and want to use `pool` as well, please install the package using the tag created for that purpose:

    ```r
    devtools::install_github("rstudio/pool@dplyr-pre-0.7.0-compat")
    ```

* Changed all time arguments to accept number of _**seconds**_, instead of milliseconds. This is because this is what the `later` package uses and there was no reason for `pool` to be different, except backward compatibility. Since both time arguments to `dbPool` (`idleTimeout` and `validationInterval`) have default values, we're hoping this change won't even be noticed by most users. If you were setting either of those directly, however, you will need to update your app if you update the `pool` package. (#44)
<!--Since this release is already breaking backward compatibility, we're going to town!-->

* Dropped the Pool methods around `dbConnect` and `dbDisconnect`, because these made it easier to lose track of whether you're operating on a Pool object or on a database connection directly. From now on, only these allow you to get a connection from the pool and return it back, respectively: (#44)

    ```r
    con <- poolCheckout(pool)
    poolReturn(con)
    ```

### New features
* Use `later` package for scheduling tasks (#44). This also has the side effect of fixing #40 and #43 since `later` allows us to get rid of the `naiveScheduler` completely.

### Library updates
* Roxygen 5.0.1 to 6.0.1. (commit #9952000)

pool 0.1.0
===========

* Initial release!

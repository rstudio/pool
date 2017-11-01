pool 0.1.1.9000
================

## Full changelog
### Breaking changes
### New features
### Minor new features and improvements

* Fixed the "test-create-destroy.R" test. Previsouly, this test had to be run manually because it uses `later` and its async nature could not be captured by `testthat`. However, using `later::run_now()` immediately after the relevant code snippet (i.e. still inside the first argument of `expect_*`) solves the issue. ([#50](https://github.com/rstudio/pool/pull/50))

* Use `difftime(t1, t0, units = "secs")` when calculating a time interval. Unlike the simpler `t1 - t0` method, this guarantees that the result will always be consistently the number of _seconds_. However, there's no change when calculating a new time (_not_ a time interval) using `t2 <- t1 - interval`, since we want `t2` to be a time, rather than a time interval (which is always what is returned by `difftime`). ([#50](https://github.com/rstudio/pool/pull/50) and [#48](https://github.com/rstudio/pool/pull/48), thank you [@caewok](https://github.com/caewok)!)

### Bug fixes

* Fix all dbplyr wrapper functions that weren't passing in additional arguments because the call to the original `dbplyr` function included `... = ...` instead of `...`. Also, pass `temporary = temporary` in `copy_to.Pool`, so that we don't defeat the whole purpose of that wrapper. ([#50](https://github.com/rstudio/pool/pull/50))

* Change the place where the check for the maximum number of objects is made. Previsouly, this chunk of code was misplaced and it would result in buggy behavior: namely, once the maximum number of objects was reached, no more objects could be checked out (**even if you returned any/all objects back to the pool**). The only reason this wasn't spotted earlier is because the default `maxSize` is `Inf` (and there's usually not a good reason to change it). ([#50](https://github.com/rstudio/pool/pull/50))

### Library updates

pool 0.1.1
================

### Breaking changes
* Fix [#39](https://github.com/rstudio/pool/issues/39): Moved `dplyr` support in `pool` from `dplyr` 0.5.0 to `dplyr` 0.7.0, which includes a lot of breaking changes including the addition of a brand new package called `dbplyr`. ([#42](https://github.com/rstudio/pool/pull/42))

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

* Changed all time arguments to accept number of _**seconds**_, instead of milliseconds. This is because this is what the `later` package uses and there was no reason for `pool` to be different, except backward compatibility. Since both time arguments to `dbPool` (`idleTimeout` and `validationInterval`) have default values, we're hoping this change won't even be noticed by most users. If you were setting either of those directly, however, you will need to update your app if you update the `pool` package. ([#44](https://github.com/rstudio/pool/pull/44))
<!--Since this release is already breaking backward compatibility, we're going to town!-->

* Dropped the Pool methods around `dbConnect` and `dbDisconnect`, because these made it easier to lose track of whether you're operating on a Pool object or on a database connection directly. From now on, only these allow you to get a connection from the pool and return it back, respectively: ([#44](https://github.com/rstudio/pool/pull/44))

    ```r
    con <- poolCheckout(pool)
    poolReturn(con)
    ```

### New features
* Use `later` package for scheduling tasks ([#44](https://github.com/rstudio/pool/pull/44)). This also has the side effect of fixing [#40](https://github.com/rstudio/pool/issues/40) and [#43](https://github.com/rstudio/pool/issues/43) since `later` allows us to get rid of the `naiveScheduler` completely.

### Library updates
* Roxygen 5.0.1 to 6.0.1. (commit [#9952000](https://github.com/rstudio/pool/commit/99520001a65dd51a4f5eaaacad4bfbec696cc0f1))

pool 0.1.0
===========

* Initial release!

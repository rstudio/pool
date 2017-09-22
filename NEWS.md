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

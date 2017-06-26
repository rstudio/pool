pool 0.1.0.9000
================

## Full changelog

### Breaking changes
* Fix [#39](https://github.com/rstudio/pool/issues/39): Moved `dplyr` support in `pool` from `dplyr` 0.5.0 to `dplyr` 0.7.0, which includes a lot of breaking changes including the addition of a brand new package called `dbplyr`. ([#42](https://github.com/rstudio/pool/pull/42))

* Dropped the Pool methods around `dbConnect` and `dbDisconnect`, because these made it easier to lose track of whether you're operating on a Pool object or on a database connection directly. From now on, only `con <- poolCheckout(pool)` and `poolReturn(con)` allow you to get a connection from the pool and return it back, respectively. ([#44](https://github.com/rstudio/pool/pull/44))

### New features
* Use `later` package for scheduling tasks ([#44](https://github.com/rstudio/pool/pull/44)). This also has the side effect of fixing [#40](https://github.com/rstudio/pool/issues/40) and [#43](https://github.com/rstudio/pool/issues/43) since `later` allows us to get rid of the `naiveScheduler` completely.

### Minor new features and improvements

### Bug fixes

### Library updates
* Roxygen 5.0.1 to 6.0.1 (commit (#9952000)[https://github.com/rstudio/pool/commit/99520001a65dd51a4f5eaaacad4bfbec696cc0f1])

pool 0.1.0
===========

* Initial release!

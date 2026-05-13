# Use pool with dbplyr

Wrappers for key dplyr (and dbplyr) methods so that pool works
seemlessly with [dbplyr](https://dbplyr.tidyverse.org/).

## Usage

``` r
tbl.Pool(src, from, ..., vars = NULL)

copy_to.Pool(dest, df, name = NULL, overwrite = FALSE, temporary = TRUE, ...)
```

## Arguments

- src, dest:

  A [dbPool](http://rstudio.github.io/pool/reference/dbPool.md).

- from:

  Name table or
  [`dbplyr::sql()`](https://dbplyr.tidyverse.org/reference/sql.html)
  string.

- ...:

  Other arguments passed on to the individual methods

- vars:

  A character vector of variable names in `src`. For expert use only.

- df:

  A local data frame, a `tbl_sql` from same source, or a `tbl_sql` from
  another source. If from another source, all data must transition
  through R in one pass, so it is only suitable for transferring small
  amounts of data.

- name:

  Name for remote table. Defaults to the name of `df`, if it's an
  identifier, otherwise uses a random name.

- overwrite:

  If `TRUE`, will overwrite an existing table with name `name`. If
  `FALSE`, will throw an error if `name` already exists.

- temporary:

  if `TRUE`, will create a temporary table that is local to this
  connection and will be automatically deleted when the connection
  expires

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union

pool <- dbPool(RSQLite::SQLite())
# copy a table into the database
copy_to(pool, mtcars, "mtcars", temporary = FALSE)
#> # Source:   table<`mtcars`> [?? x 11]
#> # Database: sqlite 3.52.0 []
#>      mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1  21       6  160    110  3.9   2.62  16.5     0     1     4     4
#>  2  21       6  160    110  3.9   2.88  17.0     0     1     4     4
#>  3  22.8     4  108     93  3.85  2.32  18.6     1     1     4     1
#>  4  21.4     6  258    110  3.08  3.22  19.4     1     0     3     1
#>  5  18.7     8  360    175  3.15  3.44  17.0     0     0     3     2
#>  6  18.1     6  225    105  2.76  3.46  20.2     1     0     3     1
#>  7  14.3     8  360    245  3.21  3.57  15.8     0     0     3     4
#>  8  24.4     4  147.    62  3.69  3.19  20       1     0     4     2
#>  9  22.8     4  141.    95  3.92  3.15  22.9     1     0     4     2
#> 10  19.2     6  168.   123  3.92  3.44  18.3     1     0     4     4
#> # ℹ more rows

# retrieve a table
mtcars_db <- tbl(pool, "mtcars")
mtcars_db
#> # Source:   table<`mtcars`> [?? x 11]
#> # Database: sqlite 3.52.0 []
#>      mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1  21       6  160    110  3.9   2.62  16.5     0     1     4     4
#>  2  21       6  160    110  3.9   2.88  17.0     0     1     4     4
#>  3  22.8     4  108     93  3.85  2.32  18.6     1     1     4     1
#>  4  21.4     6  258    110  3.08  3.22  19.4     1     0     3     1
#>  5  18.7     8  360    175  3.15  3.44  17.0     0     0     3     2
#>  6  18.1     6  225    105  2.76  3.46  20.2     1     0     3     1
#>  7  14.3     8  360    245  3.21  3.57  15.8     0     0     3     4
#>  8  24.4     4  147.    62  3.69  3.19  20       1     0     4     2
#>  9  22.8     4  141.    95  3.92  3.15  22.9     1     0     4     2
#> 10  19.2     6  168.   123  3.92  3.44  18.3     1     0     4     4
#> # ℹ more rows
mtcars_db %>% select(mpg, cyl, disp)
#> # Source:   SQL [?? x 3]
#> # Database: sqlite 3.52.0 []
#>      mpg   cyl  disp
#>    <dbl> <dbl> <dbl>
#>  1  21       6  160 
#>  2  21       6  160 
#>  3  22.8     4  108 
#>  4  21.4     6  258 
#>  5  18.7     8  360 
#>  6  18.1     6  225 
#>  7  14.3     8  360 
#>  8  24.4     4  147.
#>  9  22.8     4  141.
#> 10  19.2     6  168.
#> # ℹ more rows
mtcars_db %>% filter(cyl == 6) %>% collect()
#> # A tibble: 7 × 11
#>     mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  21       6  160    110  3.9   2.62  16.5     0     1     4     4
#> 2  21       6  160    110  3.9   2.88  17.0     0     1     4     4
#> 3  21.4     6  258    110  3.08  3.22  19.4     1     0     3     1
#> 4  18.1     6  225    105  2.76  3.46  20.2     1     0     3     1
#> 5  19.2     6  168.   123  3.92  3.44  18.3     1     0     4     4
#> 6  17.8     6  168.   123  3.92  3.44  18.9     1     0     4     4
#> 7  19.7     6  145    175  3.62  2.77  15.5     0     1     5     6

poolClose(pool)
```

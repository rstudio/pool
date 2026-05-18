# Using pool with dplyr

``` r

library(pool)
```

This article shows you how to use dbplyr with pool in your shiny app. To
get started, make sure you have all the packages you need:

``` r
install.packages(c("shiny", "DBI", "dbplyr", "dplyr", "pool")
```

## Getting started

For the purposes of this article I’m going to start by creating a very
simple in-memory SQLite database. This makes it easy to show you real
code and the only difference from what you’ll use is details of the
database connection.

``` r

path <- tempfile()
con <- DBI::dbConnect(RSQLite::SQLite(), dbname = path)
DBI::dbWriteTable(con, "mtcars", mtcars)
```

First, let’s consider how you might connect use a simple database just
with dplyr:

``` r

library(dplyr, warn.conflicts = FALSE)

con <- DBI::dbConnect(RSQLite::SQLite(), dbname = path)
mtcars_db <- con %>% tbl("mtcars")

mtcars_db %>% 
  filter(cyl == 8) %>% 
  head()
#> # Source:   SQL [?? x 11]
#> # Database: sqlite 3.52.0 [/tmp/RtmpIXnp2Q/file1bea7384bb83]
#>     mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  18.7     8  360    175  3.15  3.44  17.0     0     0     3     2
#> 2  14.3     8  360    245  3.21  3.57  15.8     0     0     3     4
#> 3  16.4     8  276.   180  3.07  4.07  17.4     0     0     3     3
#> 4  17.3     8  276.   180  3.07  3.73  17.6     0     0     3     3
#> 5  15.2     8  276.   180  3.07  3.78  18       0     0     3     3
#> 6  10.4     8  472    205  2.93  5.25  18.0     0     0     3     4
```

Now, let’s do the same thing using with a pool:

``` r

con <- pool::dbPool(RSQLite::SQLite(), dbname = path)
mtcars_db <- con %>% tbl("mtcars")

mtcars_db %>% 
  filter(cyl == 8) %>% 
  head()
#> # Source:   SQL [?? x 11]
#> # Database: sqlite 3.52.0 [/tmp/RtmpIXnp2Q/file1bea7384bb83]
#>     mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1  18.7     8  360    175  3.15  3.44  17.0     0     0     3     2
#> 2  14.3     8  360    245  3.21  3.57  15.8     0     0     3     4
#> 3  16.4     8  276.   180  3.07  4.07  17.4     0     0     3     3
#> 4  17.3     8  276.   180  3.07  3.73  17.6     0     0     3     3
#> 5  15.2     8  276.   180  3.07  3.78  18       0     0     3     3
#> 6  10.4     8  472    205  2.93  5.25  18.0     0     0     3     4
```

As usually, all you need to do is change
[`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html) to
[`pool::dbPool()`](http://rstudio.github.io/pool/reference/dbPool.md),
and pool will take care of the rest!

### Shiny apps

Now lets show it in action in a shiny app:

``` r

library(shiny)
library(dplyr, warn.conflicts = FALSE)

pool <- pool::dbPool(RSQLite::SQLite(), dbname = path)
onStop(function() {
  pool::poolClose(pool)
})

ui <- fluidPage(
  textInput("cyl", "Enter number of cylinders:", "6"),
  numericInput("nrows", "How many rows to show?", 10),
  tableOutput("tbl")
)

server <- function(input, output, session) {
  output$tbl <- renderTable({
    pool %>% 
      tbl("mtcars") %>%
      filter(cyl == local(input$cyl)) %>% 
      head(input$nrows)
  })
}

if (interactive())
  shinyApp(ui, server)
```

There’s one other important tool to note: `dbplyr::local()`. This tells
dbplyr to run `input$cyl` locally (retrieving the value the user typed),
rather than trying to convert it to SQL.

Note that there is no need to do your own input sanitizing for SQL
injection prevention (i.e. no need to call a function like
[`DBI::sqlInterpolate()`](https://dbi.r-dbi.org/reference/sqlInterpolate.html))
because dbplyr takes care of that for you.

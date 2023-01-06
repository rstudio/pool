pool
====

<!-- badges: start -->
[![R-CMD-check](https://github.com/rstudio/pool/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rstudio/pool/actions/workflows/R-CMD-check.yaml)
[![R build status](https://github.com/rstudio/pool/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rstudio/pool/actions)
[![Codecov test coverage](https://codecov.io/gh/rstudio/pool/branch/main/graph/badge.svg)](https://codecov.io/gh/rstudio/pool?branch=main)
<!-- badges: end -->

The goal of the **pool** package is to abstract away the logic of database connection management and the performance cost of repeated connection creation. These concerns are especially prominent in interactive contexts, e.g. Shiny apps that connect to a remote database). pool provides objects that work seamlessly with the DBI and dplyr, so in most cases using pool is as simple replacing `DBI::dbConnect()` with `dbPool()` and adding a call to `poolClose()`.

The `pool` package adds a new level of abstraction when connecting to a database: instead of directly fetching a connection from the database, you will create an object (called a pool) with a reference to that database. The pool holds a number of connections to the database. Some of these may be currently in-use and some of these may be idle, waiting for a query to request them. Each time you make a query, you are querying the pool, rather than the database. Under the hood, the pool will either give you an idle connection that it previously fetched from the database or, if it has no free connections, fetch one and give it to you. You never have to create or close connections directly: the pool knows when it should grow, shrink or keep steady. You only need to close the pool when you’re done. 

Learn more about why pool is needed in `vignette("why-pool")`.

(The pool package is actually general enough to allow you to construct a pool of any kind of object, not just database connections, but database connections are currently it's primary claim to fame.)

## Usage

Here’s a simple example of using a pool within a Shiny app (feel free to try it yourself):

```r
library(shiny)
library(dplyr)
library(pool)
loadNamespace("dbplyr")

pool <- dbPool(
  drv = RMySQL::MySQL(),
  dbname = "shinydemo",
  host = "shiny-demo.csa7qlmguqrf.us-east-1.rds.amazonaws.com",
  username = "guest",
  password = "guest"
)
onStop(function() {
  poolClose(pool)
})

ui <- fluidPage(
  textInput("ID", "Enter your ID:", "5"),
  tableOutput("tbl"),
  numericInput("nrows", "How many cities to show?", 10),
  plotOutput("popPlot")
)

server <- function(input, output, session) {
  output$tbl <- renderTable({
    pool %>% tbl("City") %>% filter(ID == !!input$ID) %>% collect()
  })
  output$popPlot <- renderPlot({
    df <- pool %>% tbl("City") %>% head(input$nrows) %>% collect()
    pop <- df$Population
    names(pop) <- df$Name
    barplot(pop)
  })
}

shinyApp(ui, server)
```

Note: the `loadNamespace("dbplyr")` line is there to help the [rsconnect](https://github.com/rstudio/rsconnect) package when deploying the application to [shinyapps.io](https://www.shinyapps.io/) or [Posit Connect](https://posit.co/products/enterprise/connect/). Without that line, rsconnect will not detect that the dbplyr package is needed, and the application will not work properly.

pool
====

<!-- badges: start -->
[![R-CMD-check](https://github.com/rstudio/pool/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rstudio/pool/actions/workflows/R-CMD-check.yaml)
[![R build status](https://github.com/rstudio/pool/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/rstudio/pool/actions)
[![Codecov test coverage](https://codecov.io/gh/rstudio/pool/branch/main/graph/badge.svg)](https://app.codecov.io/gh/rstudio/pool?branch=main)
<!-- badges: end -->

The goal of the **pool** package is to abstract away the challenges of database connection management, which is particularly relevant in interactive contexts like Shiny apps that connect to a database. 

Instead of creating and close connections yourself, you create a "pool" of connections, and the pool package manages them for you. You never have to create or close connections directly: the pool knows when it should grow, shrink or keep steady. You only need to close the pool when you’re done. The pool works seamlessly with DBI and dplyr, so in most cases using the pool package is as simple replacing `DBI::dbConnect()` with `dbPool()` and adding a call to `poolClose()`.

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
  city <- tbl(pool, "City")

  output$tbl <- renderTable({
    city |> filter(ID == !!input$ID) |> collect()
  })
  output$popPlot <- renderPlot({
    df <- city |> head(input$nrows) |> collect() 
    pop <- df |> pull("Population", name = "Name")
    barplot(pop)
  })
}

shinyApp(ui, server)
```

Note: the `loadNamespace("dbplyr")` line is there to help the [rsconnect](https://github.com/rstudio/rsconnect) package when deploying the application to [shinyapps.io](https://www.shinyapps.io/) or [Posit Connect](https://posit.co/products/enterprise/connect/). Without that line, rsconnect will not detect that the dbplyr package is needed, and the application will not work properly.

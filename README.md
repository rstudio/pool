pool
======
##### *Database Connection Pooling in R*

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/pool)](https://CRAN.R-project.org/package=pool)
[![R build status](https://github.com/rstudio/pool/workflows/R-CMD-check/badge.svg)](https://github.com/rstudio/pool/actions)
[![Codecov test coverage](https://codecov.io/gh/rstudio/pool/branch/main/graph/badge.svg)](https://codecov.io/gh/rstudio/pool?branch=main)
<!-- badges: end -->

The goal of the `pool` package is to abstract away the logic of connection management and the performance cost of fetching a new connection from a remote database. These concerns are especially prominent in interactive contexts, like Shiny apps (which connect to a remote database) or even at the R console. So, while this package is of most practical value to Shiny developers, there is no harm if it is used in other contexts. Since `pool` integrates with both `DBI` and `dplyr`, there are very few things that will be new to you, if you're already using either of those packages. Essentially, you shouldn't feel the difference, with the exception of creating and closing a Pool object (as opposed to connecting and disconnecting a DBIConnection object).

## Usage
Here’s a simple example of using a pool within a Shiny app (feel free to try it yourself):

```r
library(shiny)
library(dplyr)
library(pool)

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
## Concept
The `pool` package adds a new level of abstraction when connecting to a database: instead of directly fetching a connection from the database, you will create an object (called a pool) with a reference to that database. The pool holds a number of connections to the database. Some of these may be currently in-use and some of these may be idle, waiting for a query to request them. Each time you make a query, you are querying the pool, rather than the database. Under the hood, the pool will either give you an idle connection that it previously fetched from the database or, if it has no free connections, fetch one and give it to you. You never have to create or close connections directly: the pool knows when it should grow, shrink or keep steady. You only need to close the pool when you’re done.

## Context and motivation
When you’re connecting to a database, it is important to manage your connections: when to open them (taking into account that this is a potentially long process for remote databases), how to keep track of them, and when to close them. This is always true, but it becomes especially relevant for Shiny apps, where not following best practices can lead to _many_ slowdowns (from inadvertently opening too many connections) and/or _many_ leaked connections (i.e. forgetting to close connections once you no longer need them). Over time, leaked connections could accumulate and substantially slow down your app, as well as overwhelming the database itself.

Oversimplifying a bit, we can think of connection management in Shiny as a spectrum from the extreme of just having one connection per app (potentially serving several sessions of the app) to the extreme of opening (and closing) one connection for each query you make. Neither of these approaches is great. You can expand either of the arrows below to see the source code for each extreme, but that is not essential to understanding the problems described below.

<details>
  <summary><code>oneConnectionPerApp.R</code></summary>

```r
library(shiny)
library(dplyr)
library(DBI)

conn <- dbConnect(
    drv = RMySQL::MySQL(),
    dbname = "shinydemo",
    host = "shiny-demo.csa7qlmguqrf.us-east-1.rds.amazonaws.com",
    username = "guest",
    password = "guest"
  )
onStop(function() {
  dbDisconnect(conn)
})

ui <- fluidPage(
  textInput("ID", "Enter your ID:", "5"),
  tableOutput("tbl"),
  numericInput("nrows", "How many cities to show?", 10),
  plotOutput("popPlot")
)

server <- function(input, output, session) {
  output$tbl <- renderTable({
    conn %>% tbl("City") %>% filter(ID == !!input$ID) %>% collect()
  })
  output$popPlot <- renderPlot({
    df <- conn %>% tbl("City") %>% head(input$nrows) %>% collect()
    pop <- df$Population
    names(pop) <- df$Name
    barplot(pop)
  })
}

shinyApp(ui, server)
```

</details>

<details>
  <summary><code>oneConnectionPerQuery.R</code></summary>

```r
library(shiny)
library(dplyr)
library(DBI)

args <- list(
  drv = RMySQL::MySQL(),
  dbname = "shinydemo",
  host = "shiny-demo.csa7qlmguqrf.us-east-1.rds.amazonaws.com",
  username = "guest",
  password = "guest"
)

ui <- fluidPage(
  textInput("ID", "Enter your ID:", "5"),
  tableOutput("tbl"),
  numericInput("nrows", "How many cities to show?", 10),
  plotOutput("popPlot")
)

server <- function(input, output, session) {
  output$tbl <- renderTable({
    conn <- do.call(dbConnect, args)
    on.exit(dbDisconnect(conn))

    conn %>% tbl("City") %>% filter(ID == !!input$ID) %>% collect()
  })
  output$popPlot <- renderPlot({
    conn <- do.call(dbConnect, args)
    on.exit(dbDisconnect(conn))

    df <- conn %>% tbl("City") %>% head(input$nrows) %>% collect()
    pop <- df$Population
    names(pop) <- df$Name
    barplot(pop)
  })
}

shinyApp(ui, server)
```

</details>
<br>

Opening only one connection per app makes it fast (because, in the whole app, you only fetch one connection) and your code is kept as simple as possible. However:

- it cannot handle simultaneous requests (e.g. two sessions open, both querying the database at the same time);
- if the connection breaks at some point (maybe the database server crashed), you won’t get a new connection (you have to exit the app and re-run it);
- finally, if you are not quite at this extreme, and you use more than one connection per app (but fewer than one connection per query), it can be difficult to keep track of all your connections, since you’ll be opening and closing them in potentially very different places.

While the other extreme of opening (and closing) one connection for each query you make resolves all of these points, it is terribly slow (each time we need to access the database, we first have to fetch a connection), and
you need a lot more (boilerplate) code to connect and disconnect the connection within each reactive/function.

The `pool` package was created so you don't have to worry about this at all. Since `pool` abstracts away the logic of connection management, for the vast majority of cases, you never have to deal with connections directly. Since the pool “knows” when it should have more connections and how to manage them, you have all the advantages of the second approach (one connection per query), without the disadvantages. You are still using one connection per query, but that connection is always fetched and returned to the pool, rather than getting it from the database directly. This is a whole lot faster and more efficient. Finally, the code is kept just as simple as the code in the first approach (only one connection for the entire app). In fact, if you look back at the `pool` Shiny app example above, you will notice that the code structure is essentially the same that you'd use to open a connection at the start of an app and close it at the end.

## More resources

- [db.rstudio.com](https://db.rstudio.com/) for a lot of best practices, articles and demos on databases in R and in RStudio.
- [shiny.rstudio.com](https://shiny.rstudio.com/) to learn more about Shiny (there are also more articles, including on data and databases in [/articles](https://shiny.rstudio.com/articles/)).

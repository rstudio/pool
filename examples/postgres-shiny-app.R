library(shiny)
library(DBI)
library(pool)
library(dplyr)

pool <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = "postgresdemo",
  host = "postgresdemo.cfd8mtk93q6a.us-west-2.rds.amazonaws.com",
  user = "guest",
  password = "guest"
)

ui <- fluidPage(
  textInput("id", "Enter your ID:", "5"),
  tableOutput("tbl"),
  numericInput("nrows", "How many cities to show?", 10),
  plotOutput("popPlot")
)

diffs <- c()

server <- function(input, output, session) {
  output$tbl <- renderTable({
    t0 <- Sys.time()
    tbl <- pool %>% tbl("city") %>% filter(id == input$id)
    t1 <- Sys.time()
    diff <- t1 - t0
    print(diff)
    diffs[length(diffs) + 1] <<- diff
    tbl
  })

  output$popPlot <- renderPlot({
    df <- pool %>% tbl("city") %>% head(as.integer(input$nrows)[1]) %>% collect()
    pop <- df$population
    names(pop) <- df$name
    barplot(pop)
  })

  observe({
    invalidateLater(500, session)
    free <- dbGetInfo(pool)$numberFreeObjects
    taken <- dbGetInfo(pool)$numberTakenObjects
    print(paste("FREE", free))
    print(paste("TAKN", taken))
    print(paste("----------"))
  })
}

shinyApp(ui, server)

# Load packages
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(DT)

source("investing_build.R")


# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
                titlePanel("Stock Ticker Closing Price Trends with % Return"),
                sidebarLayout(
                  sidebarPanel(
                    
                    # Select type of trend to plot
                    selectInput(inputId = "type", label = strong("Stock index"),
                                choices = unique(series_ticker$ticker),
                                selected = "YNDX"),
                    
                    # Select date range to be plotted
                    dateRangeInput("date", strong("Date range"), start = "2020-04-01", end = today()-days(1),
                                   min = "2019-01-01", max = today()-days(1)),
                    

                    
                    # Display only if the smoother is checked
                    sliderInput(inputId = "weekcount", label = strong("Weeks between Buy and Sell"),
                                                 min = 1, max = 100, value = 4, step = 1,
                                                 # animate = animationOptions(interval = 100)
                                ),
                                     HTML("4 weeks by default (1 month later)"),
                                     br(),
                                  
                    # Select whether to overlay smooth trend line
                    checkboxInput(inputId = "smoother", label = strong("Display stock picks"), value = FALSE),
                                     HTML("Wait 2-3 minutes after click for stock picks"),
                       HTML("Note: min average return is 10% and max deviation is 7% for stock picks in bottom table"),
                                     br(),br(),
               
                  # sliderInput(inputId = "minavgreturn", label = strong("Min Average Return"),
                  #             min = 0, max = 1, value = 0.1, step = 0.05,
                  #             # animate = animationOptions(interval = 100)
                  # ),     HTML("10% return by minimum average return by default"),
                  # 
                  # sliderInput(inputId = "maxdevreturn", label = strong("Max Deviation Return"),
                  #             min = 0.01, max = 1, value = 0.07, step = 0.01,
                  #             # animate = animationOptions(interval = 100)
                  # ),     HTML("7% return by max average deviation by default"),
                  
                  ),
                  
                  # Output: Description, lineplot, and reference
                  mainPanel(
                    plotOutput(outputId = "lineplot", height = "500px"),
                    textOutput(outputId = "desc"),
                    tags$a(href = "https://finance.yahoo.com/", "Source: Yahoo Finance", target = "_blank"), br(),
                    tags$a(href = "https://github.com/parmsam/simple-stock-picker/", "Github Repo", target = "_blank"),
                    br(),
                    strong("Special thanks to RitvikMath for his How I Pick Stocks video"),
                    conditionalPanel(condition = "input.smoother == true",
                                     DT::dataTableOutput("mytable"))
                  )
                )
)

# Define server function
server <- function(input, output) {
  
  # Subset data
  selected_trends <- reactive({
    getStockdata(input$type)
  })
  
  # Create scatterplot object the plotOutput function is expecting
  output$lineplot <- renderPlot({
    grid.arrange(buildStockGraph(getStockdata(input$type, weeks=input$weekcount, 
                                              start_date=input$date[1], end_date=input$date[2]), 
                                 desc=series_ticker %>% filter(ticker==input$type) %>% 
                                   select(name) %>% as.character()),
                 buildReturnGraph(getStockdata(input$type, weeks=input$weekcount, 
                                               start_date=input$date[1], end_date=input$date[2]), 
                                  desc=series_ticker %>% filter(ticker==input$type) %>% 
                                    select(name) %>% as.character())
    )
  })
  
  # Pull in description of trend
  output$desc <- renderText({
    # trend_text <- filter(trend_description, type == input$type) %>% pull(text)
    # trend_text <- "tes2020-04-01t"
    # paste(trend_text, "The index is set to 1.0 on January 1, 2004 and is calculated only for US search traffic.")
  })
  
  output$mytable = DT::renderDataTable({
    if(input$smoother){
      suppressWarnings(stock_metric_df<-series_ticker$ticker %>% map_df(criteriaCheck,weeks=input$weekcount, 
                                                       start_date=input$date[1], end_date=input$date[2]))
      return(stock_metric_df)}
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)


library(tidyverse)
library(shiny)
library(lubridate)
library(quantmod)
library(readxl)
library(zoo)
library(plotly)
library(scales)
library(gridExtra)
sdp <- function(x) sqrt(mean((x-mean(x))^2))
#r uses sample sd by default
#function to get pop sd

#declare default parameters
start_date0 = "2020-04-01"
end_date0 = today()+1
number_weeks = 4
min_avg_return = 0.1
max_dev_return = 0.07
days = 20 
#4 weeks after buy date (20 days)
#note many stock exchanges not open on weekends

series_ticker <- read.csv("https://raw.githubusercontent.com/parmsam/investing/master/series_tickers.csv")

getStockdata <- function(ticker, weeks=number_weeks, end_date=end_date0, start_date=start_date0) {
  #get stock price data
  stock_data <- getSymbols(ticker, src = "yahoo", from = start_date, 
                    to = end_date, auto.assign = FALSE) %>% 
    data.frame() %>%
    select(4)
  # stock_data %>% glimpse()
  colnames(stock_data) <- "Close"
  stock_data$Buy_date <- rownames(stock_data)
  # rownames(stock_data) <- NULL
  stock_data <- stock_data %>% select(Buy_date, Close) %>% 
    mutate(Buy_date = ymd(Buy_date))
  
  #transform to get return data
    sell_data <- stock_data %>% select(Sell_date = Buy_date, Sell_close = Close) %>% 
    mutate(Sell_date = Sell_date - weeks(weeks))
  stock_return_data <- stock_data %>% 
    inner_join(sell_data, by = c("Buy_date" = "Sell_date")) %>% 
    mutate(Pct_return = (Sell_close-Close)/Close)
  ticker <- ticker
  Sys.sleep(0.07)
  return(stock_return_data)
}

getSummaryMetrics <- function(stock_return_data) { 
  stock_metrics <- stock_return_data %>% drop_na() %>% 
  summarize(Avg_Return = round(mean(Pct_return),3), 
            Dev_Return = round(sdp(Pct_return),4)) #sd uses the sample standard deviation
  return(stock_metrics)
}

criteriaCheck <- function(ticker, min_avg_return1=min_avg_return, max_dev_return1=max_dev_return, weeks=number_weeks, 
                          end_date=end_date0, start_date=start_date0) {
  print(ticker)
  stockData <- getStockdata(ticker, weeks, end_date, start_date)
  stock_metrics <- getSummaryMetrics(stockData)
  if(!is.na(stock_metrics$Avg_Return)){ if(stock_metrics$Avg_Return > min_avg_return1 & stock_metrics$Dev_Return < max_dev_return1){
    stock_metrics<-stock_metrics %>% mutate(ticker=ticker) %>% select(ticker,everything())
    return(stock_metrics)
    }}
}

buildStockGraph <- function(stock_return_data, ticker="", desc=""){
  #graphs
  stock_metrics <- getSummaryMetrics(stock_return_data)
  stock_chart <- stock_return_data %>% ggplot() + 
    geom_line(aes(x=Buy_date, y=Close), color = "blue", size = 0.75) +
    xlab("Date") + ylab("Price ($)") + scale_x_date(date_labels = "%b %d") + 
    ggtitle(paste(ticker, desc, sep="\n")) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  return(stock_chart)
}

buildReturnGraph <- function(stock_return_data, ticker="", desc=""){
  #graphs
  stock_metrics <- getSummaryMetrics(stock_return_data)
  return_chart <- stock_return_data %>% ggplot() + 
    geom_line(aes(x=Buy_date, y=Pct_return), color = "darkgreen", size = 0.75) +
    xlab("Date") + ylab("Pct. Return") + scale_x_date(date_labels = "%b %d") + 
    ggtitle(paste("Avg Return:", 
                  percent(stock_metrics$Avg_Return, accuracy = 0.01),"|", 
                  "Dev Return:",
                  percent(stock_metrics$Dev_Return, accuracy = 0.01))) + 
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_hline(yintercept=0, linetype="dashed", color = "red")
  return(return_chart)
}

#testing functions:
# getStockdata("COST")
# getSummaryMetrics(getStockdata("COST"))
# buildGraphs(getStockdata("COST"), desc="Costco")
# criteriaCheck("YNDX")
# criteriaCheck("VG")
# 
# criteriaCheck("VIAC")
# getSummaryMetrics(getStockdata("VIAC"))
# getStockdata("VIAC")
# 
# getSummaryMetrics(getStockdata("DCT"))
# criteriaCheck("DCT")

#run functions on dataset and get relevant metrics:

# stock_metric_df <- series_ticker$ticker %>% map_df(criteriaCheck)
# stock_metric_df

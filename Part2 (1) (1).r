library(quantmod)
library(PerformanceAnalytics)
library(imputeTS)
library(PortfolioAnalytics)
library(shiny)
library(DT)
library(ggplot2)
library(TSstudio)
?plot_ly
do=c(2,4,5)

plot(as.data.frame(do))
df=as.data.frame(do)

webpage <- 'https://stockanalysis.com/stocks/'
r <- webpage %>% 
  httr::GET(config = httr::config(ssl_verifypeer = FALSE)) %>% 
  read_html() %>% 
  html_element('#__NEXT_DATA__') %>% 
  html_text() %>% 
  jsonlite::parse_json()


stocks <- map_df(r$props$pageProps$stocks,  ~ .x)%>%
  mutate(url = paste0('https://stockanalysis.com/stocks/', s))
stocks
colnames(stocks) <- c('Ticker', 'Company Name', 'Market Cap', 'Industry', 'URL')
stocks <- stocks[,1:4]
stocks
all = (stocks[,1])

getwd()

ui<-fluidPage( 
  
  selectInput("x","Enter tickers", choices = all, multiple = TRUE),
  textInput("num","Enter weights"),
  dateInput("dt", "Select a date:"),
  actionButton("GO","Go"),
  actionButton("GO2","Go Again"),
  verbatimTextOutput("txt"),
  tags$image(src="KPI.png" , height = "200px", width = "300px", alt="Something went wrong",deleteFile = FALSE),  
  plotOutput("plot1"),
  plotOutput("plot2")
  
)

server <- function(input, output){  
  portfolioPrices <- NULL
  
  data <- eventReactive(input$GO, {
    req(input$dt)
    tickers <- as.character(unlist(strsplit(input$x, ",")))
    for (ticker in tickers) {
      portfolioPrices <- cbind(portfolioPrices, getSymbols.yahoo(ticker, from = input$dt,  periodicity = "daily", auto.assign = FALSE)[,4])
    }
    #Note to self: Write Code to check benchmark prices
   # colnames(portfolioPrices) <- tickers
    #dframe<- data.frame(portfolioPrices)
    #dframe
    })
  
  #Calculate Returns For DF
  PerformanceSummary <- eventReactive(input$GO, {
  weights <- as.numeric(unlist(strsplit(input$num, ",")))
  dailyReturns <- na.omit(ROC(data(), type="discrete"))
  
  #Calculate Portfolio Returns
  portfolioReturn <- Return.portfolio(dailyReturns, weights=weights)
  
  #Plot Performance
  chart.CumReturns(portfolioReturn)
  charts.PerformanceSummary(portfolioReturn)
  })
  Stats <- eventReactive(input$GO2, {
  CAPM.beta(portfolioReturn, benchmarkReturns, .035/252)
  CAPM.beta.bull(portfolioReturn, benchmarkReturns, .035/252)
  CAPM.beta.bear(portfolioReturn, benchmarkReturns, .035/252)
  
  #CAPM.alpha(portfolioReturn, benchmarkReturns, .035/252)
  CAPM.jensenAlpha(portfolioReturn, benchmarkReturns, .035/252)
  
  SharpeRatio(portfolioReturn, Rf = .035/252, p = 0.95, FUN = "StdDev",
              weights = NULL, annualize = FALSE)
  
  table.AnnualizedReturns(portfolioReturn, Rf=.035/252, geometric=TRUE)
  })
  
  output$plot1 <- renderPlot({
  plot(data())
  })
  output$plot2 <- renderPlot({
    PerformanceSummary()
  })
 ?ggPlot 
  output$txt <- renderPrint({
    Stats()
  })
  
}

shinyApp(ui = ui, server = server)
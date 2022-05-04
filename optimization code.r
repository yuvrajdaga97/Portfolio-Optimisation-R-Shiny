library(quantmod)
library(PerformanceAnalytics)
library(imputeTS)
library(PortfolioAnalytics)
library(shiny)
library(DT)

stocks <- map_df(r$props$pageProps$stocks,  ~ .x)%>%
  mutate(url = paste0('https://stockanalysis.com/stocks/', s))
stocks
colnames(stocks) <- c('Ticker', 'Company Name', 'Market Cap', 'Industry', 'URL')
stocks <- stocks[,1:4]
stocks
all = (stocks[,1])


ui<-fluidPage( 
  selectInput("x","Enter tickers", choices = all, multiple = TRUE),
  dateInput("dt", "Select a date:"),
  actionButton("GO","GO"),
  DT::dataTableOutput("table"),
  plotOutput("myplot"),
  verbatimTextOutput("txt")
)

server <- function(input, output){  
  portfolioPrices <- NULL
  data <- eventReactive(input$GO, {
    req(input$dt)
    tickers <- as.character(unlist(strsplit(input$x, ",")))
    for (ticker in tickers) {
      portfolioPrices <- cbind(portfolioPrices, getSymbols.yahoo(ticker, from = input$dt,  periodicity = "daily", auto.assign = F)[,4])
    }
    
    portfolioReturns <- na.omit(ROC(portfolioPrices))
    portf <- portfolio.spec(colnames(portfolioReturns))
    
    portf <- add.constraint(portf, type="weight_sum", min_sum=1, max_sum=1)
    portf <- add.constraint(portf, type="box", min=.10, max=.40)
    portf <- add.objective(portf, type="return", name="mean")
    portf <- add.objective(portf, type="risk", name="StdDev")
    
    optPort <- optimize.portfolio(portfolioReturns, portf, optimize_method = "ROI", trace=TRUE)
    wt<-chart.Weights(optPort)
    
    ef <- extractEfficientFrontier(optPort, match.col = "StdDev", n.portfolios = 25,
                                   risk_aversion = NULL)
  })
  
  output$myplot <- renderPlot({
    chart.EfficientFrontier(data(),
                            match.col = "StdDev", n.portfolios = 25, xlim = NULL, ylim = NULL,
                            cex.axis = 0.8, element.color = "darkgray", main = "Efficient Frontier",
                            RAR.text = "SR", rf = 0, tangent.line = TRUE, cex.legend = 0.8,
                            chart.assets = TRUE, labels.assets = TRUE, pch.assets = 21,
                            cex.assets = 0.8)
  })
  
}

shinyApp(ui = ui, server = server)
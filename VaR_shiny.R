
library(shiny)
library(quantmod)
library(PerformanceAnalytics)
library(copula)
library(MASS) # for rmnorm

ui <- fluidPage(
  titlePanel("VaR & CVaR Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("etf", "Choose an ETF:", choices = c('Portfolio', 'QQQ', 'SPY', 'IWM', 'EFA', 'TLT')),
      radioButtons("method", "Choose VaR Calculation Method:", 
                   choices = c("Historical", "Variance-Covariance", "Monte Carlo", "T-Copula")),
      checkboxInput("cvar", "Include CVaR?", FALSE),
      dateInput("start_date", "Start Date", "2016-01-01"),
      dateInput("end_date", "End Date", "2016-12-31"),
      actionButton("submit", "Calculate")
    ),
    mainPanel(
      plotOutput("var_plot")
    )
  )
)

server <- function(input, output) {
  
  observeEvent(input$submit, {
    
    start_date <- input$start_date
    end_date <- input$end_date
    etf <- input$etf
    weights <- rep(1/5, 5) # Assuming equal weights for the portfolio
    
    if (etf == "Portfolio") {
      etfs <- c('QQQ', 'SPY', 'IWM', 'EFA', 'TLT')
      data <- NULL
      for (e in etfs) {
        etf_data <- Cl(getSymbols(e, from = start_date, to = end_date, auto.assign = FALSE))
        data <- cbind(data, etf_data)
      }
      colnames(data) <- etfs
      returns <- diff(log(data))
      returns <- returns[-1, ]  # Remove NA row
      returns <- rowSums(returns)/length(etfs)
    } else {
      data <- Cl(getSymbols(etf, from = start_date, to = end_date, auto.assign = FALSE))
      returns <- diff(log(data))
      returns <- returns[-1]  # Remove NA row
    }
    
    var <- NA
    cvar <- NA
    
    if (input$method == "Historical") {
      var <- -quantile(returns, probs = 0.05)
      if (input$cvar) {
        losses_below_var <- returns[returns <= -var]
        cvar <- -mean(losses_below_var)
      }
      
    } else if (input$method == "Variance-Covariance") {
      # ...[Your Variance-Covariance VaR Code]...
      mu <- mean(returns)
      sigma <- sd(returns)
      z <- qnorm(0.05)
      var <- -(mu + sigma * z)
      if (input$cvar) {
        alpha <- 0.05
        phi <- dnorm(qnorm(alpha))
        cvar <- -(mu + (phi/alpha) * sigma)
      }
      
    } else if (input$method == "Monte Carlo") {
      mu <- mean(returns)
      sigma <- cov(returns)
      num_simulations <- 10000
      simulated_returns <- replicate(num_simulations, sum(rmnorm(1, mu, sigma) * weights))
      var <- -quantile(simulated_returns, probs = 0.05)
      if (input$cvar) {
        losses_below_var <- simulated_returns[simulated_returns <= -var]
        cvar <- -mean(losses_below_var)
      }
      
    } else if (input$method == "T-Copula") {
      tcop <- tCopula(dim = length(weights))
      fit <- fitCopula(tcop, cor(returns), method = "ml")
      param <- fit@estimate
      simulated_data <- rCopula(1000, fit@copula)
      simulated_returns <- matrix(0, ncol = length(weights), nrow = 1000)
      for (i in 1:ncol(returns)) {
        simulated_returns[,i] <- qt(simulated_data[,i], df = param[1])
      }
      simulated_portfolio_returns <- rowSums(simulated_returns)
      var <- -quantile(simulated_portfolio_returns, probs = 0.05)
      if (input$cvar) {
        losses_below_var <- simulated_portfolio_returns[simulated_portfolio_returns <= -var]
        cvar <- -mean(losses_below_var)
      }
    }
    
    output$var_plot <- renderPlot({
      hist(returns, breaks =50, col = "lightblue", main = "Returns with VaR & CVaR", xlab = "Returns", freq = FALSE)
      abline(v = -var, col = "red", lwd = 2)
      if (input$cvar) {
        abline(v = -cvar, col = "blue", lwd = 2)
        legend("topright", c("VaR", "CVaR"), fill = c("red", "blue"))
      } else {
        legend("topright", c("VaR"), fill = c("red"))
      }
    })
    
  })
  
}

shinyApp(ui, server)

             
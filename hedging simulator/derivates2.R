#author: Briell Menezes
library(tidyquant)
library(shiny)
library(ggplot2)
library(quantmod)
library(dplyr)
library(scales)



#data import
symbols <- tidyquant::tq_index("SP500") %>%
  select(symbol, company)

#UI
ui <- fluidPage(
  
  
  #CSS styling
  tags$head(
    tags$style(HTML("
      /* Overall theme */
      body {
        background-color: #f5f7fa;
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
      }
      
      /* Title styling */
      .title-panel {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        padding: 30px;
        margin: -20px -15px 20px -15px;
        border-radius: 0 0 15px 15px;
        box-shadow: 0 4px 6px rgba(0,0,0,0.1);
      }
      
      h2.title-panel {
        margin: 0;
        font-size: 32px;
        font-weight: 600;
        text-align: center;
      }
      
      /* Sidebar styling */
      .well {
        background-color: white;
        border: none;
        border-radius: 10px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
        padding: 20px;
      }
      
      /* Section headers in sidebar */
      .well h4 {
        color: #667eea;
        font-weight: 600;
        margin-top: 15px;
        margin-bottom: 15px;
        padding-bottom: 10px;
        border-bottom: 2px solid #e9ecef;
      }
      
      /* Main panel styling */
      .tab-content {
        background-color: white;
        padding: 25px;
        border-radius: 10px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
        margin-top: 10px;
      }
      
      /* Tab styling */
      .nav-tabs > li > a {
        color: #495057;
        font-weight: 500;
      }
      
      .nav-tabs > li.active > a {
        color: #667eea;
        border-bottom: 3px solid #667eea;
        font-weight: 600;
      }
      
      /* Button styling */
      .btn-primary {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        border: none;
        border-radius: 8px;
        padding: 10px 20px;
        font-weight: 600;
        transition: transform 0.2s;
      }
      
      .btn-primary:hover {
        transform: translateY(-2px);
        box-shadow: 0 4px 12px rgba(102, 126, 234, 0.4);
      }
      
      /* Info boxes */
      .info-box {
        background-color: #f8f9fa;
        border-left: 4px solid #667eea;
        padding: 15px;
        margin: 15px 0;
        border-radius: 5px;
      }
      
      /* Tables */
      table {
        width: 100%;
        margin-top: 15px;
      }
      
      table thead {
        background-color: #667eea;
        color: white;
      }
      
      table tbody tr:hover {
        background-color: #f8f9fa;
      }
      
      /* Metric cards */
      .metric-card {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        padding: 20px;
        border-radius: 10px;
        margin: 10px 0;
        box-shadow: 0 4px 6px rgba(0,0,0,0.1);
      }
      
      .metric-card h3 {
        margin: 0;
        font-size: 28px;
        font-weight: 700;
      }
      
      .metric-card p {
        margin: 5px 0 0 0;
        opacity: 0.9;
      }
      
      /* Verbatim output boxes */
      pre {
        background-color: #f8f9fa;
        border: 1px solid #e9ecef;
        border-radius: 8px;
        padding: 15px;
        font-family: 'Courier New', monospace;
        font-size: 13px;
        line-height: 1.6;
      }
      
      /* Section headers in main panel */
      .tab-pane h4 {
        color: #667eea;
        font-weight: 600;
        margin-top: 25px;
        margin-bottom: 15px;
        padding-bottom: 10px;
        border-bottom: 2px solid #e9ecef;
      }
      
      /* Input styling */
      .form-control, .selectize-input {
        border-radius: 8px;
        border: 1px solid #e9ecef;
        transition: border-color 0.3s;
      }
      
      .form-control:focus, .selectize-input.focus {
        border-color: #667eea;
        box-shadow: 0 0 0 0.2rem rgba(102, 126, 234, 0.25);
      }
      
      /* Alert/notification styling */
      .shiny-notification {
        border-radius: 8px;
        font-weight: 500;
      }
    "))
  ),
  
  # Title 
  div(class = "title-panel",
      h2("📊 Hedging Simulator", class = "title-panel")
  ),
 
#sidebar
  sidebarLayout(
    sidebarPanel(
      # Stock Selection Section
      div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
          h4(style = "margin-top: 0;", "📈 Stock Selection"),
          selectizeInput("ticker", "Choose a Stock",
                         choices = setNames(symbols$symbol, paste(symbols$symbol, "-", symbols$company)),
                         selected = "AAPL",
                         options = list(maxOptions = 10000)),      
          dateRangeInput("dates", "Date Range", 
                         start = Sys.Date() - 365, 
                         end = Sys.Date()),
          actionButton("get", "Load Market Data", 
                       class = "btn-primary btn-block",
                       style = "margin-top: 10px;")
      ),
      
      hr(style = "border-color: #e9ecef;"),
      
      # Position Details Section
      div(style = "background-color: #fff3cd; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
          h4(style = "margin-top: 0;", "💼 Position Details"),
          numericInput("shares", "Number of Shares",
                       value = 100, min = 1, step = 10),
          radioButtons("position_type", "Position Type:",
                       choices = c("Long (I own the stock)" = "long", 
                                   "Short (I borrowed/sold the stock)" = "short"),
                       selected = "long"),
          
          div(style = "margin-bottom: 10px;",
              tags$label("Risk Tolerance:"),
              sliderInput("risk_tolerance", NULL,
                          min = 1, max = 3, value = 2, step = 1,
                          ticks = FALSE)
          ),
          tags$div(style = "margin-top: -15px; margin-bottom: 10px; font-size: 11px;",
                   tags$span("🛡️ Low", style = "float: left; color: #28a745;"),
                   tags$span("⚖️ Medium", style = "text-align: center; display: block; color: #ffc107;"),
                   tags$span("🎲 High", style = "float: right; color: #dc3545;")
          )
      ),
      
      hr(style = "border-color: #e9ecef;"),
      
      # Options Filtering Section
      div(style = "background-color: #d1ecf1; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
          h4(style = "margin-top: 0;", "🔍 Options Filtering"),
          selectInput("expiration_filter", "Expiration Date", 
                      choices = NULL),
          sliderInput("strike_range", 
                      "Strike Range (% of current price):",
                      min = 5, max = 30, value = 15, step = 5,
                      post = "%")
      ),
      
      hr(style = "border-color: #e9ecef;"),
      
      # Hedging Strategy Section
      div(style = "background-color: #d4edda; padding: 15px; border-radius: 8px;",
          h4(style = "margin-top: 0;", "🎯 Hedging Strategy"),
          radioButtons("hedge_strategy", "Select Strategy:",
                       choices = c(
                         "No Hedge" = "none",
                         "🛡️ Protective Put (Insurance)" = "put",
                         "💰 Covered Call (Income)" = "call"
                       ),
                       selected = "none"),
          conditionalPanel(
            condition = "input.hedge_strategy == 'put'",
            selectInput("put_strike_select", "Put Strike Price:", choices = NULL)
          ),
          conditionalPanel(
            condition = "input.hedge_strategy == 'call'",
            selectInput("call_strike_select", "Call Strike Price:", choices = NULL)
          )
      )
    ),
    
    mainPanel(
      # Stock name display
      h2(textOutput("stock"), style = "color: #667eea; font-weight: 600; margin-bottom: 20px;"),
      
      tabsetPanel(
        id = "main_tabs",
        
        # Instructions Tab with Better Formatting
        tabPanel("Instructions",
                 icon = icon("info-circle"),
                 div(class = "info-box",
                     h3("Welcome to the Hedging Simulator!"),
                     p("This tool helps you understand how to protect your stock positions using options.")
                 ),
                 
                 h4("How to Use This App:"),
                 tags$ol(
                   tags$li(tags$strong("Select a Stock:"), " Choose any S&P 500 stock from the dropdown"),
                   tags$li(tags$strong("Load Data:"), " Click 'Load Market Data' to fetch current prices and options"),
                   tags$li(tags$strong("Set Your Position:"), " Enter how many shares you own and your risk tolerance"),
                   tags$li(tags$strong("Explore Options:"), " View available calls and puts in the options tabs"),
                   tags$li(tags$strong("Choose a Hedge:"), " Select a protective put or covered call strategy"),
                   tags$li(tags$strong("Analyze Results:"), " See the payoff diagram and scenarios")
                 )
                 
                 
        ),
        
        # Position Summary with Cards
        tabPanel("Position Summary",
                 icon = icon("chart-line"),
                 
                 div(class = "metric-card",
                     verbatimTextOutput("position_summary")
                 ),
                 
                 h4("💭 Risk Analysis"),
                 div(style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px;",
                     verbatimTextOutput("risk_description")
                 ),
                 
                 h4("📅 Recent Price History"),
                 tableOutput("stock_data")
        ),
        
        # Hedging Strategy Tab
        tabPanel("Hedging Strategy",
                 icon = icon("shield-alt"),
                 
                 conditionalPanel(
                   condition = "input.hedge_strategy == 'none'",
                   div(class = "info-box",
                       style = "text-align: center; padding: 40px;",
                       icon("arrow-left", style = "font-size: 48px; color: #667eea;"),
                       h3("Select a Hedging Strategy"),
                       p("Choose a protective put or covered call from the sidebar to see analysis")
                   )
                 ),
                 
                 conditionalPanel(
                   condition = "input.hedge_strategy != 'none'",
                   
                   h4("📋 Strategy Details"),
                   div(style = "background-color: #d4edda; padding: 15px; border-radius: 8px;",
                       verbatimTextOutput("hedge_summary")
                   ),
                   
                   h4("📊 Payoff Visualization"),
                   plotOutput("payoff_chart", height = "450px"),
                   
                   # Download button for chart
                   downloadButton("download_plot", "Download Chart", class = "btn-primary"),
                   
                   h4("💵 Scenario Analysis"),
                   div(style = "overflow-x: auto;",
                       tableOutput("scenario_table")
                   ),
                   
                   # Download button for scenarios - ADD COMMA BEFORE THIS!
                   downloadButton("download_scenarios", "Download Scenarios", class = "btn-primary")
                 )
        ), # <-- MAKE SURE THIS COMMA IS HERE TOO!
        
        # Calls Tab
        tabPanel("Call Options",
                 icon = icon("arrow-up"),
                 
                 div(class = "info-box",
                     tags$strong("Call Options:"), 
                     " Right to BUY stock at the strike price. ",
                     "Use for income generation (covered calls) or protecting short positions."
                 ),
                 
                 tableOutput("calls_table")
        ),
        
        # Puts Tab
        tabPanel("Put Options",
                 icon = icon("arrow-down"),
                 
                 div(class = "info-box",
                     tags$strong("Put Options:"), 
                     " Right to SELL stock at the strike price. ",
                     "Use for downside protection (protective puts) on long positions."
                 ),
                 
                 tableOutput("puts_table")
        ),
        tabPanel("About",
                 h3("Purpose"),
                 p("This app helps investors understand options hedging..."),
                 
                
                 
                 h3("Data Sources"),
                 p("Yahoo Finance API via quantmod package"),
                 
                 h3("Limitations"),
                 tags$ul(
                   tags$li("Only allows for the S&P 500"),
                   tags$li("Doesn't account for dividends"),
                   tags$li("Uses mid-price for premiums")
                 )
        )
      )
    )
  
)
)

#server
server <- function(input, output, session){
 
  #button functionality
  get_data <- eventReactive(input$get, {
    req(input$ticker, input$dates)
    
    tryCatch({
      stock_data <- tidyquant::tq_get(input$ticker, 
                                      from = input$dates[1], 
                                      to = input$dates[2])
      options_data <- quantmod::getOptionChain(input$ticker, NULL)
      
      list(
        stock = stock_data,
        options = options_data,
        current_price = tail(stock_data$adjusted, 1)
      )
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
      NULL
    })
  })
  
#display my chosen stock
  output$stock <- renderText({
    ticker <- input$ticker
    company <- symbols$company[symbols$symbol == ticker]
    if (length(company) == 0){
      paste("Ticker ", ticker, " not found.")
    } else{
      paste(ticker, " - ", company)
    }
  })

  #show stock data
  output$stock_data <- renderTable({
    req(get_data()$stock)
    tail(get_data()$stock, 10) %>%
      select(date, open, high, low, close, volume) %>%
      mutate(date = format(date, "%Y-%m-%d"),
             open = sprintf("$%.2f", open),
             high = sprintf("$%.2f", high),
             low = sprintf("$%.2f", low),
             close = sprintf("$%.2f", close),
             volume = format(volume, big.mark = ","))
  })
  
#where we're at
 output$position_summary <- renderText({
   req(get_data())
   
   current_price <- get_data()$current_price
   shares <- input$shares
   position_type <- ifelse(input$position_type == "long", 
                           "Long (You own the stock)", 
                           "Short (You owe the stock)")
   
   position_value <- current_price * shares
   
   if (input$position_type == "long") {
     profit_scenario <- "Stock price goes UP"
     loss_scenario <- "Stock price goes DOWN"
   } else {
     profit_scenario <- "Stock price goes DOWN"
     loss_scenario <- "Stock price goes UP"
   }
   
   paste0(
     "Current Stock Price: $", sprintf("%.2f", current_price), "\n",
     "Position Type: ", position_type, "\n",
     "Number of Shares: ", format(shares, big.mark = ","), "\n",
     "Position Value: $", format(round(position_value), big.mark = ","), "\n\n",
     "You profit if: ", profit_scenario, "\n",
     "You lose if: ", loss_scenario
   )
   
 })
 
 #risk explanation
 output$risk_description <- renderText({
   position <- ifelse(input$position_type == "long", "long", "short")
   
   risk_text <- switch(as.character(input$risk_tolerance),
                       "1" = paste0(
                         "LOW RISK TOLERANCE:\n",
                         "You want maximum protection against losses, even if hedging costs more.\n",
                         if (position == "long") {
                           "→ Consider buying protective puts closer to current price"
                         } else {
                           "→ Consider buying call options closer to current price"
                         }
                       ),
                       "2" = paste0(
                         "MEDIUM RISK TOLERANCE:\n",
                         "You want reasonable protection at a balanced cost.\n",
                         if (position == "long") {
                           "→ Consider protective puts slightly below current price"
                         } else {
                           "→ Consider call options slightly above current price"
                         }
                       ),
                       "3" = paste0(
                         "HIGH RISK TOLERANCE:\n",
                         "You're comfortable with risk and want minimal hedging costs.\n",
                         if (position == "long") {
                           "→ Consider cheaper puts further below current price, or skip hedging"
                         } else {
                           "→ Consider cheaper calls further above current price, or skip hedging"
                         }
                       )
   )
   
   position_explanation <- if (position == "long") {
     paste0(
       "\n\nYOUR LONG POSITION EXPLAINED:\n",
       "• You own ", input$shares, " shares\n",
       "• You make money if the stock price rises\n",
       "• You lose money if the stock price falls\n",
       "• Main risk: Price drops → Use PUT options to protect"
     )
   } else {
     paste0(
       "\n\nYOUR SHORT POSITION EXPLAINED:\n",
       "• You borrowed and sold ", input$shares, " shares\n",
       "• You make money if the stock price falls\n",
       "• You lose money if the stock price rises\n",
       "• Main risk: Price rises → Use CALL options to protect"
     )
   }
   
   paste0(risk_text, position_explanation)
 })
 
 #update hedges selections
 observe({
   req(get_data()$options, input$expiration_filter, input$hedge_strategy)
   
   # Only run if a hedge is selected
   if (input$hedge_strategy == "none") return()
   
   current_price <- get_data()$current_price
   exp_data <- get_data()$options[[input$expiration_filter]]
   
   # For protective puts
   if (input$hedge_strategy == "put") {
     # Get puts at or below current price (for protection)
     put_options <- exp_data$puts %>%
       filter(Strike <= current_price * 1.05) %>%  # Within 5% above current
       arrange(desc(Strike))  # Highest to lowest
     
     # Create nice labels: "$150 (-6.3%)"
     put_choices <- setNames(
       put_options$Strike, 
       paste0("$", sprintf("%.2f", put_options$Strike),
              " (", sprintf("%.1f%%", (put_options$Strike/current_price - 1) * 100), ")")
     )
     
     # Find default: closest to 5% below current price (good protection)
     default_strike <- put_options$Strike[which.min(abs(put_options$Strike - current_price * 0.95))]
     
     updateSelectInput(session, "put_strike_select",
                       choices = put_choices,
                       selected = default_strike)
   }
   
   # For covered calls (selling calls for income)
   if (input$hedge_strategy == "call") {
     # Get calls at or above current price
     call_options <- exp_data$calls %>%
       filter(Strike >= current_price * 0.95) %>%  # Within 5% below current
       arrange(Strike)  # Lowest to highest
     
     # Create nice labels: "$170 (+6.3%)"
     call_choices <- setNames(
       call_options$Strike,
       paste0("$", sprintf("%.2f", call_options$Strike),
              " (+", sprintf("%.1f%%", (call_options$Strike/current_price - 1) * 100), ")")
     )
     
     # Find default: closest to 5% above current price
     default_strike <- call_options$Strike[which.min(abs(call_options$Strike - current_price * 1.05))]
     
     updateSelectInput(session, "call_strike_select",
                       choices = call_choices,
                       selected = default_strike)
   }
 })
 
 calculate_payoff <- reactive({
   req(get_data(), input$hedge_strategy)
   
   current_price <- get_data()$current_price
   shares <- input$shares
   
   # Create a range of possible future stock prices (70% to 130% of current)
   price_range <- seq(current_price * 0.7, current_price * 1.3, length.out = 100)
   
   # Calculate unhedged position (just stock P&L)
   stock_pnl <- (price_range - current_price) * shares
   
   # Start with unhedged as the baseline
   hedged_pnl <- stock_pnl
   hedge_cost <- 0
   strategy_description <- "No hedge applied - full exposure to price movements"
   
   # Apply hedge based on strategy
   if (input$hedge_strategy != "none") {
     req(input$expiration_filter)
     exp_data <- get_data()$options[[input$expiration_filter]]
     
     if (input$hedge_strategy == "put") {
       # Protective Put Strategy
       req(input$put_strike_select)
       put_strike <- as.numeric(input$put_strike_select)
       
       # Find the put option premium (average of bid/ask)
       put_option <- exp_data$puts %>% filter(Strike == put_strike)
       put_premium <- mean(c(put_option$Bid, put_option$Ask), na.rm = TRUE)
       
       # Total cost to buy the put
       hedge_cost <- put_premium * shares
       
       # Put payoff at expiration: max(strike - price, 0) - premium paid
       # If stock drops below strike, put gains value
       # If stock stays above strike, put expires worthless
       put_pnl <- pmax(put_strike - price_range, 0) * shares - hedge_cost
       
       # Total hedged position = stock + put
       hedged_pnl <- stock_pnl + put_pnl
       
       strategy_description <- sprintf(
         "Protective Put: Buy %d puts at $%.2f strike for $%.2f/share premium\nTotal cost: $%.0f | Max loss protected below $%.2f",
         shares, put_strike, put_premium, hedge_cost, put_strike
       )
       
     } else if (input$hedge_strategy == "call") {
       # Covered Call Strategy
       req(input$call_strike_select)
       call_strike <- as.numeric(input$call_strike_select)
       
       # Find the call option premium
       call_option <- exp_data$calls %>% filter(Strike == call_strike)
       call_premium <- mean(c(call_option$Bid, call_option$Ask), na.rm = TRUE)
       
       # Income received from selling the call (negative cost = income!)
       hedge_cost <- -call_premium * shares
       
       # Call payoff: premium received - max(price - strike, 0)
       # If stock stays below strike, you keep the premium
       # If stock rises above strike, you must sell at strike (cap your gains)
       call_pnl <- (call_premium * shares) - pmax(price_range - call_strike, 0) * shares
       
       # Total hedged position = stock + call
       hedged_pnl <- stock_pnl + call_pnl
       
       strategy_description <- sprintf(
         "Covered Call: Sell %d calls at $%.2f strike for $%.2f/share premium\nIncome received: $%.0f | Max gain capped at $%.2f",
         shares, call_strike, call_premium, abs(hedge_cost), call_strike
       )
     }
   }
   
   # Return all the data needed for plotting and analysis
   list(
     price_range = price_range,
     unhedged = stock_pnl,
     hedged = hedged_pnl,
     cost = hedge_cost,
     description = strategy_description,
     current_price = current_price
   )
 })
 
  observe({
    req(get_data()$options)
    
    # The options data is a list where NAMES are expiration dates
    expirations <- names(get_data()$options)
    
    print(paste("Found", length(expirations), "expiration dates"))
    print(expirations)
    
    # Convert to proper format for dropdown
    updateSelectInput(session, "expiration_filter", 
                      choices = expirations,
                      selected = expirations[1])
  })
  
  filter_options <- function(option_type = "calls") {
    req(get_data()$current_price, input$expiration_filter, input$strike_range)
    
    current_price <- get_data()$current_price
    range_pct <- input$strike_range / 100
    
    lower_bound <- current_price * (1 - range_pct)
    upper_bound <- current_price * (1 + range_pct)
    
    # Get the specific expiration's data
    exp_data <- get_data()$options[[input$expiration_filter]]
    
    # Get calls or puts from that expiration
    if (option_type == "calls") {
      options_df <- exp_data$calls
    } else {
      options_df <- exp_data$puts
    }
    
    # Filter by strike range
    filtered <- options_df %>%
      filter(Strike >= lower_bound,
             Strike <= upper_bound) %>%
      arrange(Strike)
    
    return(filtered)
  }
  output$options_data <- renderTable({
    req(get_data()$options)
    
    # Get the structure - it's a list of lists
    opt_data <- get_data()$options
    
    # Check if we got data
    if (is.null(opt_data) || length(opt_data) == 0) {
      return(data.frame(Message = "No options data available"))
    }
    
    if (!is.null(opt_data$calls) && nrow(opt_data$calls) > 0) {
      calls_df <- opt_data$calls
      
      # Sort by expiration date to get nearest first
      calls_df <- calls_df[order(calls_df$Expiration), ]
      
      # Filter to nearest expiration only
      nearest_exp <- min(calls_df$Expiration)
      calls_df <- calls_df[calls_df$Expiration == nearest_exp, ]
      
      head(calls_df, 10)
    } else {
      data.frame(Message = "No calls data found")
    }
  })
 
  output$calls_table <- renderTable({
    req(get_data()$options, input$expiration_filter)
    
    # Use the new filter function
    filtered <- filter_options("calls")
    
    if (is.null(filtered) || nrow(filtered) == 0) {
      return(data.frame(Message = "No options in this strike range. Try increasing the % range."))
    }
    
    current_price <- get_data()$current_price
    
    filtered %>%
      mutate(
        `vs Current` = ifelse(Strike > current_price, 
                              sprintf("+%.1f%%", (Strike/current_price - 1) * 100),
                              sprintf("%.1f%%", (Strike/current_price - 1) * 100))
      ) %>%
      select(Strike, `vs Current`, Last, Bid, Ask, Vol, OI, IV) %>%
      mutate(
        across(c(Strike, Last, Bid, Ask), ~sprintf("$%.2f", .)),
        IV = sprintf("%.1f%%", IV * 100),
        Vol = format(Vol, big.mark = ","),
        OI = format(OI, big.mark = ",")
      )
  })

  output$puts_table <- renderTable({
    req(get_data()$options, input$expiration_filter)
    
    # Use the new filter function
    filtered <- filter_options("puts")
    
    if (is.null(filtered) || nrow(filtered) == 0) {
      return(data.frame(Message = "No options in this strike range. Try increasing the % range."))
    }
    
    current_price <- get_data()$current_price
    
    filtered %>%
      mutate(
        `vs Current` = ifelse(Strike > current_price, 
                              sprintf("+%.1f%%", (Strike/current_price - 1) * 100),
                              sprintf("%.1f%%", (Strike/current_price - 1) * 100))
      ) %>%
      select(Strike, `vs Current`, Last, Bid, Ask, Vol, OI, IV) %>%
      mutate(
        across(c(Strike, Last, Bid, Ask), ~sprintf("$%.2f", .)),
        IV = sprintf("%.1f%%", IV * 100),
        Vol = format(Vol, big.mark = ","),
        OI = format(OI, big.mark = ",")
      )
  })
  
  output$hedge_summary <- renderText({
    # Check if a hedge is selected
    if (input$hedge_strategy == "none") {
      return("No hedge selected.\n\nYour position is fully exposed to price movements.")
    }
    
    # Need options data
    req(get_data()$options, input$expiration_filter)
    
    current_price <- get_data()$current_price
    shares <- input$shares
    exp_data <- get_data()$options[[input$expiration_filter]]
    
    # PROTECTIVE PUT
    if (input$hedge_strategy == "put") {
      req(input$put_strike_select)
      
      put_strike <- as.numeric(input$put_strike_select)
      put_option <- exp_data$puts %>% filter(Strike == put_strike)
      
      # Calculate premium (average of bid/ask)
      put_premium <- mean(c(put_option$Bid, put_option$Ask), na.rm = TRUE)
      total_cost <- put_premium * shares
      
      # How much below current price is the strike?
      protection_level <- (put_strike / current_price - 1) * 100
      
      paste0(
        "PROTECTIVE PUT SELECTED\n\n",
        "Strike Price: $", sprintf("%.2f", put_strike), " (", sprintf("%.1f%%", protection_level), " from current)\n",
        "Premium per share: $", sprintf("%.2f", put_premium), "\n",
        "Total Cost: $", sprintf("%.0f", total_cost), "\n\n",
        "Protection: If stock drops below $", sprintf("%.2f", put_strike), ", your losses are limited.\n",
        "Max Loss: $", sprintf("%.0f", (put_strike - current_price) * shares - total_cost)
      )
      
      # COVERED CALL  
    } else if (input$hedge_strategy == "call") {
      req(input$call_strike_select)
      
      call_strike <- as.numeric(input$call_strike_select)
      call_option <- exp_data$calls %>% filter(Strike == call_strike)
      
      call_premium <- mean(c(call_option$Bid, call_option$Ask), na.rm = TRUE)
      total_income <- call_premium * shares
      
      # How much above current price is the strike?
      upside_cap <- (call_strike / current_price - 1) * 100
      
      paste0(
        "COVERED CALL SELECTED\n\n",
        "Strike Price: $", sprintf("%.2f", call_strike), " (+", sprintf("%.1f%%", upside_cap), " from current)\n",
        "Premium per share: $", sprintf("%.2f", call_premium), "\n",
        "Income Received: $", sprintf("%.0f", total_income), "\n\n",
        "Trade-off: You earn $", sprintf("%.0f", total_income), " now, but give up gains above $", sprintf("%.2f", call_strike), "\n",
        "Max Gain: $", sprintf("%.0f", (call_strike - current_price) * shares + total_income)
      )
    }
  })
  
  output$scenario_table <- renderTable({
    # Only show if hedge is selected
    if (input$hedge_strategy == "none") {
      return(data.frame(Message = "Select a hedge strategy to see scenarios"))
    }
    
    req(get_data()$options, input$expiration_filter)
    
    current_price <- get_data()$current_price
    shares <- input$shares
    exp_data <- get_data()$options[[input$expiration_filter]]
    
    # Create different price scenarios (stock drops 20%, 10%, stays same, rises 10%, 20%)
    price_scenarios <- c(
      current_price * 0.8,   # -20%
      current_price * 0.9,   # -10%
      current_price * 1.0,   #  0%
      current_price * 1.1,   # +10%
      current_price * 1.2    # +20%
    )
    
    scenario_labels <- c("-20%", "-10%", "No Change", "+10%", "+20%")
    
    # Calculate stock P&L (no hedge)
    stock_pnl <- (price_scenarios - current_price) * shares
    
    # PROTECTIVE PUT
    if (input$hedge_strategy == "put") {
      req(input$put_strike_select)
      
      put_strike <- as.numeric(input$put_strike_select)
      put_option <- exp_data$puts %>% filter(Strike == put_strike)
      put_premium <- mean(c(put_option$Bid, put_option$Ask), na.rm = TRUE)
      put_cost <- put_premium * shares
      
      # Put payoff = max(strike - price, 0) * shares - cost
      put_payoff <- pmax(put_strike - price_scenarios, 0) * shares - put_cost
      
      # Total = stock + put
      total_pnl <- stock_pnl + put_payoff
      
      data.frame(
        Scenario = scenario_labels,
        `Stock Price` = sprintf("$%.2f", price_scenarios),
        `Stock P&L` = sprintf("$%.0f", stock_pnl),
        `Put Payoff` = sprintf("$%.0f", put_payoff),
        `Total P&L` = sprintf("$%.0f", total_pnl),
        check.names = FALSE
      )
      
      # COVERED CALL
    } else if (input$hedge_strategy == "call") {
      req(input$call_strike_select)
      
      call_strike <- as.numeric(input$call_strike_select)
      call_option <- exp_data$calls %>% filter(Strike == call_strike)
      call_premium <- mean(c(call_option$Bid, call_option$Ask), na.rm = TRUE)
      call_income <- call_premium * shares
      
      # Call payoff = income - max(price - strike, 0) * shares
      call_payoff <- call_income - pmax(price_scenarios - call_strike, 0) * shares
      
      # Total = stock + call
      total_pnl <- stock_pnl + call_payoff
      
      data.frame(
        Scenario = scenario_labels,
        `Stock Price` = sprintf("$%.2f", price_scenarios),
        `Stock P&L` = sprintf("$%.0f", stock_pnl),
        `Call Payoff` = sprintf("$%.0f", call_payoff),
        `Total P&L` = sprintf("$%.0f", total_pnl),
        check.names = FALSE
      )
    }
  })

#update selections
observe({
  req(get_data()$options, input$expiration_filter, input$hedge_strategy)
  
  if (input$hedge_strategy == "none") return()
  
  current_price <- get_data()$current_price
  exp_data <- get_data()$options[[input$expiration_filter]]
  
  # For puts
  if (input$hedge_strategy == "put") {
    put_options <- exp_data$puts %>%
      filter(Strike <= current_price * 1.05) %>%
      arrange(desc(Strike))
    
    if (nrow(put_options) == 0) return()
    
    put_choices <- setNames(
      put_options$Strike, 
      paste0("$", sprintf("%.2f", put_options$Strike))
    )
    
    updateSelectInput(session, "put_strike_select",
                      choices = put_choices,
                      selected = put_options$Strike[1])
  }
  
  # For calls
  if (input$hedge_strategy == "call") {
    call_options <- exp_data$calls %>%
      filter(Strike >= current_price * 0.95) %>%
      arrange(Strike)
    
    if (nrow(call_options) == 0) return()
    
    call_choices <- setNames(
      call_options$Strike,
      paste0("$", sprintf("%.2f", call_options$Strike))
    )
    
    updateSelectInput(session, "call_strike_select",
                      choices = call_choices,
                      selected = call_options$Strike[1])
  }
  
  
  
})

output$payoff_chart <- renderPlot({
  # Only show if hedge is selected
  if (input$hedge_strategy == "none") {
    ggplot() + 
      annotate("text", x = 0.5, y = 0.5, 
               label = "Select a hedge strategy to see the payoff chart", 
               size = 6) +
      theme_void()
    return()
  }
  
  req(get_data()$options, input$expiration_filter)
  
  current_price <- get_data()$current_price
  shares <- input$shares
  exp_data <- get_data()$options[[input$expiration_filter]]
  
  # Create a range of future prices (70% to 130% of current)
  price_range <- seq(current_price * 0.7, current_price * 1.3, length.out = 100)
  
  # Stock P&L (unhedged)
  stock_pnl <- (price_range - current_price) * shares
  
  # Calculate hedged P&L based on strategy
  if (input$hedge_strategy == "put") {
    req(input$put_strike_select)
    
    put_strike <- as.numeric(input$put_strike_select)
    put_option <- exp_data$puts %>% filter(Strike == put_strike)
    put_premium <- mean(c(put_option$Bid, put_option$Ask), na.rm = TRUE)
    put_cost <- put_premium * shares
    
    # Put payoff
    put_payoff <- pmax(put_strike - price_range, 0) * shares - put_cost
    hedged_pnl <- stock_pnl + put_payoff
    
    chart_title <- paste0("Protective Put: $", sprintf("%.2f", put_strike), " strike")
    
  } else if (input$hedge_strategy == "call") {
    req(input$call_strike_select)
    
    call_strike <- as.numeric(input$call_strike_select)
    call_option <- exp_data$calls %>% filter(Strike == call_strike)
    call_premium <- mean(c(call_option$Bid, call_option$Ask), na.rm = TRUE)
    call_income <- call_premium * shares
    
    # Call payoff
    call_payoff <- call_income - pmax(price_range - call_strike, 0) * shares
    hedged_pnl <- stock_pnl + call_payoff
    
    chart_title <- paste0("Covered Call: $", sprintf("%.2f", call_strike), " strike")
  }
  
  # Create dataframe for ggplot
  plot_data <- data.frame(
    Price = price_range,
    Unhedged = stock_pnl,
    Hedged = hedged_pnl
  )
  
  # Create the plot
  ggplot(plot_data, aes(x = Price)) +
    # Add the lines
    geom_line(aes(y = Unhedged, color = "Unhedged (Stock Only)"), 
              linewidth = 1.2, linetype = "dashed") +
    geom_line(aes(y = Hedged, color = "Hedged (Stock + Option)"), 
              linewidth = 1.5) +
    
    # Add reference lines
    geom_hline(yintercept = 0, linetype = "dotted", color = "gray50", linewidth = 0.8) +
    geom_vline(xintercept = current_price, linetype = "dotted", color = "blue", linewidth = 0.8) +
    
    # Add current price label
    annotate("text", x = current_price, y = max(hedged_pnl) * 0.9,
             label = paste("Current\n$", sprintf("%.2f", current_price)),
             color = "blue", size = 3.5) +
    
    # Customize colors
    scale_color_manual(values = c("Unhedged (Stock Only)" = "red", 
                                  "Hedged (Stock + Option)" = "darkgreen")) +
    
    # Labels and theme
    labs(
      title = chart_title,
      x = "Stock Price at Expiration",
      y = "Profit / Loss ($)",
      color = "Strategy"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      legend.position = "top",
      legend.title = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    
    # Format axis labels as currency
    scale_x_continuous(labels = scales::dollar_format()) +
    scale_y_continuous(labels = scales::dollar_format())
})


# Download handler for payoff chart
output$download_plot <- downloadHandler(
  filename = function() {
    paste0("payoff_chart_", Sys.Date(), ".png")
  },
  content = function(file) {
    # Recreate the plot
    if (input$hedge_strategy == "none") return()
    
    req(get_data()$options, input$expiration_filter)
    
    current_price <- get_data()$current_price
    shares <- input$shares
    exp_data <- get_data()$options[[input$expiration_filter]]
    
    price_range <- seq(current_price * 0.7, current_price * 1.3, length.out = 100)
    stock_pnl <- (price_range - current_price) * shares
    
    if (input$hedge_strategy == "put") {
      req(input$put_strike_select)
      put_strike <- as.numeric(input$put_strike_select)
      put_option <- exp_data$puts %>% filter(Strike == put_strike)
      put_premium <- mean(c(put_option$Bid, put_option$Ask), na.rm = TRUE)
      put_cost <- put_premium * shares
      put_payoff <- pmax(put_strike - price_range, 0) * shares - put_cost
      hedged_pnl <- stock_pnl + put_payoff
      chart_title <- paste0("Protective Put: $", sprintf("%.2f", put_strike), " strike")
      
    } else if (input$hedge_strategy == "call") {
      req(input$call_strike_select)
      call_strike <- as.numeric(input$call_strike_select)
      call_option <- exp_data$calls %>% filter(Strike == call_strike)
      call_premium <- mean(c(call_option$Bid, call_option$Ask), na.rm = TRUE)
      call_income <- call_premium * shares
      call_payoff <- call_income - pmax(price_range - call_strike, 0) * shares
      hedged_pnl <- stock_pnl + call_payoff
      chart_title <- paste0("Covered Call: $", sprintf("%.2f", call_strike), " strike")
    }
    
    plot_data <- data.frame(
      Price = price_range,
      Unhedged = stock_pnl,
      Hedged = hedged_pnl
    )
    
    # Save the plot
    p <- ggplot(plot_data, aes(x = Price)) +
      geom_line(aes(y = Unhedged, color = "Unhedged (Stock Only)"), 
                linewidth = 1.2, linetype = "dashed") +
      geom_line(aes(y = Hedged, color = "Hedged (Stock + Option)"), 
                linewidth = 1.5) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "gray50", linewidth = 0.8) +
      geom_vline(xintercept = current_price, linetype = "dotted", color = "blue", linewidth = 0.8) +
      annotate("text", x = current_price, y = max(hedged_pnl) * 0.9,
               label = paste("Current\n$", sprintf("%.2f", current_price)),
               color = "blue", size = 3.5) +
      scale_color_manual(values = c("Unhedged (Stock Only)" = "red", 
                                    "Hedged (Stock + Option)" = "darkgreen")) +
      labs(title = chart_title, x = "Stock Price at Expiration", y = "Profit / Loss ($)", color = "Strategy") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
            legend.position = "top", legend.title = element_blank(), panel.grid.minor = element_blank()) +
      scale_x_continuous(labels = scales::dollar_format()) +
      scale_y_continuous(labels = scales::dollar_format())
    
    ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
  }
)

# Download handler for scenario table
output$download_scenarios <- downloadHandler(
  filename = function() {
    paste0("scenarios_", input$ticker, "_", Sys.Date(), ".csv")
  },
  content = function(file) {
    if (input$hedge_strategy == "none") return()
    
    req(get_data()$options, input$expiration_filter)
    
    current_price <- get_data()$current_price
    shares <- input$shares
    exp_data <- get_data()$options[[input$expiration_filter]]
    
    price_scenarios <- c(
      current_price * 0.8,
      current_price * 0.9,
      current_price * 1.0,
      current_price * 1.1,
      current_price * 1.2
    )
    
    scenario_labels <- c("-20%", "-10%", "No Change", "+10%", "+20%")
    stock_pnl <- (price_scenarios - current_price) * shares
    
    if (input$hedge_strategy == "put") {
      req(input$put_strike_select)
      put_strike <- as.numeric(input$put_strike_select)
      put_option <- exp_data$puts %>% filter(Strike == put_strike)
      put_premium <- mean(c(put_option$Bid, put_option$Ask), na.rm = TRUE)
      put_cost <- put_premium * shares
      put_payoff <- pmax(put_strike - price_scenarios, 0) * shares - put_cost
      total_pnl <- stock_pnl + put_payoff
      
      scenario_data <- data.frame(
        Scenario = scenario_labels,
        Stock_Price = price_scenarios,
        Stock_PnL = stock_pnl,
        Put_Payoff = put_payoff,
        Total_PnL = total_pnl
      )
      
    } else if (input$hedge_strategy == "call") {
      req(input$call_strike_select)
      call_strike <- as.numeric(input$call_strike_select)
      call_option <- exp_data$calls %>% filter(Strike == call_strike)
      call_premium <- mean(c(call_option$Bid, call_option$Ask), na.rm = TRUE)
      call_income <- call_premium * shares
      call_payoff <- call_income - pmax(price_scenarios - call_strike, 0) * shares
      total_pnl <- stock_pnl + call_payoff
      
      scenario_data <- data.frame(
        Scenario = scenario_labels,
        Stock_Price = price_scenarios,
        Stock_PnL = stock_pnl,
        Call_Payoff = call_payoff,
        Total_PnL = total_pnl
      )
    }
    
    write.csv(scenario_data, file, row.names = FALSE)
  }
)
}

shinyApp(ui = ui, server = server)
# Load required libraries
library(quantmod)
library(TTR)

# Step 5: Function to import stock data
load_stock_data <- function(file_path) {
  # Read the portfolio.txt file
  symbols <- readLines(file_path)
  stock_data_list <- list()
  
  # Import data for each symbol using quantmod
  for (symbol in symbols) {
    data <- getSymbols(symbol, auto.assign = FALSE)
    stock_data_list[[symbol]] <- data
  }
  return(stock_data_list)
}

# Helper function to calculate mode (Base R lacks a built-in mode function)
get_mode <- function(v) {
  uniqv <- unique(na.omit(v))
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Step 6: Function to compute basic statistics
calculate_statistics <- function(stock_data) {
  # Extract the closing prices for calculations
  close_prices <- Cl(stock_data) 
  
  # Calculate required statistics
  stats <- list(
    Mean = mean(close_prices, na.rm = TRUE),
    Median = median(close_prices, na.rm = TRUE),
    Mode = get_mode(as.numeric(close_prices)),
    Standard_Deviation = sd(close_prices, na.rm = TRUE),
    Moving_Average_Last5 = tail(SMA(close_prices, n = 14), 5) # 14-day Simple Moving Average
  )
  return(stats)
}

# Step 7: Function to display loaded data and statistics
display_output <- function(stock_list) {
  for (symbol in names(stock_list)) {
    cat("\n========================================\n")
    cat("Stock Symbol:", symbol, "\n")
    cat("========================================\n")
    
    # Display the loaded data frame (showing the first 6 rows for brevity)
    cat("--- Imported Data (Top Rows) ---\n")
    print(head(stock_list[[symbol]]))
    
    # Calculate and display the statistics
    cat("\n--- Calculated Statistics ---\n")
    stats <- calculate_statistics(stock_list[[symbol]])
    print(stats)
  }
}

# --- Execution ---
# Ensure 'portfolio.txt' is in your working directory before running
my_portfolio <- load_stock_data("portfolio.txt")
display_output(my_portfolio)
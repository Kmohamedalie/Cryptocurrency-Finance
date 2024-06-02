## **Crypto price 🪙 visualization 📈 using historical [CoinMarketCap](https://coinmarketcap.com/) data**
# Analyzing [Bitcoin](https://coinmarketcap.com/currencies/bitcoin/) and [Ether](https://coinmarketcap.com/currencies/ethereum/) prices using R programming.
# Import all necessary library


install.packages("plyr")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("reshape2")

#required libraries for the stacked area chart
library("ggplot2") # visualization
library("dplyr") # data manipulation
library("plyr")
library("reshape2") # data transformation. For instance from wide to long format

# Importing the data

# btc historical prices
BTC_price = read.csv("BTC_All_graph_coinmarketcap.csv", sep=";")
# eth historical prices
ETH_price = read.csv("ETH_All_graph_coinmarketcap.csv", sep=";")

# converting the columns to timestamp and drop the original to avoid duplicate
# BTC
BTC_price$date  <- as.Date(BTC_price$timestamp,
                                   format = "%Y-%m-%d")
BTC_price  <- BTC_price[, -8]


# ETH
ETH_price$date  <- as.Date(ETH_price$timestamp,
                                   format = "%Y-%m-%d")
ETH_price  <- ETH_price[, -8]

# Explore the first 5 rows of the dataset

head(BTC_price)

head(ETH_price)


### Bitcoin summary statistics and viz
# summary statistics of the BTC's closing prices
summary(BTC_price[['close']])

# line chart for BITCOIN 🪙

# set size
options(repr.plot.width = 20, repr.plot.height =8)

# plot the price
ggplot(BTC_price, aes(x = date, y = close)) +
  geom_line(size = 1,  colour = "red") +
  theme_minimal() + theme(text = element_text(size=25)) +
  ggtitle("Bitcoin (BTC) price [2013 - 2024]") + ylab("Closing price [USD]") + xlab("Date")

### ETHER summary statistics and viz

# summary statistics of the ETH's closing prices
summary(ETH_price['close'])

# line chart for ETHER 🪙

# set size
options(repr.plot.width = 20, repr.plot.height =8)

# plot the price
ggplot(ETH_price, aes(x = date, y = close)) +
  geom_line(size = 1,  colour = "blue") +
  theme_minimal() + theme(text = element_text(size=25)) +
  ggtitle("ETHER (ETH) price [2015 - 2024]") + ylab("Closing price [USD]") + xlab("Date")



### Calculate simple returns for Bitcoin

# formula to calculate the rate of return
BTC_prices <- BTC_price[, "close", drop = FALSE]

# Denote n the number of time periods:
n <- nrow(BTC_prices)

BTC_simple_return <- ((BTC_prices[2:n, 1] - BTC_prices[1:(n-1), 1])/BTC_prices[1:(n-1), 1])

# print the returns
BTC_simple_return[1:10]

# summary statistics on the rate of returns
summary(BTC_simple_return)

# create columns
dates = BTC_price[2:135,8]
returns = BTC_simple_return

# create a new dataframe, add dates and returns as columns
simple_return_df <- data.frame(date = dates, return = returns)
head(simple_return_df)

# visualize the rate of return for BITCOIN 🪙

# set size
options(repr.plot.width = 20, repr.plot.height =8)

# plot the price
ggplot(simple_return_df, aes(x = date, y = return)) +
  geom_line(size = 1, colour = "red") +
  theme_minimal() + theme(text = element_text(size=25)) +
  ggtitle("Simple rate of Returns BTC [2015 - 2024]") + ylab("Returns") + xlab("Date")

### Calculate simple returns for Ether

# formula to calculate the rate of return
ETH_prices <- ETH_price[, "close", drop = FALSE]

# Denote n the number of time periods:
n <- nrow(ETH_prices)

ETH_simple_return <- ((ETH_prices[2:n, 1] - ETH_prices[1:(n-1), 1])/ETH_prices[1:(n-1), 1])

# print the returns
ETH_simple_return[1:10]

# summary statistics on the rate of returns
summary(ETH_simple_return)

# create columns
dates = ETH_price[2:nrow(ETH_price),8]
returns = ETH_simple_return

# create a new dataframe, add dates and returns as columns
ETH_simple_return_df <- data.frame(date = dates, returns = returns)
head(ETH_simple_return_df)

# visualize the rate of return for ETHER 🪙

# set size
options(repr.plot.width = 20, repr.plot.height =8)

# plot the price
ggplot(ETH_simple_return_df, aes(x = date, y = returns)) +
  geom_line(size = 1, colour="#000099") +
  theme_minimal() + theme(text = element_text(size=25)) +
  ggtitle("Simple rate of Returns ETH [2015 - 2024]") + ylab("Returns") + xlab("Date")
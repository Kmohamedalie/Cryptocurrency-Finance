# -*- coding: utf-8 -*-
"""Cryptocurrency-Prices-python.ipynb

Automatically generated by Colab.

Original file is located at
    https://colab.research.google.com/drive/1Y9X8cVKlJy3OX9GG9ySiAfjnFQT-LDXT

## **Crypto price 🪙 visualization 📈 using historical [CoinMarketCap](https://coinmarketcap.com/) data**


[Bitcoin](https://coinmarketcap.com/currencies/bitcoin/) and [Ether](https://coinmarketcap.com/currencies/ethereum/) prices using historical data.

**Import all necessary library**
"""

import pandas as pd # library for data manipulation
import matplotlib.pyplot as plt # library for visualization
import seaborn as sns # library for visualization
import datetime       # Load datetime
sns.set() # this command sets the seaborn chart style as the default

"""**Importing the data**"""

# btc historical prices
BTC_price = pd.read_csv("BTC_All_graph_coinmarketcap.csv", sep=";")
# eth historical prices
ETH_price = pd.read_csv("ETH_All_graph_coinmarketcap.csv", sep=";")

# converting the columns to timestamp
BTC_price['timestamp'] = pd.to_datetime(BTC_price['timestamp'], format='%Y-%m-%d')
ETH_price['timestamp'] = pd.to_datetime(ETH_price['timestamp'], format='%Y-%m-%d')

"""**Explore the first 5 rows of the dataset**"""

BTC_price.head()

ETH_price.head()

"""### **Bitcoin summary statistics and viz**"""

# summary statistics of the BTC's closing prices
BTC_price['close'].describe()

# line chart for BITCOIN 🪙
plt.figure(figsize = (18, 8))
plt.plot(BTC_price["timestamp"], BTC_price["close"], color = "midnightblue")
plt.title("Bitcoin (BTC) price [2013 - 2024]", fontsize = 25, fontweight = "bold")
plt.ylabel("Closing price [USD]")
plt.xlabel("Date")
plt.show()

"""### **ETHER summary statistics and viz**"""

# summary statistics of the ETH's closing prices
ETH_price['close'].describe()

# line chart for ETHER 🪙
plt.figure(figsize = (18, 8))
plt.plot(ETH_price["timestamp"], ETH_price["close"], color = "crimson")
plt.title("ETHER (ETH) price [2015 - 2024]", fontsize = 25, fontweight = "bold")
plt.ylabel("Closing price [USD]")
plt.xlabel("Date")
plt.show()



"""### **Calculate simple returns for Bitcoin**"""

# formula to calculate the rate of return
BTC_price['simple_return'] = (BTC_price['close'] / BTC_price['close'].shift(1)) - 1
# show the first five rate of return
BTC_price.iloc[:,[7,4,8]].head()

# summary statistics on the rate of returns
BTC_price['simple_return'].describe()

# visualize the rate of return
sns.set_style('white')
plt.figure(figsize = (18, 8))
plt.plot(BTC_price["timestamp"], BTC_price['simple_return'], color = "blue")
plt.title(f"Simple rate of Returns BTC", fontsize = 14, fontweight = "bold")
plt.ylabel("Returns")
plt.xlabel("Date")
plt.show()

"""### **Calculate simple returns for Ether**"""

# formula to calculate the rate of return
ETH_price['simple_return'] = (ETH_price['close'] / ETH_price['close'].shift(1)) - 1
# show the first five rate of return
ETH_price.iloc[:,[7,4,8]].head()

# summary statistics on the rate of returns
ETH_price['simple_return'].describe()

# visualize the rate of return
sns.set_style('white')
plt.figure(figsize = (18, 8))
plt.plot(ETH_price["timestamp"], ETH_price['simple_return'], color = "crimson")
plt.title(f"Simple rate of Returns ETH", fontsize = 14, fontweight = "bold")
plt.ylabel("Returns")
plt.xlabel("Date")
plt.show()
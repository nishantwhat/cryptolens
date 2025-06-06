#install.packages("plotly")
install.packages("TTR")

# 🚀 Load Libraries
#library(tidyverse)
#library(zoo)
#library(VIM)
#library(corrplot)
#library(plotly)

# 📥 Load Datasets
bitcoin <- read.csv("coin_Bitcoin.csv", stringsAsFactors = FALSE)
dogecoin <- read.csv("coin_Dogecoin.csv", stringsAsFactors = FALSE)
ethereum <- read.csv("coin_Ethereum.csv", stringsAsFactors = FALSE)

#bitcoin
# 🧠 Feature Engineering
bitcoin <- bitcoin %>%
  mutate(
    PriceRange = High - Low,
    PriceChange = abs(Close - Open),
    AvgPrice = (High + Low + Open + Close) / 4,
    Date = as.Date(Date)
  )

# ✅ Handle Other Zero Volume Values
bitcoin$Volume[bitcoin$Volume == 0] <- (bitcoin$Marketcap * bitcoin$PriceRange) / bitcoin$AvgPrice

# 🗃 Export Cleaned Dataset
write.csv(bitcoin, "bitcoin_cleaned.csv", row.names = FALSE)
dataset_clean <- read.csv("bitcoin_cleaned.csv")

cleaned_data <- dataset_clean %>%
  select(Date, High, Low, Open, Close, Marketcap, Volume) %>%
  mutate(
    Date = as.Date(Date),   # This will handle both date and time and just keep the date part
    avg_price = (High + Low) / 2,
    price_range = Close - Open,
    price_change = High - Low,
    intraday_volitality = ((High - Low) / ((High + Low)/2) * 100),

  )
cleaned_data$Date <- as.Date(dataset_clean$Date)




ggplot(cleaned_data, aes(x = Date, y = Volume)) +
  geom_area(fill = 'darkorange', alpha = 0.7) +
  labs(title = "Bitcoin Trading Volume Over Time", x = "Date", y = "Volume (USD)") +
  theme_minimal()

bitcoin_subset <- cleaned_data %>%
  filter(Date >= as.Date("2019-01-01") & Date <= as.Date("2021-12-31"))

ggplot(bitcoin_subset, aes(x = Date, y = High)) +
  geom_line(color = "gold") +
  labs(title = "Bitcoin High Price (2019–2021)", x = "Date", y = "High Price") +
  theme_minimal()

ggplot(cleaned_data, aes(x = as.Date(Date), y = intraday_volitality)) +
  geom_area(color = "orange") +
  labs(title = "Daywise volitality of BTC", x = "Date", y = "Intraday Volitilty")

ggplot(cleaned_data, aes(x = Date, y = price_range)) +
  geom_line(color = "firebrick") +
  labs(title = "Daily Price Range Over Time", y = "Price Range") +
  theme_minimal()

install.packages("plotly")
library(plotly)

plot_ly(cleaned_data, type = "candlestick",
        x = ~Date,
        open = ~Open,
        high = ~High,
        low = ~Low,
        close = ~Close) %>%
  layout(title = "Bitcoin OHLC Candlestick Chart")
#bitcoin ends
#dogecoin starts
dogecoin <- dogecoin %>%
  mutate(
    PriceRange = High - Low,
    PriceChange = abs(Close - Open),
    AvgPrice = (High + Low + Open + Close) / 4,
    Date = as.Date(Date)
  )

# ✅ Handle Other Zero Volume Values
dogecoin$Volume[dogecoin$Volume == 0] <- (dogecoin$Marketcap * dogecoin$PriceRange) / dogecoin$AvgPrice

# 🗃 Export Cleaned Dataset
write.csv(dogecoin, "dogecoin_cleaned.csv", row.names = FALSE)
dataset_clean1 <- read.csv("dogecoin_cleaned.csv")

cleaned_data1 <- dataset_clean1 %>%
  select(Date, High, Low, Open, Close, Marketcap, Volume) %>%
  mutate(
    Date = as.Date(Date),   # This will handle both date and time and just keep the date part
    avg_price = (High + Low) / 2,
    price_range = Close - Open,
    price_change = High - Low,
    intraday_volitality = ((High - Low) / ((High + Low)/2) * 100),
    year = year(Date),
    month = month(Date),
    day = day(Date),
  )
dataset_clean1$Date <- as.Date(dataset_clean1$Date)
ggplot(cleaned_data1, aes(x = as.Date(Date), y = Close)) +
  geom_line(color = "blue") +
  labs(title = "Dogecoin Closing Price Over Time", x = "Date", y = "Close Price")

dogecoin_subset <- cleaned_data1 %>%
  filter(Date >= as.Date("2019-01-01") & Date <= as.Date("2021-12-31"))

ggplot(dogecoin_subset, aes(x = Date, y = High)) +
  geom_line(color = "pink") +
  labs(title = "Dogecoin Closing Price (2019–2021)", x = "Date", y = "High Price") +
  theme_minimal()

ggplot(cleaned_data1, aes(x = as.Date(Date), y = intraday_volitality)) +
  geom_line(color = "orange") +
  labs(title = "Daywise volitality of DC", x = "Date", y = "Intraday Volitilty")

ggplot(cleaned_data1, aes(x = Date, y = price_range)) +
  geom_line(color = "firebrick") +
  labs(title = "Daily Price Range Over Time", y = "Price Range") +
  theme_minimal()

install.packages("plotly")
library(plotly)

plot_ly(cleaned_data1, type = "candlestick",
        x = ~Date,
        open = ~Open,
        high = ~High,
        low = ~Low,
        close = ~Close) %>%
  layout(title = "Dogecoin OHLC Candlestick Chart")

ggplot(dataset_clean1, aes(x = Date, y = Volume)) +
  geom_area(fill = 'darkorange', alpha = 0.7) +
  labs(title = "Dogecoin Trading Volume Over Time", x = "Date", y = "Volume (USD)") +
  theme_minimal()
#dogecoin ends
#ethereum starts
ethereum <- ethereum %>%
  mutate(
    PriceRange = High - Low,
    PriceChange = abs(Close - Open),
    AvgPrice = (High + Low + Open + Close) / 4,
    Date = as.Date(Date)
  )

# ✅ Handle Other Zero Volume Values
ethereum$Volume[ethereum$Volume == 0] <- (ethereum$Marketcap * ethereum$PriceRange) / ethereum$AvgPrice

# 🗃 Export Cleaned Dataset
write.csv(ethereum, "ethereum_cleaned.csv", row.names = FALSE)
dataset_clean2 <- read.csv("ethereum_cleaned.csv")

cleaned_data2 <- dataset_clean2 %>%
  select(Date, High, Low, Open, Close, Marketcap, Volume) %>%
  mutate(
    Date = as.Date(Date),   # This will handle both date and time and just keep the date part
    avg_price = (High + Low) / 2,
    price_range = Close - Open,
    price_change = High - Low,
    intraday_volitality = ((High - Low) / ((High + Low)/2) * 100),
    year = year(Date),
    month = month(Date),
    day = day(Date),
  )
dataset_clean2$Date <- as.Date(dataset_clean2$Date)

ggplot(dataset_clean2, aes(x = Date, y = Volume)) +
  geom_area(fill = 'darkorange', alpha = 0.7) +
  labs(title = "Bitcoin Trading Volume Over Time", x = "Date", y = "Volume (USD)") +
  theme_minimal()
ggplot(cleaned_data2, aes(x = as.Date(Date), y = Close)) +
  geom_line(color = "blue") +
  labs(title = "Ethereum Closing Price Over Time", x = "Date", y = "Close Price")

ethereum_subset <- cleaned_data2 %>%
  filter(Date >= as.Date("2019-01-01") & Date <= as.Date("2021-12-31"))

ggplot(ethereum_subset, aes(x = Date, y = High)) +
  geom_line(color = "pink") +
  labs(title = "Ethereum Closing Price (2019–2021)", x = "Date", y = "High Price") +
  theme_minimal()

ggplot(cleaned_data2, aes(x = as.Date(Date), y = intraday_volitality)) +
  geom_line(color = "orange") +
  labs(title = "Daywise volitality of Ethereum", x = "Date", y = "Intraday Volitilty")

ggplot(cleaned_data2, aes(x = Date, y = price_range)) +
  geom_line(color = "firebrick") +
  labs(title = "Daily Price Range Over Time", y = "Price Range") +
  theme_minimal()

install.packages("plotly")
library(plotly)

plot_ly(cleaned_data2, type = "candlestick",
        x = ~Date,
        open = ~Open,
        high = ~High,
        low = ~Low,
        close = ~Close) %>%
  layout(title = "Ethereum OHLC Candlestick Chart")
#ethereum ends


library(TTR)

# Named list of your cleaned datasets
crypto_data <- list(
  BTC = cleaned_data,     # Bitcoin
  ETH = cleaned_data2,    # Ethereum
  DOGE = cleaned_data1    # Dogecoin
)

# Calculate 30-day SMA for each and stack into one tidy dataframe
sma_combined <- map2_dfr(
  .x = names(crypto_data),
  .y = crypto_data,
  .f = ~ .y %>%
    arrange(Date) %>%
    mutate(SMA_30 = SMA(Close, n = 30)) %>%
    select(Date, SMA_30) %>%
    mutate(Crypto = .x)
)

# Plot all SMAs on one graph
ggplot(sma_combined, aes(x = Date, y = SMA_30, color = Crypto)) +
  geom_line(size = 1) +
  labs(
    title = "30-Day Simple Moving Average (SMA) Comparison",
    subtitle = "Bitcoin vs Ethereum vs Dogecoin",
    x = "Date",
    y = "SMA Price (USD)",
    color = "Cryptocurrency"
  ) +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_color_manual(values = c("BTC" = "blue", "ETH" = "green", "DOGE" = "orange"))



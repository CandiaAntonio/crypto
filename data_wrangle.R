# Automatically reformat the code (Ctrl+Shift+A in RStudio)
# Import data----
library(readxl)

cryptodata_tbl <- read_excel(
        "cryptodata.xlsx",
        sheet     = "values",
        col_types = c(
            "date",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric",
            "numeric"
        )
    )

library(tidyverse)
library(tidyquant)
library(forcats)

cryptodata_tbl %>%
    glimpse() %>%
    summary()

# Tidying to long data
# Assets reordering as factors 
# Factors ordered by risk----

cryptodata_long_tblt <- cryptodata_tbl %>%
    gather(key = "asset", value = "prices", -date) %>% 
    mutate(asset = as_factor(asset)) %>% 
    mutate(asset = fct_relevel(asset, levels = c(
        "treasuries",
        "ig",
        "hy",
        "em",
        "commodities",
        "sp500",
        "crypto_index",
        "bitcoin",
        "ripple",
        "ethereum",
        "eos",
        "litecoin",
        "ethereum_classic",
        "zcash"))) %>% 
    mutate(asset_asfactor = as.numeric(asset)) %>% 
    group_by(asset) %>% 
    glimpse()


# Visualizing prices graph----

daily_prices_graph <- cryptodata_long_tblt %>%
    ggplot(aes(x = date, y = prices)) +
    geom_line() +
    facet_wrap( ~ asset, scales = "free") +
    theme_tq()

daily_prices_graph

# Converting Daily Prices to daily returns in the tidyquant world----

returns_daily_long_tbl <- cryptodata_long_tblt %>% 
    group_by(asset) %>% 
    mutate(returns = (log(prices) - log(lag(prices))))

# Visualizing returns graph----

returns_graph <- returns_daily_long_tbl %>% 
    ggplot(aes(x = date, y = returns)) +
    geom_line() +
    facet_wrap( ~ asset, scales = "free") +
    theme_tq()
    
returns_graph    

# Convert data to tibbletime from daily to monthly----

library(tibbletime)

data_monthly_tbl <- cryptodata_long_tblt %>% 
    as_tbl_time(index = date) %>% 
    as_period(period = "month", side = "end")

# Visualizing monthly data

monthly_prices_graph <- data_monthly_tbl %>%
    ggplot(aes(x = date, y = prices)) +
    geom_line() +
    facet_wrap( ~ asset, scales = "free") +
    theme_tq()

monthly_prices_graph


# Converting monthly prices to monthly returns----
    
returns_monthly_long_tbl <- data_monthly_tbl %>% 
    group_by(asset) %>% 
    mutate(returns = (log(prices) - log(lag(prices))))

# Visualizing monthly returns

monthly_returns_graph <- returns_monthly_long_tbl %>%
    ggplot(aes(x = date, y = returns)) +
    geom_line() +
    facet_wrap( ~ asset, scales = "free") +
    theme_tq()

monthly_returns_graph

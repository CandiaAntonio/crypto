# Automatically reformat the code (Ctrl+Shift+A in RStudio)
# Import data
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

cryptodata_tbl %>%
    glimpse() %>%
    summary()

cryptodata_long_tblt <- cryptodata_tbl %>%
    gather(key = "key", value = "value",-date)


prices_graph <- cryptodata_long_tblt %>%
    ggplot(aes(x = date, y = value)) +
    geom_line() +
    facet_wrap( ~ key, scales = "free") +
    theme_tq()

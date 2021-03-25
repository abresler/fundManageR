library(asbviz)
library(fundManageR)
df <- tv_metrics_data(regions = c("china", "america"))
df %>%
  filter(country == "UNITED STATES") %>%
  select(amount_market_cap = market_cap_basic, amount_ebitda = "ebitda", id_ticker, exchange, sector) %>%
  mutate_if(is.numeric, log10) %>%
  filter(is.finite(amount_ebitda)) %>%
  hc_xy(
    x = "amount_ebitda",
    y = "amount_market_cap",
    group = "sector",
    facet = "exchange",
    name = "id_ticker"
  )



# urls --------------------------------------------------------------------

parse_morningsstar_json <- function() {
  data_frame(nameItem = c("Gainers/Losers", 'Commodity Summary', "Primary Indicies"),
             urlJSON =
               c("http://www.morningstar.com/api/v1/market-gla/en_us/USA/126.1.MSTATS_ALL_MS/10",
                 'http://www.morningstar.com/api/v1/markets/commodity-summary',
                 "http://www.morningstar.com/api/v1/market-indexes/en_us/quote/0P00001FJG,0P00001G7B,0P00001G7J,0P00001GJH,0P00001GJK")
  )
}


# file_import -------------------------------------------------------------

parse_morningstar_csv <-
  function(url = "") {

  }

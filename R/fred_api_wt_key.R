# http://sboysel.github.io/fredr/reference/fredr_category_series.html
# fredr::fredr_set_key(key = "eb329cc8ffb84969ecbe7f2ed9ce65dd")


# categories --------------------------------------------------------------

fred_api_calls <-
  function() {
    page <-
      "https://research.stlouisfed.org/docs/api/fred/" %>% read_html()
    nodes <-
      c("ul:nth-child(25) li , ul:nth-child(22) li , ul:nth-child(19) li , ul:nth-child(16) li , ul:nth-child(13) li")

    categories <-
      page %>% html_nodes(css = nodes) %>% html_text()

    categories %>%
      map_df(function(category){

      })


  }

# dictionary --------------------------------------------------------------

parse_table_node <-
  function(table_nodes, x = 1) {
    has_series <-
      table_nodes %>%
      html_nodes('.series-meta a') %>%
      html_text() %>% length() > 0
    
    has_attributes <-
      table_nodes %>%
      html_nodes('.attributes') %>%
      html_text() %>% length() > 0
    
    has_tags <-
      table_nodes %>%
      html_nodes('.tag') %>%
      html_text() %>% length() > 0
    
    if (has_attributes) {
      attribute <-
        table_nodes %>%
        html_nodes('.attributes') %>%
        html_text() %>%
        str_to_upper()
      
      df_att <-
        data_frame(countItemPage = x, nameAttribute = attribute)
    } else {
      df_att <-
        data_frame(countItemPage = x)
    }
    
    if (has_series) {
      periods <-
        table_nodes %>%
        html_nodes('.series-meta a') %>%
        html_text() %>%
        str_replace_all('\n', '') %>%
        str_trim() %>%
        gsub("\\s+", " ", .)
      
      series_ids <-
        table_nodes %>%
        html_nodes('.series-meta a') %>%
        strip_series()
      
      dates <-
        table_nodes %>%
        html_nodes('.series-meta-dates') %>% html_text() %>%
        str_replace_all('\n', '') %>%
        str_trim() %>%
        gsub("\\s+", " ", .)
      
      df_series <-
        data_frame(
          countItemPage = x,
          nameItem = periods,
          idSeries = series_ids,
          dateUpdated = dates
        ) %>%
        mutate(countItem = 1:n())
      
      df_series <-
        df_series %>%
        nest(-countItemPage, .key = dataSeries)
    } else {
      df_series <-
        data_frame(countItemPage = x)
    }
    
    if (has_tags) {
      tags <-
        table_nodes %>%
        html_nodes('.tag') %>%
        html_text() %>%
        str_replace_all('\n', '') %>%
        str_trim() %>%
        gsub("\\s+", " ", .)
      
      df_tags <-
        data_frame(countItemPage = x, nameTag = tags) %>%
        mutate(countItem = 1:n())
      
      df_tags <-
        df_tags %>%
        nest(-countItemPage, .key = dataTags)
    } else {
      df_tags <-
        data_frame(countItemPage = x)
    }
    df <-
      df_att %>%
      left_join(df_series) %>%
      left_join(df_tags) %>%
      suppressMessages()
    
    return(df)
  }

strip_series <-
  function(x) {
    x %>%
      html_attr('href') %>%
      str_replace_all('/series/', '')
  }

get_fred_page_count <- 
  function(base_url = "https://fred.stlouisfed.org/tags/series?t=&et=&pageID=1") {
    page <- 
      base_url %>% 
      read_html()
    
    last_page <- 
      page %>% 
      html_nodes('a:nth-child(8)') %>% 
      html_text() %>% 
      readr::parse_number()
    
    fred_pages <-
      seq(1, last_page)
    
    urls <- 
      list("https://fred.stlouisfed.org/tags/series?t=&et=&pageID=", fred_pages, '&t=') %>% 
      purrr::reduce(paste0)
    
    data_frame(numberPage = fred_pages, urlPage = urls)
  }

parse_fred_page <-
  function(url = "https://fred.stlouisfed.org/tags/series?t=&et=&pageID=1",
         return_message = TRUE) {
  page <-
    url %>%
    read_html()
  
  no_page <-
    url %>%
    str_split('&pageID=') %>%
    flatten_chr() %>%
    .[[2]] %>%
    readr::parse_number()
  
  item_count <-
    page %>%
    html_nodes('.series-title') %>%
    length()
  
  df_css_nodes <-
    data_frame(idRow = 1:item_count,
               nodeNumber = c(2, seq(
                 5, by = 3, length.out = item_count-1
               )))
  df_items <-
    1:item_count %>%
    map_df(function(x) {
      table_no <-
        df_css_nodes %>%
        filter(idRow == x) %>%
        .$nodeNumber
      
      table_node_css <-
        list('.series-pager-attr:nth-child(', table_no, ') td') %>%
        purrr::reduce(paste0)
      
      page_no <-
        x
      
      table_nodes <-
        page %>%
        html_nodes(css = table_node_css)
      
      df_table <-
        table_nodes %>%
        parse_table_node(x = page_no)
      
      link_nodes <-
        page %>%
        html_nodes('.series-title') %>%
        .[[x]]
      
      series_id <-
        link_nodes %>%
        strip_series()
      
      series_name <-
        link_nodes %>%
        html_text() %>%
        str_to_upper()
      
      table_df <-
        data_frame(
          countItemPage = x,
          idSeries = series_id,
          nameSeries = series_name
        ) %>%
        mutate(
          urlFREDAPI = list(
            "https://fred.stlouisfed.org/graph/graph-data.php?id=",
            idSeries
          ) %>% purrr::reduce(paste0)
        ) %>%
        left_join(df_table) %>%
        suppressMessages()
      return(table_df)
    })
  
  df_items <-
    df_items %>%
    mutate(urlPage = url,
           idPage = no_page) %>%
    select(idPage, everything())
  
  if (return_message) {
    list("Parsed: ", url) %>%
      purrr::invoke(paste0, .) %>% message()
    
  }
  
  return(df_items)
  
}

get_data_all_fred_series_ids <- 
  function(return_message = TRUE,
           nest_data = TRUE, 
           sleep = NULL) {
    
    parse_fred_page_safe <-
      purrr::possibly(parse_fred_page, data_frame())
    
    urls <- 
      get_fred_page_count() %>% 
      .$urlPage
    
    df_fred_ids <-
      urls %>%
      map_df(function(x) {
        parse_fred_page_safe(url = x, return_message = return_message)
      })
    
  }

# api ---------------------------------------------------------------------


get_data_fred_index_symbol_time_series <-
  function(symbol = 'DGS2',
           convert_date_time = TRUE,
           return_wide = FALSE,
           return_message = TRUE) {
    url_json <-
      list("https://fred.stlouisfed.org/graph/graph-data.php?id=",
           symbol) %>%
      purrr::reduce(paste0)

    json_data <-
      url_json %>%
      jsonlite::fromJSON(simplifyDataFrame = TRUE)

    series_name <-
      json_data$seriess$title %>%
      stringr::str_to_upper()

    frequency_data <-
      json_data$frequency %>% stringr::str_to_upper()

    series_type <-
      json_data$seriess$series$a$units %>%
      str_to_upper()

    is_percent <-
      series_type %>% stringr::str_detect('PERCENT')

    df_data <-
      json_data$series$obs[[1]] %>%
      as_data_frame() %>%
      purrr::set_names(c('datetimeData', 'value')) %>%
      mutate(
        idSymbol = symbol,
        nameSeries = series_name,
        typeValue = series_type,
        frequencyData = frequency_data,
        urlData = url_json
      ) %>%
      select(idSymbol, nameSeries, everything()) %>%
      mutate(
        datetimeData = as.POSIXct(datetimeData / 1000, origin = "1970-01-01", tz = "UTC"),
        dateData = datetimeData %>% as.Date()
      ) %>%
      select(-datetimeData) %>%
      select(dateData, everything())

    date_updated <-
      df_data$dateData %>% max()

    df_data <-
      df_data %>%
      mutate(dateUpdated = date_updated)

    if (convert_date_time) {
      df_data <-
        df_data %>%
        select(-datetimeData)
    } else {
      df_data <-
        df_data %>%
        select(-dateData)
    }

    if (is_percent) {
      df_data <-
        df_data %>%
        mutate(value = value / 100)
    }
    if (return_message) {
      list("Parsed: ", url_json) %>%
        purrr::invoke(paste0, .) %>%
        message()
    }
    return(df_data)
  }

#'  Federal Reserve Economic Data time series data_frame
#'
#'
#' This function returns a data for specified series from
#'  \href{https://fred.stlouisfed.org/}{FRED}.
#'
#' @param convert_date_time converts date from datetime to date form
#' @param symbols fred symbols to search, see \href{https://fred.stlouisfed.org/tags/series}{FRED symbols} for options
#' @param return_wide \code{TRUE} return data in wide form
#'
#' @return a \code{data_frame}
#' @export
#' @import dplyr lubridate tidyr purrr jsonlite
#' @family index values
#' @examples
#' get_data_fred_symbols(symbol = c('DGS30','DGS10','DGS2'),
#' return_wide = TRUE, nest_data = FALSE)
#'
#' get_data_fred_symbols(
#' symbols = c("CPIAUCSL", "A191RL1Q225SBEA", "UNRATE"),
#' return_wide = FALSE,
#' convert_date_time = TRUE,
#' nest_data = TRUE
#' )
#'
get_data_fred_symbols <-
  function(symbols = c('DGS10', 'DGS2'),
           convert_date_time = TRUE,
           return_wide = FALSE,
           nest_data = TRUE,
           return_message = TRUE)  {
    get_data_fred_index_symbol_time_series_safe <-
      purrr::possibly(get_data_fred_index_symbol_time_series, data_frame())

    all_data <-
      symbols %>%
      purrr::map_df(function(x){
        get_data_fred_index_symbol_time_series(symbol = x, return_wide = FALSE)
      })

    if (return_wide) {
      all_data <-
        all_data %>%
        select(-c(nameSeries, typeValue, urlData, frequencyData, dateUpdated)) %>%
        spread(idSymbol, value)
    }

    if (nest_data) {
      all_data <-
        all_data %>%
        nest(-c(nameSeries, typeValue, frequencyData, dateUpdated), .key = dataFRED)
    }
    return(all_data)
  }

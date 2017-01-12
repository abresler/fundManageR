#' Get CRSP url dictionary data frame
#'
#' @return
#' @export
#' @import rvest dplyr tidyr stringr lubridate readr
#' @examples
get_data_crsp_index_df <-
  function() {
  page <-
    "http://www.crsp.com/indexes-pages/returns-and-constituents" %>%
    read_html()

  file_names <-
    page %>%
    html_nodes('#const-list a') %>%
    html_text() %>%
    str_replace_all(' >>', '') %>%
    str_trim() %>%
    str_to_upper()

  slugs <-
    page %>%
    html_nodes('#const-list a') %>%
    html_attr('href')

  urls <-
    slugs %>%
    map_chr(function(x){
      has_http <-
        x %>% str_detect('http')

      if (!has_http) {
        url <-
          x %>% substr(3, nchar(.)) %>%
          paste0('http://www.crsp.com',.)
        return(url)
      }
      x
    })

  crsp_urls <-
    data_frame(nameFile = file_names, urlData = urls) %>%
    filter(urlData %>% str_detect(".xls")) %>%
    mutate(idRow = 1:n()) %>%
    mutate(isSummaryFile = ifelse(idRow < 4, TRUE, FALSE)) %>%
    select(-idRow)

  return(crsp_urls)

  }

parse_crsp_index_url <-
  function(url = "http://www.crsp.com/files/CRSP-US-Total-Market.xlsx", return_message = TRUE) {
    index_name <-
      url %>%
      str_replace_all('http://www.crsp.com/files/|CRSP-|.xlsx|US-','') %>%
      str_replace_all('\\-|\\_', ' ') %>%
      str_replace('\\ 0', '') %>%
      str_to_upper()

    data <-
      url %>%
      rio::import() %>%
      suppressWarnings()

    is_index <-
      data %>% ncol() == 3
    if (is_index) {
    data <-
        data %>%
        as_data_frame()
    period_data <-
      data %>% select(3) %>% names() %>%
      str_split('\\ ') %>%
      flatten_chr() %>%
      .[[1]] %>%
      lubridate::ymd()

    data <-
      data %>%
      purrr::set_names(c('idTicker', 'nameCompany', 'pctWeight')) %>%
      mutate(dateData = period_data,
             urlData = url,
             nameIndex = index_name,
             countryIndex = 'US') %>%
      select(dateData, countryIndex, nameIndex, everything())
    }

    data <-
      data[,!names(data) == ''] %>%
      as_data_frame()

    if ('Date' %in% names(data)) {
      data <-
        data %>%
        dplyr::rename(periodData = Date) %>%
        mutate(periodData = periodData %>% readr::parse_number()) %>%
        filter(!periodData %>% is.na()) %>%
        mutate(periodData = periodData %>% lubridate::ymd()) %>%
        suppressWarnings()

      data <-
        data %>%
        gather(nameIndex, pctReturns, -periodData, na.rm = TRUE) %>%
        arrange(periodData) %>%
        mutate(nameIndex = nameIndex %>% str_to_upper(),
               urlData = url,
               nameFile = index_name) %>%
        select(nameFile, urlData, periodData, everything())
    }

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>%
        message()
    }
    return(data)
  }

#' Get CRSP constituency data by index
#'
#' @param nest_data return nested data frame \code{TRUE, FALSE}
#' @param return_message return a message after data is parsed \code{TRUE, FALSE}
#' @import rvest dplyr tidyr stringr lubridate readr
#' @importFrom rio import
#' @return
#' @export
#'
#' @examples
#' get_data_crsp_indicies_constituents(nest_data = FALSE, return_message = TRUE)
get_data_crsp_indicies_constituents <-
  function(nest_data = TRUE, return_message = TRUE) {
    url_df <-
      get_data_crsp_index_df() %>%
      filter(isSummaryFile == FALSE)
    parse_crsp_index_url_safe <-
      purrr::possibly(parse_crsp_index_url, data_frame())
    all_data <-
      url_df$urlData %>%
      map_df(function(x){
        parse_crsp_index_url(url = x, return_message = return_message)
      })

    if (nest_data) {
      all_data %>%
        tidyr::nest(.key = dataTable, -c(dateData, countryIndex, nameIndex))
    }
    return(all_data)
  }

#' Get CRSP index return by month
#' @param return_wide return data in wide form \code{TRUE, FALSE}
#' @param nest_data return nested data frame \code{TRUE, FALSE}
#' @param return_message return a message after data is parsed \code{TRUE, FALSE}
#'
#' @return
#' @export
#' @import rvest dplyr tidyr stringr lubridate readr
#' @importFrom rio import
#' @examples
#' get_data_crsp_indicies_returns(return_wide = FALSE, nest_data = FALSE, return_message = TRUE)
get_data_crsp_indicies_returns <-
  function(return_wide = FALSE,
           nest_data = TRUE, return_message = TRUE) {
    url_df <-
      get_data_crsp_index_df() %>%
      filter(isSummaryFile == TRUE)
    parse_crsp_index_url_safe <-
      purrr::possibly(parse_crsp_index_url, data_frame())
    all_data <-
      url_df$urlData %>%
      map_df(function(x){
        parse_crsp_index_url(url = x, return_message = return_message)
      }) %>%
      select(-nameFile) %>%
      select(periodData, nameIndex, pctReturns, everything())

    if (return_wide) {
      all_data <-
        all_data %>%
        mutate(nameIndex = 'index' %>% paste0(nameIndex %>% str_to_title() %>% str_replace_all('\\ ', '') %>% str_replace_all('\\&','and'))) %>%
        spread(nameIndex, pctReturns)
    }

    if (nest_data) {
      all_data <-
        all_data %>%
        tidyr::nest(.key = dataTable,  -periodData)
    }

    return(all_data)
  }

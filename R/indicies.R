

# wiki --------------------------------------------------------------------
#' S&P 500 constituents
#'
#' This function returns the constituents
#' in the S&P 500.
#'
#' @param return_message \code{TRUE} return a message after data import
#'
#' @return a \code{data_frame}
#' @export
#' @family index constituents
#' @import dplyr purrr tidyr stringr formattable rvest
#' @examples
#' get_data_sp500_constituents(return_message = TRUE)
get_data_sp500_constituents <-
  function(return_message = TRUE) {
    page <-
      "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies" %>%
      read_html()

    df <-
      page %>%
      html_table(fill = TRUE) %>%
      .[[1]] %>%
      data.frame(stringsAsFactors = FALSE) %>%
      as_data_frame() %>%
      select(-3) %>%
      mutate_all(str_to_upper) %>%
      purrr::set_names(
        c(
          'idTicker',
          'nameCompany',
          'nameSectorGICS',
          'nameIndustryGICS',
          'locationHeadquarters' ,
          'dateAdded',
          'idCIK'
        )
      ) %>%
      mutate(dateAdded = dateAdded %>% lubridate::ymd(),
             nameIndex = "S&P 500")

    ticker <-
      page %>%
      html_nodes('td:nth-child(1) .text') %>%
      html_attr('href')

    wiki <-
      page %>%
      html_nodes('td:nth-child(2) a') %>%
      html_attr('href') %>%
      .[1:nrow(df)] %>%
      paste0('https://en.wikipedia.org', .)

    df <-
      df %>%
      mutate(urlCompanyQuote = ticker,
             urlCompanyWikipedia = wiki)

    if (return_message) {
      list("Returned S&P 500 constituents as of ", Sys.Date()) %>%
        purrr::invoke(paste0, .) %>%
        message()
    }
    closeAllConnections()
    return(df)
  }

# https://en.wikipedia.org/wiki/Dow_Jones_Global_Titans_50

# https://en.wikipedia.org/wiki/S%26P_100#Components



# msci --------------------------------------------------------------------

#' MSCI indicies
#'
#' Returns all MSCI indicies.  This function
#' can be used to find indicies to search with
#' \code{\link{get_data_msci_indicies_constituents}}
#' to specify indicies to extract constituents.
#'
#' @return \code{data_frame}
#' @export
#' @family MSCI
#' @import dplyr purrr formattable tidyr stringr jsonlite
#' @examples
#' get_data_msci_indicies()
get_data_msci_indicies <-
  function() {
    index_name_df <-
      "https://www.msci.com/c/portal/layout?p_l_id=1317535&p_p_cacheability=cacheLevelPage&p_p_id=indexconstituents_WAR_indexconstituents_INSTANCE_nXWh5mC97ig8&p_p_lifecycle=2&p_p_resource_id=" %>%
      fromJSON(simplifyDataFrame = TRUE) %>%
      .$indices %>%
      as_data_frame() %>%
      purrr::set_names(c(
        'dateIndexAsOf',
        'idTypeRebalance',
        'idIndex',
        'nameIndex',
        'hasDisclaimer'
      )) %>%
      mutate(
        nameIndex = nameIndex %>% str_to_upper(),
        dateIndexAsOf = dateIndexAsOf %>% lubridate::ymd(),
        urlIndexConstituents = 'https://www.msci.com/c/portal/layout?p_l_id=1317535&p_p_cacheability=cacheLevelPage&p_p_id=indexconstituents_WAR_indexconstituents_INSTANCE_nXWh5mC97ig8&p_p_lifecycle=2&p_p_resource_id=' %>% paste0(idIndex)
      ) %>%
      group_by(nameIndex) %>%
      filter(idIndex == min(idIndex)) %>%
      ungroup()
    closeAllConnections()
    return(index_name_df)

  }

parse_msci_json_constituent_url <- function(url = "https://www.msci.com/c/portal/layout?p_l_id=1317535&p_p_cacheability=cacheLevelPage&p_p_id=indexconstituents_WAR_indexconstituents_INSTANCE_nXWh5mC97ig8&p_p_lifecycle=2&p_p_resource_id=701268",
                                            return_message = TRUE) {
  df <-
    data_frame()
  success <- function(res) {
    data <-
      res$url %>%
      fromJSON(simplifyDataFrame = TRUE) %>%
      .$constituents %>%
      as_data_frame() %>%
      purrr::set_names(c('pctWeight', 'nameCompany')) %>%
      mutate_all(str_to_upper) %>%
      select(nameCompany, pctWeight) %>%
      mutate(pctWeight = (pctWeight %>% as.numeric() / 100) %>% formattable::percent(digits = 5)) %>%
      mutate(urlIndexConstituents = res$url)

    if (return_message) {
      index_id <-
        res$url %>% str_split('p_p_resource_id=') %>%
        flatten_chr() %>%
        .[[2]] %>%
        as.numeric()
      list("Parsed Index ID: ", index_id) %>%
        purrr::invoke(paste0, .) %>%
        message()
    }

    closeAllConnections()

    df <<-
      df %>%
      bind_rows(data)
  }
  failure <- function(msg) {
    data_frame()
  }
  url %>%
    walk(function(x) {
      curl_fetch_multi(url = x, success, failure)
    })
  multi_run()
  df
}

#' MSCI Indicies Constituents
#'
#' This function returns weights and constituents for a specified
#' MSCI index.
#' @param indicies vector of indicies \itemize{
#' \item \code{NULL}: returns all indicies (default)
#' \item \code{index}: index name
#' }
#' @param return_message \code{TRUE} return a message after data import
#' @param nest_data \code{TRUE} return nested data frame
#' @references \href{http://msci.com}{MSCI Inc}
#' @return nested \code{data_frame} or \code{data_frame} if \code{nest_data = FALSE}
#' @family MSCI
#' @family index constituents
#' @export
#' @import dplyr purrr formattable tidyr stringr jsonlite curl
#' @examples
#' library(stringr)
#' df_msci_indicies <-
#' get_data_msci_indicies()
#' ## Get all technology indicies and their constituents
#' tech_indicies <-
#' df_msci_indicies %>%
#' filter(nameIndex %>% str_detect('TECH')) %>%
#' .$nameIndex
#' get_data_msci_indicies_constituents(indicies = tech_indicies, nest_data = TRUE, return_message  = TRUE)

#' get_data_msci_indicies_constituents(indicies = "NORTH AMERICA", nest_data = FALSE, return_message = TRUE)#'
#' get_data_msci_indicies_constituents(indicies = NULL, nest_data = TRUE, return_message = TRUE)
get_data_msci_indicies_constituents <-
  function(indicies = NULL,
           nest_data = TRUE,
           return_message = TRUE) {
    index_df <-
      get_data_msci_indicies()

    if (!indicies %>% is_null()) {
      index_options <-
        index_df$nameIndex
      if (indicies %in% index_options %>% sum(na.rm = T) == 0) {
        stop(list(
          "Indexes can only be:\n",
          paste(index_options, collapse = "\n")
        ) %>%
          purrr::invoke(paste0, .))
      }

      index_df <-
        index_df %>%
        filter(nameIndex %>%  str_detect(indicies %>% paste0(collapse = "|")))

    }

    parse_msci_json_constituent_url_safe <-
      purrr::possibly(parse_msci_json_constituent_url2, data_frame())

    const_df <-
      index_df$urlIndexConstituents %>%
      map_df(function(x) {
        parse_msci_json_constituent_url(url = x, return_message = return_message)
      })

    const_df <-
      const_df %>%
      left_join(index_df) %>%
      suppressMessages() %>%
      group_by(idIndex) %>%
      mutate(rankCompanyIndex = 1:n()) %>%
      ungroup() %>%
      select(
        idIndex,
        nameIndex,
        dateIndexAsOf,
        idTypeRebalance,
        rankCompanyIndex,
        nameCompany,
        pctWeight,
        everything()
      ) %>%
      mutate(pctWeight = pctWeight %>% formattable::percent(digits = 7))

    if (nest_data) {
      const_df <-
        const_df %>%
        nest(-c(idIndex,
                nameIndex,
                dateIndexAsOf,
                idTypeRebalance),
             .key = dataIndexMSCI)
    }

    if (return_message) {
      list(
        "Acquired index constituents for ",
        const_df$nameIndex %>% unique() %>% length(),
        ' MSCI indicies'
      ) %>%
        purrr::invoke(paste0, .) %>%
        message()
    }
    closeAllConnections()
    return(const_df)
  }

#' MSCI indicies values
#'
#' This function returns the most recent
#' index values for the
#' 41 primary MSCI indicies on a 15 minute time delay.
#'
#' @param return_message \code{TRUE} return a message after data import
#' @param return_wide \code{TRUE} return data in wide form
#' @references \href{http://msci.com}{MSCI Inc}
#' @return \code{data_frame}
#' @export
#' @family MSCI
#' @family index values
#' @family real-time data
#' @import jsonlite dplyr purrr stringr lubridate
#' @examples
#' get_data_msci_realtime_index_values()
get_data_msci_realtime_index_values <-
  function(return_wide = TRUE,
           return_message = TRUE) {
    raw <-
      "https://app2.msci.com/webapp/indexperf/GetDelayedRealTime" %>%
      read_lines()

    json_data <-
      raw %>% substr(18, nchar(raw) - 1) %>%
      fromJSON()
    current_year <-
      Sys.Date() %>% lubridate::year()

    index_data <-
      json_data$xmfIndices$index %>%
      as_data_frame() %>%
      mutate_all(str_to_upper) %>%
      purrr::set_names(
        c(
          "datetimeData",
          "valueIndexClosePrevious",
          "pctChange",
          "valueIndex",
          "valueChange",
          "nameIndex",
          "isTradingClosed",
          "typeCurrency",
          "idIndex"
        )
      ) %>%
      mutate(
        datetimeData = list(current_year, " ", datetimeData) %>% purrr::invoke(paste0, .) %>% ydm_hm(),
        isTradingClosed = isTradingClosed %>% as.logical()
      ) %>%
      select(datetimeData,
             nameIndex,
             idIndex,
             isTradingClosed,
             typeCurrency,
             everything())

    index_data <-
      index_data %>%
      mutate_at(index_data %>% select(matches('idIndex|value|pct')) %>% names(),
                funs(. %>% readr::parse_number())) %>%
      mutate_at(index_data %>% select(matches('value')) %>% names(),
                funs(. %>% formattable::comma())) %>%
      mutate_at(index_data %>% select(matches('pct')) %>% names(),
                funs((. / 100) %>% formattable::percent(digits = 3)))

    if (return_message) {
      list("Got MSCI Index values as of ", Sys.time()) %>%
        purrr::invoke(paste0, .) %>%
        message()
    }
    if (!return_wide) {
      index_data <-
        index_data %>%
        gather(item,
               value,
               -c(
                 datetimeData,
                 nameIndex,
                 idIndex,
                 isTradingClosed,
                 typeCurrency
               ))
    }
    closeAllConnections()
    return(index_data)
  }

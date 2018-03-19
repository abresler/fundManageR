#' CRSP file dictionary
#'
#' This function returns information about available
#' CRSP index data
#'
#' @param include_summary_file \code{TRUE} (default) or \code{FALSE} to exclude
#' @return a \code{data_frame}
#' @references \href{http://crsp.com}{The Center for Research in Security Prices}
#' @export
#' @import rvest dplyr tidyr stringr lubridate readr
#' @family CRSP
#' @examples
#' get_data_crsp_files()
get_data_crsp_files <-
  function(include_summary_files = TRUE) {
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

  if (!include_summary_files) {
    crsp_urls <-
      crsp_urls %>%
      filter(isSummaryFile == F)
  }

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

#' CRSP index constituents
#'
#' This function returns information
#' about a CRSP index's constituents
#'
#' @param return_message \code{TRUE} return a message after data import
#' @param nest_data \code{TRUE} return nested data frame
#' @references \href{http://crsp.com}{The Center for Research in Security Prices}
#' @import rvest dplyr tidyr stringr lubridate readr purrr
#' @importFrom rio import
#' @return nested \code{data_frame} or \code{data_frame} if \code{nest_data = FALSE}
#' @export
#' @family CRSP
#' @family index constituents
#'
#' @examples
#' get_data_crsp_indicies_constituents(nest_data = FALSE, return_message = TRUE)
get_data_crsp_indicies_constituents <-
  function(nest_data = TRUE, return_message = TRUE) {
    url_df <-
      get_data_crsp_files() %>%
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

#' CRSP monthly index returns
#'
#' This function returns data about
#' monthly CRSP performance by index
#'
#' @param return_wide \code{TRUE} return data in wide form
#' @param return_message \code{TRUE} return a message after data import
#' @param nest_data \code{TRUE} return nested data frame
#' @return nested \code{data_frame} or \code{data_frame} if \code{nest_data = FALSE}
#' @references \href{http://crsp.com}{The Center for Research in Security Prices}
#' @export
#' @import rvest dplyr tidyr stringr lubridate readr
#' @importFrom rio import
#' @family CRSP
#' @family index values
#' @examples
#' get_data_crsp_indicies_returns(return_wide = FALSE,
#' nest_data = FALSE, return_message = TRUE)
get_data_crsp_indicies_returns <-
  function(return_wide = FALSE,
           nest_data = TRUE, return_message = TRUE) {
    url_df <-
      get_data_crsp_files() %>%
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

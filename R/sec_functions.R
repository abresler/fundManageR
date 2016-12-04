# other sec ---------------------------------------------------------------

#' Get all data for all CIK registered entities
#'
#' @param return_message Return a message
#'
#' @return
#' @export
#' @import dplyr readr tidyr stringr
#' @examples
get_data_cik_codes <-
  function(return_message = TRUE) {
    url <- 'https://www.sec.gov/edgar/NYU/cik.coleft.c'
    cik_data <-
      url %>%
      readr::read_table(col_names = F) %>%
      suppressMessages()

    cik_data <-
      cik_data %>%
      tidyr::separate(X1,
                      into = c('nameIssuer', 'codeCIK'),
                      sep = '\\:[0][0]') %>%
      mutate(codeCIK = codeCIK %>% stringr::str_replace('\\:', ''),
             codeCIK = "00" %>% paste0(codeCIK),
             idCIK = codeCIK %>% as.numeric) %>%
      mutate(datetimeData = Sys.time()) %>%
      dplyr::select(idCIK, nameIssuer, everything())

    if (return_message) {
      "You returned " %>%
        paste0(cik_data %>% nrow,' entities with registered CIK codes') %>%
        message()
    }
    return(cik_data)
  }


get_foia_url_df <-
  function() {
    page <-
      'https://www.sec.gov/foia/docs/foia-logs.htm' %>%
      read_html()

    slugs <-
      page %>%
      html_nodes('.medium-9 a') %>%
      html_attr('href') %>%
      str_replace_all('http://www.sec.gov', '')

    urls <-
      slugs %>%
      paste0('http://www.sec.gov', .)

    url_df <-
      slugs %>%
      map_df(function(x) {
        items <-
          x %>%
          str_replace_all('/foia/logs/foia-log-fy|.csv', '') %>%
          str_split('\\-') %>%
          flatten_chr()

        if (items %>% length() == 2) {
          df <-
            data_frame(year = items[[1]] %>% as.numeric,
                       quarter = items[[2]])
        } else {
          df <-
            data_frame(year = items %>% as.numeric())
        }
        return(df)
      }) %>%
      mutate(urlFOIA = urls)

    return(url_df)
  }

parse_foia_url_df <-
  function(url = 'http://www.sec.gov/foia/logs/foia-log-fy2013.csv',
           return_message = TRUE) {
    data <-
      url %>%
      read_csv() %>%
      slice(-1) %>%
      suppressWarnings() %>%
      suppressMessages()

    if ("X10" %in% names(data)) {
      data <-
        data %>%
        select(-10) %>%
        purrr::set_names(
          c(
            'idSECRequest',
            'nameRequester',
            'nameOrganization',
            'descriptionRequest',
            'dateRequest',
            'dateReceived',
            'statusRequest',
            'dateClosed',
            'typeOutcome'
          )
        ) %>%
        filter(!nameRequester %>% is.na())
    } else {
      data <-
        data %>%
        select(-matches("^X")) %>%
        purrr::set_names(
          c(
            'idSECRequest',
            'nameRequester',
            'nameOrganization',
            'categoryOrganization',
            'descriptionRequest',
            'dateRequest',
            'dateReceived',
            'statusRequest',
            'dateClosed',
            'typeOutcome'
          )
        ) %>%
        filter(!nameRequester %>% is.na())
    }

    data <-
      data %>%
      mutate_at(.cols = data %>% select(matches("date")) %>% names,
                funs(. %>% lubridate::mdy())) %>%
      mutate_at(.cols = data %>% select(matches("^name|^description|^category")) %>% names,
                funs(. %>% str_replace('\\-','') %>% stringr::str_to_upper())) %>%
      mutate(nameOrganization = ifelse(nameOrganization == '', NA, nameOrganization),
             typeOutcome = ifelse(typeOutcome == '-', NA, typeOutcome)
             ) %>%
      tidyr::separate(nameRequester, into = c('nameLast', 'nameFirst', 'titlePerson'), sep = '\\, ') %>%
      mutate(nameRequester = ifelse(!nameFirst %>% is.na(),
                                 paste(nameFirst, nameLast),
                                 nameLast),
             nameRequester = ifelse(!titlePerson %>% is.na(),
                                    paste(nameRequester, titlePerson, sep = ', '),
                                    nameRequester),
             isClosed = ifelse(statusRequest %>% str_detect('Closed'), TRUE, FALSE),
             isGranted = ifelse(typeOutcome %>% str_detect('Granted '), TRUE, FALSE),
             isGrantedPartial = ifelse(typeOutcome %>% str_detect('Granted/Denied'), TRUE, FALSE),
             urlFOIA = url) %>%
      select(-c(nameLast, nameFirst, titlePerson)) %>%
      select(idSECRequest, nameRequester, everything()) %>%
      suppressWarnings() %>%
      suppressMessages()

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>%
        message()
    }
    return(data)
  }

#' Get SEC FOIA Requests
#'
#' @param search_years
#' @param return_message
#'
#' @return
#' @export
#' @import purrr stringr dplyr rvest
#' @importFrom lubridate mdy
#' @importFrom readr read_csv
#' @examples
get_data_sec_foia_requests <-
  function(search_years = 2006:2016, return_message = TRUE) {
    url_df <-
      get_foia_url_df()

    if (!'search_years' %>% exists()) {
      urls <-
        url_df$urlFOIA
    } else {
      urls <-
        url_df %>%
        filter(year %in% search_years) %>%
        .$urlFOIA
    }

    parse_foia_url_df_safe <-
      purrr::possibly(parse_foia_url_df, NULL)

    all_data <-
      urls %>%
      map_df(function(x) {
        parse_foia_url_df_safe(url = x, return_message = TRUE)
      }) %>%
      arrange(desc(dateRequest))

    if (return_message) {
      list(
        "You parsed ",
        all_data %>% nrow() %>% formattable::comma(digits = 0),
        " SEC FOIA Requests from ",
        url_df$year %>% unique() %>% min(),
        ' to ',
        url_df$year %>% unique() %>% max()
      ) %>%
        purrr::invoke(paste0, .) %>%
        message()
    }

    return(all_data)
  }


get_sec_moneymarket_data_urls <-
  function() {
    page <-
      "https://www.sec.gov/opa/data/opendatasets-mmfhtml.html" %>%
      read_html()
  }
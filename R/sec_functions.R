
# CIKs --------------------------------------------------------------------

#' SEC CIK registered entities
#'
#' This function returns information on every \href{https://en.wikipedia.org/wiki/Central_Index_Key}{Central Index Key}
#' registered entity.  Data about CIK filing entity can be discovered
#' using the \code{\link{sec_filer}} function inputing the CIK.
#'
#' @param return_message \code{TRUE} return a message after data import
#'
#' @return \code{data_frame}
#' @references \href{http://sec.gov}{The Securities and Exchange Commission}
#' @export
#' @import dplyr readr tidyr stringr
#' @family SEC
#' @family entity search
#' @family fund data
#' @examples
#' sec_ciks(return_message = TRUE)
sec_ciks <-
  function(return_message = TRUE) {
    url <-
      'https://www.sec.gov/edgar/NYU/cik.coleft.c'

    cik_data <-
      url %>%
      readr::read_table(col_names = F, progress = FALSE) %>%
      suppressMessages() %>%
      suppressWarnings()

    cik_data <-
      cik_data %>%
      tidyr::separate(X1,
                      into = c('nameIssuer', 'codeCIK'),
                      sep = '\\:[0][0]') %>%
      mutate(
        codeCIK = codeCIK %>% stringr::str_replace('\\:', ''),
        codeCIK = "00" %>% paste0(codeCIK),
        idCIK = codeCIK %>% as.numeric
      ) %>%
      mutate(
        nameIssuer = nameIssuer %>% str_to_upper(),
        urlRankAndFiled = paste0('http://rankandfiled.com/#/', idCIK, '/table'),
        urlEDGAR = list(
          'https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=',
          codeCIK,
          '&type=&dateb=&owner=exclude&start=0&count=100&output=xml'
        ) %>% purrr::invoke(paste0, .),
        datetimeData = Sys.time()
      ) %>%
      dplyr::select(idCIK, nameIssuer, everything()) %>%
      suppressMessages() %>%
      suppressWarnings() %>%
      arrange(desc(idCIK))

    if (return_message) {
      "You returned " %>%
        paste0(
          cik_data %>% nrow %>% formattable::comma(digits = 0),
          ' entities with registered CIK codes'
        ) %>%
        cat(fill = T)
    }

    return(cik_data)
  }


# FOIA --------------------------------------------------------------------


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
      paste0('https://www.sec.gov', .)

    url_df <-
      slugs %>%
      future_map_dfr(function(x) {
        items <-
          x %>%
          str_replace_all('/foia/logs/foia-log-fy|.csv', '') %>%
          str_split('\\-') %>%
          flatten_chr()

        if (items %>% length() == 2) {
          df <-
            data_frame(yearData = items[[1]] %>% as.numeric,
                       quarterData = items[[2]])
        } else {
          df <-
            data_frame(yearData = items %>% as.numeric())
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
        select(-dplyr::matches("^X")) %>%
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
      mutate_at(.vars = data %>% select(dplyr::matches("date")) %>% names,
                funs(. %>% lubridate::mdy())) %>%
      mutate_at(
        .vars = data %>% select(dplyr::matches("^name|^description|^category")) %>% names,
        funs(. %>% str_replace('\\-', '') %>% stringr::str_to_upper())
      ) %>%
      mutate(
        nameOrganization = ifelse(nameOrganization == '', NA, nameOrganization),
        typeOutcome = ifelse(typeOutcome == '-', NA, typeOutcome)
      ) %>%
      tidyr::separate(
        nameRequester,
        into = c('nameLast', 'nameFirst', 'titlePerson'),
        sep = '\\, '
      ) %>%
      mutate(
        nameRequester = ifelse(!nameFirst %>% is.na(),
                               paste(nameFirst, nameLast),
                               nameLast),
        nameRequester = ifelse(
          !titlePerson %>% is.na(),
          paste(nameRequester, titlePerson, sep = ', '),
          nameRequester
        ),
        isClosed = ifelse(statusRequest %>% str_detect('Closed'), TRUE, FALSE),
        isGranted = ifelse(typeOutcome %>% str_detect('Granted '), TRUE, FALSE),
        isGrantedPartial = ifelse(typeOutcome %>% str_detect('Granted/Denied'), TRUE, FALSE),
        urlFOIA = url
      ) %>%
      select(-c(nameLast, nameFirst, titlePerson)) %>%
      select(idSECRequest, nameRequester, everything()) %>%
      suppressWarnings() %>%
      suppressMessages()

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>%
        cat(fill = T)
    }

    return(data)
  }



#' SEC FOIA requests
#'
#' This function returns returns all Freedom of Information Act requests
#' submitted to the SEC begining in 2006.
#'
#' @param search_years vector of years to search, starting in 2006
#' @param return_message \code{TRUE} return a message after data import
#' @param nest_data \code{TRUE} return nested data frame
#' @return nested \code{data_frame} or \code{data_frame} if \code{nest_data = FALSE}
#' @references \href{http://sec.gov}{The Securities and Exchange Commission}
#' @export
#' @import purrr stringr dplyr rvest tidyr curl
#' @importFrom lubridate mdy
#' @importFrom readr read_csv
#' @family SEC
#' @examples
#' \dontrun{
#' sec_foia_requests(search_years = 2006:2017, nest_data = TRUE,
#' return_message = TRUE)
#' }
sec_foia_requests <-
  function(search_years = 2006:2016,
           nest_data = TRUE,
           return_message = TRUE) {
    url_df <-
      get_foia_url_df()

    if (!'search_years' %>% exists()) {
      urls <-
        url_df$urlFOIA
    } else {
      urls <-
        url_df %>%
        filter(yearData %in% search_years) %>%
        .$urlFOIA
    }

    parse_foia_url_df_safe <-
      purrr::possibly(parse_foia_url_df, NULL)

    all_data <-
      urls %>%
      future_map_dfr(function(x) {
        parse_foia_url_df_safe(url = x, return_message = TRUE)
      }) %>%
      arrange(desc(dateRequest))

    all_data <-
      all_data %>%
      mutate(urlFOIA = urlFOIA %>% str_replace_all('\\http:', '\\https:')) %>%
      left_join(url_df) %>%
      select(yearData, everything()) %>%
      suppressMessages()

    if (return_message) {
      list(
        "You parsed ",
        all_data %>% nrow() %>% formattable::comma(digits = 0),
        " SEC FOIA Requests from ",
        url_df$yearData %>% unique() %>% min(),
        ' to ',
        url_df$yearData %>% unique() %>% max()
      ) %>%
        purrr::invoke(paste0, .) %>%
        cat(fill = T)
    }

    if (nest_data) {
      all_data <-
        all_data %>%
        nest(-yearData, .key = dataFOIA)
    }

    return(all_data)
  }


# cusip -------------------------------------------------------------------

get_cusip_url_df <-
  function() {
    page <-
      "https://www.sec.gov/divisions/investment/13flists.htm" %>%
      read_html()

    periods <-
      page %>%
      html_nodes("#main-content a") %>%
      html_text() %>%
      str_replace_all("Current List ", '') %>%
      str_replace_all('\\(|\\)', '') %>%
      str_replace_all('Third', '3rd') %>%
      str_replace_all('First', '1st') %>%
      str_replace_all('Second', '2nd') %>%
      str_replace_all('Fourth', '4th')

    urls <-
      page %>%
      html_nodes("#main-content a") %>%
      html_attr('href') %>%
      paste0('https://www.sec.gov', .)

    url_df <-
      data_frame(namePeriod = periods, urlSEC = urls) %>%
      separate(namePeriod,
               sep = '\\ ',
               into = c('idPeriod', 'quarter', 'yearData')) %>%
      mutate(
        idPeriod = idPeriod %>% readr::parse_number(),
        yearData = yearData %>% as.numeric,
        quarter = 'q'
      ) %>%
      unite(periodData,
            yearData,
            quarter,
            idPeriod,
            sep = '',
            remove = F) %>%
      select(-quarter)

    current_year <-
      Sys.Date() %>% lubridate::year()

    year_df <-
      data_frame(yearData = 2004:(current_year),
                 pageStart = 3)

    url_df <-
      url_df %>%
      left_join(
        url_df %>% filter(yearData >= 2004) %>%
          select(periodData) %>%
          mutate(pageStart = 3) %>%
          bind_rows(data_frame(
            periodData = c(
              "2003q4",
              "2003q3",
              "2003q2",
              "2003q1",
              "2002q4",
              "2002q3",
              "2002q2",
              "2002q1",
              "2001q4",
              "2001q3",
              "2001q2",
              "2001q1",
              "2000q4",
              "2000q3",
              "2000q2",
              "2000q1",
              "1999q4",
              "1999q3",
              "1999q2",
              "1999q1",
              "1998q4",
              "1998q3",
              "1998q2",
              "1998q1",
              "1997q3",
              "1997q2",
              "1997q1",
              "1996q4",
              "1996q3",
              "1996q2",
              "1996q1"
            ),
            pageStart = c(
              2,
              3,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              2,
              1,
              1,
              1,
              1,
              1,
              1,
              2,
              2,
              2,
              2,
              2,
              3,
              2,
              2,
              2,
              2,
              2,
              2,
              3
            )
          ))
      ) %>%
      left_join(data_frame(
        periodData = "2016q3",
        areaTable = c("113.14286-64.42471-727.78378-520.05405")
      )) %>%
      suppressWarnings() %>%
      suppressMessages()

    return(url_df)
  }

parse_sec_cusip_url <-
  function(url = "https://www.sec.gov/divisions/investment/13f/13flist2016q3.pdf",
           start_page = 3,
           table_area = "113.14286-64.42471-727.78378-520.05405",
           return_message) {
    list("Be patient ocr'ing ", url, ' may take a while') %>%
      purrr::invoke(paste0, .) %>%
      cat(fill = T)

    if (!table_area %>% is.na()) {
      table_area <-
        table_area %>%
        str_split('\\-') %>%
        flatten_chr %>%
        as.numeric()
    }

    df_metadata <-
      url %>%
      tabulizer::extract_metadata() %>%
      flatten_df()

    if ('modified' %in% names(df_metadata)) {
      date_parts <-
        df_metadata$modified %>% str_replace_all(" EDT | EST ", ' ') %>%
        str_split('\\ ') %>%
        flatten_chr()

      date_year <-
        date_parts[date_parts %>% length()]

      other_parts <-
        date_parts[1:(date_parts %>% length() - 1)] %>%
        .[2:length(.)]

      datetime_file <-
        c(date_year, other_parts) %>%
        paste0(collapse = ' ') %>%
        lubridate::ymd_hms()
    } else {
      datetime_file <- NA
    }

    if (table_area %>% is.na()) {
      tables <-
        url %>%
        tabulizer::extract_tables(start_page:df_metadata$pages)
    } else {
      tables <-
        url %>%
        tabulizer::extract_tables(start_page:df_metadata$pages,
                       area = list(table_area))
    }

    all_data <-
      seq_along(tables) %>%
      future_map_dfr(function(x) {
        tables[[x]] %>%
          as_data_frame() %>%
          mutate_all(str_trim)
      }) %>%
      select(-6) %>%
      tidyr::separate(V1,
                      sep = ' ',
                      into = c('idCUSIPBase', 'codeCUSIP1', 'codeCUSIP2')) %>%
      mutate_all(str_trim) %>%
      unite(idCUSIP,
            idCUSIPBase,
            codeCUSIP1,
            codeCUSIP2,
            sep = '',
            remove = FALSE) %>%
      select(idCUSIP, everything()) %>%
      dplyr::rename(
        isEquitySecurity = V2,
        nameIssuer = V3,
        descriptionIssuer = V4,
        statusIssuer = V5
      ) %>%
      filter(!idCUSIPBase %>% str_detect("CUSIP")) %>%
      suppressWarnings()

    all_data <-
      all_data %>%
      mutate(
        codeCUSIP2 = codeCUSIP2 %>% as.numeric(),
        isEquitySecurity = ifelse(isEquitySecurity == "*", TRUE, FALSE),
        statusIssuer = ifelse(statusIssuer == '', NA, statusIssuer),
        descriptionIssuer = ifelse(descriptionIssuer == '', statusIssuer, descriptionIssuer),
        statusIssuer = ifelse(statusIssuer == descriptionIssuer, NA, statusIssuer),
        isSecurityNew = ifelse(statusIssuer == "ADDED", TRUE, FALSE),
        isSecurityDeleted = ifelse(statusIssuer %>% str_detect("DELETED"), TRUE, FALSE)
      ) %>%
      unite(nameSecurity,
            nameIssuer,
            descriptionIssuer,
            sep = ' ',
            remove = FALSE)

    all_data <-
      all_data %>%
      left_join(
        all_data %>% mutate(idRow = 1:n()) %>% group_by(idCUSIPBase) %>% filter(idRow == min(idRow)) %>%
          ungroup() %>% select(idCUSIPBase, nameEntity = nameIssuer)
      ) %>%
      select(idCUSIP:codeCUSIP2,
             nameEntity,
             nameIssuer,
             nameSecurity,
             everything()) %>%
      suppressMessages() %>%
      mutate(urlSEC = url) %>%
      distinct()

    all_data <-
      all_data %>%
      mutate(
        isDebtSecurity = descriptionIssuer %>% str_detect("%"),
        pctRateNote = ifelse(isDebtSecurity == T, descriptionIssuer, NA)
      ) %>%
      tidyr::separate(pctRateNote,
                      sep = '\\%',
                      into = c('pctRateNote', 'ignore')) %>%
      mutate(
        pctRateNote = pctRateNote %>% readr::parse_number() / 100,
        isADR = descriptionIssuer %>% str_detect("\\ ADR")
      ) %>%
      select(-dplyr::matches("ignore")) %>%
      select(idCUSIP:nameSecurity,
             dplyr::matches("is"),
             dplyr::matches("pct"),
             everything())

    if (return_message) {
      list(
        "Retrieved ",
        all_data %>% nrow() %>% formattable::comma(digits = 0),
        ' CUSIPs as of ',
        datetime_file
      ) %>%
        purrr::invoke(paste0, .) %>%
        cat(fill = T)
    }

    return(all_data)
  }

#' SEC CUSIPs
#'
#' This function returns information on all Committee on Uniform Securities Identification Procedures [CUSIP]
#' registered entities for a specified period.  The SEC exposes this data in PDF
#' form so this function can take a long time to run.
#'
#' @descripton returns a data frame with all CUSIPs for specified years and quarters
#'
#' @param years vector years to search, starting in 1996
#' @param quarters quarters to search \itemize{
#' \item\code{NULL}(default): all quarters,
#' \item \code{1}: Q1
#' \item \code{2}: Q2
#' \item \code{3}: Q3
#' \item \code{4}: Q4
#' }
#' @param only_most_recent \code{TRUE} search only the most recent period
#' @param return_message \code{TRUE} return a message after data import
#' @param nest_data \code{TRUE} return nested data frame
#' @return nested \code{data_frame} or \code{data_frame} if \code{nest_data = FALSE}
#' @references \href{http://sec.gov}{The Securities and Exchange Commission}
#' @import purrr stringr dplyr rvest formattable tidyr xml2 tabulizer
#' @importFrom lubridate mdy
#' @importFrom readr read_csv
#' @return
#' @export
#' @family SEC
#' @family entity search
#' @family securities search
#'
#' @examples
#' \dontrun{
#' sec_cusips(only_most_recent = FALSE, years = c(2016, 2010),
#' quarters = c(4),
#' return_message = TRUE)
#' }
sec_cusips <-
  function(only_most_recent = FALSE,
           years = 1996:2016,
           quarters = NULL,
           nest_data = FALSE,
           return_message = TRUE) {
    is_blank <-
      (only_most_recent == F &
         years %>% is_null() & quarters %>% is_null())

    if (is_blank) {
      stop("Please enter years and/or quarters or select only most recent")
    }

    message(
      "Please be patient this function can take a while\nCUSIP dumps are generally over 500 pages\neach one of which must be OCR'd"
    )

    url_df <-
      get_cusip_url_df()

    is_current <-
      only_most_recent == TRUE

    if (is_current) {
      urls_df <-
        url_df %>%
        slice(1)
    } else {
      urls_df <-
        url_df %>%
        filter(yearData %in% years)
      if (!quarters %>% is_null()) {
        if (quarters %in% 1:4 %>% sum() == 0) {
          stop("Quarters can only be 1, 2, 3, or 4")
        }
        urls_df <-
          urls_df %>%
          filter(idPeriod %in% quarters)
      }
    }

    parse_sec_cusip_url_safe <-
      purrr::possibly(parse_sec_cusip_url, data_frame())

    all_data <-
      1:nrow(urls_df) %>%
      future_map_dfr(function(x) {
        parse_sec_cusip_url_safe(
          url = urls_df$urlSEC[[x]],
          return_message = return_message,
          table_area = urls_df$areaTable[[x]],
          start_page = urls_df$pageStart[[x]]
        )
      }) %>%
      suppressWarnings() %>%
      suppressMessages()

    all_data <-
      all_data %>%
      left_join(url_df %>% select(periodData, yearData, urlSEC)) %>%
      suppressMessages()

    if (return_message) {
      "You returned " %>%
        paste0(
          all_data %>% dplyr::select(idCUSIP) %>% distinct() %>% nrow() %>% formattable::comma(digits = 0),
          ' securities with cusips codes'
        ) %>%
        cat(fill = T)

    }

    if (nest_data) {
      all_data <-
        all_data %>%
        nest(-periodData, .key = dataCUSIPs)

    }

    return(all_data)
  }

# closed_end_funds --------------------------------------------------------

get_closed_end_fund_url_df <-
  function() {
    page <-
      "https://www.sec.gov/opa/data/opendatasetsshtmlclosed-end-investment_company.html" %>%
      read_html()

    urls <-
      page %>%
      html_nodes('.medium-9 a:nth-child(1)') %>%
      html_attr('href') %>%
      paste0('https://www.sec.gov', .)

    years <-
      urls %>% str_replace_all("https://www.sec.gov/open/datasets/closed-end_investment_company|.csv",
                               '') %>%
      readr::parse_number()

    years[[1]] <-
      Sys.Date() %>% lubridate::year()

    data_frame(yearData = years,
               urlData = urls)

  }

parse_closed_end_fund_url <-
  function(url = "https://www.sec.gov/open/datasets/closed-end_investment_company.csv",
           return_message = TRUE) {
    df <-
      url %>%
      read_csv() %>%
      slice(-1) %>%
      mutate_all(str_to_upper) %>%
      suppressWarnings() %>%
      suppressMessages() %>%
      select(1:10) %>%
      purrr::set_names(
        c(
          'idSEC',
          'idCIK',
          'nameEntity',
          'addressStreet1Entity',
          'addressStreet2Entity',
          'cityEntity',
          'stateEntity',
          'zipcodeEntity',
          'dateLastFiling',
          'typeLastFiling'
        )
      )
    df <-
      df %>%
      mutate(
        idCIK = idCIK %>% as.numeric(),
        dateLastFiling = dateLastFiling %>% lubridate::mdy(),
        hasCO = addressStreet1Entity %>% str_detect("C/O"),
        hasCO1 = addressStreet2Entity %>% str_detect("C/O"),
        addressStreetEntity = ifelse(
          addressStreet2Entity %>% is.na(),
          addressStreet1Entity,
          paste(addressStreet1Entity, addressStreet2Entity)
        ),
        addressEntity = list(
          addressStreetEntity,
          " ",
          cityEntity,
          ", ",
          stateEntity,
          " ",
          zipcodeEntity
        ) %>% purrr::invoke(paste0, .),
        nameManager = ifelse(hasCO == TRUE, addressStreet1Entity, NA) %>% str_replace_all("C/O", ""),
        nameManager1 = ifelse(hasCO1 == TRUE, addressStreet2Entity, NA) %>% str_replace_all("C/O", ""),
        nameManager = ifelse(!nameManager1 %>% is.na(), nameManager1, nameManager),
        urlSECData = url
      ) %>%
      select(-dplyr::matches("nameManager1|hasCO")) %>%
      select(
        idSEC,
        idCIK,
        dateLastFiling,
        nameEntity,
        nameManager,
        typeLastFiling,
        everything()
      )

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>%
        cat(fill = T)
    }

    return(df)

  }

#' SEC registered closed-end funds
#'
#' This function returns information on SEC registered
#' closed-end funds for a specified period.
#'
#' @param years vector of years to search starting in 2012
#' @param only_most_recent \code{TRUE} return only the most recent year
#' @param return_message \code{TRUE} return a message after data import
#' @param nest_data \code{TRUE} return nested data frame
#' @return nested \code{data_frame} or \code{data_frame} if \code{nest_data = FALSE}
#' @references \href{http://sec.gov}{The Securities and Exchange Commission}
#' @export
#' @import purrr stringr dplyr rvest readr lubridate formattable tidyr
#' @importFrom readr read_csv
#' @family SEC
#' @family entity search
#' @family fund data
#' @examples
#' \dontrun{
#' sec_closed_end_funds(years = 2015:2017)
#' }
sec_closed_end_funds <-
  function(only_most_recent = FALSE,
           years = 2014:2017,
           nest_data = TRUE,
           return_message = TRUE) {
    url_df <-
      get_closed_end_fund_url_df() %>%
      suppressWarnings()

    if (!years %>% is_null()) {
      urls  <-
        url_df %>%
        filter(yearData %in% years) %>%
        .$urlData
    }

    if (years %>% length() > 1) {
      only_most_recent <-
        FALSE
    }

    if (years %>% is_null() & (!only_most_recent)) {
      urls <-
        url_df$urlData
    }

    if (only_most_recent) {
      urls <-
        url_df %>%
        slice(1) %>%
        .$urlData
    }

    parse_closed_end_fund_url_safe <-
      purrr::possibly(parse_closed_end_fund_url, data_frame())

    all_data <-
      urls %>%
      future_map_dfr(function(x) {
        parse_closed_end_fund_url_safe(url = x, return_message = TRUE)
      })

    all_data <-
      all_data %>%
      left_join(url_df %>% dplyr::rename(urlSECData = urlData)) %>%
      suppressMessages() %>%
      select(yearData, everything()) %>%
      suppressMessages()

    if (return_message) {
      list(
        "Acquired ",
        all_data %>% nrow() %>% formattable::comma(digits = 0),
        ' Closed End Funds'
      ) %>%
        purrr::invoke(paste0, .) %>%
        cat(fill = T)
    }

    if (nest_data) {
      all_data <-
        all_data %>%
        nest(-yearData, .key = dataClosedEndFunds)
    }

    return(all_data)
  }


# investment_companies ----------------------------------------------------

#' SEC registered investment companies
#'
#' This function returns information about SEC
#' registered investment companies including:\itemize{
#' \item Insurance Companies
#' \item Insurance Accounts
#' \item Mutual Funds
#' \item Closed-end Funds
#' }
#'
#' @param only_most_recent \code{TRUE} return only the most recent year
#' @param return_message \code{TRUE} return a message after data import
#' @return nested \code{data_frame} or \code{data_frame} if \code{nest_data = FALSE}
#' @references \href{http://sec.gov}{The Securities and Exchange Commission}
#' @export
#' @import purrr stringr dplyr rvest formattable tidyr
#' @importFrom lubridate mdy
#' @importFrom readr read_csv
#' @family SEC
#' @family entity search
#' @family fund search
#' @examples
#' sec_investment_companies(nest_data = TRUE, return_message = TRUE)

sec_investment_companies <-
  function(nest_data = TRUE,
           return_message = TRUE) {
    df <-
      "https://www.sec.gov/open/datasets/investment_company_series_class.csv" %>%
      read_csv() %>%
      purrr::set_names(
        c(
          'idSEC',
          'idCIK',
          'nameManager',
          'idOrganizationType',
          'idSeries',
          'nameFund',
          'idClass',
          'nameClass',
          'idTicker',
          'addressStreet1Manager',
          'addressStreet2Manager',
          'cityManager',
          'stateManager',
          'zipcodeManager'
        )
      ) %>%
      mutate_all(funs(. %>% str_to_upper() %>% str_trim())) %>%
      mutate_at(.vars = c('idCIK', 'idOrganizationType'),
                funs(. %>% as.numeric())) %>%
      suppressMessages()
    df <-
      df %>%
      mutate(
        countCharAddress2 = addressStreet2Manager %>% nchar,
        addressStreet2Manager = ifelse(countCharAddress2 == 1, NA, addressStreet2Manager),
        addressStreet2Manager = ifelse(addressStreet2Manager == "[NULL]", NA, addressStreet2Manager),
        idTicker = ifelse(idTicker == "[NULL]", NA, idTicker)
      ) %>%
      select(-countCharAddress2) %>%
      mutate(
        addressStreetManager = ifelse(
          addressStreet2Manager %>% is.na(),
          addressStreet1Manager,
          paste(addressStreet1Manager, addressStreet2Manager)
        ),
        addressEntity = list(
          addressStreetManager,
          " ",
          cityManager,
          ", ",
          stateManager,
          " ",
          zipcodeManager
        ) %>% purrr::invoke(paste0, .)
      )

    df <-
      df %>%
      left_join(data_frame(
        idOrganizationType = c(30, 31, 32, 33, 55, 75),
        typeOrganization = c(
          'Open-end Mutual Fund',
          'Open-end Insurance Separate Account',
          'Variable Annuity Separate Account',
          'Variable Life Separate Account',
          'Closed-end Fund',
          'Insurance Unit Investment Trust'
        )
      )) %>%
      mutate(typeOrganization = typeOrganization %>% stringr::str_to_upper()) %>%
      select(idSEC:idOrganizationType, typeOrganization, everything()) %>%
      suppressMessages()

    if (return_message) {
      list(
        "Acquired ",
        df %>% nrow() %>% formattable::comma(digits = 0),
        ' SEC registered investment funds'
      ) %>%
        purrr::invoke(paste0, .) %>%
        cat(fill = T)
    }

    if (nest_data) {
      df <-
        df %>%
        nest(-nameManager, .key = dataManager)
    }

    return(df)

  }


# money_market_funds ------------------------------------------------------

get_mmf_url_df <-
  function() {
    page <-
      "https://www.sec.gov/opa/data/opendatasets-mmfhtml.html" %>%
      read_html()

    slugs <-
      page %>%
      html_nodes('p:nth-child(3) em a') %>%
      html_attr('href')

    urls_csv <-
      slugs[slugs %>% str_detect(".csv")] %>%
      paste0('https://www.sec.gov', .)

    csv_dates <-
      urls_csv %>%
      str_replace_all('https://www.sec.gov/open/datasets/mmf|.csv|\\-', '')

    if (csv_dates[csv_dates %>% length()] == '') {
      csv_dates[csv_dates %>% length()] <-
        "201301"
    }

    csv_dates <-
      list(csv_dates, "01") %>%
      purrr::invoke(paste0, .) %>%
      lubridate::ymd()

    data_frame(dateData = csv_dates,
               urlData = urls_csv) %>%
      mutate(
        yearData = dateData %>% lubridate::year(),
        monthData = dateData %>% lubridate::month()
      )

  }

parse_mmf_url <-
  function(url = "https://www.sec.gov/open/datasets/mmf-2016-09.csv",
           return_message = TRUE) {
    df <-
      url %>%
      read_csv() %>%
      mutate_all(str_to_upper) %>%
      purrr::set_names(
        c(
          'dateData',
          'idCIK',
          'nameFiler',
          'nameIssuer',
          'idSeries',
          'typeFund',
          'nameClass',
          'idClass',
          'nameManager',
          'idTicker',
          'nameSubAdviser'
        )
      ) %>%
      suppressMessages() %>%
      suppressWarnings()

    if (df$dateData[[1]] %>% is.na()) {
      df <-
        df %>%
        slice(-(1:3))
    }

    is_dmy <-
      !df$dateData[[1]] %>% dmy() %>% is.na() %>%
      suppressWarnings()

    if (is_dmy) {
      df <-
        df %>%
        mutate(dateData = dateData %>% lubridate::dmy(),
               urlSEC = url) %>%
        suppressMessages() %>%
        suppressWarnings()
    }

    is_mdy <-
      !df$dateData[[1]] %>% mdy() %>% is.na() %>%
      suppressWarnings()

    if (is_mdy) {
      df <-
        df %>%
        mutate(dateData = dateData %>% lubridate::mdy(),
               urlSEC = url) %>%
        suppressMessages() %>%
        suppressWarnings()
    }

    is_ymd <-
      !df$dateData[[1]] %>% ymd() %>% is.na() %>%
      suppressWarnings()

    if (is_ymd) {
      df <-
        df %>%
        mutate(dateData = dateData %>% lubridate::ymd(),
               urlSEC = url) %>%
        suppressMessages() %>%
        suppressWarnings()
    }

    df <-
      df %>%
      mutate(idCIK = idCIK %>% as.numeric()) %>%
      suppressWarnings()

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>%
        cat(fill = T)
    }

    return(df)

  }

#' SEC registered money market funds
#'
#' This function returns information about SEC registered
#' money market funds for specified months and years from
#' January 2013 onwards.
#'

#' @param years years to include starting in 2013
#' @param months months to include
#' @param only_most_recent \code{TRUE} search only the most recent period
#' @param return_message \code{TRUE} return a message after data import
#' @param nest_data \code{TRUE} return nested data frame
#' @return nested \code{data_frame} or \code{data_frame} if \code{nest_data = FALSE}
#' @references \href{http://sec.gov}{The Securities and Exchange Commission}
#' @import purrr stringr dplyr rvest formattable lubridate readr
#' @return
#' @family SEC
#' @family entity search
#' @family fund data
#' @export
#'
#' @examples
#' \dontrun{
#' sec_money_market_funds(only_most_recent = TRUE, nest_data = FALSE)
#' sec_money_market_funds(only_most_recent = FALSE, years = 2013:2016)
#' }
sec_money_market_funds <-
  function(only_most_recent = TRUE,
           years = NULL,
           months = NULL,
           nest_data = TRUE,
           return_message = TRUE) {
    missing <-
      only_most_recent == F & (years %>% is_null())
    if (missing) {
      stop("Please enter a years or only most recent")
    }

    parse_mmf_url_safe <-
      purrr::possibly(parse_mmf_url, data_frame())

    mmf_url_df <-
      get_mmf_url_df()

    if (only_most_recent) {
      urls <-
        mmf_url_df %>%
        slice(1) %>%
        .$urlData
    }

    if (years %>% length() > 1) {
      only_most_recent <-
        FALSE
    }

    if (!only_most_recent) {
      possibly_years <-
        (mmf_url_df$yearData %>% min()):(mmf_url_df$yearData %>% max())

      if (years %in% possibly_years %>% sum()  == 0) {
        stop("Only possible years are " %>% paste0(paste0(years, collapse = ', ')))
      }

      url_df <-
        mmf_url_df %>%
        filter(yearData %in% years)

      if (!months %>% is_null()) {
        url_df <-
          url_df %>%
          filter(monthData %in% months)
      }

      urls <-
        url_df %>%
        .$urlData
    }

    all_data <-
      urls %>%
      future_map_dfr(function(x) {
        parse_mmf_url_safe(url = x, return_message = return_message)
      })

    has_missing_cik <-
      all_data %>%
      filter(idCIK %>% is.na()) %>%
      nrow() > 0

    if (has_missing_cik) {
      all_data <-
        all_data %>%
        select(-idCIK) %>%
        left_join(all_data %>%
                    filter(!idCIK %>% is.na()) %>%
                    select(idSeries, idCIK) %>%
                    distinct()) %>%
        select(dateData, idCIK, everything()) %>%
        suppressMessages()
    }

    all_data <-
      all_data %>%
      mutate(
        urlRankandFiled = list('http://rankandfiled.com/', '#/filers/', idCIK, '/table') %>% purrr::invoke(paste0, .)
      )

    if (return_message) {
      list(
        "You acquired ",
        all_data %>% select(idCIK, nameIssuer) %>% distinct() %>% nrow() %>% formattable::comma(digits = 0),
        " money market funds"
      ) %>%
        purrr::invoke(paste0, .) %>%
        cat(fill = T)
    }

    if (nest_data) {
      if (only_most_recent) {
        all_data <-
          all_data %>%
          nest(-nameFiler, .key = dataMutualFund)
      } else {
        all_data <-
          all_data %>%
          nest(-c(nameFiler, dateData), .key = dataMutualFund)
      }
    }

    return(all_data)
  }


# bankruptcies ------------------------------------------------------------

parse_bankruptcy_url <-
  function(url = "https://www.sec.gov/open/datasets/public_company_bankruptcy_cases.csv",
           return_message = TRUE) {
    df <-
      url %>%
      read_csv() %>%
      mutate_all(str_to_upper) %>%
      suppressWarnings() %>%
      purrr::set_names(
        c(
          'idDistrictBankruptcy',
          'stateBankruptcy',
          'nameEntity',
          'amountAssets',
          'amountLiabilities'
        )
      ) %>%
      mutate(urlData = url) %>%
      suppressMessages() %>%
      suppressWarnings()

    df <-
      df %>%
      mutate_at(c('amountAssets', 'amountLiabilities'),
                funs(. %>% as.numeric() * 1000000)) %>%
      mutate(amountEquity = amountAssets - amountLiabilities) %>%
      select(idDistrictBankruptcy:amountLiabilities,
             amountEquity,
             everything()) %>%
      suppressWarnings()

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>%
        cat(fill = T)
    }

    return(df)
  }

#' SEC public filing bankruptcies
#'
#' This function returns information containing
#' bankruptcies of once public companies from 2009 to 2011
#'
#' @param return_message \code{TRUE} return a message after data import
#' @param nest_data \code{TRUE} return nested data frame
#' @return nested \code{data_frame} or \code{data_frame} if \code{nest_data = FALSE}
#' @references \href{http://sec.gov}{The Securities and Exchange Commission}
#' @export
#' @import purrr stringr dplyr rvest formattable
#' @family SEC
#' @family entity search
#' @examples
#' \dontrun{
#' sec_bankruptcies(nest_data = FALSE, return_message = TRUE)
#' }
sec_bankruptcies <-
  function(nest_data = TRUE,
           return_message = TRUE) {
    page <-
      "https://www.sec.gov/opa/data/opendatasetsshtmlbankruptcy.html" %>%
      read_html()

    url_data <-
      page %>%
      html_nodes('.medium-9 a:nth-child(1)') %>%
      html_attr('href') %>%
      paste0("https://www.sec.gov", .)

    years <-
      url_data %>%
      str_replace_all("https://www.sec.gov/open/datasets/public_company_bankruptcy_cases|.csv",
                      '') %>%
      readr::parse_number()

    years[[1]] <-
      2009

    url_df <-
      data_frame(yearData = years,
                 urlData = url_data)

    parse_bankruptcy_url_safe <-
      purrr::possibly(parse_bankruptcy_url, data_frame())

    all_data <-
      url_df$urlData %>%
      future_map_dfr(function(x) {
        parse_bankruptcy_url_safe(url = x, return_message = return_message)
      })


    all_data <-
      all_data %>%
      left_join(url_df) %>%
      select(yearData, everything()) %>%
      mutate_at(
        .vars = c('amountAssets', 'amountLiabilities', 'amountEquity'),
        funs(. %>% formattable::currency(digits = 0))
      ) %>%
      suppressWarnings() %>%
      suppressMessages()

    if (return_message) {
      list(
        "You acquired ",
        all_data %>% nrow() %>% formattable::comma(digits = 0),
        " Public Company Bankruptcies from ",
        all_data$yearData %>% min(),
        ' to ',
        all_data$yearData %>% max(),
        " totaling ",
        all_data$amountLiabilities %>% sum(na.rm = TRUE) %>% formattable::currency(digits = 0),
        ' in Liabilities'
      ) %>%
        purrr::invoke(paste0, .) %>%
        cat(fill = T)
    }

    if (nest_data) {
      all_data <-
        all_data %>%
        nest(-yearData, .key = dataBankruptcies)
    }

    return(all_data)
  }

# broker_dealers ----------------------------------------------------------

get_broker_data_urls <-
  function() {
    page <-
      "https://www.sec.gov/foia/docs/bd-archive.htm" %>%
      read_html()

    slugs <-
      page %>%
      html_nodes('#main-content a') %>%
      html_attr('href')

    url_data <-
      'https://www.sec.gov' %>%
      paste0(slugs)

    periods <-
      slugs %>%
      str_replace_all('/foia/bdreports/bd|.txt', '') %>%
      lubridate::mdy()

    data_frame(dateData = periods,
               urlData = url_data) %>%
      mutate(
        yearData = dateData %>% lubridate::year(),
        monthData = dateData %>% lubridate::month()
      )
  }

parse_brokers_url <-
  function(url = "https://www.sec.gov/foia/bdreports/bd120116.txt",
           return_message = TRUE) {
    df <-
      url %>%
      read_tsv(col_names = FALSE) %>%
      select(-dplyr::matches("X9")) %>%
      mutate_all(str_to_upper) %>%
      purrr::set_names(
        c(
          'idCIK',
          'nameEntity',
          'idReportingFilingNumber',
          'addressStreet1Entity',
          'addressStreet2Entity',
          'cityEntity',
          'stateEntity',
          'zipcodeEntity'
        )
      ) %>%
      mutate(
        typeFiler = 'Registered Broker Dealer',
        addressStreetEntity = ifelse(
          addressStreet2Entity %>% is.na(),
          addressStreet1Entity,
          paste(addressStreet1Entity, addressStreet2Entity)
        ),
        addressEntity = list(
          addressStreetEntity,
          " ",
          cityEntity,
          ", ",
          stateEntity,
          " ",
          zipcodeEntity
        ) %>% purrr::invoke(paste0, .),
        urlData = url
      ) %>%
      mutate_at(.vars = c('idCIK', 'idReportingFilingNumber'),
                funs(. %>% as.numeric())) %>%
      suppressWarnings() %>%
      suppressMessages() %>%
      select(idCIK:idReportingFilingNumber, addressEntity, everything())

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>%
        cat(fill = T)
    }

    return(df)
  }

#' SEC registered broker-dealers
#'
#' This function returns information about SEC registered
#' broker dealers for specified periods from March 2007 onwards.
#'
#' @param years years to include starting in 2007
#' @param months months to include
#' @param only_most_recent \code{TRUE} search only the most recent period
#' @param return_message \code{TRUE} return a message after data import
#' @param nest_data \code{TRUE} return nested data frame
#' @return nested \code{data_frame} or \code{data_frame} if \code{nest_data = FALSE}
#' @references \href{http://sec.gov}{The Securities and Exchange Commission}
#' @import purrr stringr dplyr rvest formattable readr
#' @export
#'
#' @examples
#' @family SEC
#' @family entity search
#' @family broker dealers
#' \dontrun{
#' sec_broker_dealers(only_most_recent = TRUE)
#' sec_broker_dealers(only_most_recent = FALSE, years = 2016:2017, nest_data = FALSE)
#' }
sec_broker_dealers <-
  function(only_most_recent = TRUE,
           years = NULL,
           months = NULL,
           nest_data = FALSE,
           return_message = TRUE) {
    missing <-
      only_most_recent == F & (years %>% is_null())
    if (missing) {
      stop("Please enter a years or only most recent")
    }

    parse_brokers_url_safe <-
      purrr::possibly(parse_brokers_url, data_frame())

    broker_df <-
      get_broker_data_urls()

    if (only_most_recent) {
      urls <-
        broker_df %>%
        slice(1) %>%
        .$urlData
    }

    if (years %>% length() > 1) {
      only_most_recent <-
        FALSE
    }

    if (!only_most_recent) {
      possibly_years <-
        (broker_df$yearData %>% min()):(broker_df$yearData %>% max())

      if (years %in% possibly_years %>% sum()  == 0) {
        stop("Only possible years are " %>% paste0(paste0(years, collapse = ', ')))
      }

      url_df <-
        broker_df %>%
        filter(yearData %in% years)

      if (!months %>% is_null()) {
        url_df <-
          url_df %>%
          filter(monthData %in% months)
      }

      urls <-
        url_df %>%
        .$urlData
    }

    all_data <-
      urls %>%
      future_map_dfr(function(x) {
        parse_brokers_url_safe(url = x, return_message = return_message)
      })

    all_data <-
      all_data %>%
      left_join(broker_df %>% select(dateData, urlData)) %>%
      suppressWarnings() %>%
      select(dateData, everything()) %>%
      suppressWarnings() %>%
      suppressMessages()

    if (return_message) {
      list(
        "You acquired ",
        all_data %>% select(nameEntity, idCIK) %>% distinct() %>% nrow() %>% formattable::comma(digits = 0),
        ' SEC registered broker dealers from ',
        all_data$dateData %>% min(),
        ' to ',
        all_data$dateData %>% max()
      ) %>%
        purrr::invoke(paste0, .) %>%
        cat(fill = T)
    }

    if (nest_data) {
      all_data <-
        all_data %>%
        nest(-dateData, .key = dataBrokers)
    }

    return(all_data)

  }


# municipal_dealers -------------------------------------------------------

parse_municpal_dealer_url <-
  function(url = "https://www.sec.gov/foia/muniadvisors/ma120116.zip",
           return_message = TRUE) {
    df <-
      url %>%
      rio::import() %>%
      data.frame(stringsAsFactors = FALSE) %>%
      as_data_frame() %>%
      mutate_all(str_to_upper) %>%
      suppressWarnings() %>%
      slice(-c(1:3)) %>%
      purrr::set_names(c('nameEntity', 'idReportingFilingNumber', 'idCIK')) %>%
      mutate(idCIK = idCIK %>% as.numeric()) %>%
      mutate(typeFiler = 'Registered Municipal Advisors',
             urlData = url)

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>%
        cat(fill = T)
    }

    return(df)

  }

get_muni_advisor_urls <-
  function() {
    page <-
      "https://www.sec.gov/foia/docs/muniadvisors-archive.htm" %>%
      read_html()

    slugs <-
      page %>%
      html_nodes('#main-content a') %>%
      html_attr('href')

    slugs <-
      slugs[slugs %>% str_detect(".zip")]

    url_data <-
      'https://www.sec.gov' %>%
      paste0(slugs)

    periods <-
      slugs %>%
      str_replace_all('/foia/muniadvisors/ma|.zip', '') %>%
      lubridate::mdy()

    data_frame(dateData = periods,
               urlData = url_data) %>%
      mutate(
        yearData = dateData %>% lubridate::year(),
        monthData = dateData %>% lubridate::month()
      )

  }

#' SEC registered municipal advisors
#'
#' This function returns data on SEC registered
#' municipal advisors from March 2015 onwards.
#'
#' @param years years to include starting in 2015
#' @param months months to include
#' @param only_most_recent \code{TRUE} search only the most recent period
#' @param return_message \code{TRUE} return a message after data import
#' @param nest_data \code{TRUE} return nested data frame
#' @return nested \code{data_frame} or \code{data_frame} if \code{nest_data = FALSE}
#' @references \href{http://sec.gov}{The Securities and Exchange Commission}
#' @export
#' @import purrr stringr dplyr rvest formattable lubridate readr
#' @importFrom rio import
#' @family SEC
#' @family broker dealers
#' @family entity search
#' @examples
#' \dontrun{
#' sec_municipal_advisors(only_most_recent = TRUE, nest_data = FALSE)
#' sec_municipal_advisors(only_most_recent = FALSE, years = 2015:2017, nest_data = TRUE)
#' }
sec_municipal_advisors <-
  function(only_most_recent = TRUE,
           years = NULL,
           months = NULL,
           nest_data = FALSE,
           return_message = TRUE) {
    missing <-
      only_most_recent == F & (years %>% is_null())
    if (missing) {
      stop("Please enter a years or only most recent")
    }

    parse_municpal_dealer_url_safe <-
      purrr::possibly(parse_municpal_dealer_url, data_frame())

    muni_df <-
      get_muni_advisor_urls()

    if (only_most_recent) {
      urls <-
        muni_df %>%
        slice(1) %>%
        .$urlData
    }

    if (years %>% length() > 1) {
      only_most_recent <-
        FALSE
    }

    if (!only_most_recent) {
      possibly_years <-
        (muni_df$yearData %>% min()):(muni_df$yearData %>% max())

      if (years %in% possibly_years %>% sum()  == 0) {
        stop("Only possible years are " %>% paste0(paste0(years, collapse = ', ')))
      }

      url_df <-
        muni_df %>%
        filter(yearData %in% years)

      if (!months %>% is_null()) {
        url_df <-
          url_df %>%
          filter(monthData %in% months)
      }

      urls <-
        url_df %>%
        .$urlData
    }

    all_data <-
      urls %>%
      future_map_dfr(function(x) {
        parse_municpal_dealer_url_safe(url = x, return_message = return_message)
      })

    all_data <-
      all_data %>%
      left_join(muni_df %>% select(dateData, urlData)) %>%
      suppressWarnings() %>%
      select(dateData, everything()) %>%
      suppressWarnings() %>%
      suppressMessages()

    if (return_message) {
      list(
        "You acquired ",
        all_data %>% select(nameEntity, idCIK) %>% distinct() %>% nrow() %>% formattable::comma(digits = 0),
        ' SEC registered broker dealers from ',
        all_data$dateData %>% min(),
        ' to ',
        all_data$dateData %>% max()
      ) %>%
        purrr::invoke(paste0, .) %>%
        cat(fill = T)
    }

    if (nest_data) {
      all_data <-
        all_data %>%
        nest(-dateData, .key = dataMunicipalDealers)
    }

    return(all_data)

  }




# fail_to_deliver ---------------------------------------------------------

parse_fail_to_deliver_url <-
  function(url = "https://www.sec.gov/foia/failsreports/cnsfails201611b.zip",
           return_message = TRUE) {
    td <-
      tempdir()
    tf <-
      tempfile(tmpdir = td, fileext = ".zip")

    url %>%
      curl::curl_download(destfile = tf)

    con <-
      unzip(tf, exdir = td)

    df <-
      con %>%
      read_delim(delim = '|', col_names = FALSE) %>%
      slice(-1) %>%
      purrr::set_names(
        c(
          'dateSettlement',
          'idCUSIP',
          'idTicker',
          'countShares',
          'descriptionSecurity',
          'amountPrice'
        )
      ) %>%
      mutate_all(str_to_upper) %>%
      mutate(
        dateSettlement = dateSettlement %>% lubridate::ymd(),
        amountPrice = amountPrice %>% readr::parse_number(),
        countShares = countShares %>% readr::parse_number(),
        valueTransaction = amountPrice * countShares,
        urlData = url
      ) %>%
      suppressWarnings() %>%
      suppressMessages()

    con %>%
      unlink()

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>%
        cat(fill = T)
    }

    return(df)
  }

get_fail_to_deliver_urls <-
  function() {
    page <-
      "https://www.sec.gov/foia/docs/failsdata-archive.htm" %>%
      read_html()

    slugs <-
      page %>%
      html_nodes('li a') %>%
      html_attr('href')

    urls <-
      slugs %>% paste0('https://www.sec.gov', .)

    periods <-
      slugs %>%
      str_replace_all(
        'https://www.sec.gov/foia/failsreports/cnsp_sec_fails_|https://www.sec.gov/foia/failsreports/cnsfails2|.zip|foia/failsreports/cnsp_sec_fails_|/foia/failsreports/cnsfails|/',
        ''
      )

    url_df <-
      data_frame(period = periods, urlData = urls) %>%
      mutate(char = period %>% substr(stop = period %>% nchar(), start = period %>% nchar())) %>%
      left_join(data_frame(char = c('a', 'b'), day = c("01", "15"))) %>%
      left_join(data_frame(
        char = c("1", "2", "3", "4"),
        monthday = c('0331', '0630', '0930', '1231')
      )) %>%
      mutate(period = period %>% str_replace_all('\\q', '')) %>%
      suppressMessages()

    url_df <-
      url_df %>%
      mutate(
        idPeriod = period %>% substr(start = 1, stop = (period %>% nchar() - 1)),
        idPeriod = ifelse(
          monthday %>% is.na(),
          idPeriod %>% paste0(day),
          idPeriod %>% paste0(monthday)
        )
      ) %>%
      select(dateData = idPeriod, urlData) %>%
      mutate(
        dateData = dateData %>% lubridate::ymd(),
        monthData = dateData %>% lubridate::month(),
        yearData = dateData %>% lubridate::year()
      )

    return(url_df)
  }

#' SEC failed to deliver securities
#'
#' This function returns informato on equities
#' that failed to clear as reported to the National Securities Clearing Corporation
#' from  2004 onward.
#'
#' @param years years to include starting in 2004
#' @param months months to include
#' @param only_most_recent \code{TRUE} search only the most recent period
#' @param return_message \code{TRUE} return a message after data import
#' @param nest_data \code{TRUE} return nested data frame
#' @return nested \code{data_frame} or \code{data_frame} if \code{nest_data = FALSE}
#' @references \href{http://sec.gov}{The Securities and Exchange Commission}
#' @export
#' @import purrr stringr dplyr rvest formattable readr xml2
#' @importFrom curl curl_download
#' @family SEC
#' @family public company data
#' @examples
#' \dontrun{
#' sec_failed_to_deliver_securities(nest_data = TRUE)
#' sec_failed_to_deliver_securities(years = 2004:2016, nest_data = TRUE)
#' }
sec_failed_to_deliver_securities <-
  function(only_most_recent = TRUE,
           years = 2016,
           months = NULL,
           nest_data = TRUE,
           return_message = TRUE) {
    missing <-
      only_most_recent == F & (years %>% is_null())
    if (missing) {
      stop("Please enter a years or only most recent")
    }

    parse_fail_to_deliver_url_safe <-
      purrr::possibly(parse_fail_to_deliver_url, data_frame())

    urls_df <-
      get_fail_to_deliver_urls()

    if (only_most_recent) {
      urls <-
        urls_df %>%
        slice(1) %>%
        .$urlData
    }

    if (years %>% length() > 1) {
      only_most_recent <-
        FALSE
    }

    if (!only_most_recent) {
      possibly_years <-
        (urls_df$yearData %>% min()):(urls_df$yearData %>% max())

      if (years %in% possibly_years %>% sum()  == 0) {
        stop("Only possible years are " %>% paste0(paste0(years, collapse = ', ')))
      }

      url_df <-
        urls_df %>%
        filter(yearData %in% years)

      if (!months %>% is_null()) {
        url_df <-
          url_df %>%
          filter(monthData %in% months)
      }

      urls <-
        url_df %>%
        .$urlData
    }

    all_data <-
      urls %>%
      future_map_dfr(function(x) {
        parse_fail_to_deliver_url_safe(url = x, return_message = return_message)
      })

    all_data <-
      all_data %>%
      left_join(urls_df %>% select(dateData, urlData)) %>%
      suppressWarnings() %>%
      select(dateData, everything()) %>%
      suppressWarnings() %>%
      suppressMessages()

    if (return_message) {
      list(
        "You acquired ",
        all_data %>% nrow() %>% formattable::comma(digits = 0),
        ' trades that failed to deliver from ',
        all_data$dateData %>% min(),
        ' to ',
        all_data$dateData %>% max()
      ) %>%
        purrr::invoke(paste0, .) %>%
        cat(fill = T)
    }

    if (nest_data) {
      all_data <-
        all_data %>%
        nest(-dateData, .key = dataFailedToDeliverSecurities)
    }

    return(all_data)

  }


# market_structure --------------------------------------------------------

get_market_structure_url_df <-
  function() {
    page <-
      "https://www.sec.gov/opa/data/market-structure/marketstructuredownloadshtml-by_security.html" %>%
      read_html()

    slugs <-
      page %>%
      html_nodes('.medium-9 a') %>%
      html_attr('href')

    urls <-
      slugs %>%
      map_chr(function(x) {
        if (x %>% str_detect("http")) {
          return(x)
        } else {
          x %>% paste0('https://www.sec.gov', .)
        }
      })

    periods <-
      slugs %>%
      str_replace_all(
        'http://www.sec.gov/files/individual_security|http://www.sec.gov/marketstructure/data_uploads/|/files/individual_security_|.zip|individual_security|,0|\\q|\\_',
        ''
      )

    url_df <-
      data_frame(period = periods, urlData = urls) %>%
      mutate(char = period %>% substr(stop = period %>% nchar(), start = period %>% nchar())) %>%
      left_join(data_frame(
        char = c("1", "2", "3", "4"),
        monthday = c('0331', '0630', '0930', '1231')
      )) %>%
      mutate(
        idPeriod = period %>% substr(start = 1, stop = (period %>% nchar() - 1)),
        idPeriod = idPeriod %>% paste0(monthday) %>% lubridate::ymd()
      ) %>%
      suppressMessages() %>%
      select(dateData = idPeriod, urlData)

    return(url_df)
  }

parse_market_structure_url <-
  function(url = "https://www.sec.gov/files/individual_security_2016_q3.zip",
           return_message = TRUE) {
    td <-
      tempdir()
    tf <-
      tempfile(tmpdir = td, fileext = ".zip")

    url %>%
      curl::curl_download(destfile = tf)

    con <-
      unzip(tf, exdir = td)
    csv_loc <-
      con[con %>% str_detect('.csv')] %>%
      .[[1]]
    df <-
      csv_loc %>%
      read_csv() %>%
      purrr::set_names(
        c(
          "dateTrading",
          "typeSecurity",
          "idTicker",
          "rankMCAP",
          "rankTurn",
          "rankVolatility",
          "rankPrice",
          "volumeLIT",
          "volumeOrders",
          "countHidden",
          "countTradesForHidden",
          "volumeHidden",
          "volumeTradeForHidden",
          "countCancels",
          "countLitTrades",
          "countOddLots",
          "countTradesForOddLots",
          "volumeOddLot",
          "volumeTradeVolForOddLots"
        )
      ) %>%
      mutate_at(.vars = c(8, 9, 12, 13, 18, 19),
                funs(. %>% as.numeric() * 1000)) %>%
      mutate_at(
        .vars = c('typeSecurity', 'idTicker'),
        .funs = funs(. %>% str_to_upper())
      ) %>%
      mutate(urlData = url,
             dateTrading = dateTrading %>% lubridate::ymd()) %>%
      suppressMessages() %>%
      suppressWarnings()

    con %>%
      unlink()

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>%
        cat(fill = T)
    }

    return(df)
  }

#' SEC quarterly securities transaction metrics
#'
#' This function returns a variety of metrics surrounding
#' transactions for individual securities.  The quarterly data
#' starts in 2012.
#'
#' @param years years to include
#' @param months months to include
#' @param only_most_recent \code{TRUE} search only the most recent period
#' @param return_message \code{TRUE} return a message after data import
#' @param nest_data \code{TRUE} return nested data frame
#' @return nested \code{data_frame} or \code{data_frame} if \code{nest_data = FALSE}
#' @references \href{http://sec.gov}{The Securities and Exchange Commission}
#' @import purrr stringr dplyr rvest formattable readr
#' @importFrom curl curl_download
#' @export
#' @family SEC
#' @family transaction data
#' @family public company data
#' @examples
#' \dontrun{
#' sec_securities_metrics_by_exchange(only_most_recent = TRUE)
#' sec_securities_metrics_by_exchange(years = 2016)
#' }
sec_securities_metrics_by_exchange <-
  function(only_most_recent = FALSE,
           years = 2016,
           months = NULL,
           nest_data = TRUE,
           return_message = TRUE) {
    missing <-
      only_most_recent == F & (years %>% is_null())
    if (missing) {
      stop("Please enter a years or only most recent")
    }

    parse_market_structure_url_safe <-
      purrr::possibly(parse_market_structure_url, data_frame())

    urls_df <-
      get_market_structure_url_df() %>%
      mutate(
        yearData = dateData %>% lubridate::year(),
        monthData = dateData %>% lubridate::month()
      )

    if (!only_most_recent) {
      possibly_years <-
        (urls_df$yearData %>% min()):(urls_df$yearData %>% max())

      if (years %in% possibly_years %>% sum()  == 0) {
        stop("Only possible years are " %>% paste0(paste0(years, collapse = ', ')))
      }

      if (years %>% length() > 1) {
        only_most_recent <-
          FALSE
      }

      if (only_most_recent) {
        urls <-
          urls_df %>%
          slice(1) %>%
          .$urlData
      }


      url_df <-
        urls_df %>%
        filter(yearData %in% years)

      if (!months %>% is_null()) {
        url_df <-
          url_df %>%
          filter(monthData %in% months)
      }

      urls <-
        url_df %>%
        .$urlData
    }

    if (only_most_recent) {
      urls_df <-
        get_market_structure_url_df()

      urls <-
        urls_df %>% slice(1) %>% .$urlData
    }

    all_data <-
      urls %>%
      future_map_dfr(function(x) {
        parse_market_structure_url_safe(url = x, return_message = return_message)
      })

    all_data <-
      all_data %>%
      left_join(urls_df %>% select(dateData, urlData)) %>%
      suppressWarnings() %>%
      select(dateData, everything()) %>%
      suppressWarnings() %>%
      suppressMessages()

    all_data <-
      all_data %>%
      mutate_at(.vars = all_data %>% select(dplyr::matches("^count|^volume")) %>% names(),
                funs(. %>% formattable::comma(digits = 0)))

    if (return_message) {
      list(
        "You acquired ",
        all_data$volumeOrders %>% sum(na.rm = TRUE) %>% formattable::comma(digits = 0),
        ' traded shares from ',
        all_data$dateData %>% min(),
        ' to ',
        all_data$dateData %>% max()
      ) %>%
        purrr::invoke(paste0, .) %>%
        cat(fill = T)
    }

    if (nest_data) {
      all_data <-
        all_data %>%
        nest(-dateData, .key = dataQuarterlyMetrics)
    }

    return(all_data)

  }


# xbrl --------------------------------------------------------------------

# Note: The SEC website folder http://www.sec.gov/Archives/edgar/data/{cik}/{accession}/ will always contain all the files for a given submission, where {accession} is the adsh with the ‘-‘characters removed.
get_xbrl_name_df <-
  function() {
    data_frame(
      nameSEC = c(
        "adsh",
        "report",
        "line",
        "stmt",
        "inpth",
        "rfile",
        "tag",
        "version",
        "plabel",
        "negating",
        "coreg",
        "ddate",
        "qtrs",
        "uom",
        "value",
        "footnote",
        "cik",
        "name",
        "sic",
        "countryba",
        "stprba",
        "cityba",
        "zipba",
        "bas1",
        "bas2",
        "baph",
        "countryma",
        "stprma",
        "cityma",
        "zipma",
        "mas1",
        "mas2",
        "countryinc",
        "stprinc",
        "ein",
        "former",
        "changed",
        "afs",
        "wksi",
        "fye",
        "form",
        "period",
        "fy",
        "fp",
        "filed",
        "accepted",
        "prevrpt",
        "detail",
        "instance",
        "nciks",
        "aciks",
        "pubfloatusd",
        "floatdate",
        "custom",
        "abstract",
        "datatype",
        "iord",
        "crdr",
        "tlabel",
        "doc"
      ),
      nameActual = c(
        "idEDGARAccession",
        "idReport",
        "idLine",
        "idFinancialStatementLocation",
        "isValueNet",
        "idDataFileType",
        "itemTag",
        "idVersion",
        "labelPreferred",
        "isNegating",
        "nameCoRegistrant",
        "datePeriodEnd",
        "durationQuarters",
        "typeUOM",
        "valueItem",
        "descriptionFootnote",
        "idCIK",
        "nameFiler",
        "idSIC",
        "countryFilerBusiness",
        "stateprovinceFilerBusiness",
        "cityFilerBusiness",
        "zipcodeFilerBusiness",
        "addressStreet1FilerBusiness",
        "addressStreet2FilerBusiness",
        "phoneFilerBusiness",
        "countryFilerMailing",
        "stateprovinceFilerMailing",
        "cityFilerMailing",
        "zipcodeFilerMailing",
        "addressStreet1FilerMailing",
        "addressStreet2FilerMailing",
        "countryIncorporation",
        "stateprovinceIncorporation",
        "idEIN",
        "nameFilerFormer",
        "dateNameChanged",
        "idAFS",
        "isWellKnownSeasonedIssuer",
        "monthdayFiscalYearEnd",
        "idForm",
        "dateBalanceSheet",
        "yearFiscalEnd",
        "idFiscalPeriod",
        "dateFiled",
        "datetimeAccepted",
        "isAmmendedFiling",
        "hasQuantitativeFootnotes",
        "slugXBRLInstance",
        "countCIKFiler",
        "additionalCIKs",
        "amountFloatUSD",
        "dateFloatUSD",
        "isTagCustom",
        "isTagNonNumeric",
        "typeData",
        "idValueType",
        "idAccountingType",
        "labelTaxonomy",
        "descriptionItem"
      )

    )
  }

get_xbrl_url_df <-
  function() {
    page <-
      "https://www.sec.gov/dera/data/financial-statement-data-sets.html" %>%
      read_html()

    slugs <-
      page %>%
      html_nodes('.views-field-field-display-title a') %>%
      html_attr('href') %>%
      str_replace_all('http://www.sec.gov', '')


    periods <-
      slugs %>%
      str_replace_all('/files/dera/data/financial-statement-data-sets/|.zip', '')

    urls <-
      'http://www.sec.gov' %>%
      paste0(slugs)

    url_df <-
      data_frame(idPeriod = periods, urlSEC = urls) %>%
      separate(
        idPeriod,
        into = c('yearData', 'quarterData'),
        sep = '\\q',
        remove = FALSE
      ) %>%
      mutate_at(.vars = c('yearData', 'quarterData'),
                funs(. %>% as.numeric()))

    return(url_df)
  }

read_xbrl_file <-
  function(path = './tag.txt') {
    path %>%
      readr::read_tsv() %>%
      suppressWarnings() %>%
      suppressMessages()
  }

parse_xbrl_url <-
  function(url = "http://www.sec.gov/data/financial-statements/2016q3.zip",
           only_all = TRUE,
           return_message = TRUE) {
    options(scipen = 999999)

    period_data <-
      url %>% str_replace_all("http://www.sec.gov/data/financial-statements/|.zip|/files/dera/data/financial-statement-data-sets/", '')

    year_data <-
      period_data %>%
      str_split('\\q') %>%
      flatten_chr() %>%
      .[[1]] %>%
      as.numeric()

    quarter_data <-
      period_data %>%
      str_split('\\q') %>%
      flatten_chr() %>%
      .[[2]] %>%
      as.numeric()

    date_data <-
      data_frame(yearData = year_data,
                 quarterData = quarter_data) %>%
      left_join(data_frame(
        quarterData = 1:4,
        monthday = c('0331', '0630', '0930', '1231')
      )) %>%
      tidyr::unite(dateData, yearData, monthday, sep = '') %>%
      .$dateData %>% lubridate::ymd() %>%
      suppressMessages()

    read_xbrl_file_safe <-
      purrr::possibly(read_xbrl_file, data_frame())

    tmp <-
      tempfile()
    url %>%
      curl::curl_download(url = ., tmp)
    con <-
      unzip(tmp)

    name_xbrl <-
      get_xbrl_name_df()

    con <- con[con %>% str_detect("txt")]
    sub <-
      con[con %>% str_detect('sub')] %>%
      read_xbrl_file

    sub_names <-
      name_xbrl %>%
      filter(nameSEC %in% names(sub)) %>%
      .$nameActual

    sub <-
      sub %>%
      purrr::set_names(sub_names)

    sub <-
      sub %>%
      mutate_at(
        sub %>% select(dplyr::matches("^date[A-Z]")) %>% select(-dplyr::matches("datetime")) %>% names(),
        funs(. %>% as.numeric() %>% lubridate::ymd())
      ) %>%
      mutate_at(sub %>% select(dplyr::matches("^idCIK|idSIC|idEIN|^amount[A-Z]")) %>% names(),
                funs(. %>% as.numeric())) %>%
      mutate_at(sub %>% select(dplyr::matches("^is[A-Z]|^has[A-Z]")) %>% names(),
                funs(. %>% as.logical())) %>%
      mutate_at(sub %>% select(dplyr::matches("^amount")) %>% names(),
                funs(. %>% formattable::currency(digits = 0))) %>%
      suppressWarnings() %>%
      mutate(
        urlSECXBRLFiling =
          list(
            'http://www.sec.gov/Archives/edgar/data/',
            idCIK,
            '/' ,
            idEDGARAccession %>% str_replace_all('\\-', ''),
            '/',
            slugXBRLInstance
          ) %>% purrr::invoke(paste0, .),
        addressStreetFilerBusiness = ifelse(
          addressStreet2FilerBusiness %>% is.na(),
          addressStreet1FilerBusiness,
          paste(addressStreet1FilerBusiness,
                addressStreet2FilerBusiness)
        ),
        addressFilerBusiness = list(
          addressStreetFilerBusiness,
          " ",
          cityFilerBusiness,
          ", ",
          stateprovinceFilerBusiness,
          " ",
          zipcodeFilerBusiness
        ) %>% purrr::invoke(paste0, .),
        addressStreetFilerMailing = ifelse(
          addressStreet2FilerMailing %>% is.na(),
          addressStreet1FilerMailing,
          paste(addressStreet1FilerMailing,
                addressStreet2FilerMailing)
        ),
        addressFilerMailing = list(
          addressStreetFilerMailing,
          " ",
          cityFilerMailing,
          ", ",
          stateprovinceFilerMailing,
          " ",
          zipcodeFilerMailing
        ) %>% purrr::invoke(paste0, .)
      ) %>%
      left_join(data_frame(
        idAFS = c("1-LAF", "2-ACC", "3-SRA", "4-NON", "5-SML", NA),
        typeAFS = c(
          'Large Accelerated',
          'Accelerated',
          'Smaller Reporting Accelerated',
          'Non-Accelerated',
          'Smaller Reporting Filer',
          'Not Assigned'
        )
      )) %>%
      suppressWarnings() %>%
      suppressMessages()

    pre <-
      con[con %>% str_detect('pre')] %>%
      read_xbrl_file()

    pre_names <-
      name_xbrl %>%
      filter(nameSEC %in% names(pre)) %>%
      .$nameActual

    pre <-
      pre %>%
      purrr::set_names(pre_names)

    pre <-
      pre %>%
      mutate_at(
        pre %>% select(dplyr::matches("^date[A-Z]")) %>% select(-dplyr::matches("datetime")) %>% names(),
        funs(. %>% as.numeric() %>% lubridate::ymd())
      ) %>%
      mutate_at(pre %>% select(dplyr::matches("^idCIK|idSIC|idEIN|^amount[A-Z]")) %>% names(),
                funs(. %>% as.numeric())) %>%
      mutate_at(pre %>% select(dplyr::matches("^is[A-Z]|^has[A-Z]")) %>% names(),
                funs(. %>% as.logical())) %>%
      mutate_at(pre %>% select(dplyr::matches("^amount")) %>% names(),
                funs(. %>% formattable::currency(digits = 0))) %>%
      suppressWarnings() %>%
      unite(idReportLine,
            idReport,
            idLine,
            sep = ".",
            remove = FALSE) %>%
      mutate(idReportLine = idReportLine %>% as.numeric()) %>%
      left_join(
        data_frame(
          idFinancialStatementLocation = c("BS", "IS", "CF", "EQ", "CI", "UN"),
          nameFinancialStatementLocation = c(
            "Balance Sheet",
            "Income Statement",
            "Cash Flow",
            "Equity",
            "Comprehensive Income",
            "Unclassifiable Statement"
          )
        )
      ) %>%
      left_join(data_frame(
        idDataFileType = c("H", "X"),
        typeDataFile = c("HTML", "XML")
      )) %>%
      suppressMessages()

    num <-
      con[con %>% str_detect('num')] %>%
      read_xbrl_file()

    num_names <-
      name_xbrl %>%
      filter(nameSEC %in% names(num)) %>%
      .$nameActual

    num <-
      num %>%
      purrr::set_names(num_names)

    num <-
      num %>%
      mutate_at(
        num %>% select(dplyr::matches("^date[A-Z]")) %>% select(-dplyr::matches("datetime")) %>% names(),
        funs(. %>% as.numeric() %>% lubridate::ymd())
      ) %>%
      mutate_at(num %>% select(dplyr::matches("^idCIK|idSIC|idEIN|^amount[A-Z]")) %>% names(),
                funs(. %>% as.numeric())) %>%
      mutate_at(num %>% select(dplyr::matches("^is[A-Z]|^has[A-Z]")) %>% names(),
                funs(. %>% as.logical())) %>%
      mutate_at(num %>% select(dplyr::matches("^amount")) %>% names(),
                funs(. %>% formattable::currency(digits = 0))) %>%
      left_join(data_frame(
        typeUOM = c("AUD", "CAD", "CHF", "EUR", "JPY", "shares", "USD"),
        itemUOM = c(
          "amountAUD",
          "amountCAD",
          "amountCHF",
          "amountEUR",
          "amountJPY",
          "amuntShares",
          "amountUSD"
        )
      )) %>%
      select(-typeUOM) %>%
      select(idEDGARAccession:durationQuarters, itemUOM, everything()) %>%
      suppressMessages() %>%
      suppressWarnings()

    tag <-
      con[con %>% str_detect('tag')] %>%
      read_xbrl_file()

    tag_names <-
      name_xbrl %>%
      filter(nameSEC %in% names(tag)) %>%
      .$nameActual

    tag <-
      tag %>%
      purrr::set_names(tag_names)

    tag <-
      tag %>%
      mutate_at(
        tag %>% select(dplyr::matches("^date[A-Z]")) %>% select(-dplyr::matches("datetime")) %>% names(),
        funs(. %>% as.numeric() %>% lubridate::ymd())
      ) %>%
      mutate_at(tag %>% select(dplyr::matches("^idCIK|idSIC|idEIN|^amount[A-Z]")) %>% names(),
                funs(. %>% as.numeric())) %>%
      mutate_at(tag %>% select(dplyr::matches("^is[A-Z]|^has[A-Z]")) %>% names(),
                funs(. %>% as.logical())) %>%
      mutate_at(tag %>% select(dplyr::matches("^amount")) %>% names(),
                funs(. %>% formattable::currency(digits = 0))) %>%
      suppressWarnings() %>%
      left_join(data_frame(
        idValueType = c("I", "D"),
        typeValue = c("Point-in Time", "Duration")
      )) %>%
      left_join(data_frame(
        idAccountingType = c("C", "D"),
        typeAccounting = c("Credit", "Debit")
      )) %>%
      suppressMessages()
    con %>%
      unlink()
    if (only_all) {
      all <-
        sub %>%
        inner_join(num) %>%
        inner_join(tag) %>%
        left_join(pre) %>%
        select(
          idEDGARAccession,
          idCIK,
          nameFiler:typeAFS,
          idReport,
          idLine,
          idFinancialStatementLocation,
          idAccountingType,
          typeData,
          itemTag,
          labelTaxonomy,
          labelPreferred,
          everything()
        ) %>%
        distinct() %>%
        suppressWarnings() %>%
        suppressMessages()

      all_data <-
        data_frame(nameTable = c('All Data'),
                   dataTable = list(all)) %>%
        mutate(
          periodData = period_data,
          yearData = year_data,
          quarterData = quarter_data,
          urlData = url
        ) %>%
        select(periodData:urlData, nameTable, dataTable)

      return(all_data)
    }

    all_data <-
      data_frame(
        nameTable = c('Presentation', 'Values', 'Filers', 'Taxonomy'),
        dataTable = list(pre, num, sub, tag)
      ) %>%
      mutate(
        periodData = period_data,
        yearData = year_data,
        quarterData = quarter_data,
        urlData = url
      ) %>%
      select(periodData:urlData, nameTable, dataTable)
    if (return_message) {
      list("Parsed XBRL ",
           sub %>% nrow() %>% comma(digits = 0),
           " filers for ",
           period_data) %>%
        purrr::invoke(paste0, .) %>%
        cat(fill = T)
    }

    return(all_data)
  }

#' SEC quarterly XBRL data
#'
#' This function returns quarterly full \href{https://en.wikipedia.org/wiki/XBRL}{XBRL} dumps
#' for all SEC registered public companies begining in 2009.
#'
#' @param years years to include starting in 2009
#' @param quarters quarters to search \itemize{
#' \item \code{NULL}(default): all quarters,
#' \item \code{1}: Q1
#' \item \code{2}: Q2
#' \item \code{3}: Q3
#' \item \code{4}: Q4
#' }
#' @param only_most_recent \code{TRUE} search only the most recent period
#' @param return_message \code{TRUE} return a message after data import
#' @param tables underlying XBRL tables you want to include \itemize{
#' \item \code{NULL}: all tables (default)
#' \item \code{Presentation}: presentation data
#' \item \code{Values}: XBRL value data
#' \item \code{Filers}: filer data
#' \item \code{Taxonomy}: taxonomy data
#' }
#' @param only_all \code{TRUE} returns a joined file of the four tables
#' @param assign_to_environment \code{TRUE} assign the results into your environment
#'
#' @return nested \code{data_frame} or \code{data_frame}
#' @references \href{http://sec.gov}{The Securities and Exchange Commission}
#' @export
#' @import purrr stringr dplyr rvest formattable
#' @importFrom curl curl_download
#' @examples
#' @family SEC
#' @family public company data
#' @family entity search
#' @examples
#' \dontrun{
#' sec_xbrl_periods(only_most_recent = TRUE, only_all = TRUE, assign_to_environment = TRUE, return_message = TRUE)
#' }
sec_xbrl_periods <-
  function(only_most_recent = TRUE,
           years = NULL,
           quarters = NULL,
           tables = NULL,
           only_all = TRUE,
           assign_to_environment = TRUE,
           return_message = TRUE) {
    url_df <-
      get_xbrl_url_df() %>%
      arrange(desc(yearData), desc(quarterData))

    has_years <-
      (!years %>% purrr::is_null())
    if (has_years) {
      url_df <-
        url_df %>%
        filter(yearData %in% years)
    }

    has_quarter <-
      (!quarters %>% purrr::is_null())
    if (has_quarter) {
      url_df <-
        url_df %>%
        filter(quarterData %in% quarters)
    }

    if (only_most_recent) {
      url_df <-
        get_xbrl_url_df() %>%
        arrange(desc(yearData), desc(quarterData)) %>%
        slice(1)
    }

    urls <-
      url_df$urlSEC

    parse_xbrl_url_safe <-
      purrr::possibly(parse_xbrl_url, data_frame())

    all_data <-
      urls %>%
      future_map_dfr(function(x) {
        parse_xbrl_url_safe(url = x,
                            only_all = only_all,
                            return_message = return_message)
      }) %>%
      suppressWarnings()

    if (assign_to_environment) {
      table_name_df <-
        all_data %>%
        select(nameTable) %>%
        distinct() %>%
        mutate(nameDF =
                 list('xbrl', nameTable %>% str_replace_all('\\ ', '')) %>% purrr::invoke(paste0, .))

      if (!tables %>% is_null()) {
        all_data <-
          all_data %>%
          filter(nameTable %>% str_detect(tables))
      }

      1:nrow(all_data) %>%
        walk(function(x) {
          df_name <-
            table_name_df %>%
            filter(nameTable == all_data %>% slice(x) %>% .$nameTable) %>%
            .$nameDF

          df_data <-
            all_data %>%
            slice(x) %>%
            select(yearData, quarterData, periodData, dataTable) %>%
            unnest()

          df_data <-
            df_data %>%
            mutate_at(.vars =
                        df_data %>% select(dplyr::matches("^amount|^price|^value")) %>% names(),
                      funs(. %>% formattable::currency(digits = 2))) %>%
            mutate_at(
              .vars =
                df_data %>% select(dplyr::matches("^count[A-Z]")) %>% select(-dplyr::matches("country")) %>% names(),
              funs(. %>% formattable::comma(digits = 0))
            )

          assign(x = df_name, eval(df_data), envir = .GlobalEnv)

        })
    }

    if (!only_all) {
      return(all_data)
    }

  }
get_data_dallas_fed_housing_urls <-
  function() {
  page <-
    "https://www.dallasfed.org/institute/houseprice#tab2" %>%
    xml2::read_html()

  quarter_names <-
    page %>%
    html_nodes('#tab2 a:nth-child(1)') %>%
    html_text()

  urls <-
    page %>%
    html_nodes('#tab2 a:nth-child(1)') %>%
    html_attr("href") %>%
    paste0('https://www.dallasfed.org',.)

  df_quarters <-
    data_frame(
      nameQuarter = c('first', 'second', 'third', 'fourth'),
      idQuarter = 1:4,
      dateEnd = c('03-31', "06-30", "09-30", "12-31")
    )
  df <-
    quarter_names %>%
    map_df(function(x) {
      year <- x %>% readr::parse_number()
      quarter <-
        x %>% str_to_lower %>% str_split('\\ ') %>% flatten_chr() %>% .[[1]]
      data_frame(yearData = year,
                 nameQuarter = quarter) %>%
        left_join(df_quarters) %>%
        mutate(
          dateData = list(yearData, dateEnd) %>% purrr::reduce(paste0) %>% lubridate::ymd(),
          periodData = list(yearData, ".", idQuarter) %>% purrr::reduce(paste0)
        ) %>%
        select(dateData, periodData, yearData, idQuarter) %>%
        suppressMessages()
    })

  df <-
    df %>%
    mutate(urlData = urls)

  return(df)
  }

parse_housing_excel <-
  function(url = "https://www.dallasfed.org/-/media/Documents/institute/houseprice/hp1603.xlsx?la=en",
           return_message = TRUE){
    td <-
      tempdir()
    tf <-
      tempfile(tmpdir = td, fileext = ".xlsx")

    url %>%
      curl::curl_download(destfile = tf)
    sheet_names <-
      tf %>%
      readxl::excel_sheets()

    data <-
      2:length(sheet_names) %>%
      map_df(function(x){
        code_index <-
          sheet_names[[x]]

        data <-
          tf %>%
          readxl::read_excel(sheet = x, col_names = TRUE) %>%
          slice(-1)

        names(data)[[1]] <-
          'yearQuarter'

        data %>%
          gather(nameCountry, value, -yearQuarter) %>%
          mutate(nameCountry = nameCountry %>% str_to_upper()) %>%
          mutate(codeIndex = code_index) %>%
          select(codeIndex, everything())
      })

    data <-
      data %>%
      tidyr::separate(yearQuarter, c('yearData', 'idQuarter'), sep = '\\:') %>%
      mutate_at(c('yearData', 'idQuarter'),
                funs(. %>% readr::parse_number())) %>%
      mutate(periodData = list(yearData, '.', idQuarter) %>% purrr::reduce(paste0)) %>%
      select(codeIndex, periodData, everything())

    data <-
      data %>%
      left_join(data_frame(
        nameCountry = c("AGGREGATE", "S. AFRICA", "US", "S. KOREA", "UK"),
        nameCountryActual = c(
          "TOTAL",
          "South Africa",
          'United States of America',
          'South Korea',
          'United Kingdom'
        ) %>% str_to_upper()
      )) %>%
      mutate(nameCountry = ifelse(nameCountryActual %>% is.na(),nameCountry, nameCountryActual)) %>%
      select(-nameCountryActual) %>%
      left_join(
        countrycode::countrycode_data %>%
          as_data_frame() %>%
          select(nameCountry = country.name.en, idISO3c = iso3c, nameContinent = continent) %>%
          mutate(nameCountry = nameCountry %>% str_replace("Republic of Korea", "South Korea"),
                 nameCountry = nameCountry %>% str_replace("United Kingdom of Great Britain and Northern Ireland", "United Kingdom")) %>%
          mutate_all(str_to_upper)
      ) %>%
      select(codeIndex, periodData, nameContinent, idISO3c, nameCountry, everything()) %>%
      suppressMessages() %>%
      mutate(urlData = url)

    df_quarters <-
      data_frame(
        nameQuarter = c('first', 'second', 'third', 'fourth'),
        idQuarter = 1:4,
        dateEnd = c('03-31', "06-30", "09-30", "12-31")
      )

    data <- data %>%
      left_join(df_quarters) %>%
      mutate(
        dateData = list(yearData, dateEnd) %>% purrr::reduce(paste0) %>% lubridate::ymd(),
        periodData = list(yearData, ".", idQuarter) %>% purrr::reduce(paste0)
      ) %>%
      select(codeIndex, dateData, periodData, yearData, idQuarter, everything()) %>%
      suppressMessages() %>%
      select(-c(nameQuarter, dateEnd))
    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% message()
    }
    unlink(tf)
    unlink(td)
    return(data)
  }

#' Dallas Federal Reserve International House Price Database
#'
#' This retrieves data for the international house
#' price database maintained by the Dallas FED
#'
#' @param nest_data
#' @param return_message
#'
#' @return
#' @export
#' @import countrycode readxl stringr dplyr purrr dplyr lubridate tidyr
#' @examples
get_data_dallas_fed_international_housing <-
  function(indicies = NULL ,
           nest_data = FALSE, return_message = TRUE) {

    df_url <-
      get_data_dallas_fed_housing_urls() %>%
      slice(1)

    parse_housing_excel_safe <-
      purrr::possibly(parse_housing_excel, data_frame())

    data <-
      df_url$urlData %>%
      map_df(function(x){
        parse_housing_excel_safe(url = x, return_message = return_message)
      })

    df_index <-
      data_frame(codeIndex = c("HPI", "RHPI", "PDI", "RPDI"),
                 nameIndex = c("house price index", "real house price index", "personal disposable income index", "real personal disposable income index") %>% str_to_upper())

    data <-
      data %>%
      left_join(df_index) %>%
      select(nameIndex, everything()) %>%
      suppressMessages() %>%
      suppressWarnings()

    spec_ind <-
      !indicies %>% purrr::is_null()
    if (spec_ind) {
      indicies <-
        indicies %>% str_to_upper()

      if (indicies %>% str_count(df_index$codeIndex) %>% sum(na.rm = TRUE) == 0) {
        stop(list("Sorry indicies can only be ", paste0(df_index$codeIndex, collapse = '\n')) %>% purrr::reduce(paste0))
      }
      data <-
        data %>%
        filter(codeIndex %in% indicies)
    }

    if (nest_data) {
      data <-
        data %>%
        nest(-c(codeIndex, nameIndex), .key = dataHousing)
    }
    return(data)
  }
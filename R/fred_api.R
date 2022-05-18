


# utilities ---------------------------------------------------------------

import_rda_file <-
  function(file = NULL,
           return_tibble = TRUE) {
    if (file %>% purrr::is_null()) {
      stop("Please enter a file path")
    }

    env <- new.env()
    nm <- load(file, env)[1]
    if (return_tibble) {
      data <-
        env[[nm]] %>%
        dplyr::as_tibble()
    } else {
      data <-
        env[[nm]]
    }
    return(data)
  }


curl_url <-
  function(url = "https://github.com/abresler/FRED_Dictionaries/blob/master/data/fred_series_data.rda?raw=true",
           return_tibble = TRUE) {
    con <-
      url %>%
      curl::curl()

    data <-
      con %>%
      import_rda_file(return_tibble = return_tibble)
    close(con)
    return(data)
  }

#' read RDA file
#'
#' @param file
#' @param return_tibble
#' @return
#' @export
#' @import dplyr stringr purrr
#' @importFrom curl curl_download
#' @importFrom curl curl
#' @examples
read_rda_file <-
  function(file = "https://github.com/abresler/FRED_Dictionaries/blob/master/data/fred_series_data.rda?raw=true",
           return_tibble = TRUE) {
    is_html <-
      file %>% stringr::str_detect("http")

    if (is_html) {
      data <- curl_url(url = file, return_tibble = return_tibble)
    } else {
      data <-
        import_rda_file(file = file, return_tibble = return_tibble)
    }
    return(data)
  }



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
        tibble(countItemPage = x, nameAttribute = attribute)
    } else {
      df_att <-
        tibble(countItemPage = x)
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
        tibble(
          countItemPage = x,
          nameItem = periods,
          idSeriesDetailed = series_ids,
          dateUpdated = dates
        ) %>%
        mutate(countItem = 1:n())

      df_series <-
        df_series %>%
        nest(-countItemPage, .key = dataSeries)

      df_series <-
        df_series %>%
        mutate(hasDetailedSeries = !dataSeries %>% map_lgl(is.null))
    } else {
      df_series <-
        tibble(countItemPage = x)
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
        tibble(countItemPage = x, nameTag = tags) %>%
        mutate(countItem = 1:n())

      df_tags <-
        df_tags %>%
        nest(-countItemPage, .key = dataTags)
    } else {
      df_tags <-
        tibble(countItemPage = x)
    }
    df <-
      df_att %>%
      left_join(df_series, by = "countItemPage") %>%
      left_join(df_tags,by = "countItemPage")

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
      list("https://fred.stlouisfed.org/tags/series?t=&et=&pageID=",
           fred_pages,
           '&t=') %>%
      purrr::reduce(paste0)

    tibble(numberPage = fred_pages, urlPage = urls)
  }

.parse_fred_ft_html <-
  function(url = "https://fred.stlouisfed.org/search?st=China",
           page_no = 1,
           return_message = T) {
    page <-
      url %>%
      read_html()

    item_count <-
      page %>%
      html_nodes('.series-title') %>%
      length()

    df_css_nodes <-
      tibble(idRow = 1:item_count,
                 nodeNumber = c(2, seq(
                   5, by = 3, length.out = item_count - 1
                 )))

    df_items <-
      1:item_count %>%
      future_map_dfr(function(x) {
        table_no <-
          df_css_nodes %>%
          filter(idRow == x) %>%
          .$nodeNumber

        table_node_css <-
          glue::glue(".series-pager-attr:nth-child({table_no}) td")

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
          tibble(
            countItemPage = x,
            idSeries = series_id,
            nameSeries = series_name
          ) %>%
          mutate(
            urlFREDAPI = glue::glue(
              "https://fred.stlouisfed.org/graph/graph-data.php?id={idSeries}"
            ) %>% as.character()
          ) %>%
          left_join(df_table, by = "countItemPage")
        table_df
      }) %>%
      mutate_if(is.character, str_squish)

    df_items <-
      df_items %>%
      mutate(urlPage = url,
             numberPage = page_no) %>%
      select(numberPage, everything())

    if (return_message) {
      glue::glue("Parsed {url}") %>% cat(fill = T)
    }

    df_items <-
      df_items %>%
      mutate(isDiscontinued = nameSeries %>% str_detect("DISCONTINUED")) %>%
      select(numberPage, countItemPage, isDiscontinued, everything())

    df_items
  }

.parse_fred_ft_slug_count <-
  function(url = "https://fred.stlouisfed.org/search?st=China&pageID=1") {
    page <-
      url %>%
      read_html()

    base_url <-
      url %>% str_remove_all("&pageID=1")

    no_extra <-
      page %>%
      html_nodes('.ph20 p a') %>%
      html_text() %>%
      readr::parse_number() %>%
      discard(is.na) %>% length() == 0

    if (no_extra) {
      df <- tibble(numberPage = 1, urlPage = as.character(url))
      return(df)
    }

    last_page <-
      page %>%
      html_nodes('.ph20 p a') %>%
      html_text() %>%
      readr::parse_number() %>%
      discard(is.na) %>%
      max() %>%
      suppressWarnings()

    fred_pages <-
      seq(1, last_page)

    urls <-
      glue::glue("{base_url}&pageID={fred_pages}") %>% as.character()

    tibble(numberPage = fred_pages, urlPage = as.character(urls))

  }

.parse_fred_html_search_term <-
  function(search_term = "China Debt",
           return_message = TRUE) {
    if (return_message) {
      glue::glue("Searching for for {search_term}\n") %>% cat(fill = T)
    }
    term_slug <- search_term %>% URLencode()
    search_url <-
      glue::glue("https://fred.stlouisfed.org/search?st={term_slug}&pageID=1")

    df_urls <-
      search_url %>% .parse_fred_ft_slug_count() %>%
      mutate(termSearch = search_term) %>%
      select(termSearch, everything())

    .parse_fred_ft_html_safe <-
      purrr::possibly(.parse_fred_ft_html, tibble())
    df_data <-
      1:nrow(df_urls) %>%
      future_map_dfr(function(x) {
        .parse_fred_ft_html(
          url = df_urls$urlPage[[x]],
          page_no = df_urls$numberPage[[x]],
          return_message = return_message
        )
      })

    all_data <-
      df_data %>%
      left_join(df_urls,by = c("numberPage", "urlPage")) %>%
      select(termSearch, numberPage, everything())

    if (return_message) {
      glue::glue("Found {nrow(all_data)} FRED series for {search_term}") %>% cat(fill = T)
    }

    all_data
  }

.fred_terms_ids_html <-
  function(search_terms = c("China Debt", "China Investment"),
           return_message = TRUE) {
    .parse_fred_html_search_term_safe <-
      purrr::possibly(.parse_fred_html_search_term, tibble())

    all_data <-
      search_terms %>%
      future_map_dfr(function(search_term) {
        .parse_fred_html_search_term(search_term = search_term,
                                     return_message = return_message)
      })

    all_data <-
      all_data %>%
      group_by(idSeries) %>%
      mutate(idRow = 1:n()) %>%
      ungroup() %>%
      filter(idRow == min(idRow)) %>%
      select(-idRow)

    all_data
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
      tibble(idRow = 1:item_count,
                 nodeNumber = c(2, seq(
                   5, by = 3, length.out = item_count - 1
                 )))
    df_items <-
      1:item_count %>%
      future_map_dfr(function(x) {
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
          tibble(
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
      mutate(urlPage = as.character(url),
             idPage = no_page) %>%
      select(idPage, everything())

    if (return_message) {
      glue::glue("Parsed {url}") %>% cat(fill = T)
    }
    df_items
  }

.all_fred_series_ids <-
  function(return_message = TRUE,
           nest_data = TRUE,
           sleep = NULL) {
    parse_fred_page_safe <-
      purrr::possibly(parse_fred_page, tibble())

    urls <-
      get_fred_page_count() %>%
      .$urlPage

    df_fred_ids <-
      urls %>%
      future_map_dfr(function(x) {
        parse_fred_page_safe(url = x, return_message = return_message)
      })

    df_fred_ids <-
      df_fred_ids %>%
      mutate(idItem = 1:n()) %>%
      select(idItem, everything())

    return(df_fred_ids)
  }

.curl_url <-
  function(url = "https://github.com/abresler/FRED_Dictionaries/blob/master/data/fred_series_data.rda?raw=true") {
    con <-
      url %>%
      curl::curl()

    data <-
      con %>%
      import_rda_file()
    close(con)
    return(data)
  }

#' FRED Series IDs
#'
#' This function imports dictionaries for all possible FRED
#' series.  You can us this data to find series to search in the
#' \code{\link{fred_symbols}} function.
#'
#' This dictionary was indexed on January 22, 2017, please be aware
#' that there may be new series added to FRED since then.
#'
#' @param fred_file the type of dictionary you wish to import \itemize{
#' \item \code{NULL, All}:  returns all dictionary data in a nested data frame (default)
#' \item \code{series}: returns a data frame of the series name, id, and attributes
#' \item \code{tags}: returns a data frame of the series id and their corresponding search tag
#' \item \code{subindicies}: returns a data frame with the parent series and any sub-indicies if they have them
#' }
#' @param return_message \code{TRUE} return a message after data import
#'
#' @return a \code{tibble}
#' @export
#' @family FRED
#' @family dictionary
#'
#' @examples
#'
#' dictionary_fred_ids(fred_file = NULL, return_message  = TRUE)
dictionary_fred_ids <-
  function(fred_file = NULL,
           return_message = TRUE) {
    fred_files <-
      c('all',  'series', 'tags', 'subindicies')

    if (!fred_file %>% purrr::is_null()) {
      fred_file <-
        fred_file %>%
        str_to_lower()

      if (!fred_file %in% fred_files) {
        stop(list(
          "FRED file names can only be\n",
          paste0(fred_files, collapse = '\n')
        ) %>% purrr::reduce(paste0))
      }
    }

    if (fred_file %>% purrr::is_null()) {
      data <-
        "https://github.com/abresler/FRED_Dictionaries/blob/master/data/fred_series_data.rda?raw=true" %>%
        read_rda_file()

      if (return_message) {
        message("Imported all FRED series data")
      }

      return(data)
    }

    if (fred_file == 'all') {
      data <-
        "https://github.com/abresler/FRED_Dictionaries/blob/master/data/fred_series_data.rda?raw=true" %>%
        read_rda_file()

      if (return_message) {
        message("Imported all FRED series data")
      }

      return(data)
    }

    if (fred_file == 'series') {
      data <-
        "https://github.com/abresler/FRED_Dictionaries/blob/master/data/fred_series_dictionary.rda?raw=true" %>%
        read_rda_file()
      if (return_message) {
        message("Imported all FRED series IDs")
      }
      return(data)
    }

    if (fred_file == 'tags') {
      data <-
        "https://github.com/abresler/FRED_Dictionaries/blob/master/data/fred_series_tags.rda?raw=true" %>%
        read_rda_file()
      if (return_message) {
        message("Imported all FRED series tags")
      }
      return(data)
    }

    if (fred_file == 'subindicies') {
      data <-
        "https://github.com/abresler/FRED_Dictionaries/blob/master/data/fred_series_subindicies.rda?raw=true" %>%
        read_rda_file()
      if (return_message) {
        message("Imported all FRED series subindicies")
      }
      return(data)
    }
  }

.parse_fred_search <-
  function(urls, return_message = TRUE) {
    df <-
      tibble()
    success <- function(res) {
      works <- res$status_code == 200
      if (works) {
        json_data <-
          res$url %>%
          jsonlite::fromJSON(simplifyDataFrame = TRUE)

        search_term <-
          res$url %>%
          str_split(pattern = '\\q=') %>%
          flatten() %>%
          .[[2]] %>%
          URLdecode()

        data <-
          json_data %>%
          as_tibble() %>%
          purrr::set_names(
            c(
              "periodDataStart",
              "periodDataEnd",
              "periodUpdated",
              "descriptionData",
              "nameSeries",
              "frequencyDataReporting",
              "dataUnitMeasure",
              "descriptionSeasonality",
              "idSeries",
              "rankPopularity"
            )
          ) %>%
          mutate(termSearch = search_term) %>%
          select(termSearch,
                 idSeries,
                 nameSeries,
                 dataUnitMeasure,
                 everything())

        if (return_message) {
          glue::glue("Found {nrow(data)} FRED series for {search_term}") %>% cat(fill = T)

        }
        rm(res)


        df <<-
          df %>%
          bind_rows(data)
      }
    }
    failure <- function(msg) {
      cat(sprintf("Fail: %s (%s)\n", res$url, msg))
    }
    urls %>%
      future_map(function(x) {
        curl_fetch_multi(url = x, success, failure)
      })
    multi_run()

    df
  }

.fred_terms_ids_json <-
  function(search_terms = c("China", "Property"),
           return_message = TRUE) {
    terms <-
      search_terms %>% map_chr(URLencode)

    json_urls <-
      glue::glue(
        "https://fred.stlouisfed.org/graph/ajax-requests.php?action=find_series&q={terms}"
      ) %>% as.character()

    all_data <-
      json_urls %>%
      .parse_fred_search(return_message = return_message)


    char_names <- all_data %>% select_if(is.character) %>% select(-matches("period|url|term")) %>% names()

    all_data <- all_data %>%
      mutate_at(char_names, str_to_upper)

    all_data
  }


#' FRED Terms Series IDs
#'
#' This function returns
#' matching FRED series ID for a given
#' search term.  When using
#' `use_json_api` search results limited to 100,
#'
#' @param search_terms vector of search terms
#' @param use_json_api if \code{TRUE} uses faster json API but results limted to 100
#' default \code{FALSE}
#' @param return_message if \code{TRUE} returns message
#'
#' @return a \code{tibble}
#' @export
#' @import dplyr lubridate tidyr purrr jsonlite curl rvest glue stringr
#' @examples
#' fred_terms_ids(search_terms = c("China Debt", "China Housing"))
fred_terms_ids <-
  function(search_terms = NULL,
           use_json_api = TRUE,
           snake_names = F,
           return_message = TRUE) {
    if (purrr::is_null(search_terms)) {
      stop("Please enter search terms")
    }

    if (use_json_api) {
      .fred_terms_ids_json_safe <-
        purrr::possibly(.fred_terms_ids_json, tibble())
      all_data <-
        .fred_terms_ids_json_safe(search_terms = search_terms, return_message = return_message) %>%
        mutate_if(is.character, str_squish)

      if (snake_names) {
        all_data <- janitor::clean_names(dat = all_data)
      }
      return(all_data)

    }

    .fred_terms_ids_html_safe <-
      purrr::possibly(.fred_terms_ids_html, tibble())

    all_data <-
      .fred_terms_ids_html_safe(search_terms = search_terms, return_message = return_message) %>%
      mutate_if(is.character, str_squish)

    if (snake_names) {
      all_data <- janitor::clean_names(dat = all_data)
    }

    all_data
  }

.get_fred_tag <-
  function(tag = "interest rate",
           return_message = TRUE) {
    if (return_message) {
      glue::glue("Searching for for tag {tag}\n") %>% cat(fill = T)
    }
    term_slug <- tag %>% URLencode()
    search_url <-
      glue::glue("https://fred.stlouisfed.org/tags/series?t={term_slug}&pageID=1")

    df_urls <-
      search_url %>% .parse_fred_ft_slug_count() %>% suppressWarnings() %>%
      mutate(tagSearch = tag) %>%
      select(tagSearch, everything())

    .parse_fred_ft_html_safe <-
      purrr::possibly(.parse_fred_ft_html, tibble())
    df_data <-
      1:nrow(df_urls) %>%
      future_map_dfr(function(x) {
        .parse_fred_ft_html(
          url = df_urls$urlPage[[x]],
          page_no = df_urls$numberPage[[x]],
          return_message = return_message
        )
      })

    all_data <-
      df_data %>%
      left_join(df_urls) %>%
      select(tagSearch, numberPage, everything()) %>%
      suppressMessages()

    if (return_message) {
      glue::glue("Found {nrow(all_data)} FRED series for {tag}") %>% cat(fill = T)
    }

    all_data
  }

#' FRED tag search
#'
#' @param tags vector of tags
#' @param return_message if \code{TRUE} returns message
#' @param nest_data if \code{TRUE} nests data
#'
#' @return \code{tibble}
#' @export
#' @import dplyr purrr glue rvest curl stringr tidyr
#' @examples
#' fred_tags(tags = c("spread", "swaps"))
fred_tags <-
  function(tags = NULL,
           return_message = T,
           nest_data = F) {

    if (tags %>% purrr::is_null()) {
      stop("Please Enter Tag")
    }
    .get_fred_tag_safe <-
      purrr::possibly(.get_fred_tag, tibble())

    all_data <-
      tags %>%
      future_map_dfr(function(tag){
        .get_fred_tag_safe(tag = tag, return_message = return_message)
      })

    all_data <-
      all_data %>%
      group_by(idSeries) %>%
      mutate(id = row_number()) %>%
      ungroup() %>%
      filter(id == min(id)) %>%
      select(-id)

    if (nest_data) {
      all_data <-
        all_data %>%
        nest(-c(tagSearch), .key = dataTag) %>%
        mutate(countSeriesTag = dataTag %>% map_dbl(nrow))
    }

    all_data
  }


# muge --------------------------------------------------------------------

.munge_fred_data <-
  function(data) {

    if (nrow(data) <= 1) {
      return(tibble())
    }
    obs <- nrow(data)
    date_first <-
      data$dateData %>% min()
    date_max <- data$dateData %>% max()
    value_first <-
      data$value[[1]]

    value_peak <-
      data %>% filter(value == max(value)) %>% pull(value) %>% unique() %>% .[[1]]

    date_peak <- data %>% filter(value == max(value)) %>% pull(dateData) %>% unique() %>% .[[1]]

    value_trough <-
      data %>% filter(value == min(value)) %>% pull(value) %>% unique() %>% .[[1]]

    date_trough <-
      data %>% filter(value == min(value)) %>% pull(dateData) %>% unique() %>% .[[1]]

    value_last <-
      data$value[nrow(data)]

    value_change <-
      value_last -  value_first

    calculate_irr_periods_safe <-
      purrr::possibly(calculate_irr_periods, tibble())

    df_irr <-
      calculate_irr_periods_safe(
        dates = c(date_first, date_max),
        cash_flows = c(value_first, -value_last),
        return_percentage = F,
        return_message = F
      )

    if (df_irr %>% nrow() > 0) {
      df_irr <-
        df_irr %>%
        select(
          dateFirst = dateStart,
          dateLast = dateEnd,
          pctIRRIndex = pctIRR,
          ratioIndex = multipleCapital
        ) %>%
        mutate(pctIRRIndex = pctIRRIndex * 100,
               ratioIndex = ratioIndex %>% as.numeric())

      df_irr <- df_irr %>%
        mutate(
          countObservations = obs,
          valueInitial = value_first,
          valueRecent = value_last,
          isIncrease = value_last > value_first,
          valueChange = value_change,
          datePeak = date_peak,
          valuePeak = value_peak,
          dateTrough = date_trough,
          valueTrough = value_trough,
          ratioPeak = valueRecent / valuePeak,
          ratioTrough = valueRecent / valueTrough
        )
    } else {
     df_irr <-
       tibble(
         countObservations = obs,
         valueInitial = value_first,
         valueRecent = value_last,
         isIncrease = value_last > value_first,
         valueChange = value_change,
         datePeak = date_peak,
         valuePeak = value_peak,
         dateTrough = date_trough,
         valueTrough = value_trough,
         ratioPeak = valueRecent / valuePeak,
         ratioTrough = valueRecent / valueTrough
       )
    }

    df_irr
  }


# api ---------------------------------------------------------------------
.generate_fred_symbol_url <-
  function(symbol = c('DGS10'),
           transformation = NULL) {
    df_transforms <-
      tibble(
        nameTransformation = c(
          'default',
          'change',
          'change prior year',
          'percent change',
          'percent change prior year',
          'compounded rate of change',
          'continiously compounded rate of change',
          'continiously compounded annual rate of change',
          'natural log',
          'index'
        ),
        idTransformation = c(
          'lin',
          'chg',
          'ch1',
          'pch',
          'pc1',
          'pca',
          'cch',
          'cca',
          'log',
          'nbd'
        )
      )

    if (transformation %>% purrr::is_null()) {
      slug_transformation <-
        ''
    }

    if (!transformation %>% purrr::is_null()) {
      search_transformation <-
        transformation %>%
        str_to_lower()

      if (search_transformation %in% df_transforms$nameTransformation %>% sum(na.rm = TRUE) == 0) {
        slug_transformation <-
          ''
      }

      slug_transformation <-
        df_transforms %>%
        filter(nameTransformation %in% search_transformation) %>%
        .$idTransformation
    }
    urls_json <-
      list(
        "https://fred.stlouisfed.org/graph/graph-data.php?id=",
        symbol,
        '&transformation=',
        slug_transformation
      ) %>%
      purrr::reduce(paste0)
    return(urls_json)
  }

.parse_json_fred <-
  function(url = "https://fred.stlouisfed.org/graph/graph-data.php?id=DGS10&transformation=",
           convert_date_time = TRUE,
           return_message = TRUE) {
    json_data <-
      url %>%
      jsonlite::fromJSON(simplifyDataFrame = TRUE)

    symbol <-
      url %>%
      str_split('id=') %>%
      flatten_chr() %>%
      .[[2]] %>%
      str_split('\\&') %>%
      flatten_chr() %>%
      .[[1]]

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

    source <-
      json_data$chart$labels$subtitle %>%
      str_replace_all("Source: ", "")

    name_value <-
      json_data$chart$labels$left_axis

    df_data <-
      json_data$series$obs[[1]] %>%
      data.frame(stringsAsFactors = F) %>%
      as_tibble() %>%
      purrr::set_names(c('datetimeData', 'value'))

    df_data <-
      df_data %>%
      mutate(
        idSymbol = symbol,
        nameSeries = series_name,
        nameSource = source,
        nameTransformation = name_value,
        typeValue = series_type,
        frequencyData = frequency_data,
        urlData = url
      ) %>%
      select(idSymbol, nameSeries, nameTransformation, everything()) %>%
      mutate(
        datetimeData = as.POSIXct(datetimeData / 1000, origin = "1970-01-01", tz = "UTC"),
        dateData = datetimeData %>% as.Date()
      ) %>%
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
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>%
        cat(fill = T)
    }
    return(df_data)
  }


.fred_symbol <-
  function(symbol = 'DGS2',
           transformation = NULL,
           convert_date_time = TRUE,
           include_metadata = TRUE,
           widen_data = F,
           return_message = TRUE) {
    if (symbol %>% purrr::is_null()) {
      stop("Please enter a FRED series ID")
    }
    url <-
      .generate_fred_symbol_url(symbol = symbol, transformation = transformation)

    .parse_json_fred_safe <-
      purrr::possibly(.parse_json_fred, tibble())

    data <-
      .parse_json_fred_safe(
        url = url,
        convert_date_time = convert_date_time,
        return_message = return_message
      )




    if (data %>% nrow() == 0) {
      stop(list("No data for ", symbol) %>% purrr::reduce(paste0),
           call. = TRUE)
    }

    data <-
      data %>%
      filter(!is.na(value)) %>%
      mutate(
        urlSeries = glue::glue("https://fred.stlouisfed.org/series/{symbol}") %>% as.character()
      )

    df_data <-
      data %>%
      select(dateData, value)

    char_cols <- data %>% select_if(is.character) %>% select(-matches("url")) %>% names()

    data <- data %>%
      mutate_at(char_cols, list(function(x) {
        x %>% str_to_upper() %>% str_squish()
      }))

    df_meta <-
      data %>%
      select(-one_of("dateData", "value")) %>%
      distinct(
      )

    if (include_metadata) {
      df_m <-
        .munge_fred_data(data = df_data) %>%
        mutate(idSymbol = symbol)
      df_meta <-
        df_meta %>%
        left_join(df_m, by = "idSymbol")
    }

    if (widen_data) {
      data <-
        data %>%
        spread(idSymbol, value)
    }


    data <-
      df_meta %>%
      mutate(dataFRED = list(df_data))

    if (return_message) {
      list(
        '\nReturned ',
        df_data %>% nrow() %>% formattable::comma(digits = 0),
        ' values for ',
        data$nameSeries %>% unique(),
        ' from ',
        df_data$dateData %>% min(na.rm = T),
        ' to ',
        df_data$dateData %>% max(na.rm = T)
      ) %>% purrr::reduce(paste0) %>% cat(fill = T)
    }


    data
  }

#'  Federal Reserve Economic Data time series tibble
#'
#'
#' This function returns a data for specified series from
#'  \href{https://fred.stlouisfed.org/}{FRED}.
#'
#' @param convert_date_time converts date from datetime to date form
#' @param symbols fred symbols to search, see \href{https://fred.stlouisfed.org/tags/series}{FRED symbols} for options
#' @param transformations transformations
#' @param return_message if \code{TRUE} return message
#' @param nest_data \code{TRUE} return nested data frame
#' @param include_metadata if \code{TRUE} returns meta data from time series
#' @param widen_data if \code{TRUE} widens data
#'
#' @return a \code{tibble}
#' @export
#' @import dplyr lubridate tidyr purrr jsonlite
#' @family index values
#' @examples
#' fred_symbols(symbols = c('DGS30','DGS10','DGS2'),
#' return_wide = TRUE, nest_data = FALSE)
#'
#' fred_symbols(
#' symbols = c("CPIAUCSL", "A191RL1Q225SBEA", "UNRATE"),
#' convert_date_time = TRUE,
#' nest_data = TRUE,
#' include_metadata = TRUE
#' )
#'
fred_symbols <-
  function(symbols = c('DGS2', "DGS10", "DGS30"),
           transformations = c("default"),
           widen_data = F,
           convert_date_time = TRUE,
           nest_data = TRUE,
           include_metadata = TRUE,
           return_message = TRUE) {
    df_options <-
      expand.grid(symbol = symbols,
                  transform = transformations,
                  stringsAsFactors = FALSE) %>%
      as_tibble()

    fred_symbol_safe <-
      purrr::possibly(.fred_symbol, tibble())

    all_data <-
      1:nrow(df_options) %>%
      future_map_dfr(function(x) {
        fred_symbol_safe(
          symbol = df_options$symbol[[x]],
          transformation = df_options$transform[[x]],
          convert_date_time = convert_date_time,
          include_metadata = include_metadata,
          widen_data = widen_data,
          return_message = return_message
        )
      })

    if (all_data %>% hasName("dataFRED")) {
      all_data <- all_data %>%
        mutate(countObservations = dataFRED %>% map_dbl(nrow))
    }



    if (!nest_data) {
      all_data <-
        all_data %>%
        unnest()
    }


    all_data

  }


# description -------------------------------------------------------------

.parse_fred_description_url <-
  function(url = "https://fred.stlouisfed.org/series/A756RA3A086NBEA", include_tags = F) {
    page <-
    url %>%
    read_html()

  text_citation <-
    page %>%
    html_nodes(".citation") %>%
    html_text() %>%
    str_trim() %>%
    str_split(", \n ") %>%
    flatten_chr() %>%
    str_trim() %>%
    str_split("\n|\\;") %>%
    flatten_chr() %>%
    str_trim() %>%
    purrr::discard(function(x){ x %in% c("", url)}) %>%
    str_c(collapse =  " -- ")

  links <-
    page %>% html_nodes("strong+ a")

  sources <- links %>% html_text() %>% str_remove_all("\n")

  urls <- links %>% html_attr('href')

  items <- c("nameSource", "nameRelease")

  data <-
    tibble(item = items[seq_along(sources)], value = sources) %>%
    tidyr::spread(item, value) %>%
    select(one_of(items))

  items <- c("urlSource", "urlRelease")

  df_urls <-
    tibble(item = items[seq_along(urls)], value = urls) %>%
    tidyr::spread(item, value) %>%
    select(one_of(items))

  data <-
    data %>%
    bind_cols(df_urls) %>%
    mutate(descriptionSeries = text_citation,
           urlSeries = url)
  if (include_tags) {
  categories <-
    page %>%
    html_nodes(".fg-related-category-link-gtm") %>% html_text() %>% str_trim()

  df_cat <-
    tibble(nameCategory = categories) %>%
    mutate(numberCategory = 1:n()) %>%
    select(numberCategory, everything())

  tags <-
    page %>%
    html_nodes(".fg-tag-lnk-gtm") %>% html_text() %>% str_trim()

  df_tags <-
    tibble(nameTag = tags) %>%
    mutate(numberTag = 1:n()) %>%
    select(numberTag, everything())

  data <-
    data %>%
    mutate(dataTags = list(df_tags),
           dataCategories = list(df_cat))

  }

  data
  }

.parse_fred_description_urls <-
  function(urls = c(
    "https://fred.stlouisfed.org/series/PECILBU18PA42011A647NCEN",
    "https://fred.stlouisfed.org/series/A862RS2Q224SBEA",
    "https://fred.stlouisfed.org/series/SMU34350842023600001A"
  ),
  include_tags = F,
  return_message = TRUE) {
    df <-
      tibble()

    success <- function(res) {
      url <-
        res$url

      if (return_message) {
        glue::glue("Parsing {url}") %>%
          cat(fill = T)
      }
      .parse_fred_description_url_safe <-
        purrr::possibly(.parse_fred_description_url, tibble())

      all_data <-
        .parse_fred_description_url_safe(url = url, include_tags = include_tags)


      df <<-
        df %>%
        bind_rows(all_data)
    }
    failure <- function(msg) {
      tibble()
    }
    urls %>%
      future_map(function(x) {
        curl_fetch_multi(url = x, success, failure)
      })
    multi_run()
    df <-
      df %>%
      mutate_if(is.character, str_squish)

    df
  }

#' FRED Symbols descriptions
#'
#' @param symbols vector of symbols
#' @param include_tags if \code{TRUE} returns symbols
#' @param return_message if \code{TRUE} returns message
#'
#' @return a \code{tibble}
#' @export
#'
#' @examples
#' fred_symbols_descriptions(symbols = c("DGS2", "DGS10"))
fred_symbols_descriptions <-
  function(symbols = c("DGS2", "DGS10"),
           include_tags = FALSE,
           return_message = TRUE) {
    df_urls <-
      tibble(idSymbol = symbols,
               urlSeries = glue::glue("https://fred.stlouisfed.org/series/{idSymbol}") %>% as.character())

    data <-
      .parse_fred_description_urls(urls = df_urls$urlSeries, return_message = return_message,
                                   include_tags = include_tags)


    if (data %>% hasName("urlSeries")) {
      data <-
        df_urls %>%
        left_join(data , by = "urlSeries")
    }

    data
  }
# plot --------------------------------------------------------------------

plot_time_series <-
  function(data,
           date_start = NULL,
           date_end = NULL,
           fred_data_transformation = NULL,
           transformations = c('mean', 'median', 'smooth'),
           plot_labels = FALSE,
           use_hrbr_theme = FALSE,
           interactive = FALSE) {
    transformation_options <- c('mean', 'median', 'smooth')

    wrong_transforms <-
      transformations %>%
      purrr::map_dbl(function(x) {
        transformation_options %>% grep(x, .) %>% length()
      }) %>%
      sum(na.rm = TRUE) == 0


    if (wrong_transforms) {
      stop("Transformations can only be mean, median, or smooth")
    }

    data <- data %>%
      unnest_legacy()

    if (length(date_start) > 0) {
      data <-
        data %>%
        filter(dateData >= date_start)
    }

    if (length(date_end) > 0) {
      data <-
        data %>%
        filter(dateData <= date_end)
    }

    series_name <-
      data$nameSeries %>% unique()

    split_series <-
      series_name %>% nchar() > 50

    if (split_series) {
      if (series_name %>% nchar > 50) {
        title <-
          series_name %>% str_split_fixed(pattern = '\\ ', 5) %>% as.character() %>% {
            paste0(paste0(.[1:4], collapse = ' '), '\n', .[5])
          }
      }
    } else {
      title <- series_name
    }

    sub_title <-
      list(
        'Data from ',
        data$dateData %>% min(na.rm = TRUE),
        ' to ',
        data$dateData %>% max(na.rm = TRUE),
        ' - FRED Series ID: ',
        data$idSymbol %>% unique()
      ) %>% purrr::reduce(paste0)

    type <-
      data$typeValue %>% unique()

    is_percent <- type %>% stringr::str_detect('PERCENT')

    if (length(fred_data_transformation) > 0) {
      sub_title <-
        list(sub_title,
          '\nFRED Transformation of ',
          fred_data_transformation) %>%
        purrr::reduce(paste0)
    }

    if ('nameSource' %in% names(data)) {
      caption_text <-
        list("Source data from ",
          data$nameSource %>% unique,
          '\n',
          'via FRED from fundManageR') %>%
        purrr::reduce(paste0)
    } else {
      caption_text <-
        "Sourced from FRED via fundManageR"
    }

    plot <-
      data %>%
      ggplot(aes(x = dateData, y = value)) +
      theme_minimal() +
      geom_line(color = "#00B0F0", size = .5) +
      geom_area(fill = "#00B0F0",
                alpha = 0.25,
                color = NA) +
      theme(
        panel.background = element_rect(fill = "#fffff8", color = NA),
        plot.background = element_rect(fill = "#fffff8", color = NA)
      ) +
      labs(
        x = NULL,
        y = type,
        title = title,
        subtitle = sub_title,
        caption = caption_text
      ) +
      scale_x_date(expand = c(0, 0))

    if (use_hrbr_theme) {
      check_for_hrb()
      plot <-
        plot +
        hrbrthemes::theme_ipsum_rc(grid = "XY")

      if (is_percent) {
        plot <-
          plot +
          hrbrthemes::scale_y_percent()
      } else {
        plot <-
          plot +
          hrbrthemes::scale_y_comma()
      }

    }

    include_mean <-
      'mean' %in% (transformations %>% str_to_lower())

    include_median <-
      'median' %in% (transformations %>% str_to_lower())

    include_smooth <-
      'smooth' %in% (transformations %>% str_to_lower())

    if (include_mean) {
      mean_value <-
        data$value %>% mean(na.rm = TRUE)
      plot <-
        plot +
        geom_hline(yintercept = mean_value,
                   colour = "#ff0f0f",
                   linetype = "dashed")

      if (plot_labels) {
        mean_label <-
          list("Mean: ", digits(mean_value, 3)) %>% purrr::reduce(paste0)

        plot <-
          plot +
          annotate(
            "text",
            x = mean(data$dateData, na.rm = TRUE),
            y = mean_value * 1.75,
            label = mean_label,
            colour = "#ff0f0f"
          )

      }
    }

    if (include_median) {
      median_value <- data$value %>% median(na.rm = TRUE)
      plot <-
        plot +
        geom_hline(yintercept = median_value,
                   colour = "#6600ff",
                   linetype = "dashed")
      if (plot_labels) {
        median_label <-
          list("Median: ", digits(median_value, 3)) %>% purrr::reduce(paste0)

        plot <-
          plot +
          annotate(
            "text",
            x = mean(data$dateData, na.rm = TRUE),
            y = median_value * 1.5,
            label = median_label,
            colour = "#6600ff"
          )

      }
    }

    if (include_smooth) {
      plot <-
        plot +
        geom_smooth(
          colour = "#000000",
          method = 'auto',
          span = .3,
          size = .5,
          alpha = 0.5
        ) %>%
        suppressWarnings() %>%
        suppressMessages()
    }


    if (interactive) {
      plot_title <-
        list(title, '\n', sub_title, '\n', caption_text) %>% purrr::reduce(paste0)
      plot <-
        plot +
        labs(
          x = NULL,
          y = type,
          title = plot_title,
          subtitle = NULL,
          caption = NULL
        ) +
        theme(plot.title = element_text(size = 8))
      plot <-
        plotly::ggplotly(plot)
    }
    return(plot)
  }

check_for_hrb <- function() {
  df_pkgs <-
    installed.packages() %>%
    dplyr::as_tibble()

  missing_hrb_themes <-
    df_pkgs %>%
    filter(Package %>% stringr::str_detect('hrbrthemes')) %>% nrow() == 0

  if (missing_hrb_themes) {
    stop(
      list(
        "Missing hrbrthemes which is needed to plot please install using devtools::install_github('hrbrmstr/hrbrthemes')\nAlso make sure to download Roboto font at https://fonts.google.com/specimen/Roboto"
      ) %>%
        purrr::reduce(paste0)
    )
  }
}

#' Plot any FRED time series
#'
#' @param series_id any FRED series ID
#' @param use_random if \code{TRUE} a random FRED series ID is chosen
#' @param fred_data_transformations Any FRED transformation \itemize{
#' \item \code{default}
#' \item \code{change}
#' \item \code{change prior year}
#' \item \code{percent change}
#' \item \code{percent change prior year}
#' \item \code{compounded rate of change}
#' \item \code{continiously compounded rate of change}
#' \item \code{continiously compounded annual rate of change}
#' \item \code{natural log}
#' \item \code{index}
#' }
#' @param date_start data start date, if \code{NULL} all chosen
#' @param date_end data end date
#' @param use_hrbr_theme uses Bob Rudis theme
#' @param plot_transformations Any plot transformations you wish to apply \itemize{
#' \code{median}: Median value
#' \code{mean}: Mean value
#' \code{smooth}: GAM smooth line
#' }
#' @param plot_labels if \code{TRUE} text of any plot transformations are plotted
#' @param interactive if \code{TRUE} visualization turned into an interactive plotly widget
#' @import purrr jsonlite dplyr stringr ggplot2 tidyr
#' @importFrom plotly ggplotly
#' @return if \code{interactive} a plotly htmlwidget or else a static ggplot2 visualization
#' @export
#'
#' @examples
#'

visualize_fred_time_series <-
  function(series_id = "DGS2",
           use_random = FALSE,
           fred_data_transformation = NULL,
           date_start = NULL,
           date_end = NULL,
           plot_transformations = c('mean', 'median', 'smooth'),
           use_hrbr_theme = FALSE,
           plot_labels = FALSE,
           interactive = FALSE) {
    if (use_random) {
      if (!'df_fred_symbols' %>% exists()) {
        "Asssigning FRED symbols to your environment as df_fred_symbols" %>% cat(fill = T)
        assign(
          x = 'df_fred_symbols',
          value = eval(dictionary_.all_fred_series_ids()),
          envir = .GlobalEnv
        )
      }
      "\nBuckle your seatbelts we are plotting one random time series from the nearly 400,000 available on FRED" %>% cat(fill = T)
      series_id <- df_fred_symbols %>% sample_n(1) %>% .$idSeries
    }

    fred_df <-
      .fred_symbol(
        symbol = series_id,
        transformation = fred_data_transformation,
        widen_data = F
      ) %>%
      suppressWarnings() %>%
      suppressMessages()

    plot <-
      plot_time_series(
        data = fred_df,
        date_start = date_start,
        date_end = date_end,
        fred_data_transformation = fred_data_transformation,
        transformations = plot_transformations,
        use_hrbr_theme = use_hrbr_theme,
        plot_labels = plot_labels,
        interactive = interactive
      )
    plot
  }


#' Plot ggplot time series
#'
#' @param data a data frame
#' @param date_start if not NULL date starting
#' @param date_end if not NULL date ending
#' @param transformations \itemize{
#' \item \code{smooth} -- smoothed line
#' \item \code{mean} -- mean
#' \item \code{median}
#' }
#' @param include_recessions if \code{TRUE} includes recession bars
#' @param x_breaks  number of x breks
#' @param plot_labels if \code{TRUE} plots labels
#' @param use_hrbr_theme if \code{TRUE} returns hrbr theme
#'
#' @return a ggplot2 object
#' @export
#' @import dplyr glue tis grid hrbrthemes purrr readr stringr formattable
#' @examples
plot_time_series_static <-
  function(data,
           date_start = NULL,
           date_end = NULL,
           transformations = c('mean', 'median', 'smooth'),
           include_recessions = TRUE,
           x_breaks = 12,
           plot_labels = TRUE,
           use_hrbr_theme = TRUE)  {
    data <-
      data %>%
      filter(!is.na(value))

    value_type <-
      data$typeValue %>% unique()
    data_source <- data$nameSource %>% unique()
    data_frequency <-
      data$frequencyData %>% unique() %>% str_to_lower()
    series_name <- data %>% pull(nameSeries) %>% unique()
    start_date <-
      data %>% pull(dateData) %>% min(na.rm = T) %>% .[[1]]
    end_date <-
      data %>% pull(dateData) %>% max(na.rm = T) %>% .[[1]]
    data <-
      data %>%
      select(-c(nameSeries, typeValue, nameSource))

    split_series <-
      series_name %>% nchar() > 50

    if (split_series) {
      title <-
        stringi::stri_wrap(str = series_name, width = 50) %>% str_c(collapse = "\n")
    } else {
      title <- series_name
    }

    if (include_recessions) {
      recession_title <- " -- U.S. recessions in red"
    } else {
      recession_title <- ''
    }
    start_d <- data$dateData %>% min(na.rm = TRUE)
    end_d <- data$dateData %>% max(na.rm = TRUE)
    symbol <- data$idSymbol %>% unique()

    sub_title <-
      glue::glue("Data from {start_d} to {end_d} for FRED ID {symbol}{recession_title}")

    type <-
      value_type

    is_percent <- type %>% stringr::str_detect('PERCENT')

    if (!data_source %>% purrr::is_null()) {
      caption_text <-
        glue::glue(
          "Source data: {data_source}\nReported {data_frequency}\nvia FRED from fundManageR"
        )
    } else {
      caption_text <-
        "Sourced from FRED via fundManageR"
    }

    plot <-
      data %>%
      ggplot(aes(x = dateData, y = value)) +
      theme_minimal() +
      geom_line(color = "#00B0F0", size = .5) +
      geom_area(fill = "#00B0F0",
                alpha = 0.25,
                color = NA) +
      theme(
        panel.background = element_rect(fill = "#fffff8", color = NA),
        plot.background = element_rect(fill = "#fffff8", color = NA)
      ) +
      labs(
        x = NULL,
        y = type,
        title = title,
        subtitle = sub_title,
        caption = caption_text
      ) +
      scale_x_date(expand = c(0, 0),
                   breaks = scales::pretty_breaks(n = x_breaks))

    plot <-
      plot +
      hrbrthemes::theme_ipsum_rc(
        grid = "XY",
        plot_title_size = 10,
        subtitle_size = 8.5,
        caption_size = 8.5,
        axis_text_size = 10,
        axis_title_size = 10,
        strip_text_size = 10
      )

    if (is_percent) {
      plot <-
        plot +
        hrbrthemes::scale_y_percent(breaks = scales::pretty_breaks(n = 10))
    } else {
      plot <-
        plot +
        hrbrthemes::scale_y_comma(breaks = scales::pretty_breaks(n = 10))
    }


    include_mean <-
      'mean' %in% (transformations %>% str_to_lower())

    include_median <-
      'median' %in% (transformations %>% str_to_lower())

    include_smooth <-
      'smooth' %in% (transformations %>% str_to_lower())

    if (include_mean) {
      mean_value <-
        data$value %>% mean(na.rm = TRUE)
      plot <-
        plot +
        geom_hline(yintercept = mean_value,
                   colour = "#ff0f0f",
                   linetype = "dashed")

      if (plot_labels) {
        mean_label <-
          list("Mean: ", formattable::comma(mean_value, digits = 3)) %>% purrr::reduce(paste0)


        my_grob <-
          grid::grid.text(
            mean_label,
            x = 0.7,
            y = .93,
            gp = gpar(
              col = "#ff0f0f",
              fontsize = 8,
              fontface = "bold"
            )
          )

        plot <-
          plot + annotation_custom(my_grob)
      }
    }

    if (include_median) {
      median_value <- data$value %>% median(na.rm = TRUE)
      plot <-
        plot +
        geom_hline(yintercept = median_value,
                   colour = "#6600ff",
                   linetype = "dashed")

      if (plot_labels) {
        median_label <-
          list("Median: ",  formattable::comma(median_value, digits = 3)) %>% purrr::reduce(paste0)

        my_grob2 <-
          grid::grid.text(
            median_label,
            x = 0.35,
            y = .93,
            gp = gpar(
              col = "#6600ff",
              fontsize = 8,
              fontface = "bold"
            )
          )

        plot <-
          plot + annotation_custom(my_grob2)
      }
    }

    if (include_smooth) {
      plot <-
        plot +
        geom_smooth(
          colour = "#000000",
          method = 'loess',
          span = .3,
          size = .5,
          alpha = 0.45
        ) %>%
        suppressWarnings() %>%
        suppressMessages()
    }

    if (include_recessions) {
      df_recession_start_end <-
        tis::nberDates() %>%
        as_tibble() %>%
        mutate_all(lubridate::ymd) %>%
        purrr::set_names(c("dateStart", "dateEnd"))


      df_recessions <-
        df_recession_start_end %>%
        filter(dateStart >= start_date)

      has_recessions <- df_recessions %>% nrow() > 0

      df_recessions <-
        df_recessions %>%
        filter(dateEnd <= end_date)

      really_has_recessions <-
        nrow(df_recessions) > 0 && has_recessions

      if (really_has_recessions) {
        plot <-
          plot +
          geom_rect(
            data = df_recessions ,
            inherit.aes = F,
            aes(
              xmin = dateStart,
              xmax = dateEnd,
              ymin = -Inf,
              ymax = +Inf
            ),
            fill = "red",
            alpha = 0.2
          )

      }
    }

    plot

  }

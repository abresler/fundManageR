#' Drops NA columns
#'
#' @param data
#'
#' @return
#' @export
#' @import dplyr
#' @examples
#' data_frame(nameFirm = 'Goldman Sachs', countSuperHeros = NA, countCriminals = 0, countFinedEmployees = 10) %>% drop_na_columns()
drop_na_columns <-
  function(data) {
    data %>%
      select(which(colMeans(is.na(.)) < 1)) %>%
      suppressMessages() %>%
      suppressWarnings()
  }

get_class_df <-
  function(data) {
    class_data <-
      data %>% map(class)

    class_df <-
      1:length(class_data) %>%
      map_df(function(x) {
        data_frame(nameColumn = names(data)[[x]],
                   typeColumn = class_data[[x]] %>% .[length(.)])
      })

    return(class_df)
  }


#' Tidy column formatting
#'
#' @param data
#'
#' @return
#' @export
#' @import dplyr stringr formattable purrr tidyr
#' @examples
tidy_column_formats <-
  function(data) {
    data <-
      data %>%
      mutate_if(is_character,
                funs(ifelse(. == "N/A", NA, .))) %>%
      mutate_if(is_character,
                funs(ifelse(. == "", NA, .)))

    data <-
      data %>%
      mutate_at(data %>% select(matches("^idCRD")) %>% names(),
                funs(. %>% as.numeric())) %>%
      mutate_at(data %>% select(
        matches(
          "^name[A-Z]|^details[A-Z]|^description[A-Z]|^city[A-Z]|^state[A-Z]|^country[A-Z]|^count[A-Z]|^street[A-Z]|^address[A-Z]"
        )
      ) %>% names(),
      funs(. %>% str_to_upper())) %>%
      mutate_at(data %>% select(matches("^url[A-Z]")) %>% names(),
                funs(. %>% str_to_lower())) %>%
      mutate_at(data %>% select(matches("^amount")) %>% names(),
                funs(. %>% formattable::currency(digits = 0))) %>%
      mutate_at(
        data %>% select(matches("latitude|longitude")) %>% names(),
        funs(. %>% as.numeric() %>% formattable::digits(digits = 5))
      ) %>%
      mutate_at(
        data %>% select(matches("^price[A-Z]|pershare")) %>% select(-matches("priceNotation")) %>% names(),
        funs(. %>% formattable::currency(digits = 3))
      ) %>%
      mutate_at(
        data %>% select(matches(
          "^count[A-Z]|^number[A-Z]|^year[A-Z]"
        )) %>% select(-matches("country|county")) %>% names(),
        funs(. %>% formattable::comma(digits = 0))
      ) %>%
      mutate_at(data %>% select(matches("^ratio|^multiple|^priceNotation")) %>% names(),
                funs(. %>% formattable::comma(digits = 3))) %>%
      mutate_at(data %>% select(matches("^pct|^percent")) %>% names(),
                funs(. %>% formattable::percent(digits = 3)))
    has_dates <-
      data %>% select(matches("^date")) %>% ncol() > 0

    if (has_dates) {
      data %>% select(matches("^date")) %>% map(class)
    }
    data <-
      data %>%
      drop_na_columns()
    return(data)
  }


#' Tidy columns with nested list or related columns
#'
#' @param data data frame column
#' @param column_keys column keys
#' @param bind_to_original_df bind to the original data frame
#' @param clean_column_formats clean the columns
#'
#' @return
#' @export
#' @import dplyr stringr formattable purrr tidyr
#' @examples
tidy_column_relations <-
  function(data,
           column_keys = c('idCIK', 'nameEntity'),
           bind_to_original_df = FALSE,
           clean_column_formats = TRUE) {
    class_df <-
      data %>%
      get_class_df()

    df_lists <-
      class_df %>%
      filter(typeColumn %in% c('list', 'date.frame'))

    has_lists <-
      df_lists %>% nrow() > 0

    columns_matching <-
      names(data)[!names(data) %>% substr(nchar(.), nchar(.)) %>% readr::parse_number() %>% is.na() %>% suppressWarnings()] %>%
      suppressWarnings() %>%
      suppressMessages() %>%
      suppressWarnings()

    if (has_lists == F & columns_matching %>% length() == 0) {
      return(data)
    }


    data <-
      data %>%
      mutate(idRow = 1:n()) %>%
      select(idRow, everything())
    df <-
      data_frame()

    if (columns_matching %>% length() > 0) {
      match <-
        columns_matching %>% str_replace_all('[0-9]', '') %>% unique() %>% paste0(collapse = '|')

      df_match <-
        data %>%
        select(idRow, one_of(column_keys), matches(match))

      has_dates <-
        names(df_match) %>% str_count("date") %>% sum() > 0

      if (has_dates) {
        df_match <-
          df_match %>%
          mutate_at(df_match %>% select(matches("^date")) %>% names(),
                    funs(. %>% as.character()))
      }

      key_cols <-
        df_match %>%
        select(c(idRow, one_of(column_keys))) %>%
        names()

      valuecol <-
        'value'

      gathercols <-
        df_match %>%
        select(-c(idRow, one_of(column_keys))) %>%
        names()

      df_match <-
        df_match %>%
        gather_('item', 'value', gathercols, na.rm = T) %>%
        mutate(
          countItem = item %>% readr::parse_number(),
          countItem = ifelse(countItem %>% is.na(), 0, countItem) + 1,
          item = item %>% str_replace_all('[0-9]', '')
        ) %>%
        suppressWarnings() %>%
        suppressMessages()
      df <-
        df %>%
        bind_rows(df_match)

    }

    if (has_lists) {
      df_list <-
        1:nrow(df_lists) %>%
        map_df(function(x) {
          column <-
            df_lists$nameColumn[x]
          if (column == 'dataInsiderCompaniesOwned') {
            is_insider <-
              TRUE
          } else {
            is_insider <-
              FALSE
          }
          df <-
            data %>%
            select(idRow, one_of(c(column_keys, column)))
          col_length_df <-
            1:nrow(df) %>%
            map_df(function(x) {

              value <-
                df[[column]][[x]]

              if (value %>% purrr::is_null()) {
                return(data_frame(idRow = x))
              }
              columns_matching <-
                names(value)[!names(value) %>% substr(nchar(.), nchar(.)) %>% readr::parse_number() %>% is.na() %>% suppressWarnings()] %>%
                suppressWarnings() %>%
                suppressMessages() %>%
                suppressWarnings()

              if (columns_matching %>% length() == 0) {
                return(data_frame(idRow = x))
              }

              data_frame(idRow = x, countCols = value %>% ncol())
            })

          if (col_length_df %>% ncol() == 1) {
            return(data_frame())
          }

          df <-
            df %>%
            left_join(col_length_df) %>%
            filter(!countCols %>% is.na()) %>%
            select(-countCols) %>%
            suppressWarnings() %>%
            suppressMessages()

          df <-
            df %>%
            unnest()

          has_dates <-
            names(df) %>% str_count("date") %>% sum() > 0

          if (has_dates) {
            df <-
              df %>%
              mutate_at(df %>% select(matches("^date")) %>% names(),
                        funs(. %>% as.character()))
          }

          gathercols <-
            df %>%
            select(-c(idRow, one_of(column_keys))) %>%
            names()

          df <-
            df %>%
            gather_('item', 'value', gathercols, na.rm = T) %>%
            mutate(
              countItem = item %>% readr::parse_number(),
              countItem = ifelse(countItem %>% is.na(), 0, countItem) + 1,
              item = item %>% str_replace_all('[0-9]', ''),
              value = value %>% as.character()
            ) %>%
            suppressWarnings() %>%
            suppressMessages()
          if (is_insider) {
            df <-
              df %>%
              mutate(item = item %>% paste0('Insider'))
          }
          return(df)
        }) %>%
        arrange(idRow) %>%
        distinct()
      df <-
        df %>%
        bind_rows(df_list)
    }

    has_data <-
      df %>% nrow() > 0

    if (!has_data) {
      return(data %>% select(-matches("idRow")))
    }
    if (has_data) {
      df <-
        df %>%
        arrange(idRow)

      col_order <-
        c(df %>% select(-c(item, value)) %>% names(), df$item)


      df <-
        df %>%
        spread(item, value) %>%
        select(one_of(col_order))

      has_dates <-
        data %>% select(matches("^date")) %>% ncol() > 0

      if (has_dates) {
        df <-
          df %>%
          mutate_at(df %>% select(matches("^date[A-Z]")) %>% names(),
                    funs(. %>% lubridate::ymd())) %>%
          mutate_at(df %>% select(matches("^datetime[A-Z]")) %>% names(),
                    funs(. %>% lubridate::ymd_hm()))
      }
      if (clean_column_formats) {
        df <-
          df %>%
          tidy_column_formats()
      }

      if ('nameFormType' %in% names(df)) {
        df <-
          df %>%
          filter(!nameFormType %>% is.na())
      }

      if (bind_to_original_df) {
        if (has_lists) {
          if (columns_matching %>% length() > 0) {
            ignore_cols <-
              c(columns_matching, df_lists$nameColumn) %>%
              paste0(collapse = '|')
          } else {
            ignore_cols <-
              c(df_lists$nameColumn) %>%
              paste0(collapse = '|')
          }
        }
        if (!has_lists) {
          ignore_cols <-
            columns_matching %>% paste0(collapse = '|')
        }

        data <-
          data %>%
          select(-matches(ignore_cols)) %>%
          left_join(df %>%
                      nest(-idRow, .key = 'dataResolved')) %>%
          suppressMessages()
        return(data)
      }
      if (!bind_to_original_df) {
        return(df)
      }
    }

  }
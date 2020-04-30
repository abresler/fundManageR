.build_address <-
  function(data, end_slug, end_slugs, address_parts, return_message = T) {

    if (return_message) {
      glue("Building location for {end_slug}") %>% message()
    }

    parts <-
      address_parts[address_parts %>% str_detect(end_slug)]

    remove_parts <-
      end_slugs[!end_slugs %in% end_slug] %>% str_c(collapse = "|")

    if (!end_slug %>% str_detect("Mailing|Alternate|Alt") & remove_parts != "") {
      parts <-
        parts %>% str_remove_all(remove_parts)
    }

    parts <- parts[parts %>% str_detect(end_slug)]
    new_col <- glue("location{end_slug}") %>% as.character()

    if (data %>% hasName(new_col)) {
      return(data)
    }

    city_state <- glue("cityState{end_slug}") %>% as.character()
    address <-
      parts[parts %>% str_detect("addressStreet|address_street")]
    if (length(address) > 0)  {
      address <- address[[1]]
    }
    address1 <- parts[parts %>% str_detect("addressStreet1|address_street_1")]
    if (length(address1) > 0)  {
      address1 <- address1[[1]]
    }


    address2 <- parts[parts %>% str_detect("addressStreet2|address_street_2")]

    if (length(address2) > 0)  {
      address2 <- address2[[1]]
    }

    city <- parts[parts %>% str_detect("city|City")]

    if (length(city) > 0)  {
      city <- city[[1]]
    }


    state <- parts[parts %>% str_detect("state|State")]

    if (length(state) > 0)  {
      state <- state[[1]]
    }


    zip <-
      parts[parts %>% str_detect("zip")]
    zip <- zip[!zip %>% str_detect("zipcode4|zip4")]

    if (length(zip) > 0)  {
      zip <- zip[[1]]
    }


    country <- parts[parts %>% str_detect("country")]

    if (length(country) > 0)  {
      country <- country[[1]]
    }

    df_locs <-
      data %>%
      select(one_of(address, address1, address2, city, state, zip, country)) %>%
      distinct()

    if (length(city) + length(state) == 2) {
      df_locs <-
        df_locs %>%
        unite(!!sym(city_state),
              city,
              state,
              sep = ", ",
              ,
              remove = F) %>%
        filter(!!sym(city_state) != "NA, NA")

      df_locs <-
        df_locs %>%
        mutate_if(is.character,
                  list(function(x) {
                    x %>% coalesce("")
                  })) %>%
        unite(
          !!sym(new_col),
          c(address, city_state, zip, country),
          sep = " ",
          remove = F
        ) %>%
        mutate_at(new_col, str_squish) %>%
        mutate_if(is.character,
                  list(function(x) {
                    case_when(x == "" ~ NA_character_,
                              TRUE ~ x)
                  }))

    } else {
      df_locs <-
        df_locs %>%
        mutate_if(is.character,
                  list(function(x) {
                    x %>% coalesce("")
                  })) %>%
        unite(
          !!sym(new_col),
          c(address, city, state, zip, country),
          sep = " ",
          remove = F
        ) %>%
        mutate_at(new_col, str_squish) %>%
        mutate_if(is.character,
                  list(function(x) {
                    case_when(x == "" ~ NA_character_,
                              TRUE ~ x)
                  }))
    }


    join_cols <- names(df_locs)[names(df_locs) %in% names(data)]

    data <-
      data %>%
      left_join(df_locs, by = join_cols)

    data
  }



#' Build Address from tibble
#'
#'
#' @param data \code{tibble}
#' @param return_message if \code{TRUE} returns a message
#' @param address_search_slugs vector of slugs identifying address features - defaults to `c("^address", "^streetAddress", "^city", "^state", "^codeState", "^codeCountry", "^country", "^zipcode")`
#' @param include_snake_versions `TRUE` includes snaked version of names
#' @param part_threshold minimum number of matches
#' @param snake_names if \code{TRUE} snakes names
#'
#' @return
#' @export
#'
#' @examples
build_address <-
  function(data,
           address_search_slugs = c("^address", "^streetAddress", "^city", "^state", "^codeState", "^codeCountry", "^country", "^zipcode"),
           include_snake_versions = T,
           part_threshold = 3,
           snake_names = F,
           return_message = T) {

    if (include_snake_versions) {
      clean_n <- address_search_slugs %>% make_clean_names()
      clean_n <- glue("^{clean_n}") %>% as.character()
      address_search_slugs <- c(address_search_slugs,clean_n)  %>% unique()
    }


    address_slugs <-
      str_c(address_search_slugs, collapse = "|")

    address_parts <-
      data %>% select(matches(address_slugs)) %>% names()

    if (length(address_parts) == 0) {
      return(data)
    }
    end_slugs <-
      tibble(part = address_parts %>%
               str_remove_all(address_slugs)) %>%
      count(part, sort = T) %>%
      filter(n >= part_threshold) %>%
      pull(part)

    end_slugs %>%
      walk(function(x) {
        data <<-
          .build_address(
            data = data,
            end_slug = x,
            end_slugs = end_slugs,
            address_parts = address_parts,
            return_message = return_message
          )
      })

    if (snake_names) {
      data <- data %>% clean_names()
    }

    data
  }


#' Munge a tibble
#'
#' @param data a \code{tibble}
#' @param snake_names if \code{TRUE} returns snake case names
#' @param unformat if \code{TRUE} no formattable digits
#' @param convert_case if \code{TRUE} normalizes non url character columns to upper
#' @param amount_digits formattable digits
#' @param include_address if \code{TRUE} builds addresses
#'
#' @return
#' @export
#'
#' @examples
munge_tbl <-
  function(data, snake_names = F, unformat = F, convert_case = T,
           amount_digits = 2,
           include_address = T) {

    data <- data %>%
      mutate_if(is.character,
                list(function(x) {
                  x %>% str_squish()
                })) %>%
      mutate_if(is.character,
                list(function(x) {
                  case_when(x == "" ~ NA_character_,
                            TRUE ~ x)
                }))

    is_has <-
      data %>%
      select_if(is.character) %>%
      dplyr::select(dplyr::matches("^is|^has")) %>% names()


    if (length(is_has) > 0) {
      data <- data %>%
        mutate_at(is_has,
                  list(function(x){
                    case_when(x %in% c("Y", "YES", "TRUE", "1") ~ TRUE,
                              TRUE ~ FALSE)
                  }))
    }

    to_num <-
      data %>%
      select_if(is.character) %>%
      select(matches("amount|price|value|ratio|count[A-Z]|number|shares")) %>%
      names()

    if (length(to_num) > 0) {
      data <- data %>%
        mutate_at(to_num, readr::parse_number)
    }

    if (convert_case) {
      to_upper <-
        data %>% select_if(is.character) %>%
        select(-matches("^url")) %>%
        names()
      data <- data %>%
        mutate_at(to_upper,
                  str_to_upper)
    }

    if (!unformat) {
      pct_names <-
        data %>%
        select_if(is.numeric) %>%
        select(matches("^percent|^pct")) %>% names()
      count_names <-
        data %>%
        select_if(is.numeric) %>%
        select(matches("^count|^number")) %>% names()

      amt_names <-
        data %>%
        select_if(is.numeric) %>%
        select(matches("^amount|^amt|^price|^earnings")) %>% names()


      if (length(pct_names) > 0) {
        data <- data %>%
          mutate_at(pct_names,
                    list(function(x){
                      x %>% percent(digits = 2)
                    }))
      }

      if (length(amt_names) > 0) {
        data <- data %>%
          mutate_at(pct_names,
                    list(function(x){
                      x %>% currency(digits = amount_digits)
                    }))
      }

      if (length(count_names) > 0) {
        data <- data %>%
          mutate_at(count_names,
                    list(function(x){
                      x %>% comma(digits = 0)
                    }))
      }
    }



    if (include_address) {
      data <-
        data %>%
        build_address()
    }

    if (snake_names) {
      data <-
        data %>%
        janitor::clean_names()
    }

    data
  }

# filers ------------------------------------------------------------------
.get_cik_url_df <-
  function(cik = 1138621) {
    slugs <-
      c(
        'general',
        'filings',
        'private',
        'fundraising',
        'traders',
        'clevel',
        'mda',
        'owners',
        'subsidiaries'
      )

    url_json <-
      list('http://rankandfiled.com/data/filer/', cik, '/', slugs) %>%
      purrr::invoke(paste0, .)

    url_df <-
      dplyr::tibble(
        nameTable = c(
          'General',
          'Filings',
          'Private',
          'Fundraising',
          'Traders',
          'C Level',
          'MDA',
          'Owners',
          'Subsidiaries'
        ),
        urlJSON = url_json
      )
    return(url_df)
  }

.parse_json_general_filing <-
  function(url = "http://rankandfiled.com/data/filer/1468327/general",
           nest_data = TRUE,
           return_message = TRUE) {
    if (!url %>% httr::url_ok() %>% suppressWarnings()) {
      return(tibble())
    }

    data <-
      url %>%
      jsonlite::fromJSON(simplifyDataFrame = TRUE) %>%
      data.frame(stringsAsFactors = FALSE) %>%
      as_tibble()

    is_company <-
      'company' %in% names(data)

    is_insider <-
      'insider' %in% names(data)

    is_fund <-
      'fund' %in% names(data)

    data <-
      data %>%
      .resolve_name_df()

    cik <-
      url %>% str_replace_all('http://rankandfiled.com/data/filer/', '') %>%
      str_replace_all('\\/general', '') %>%
      as.numeric()

    data <-
      data %>%
      mutate(idCIK = cik)

    if (!'nameEntity' %in% names(data)) {
      if (is_company) {
        ticker <-
          data$company

        company_name_df <-
          ticker %>%
          .parse_company_general_safe() %>%
          suppressWarnings()

        has_rows <-
          company_name_df %>% nrow > 0
        if (has_rows) {
          return(company_name_df)
        } else {
          entity_name <-
            NA
        }
      }
      if (is_insider) {
        insider_df <-
          .parse_json_general_insider(cik = cik, return_message = return_message) %>%
          mutate(nameEntity = nameEntity %>% str_to_upper())
        return(insider_df)
      }
      if (is_fund) {
        fund_df <-
          .parse_json_fund_general(cik = cik, return_message = return_message) %>%
          mutate(nameEntity = nameEntity %>% str_to_upper())
        return(fund_df)
      }
      data <-
        data %>%
        mutate(nameEntity = entity_name,
               idTicker = ticker) %>%
        select(-dplyr::matches("company"))
    }

    data <-
      data %>%
      select(-dplyr::matches("object")) %>%
      mutate_at(.vars = data %>% select(dplyr::matches("idCIK|idIRS")) %>% names(),
                as.numeric) %>%
      mutate(urlJSONGeneral = url,
             nameEntity = nameEntity %>% stringr::str_to_upper())
    has_address <-
      names(data) %in% c('addressStreet1Entity',
                         'stateEntity',
                         'cityEntity',
                         'zipcodeEntity') %>% sum() == 4
    if (has_address) {
      data <-
        data %>%
        mutate(
          addressEntity = list(
            addressStreet1Entity,
            ' ',
            cityEntity,
            ' ',
            stateEntity,
            ', ',
            zipcodeEntity
          ) %>% purrr::invoke(paste0, .)
        ) %>%
        select(idCIK, dplyr::matches("nameEntity"), addressEntity, everything())
    }

    if ('detailsOwnedBy' %in% names(data)) {
      data <-
        data %>%
        dplyr::rename(detailsOwns = detailsOwnedBy)
    }

    if ('detailsOwns' %in% names(data)) {
      detail_df <-
        seq_along(data$detailsOwns) %>%
        future_map_dfr(function(x) {
          detail_value <-
            data$detailsOwns[[x]]

          if (detail_value %>% is.na()) {
            df <-
              tibble(idRow = x, nameCompanyOwns = NA)

            if (nest_data) {
              df <-
                df %>%
                nest(-idRow, .key = dataCompaniesOwns)
            }

            return(df)
          }

          values <-
            detail_value %>% str_replace('\\|', '') %>%
            str_split('\\|') %>%
            flatten_chr()

          df_data <-
            tibble(value = values) %>%
            tidyr::separate(value,
                            into = c('idTickerOwns', 'other'),
                            sep = '\\:') %>%
            tidyr::separate(other,
                            into = c('nameCompanyOwns', 'other'),
                            sep = '\\_') %>%
            tidyr::separate(other,
                            into = c('roleOwner', 'dateOwner'),
                            sep = '\\#') %>%
            mutate(nameCompanyOwns = nameCompanyOwns %>% str_to_upper(),
                   idRow = x) %>%
            gather(item, value, -idRow, na.rm = TRUE) %>%
            group_by(item) %>%
            mutate(count = 1:n() - 1) %>%
            ungroup() %>%
            arrange((count)) %>%
            mutate(item = ifelse(count == 0, item, paste0(item, count))) %>%
            select(-count)

          column_order <-
            c('idRow', df_data$item)

          df_data <-
            df_data %>%
            spread(item, value) %>%
            select(one_of(column_order))
        }) %>%
        suppressWarnings()

      detail_df <-
        detail_df %>%
        mutate_at(.vars = detail_df %>% select(dplyr::matches("date")) %>% names(),
                  funs(. %>% ymd())) %>%
        suppressWarnings()

      if (nest_data) {
        detail_df <-
          detail_df %>%
          nest(-idRow, .key = dataCompaniesOwns)
      }

      data <-
        data %>%
        mutate(idRow = 1:n()) %>%
        select(-detailsOwns) %>%
        left_join(detail_df) %>%
        select(-idRow) %>%
        suppressMessages()
    }

    data <-
      data %>%
      select(
        nameEntity,
        idCIK,
        dplyr::matches("typeCategory"),
        dplyr::matches("idtypeCompany"),
        everything()
      )

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% cat(fill = T)
    }

    return(data)

  }

.parse_json_filings <-
  function(url = "http://rankandfiled.com/data/filer/1138621/filings",
           return_message = TRUE) {
    if (!url %>% httr::url_ok() %>% suppressWarnings()) {
      return(tibble())
    }

    cik <-
      url %>% str_replace_all('http://rankandfiled.com/data/filer/|/filings', '') %>%
      as.numeric()

    json_data <-
      url %>%
      jsonlite::fromJSON(simplifyDataFrame = TRUE) %>%
      data.frame(stringsAsFactors = FALSE) %>%
      as_tibble() %>%
      tidyr::separate(
        filings,
        sep = '\\*',
        into = c(
          'dateFiling',
          'codeFiling',
          'typeForm',
          'baseIndex',
          'detailOffering',
          'slugSEC',
          'idSECSlug'
        )
      ) %>%
      mutate(
        dateFiling = dateFiling %>% as.numeric() %>% lubridate::ymd,
        idCIK = cik,
        urlJSONFilings = url,
        urlSEC = ifelse(
          slugSEC == "None",
          NA,
          list(
            "https://www.sec.gov/Archives/edgar/data/",
            idCIK,
            '/',
            slugSEC
          ) %>% purrr::invoke(paste0, .)
        ),
        pageSlug = idSECSlug %>% str_replace_all('\\-',''),
        urlSECFilingDirectory = ifelse(
          idSECSlug %>% str_detect('\\-'),
          list(
            "https://www.sec.gov/Archives/edgar/data/",
            idCIK,
            '/',
            pageSlug,
            '/',
            idSECSlug,
            '-index.htm'
          ) %>% purrr::reduce(paste0),
          NA
        )
      ) %>%
      select(-dplyr::matches("^X")) %>%
      suppressMessages() %>%
      select(-c(slugSEC, pageSlug)) %>%
      select(idCIK, dateFiling, everything())

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% cat(fill = T)
    }

    return(json_data)
  }

.parse_json_private <-
  function(url = "http://rankandfiled.com/data/filer/1438171/private",
           nest_data = TRUE,
           return_message = TRUE) {
    if (!url %>% httr::url_ok() %>% suppressWarnings()) {
      return(tibble())
    }

    json_data <-
      url %>%
      jsonlite::fromJSON()

    options(scipen = 9999)

    status_df <-
      json_data$status_history %>% flatten_df() %>%
      mutate(date = date %>% lubridate::ymd())

    offering_history_class_df <-
      json_data$offering_history %>% future_map_dfr(class) %>%
      gather(column, type) %>%
      mutate(idName = 1:n())

    offering_data <-
      json_data$offering_history %>%
      select(offering_history_class_df %>%
               filter(!type == 'list') %>%
               .$idName)

    offering_data <-
      offering_data %>%
      as_tibble() %>%
      mutate_all(funs(. %>% str_replace('\\|', '')))

    offering_data <-
      offering_data %>%
      .resolve_name_df() %>%
      resolve_names_to_upper()

    if (offering_data %>% ncol >= 9) {
      offering_data <-
        offering_data %>%
        separate_column(column_name = 'idExemption') %>%
        separate_column(column_name = 'dateAmmended') %>%
        separate_column(column_name = 'amountFindersFee') %>%
        separate_column(column_name = 'countInvestors') %>%
        separate_column(column_name = 'countInvestorsNonAccredited') %>%
        separate_column(column_name = 'amountOffered') %>%
        separate_column(column_name = 'amountRemaining') %>%
        separate_column(column_name = 'amountSold')


      offering_data <-
        offering_data %>%
        mutate_at(.vars = offering_data %>% select(dplyr::matches("^is")) %>% names,
                  funs(. %>% as.logical())) %>%
        mutate_at(.vars = offering_data %>% select(dplyr::matches("^amount|^count|^idCIK")) %>% names,
                  funs(. %>% as.numeric())) %>%
        mutate_at(.vars = offering_data %>% select(dplyr::matches("^date")) %>% names,
                  funs(. %>% lubridate::ymd())) %>%
        mutate_at(.vars = offering_data %>% select(dplyr::matches("^amount")) %>% names,
                  funs(. %>% formattable::currency(digits = 0))) %>%
        mutate_at(.vars = offering_data %>% select(dplyr::matches("^count")) %>% names,
                  funs(. %>% formattable::comma(digits = 0))) %>%
        mutate_if(is.numeric, as.numeric)
    } else {
      offering_data <-
        offering_data %>%
        mutate_at(.vars = offering_data %>% select(dplyr::matches("^amount|^count|^idCIK")) %>% names,
                  funs(. %>% as.numeric())) %>%
        mutate_at(.vars = offering_data %>% select(dplyr::matches("^date")) %>% names,
                  funs(. %>% lubridate::ymd()))
    }

    has_relations <-
      '_related_people' %in% names(json_data$offering_history)

    if (has_relations) {
      relation_df <-
        1:(json_data$offering_history$amended %>% length()) %>%
        future_map_dfr(function(x) {
          if (!json_data$offering_history$`_related_people`[[x]] %>% purrr::is_null()) {
            relation_data <-
              json_data$offering_history$`_related_people`[[x]] %>% mutate(
                name =
                  ifelse(
                    name %>% substr(1, 3) %>% str_detect('\\-'),
                    name %>% str_replace_all('\\-', '') %>% str_trim,
                    name %>% str_trim
                  )
              ) %>%
              tidyr::unite(nameRelation, name, relation, sep = '-') %>%
              .$nameRelation %>% paste0(collapse = '&')
          } else {
            relation_data <-
              NA
          }
          tibble(nameRelation = relation_data)
        }) %>%
        resolve_names_to_upper()

      relation_df <-
        1:nrow(relation_df) %>%
        future_map_dfr(function(x) {
          person_title <-
            relation_df$nameRelation[[x]] %>%
            str_split('\\&') %>%
            flatten_chr() %>%
            str_to_upper() %>%
            str_trim()

          df <-
            tibble(idRow = x, person_title) %>%
            tidyr::separate(
              person_title,
              sep = '\\-',
              into = c('nameRelatedParty', 'titleRelatedParty')
            ) %>%
            mutate(countItem = 1:n() - 1) %>%
            gather(item, value, -c(idRow, countItem)) %>%
            arrange(countItem)

          df <-
            df %>%
            mutate(item = ifelse(countItem == 0, item, item %>% paste0(countItem))) %>%
            select(-countItem)
          column_order <-
            c('idRow', df$item)

          df <-
            df %>%
            spread(item, value) %>%
            select(one_of(column_order))

          if (nest_data) {
            df <-
              df %>%
              nest(-idRow, .key = dataRelations)
          }
          return(df)
        })

      offering_data <-
        offering_data %>%
        mutate(idRow = 1:n()) %>%
        left_join(relation_df) %>%
        suppressMessages() %>%
        select(-idRow)
    }

    has_brokers <-
      '_brokers' %in% names(json_data$offering_history)

    if (has_brokers) {
      broker_df <-
        1:(json_data$offering_history$amended %>% length()) %>%
        map_dfr(function(x) {
          empty_value <-
            json_data$offering_history$`_brokers`[[x]] %>% length() ==0
          if (empty_value) {
            broker_crd <-
              NA
          } else {
            broker_crd <-
              json_data$offering_history$`_brokers`[[x]] %>%
              tidyr::unite(nameBrokerCRD, name, crd, sep = '&') %>%
              .$nameBrokerCRD %>%
              paste0(collapse = ' | ')
          }
          tibble(nameBrokerCRD = broker_crd)
        }) %>%
        resolve_names_to_upper()

      broker_df <-
        1:nrow(broker_df) %>%
        future_map_dfr(function(x) {
          broker_crd <-
            broker_df$nameBrokerCRD[[x]] %>%
            str_split('\\|') %>%
            flatten_chr() %>%
            str_to_upper() %>%
            str_trim()

          if (broker_crd %>% is.na() %>% sum() > 0) {
            df <-
              tibble(
                idRow = x,
                nameBroker = "NONE",
                idCRDBroker = NA
              )
            if (nest_data) {
              df <-
                df %>%
                nest(-idRow, .key = dataBrokers)
            }
            return(tibble())
          }

          df <-
            tibble(idRow = x, broker_crd) %>%
            tidyr::separate(broker_crd,
                            sep = '\\&',
                            into = c('nameBroker', 'idCRDBroker')) %>%
            mutate(countItem = 1:n() - 1) %>%
            gather(item, value, -c(idRow, countItem)) %>%
            arrange(countItem) %>%
            mutate(item = ifelse(countItem == 0, item, item %>% paste0(countItem))) %>%
            select(-countItem)

          column_order <-
            c('idRow', df$item)

          df <-
            df %>%
            spread(item, value) %>%
            select(one_of(column_order))

          df <-
            df %>%
            mutate_at(df %>% select(dplyr::matches("idCRD")) %>% names(),
                      funs(. %>% as.numeric())) %>%
            resolve_names_to_upper()

          if (nest_data) {
            df <-
              df %>%
              nest(-idRow, .key = dataBrokers)
          }
          return(df)
        })

      offering_data <-
        offering_data %>%
        mutate(idRow = 1:n()) %>%
        left_join(broker_df) %>%
        suppressMessages() %>%
        select(-idRow)
    }

    if ('date' %in% names(status_df)) {
      initial_date <-
        status_df$date
    } else {
      initial_date <-
        NA
    }

    if ('entity_type' %in% names(status_df)) {
      typeEntity <-
        status_df$entity_type
    } else {
      typeEntity <-
        NA
    }

    if ('jurisdiction' %in% names(status_df)) {
      jurisdiction <-
        status_df$jurisdiction
    } else {
      jurisdiction <-
        NA
    }

    if ('over_five' %in% names(status_df)) {
      has_five <-
        status_df$over_five
    } else {
      has_five <-
        FALSE
    }
    offering_data <-
      offering_data %>%
      mutate(
        dateInitialFiling = initial_date,
        typeEntity = typeEntity,
        locationJurisdiction = jurisdiction,
        hasOver5FileFilings = has_five,
        urlJSONFilings = url
      ) %>%
      select(
        idCIK,
        dateInitialFiling,
        typeEntity,
        locationJurisdiction,
        hasOver5FileFilings,
        dplyr::matches("nameIndustry"),
        dplyr::matches("typeFund"),
        dplyr::matches("^is"),
        dplyr::matches("^amount"),
        dplyr::matches("^count"),
        everything()
      ) %>%
      resolve_names_to_upper() %>%
      select(which(colMeans(is.na(.)) < 1))

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% cat(fill = T)
    }
    return(offering_data)
  }

.parse_json_fundraising <-
  function(url = "http://rankandfiled.com/data/filer/1138621/fundraising",
           nest_data = TRUE,
           return_message = TRUE) {
    if (!url %>% httr::url_ok() %>% suppressWarnings()) {
      return(tibble())
    }

    json_data <-
      url %>%
      jsonlite::fromJSON()

    fundraising_df <-
      json_data$results %>%
      as_tibble() %>%
      purrr::set_names(c(
        'idCIKs',
        'nameCompanies',
        'isCIKFiler',
        'namePerson',
        'offeringsValues'
      )) %>%
      mutate(
        idPerson = 1:n(),
        idCIK = url %>% str_replace_all('http://rankandfiled.com/data/filer/|/fundraising', '') %>% as.numeric(),
        namePerson = namePerson %>% str_replace_all('\\-', '') %>% stringr::str_to_upper() %>% str_trim(),
        urlJSONFundraising = url
      ) %>%
      suppressWarnings()

    company_name_df <-
      seq_along(fundraising_df$nameCompanies) %>%
      future_map_dfr(function(x) {
        company_name_data <-
          fundraising_df$nameCompanies[[x]]

        company_name_data <-
          company_name_data %>%
          str_split('\\*') %>%
          flatten_chr() %>%
          str_to_upper()

        df <-
          tibble(value = company_name_data) %>%
          mutate(item = 'nameCompanyFundraisingRelated') %>%
          mutate(countRow = 1:n()) %>%
          mutate(
            countRow = countRow - 1,
            item = ifelse(countRow == 0, item, item %>% paste0(countRow)),
            idPerson = x
          ) %>%
          select(-countRow)

        col_order <-
          c('idPerson', df$item)

        df <-
          df %>%
          spread(item, value) %>%
          select(one_of(col_order)) %>%
          resolve_names_to_upper()

        if (nest_data) {
          df <-
            df %>%
            nest(-idPerson, .key = dataCompaniesRelated)
        }
        return(df)
      })


    offering_value_df <-
      seq_along(fundraising_df$offeringsValues) %>%
      future_map_dfr(function(x) {
        offering_value_data <-
          fundraising_df$offeringsValues[[x]]

        offering_value_data <-
          offering_value_data %>%
          str_split('\\*') %>%
          flatten_chr()

        df <-
          tibble(offering = offering_value_data) %>%
          tidyr::separate(
            offering,
            into = c(
              'idCIKRelatedCompanyFundraising',
              'idIndustryRelatedCompanyFundRaising',
              'amountRaisedRelatedCompanyFundRaising'
            ),
            sep = '\\|'
          ) %>%
          mutate(countRow = 1:n()) %>%
          gather(item, value, -countRow) %>%
          mutate(
            countRow = countRow - 1,
            value = value %>% as.numeric(),
            item = ifelse(countRow == 0, item, item %>% paste0(countRow)),
            idPerson = x
          ) %>%
          select(-countRow)

        col_order <-
          c('idPerson', df$item)

        df <-
          df %>%
          spread(item, value) %>%
          select(one_of(col_order)) %>%
          resolve_names_to_upper()

        if (nest_data) {
          df <-
            df %>%
            nest(-idPerson, .key = dataOfferingValues)
        }
        return(df)
      })

    fundraising_df <-
      fundraising_df %>%
      left_join(company_name_df) %>%
      left_join(offering_value_df) %>%
      select(-c(idCIKs, nameCompanies, idPerson, offeringsValues)) %>%
      select(idCIK, namePerson, isCIKFiler, everything()) %>%
      suppressMessages() %>%
      suppressWarnings()

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% cat(fill = T)
    }
    return(fundraising_df)
  }

.parse_json_traders <-
  function(url = "http://rankandfiled.com/data/filer/1326801/traders",
           return_message = TRUE) {
    if (!url %>% httr::url_ok() %>% suppressWarnings()) {
      return(tibble())
    }

    json_data <-
      url %>%
      jsonlite::fromJSON()

    options(scipen = 9999)
    cik <-
      url %>% str_replace_all('http://rankandfiled.com/data/filer/|/traders', '') %>%
      as.numeric()
    traders <-
      json_data$owners$count

    df <-
      json_data$owners$owners %>%
      as_tibble() %>%
      purrr::set_names(c('nameEntityTrader', 'idCIKTrader', 'titleEntityTrader')) %>%
      mutate(
        nameEntityTrader = nameEntityTrader %>% str_to_upper(),
        idCIKTrader = idCIKTrader %>% as.numeric(),
        idCIK = cik
      ) %>%
      select(idCIK, everything()) %>%
      mutate(countTraders = traders) %>%
      resolve_names_to_upper()

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% cat(fill = T)
    }

    return(df)
  }

.parse_json_clevel <-
  function(url = "http://rankandfiled.com/data/filer/1326801/clevel",
           return_message = TRUE) {
    if (!url %>% httr::url_ok() %>% suppressWarnings()) {
      return(tibble())
    }

    json_data <-
      url %>%
      jsonlite::fromJSON()

    options(scipen = 9999)
    cik <-
      url %>% str_replace_all('http://rankandfiled.com/data/filer/|/clevel', '') %>%
      as.numeric()

    clevel_df <-
      json_data$clevel %>%
      as_tibble() %>%
      tidyr::separate(
        value,
        into = c(
          "idCIKCSuite",
          "nameEntityCSuite",
          "dateStartCSuite",
          "dateEndCSuite",
          "nameCSuiteRole",
          'codeCSuiteRole'
        ),
        sep = '\\*'
      ) %>%
      mutate(
        idCIKCSuite = idCIKCSuite %>% as.numeric(),
        nameEntityCSuite = nameEntityCSuite %>% str_to_upper(),
        idCIK = cik,
        dateStartCSuite = dateStartCSuite %>% lubridate::ymd(),
        dateEndCSuite = dateEndCSuite %>% lubridate::ymd()
      ) %>%
      select(idCIK,
             idCIKCSuite,
             nameEntityCSuite,
             codeCSuiteRole,
             everything()) %>%
      mutate(isActiveCSuite = ifelse(dateEndCSuite %>% is.na(), TRUE, FALSE)) %>%
      resolve_names_to_upper()

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% cat(fill = T)
    }

    return(clevel_df)
  }

.parse_json_mda <-
  function(url = "http://rankandfiled.com/data/filer/1326801/mda",
           return_message = TRUE) {
    if (!url %>% httr::url_ok() %>% suppressWarnings()) {
      return(tibble())
    }

    json_data <-
      url %>%
      jsonlite::fromJSON()

    options(scipen = 9999)

    cik <-
      url %>% str_replace_all('http://rankandfiled.com/data/filer/|/mda', '') %>%
      as.numeric()

    data <-
      json_data$results$matrix %>%
      as_tibble()

    names(data) <-
      json_data$results$dates %>% lubridate::ymd()
    words <-
      json_data$results$words

    data <-
      data %>%
      mutate(nameWord = words) %>%
      gather(date10K, countWord, -nameWord) %>%
      mutate(date10K = date10K %>% lubridate::ymd(),
             idCIK = cik) %>%
      select(idCIK, date10K, nameWord, countWord) %>%
      arrange(desc(date10K)) %>%
      resolve_names_to_upper()

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% cat(fill = T)
    }

    return(data)
  }

.parse_json_owners <-
  function(url = "http://rankandfiled.com/data/filer/1326801/owners",
           nest_data = TRUE,
           return_message = TRUE) {
    if (!url %>% httr::url_ok() %>% suppressWarnings()) {
      return(tibble())
    }

    json_data <-
      url %>%
      jsonlite::fromJSON()

    cik <-
      url %>% str_replace_all('http://rankandfiled.com/data/filer/|/owners', '') %>%
      as.numeric()

    general_df <-
      tibble(idCIK = cik,
             idCIKOwned = json_data$insiders$cik %>% as.numeric())

    has_filer <-
      'filer' %in% names(json_data$insiders)

    if (has_filer) {
      filing_df <-
        json_data$insiders$filer %>%
        as_tibble()

      filing_df <-
        filing_df %>%
        .resolve_name_df()

      filing_df <-
        filing_df %>%
        mutate_at(.vars = filing_df %>% select(dplyr::matches("nameEntity")) %>% names(),
                  funs(. %>% str_to_upper())) %>%
        resolve_names_to_upper()

      if ('name' %in% names(filing_df)) {
        filing_df <-
          filing_df %>%
          select(-name)
      }

      if ('sic' %in% names(filing_df)) {
        filing_df <-
          filing_df %>%
          dplyr::rename(idSIC = sic) %>%
          mutate(idSIC = idSIC %>% as.numeric())
      }

      names(filing_df) <-
        names(filing_df) %>% str_replace('OwnedBy', '') %>%
        paste0('Owner')
      if ('detailsOwner' %in% names(filing_df)) {
        detail_df <-
          seq_along(filing_df$detailsOwner) %>%
          future_map_dfr(function(x) {
            detail_value <-
              filing_df$detailsOwner[[x]]

            if (detail_value %>% is.na()) {
              df <-
                tibble(idRow = x, nameCompanyOwned = NA)

              if (nest_data) {
                df <-
                  df %>%
                  nest(-idRow, .key = dataCompaniesOwned)
              }
              return(df)
            }

            values <-
              detail_value %>% str_replace('\\|', '') %>%
              str_split('\\|') %>%
              flatten_chr()

            df_data <-
              tibble(value = values) %>%
              tidyr::separate(value,
                              into = c('idTickerOwned', 'other'),
                              sep = '\\:') %>%
              tidyr::separate(other,
                              into = c('nameCompanyOwned', 'other'),
                              sep = '\\_') %>%
              tidyr::separate(other,
                              into = c('roleOwned', 'dateOwned'),
                              sep = '\\#') %>%
              mutate(nameCompanyOwned = nameCompanyOwned %>% str_to_upper(),
                     idRow = x) %>%
              gather(item, value, -idRow, na.rm = TRUE) %>%
              group_by(item) %>%
              mutate(count = 1:n() - 1) %>%
              ungroup() %>%
              arrange((count)) %>%
              mutate(item = ifelse(count == 0, item, paste0(item, count))) %>%
              select(-count)

            column_order <-
              c('idRow', df_data$item)

            df_data <-
              df_data %>%
              spread(item, value) %>%
              select(one_of(column_order)) %>%
              resolve_names_to_upper()

            if (nest_data) {
              df_data <-
                df_data %>%
                nest(-idRow, .key = dataCompaniesOwned)
            }

            return(df_data)
          }) %>%
          suppressWarnings()

        detail_df <-
          detail_df %>%
          mutate_at(.vars = detail_df %>% select(dplyr::matches("date")) %>% names(),
                    funs(. %>% ymd())) %>%
          suppressWarnings()

        filing_df <-
          filing_df %>%
          mutate(idRow = 1:n()) %>%
          select(-detailsOwner) %>%
          left_join(detail_df) %>%
          select(-idRow) %>%
          suppressMessages()
      }

      general_df <-
        general_df %>%
        bind_cols(filing_df)
    }

    has_companies <-
      'companies' %in% names(json_data$insiders)

    if (has_companies) {
      company_df <-
        1:nrow(general_df) %>%
        future_map_dfr(function(x) {
          has_no_data <-
            json_data$insiders$companies[[x]] %>%
            nrow() == 0

          if (has_no_data) {
            df <-
              tibble(idRow = x, nameFiler = NA)
            if (nest_data) {
              df <-
                df %>%
                nest(idRow, .key = dataInsiderCompaniesOwned)
            }
          }

          company_df <-
            json_data$insiders$companies[[x]] %>%
            as_tibble() %>%
            .resolve_name_df() %>%
            mutate(idRow = x) %>%
            mutate(nameFiler = nameFiler %>% str_to_upper())

          if ('sic' %in% names(company_df)) {
            company_df <-
              company_df %>%
              dplyr::rename(idSICCompanyOwned = sic) %>%
              mutate(idSICCompanyOwned = idSICCompanyOwned %>% as.numeric())
          }

          df_data <-
            company_df %>%
            gather(item, value, -c(nameFiler, idRow)) %>%
            group_by(item) %>%
            mutate(count = 1:n() - 1) %>%
            ungroup() %>%
            arrange((count)) %>%
            mutate(item = ifelse(count == 0, item, paste0(item, count))) %>%
            select(-count)

          column_order <-
            c('idRow', 'nameFiler', df_data$item)

          df_data <-
            df_data %>%
            spread(item, value) %>%
            select(one_of(column_order)) %>%
            resolve_names_to_upper()

          if (nest_data) {
            df_data <-
              df_data %>%
              nest(-idRow, .key = dataInsiderCompaniesOwned)
          }
          return(df_data)
        })

      company_df <-
        company_df %>%
        mutate_at(.vars =
                    company_df %>% select(dplyr::matches("date")) %>% names(),
                  funs(. %>% lubridate::ymd())) %>%
        mutate_at(.vars =
                    company_df %>% select(dplyr::matches("idCIK")) %>% names(),
                  .funs = as.numeric) %>%
        mutate_at(
          .vars =
            company_df %>% select(dplyr::matches("nameCompany")) %>% names(),
          .funs = stringr::str_to_upper
        )

      general_df <-
        general_df %>%
        mutate(idRow = 1:n()) %>%
        left_join(company_df %>% select(-dplyr::matches("idCIKOwned"))) %>%
        select(-idRow) %>%
        suppressMessages()
    }

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% cat(fill = T)
    }

    general_df <-
      general_df %>%
      select(idCIK,
             idCIKOwned,
             nameEntityOwner,
             dplyr::matches("nameFiler"),
             everything()) %>%
      resolve_names_to_upper()

    return(general_df)
  }

.parse_json_public_filers <-
  function(url = "http://rankandfiled.com/data/filer/1680780/all?start=0",
           return_message = TRUE) {
    if (!url %>% httr::url_ok() %>% suppressWarnings()) {
      return(tibble())
    }

    json_data <-
      url %>%
      jsonlite::fromJSON()

    options(scipen = 9999)

    cik <-
      url %>% str_replace_all('http://rankandfiled.com/data/filer/|/all', '') %>%
      str_split('\\?') %>%
      flatten_chr() %>%
      .[[1]] %>%
      as.numeric()

    filing_df <-
      json_data$filings %>%
      as_tibble()

    filing_df <-
      filing_df %>%
      separate(
        value,
        into = c(
          "idRF",
          "idForm",
          "detailForm",
          "typeReport",
          "typeFiling",
          "slugSEC",
          "idSECSlug",
          "dateFiling",
          "X9"
        ),
        sep = '\\*'
      ) %>%
      select(-dplyr::matches("X")) %>%
      suppressMessages() %>%
      suppressWarnings()

    filing_df <-
      filing_df %>%
      mutate(
        idCIK = cik,
        pageSlug = idSECSlug %>% str_replace_all('\\-',''),
        urlSECFilingDirectory = ifelse(
          idSECSlug %>% str_detect('\\-'),
          list(
            "https://www.sec.gov/Archives/edgar/data/",
            idCIK,
            '/',
            pageSlug,
            '/',
            idSECSlug,
            '-index.htm'
          ) %>% purrr::reduce(paste0),
          NA
        ),
        urlSEC = ifelse(
          slugSEC == "None",
          NA,
          list(
            "https://www.sec.gov/Archives/edgar/data/",
            idCIK,
            '/',
            slugSEC
          ) %>% purrr::invoke(paste0, .)
        )
      ) %>%
      select(-pageSlug) %>%
      suppressWarnings()

    filing_df <-
      filing_df %>%
      mutate(
        typeFiling = typeFiling %>% str_to_upper(),
        dateFiling = dateFiling %>% as.numeric() %>% lubridate::ymd(),
        detailForm = ifelse(detailForm == '', NA, detailForm),
        typeReport = ifelse(typeReport == '', NA, typeReport),
        is13FFiling = (urlSEC %>% str_detect("xslForm13F")) &
          (typeFiling == "HOLDINGS")
      ) %>%
      tidyr::fill(dateFiling) %>%
      tidyr::fill(detailForm) %>%
      select(-slugSEC) %>%
      left_join(dictionary_sec_form_codes()) %>%
      tidyr::fill(nameForm) %>%
      select(idCIK, idRF, idForm, nameForm, everything()) %>%
      suppressMessages() %>%
      suppressWarnings() %>%
      resolve_names_to_upper()



    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% cat(fill = T)
    }
    return(filing_df)
  }

.parse_json_subsidiaries <-
  function(url = "http://rankandfiled.com/data/filer/34088/subsidiaries",
           return_message = TRUE) {
    if (!url %>% httr::url_ok() %>% suppressWarnings()) {
      return(tibble())
    }
    options(scipen = 9999)

    name_df <-
      tibble(
        nameRF = c(
          "cik",
          "country",
          "first_filed",
          "last_filed",
          "name",
          "percent"
        ),
        nameActual = c(
          'idCIK',
          'locationOrganizationSubsidiary',
          'dateFirstFiled',
          'dateLastFiled',
          'nameSubsidiary',
          'pctSubsidiaryOwned'
        )
      ) %>%
      mutate(idRow = 1:n())

    data <-
      url %>%
      jsonlite::fromJSON() %>%
      .$subsidiaries %>%
      as_tibble()

    rf_names <-
      data %>% names()

    has_missing_names <-
      rf_names[!rf_names %in% name_df$nameRF] %>% length() > 0

    if (has_missing_names) {
      df_has <-
        data %>%
        select(one_of(rf_names[rf_names %in% name_df$nameRF]))

      has_names <-
        names(df_has) %>%
        map_chr(function(x) {
          name_df %>%
            filter(nameRF == x) %>%
            filter(idRow == min(idRow)) %>%
            .$nameActual
        })

      df_has <-
        df_has %>%
        purrr::set_names(has_names)

      data <-
        df_has %>%
        bind_cols(data %>%
                    select(one_of(rf_names[!rf_names %in% name_df$nameRF])))

      data <-
        data %>%
        mutate_at(.vars =
                    data %>% select(
                      dplyr::matches(
                        "idCIK|idMidas|idIRS|^count|^price|^amount|^ratio|^pct|idMDA|^dateiso|idRF|price|amount|^year"
                      )
                    ) %>% names,
                  funs(. %>% as.character() %>% readr::parse_number())) %>%
        suppressWarnings()
      return(data)
    }

    actual_names <-
      names(data) %>%
      map_chr(function(x) {
        name_df %>%
          filter(nameRF == x) %>%
          filter(idRow == min(idRow)) %>%
          .$nameActual
      })

    data <-
      data %>%
      purrr::set_names(actual_names)

    data <-
      data %>%
      mutate(
        idCIK = idCIK %>% as.numeric(),
        nameSubsidiary = nameSubsidiary %>% str_to_upper(),
        locationOrganizationSubsidiary = locationOrganizationSubsidiary %>% str_to_upper()
      )
    has_pct <-
      'pctSubsidiaryOwned' %in% names(data)
    if (has_pct) {
      data <-
        data %>%
        mutate(
          pctSubsidiaryOwned = pctSubsidiaryOwned %>% as.numeric(),
          pctSubsidiaryOwned = pctSubsidiaryOwned / 100
        )
    }

    data <-
      data %>%
      mutate_at(.vars = data %>% select(dplyr::matches("date")) %>% names(),
                funs(. %>% lubridate::ymd()))

    data <-
      data %>%
      filter(!locationOrganizationSubsidiary %>% is.na()) %>%
      resolve_names_to_upper()

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% cat(fill = T)
    }
    return(data)
  }

.parse_cik_filings <-
  function(cik = 1527559,
           return_message = TRUE) {
    general_url <-
      list('http://rankandfiled.com/data/filer/', cik, '/general') %>%
      purrr::invoke(paste0, .)

    data_js <-
      general_url %>% jsonlite::fromJSON() %>% data.frame(stringsAsFactors = FALSE)

    is_public_company <-
      'company' %in% (data_js %>% names())

    is_insider <-
      'insider' %in% (data_js %>% names())
    if (is_public_company) {
      company_df <-
        general_url %>% jsonlite::fromJSON() %>% data.frame(stringsAsFactors = FALSE) %>%
        as_tibble()

      general_df <-
        .parse_company_general_safe(ticker = company_df$company)
    }

    if (is_insider) {
      general_df <-
        .parse_json_general_insider(cik = cik)
    }

    is_private_filer <-
      (!is_public_company) & (!is_insider)
    if (is_private_filer) {
      general_df <-
        general_url %>%
        .parse_json_general_filing()
    }

    filing_pages <-
      general_df$countFilings %/% 50
    if (filing_pages > 0) {
      filing_urls <-
        list(
          'http://rankandfiled.com/data/filer/',
          cik,
          '/all?start=',
          seq(0, by = 50, length.out = filing_pages)
        ) %>%
        purrr::invoke(paste0, .)
    }

    if (filing_pages == 0) {
      filing_urls <-
        list('http://rankandfiled.com/data/filer/',
             cik,
             '/all?start=0') %>%
        purrr::invoke(paste0, .)
    }

    .parse_json_public_filers_safe <-
      purrr::possibly(.parse_json_public_filers, NULL)

    .all_filings <-
      filing_urls %>%
      future_map_dfr(function(x) {
        .parse_json_public_filers_safe(url = x, return_message = return_message)
      }) %>%
      distinct() %>%
      suppressWarnings()

    entity <-
      general_df$nameEntity %>%
      str_to_upper()

    .all_filings <-
      .all_filings %>%
      mutate(nameEntity = entity) %>%
      select(idCIK, nameEntity, dateFiling,
             dplyr::matches("idRF"),
             everything())

    if ('typeReport' %in% names(.all_filings)) {
      report_dict_df <-
        dictionary_sec_filing_codes()

      report_df <-
        .all_filings %>%
        mutate(idRow = 1:n()) %>%
        select(typeReport, idRow) %>%
        filter(!typeReport %>% is.na())

      report_df <-
        1:nrow(report_df) %>%
        future_map_dfr(function(x) {
          is_none <-
            report_df$typeReport[[x]] == 'None'

          if (is_none) {
            return(tibble(
              idRow = report_df$idRow[[x]],
              idFormType = 'None',
              nameFormType = NA
            ))
          }

          row_df <-
            report_df %>%
            slice(x)
          reports <-
            row_df$typeReport %>%
            str_split('\\|') %>%
            flatten_chr()

          item_df <-
            tibble(idFormType = reports, idRow = row_df$idRow) %>%
            left_join(report_dict_df) %>%
            gather(item, value, -idRow) %>%
            group_by(item) %>%
            mutate(countItems = 1:n() - 1) %>%
            ungroup() %>%
            mutate(item = ifelse(countItems == 0, item, paste0(item, countItems))) %>%
            arrange(countItems) %>%
            select(-countItems) %>%
            suppressMessages()

          col_order <-
            c('idRow', item_df$item)

          item_df <-
            item_df %>%
            spread(item, value) %>%
            select(one_of(col_order))
          return(item_df)

        })

      .all_filings <-
        .all_filings %>%
        mutate(idRow = 1:n()) %>%
        dplyr::rename(typesReport = typeReport) %>%
        left_join(report_df) %>%
        suppressMessages() %>%
        select(-idRow)
    }


    if (return_message) {
      list(
        "Parsed ",
        .all_filings %>% nrow() %>% formattable::comma(digits = 0),
        ' SEC Filings for ',
        entity
      ) %>%
        purrr::invoke(paste0, .) %>%
        cat(fill = T)
    }
    .all_filings <-
      .all_filings %>%
      resolve_names_to_upper()
    return(.all_filings)
  }

.parse_cik_data <-
  function(cik = 899689,
           nest_data = TRUE,
           tables = NULL,
           return_message = TRUE) {
    url_df <-
      cik %>%
      .get_cik_url_df()

    table_options <-
      c(
        'General',
        'CIK Filings',
        'Filings',
        'Private Offerings',
        'Related Parties',
        'Traders',
        'C Level',
        'MDA',
        'Owners',
        'Insider Trades',
        'Trades',
        'Subsidiaries'
      )

    null_tables <-
      length(tables) == 0
    if (null_tables) {
      tables <-
        c(
          'General',
          'CIK Filings',
          'Filings',
          'Private Offerings',
          'Related Parties',
          'Traders',
          'C Level',
          'MDA',
          'Owners',
          'Insider Trades',
          'Trades',
          'Subsidiaries'
        )
    }
    missing_tables <-
      (tables %>% str_to_upper()) %in% (table_options %>% str_to_upper()) %>% sum() == 0
    if (missing_tables) {
      stop(list(
        "Sorry Tables Can Only Be:",
        '\n',
        paste0(table_options, collapse = '\n')
      ) %>%
        purrr::invoke(paste0, .))
    }

    table_options <-
      table_options %>% str_to_upper()

    tables <-
      tables %>% str_to_upper()

    if (!'GENERAL' %in% tables) {
      tables <-
        tables %>%
        append('GENERAL')
    }

    has_general <-
      'general' %>% str_to_upper() %>% str_detect(tables) %>% sum() > 0

    has_filings <-
      'filings' %>% str_to_upper() %>% str_detect(tables) %>% sum() > 0

    has_cik_filings <-
      'cik filings' %>% str_to_upper() %>% str_detect(tables) %>% sum() > 0

    has_private <-
      'private offerings' %>% str_to_upper() %>% str_detect(tables) %>% sum() > 0

    has_related <-
      'related parties' %>% str_to_upper() %>% str_detect(tables) %>% sum() > 0

    has_traders <-
      'traders' %>% str_to_upper() %>% str_detect(tables) %>% sum() > 0

    has_clevel <-
      'c level' %>% str_to_upper() %>% str_detect(tables) %>% sum() > 0

    has_mda <-
      'mda' %>% str_to_upper() %>% str_detect(tables) %>% sum() > 0

    has_owners <-
      'owners' %>% str_to_upper() %>% str_detect(tables) %>% sum() > 0

    has_insider_trades <-
      'insider trades' %>% str_to_upper() %>% str_detect(tables) %>% sum() > 0

    has_subs <-
      'subsidiaries' %>% str_to_upper() %>% str_detect(tables) %>% sum() > 0

    if (has_general) {
      .parse_json_general_filing_safe <-
        purrr::possibly(.parse_json_general_filing, tibble())
      general_df <-
        url_df$urlJSON[[1]] %>%
        .parse_json_general_filing(nest_data = nest_data,
                                   return_message = return_message) %>%
        mutate(nameEntity = nameEntity %>% str_to_upper()) %>%
        as_tibble()

      if (general_df %>% nrow() == 0) {
        general_df <-
          tibble(idCIK = cik,
                 nameEntity = NA)
      }
    } else {
      general_df <-
        tibble(idCIK = cik)
    }


    if (has_filings) {
      .parse_json_filings_safe <-
        purrr::possibly(.parse_json_filings, tibble())

      filing_df <-
        url_df$urlJSON[[2]] %>%
        .parse_json_filings_safe(return_message = return_message) %>%
        mutate_if(is_character,
                  str_to_upper)

      has_rows  <-
        filing_df %>% nrow() > 0

      if (has_rows) {
        filing_df <-
          filing_df %>%
          left_join(general_df %>% select(idCIK, nameEntity)) %>%
          select(nameEntity, idCIK, everything()) %>%
          suppressMessages()
      }
    } else {
      filing_df <-
        tibble(idCIK = cik)
    }


    if (has_private) {
      .parse_json_private_safe <-
        purrr::possibly(.parse_json_private, tibble())

      private_df <-
        url_df$urlJSON[[3]] %>%
        .parse_json_private_safe(nest_data = nest_data,
                                 return_message = return_message)

      has_rows  <-
        private_df %>% nrow() > 0

      if (has_rows) {
        private_df <-
          private_df %>%
          left_join(general_df %>% select(idCIK, nameEntity)) %>%
          select(nameEntity, idCIK, everything()) %>%
          suppressMessages()
      }
    } else {
      private_df <-
        tibble(idCIK = cik)
    }


    if (has_related) {
      .parse_json_fundraising_safe <-
        purrr::possibly(.parse_json_fundraising, tibble())

      fundraising_df <-
        url_df$urlJSON[[4]] %>%
        .parse_json_fundraising_safe(nest_data = nest_data,
                                     return_message = return_message)

      has_rows  <-
        fundraising_df %>% nrow() > 0
      if (has_rows) {
        fundraising_df <-
          fundraising_df %>%
          left_join(general_df %>% select(idCIK, nameEntity)) %>%
          select(nameEntity, idCIK, everything()) %>%
          suppressMessages()
      }
    } else {
      fundraising_df <-
        tibble(idCIK = cik)
    }

    if (has_traders) {
      .parse_json_traders_safe <-
        purrr::possibly(.parse_json_traders, tibble())

      traders_df <-
        url_df$urlJSON[[5]] %>%
        .parse_json_traders_safe(return_message = return_message)

      has_rows  <-
        traders_df %>% nrow() > 0
      if (has_rows) {
        traders_df <-
          traders_df %>%
          left_join(general_df %>% select(idCIK, nameEntity)) %>%
          select(nameEntity, idCIK, everything()) %>%
          suppressMessages()
      }
    } else {
      traders_df <-
        tibble(idCIK = cik)
    }


    if (has_clevel) {
      .parse_json_clevel_safe <-
        purrr::possibly(.parse_json_clevel, tibble())

      clevel_df <-
        url_df$urlJSON[[6]] %>%
        .parse_json_clevel_safe(return_message = return_message)

      has_rows  <-
        clevel_df %>% nrow() > 0
      if (has_rows) {
        clevel_df <-
          clevel_df %>%
          left_join(general_df %>% select(idCIK, nameEntity)) %>%
          select(nameEntity, idCIK, everything()) %>%
          suppressMessages()
      }
    } else {
      clevel_df <-
        tibble(idCIK = cik)
    }

    if (has_mda) {
      .parse_json_mda_safe <-
        purrr::possibly(.parse_json_mda, tibble())

      mda_df <-
        url_df$urlJSON[[7]] %>%
        .parse_json_mda_safe(return_message = return_message)

      has_rows  <-
        mda_df %>% nrow() > 0

      if (has_rows) {
        mda_df <-
          mda_df %>%
          left_join(general_df %>% select(idCIK, nameEntity)) %>%
          select(nameEntity, idCIK, everything()) %>%
          suppressMessages()
      }
    } else {
      mda_df <-
        tibble(idCIK = cik)
    }

    if (has_owners) {
      .parse_json_owners_safe <-
        purrr::possibly(.parse_json_owners, tibble())

      owners_df <-
        url_df$urlJSON[[8]] %>%
        .parse_json_owners_safe(nest_data = nest_data,
                                return_message = return_message)

      if ('idTypeFilerOwner' %in% names(owners_df)) {
        owners_df <-
          owners_df %>%
          left_join(.filer_type_df()) %>%
          select(idCIK:nameEntityOwner, typeFilerOwner, everything()) %>%
          suppressMessages()
      }

      has_rows  <-
        owners_df %>% nrow() > 0
      if (has_rows) {
        owners_df <-
          owners_df %>%
          left_join(general_df %>% select(idCIK, nameEntity)) %>%
          select(nameEntity, idCIK, everything()) %>%
          select(-dplyr::matches("dateiso")) %>%
          suppressMessages()
      }
    } else {
      owners_df <-
        tibble(idCIK = cik)
    }

    if (has_cik_filings) {
      .parse_cik_filings_safe <-
        purrr::possibly(.parse_cik_filings, tibble())

      cik_filing_df <-
        .parse_cik_filings_safe(cik = cik, return_message = return_message)
    } else {
      cik_filing_df <-
        tibble(idCIK = cik)
    }

    if (has_insider_trades) {
      parse_insider_trades_safe <-
        purrr::possibly(.parse_insider_trades, tibble())

      insider_trade_df <-
        parse_insider_trades_safe(cik = cik,
                                  nest_data = nest_data,
                                  return_message = return_message)

      has_rows  <-
        insider_trade_df %>% nrow() > 0

      if (has_rows) {
        insider_trade_df <-
          insider_trade_df %>%
          left_join(general_df %>% select(idCIK, nameEntity)) %>%
          select(nameEntity, idCIK, everything()) %>%
          suppressMessages()
      }
    } else {
      insider_trade_df <-
        tibble(idCIK = cik)
    }

    if (has_subs) {
      .parse_json_subsidiaries_safe <-
        purrr::possibly(.parse_json_subsidiaries, tibble())

      sub_df <-
        url_df$urlJSON[[9]] %>%
        .parse_json_subsidiaries(return_message = return_message)

      has_rows  <-
        sub_df %>% nrow() > 0
      if (has_rows) {
        sub_df <-
          sub_df %>%
          left_join(general_df %>% select(idCIK, nameEntity)) %>%
          select(nameEntity, idCIK, everything()) %>%
          suppressMessages()
      }
    } else {
      sub_df <-
        tibble(idCIK = cik)
    }

    if ('nameEntity' %in% names(general_df)) {
      nameEntity <-
        general_df$nameEntity %>%
        str_to_upper()
    } else {
      nameEntity <-
        NA
    }


    all_data <-
      tibble(
        idCIK = cik,
        nameEntity,
        nameTable = c(
          'General',
          'CIK Filings',
          'Filings',
          'Private Offerings',
          'Related Parties',
          'Traders',
          'C Level',
          'MDA',
          'Owners',
          'Insider Trades',
          'Subsidiaries'
        ),
        dataTable = list(
          general_df,
          cik_filing_df,
          filing_df,
          private_df,
          fundraising_df,
          traders_df,
          clevel_df,
          mda_df,
          owners_df,
          insider_trade_df,
          sub_df
        )
      )

    if (return_message) {
      list("\nParsed SEC Private Filing Data for CIK: ",
           cik,
           ' - ',
           nameEntity,
           "\n") %>%
        purrr::invoke(paste0, .) %>%
        cat(fill = T)
    }

    all_data <-
      all_data %>%
      mutate(countCols = dataTable %>% purrr::map_dbl(ncol)) %>%
      filter(countCols > 1) %>%
      suppressWarnings() %>%
      select(-dplyr::matches("countCols")
      )

    all_data
  }

#' SEC filer
#'
#' This is function imports data
#' for a specified SEC filing entity.  An
#' SEC filing entity can be a person, public company or
#' private filer.  This function requires that the entity has a
#' Central Index Key [CIK].
#'
#' The function acquires information for the
#' specified tables and will auto parse forms if the filer has them and
#' the user activates these parameters.
#'
#' @param entity_names vector names to search
#' @param tickers character vector of ticker symbols to search
#' @param ciks numeric vector of CIKs
#' @param tables tables to include if they exist \itemize{
#' \code{NULL, General, CIK Filings, Filings, Private Offerings, Related Parties, Traders, C Level, MDA, Owners, Insider Trades, Trades}
#' \code{NULL}: selects all tables
#' \item \code{General}: general information about the filer
#' \item \code{CIK Filings}: summarised filings for a CIK
#' \item \code{Filings}: summarised filings for an entity, slightly different than \code{CIK Filings}
#' \item \code{Private Offerings}: parses any private offerings
#' \item \code{Related Parties}:  parses any related parties [people]
#' \item \code{Traders}: parses major traders
#' \item \code{C Level}: parses information about executives
#' \item \code{MDA}: parses text from company 10-K Management Discussion and Analysis [MDA] section
#' \item \code{Owners}: parses information about major owners
#' \item \code{Insider Trades}: parses insider trade information
#' \item \code{Trades}: parses all trade information
#' }
#' @param parse_all_filing_url_data \code{TRUE} parses every SEC fling link
#' @param parse_xbrl \code{TRUE} parse XBRL for public companies, data starts in 2009
#' @param parse_subsidiaries \code{TRUE} parse all filer subsidiaries (default)
#' @param parse_13F \code{TRUE} parse \href{https://en.wikipedia.org/wiki/Form_13F}{13F's} for institutional managers
#' @param parse_asset_files \code{TRUE} parses ABS XML for \href{https://www.sec.gov/info/edgar/specifications/absxml.htm}{ABS Asset Data}
#' filing entities (default)
#' @param parse_small_offerings \code{TRUE} parses \href{https://www.sec.gov/info/smallbus/secg/rccomplianceguide-051316.htm}{Regulation CrowdFunding}
#' Form 1-A data if any exists for a filer (default)
#' @param nest_data return a nested data frame \code{TRUE, FALSE}
#' @param assign_to_environment \code{true} assigns individual data frames to your environment
#' @param return_message \code{TRUE} return a message after data import
#' @import dplyr tidyr purrr stringr formattable readr lubridate XBRL curl jsonlite lazyeval
#' @importFrom jsonlite fromJSON
#' @export
#' @return where \code{nest_data} is \code{TRUE} a nested tibble by asset,
#' where \code{nest_data} is \code{FALSE} a tibble
#' @family SEC
#' @family Rank and Filed
#' @family XBRL
#' @family entity search
#' @family fund search
#' @export
#'
#' @examples
#' \dontrun{
#' sec_filer(entity_names = 'HLT Holdco', tickers = c('FB'),
#' nest_data = TRUE, parse_subsidiaries = TRUE, parse_all_filing_url_data = TRUE,
#' parse_13F = TRUE, assign_to_environment = TRUE,
#' return_message = TRUE)
#'
#' ## Small Asset Filer Example
#'
#' ## ABS Example
#'
#' #XBRL Example
#'
#'}
sec_filer <-
  function(entity_names = NULL,
           tickers = NULL,
           ciks = NULL,
           tables = NULL,
           nest_data = FALSE,
           parse_all_filing_url_data = FALSE,
           parse_xbrl = FALSE,
           parse_subsidiaries = FALSE,
           parse_13F = FALSE,
           parse_asset_files = FALSE,
           parse_small_offerings = FALSE,
           parse_complete_text_filings = FALSE,
           parse_form_d = FALSE,
           parse_form_3_4s = FALSE,
           assign_to_environment = TRUE,
           return_message = TRUE) {
    has_entities <-
      (('entity_names' %>% exists()) &
         (!entity_names %>% purrr::is_null()))

    has_ciks <-
      (('ciks' %>% exists()) & (!ciks %>% purrr::is_null()))

    has_tickers <-
      (('tickers' %>% exists()) & (!tickers %>% purrr::is_null()))

    has_nothing <-
      ((!has_ciks) & (!has_entities) & (!has_tickers))

    has_tables <-
      (!tables %>% purrr::is_null()) #(('tables' %>% exists()) |

    if (has_nothing) {
      stop("Please enter a CIK, ticker, or an entity name")
    }

    all_ciks <-
      c()

    if (has_entities) {
      sec_filing_entities_safe <-
        purrr::possibly(sec_filing_entities, tibble())

      search_df <-
        entity_names %>%
        sec_filing_entities_safe(return_message = return_message)

      has_rows <-
        search_df %>% nrow() > 0

      if (has_rows) {
        search_ciks <-
          search_df %>%
          .$idCIK
        all_ciks <-
          all_ciks %>%
          append(search_ciks)
      }
    }

    if (has_ciks) {
      all_ciks <-
        all_ciks %>%
        append(ciks)
    }

    .parse_cik_data_safe <-
      possibly(.parse_cik_data, NULL)

    if (all_ciks %>% length() > 0) {
      all_data <-
        all_ciks %>%
        sort() %>%
        future_map_dfr(function(x) {
          .parse_cik_data_safe(
            tables = tables,
            nest_data = nest_data,
            cik = x,
            return_message = return_message
          )
        }) %>%
        mutate(
          urlRankAndFiled =
            list('http://rankandfiled.com/#/filers/', idCIK, '/filings') %>% purrr::invoke(paste0, .)
        ) %>%
        select(idCIK, nameEntity, urlRankAndFiled, nameTable, dataTable) %>%
        distinct() %>%
        suppressWarnings()
    }

    if (has_tickers) {
      .parse_ticker_data_safe <-
        purrr::possibly(.parse_ticker_data, tibble())

      table_exists <-
        'all_data' %>% exists()

      if (table_exists) {
        all_ticker_data <-
          tickers %>%
          future_map_dfr(function(x) {
            .parse_ticker_data(
              ticker = x,
              nest_data = nest_data,
              tables = tables,
              return_message = return_message
            )
          }) %>%
          suppressWarnings()

        all_data <-
          all_data %>%
          bind_rows(all_ticker_data)
      } else {
        all_data <-
          tickers %>%
          future_map_dfr(function(x) {
            .parse_ticker_data_safe(ticker = x,
                                    tables = tables,
                                    return_message = return_message)
          }) %>%
          suppressWarnings()
      }
    }

    if (has_tables) {
      table_options <-
        c(
          'General',
          'CIK Filings',
          'Filings',
          'Private Offerings',
          'Related Parties',
          'Traders',
          'C Level',
          'MDA',
          'Owners',
          'Insider Trades',
          'Trades'
        )
      table_names <-
        tables %>% str_to_lower() %>% paste0(collapse = "|")

      wrong_table <-
        table_options %>% str_to_lower() %>% str_count(table_names) %>% sum() == 0

      if (wrong_table) {
        stop("Sorry tables can only be:\n" %>% paste0(paste0(table_options, collapse = '\n')))
      }

      all_data <-
        all_data %>%
        mutate(table = nameTable %>% str_to_lower()) %>%
        filter(table %>% str_detect(table_names)) %>%
        select(-table)
    }

    if (!'all_data' %>% exists()) {
      return(tibble())
    }

    missing_ciks <-
      all_ciks[!all_ciks %in% all_data$idCIK] %>% length() > 0

    if (missing_ciks) {
      list("Missing ", all_ciks[!all_ciks %in% all_data$idCIK] %>% paste(collapse = ', ')) %>%
        purrr::invoke(paste0, .) %>%
        cat(fill = T)
    }

    all_data <-
      all_data %>%
      select(-dplyr::matches("urlRankAndFiled"))

    has_filings <-
      c('CIK Filings', 'Filings') %in% all_data$nameTable %>% sum() > 0

    if (has_filings) {
      filing_df <-
        all_data %>%
        filter(nameTable %in% c('Filings', 'CIK Filings')) %>%
        select(dataTable) %>%
        unnest() %>%
        distinct()


      filing_df <-
        filing_df %>%
        mutate_at(filing_df %>% select(dplyr::matches("^url")) %>% names(),
                  funs(. %>% str_to_lower()))


      filing_df <-
        filing_df %>%
        mutate_at(filing_df %>% select(dplyr::matches("url^[A-Z]")) %>% names(),
                  funs(. %>% str_replace_all('archives', 'Archives')))

      filing_df <-
        filing_df %>%
        mutate(
          urlSECFilingDirectory = urlSECFilingDirectory %>% gsub('archives', 'Archives', .),
          urlSEC = urlSEC %>% gsub('archives', 'Archives', .)
        )

      has_subsidiaries <-
        (filing_df %>%
           filter(typeFiling == "SUBSIDIARIES OF THE REGISTRANT") %>%
           nrow() > 0) & (parse_subsidiaries)

      if (has_subsidiaries) {
        parse_sec_subsidiary_url_safe <-
          purrr::possibly(.parse_sec_subsidiary_url, tibble())

        has_list <-
          filing_df %>%
          filter(typeFiling == "LIST OF SUBSIDIARIES") %>%
          nrow() > 0

        sub_url_df <-
          filing_df %>%
          filter(
            typeFiling %in% c(
              "SUBSIDIARIES OF THE REGISTRANT",
              "SUBSIDIARIES OF HOLDING COMPANY"
            )
          ) %>%
          select(dateFiling, nameEntity, urlSEC) %>%
          distinct()

        if (has_list) {
          sub_url_list_df <-
            filing_df %>%
            filter(
              typeFiling %>% str_detect(
                "LIST OF SUBSIDIARIES|LIST OF SIGNIFICANT SUBSIDIARIES|LIST OF SIGNIFCANT"
              )
            ) %>%
            select(dateFiling, nameEntity, urlSEC) %>%
            distinct()

          if ('sub_url_df' %>% exists()) {
            sub_url_df <-
              sub_url_list_df %>%
              bind_rows(sub_url_df)
          } else {
            sub_url_df <-
              sub_url_list_df
          }
        }

        sub_df <-
          sub_url_df %>%
          arrange(dateFiling) %>%
          .$urlSEC %>%
          future_map_dfr(function(x) {
            parse_sec_subsidiary_url_safe(url = x, return_message = return_message)
          }) %>%
          suppressWarnings()

        if (sub_df %>% nrow() > 0) {
          sub_df <-
            sub_df %>%
            select(-dplyr::matches("X|date")) %>%
            filter(
              !nameSubsidiary %in% c(
                '(I)',
                '(II)',
                '(III)',
                '(IV)',
                '(V)',
                '(VI)',
                '(VII)',
                '(VIII)',
                '(IX)',
                '(X)',
                'PART A'
              )
            ) %>%
            left_join(sub_url_df) %>%
            select(idCIK, dateFiling, everything()) %>%
            suppressMessages() %>%
            distinct()

          active_date_df <-
            sub_df %>%
            group_by(nameSubsidiary) %>%
            summarise(
              dateFirstFiled = min(dateFiling, na.rm = TRUE),
              dateLastFiled = max(dateFiling, na.rm = TRUE),
              isActiveSubsidiary = ifelse(
                dateLastFiled == sub_df$dateFiling %>% max(na.rm = TRUE),
                TRUE,
                FALSE
              )
            ) %>%
            ungroup()

          sub_df <-
            sub_df %>%
            left_join(active_date_df) %>%
            left_join(sub_url_df) %>%
            suppressMessages()

          sub_df <-
            sub_df %>%
            mutate(nameSubsidiaryRF = nameSubsidiary %>% str_replace_all('\\,|\\.', '')) %>%
            select(idCIK, nameEntity, dateFiling, everything()) %>%
            suppressMessages()

          has_sub_df <-
            'Subsidiaries' %in% all_data$nameTable

          if (has_sub_df) {
            ad_sub_df <-
              all_data %>%
              filter(nameTable == 'Subsidiaries') %>%
              select(dataTable) %>%
              unnest()

            if ('pctSubsidiaryOwned' %in% names(ad_sub_df)) {
              sub_df <-
                sub_df %>%
                left_join(
                  ad_sub_df %>%
                    select(nameSubsidiaryRF = nameSubsidiary, pctSubsidiaryOwned) %>%
                    distinct()
                ) %>%
                suppressMessages() %>%
                select(-nameSubsidiaryRF)
            }

            if (nest_data) {
              sub_df <-
                sub_df %>%
                nest(-c(dateFiling, idCIK, nameEntity), .key = dataSubsidiaries)
            }
            a_sub_df <-
              sub_df %>%
              group_by(idCIK, nameEntity) %>%
              nest(-c(idCIK, nameEntity), .key = dataTable) %>%
              ungroup() %>%
              mutate(nameTable = 'Subsidiaries')

            all_data <-
              all_data %>%
              filter(!nameTable == 'Subsidiaries') %>%
              bind_rows(a_sub_df)

          } else {
            if (nest_data) {
              sub_df <-
                sub_df %>%
                nest(-c(dateFiling, idCIK, nameEntity), .key = dataSubsidiaries)
            }
            a_sub_df <-
              sub_df %>%
              group_by(idCIK, nameEntity) %>%
              nest(-c(idCIK, nameEntity), .key = dataTable) %>%
              ungroup() %>%
              mutate(nameTable = 'Subsidiaries')

            all_data <-
              all_data %>%
              filter(!nameTable == 'Subsidiaries') %>%
              bind_rows(a_sub_df)
          }
        }
      }

      parse_for_tables_rf_safe <-
        purrr::possibly(.parse_for_tables_rf, tibble())

      tables_edgar <-
        parse_for_tables_rf_safe(
          filing_df = filing_df,
          parse_complete_text_filings = parse_complete_text_filings,
          parse_form_d = parse_form_d,
          parse_13F = parse_13F,
          parse_small_offerings = parse_small_offerings,
          parse_form_3_4s = parse_form_3_4s,
          parse_asset_files = parse_asset_files,
          parse_xbrl = parse_xbrl
        )
      has_edgar_tables <-
        tables_edgar %>% nrow() > 0

      if (has_edgar_tables) {

        all_data <-
          all_data %>%
          nest(-nameTable, .key = dataTable) %>%
          bind_rows(tables_edgar)

      }
    }

    if (assign_to_environment) {
      table_name_df <-
        all_data %>%
        select(nameTable) %>%
        distinct() %>%
        mutate(
          nameDF =
            list('dataFiler', nameTable %>% str_replace_all('\\ ', '')) %>% purrr::invoke(paste0, .)
        )

      1:nrow(table_name_df) %>%
        walk(function(x) {
          df_name <-
            table_name_df %>% slice(x) %>% .$nameDF
          df_name %>% cat(fill = T)
          df_data <-
            all_data %>%
            filter(nameTable == table_name_df$nameTable[[x]]) %>%
            select(dplyr::matches(c('idCIK|nameEntity|dataTable'))) %>%
            unnest() %>%
            suppressWarnings() %>%
            remove_duplicate_columns()

          has_unnest2 <-
            names(df_data) %>% str_detect('data') %>% sum(na.rm = TRUE) > 1

          if (has_unnest2) {
            base_names <-
              df_data %>% remove_duplicate_columns() %>% dplyr::select(-dplyr::matches("data")) %>% names()

            df_data_names <-
              names(df_data)[names(df_data) %>% str_detect('data')]

            for (x in seq_along(df_data_names)) {
              df_data_name <-
                df_data_names[[x]]
              table <-
                df_data %>%
                select(one_of(c(base_names, df_data_name))) %>%
                remove_duplicate_columns()
              is_null_col <-
                table[,df_data_name] %>% magrittr::extract2(1) %>% map_lgl(is_null)

              table <-
                table %>%
                mutate(is_null_col) %>%
                filter(!is_null_col) %>%
                unnest() %>%
                remove_duplicate_columns() %>%
                select(which(colMeans(is.na(.)) < 1)) %>%
                # tidy_column_formats() %>%
                select(-dplyr::matches('is_null_col')) %>%
                distinct()

              df_table_name <-
                list(df_name, df_data_name %>% str_replace_all('data', '')) %>% purrr::reduce(paste0)
              assign(x = df_table_name,
                     eval(table),
                     envir = .GlobalEnv)
            }

          } else {
            has_unnest <-
              df_data %>% names() %>% str_detect('data') %>% sum(na.rm = TRUE) > 0
            if (has_unnest) {
              if (df_name %>% str_detect("General")) {
                table <-
                  df_data %>%
                  remove_duplicate_columns() %>%
                  select(-dplyr::matches("data")) %>%
                  # tidy_column_formats() %>%
                  select(which(colMeans(is.na(.)) < 1)) %>%
                  distinct()
                assign(x = df_name,
                       eval(table),
                       envir = .GlobalEnv)
              }
              if (df_name %in% 'dataFilerTextFilings') {
                table <-
                  df_data %>%
                  unnest() %>%
                  select(which(colMeans(is.na(.)) < 1)) %>%
                  tidy_column_formats() %>%
                  distinct()
                assign(x = df_name,
                       eval(table),
                       envir = .GlobalEnv)
              }
              if (df_name %in% 'dataFilerFilingDirectories') {
                table <-
                  df_data %>%
                  select(-dplyr::matches('data')) %>%
                  filter(!idCIK %>% is.na()) %>%
                  select(which(colMeans(is.na(.)) < 1)) %>%
                  # tidy_column_formats() %>%
                  distinct()
                assign(x = df_name,
                       eval(table),
                       envir = .GlobalEnv)
              }

              other <-
                (!df_name %>% str_detect("General")) & (!df_name %in% c('dataFilerFilingDirectories', 'dataFilerTextFilings'))

              if (other) {
                df_data <-
                  df_data %>%
                  remove_duplicate_columns() %>%
                  # select(dplyr::matches("data")) %>%
                  unnest()

                select_cols <-
                  tibble(nameData = names(df_data)) %>%
                  mutate(idColumn = 1:n()) %>%
                  group_by(nameData) %>%
                  mutate(countColumn = 1:n()) %>%
                  ungroup() %>%
                  filter(countColumn == min(countColumn)) %>%
                  .$idColumn

                df_data <-
                  df_data[, select_cols]

                table <-
                  df_data %>%
                  select(which(colMeans(is.na(.)) < 1)) %>%
                  # tidy_column_formats() %>%
                  distinct()
                assign(x = df_name,
                       eval(table),
                       envir = .GlobalEnv)
              }
            } else {
              table <-
                df_data %>%
                select(which(colMeans(is.na(.)) < 1)) %>%
                # tidy_column_formats() %>%
                distinct()
              assign(x = df_name,
                     eval(table),
                     envir = .GlobalEnv)
            }
          }
        })
    }

    return(all_data)
  }


# insider -----------------------------------------------------------------

.parse_json_general_insider <-
  function(cik = 1354879,
           nest_data = TRUE,
           return_message = TRUE) {
    url <-
      list('http://rankandfiled.com/data/insider/', cik, '/general') %>%
      purrr::invoke(paste0, .)
    if (!url %>% httr::url_ok()) {
      return(tibble())
    }

    data <-
      url %>%
      jsonlite::fromJSON() %>%
      .[['insider']]

    general_cols <-
      data %>% future_map_dfr(class) %>%
      gather(item, value) %>%
      filter(!value %>% str_detect(c('list', 'data.frame'))) %>%
      .$item %>%
      suppressWarnings()

    general_df <-
      data %>%
      data.frame(stringsAsFactors = FALSE) %>%
      dplyr::select(one_of(general_cols)) %>%
      .resolve_name_df() %>%
      distinct()

    has_filer <-
      'filer' %in% names(data)

    if (has_filer) {
      filing_df <-
        data$filer %>%
        flatten_df() %>%
        .resolve_name_df()

      if ('name' %in% names(filing_df)) {
        filing_df <-
          filing_df %>%
          select(-name)
      }

      if ('detailsOwnedBy' %in% names(filing_df)) {
        filing_df <-
          filing_df %>%
          dplyr::rename(detailsOwns = detailsOwnedBy)
      }

      if ('detailsOwns' %in% names(filing_df)) {
        detail_df <-
          seq_along(filing_df$detailsOwns) %>%
          future_map_dfr(function(x) {
            detail_value <-
              filing_df$detailsOwns[[x]]

            if (detail_value %>% is.na()) {
              df <-
                tibble(idRow = x, nameCompanyOwns = NA)
              if (nest_data) {
                df <-
                  df %>%
                  nest(-idRow, .key = dataInsiderCompanies)
              }
              return(df)
            }

            values <-
              detail_value %>% str_replace('\\|', '') %>%
              str_split('\\|') %>%
              flatten_chr()

            df_data <-
              tibble(value = values) %>%
              tidyr::separate(value,
                              into = c('idTickerOwns', 'other'),
                              sep = '\\:') %>%
              tidyr::separate(other,
                              into = c('nameCompanyOwns', 'other'),
                              sep = '\\_') %>%
              tidyr::separate(other,
                              into = c('roleOwner', 'dateOwner'),
                              sep = '\\#') %>%
              mutate(nameCompanyOwns = nameCompanyOwns %>% str_to_upper(),
                     idRow = x) %>%
              gather(item, value, -idRow, na.rm = TRUE) %>%
              group_by(item) %>%
              mutate(count = 1:n() - 1) %>%
              ungroup() %>%
              arrange((count)) %>%
              mutate(item = ifelse(count == 0, item, paste0(item, count))) %>%
              select(-count)

            column_order <-
              c('idRow', df_data$item)

            df_data <-
              df_data %>%
              spread(item, value) %>%
              select(one_of(column_order))

            if (nest_data) {
              df_data <-
                df_data %>%
                nest(-idRow, .key = dataInsiderCompanies)
            }

            return(df_data)
          }) %>%
          suppressWarnings()

        detail_df <-
          detail_df %>%
          mutate_at(.vars = detail_df %>% select(dplyr::matches("date")) %>% names(),
                    funs(. %>% ymd())) %>%
          suppressWarnings()

        filing_df <-
          filing_df %>%
          mutate(idRow = 1:n()) %>%
          select(-detailsOwns) %>%
          left_join(detail_df) %>%
          select(-idRow) %>%
          suppressMessages()
      }

      general_df <-
        general_df %>%
        left_join(filing_df) %>%
        suppressMessages()
    }

    has_companies <-
      'companies' %in% names(data)

    if (has_companies) {
      companies_df <-
        data$companies %>%
        as_tibble() %>%
        .resolve_name_df()

      company_name_df <-
        companies_df %>%
        select(-dplyr::matches("status_history")) %>%
        gather(item, value, -c(idCIK, nameFiler)) %>%
        group_by(item) %>%
        mutate(countItem = 1:n() - 1) %>%
        ungroup() %>%
        mutate(item = ifelse(countItem == 0, item, item %>% paste0(countItem))) %>%
        select(-countItem) %>%
        suppressWarnings() %>%
        suppressMessages()

      col_order <-
        c('idCIK', 'nameFiler', company_name_df$item)

      company_name_df <-
        company_name_df %>%
        spread(item, value) %>%
        select(one_of(col_order))

      company_name_df <-
        company_name_df %>%
        mutate_at(company_name_df %>% select(dplyr::matches("idCIK")) %>% names(),
                  funs(. %>% as.numeric()))

      companies_df <-
        companies_df %>%
        mutate(idRow = 1:n())

      if ('status_history' %in% names(companies_df)) {
        status_df <-
          seq_along(companies_df$status_history) %>%
          future_map_dfr(function(x) {
            df <-
              companies_df$status_history[[x]] %>%
              as_tibble() %>%
              mutate(idRow = x) %>%
              select(-dplyr::matches("other|pair_id")) %>%
              gather(item, value, -idRow) %>%
              left_join(tibble(
                item = c('date', 'officer', 'title', 'ten_percent', 'director'),
                nameItem = c(
                  'dateAppointment',
                  'isOfficer',
                  'titleOfficer',
                  'is10PercentOwner',
                  'isDirector'
                )
              )) %>%
              select(-item) %>%
              group_by(nameItem) %>%
              mutate(countItem = 1:n() - 1) %>%
              ungroup() %>%
              mutate(item = ifelse(countItem == 0, nameItem, nameItem %>% paste0(countItem))) %>%
              select(idRow, item, value) %>%
              spread(item, value) %>%
              suppressMessages() %>%
              suppressWarnings()
            return(df)
          })

        status_df <-
          status_df %>%
          mutate_at(status_df %>% select(dplyr::matches("date")) %>% names(),
                    funs(. %>% lubridate::ymd())) %>%
          mutate_at(status_df %>% select(dplyr::matches("is")) %>% names(),
                    funs(. %>% as.logical())) %>%
          mutate_at(status_df %>% select(dplyr::matches("date")) %>% names(),
                    funs(. %>% as.character()))

        companies_df <-
          companies_df %>%
          select(-dplyr::matches("status")) %>%
          left_join(status_df) %>%
          suppressWarnings() %>%
          suppressMessages() %>%
          gather(item, value, -c(idCIK, nameFiler, idRow)) %>%
          group_by(item, idRow) %>%
          mutate(countItem = 1:n() - 1) %>%
          ungroup() %>%
          mutate(item = ifelse(countItem == 0, item, item %>% paste0(countItem))) %>%
          select(-countItem) %>%
          suppressWarnings()

        col_order <-
          c('idCIK', 'nameFiler', companies_df$item)

        companies_df <-
          companies_df %>%
          spread(item, value) %>%
          select(one_of(col_order)) %>%
          suppressWarnings()

        companies_df <-
          companies_df %>%
          mutate_at(status_df %>% select(dplyr::matches("date")) %>% names(),
                    funs(. %>% lubridate::ymd())) %>%
          mutate_at(status_df %>% select(dplyr::matches("^is|^has")) %>% names(),
                    funs(. %>% as.logical()))

      } else {
        companies_df <-
          company_name_df
      }

      if (nest_data) {
        companies_df <-
          companies_df %>%
          mutate(idRow = 1:n()) %>%
          nest(-c(idRow, idCIK), .key = dataDetailsCompaniesOwned) %>%
          as_tibble()
      }

      general_df <-
        general_df %>%
        mutate(idRow = 1:n()) %>%
        left_join(companies_df) %>%
        select(-idRow) %>%
        suppressMessages()

    }

    general_df <-
      general_df %>%
      mutate(urlJSONGeneral = url)

    if ('typeCompany' %in% names(general_df)) {
      general_df <-
        general_df %>%
        dplyr::rename(typeFiler = typeCompany)
    }

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% cat(fill = T)
    }

    return(general_df)


  }

.parse_insider_trade_json_url <-
  function(url = "http://rankandfiled.com/data/insider/1070844/trades?start=0",
           return_message = TRUE) {
    if (!url %>% httr::url_ok()) {
      return(tibble())
    }

    json_data <-
      url %>%
      jsonlite::fromJSON()

    options(scipen = 9999)

    cik <-
      url %>% str_replace_all('http://rankandfiled.com/data/insider/|/trades', '') %>%
      str_split('\\?') %>%
      flatten_chr() %>%
      .[[1]] %>%
      as.numeric()

    trade_df <-
      json_data$trades %>%
      as_tibble() %>%
      dplyr::rename(dateTrade = date) %>%
      mutate(dateTrade = dateTrade %>% lubridate::ymd())

    count_columns <-
      trade_df$trade %>%
      map_dbl(function(x) {
        x %>%
          str_count('\\*')
      }) %>%
      max() + 1

    column_names <-
      list("X", 1:count_columns) %>%
      purrr::invoke(paste0, .)

    trade_df <-
      trade_df %>%
      separate(trade, column_names, sep = '\\*') %>%
      suppressWarnings()

    trade_df_names <-
      c(
        'dateTrade',
        "idCIK",
        "idCIKOwns",
        "idInsiderType",
        "countSharesOwned",
        "descriptionOption",
        "idTypeInsiderTransaction",
        "amountPrice",
        "countShares",
        "idInsiderTransaction",
        "X10",
        "detailOwnershipIndirect",
        "priceExcercised",
        "dateOptionExcercisable",
        "dateOptionExpiry",
        "countSharesOptions",
        "typeSecurityOption",
        "X17"
      )

    trade_df <-
      trade_df %>%
      purrr::set_names((trade_df_names)[1:ncol(trade_df)]) %>%
      select(-dplyr::matches("X"))

    trade_df <-
      trade_df %>%
      mutate_at(.vars =
                  trade_df %>% select(dplyr::matches("date")) %>% names(),
                .funs = lubridate::ymd) %>%
      mutate_at(.vars =
                  trade_df %>% select(dplyr::matches("idCIK|count|amount|price")) %>% names(),
                funs(. %>% as.character() %>% readr::parse_number())) %>%
      left_join(tibble(
        idInsiderType = c("D", "ND"),
        typeInsider = c("Director", "Non-Director")
      )) %>%
      left_join(get_insider_code_df()) %>%
      left_join(
        tibble(
          idTypeInsiderTransaction = c("A", "D", "None"),
          typeInsiderTransaction = c('Purchase', 'Sale', 'None'),
          isBought = c(TRUE, FALSE, NA)
        )
      ) %>%
      suppressMessages() %>%
      suppressWarnings()

    trade_df <-
      trade_df %>%
      mutate(
        countShares = ifelse(isBought == T, countShares, -countShares),
        amountTransaction = countShares * amountPrice,
        urlJSON = url
      )


    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% cat(fill = T)
    }

    return(trade_df)

  }

.parse_insider_trades <-
  function(cik = 1070844,
           nest_data = TRUE,
           return_message = TRUE) {
    url_general <-
      list('http://rankandfiled.com/data/insider/', cik, '/general') %>%
      purrr::invoke(paste0, .)

    general_df <-
      .parse_json_general_insider(cik = cik,
                                 nest_data = nest_data,
                                 return_message = TRUE)

    cik <-
      general_df$idCIK

    insider <-
      general_df$nameEntity %>%
      str_to_upper()

    count_trades <-
      general_df$countTrades %/% 50

    trade_urls <-
      list(
        'http://rankandfiled.com/data/insider/',
        cik,
        '/trades?start=',
        seq(0, by = 50, length.out = count_trades)
      ) %>%
      purrr::invoke(paste0, .)

    parse_insider_trade_json_url_safe <-
      purrr::possibly(.parse_insider_trade_json_url, tibble())

    all_data <-
      trade_urls %>%
      future_map_dfr(function(x) {
        .parse_insider_trade_json_url(url = x, return_message = return_message)
      }) %>%
      distinct()

    ciks_owned <-
      all_data$idCIKOwns %>% unique()

    company_urls_general <-
      list('http://rankandfiled.com/data/filer/',
           ciks_owned,
           '/general') %>%
      purrr::invoke(paste0, .)

    owned_company_df <-
      company_urls_general %>%
      future_map_dfr(function(x) {
        .parse_json_general_filing(url = x,
                                   return_message = TRUE,
                                   nest_data = nest_data)
      })

    owned_df <-
      owned_company_df %>%
      select(dplyr::matches('idCIK|nameEntity|idTicker')) %>%
      select(-dplyr::matches("idCIKOwnedBy"))

    names(owned_df) <-
      names(owned_df) %>% paste0('Owns')

    all_data <-
      all_data %>%
      mutate(nameInsider = insider) %>%
      left_join(owned_df) %>%
      select(
        dateTrade,
        nameInsider,
        idCIK,
        nameEntityOwns,
        dplyr::matches('idCIKOwns|idTickerOwns'),
        everything()
      ) %>%
      suppressWarnings() %>%
      suppressMessages()

    all_data <-
      all_data %>%
      mutate_at(.vars = all_data %>% select(dplyr::matches("amount|price")) %>% names(),
                funs(. %>% formattable::currency(digits = 2))) %>%
      mutate_at(.vars = all_data %>% select(dplyr::matches("count")) %>% names(),
                funs(. %>% formattable::comma(digits = 0))) %>%
      mutate_if(is.numeric, as.numeric)

    if (return_message) {
      list(
        "Parsed ",
        all_data %>% nrow() %>% formattable::comma(digits = 0),
        ' insider transactions for ',
        insider
      ) %>%
        purrr::invoke(paste0, .) %>%
        cat(fill = T)
    }
    return(all_data)
  }

.parse_insider_filings <-
  function(cik = 1070844,
           nest_data = TRUE,
           return_message = TRUE) {
    general_df <-
      .parse_json_general_insider(cik = cik,
                                 nest_data = nest_data,
                                 return_messag = TRUE)

    cik <-
      general_df$idCIK

    insider <-
      general_df$nameEntity %>%
      str_to_upper()

    count_filings <-
      general_df$countFilings %/% 50

    filing_urls <-
      list(
        'http://rankandfiled.com/data/filer/',
        cik,
        '/all?start=',
        seq(0, by = 50, length.out = count_filings)
      ) %>%
      purrr::invoke(paste0, .)

    .parse_json_public_filers_safe <-
      purrr::possibly(.parse_json_public_filers, NULL)

    .all_filings <-
      filing_urls %>%
      future_map_dfr(function(x) {
        .parse_json_public_filers_safe(url = x, return_message = return_message)
      }) %>%
      distinct() %>%
      suppressWarnings() %>%
      mutate(nameInsider = insider) %>%
      select(idCIK, nameInsider, everything())

    if (return_message) {
      list("Parsed ", .all_filings %>% nrow(), ' SEC Filings for ', insider) %>%
        purrr::invoke(paste0, .) %>%
        cat(fill = T)
    }

    return(.all_filings)

  }

# funds -------------------------------------------------------------------
.generate_fund_general_url <-
  function(cik = 1034621) {

    glue("http://rankandfiled.com/data/fund/{cik}/general") %>% as.character()

  }

.parse_json_fund_general <-
  function(cik = 1034621,
           return_message = TRUE) {
    url <-
      cik %>%
      .generate_fund_general_url()

    if (!url %>% httr::url_ok() %>% suppressWarnings()) {
      return(tibble())
    }

    json_data <-
      url %>%
      jsonlite::fromJSON()

    general_cols <-
      json_data %>% future_map_dfr(class) %>%
      gather(item, value) %>%
      filter(!value %in% (c('list', 'data.frame'))) %>%
      .$item %>%
      suppressWarnings()

    general_df <-
      json_data %>%
      data.frame(stringsAsFactors = FALSE) %>%
      dplyr::select(one_of(general_cols)) %>%
      .resolve_name_df() %>%
      distinct() %>%
      select(-dplyr::matches("descriptionClasses"))

    has_funds <-
      'funds' %in% names(json_data)

    if (has_funds) {
      general_df <-
        general_df %>%
        left_join(json_data$funds %>%
                    .resolve_name_df() %>%
                    mutate(idCIK = cik)) %>%
        suppressMessages()
    }

    has_filer <-
      'filer' %in% names(json_data)

    if (has_filer) {
      filer_df <-
        json_data$filer %>%
        as_tibble()

      filer_df <-
        filer_df %>%
        .resolve_name_df()
      if (!'idCIK' %in% names(filer_df)) {
        filer_df <-
          filer_df %>%
          mutate(idCIK = cik)
      }

      if ('name' %in% names(filer_df)) {
        filer_df <-
          filer_df %>%
          mutate(nameEntity = nameEntity %>% stringr::str_to_upper()) %>%
          select(-name)
      }
      filer_df <-
        filer_df %>%
        mutate_at(filer_df %>% select(dplyr::matches("idRF|idCIK")) %>% names(),
                  funs(. %>% as.numeric()))

      merge_cols <-
        c('idCIKFiler', 'idRow', names(filer_df)[!names(filer_df) %in% names(general_df)])

      general_df <-
        general_df %>%
        mutate(idRow = 1:n()) %>%
        left_join(
          filer_df %>%
            mutate(idRow = 1:n()) %>%
            dplyr::rename(idCIKFiler = idCIK) %>%
            select(one_of(merge_cols))
        ) %>%
        select(-dplyr::matches("^object|idRow")) %>%
        distinct() %>%
        suppressMessages()

    }

    general_df <-
      general_df %>%
      select(idCIK,
             nameEntity,
             dplyr::matches("name"),
             dplyr::matches("id"),
             everything())

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% cat(fill = T)
    }

    return(general_df)

  }


# for_table ---------------------------------------------------------------

.parse_for_tables_rf <-
  function(filing_df,
           parse_complete_text_filings = TRUE,
           parse_form_d = TRUE,
           parse_13F = TRUE,
           parse_small_offerings = TRUE,
           parse_form_3_4s = TRUE,
           parse_asset_files = TRUE,
           parse_xbrl = TRUE,
           nest_data = TRUE,
           return_message = TRUE) {
    all_tables <-
      tibble()
    parse_all_filings <-
      c(
        parse_complete_text_filings,
        parse_form_d,
        parse_13F,
        parse_small_offerings,
        parse_form_3_4s,
        parse_asset_files,
        parse_xbrl
      ) %>%
      sum() > 0

    parse_form_data_safe <-
      purrr::possibly(.parse_form_data, tibble())

    if (parse_all_filings) {
      if (!'typeFile' %in% names(filing_df)) {
        filing_df <-
          filing_df %>%
          mutate(typeFile = ifelse(urlSECFilingDirectory %>% str_detect('htm'),
                                   'html', NA))
      }

      search_df <-
        filing_df %>%
        select(dateFiling,
               dplyr::matches("typeFile"),
               dplyr::matches("idForm"),
               urlSECFilingDirectory) %>%
        distinct() %>%
        filter(!urlSECFilingDirectory %>% is.na()) %>%
        distinct()

      df_all_filing_urls <-
        search_df$urlSECFilingDirectory %>%
        unique() %>%
        future_map_dfr(function(x){
          .parse_sec_filing_index(urls = x)
        })

      df_all_filing_urls <-
        df_all_filing_urls %>%
        mutate(isForm3_4 = ifelse(typeForm %in% c("3", "4") &
                                    typeFile == "xml", TRUE, FALSE))
      df_urls <-
        df_all_filing_urls %>%
        mutate(nameTable = 'Filing Directories') %>%
        nest(-nameTable, .key = dataTable)

      all_tables <-
        all_tables %>%
        bind_rows(df_urls)

      if (parse_complete_text_filings) {
        if (!'urlTextFilingFull' %in% names(df_all_filing_urls)) {
          df_all_filing_urls <-
            df_all_filing_urls %>%
            mutate(urlTextFilingFull = urlSECFilingDirectory %>% str_replace_all("-index.htm", ".txt"))
        }
        urls <-
          df_all_filing_urls$urlTextFilingFull %>%
          unique()
        sec_complete_filings_safe <-
          purrr::possibly(.sec_complete_filings, tibble())
        all_text_df <-
          .sec_complete_filings(urls = urls)

        all_tables <-
          all_tables %>%
          bind_rows(tibble(
            nameTable = 'Text Filings',
            dataTable = list(all_text_df %>% nest(-c(idCIK), .key = dataFilings))
          ))
      }

      if (parse_form_d) {
        df_form_ds <-
          df_all_filing_urls %>%
          parse_form_data_safe(filter_parameter = 'isFormD')

        all_tables <-
          all_tables %>%
          bind_rows(tibble(
            nameTable = 'FormDs',
            dataTable = list(df_form_ds)
          ))
      }

      if (parse_13F) {
        df_13F <-
          df_all_filing_urls %>%
          parse_form_data_safe(filter_parameter = 'is13FFiling')

        all_tables <-
          all_tables %>%
          bind_rows(tibble(nameTable = '13Fs', dataTable = list(df_13F)))
      }

      if (parse_small_offerings) {
        df_small_offerings <-
          df_all_filing_urls %>%
          parse_form_data_safe(filter_parameter = 'hasSmallOfferingData')
        all_tables <-
          all_tables %>%
          bind_rows(tibble(
            nameTable = 'Small Offerings',
            dataTable = list(df_small_offerings)
          ))
      }

      if (parse_form_3_4s) {
        df_form3_4 <-
          df_all_filing_urls %>%
          parse_form_data_safe(filter_parameter = 'isForm3_4')

        all_tables <-
          all_tables %>%
          bind_rows(tibble(
            nameTable = 'Form 3 and 4',
            dataTable = list(df_form3_4)
          ))
      }

      if (parse_asset_files) {
        df_assets <-
          df_all_filing_urls %>%
          parse_form_data_safe(filter_parameter = 'hasAssetFile')

        all_tables <-
          all_tables %>%
          bind_rows(tibble(nameTable = 'Asset Data', dataTable = list(df_assets)))
      }

      if (parse_xbrl) {
        df_xbrl <-
          df_all_filing_urls %>%
          parse_form_data_safe(filter_parameter = 'isXBRLInstanceFile')

        all_tables <-
          all_tables %>%
          bind_rows(tibble(nameTable = 'XBRL', dataTable = list(df_xbrl)))
      }

    }

    all_tables <-
      all_tables %>%
      mutate(countCols = dataTable %>% map_dbl(ncol)) %>%
      filter(countCols > 0) %>%
      select(-countCols)

    return(all_tables)
  }


# filing_stream -----------------------------------------------------------

.get_most_recent_rf_id <-
  function(url = "http://rankandfiled.com/data/latest") {
    json_data <-
      url %>%
      jsonlite::fromJSON()

    json_data$filings$id %>% as.numeric() %>% max()
  }


.parse_filing_stream <-
  function(url = "http://rankandfiled.com/data/latest?group=ALL&filer=All",
           nest_data = TRUE,
           return_message = TRUE) {
    json_data <-
      url %>%
      jsonlite::fromJSON()

    options(scipen = 9999)

    filing_class_df <-
      json_data$filings %>% future_map_dfr(class) %>%
      gather(column, type) %>%
      mutate(idName = 1:n())

    general_df <-
      json_data$filings %>%
      select(filing_class_df %>%
               filter(!type %in% c('list', 'data.frame')) %>%
               .$idName)

    general_df <-
      general_df %>%
      as_tibble() %>%
      mutate_all(funs(. %>% str_replace('\\|', '')))

    general_df <-
      general_df %>%
      .resolve_name_df() %>%
      distinct()

    general_df <-
      general_df %>%
      mutate_at(general_df %>% select(dplyr::matches("^datetime[A-Z]")) %>% names(),
                funs(. %>% lubridate::ymd_hms())) %>%
      mutate_at(general_df %>% select(dplyr::matches("dateFiled")) %>% names(),
                funs(. %>% lubridate::ymd())) %>%
      mutate_at(general_df %>% select(dplyr::matches("idRF|idCIK")) %>% names(),
                funs(. %>% as.numeric())) %>%
      mutate_at(general_df %>% select(dplyr::matches("^is|^has")) %>% names(),
                funs(. %>% as.logical())) %>%
      mutate_at(general_df %>% select(dplyr::matches("^description|^type")) %>% names(),
                funs(. %>% stringr::str_to_upper())) %>%
      mutate(urlSEC = ifelse(
        slugSEC == "None",
        NA,
        list(
          "https://www.sec.gov/Archives/edgar/data/",
          idCIK,
          '/',
          slugSEC
        ) %>% purrr::invoke(paste0, .)
      ))

    if ('idFormType' %in% names(general_df)) {
      general_df %>%
        left_join(dictionary_sec_filing_codes()) %>%
        suppressMessages()
    }

    has_filer <-
      'filer' %in% names(json_data$filings)

    if (has_filer) {
      filer_df <-
        json_data$filings$filer %>%
        as_tibble()

      filer_df <-
        filer_df %>%
        .resolve_name_df()

      if ('name' %in% names(filer_df)) {
        filer_df <-
          filer_df %>%
          dplyr::rename(nameLegal = name) %>%
          mutate(nameEntity = nameEntity %>% stringr::str_to_upper())
      }
      filer_df <-
        filer_df %>%
        mutate_at(filer_df %>% select(dplyr::matches("idRF|idCIK")) %>% names(),
                  funs(. %>% as.numeric())) %>%
        mutate_at(filer_df %>% select(dplyr::matches("^name|^industry|^typeFund|^details")) %>% names(),
                  funs(. %>% stringr::str_to_upper()))

      if ('detailsOwnedBy' %in% names(filer_df)) {
        filer_df <-
          filer_df %>%
          dplyr::rename(detailsOwns = detailsOwnedBy)

        filer_df <-
          filer_df %>%
          mutate(idRow = 1:n(),
                 detailsOwns = detailsOwns %>% str_replace("\\|", ''))

        owns_df <-
          1:nrow(filer_df) %>%
          future_map_dfr(function(x) {
            owns <-
              filer_df$detailsOwns[[x]] %>%
              str_split("\\|") %>%
              flatten_chr()

            df <-
              tibble(idRow = x, owns) %>%
              tidyr::separate(owns,
                              into = c('idTickerOwns', 'owns'),
                              sep = '\\:') %>%
              tidyr::separate(owns,
                              into = c('nameCompanyOwns', 'owns'),
                              sep = '\\_') %>%
              tidyr::separate(
                owns,
                into = c('typeOwnerOwns', 'dateOwnershipOwns'),
                sep = '\\#'
              ) %>%
              mutate(countItem = 1:n() - 1) %>%
              mutate(
                nameCompanyOwns = nameCompanyOwns %>% str_to_upper(),
                idTickerOwns = idTickerOwns %>% str_to_upper()
              ) %>%
              gather(item, value, -c(idRow, countItem)) %>%
              mutate(value = ifelse(value == '', NA, value)) %>%
              mutate(item = ifelse(countItem == 0, item, item %>% paste0(countItem))) %>%
              arrange(countItem) %>%
              select(-countItem)

            col_order <-
              c('idRow', df$item)
            df <-
              df %>%
              spread(item, value) %>%
              select(one_of(col_order))

            df <-
              df %>%
              mutate_at(df %>% select(dplyr::matches("date")) %>% names(),
                        funs(. %>% lubridate::ymd()))

            if (nest_data) {
              df <-
                df %>%
                nest(-idRow, .key = dataCompaniesOwns)
            }
            return(df)
          }) %>%
          suppressWarnings()

        filer_df <-
          filer_df %>%
          left_join(owns_df) %>%
          select(-idRow) %>%
          suppressMessages()
      }


      general_df <-
        general_df %>%
        mutate(idRow = 1:n()) %>%
        left_join(
          filer_df %>%
            mutate(idRow = 1:n()) %>%
            dplyr::rename(idCIKFiler = idCIK) %>%
            select(one_of(
              c('idCIKFiler', 'idRow'), names(filer_df)[!names(filer_df) %in% names(general_df)]
            ))
        ) %>%
        select(-dplyr::matches("^object|idRow")) %>%
        distinct() %>%
        suppressMessages()

    }

    has_offerings <-
      'offerings' %in% names(json_data$filings)

    if (has_offerings) {
      general_df <-
        general_df %>%
        mutate(idRow = 1:n())

      offering_df <-
        1:nrow(general_df) %>%
        future_map_dfr(function(x) {
          offering <-
            json_data$filings$offerings[[x]]

          has_no_data <-
            length(offering) == 0

          if (has_no_data) {
            return(tibble(idRow = x))
          }
          has_no_rows <-
            offering %>% nrow() == 0
          if (has_no_rows) {
            return(tibble(idRow = x))
          }

          offering_long <-
            offering %>% .resolve_name_df() %>%
            mutate(idRow = x) %>%
            gather(item, value, -idRow) %>%
            group_by(item) %>%
            mutate(countItem = 1:n() - 1) %>%
            ungroup() %>%
            mutate(item = ifelse(countItem == 0, item, item %>% paste0(countItem))) %>%
            arrange(countItem) %>%
            select(-countItem)

          col_order <-
            offering_long$item

          offering <-
            offering_long %>%
            spread(item, value) %>%
            select(one_of(c('idRow', col_order)))

          offering <-
            offering %>%
            mutate_at(offering %>% select(dplyr::matches("^count[A-Z]|^amount")) %>% names(),
                      funs(. %>% as.numeric())) %>%
            mutate_at(offering %>% select(dplyr::matches("^date")) %>% names(),
                      funs(. %>% lubridate::ymd()))

          if (nest_data) {
            offering <-
              offering %>%
              nest(-idRow, .key = dataOfferings)
          }
          return(offering)
        }) %>%
        select(idRow, everything())

      offering_df <-
        offering_df %>%
        mutate_at(dplyr::matches("^nameIndustry"),
                  funs(. %>% str_to_upper()))

      general_df <-
        general_df %>%
        mutate(idRow = 1:n()) %>%
        select(-dplyr::matches("nameIndustry")) %>%
        left_join(offering_df) %>%
        select(-idRow) %>%
        suppressWarnings() %>%
        suppressMessages()

      if ('nameIndustry' %in% names(general_df)) {
        general_df <-
          general_df %>%
          dplyr::rename(nameIndustryOffering = nameIndustry)
      }
    }

    has_trades <-
      'trades' %in% names(json_data$filings)

    if (has_trades) {
      general_df <-
        general_df %>%
        mutate(idRow = 1:n())

      trade_df <-
        1:nrow(general_df) %>%
        future_map_dfr(function(x) {
          trades <-
            json_data$filings$trades[[x]]

          has_no_data <-
            length(trades) == 0

          if (has_no_data) {
            return(tibble(idRow = x))
          }
          has_no_rows <-
            trades %>% nrow() == 0
          if (has_no_rows) {
            return(tibble(idRow = x))
          }

          trades <-
            trades %>% .resolve_name_df() %>%
            mutate(idRow = x) %>%
            dplyr::rename(idInsiderTransaction = codeTransaction)

          trades <-
            trades %>%
            mutate_at(.vars = trades %>% select(dplyr::matches("amount|count")) %>% names,
                      funs(. %>% as.numeric())) %>%
            left_join(get_insider_code_df()) %>%
            suppressWarnings() %>%
            suppressMessages()

          if ('amountPrice' %in% names(trades)) {
            if (!'isBought' %in% names(trades)) {
              trades <-
                trades %>%
                mutate(isBought = FALSE)
            }

            trades <-
              trades %>%
              mutate(
                isBought = ifelse(isBought %>% is.na(), FALSE, TRUE),
                countShares = ifelse(isBought == T, countShares, -countShares),
                amountTransaction = countShares * amountPrice
              )
          }

          trades_long <-
            trades %>%
            gather(item, value, -idRow) %>%
            group_by(item) %>%
            mutate(countItem = 1:n() - 1) %>%
            ungroup %>%
            mutate(item = ifelse(countItem == 0, item, item %>% paste0(countItem))) %>%
            arrange(countItem) %>%
            select(-countItem)

          col_order <-
            trades_long$item

          trades <-
            trades_long %>%
            spread(item, value) %>%
            select(one_of(c('idRow', col_order)))

          trades <-
            trades %>%
            mutate_at(trades %>% select(dplyr::matches("^count[A-Z]|^amount")) %>% names(),
                      funs(. %>% as.numeric())) %>%
            mutate_at(trades %>% select(dplyr::matches("^date")) %>% names(),
                      funs(. %>% lubridate::ymd()))

          if (nest_data) {
            trades <-
              trades %>%
              nest(-idRow, .key = dataTrades)
          }
          return(trades)
        }) %>%
        select(idRow, everything())

      names(trade_df)[names(trade_df) %>% str_detect('dateFiling')] <-
        trade_df %>% select(dplyr::matches("dateFiling")) %>% names() %>%
        str_replace_all("dateFiling", 'dateFilingInsider')


      general_df <-
        general_df %>%
        mutate(idRow = 1:n()) %>%
        select(-dplyr::matches("nameIndustry")) %>%
        left_join(trade_df %>% select(-dplyr::matches("idTicker")), by = 'idRow') %>%
        select(-idRow) %>%
        suppressWarnings() %>%
        suppressMessages()
    }

    general_df <-
      general_df %>%
      mutate_at(.vars = general_df %>% select(dplyr::matches("nameEntity")) %>% names(),
                funs(. %>% str_to_upper())) %>%
      suppressWarnings() %>%
      ungroup() %>%
      select(-dplyr::matches("^object[A-Z]|^slug|dateiso")) %>%
      select(idCIK,
             nameEntity,
             dplyr::matches("name"),
             dplyr::matches("id"),
             everything())


    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% cat(fill = T)
    }
    general_df
  }


.sec_filing_stream <-
  function(filers = 'All',
           filing_name = 'Registrations',
           nest_data = TRUE,
           return_message = TRUE) {
    both_all <-
      filers == 'All' & filing_name == 'All'

    if (both_all) {
      rf_id <-
        .get_most_recent_rf_id()

      start <-
        rf_id - 3500

      rf_ds <-
        seq(start, rf_id, by = 30)

      urls <-
        list('http://rankandfiled.com/data/latest?id=',
             rf_ds) %>%
        purrr::invoke(paste0, .)

      parse_filing_stream_safe <-
        purrr::possibly(.parse_filing_stream, tibble())

      data <-
        urls %>%
        future_map_dfr(function(x) {
          parse_filing_stream_safe(url = x, nest_data = nest_data)
        }) %>%
        distinct() %>%
        select(
          idRF,
          idCIK,
          dplyr::matches("nameEntity"),
          dplyr::matches("idTicker"),
          dplyr::matches("dateFiled"),
          dplyr::matches("datetimeFiled"),
          dplyr::matches("^name"),
          dplyr::matches("^date"),
          dplyr::matches("^id"),
          dplyr::matches("^type"),
          dplyr::matches("^description"),
          everything()
        ) %>%
        mutate(
          urlRankAndFiled = list('http://rankandfiled.com/#/filers/', idCIK, '/filings') %>%
            purrr::invoke(paste0, .)
        )


    } else {
      filer_names <-
        c('All',
          'Corporate Insider',
          'Companies',
          'Investment Company') %>%
        str_to_upper()

      filing_names <-
        c(
          'Annual Reports',
          'Quarterly Reports',
          'Current Reports',
          'Other Reports',
          'Registrations',
          'Private Offerings',
          'Ownership',
          'Prospectuses',
          'Exemptions',
          'Withdrawals',
          'Correspondence',
          'Proxy Statements',
          'Confidential',
          'All'
        ) %>% str_to_upper()

      no_filers <-
        !filers %>% str_to_upper() %in% filer_names

      if (no_filers) {
        stop(
          list(
            "Filers can only be:\n",
            filer_names %>%  stringr::str_to_title() %>% paste0(collapse = '\n')
          ) %>%
            purrr::invoke(paste0, .)
        )
      }
      no_filing_names <-
        !filing_name %>% str_to_upper() %in% filing_names

      if (no_filing_names) {
        stop(
          list(
            "Filing names can only be:\n",
            filing_names %>%  stringr::str_to_title() %>% paste0(collapse = '\n')
          ) %>%
            purrr::invoke(paste0, .)
        )
      }

      .filer_type_df <-
        tibble(
          codeFiler = c('All', 'insider', 'company', 'inv_co'),
          nameFiler = filer_names
        )

      slug_filer <-
        .filer_type_df %>%
        filter(nameFiler == filers %>% str_to_upper()) %>%
        .$codeFiler

      filing_name_df <-
        tibble(
          codeFiling  = c(
            "A",
            "Q",
            "CR",
            "R",
            "REG",
            "REGX",
            "O",
            "P",
            "X",
            "W",
            "SEC",
            "PROXY",
            "CT",
            "ALL"
          ),
          nameFiling = filing_names
        )

      slug_type <-
        filing_name_df %>%
        filter(nameFiling == filing_name %>% str_to_upper()) %>%
        .$codeFiling

      mr_id <-
        .get_most_recent_rf_id()

      url_json <-
        list(
          'http://rankandfiled.com/data/latest?id=',
          mr_id,
          '&group=',
          slug_type,
          '&filer=',
          slug_filer
        ) %>%
        purrr::invoke(paste0, .)

      data <-
        url_json %>%
        .parse_filing_stream() %>%
        select(
          idRF,
          idCIK,
          dplyr::matches("nameEntity"),
          dplyr::matches("idTicker"),
          dplyr::matches("dateFiled"),
          dplyr::matches("datetimeFiled"),
          dplyr::matches("^name"),
          dplyr::matches("^date"),
          dplyr::matches("^id"),
          dplyr::matches("^type"),
          dplyr::matches("^description"),
          everything()
        ) %>%
        mutate(
          urlRankAndFiled = list('http://rankandfiled.com/#/filers/', idCIK, '/filings') %>%
            purrr::invoke(paste0, .)
        )
    }

    if (return_message) {
      list("\nParsed Most Recent filings for ",
           filers,
           ' Filers\n',
           filing_name,
           ' Form Type\n') %>%
        purrr::invoke(paste0, .) %>%
        cat(fill = T)
    }
    return(data)
  }


#' SEC filing stream
#'
#' This function returns the most recent SEC filings
#' for specified filer type by filing type.
#'
#' @param filers type of filer \itemize{
#' \item \code{All}: all filer types (default)
#' \item \code{Corporate Insider}: corporate insiders
#' \item \code{Companies}:
#' \item \code{Investment Company} acquires investment company data
#' @param filing_names type of filing \itemize{
#' \item \code{Annual Reports}: annual report
#' \item \code{Quarterly Reports}: quarterly report
#' \item \code{Current Reports}: current report
#' \item \code{Other Reports}: other reports
#' \item \code{Registrations}: securities registration
#' \item \code{Private Offerings}: private securities offerings
#' \item \code{Ownership}: ownership
#' \item \code{Prospectuses}: securities prospectus
#' \item \code{Exemptions}: exempt securities
#' \item \code{Withdrawals}: securities withdrawls
#' \item \code{Correspondence}: SEC correspondence
#' \item \code{Proxy Statements}:  proxy issuances
#' \item \code{Confidential}: confidential information
#' }
#' @param return_message return a message
#' @import dplyr tidyr purrr stringr formattable readr lubridate
#' @importFrom jsonlite fromJSON
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' sec_filing_streams(filers = 'All', filing_names = 'Annual Reports')
#' }
#'
sec_filing_streams_rf <-
  function(filers = c('All', 'Corporate Insider', 'Companies', 'Investment Company'),
           filing_names = c(
             'All',
             'Annual Reports',
             'Quarterly Reports',
             'Current Reports',
             'Other Reports',
             'Registrations',
             'Private Offerings',
             'Ownership',
             'Prospectuses',
             'Exemptions',
             'Withdrawals',
             'Correspondence',
             'Proxy Statements',
             'Confidential'
           ),
           nest_data = TRUE,
           return_message = TRUE) {
    type_df <-
      expand.grid(
        nameFiler = filers,
        nameFiling = filing_names,
        stringsAsFactors = FALSE
      ) %>%
      as_tibble()

    sec_filing_stream_safe <-
      purrr::possibly(.sec_filing_stream, NULL)

    all_data <-
      1:nrow(type_df) %>%
      future_map_dfr(function(x) {
        sec_filing_stream_safe(
          filers = type_df$nameFiler[[x]],
          filing_name = type_df$nameFiling[[x]],
          nest_data = nest_data,
          return_message = return_message
        )
      }) %>%
      distinct() %>%
      mutate(idRow = 1:n()) %>%
      group_by(idRF) %>%
      filter(idRow == min(idRow)) %>%
      ungroup() %>%
      select(-idRow)

    all_data <-
      all_data %>%
      select(-dplyr::matches("dateiso")) %>%
      mutate_at(all_data %>% select(dplyr::matches("^name|^description|^industry|^typeEntity")) %>% names(),
                funs(. %>% stringr::str_to_upper()))

    return(all_data)
  }


# publics -----------------------------------------------------------------
.generate_ticker_general_url <-
  function(ticker = "FB") {
    glue("http://rankandfiled.com/data/company/{ticker}/general") %>% as.character()
  }


.parse_json_public_general <-
  function(url = "http://rankandfiled.com/data/company/BX/general",
           nest_data = TRUE,
           return_message = TRUE) {
    if (!url %>% httr::url_ok()) {
      return(tibble())
    }

    json_data <-
      url %>%
      jsonlite::fromJSON()

    ticker <-
      url %>% str_replace("http://rankandfiled.com/data/company/", '') %>%
      str_split('\\/') %>% flatten_chr() %>%
      .[[1]]

    general_class_df <-
      json_data %>% future_map_dfr(class) %>%
      gather(column, type) %>%
      mutate(idName = 1:n())

    general_df <-
      json_data[general_class_df %>%
                  filter(!type %in% c('list', 'data.frame')) %>%
                  .$idName] %>%
      flatten_df() %>%
      .resolve_name_df() %>%
      select(-dplyr::matches("idTicker")) %>%
      mutate(idTicker = ticker)

    has_market <-
      'market' %in% names(json_data)

    if (has_market) {
      general_df <-
        general_df %>%
        bind_cols(json_data$market %>%
                    flatten_df() %>%
                    .resolve_name_df() %>%
                    select(-dplyr::matches("codeExchange")))

      if ('amountEquityMarketCap' %in% names(general_df)) {
        general_df <-
          general_df %>%
          mutate(amountEquityMarketCap = amountEquityMarketCap %>% formattable::currency(digits = 0))
      }

      if ('nameIndustry' %in% names(general_df)) {
        has_semi <-
          general_df$nameIndustry %>% str_detect('\\:')
        if (has_semi) {
          general_df <-
            general_df %>%
            tidyr::separate(
              nameIndustry,
              into = c('nameIndustry', 'nameSubIndustry'),
              sep = '\\: '
            ) %>%
            suppressWarnings()
        }
      }

      general_df <-
        general_df %>%
        mutate_if(is_character,
                  str_to_upper)
    }

    has_snap_shot <-
      'snapshot' %in% names(json_data)

    if (has_snap_shot) {
      snap_shot_df <-
        json_data$snapshot %>%
        as_tibble()

      if ('ebitda' %in% names(snap_shot_df)) {
        snap_shot_df <-
          snap_shot_df %>%
          mutate(digitEBITDA = ebitda %>% substr(
            start = (ebitda %>% nchar()) ,
            stop = ebitda %>% nchar()
          ))
      }

      snap_shot_df <-
        snap_shot_df %>%
        .resolve_name_df() %>%
        select(-dplyr::matches("amountEquityMarketCap"))

      if ('digitEBITDA' %in% names(snap_shot_df)) {
        snap_shot_df <-
          snap_shot_df %>%
          mutate(
            amountEBITDA = ifelse(
              digitEBITDA == "B",
              amountEBITDA * 1000000000,
              amountEBITDA * 1000000
            )
          ) %>%
          select(-digitEBITDA)
      }

      snap_shot_df <-
        snap_shot_df %>%
        mutate_at(.vars = snap_shot_df %>% select(dplyr::matches("price|amount")) %>% names,
                  funs(. %>% currency(digits = 2))) %>%
        mutate_at(.vars = snap_shot_df %>% select(dplyr::matches("amountEBITDA")) %>% names,
                  funs(. %>% currency(digits = 0)))

      general_df <-
        general_df %>%
        bind_cols(snap_shot_df)
    }

    has_filer <-
      ((
        'filer' %in% names(json_data) &
          json_data$filer %>% as_tibble() %>% ncol > 2
      ))

    if (has_filer) {
      filer_df <-
        json_data$filer %>%
        as_tibble()

      filer_df <-
        filer_df %>%
        .resolve_name_df()

      if ('detailsOwnedBy' %in% names(filer_df)) {
        filer_df <-
          filer_df %>%
          dplyr::rename(detailsOwns = detailsOwnedBy)

        filer_df <-
          filer_df %>%
          mutate(idRow = 1:n(),
                 detailsOwns = detailsOwns %>% str_replace("\\|", ''))

        owns_df <-
          1:nrow(filer_df) %>%
          future_map_dfr(function(x) {
            owns <-
              filer_df$detailsOwns[[x]] %>%
              str_split("\\|") %>%
              flatten_chr()

            df <-
              tibble(idRow = x, owns) %>%
              tidyr::separate(owns,
                              into = c('idTickerOwns', 'owns'),
                              sep = '\\:') %>%
              tidyr::separate(owns,
                              into = c('nameCompanyOwns', 'owns'),
                              sep = '\\_') %>%
              tidyr::separate(
                owns,
                into = c('typeOwnerOwns', 'dateOwnershipOwns'),
                sep = '\\#'
              ) %>%
              mutate(countItem = 1:n() - 1) %>%
              mutate(
                nameCompanyOwns = nameCompanyOwns %>% str_to_upper(),
                idTickerOwns = idTickerOwns %>% str_to_upper()
              ) %>%
              gather(item, value, -c(idRow, countItem)) %>%
              mutate(value = ifelse(value == '', NA, value)) %>%
              mutate(item = ifelse(countItem == 0, item, item %>% paste0(countItem))) %>%
              arrange(countItem) %>%
              select(-countItem)

            col_order <-
              c('idRow', df$item)
            df <-
              df %>%
              spread(item, value) %>%
              select(one_of(col_order))

            df <-
              df %>%
              mutate_at(df %>% select(dplyr::matches("date")) %>% names(),
                        funs(. %>% lubridate::ymd()))

            if (nest_data) {
              df <-
                df %>%
                nest(-idRow, .key = dataCompaniesOwns)
            }
            return(df)
          }) %>%
          suppressWarnings()

        filer_df <-
          filer_df %>%
          left_join(owns_df) %>%
          select(-idRow) %>%
          suppressMessages()
      }

      filer_df <-
        filer_df %>%
        mutate_at(filer_df %>% select(dplyr::matches("nameEntity")) %>% names(),
                  funs(. %>% stringr::str_to_upper())) %>%
        select(
          dplyr::matches("nameEntity"),
          dplyr::matches("^id"),
          dplyr::matches("industry"),
          dplyr::matches("name"),
          dplyr::matches("type"),
          everything()
        ) %>%
        select(-dplyr::matches("object"))

      if ('addressStreet1Entity' %in% names(filer_df)) {
        filer_df <-
          filer_df %>%
          mutate(
            addressEntity = list(
              addressStreet1Entity,
              ' ',
              cityEntity,
              ' ',
              stateEntity,
              ', ',
              zipcodeEntity
            ) %>% purrr::invoke(paste0, .)
          ) %>%
          select(nameEntity, addressEntity, everything())
      }

      filer_cols <-
        names(filer_df)[!names(filer_df) %in% names(general_df)]

      general_df <-
        general_df %>%
        bind_cols(filer_df %>% select(one_of(filer_cols))) %>%
        select(idCIK, dplyr::matches("nameEntity"), everything()) %>%
        select(-dplyr::matches("detailsOwns"))
    }

    if ('detailsOwns' %in% names(general_df)) {
      detail_df <-
        seq_along(general_df$detailsOwns) %>%
        future_map_dfr(function(x) {
          detail_value <-
            general_df$detailsOwns[[x]]

          if (detail_value %>% is.na()) {
            df <-
              tibble(idRow = x, nameCompanyOwns = NA)
            if (nest_data) {
              df <-
                df %>%
                nest(-idRow, .key = dataCompaniesOwns)
            }
            return(df)
          }

          values <-
            detail_value %>% str_replace('\\|', '') %>%
            str_split('\\|') %>%
            flatten_chr()

          df_data <-
            tibble(value = values) %>%
            tidyr::separate(value,
                            into = c('idTickerOwns', 'other'),
                            sep = '\\:') %>%
            tidyr::separate(other,
                            into = c('nameCompanyOwns', 'other'),
                            sep = '\\_') %>%
            tidyr::separate(other,
                            into = c('roleOwner', 'dateOwner'),
                            sep = '\\#') %>%
            mutate(nameCompanyOwns = nameCompanyOwns %>% str_to_upper(),
                   idRow = x) %>%
            gather(item, value, -idRow, na.rm = TRUE) %>%
            group_by(item) %>%
            mutate(value = ifelse(value == '', NA, value),
                   count = 1:n() - 1) %>%
            ungroup() %>%
            arrange((count)) %>%
            mutate(item = ifelse(count == 0, item, paste0(item, count))) %>%
            select(-count)

          column_order <-
            c('idRow', df_data$item)

          df_data <-
            df_data %>%
            spread(item, value) %>%
            select(one_of(column_order))

          if (nest_data) {
            df_data <-
              df_data %>%
              nest(-idRow, .key = dataCompaniesOwns)
          }
          return(df_data)
        }) %>%
        suppressWarnings()

      detail_df <-
        detail_df %>%
        mutate_at(.vars = detail_df %>% select(dplyr::matches("date")) %>% names(),
                  funs(. %>% ymd())) %>%
        suppressWarnings()

      general_df <-
        general_df %>%
        mutate(idRow = 1:n()) %>%
        select(-detailsOwns) %>%
        left_join(detail_df) %>%
        select(-idRow) %>%
        suppressMessages()
    }

    general_df <-
      general_df %>%
      mutate(
        urlTickerRankandFiled = list('http://rankandfiled.com/#/public/', idTicker, '/filings') %>% purrr::invoke(paste0, .),
        urlJSON = url
      )

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% cat(fill = T)
    }

    return(general_df)
  }

.parse_company_general <-
  function(ticker = "FB",
           nest_data = TRUE,
           return_message = TRUE) {
    options(warn = -1)
    data <-
      .generate_ticker_general_url(ticker = ticker) %>%
      .parse_json_public_general(nest_data = nest_data,
                                 return_message = return_message)
    if ('nameEntity' %in% names(data)) {
      data <-
        data %>%
        mutate(nameCompany = nameEntity) %>%
        select(idCIK, idTicker, nameEntity, nameCompany, everything()) %>%
        select(-dplyr::matches("dateiso"))
    } else {
      df_name <-
        list('http://rankandfiled.com/data/filer/',
             data$idCIK,
             '/general') %>%
        purrr::invoke(paste0, .) %>%
        .parse_json_general_filing()

      entity <-
        df_name$nameEntity

      data <-
        data %>%
        mutate(nameEntity = entity,
               nameCompany = nameEntity) %>%
        select(idCIK, idTicker, nameEntity, nameCompany, everything()) %>%
        select(-dplyr::matches("dateiso"))
    }

    data <-
      data %>%
      resolve_names_to_upper()
    return(data)
  }
.parse_company_general_safe <-
  purrr::possibly(.parse_company_general, tibble())
.parse_json_trades <-
  function(url = "http://rankandfiled.com/data/filer/1326801/trades?start=0",
           return_message = TRUE) {
    if (!url %>% httr::url_ok() %>% suppressWarnings()) {
      return(tibble())
    }

    json_data <-
      url %>%
      jsonlite::fromJSON()

    options(scipen = 9999)

    cik <-
      url %>% str_replace_all('http://rankandfiled.com/data/filer/|/trades', '') %>%
      str_split('\\?') %>%
      flatten_chr() %>%
      .[[1]] %>%
      as.numeric()

    trade_df <-
      json_data$trades %>%
      as_tibble() %>%
      dplyr::rename(dateTrade = date) %>%
      mutate(dateTrade = dateTrade %>% lubridate::ymd())

    trade_df <-
      trade_df %>%
      separate(
        trade,
        into = c(
          "idCIKOwner",
          "idCIK",
          "idInsiderType",
          "countSharesOwned",
          "descriptionOption",
          "idTypeInsiderTransaction",
          "amountPrice",
          "countShares",
          "idInsiderTransaction",
          "X10",
          "detailOwnershipIndirect",
          "priceExcercised",
          "dateOptionExcercisable",
          "dateOptionExpiry",
          "countSharesOptions",
          "typeSecurityOption",
          "X17"
        ),
        sep = '\\*'
      ) %>%
      suppressWarnings() %>%
      select(-dplyr::matches("X"))

    trade_df <-
      trade_df %>%
      mutate_at(.vars =
                  trade_df %>% select(dplyr::matches("date")) %>% names(),
                .funs = lubridate::ymd) %>%
      mutate_at(.vars =
                  trade_df %>% select(dplyr::matches("idCIK|count|amount|price")) %>% names(),
                funs(. %>% as.character() %>% readr::parse_number())) %>%
      left_join(tibble(
        idInsiderType = c("D", "ND"),
        typeInsider = c("Director", "Non-Director")
      )) %>%
      left_join(get_insider_code_df()) %>%
      left_join(
        tibble(
          idTypeInsiderTransaction = c("A", "D", "None"),
          typeInsiderTransaction = c('Purchase', 'Sale', 'None'),
          isBought = c(TRUE, FALSE, NA)
        )
      ) %>%
      suppressMessages()

    trade_df <-
      trade_df %>%
      mutate(
        countShares = ifelse(isBought == T, countShares, -countShares),
        amountTransaction = countShares * amountPrice,
        urlJSON = url
      )

    has_indirect_owner <-
      trade_df$detailOwnershipIndirect %>% str_count("By") %>% sum() > 0

    if (has_indirect_owner) {
      trade_df <-
        trade_df %>%
        tidyr::separate(
          detailOwnershipIndirect,
          into = c('remove', "nameOwnerIndirect"),
          remove = FALSE,
          sep = 'By '
        ) %>%
        mutate(nameOwnerIndirect = nameOwnerIndirect %>% str_trim() %>% str_to_upper()) %>%
        select(-remove) %>%
        suppressWarnings()
    }

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% cat(fill = T)
    }
    return(trade_df)
  }

.parse_trades <-
  function(ticker = "FB",
           nest_data = TRUE,
           return_message = TRUE) {
    general_df <-
      .parse_company_general_safe(ticker = ticker, nest_data)

    cik <-
      general_df$idCIK

    trader_url <-
      list("http://rankandfiled.com/data/filer/", cik, '/traders') %>%
      purrr::invoke(paste0, .)

    count_trades <-
      .parse_json_traders(url = trader_url) %>%
      .$countTraders %>% unique() %/% 50

    trade_urls <-
      list(
        'http://rankandfiled.com/data/filer/',
        cik,
        '/trades?start=',
        seq(0, by = 50, length.out = count_trades)
      ) %>%
      purrr::invoke(paste0, .)

    .parse_json_trades_safe <-
      purrr::possibly(.parse_json_trades, NULL)

    all_trades <-
      trade_urls %>%
      future_map_dfr(function(x) {
        .parse_json_trades_safe(url = x, return_message = return_message)
      }) %>%
      distinct() %>%
      suppressWarnings()

    owners_df <-
      list("http://rankandfiled.com/data/filer/", cik, '/owners') %>%
      purrr::invoke(paste0, .) %>%
      .parse_json_owners(nest_data = nest_data)

    entity <-
      general_df$nameEntity

    all_trades <-
      all_trades %>%
      left_join(owners_df %>%
                  select(idCIKOwner = idCIKOwned, nameEntityOwner) %>%
                  distinct()) %>%
      suppressMessages()

    entity_name <-
      general_df$nameEntity

    all_trades <-
      all_trades %>%
      mutate(nameEntity = entity_name,
             idTicker = ticker) %>%
      select(idCIK,
             nameEntity,
             idTicker,
             dateTrade,
             idCIKOwner,
             nameEntityOwner,
             everything()) %>%
      suppressWarnings() %>%
      suppressMessages()

    all_trades <-
      all_trades %>%
      mutate_at(.vars = all_trades %>% select(dplyr::matches("count")) %>% names,
                funs(. %>% formattable::comma(digits = 0))) %>%
      mutate_at(.vars = all_trades %>% select(dplyr::matches("amount|price")) %>% names,
                funs(. %>% formattable::currency(digits = 2))) %>%
      select(idCIK:countShares, amountTransaction, everything()) %>%
      resolve_names_to_upper()

    if (return_message) {
      list("Parsed ", all_trades %>% nrow(), ' trades for ', entity) %>%
        purrr::invoke(paste0, .) %>%
        cat(fill = T)
    }

    return(all_trades)

  }

.parse_public_filings <-
  function(ticker = "FB",
           return_message = TRUE) {
    general_df <-
      .parse_company_general_safe(ticker = ticker)

    cik <-
      general_df$idCIK

    filing_pages <-
      general_df$countFilings %/% 50

    filing_urls <-
      list(
        'http://rankandfiled.com/data/filer/',
        cik,
        '/all?start=',
        seq(0, by = 50, length.out = filing_pages)
      ) %>%
      purrr::invoke(paste0, .)

    .parse_json_public_filers_safe <-
      purrr::possibly(.parse_json_public_filers, NULL)

    .all_filings <-
      filing_urls %>%
      future_map_dfr(function(x) {
        .parse_json_public_filers_safe(url = x, return_message = return_message)
      }) %>%
      distinct() %>%
      suppressWarnings()

    entity <-
      general_df$nameEntity

    .all_filings <-
      .all_filings %>%
      mutate(idTicker = ticker,
             nameCompany = entity,
             nameEntity = entity) %>%
      select(idCIK,
             idTicker,
             nameEntity,
             nameCompany,
             dateFiling,
             idRF,
             everything())

    if (return_message) {
      list("Parsed ", .all_filings %>% nrow(), ' SEC Filings for ', entity) %>%
        purrr::invoke(paste0, .) %>%
        cat(fill = T)
    }
    return(.all_filings)
  }

.parse_ticker_data <-
  function(ticker = "VNO",
           nest_data = TRUE,
           tables = NULL,
           return_message = TRUE) {
    if (length(tables) == 0) {
      tables <-
        c(
          'General',
          'CIK Filings',
          'Filings',
          'Private Offerings',
          'Related Parties',
          'Traders',
          'C Level',
          'MDA',
          'Owners',
          'Insider Trades',
          'Trades',
          'Subsidiaries'
        )
    }

    ticker <-
      ticker %>% str_to_upper()

    .parse_company_general_safe <-
      purrr::possibly(.parse_company_general, NULL)

    .parse_trades_safe <-
      purrr::possibly(.parse_trades, NULL)

    .parse_public_filings_safe <-
      purrr::possibly(.parse_public_filings, NULL)

    general <-
      .parse_company_general_safe(ticker = ticker,
                                  nest_data = nest_data,
                                  return_message = return_message) %>%
      suppressWarnings()

    has_trades <-
      "TRADES" %>% str_detect(tables %>% str_to_upper()) %>% sum() > 0

    if (has_trades) {
      trades <-
        .parse_trades_safe(ticker = ticker,
                           nest_data = nest_data,
                           return_message = return_message) %>%
        suppressWarnings()
    } else {
      trades <-
        tibble(idTicker = ticker)
    }

    cik_data <-
      general$idCIK %>%
      .parse_cik_data(tables = tables,
                      nest_data = nest_data,
                      return_message = return_message)

    if ('General' %in% cik_data$nameTable) {
      cik_data <-
        cik_data %>%
        filter(!nameTable == 'General')
    }
    all_data <-
      tibble(
        nameEntity = general$nameEntity,
        idCIK = general$idCIK,
        nameTable = c('Company Profile', 'Insider Trades'),
        dataTable = list(general, trades)
      ) %>%
      bind_rows(cik_data) %>%
      mutate(countCols = dataTable %>% map_dbl(ncol)) %>%
      filter(countCols > 1) %>%
      select(-countCols)


    if (return_message) {
      list("Acquired all data for ", all_data$nameEntity %>% unique()) %>%
        purrr::invoke(paste0, .) %>%
        cat(fill = T)
    }

    return(all_data)


  }

#' US Public Company snapshot
#'
#' This function returns snapshot details
#' of X public companies.  Information includes
#' corporate metadata, valuation metrics, and more.
#'
#' @param merge_type how to merge general information for public companies \itemize{
#' \item \code{NULL} and \code{MATCH}: only acquires metadata for unmatched batch import companies (default)
#' #' \item \code{ALL}: returns general information for all companies
#' }
#' @param return_message \code{TRUE} return a message after data import
#'
#' @return a \code{tibble}
#' @export
#' @import dplyr tidyr purrr stringr formattable readr lubridate
#' @importFrom jsonlite fromJSON
#' @family SEC
#' @family real-time data
#' @family Rank and Filed
#' @family entity search
#' @examples
#' \dontrun{
#' us_public_companies(merge_type = NULL)
#'
#' }
us_public_companies <-
  function(merge_type = NULL,
           return_message = TRUE) {
    no_merge <-
      (!'merge_type' %>% exists()) |
      (merge_type %>% purrr::is_null())

    if (no_merge) {
      merge_type <-
        'MATCH'
    }

    json_data <-
      "http://rankandfiled.com/data/public_companies" %>%
      jsonlite::fromJSON()

    company_data <-
      tibble(df = json_data$result$data %>%
               str_split(pattern = '\\|') %>%
               flatten_chr()) %>%
      tidyr::separate(
        df,
        sep = '\\*',
        into = c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9")
      ) %>%
      mutate_at(
        .vars = c("X5", "X6", "X7", "X8", "X9"),
        funs(. %>% as.character() %>% readr::parse_number())
      ) %>%
      purrr::set_names(
        c(
          'idTicker',
          'idExchange',
          'codeLocationBusiness',
          'codeLocationIncorporation',
          'idSector',
          'amountEquityMarketCap',
          'priceOpen',
          'price52WeekLow',
          'price52WeekHigh'
        )
      ) %>%
      left_join(tibble(
        idSector = 1:12,
        nameSector = c(
          'Finance',
          'Capital Goods',
          'Technology',
          'Transportation',
          'Consumer Services',
          'Health Care',
          'Consumer Durables',
          'Public Utilities',
          'Miscellaneous',
          'Basic Industries',
          'Energy',
          'Consumer Non Durables'
        )
      )) %>%
      suppressMessages() %>%
      left_join(tibble(
        idExchange = c('N', 'Q', 'A'),
        nameExchange = c('NYSE', 'NASDAQ', 'NYSE ARCA')
      )) %>%
      mutate(
        amountEquityMarketCap = ifelse(
          idTicker == 'BRK-A',
          amountEquityMarketCap * 100000000000,
          amountEquityMarketCap
        ),
        codeLocationBusiness = ifelse(
          codeLocationBusiness == '',
          codeLocationIncorporation,
          codeLocationBusiness
        ),
        codeLocationIncorporation = ifelse(codeLocationIncorporation == '',
                                           NA,
                                           codeLocationIncorporation),
        countSharesOutstanding = ifelse(priceOpen > 0,
                                        ((
                                          amountEquityMarketCap / priceOpen
                                        )),
                                        NA),
        pct52WeekHigh = ifelse(priceOpen > 0,
                               ((
                                 priceOpen / price52WeekHigh
                               )),
                               NA),
        pct52WeekLow = ifelse(priceOpen > 0,
                              ((
                                priceOpen / price52WeekLow
                              )),
                              NA),
        amountEquityMarketCap = (amountEquityMarketCap),
        urlTickerRankandFiled = list('http://rankandfiled.com/#/public/', idTicker, '/filings') %>% purrr::invoke(paste0, .)
      ) %>%
      select(idTicker:idSector, nameSector, everything()) %>%
      suppressMessages()

    countries <-
      location_codes()

    company_data <-
      company_data %>%
      left_join(
        countries %>%
          dplyr::rename(
            codeLocationBusiness = codeLocation,
            nameLocationBusiness = nameLocation
          )
      ) %>%
      left_join(
        countries %>%
          dplyr::rename(
            codeLocationIncorporation = codeLocation,
            nameLocationIncorporation = nameLocation
          )
      ) %>%
      suppressMessages()

    company_data <-
      company_data %>%
      filter(priceOpen > 0) %>%
      filter(!priceOpen %>% is.na()) %>%
      group_by(idTicker, nameSector) %>%
      filter(amountEquityMarketCap == max(amountEquityMarketCap, na.rm = TRUE)) %>%
      ungroup() %>%
      arrange(idTicker)

    ticker_count_df <-
      company_data %>%
      count(idTicker, sort = TRUE)

    fine_tickers <-
      ticker_count_df %>% filter(n < 2) %>% .$idTicker

    fine_df <-
      company_data %>%
      filter(idTicker %in% (fine_tickers))

    dup_count_df <-
      ticker_count_df %>% filter(n > 1)

    dup_df <-
      company_data %>%
      filter(idTicker %in% dup_count_df$idTicker) %>%
      arrange(idTicker)

    dup_general_df <-
      dup_count_df$idTicker %>%
      future_map_dfr(function(x) {
        .parse_company_general_safe(ticker = x)
      }) %>%
      arrange(idTicker)

    dup_df <-
      dup_general_df %>%
      select(idTicker, nameLocationBusiness = stateEntity, nameSector) %>%
      left_join(countries %>% dplyr::rename(nameLocationBusiness = nameLocation)) %>%
      dplyr::rename(codeLocationBusiness = codeLocation) %>%
      left_join(dup_df) %>%
      suppressMessages()

    company_data <-
      fine_df %>%
      bind_rows(dup_df) %>%
      arrange(idTicker)

    is_merge_all <-
      merge_type %>% str_to_upper() == 'ALL'

    is_match <-
      merge_type %>% str_to_upper() == 'MATCH'

    if (is_merge_all) {
      general_data <-
        company_data$idTicker %>%
        unique() %>%
        future_map_dfr(function(x) {
          .parse_company_general_safe(ticker = x, return_message = return_message)
        }) %>%
        suppressWarnings()

      company_data <-
        company_data %>%
        inner_join(general_data %>%
                     select(-one_of(
                       c(
                         "idExchange",
                         "nameSector",
                         "amountEquityMarketCap",
                         "priceOpen",
                         "price52WeekLow",
                         "price52WeekHigh",
                         "urlTickerRankandFiled"
                       )
                     ))) %>%
        dplyr::rename(nameCompany = nameEntity) %>%
        select(idTicker,
               nameCompany,
               idCIK,
               idSector,
               nameSector,
               nameExchange,
               everything())

      if (return_message) {
        list(
          "Acquired data for ",
          company_data %>% nrow() %>% formattable::comma(digits = 0),
          ' US stocks with a combined market capitalization of ',
          company_data$amountEquityMarketCap %>% sum(na.rm = TRUE) %>% formattable::currency(digits = 0)
        ) %>%
          purrr::invoke(paste0, .) %>%
          cat(fill = T)
      }

      return(company_data)
    }

    if (is_match) {
      all_tickers <-
        rf_us_tickers()

      company_data <-
        company_data %>%
        left_join(
          all_tickers %>%
            filter(!urlTickerRankandFiled %>% is.na()) %>%
            select(
              idTicker,
              idCIK,
              nameCompany,
              codeLocationBusiness,
              idSIC,
              classificationSIC
            )
        ) %>%
        suppressMessages()

      count_df <-
        company_data %>%
        count(idTicker, sort = TRUE)

      dup_tickers <-
        count_df %>%
        filter(n > 1) %>%
        .$idTicker %>%
        unique()

      fine_df <-
        company_data %>%
        filter(!idTicker %in% dup_tickers)

      dup_df <-
        company_data %>%
        filter(idTicker %in% dup_tickers)
      .parse_company_general_safe <-
        purrr::possibly(.parse_company_general, tibble)
      dup_general_df <-
        dup_tickers %>%
        future_map_dfr(function(x) {
          .parse_company_general_safe(ticker = x)
        }) %>%
        arrange(idTicker) %>%
        suppressWarnings()

      dup_df <-
        dup_df %>%
        select(-c(nameCompany, idCIK, idSIC, classificationSIC)) %>%
        distinct() %>%
        left_join(dup_general_df %>%
                    select(idTicker, idCIK, nameCompany = nameEntity)) %>%
        left_join(all_tickers %>%
                    select(idCIK, idSIC, classificationSIC)) %>%
        suppressWarnings() %>%
        suppressMessages()

      company_data <-
        fine_df %>%
        bind_rows(dup_df) %>%
        distinct()

      match_df <-
        company_data %>%
        filter(!nameCompany %>% is.na())

      missing_name_df <-
        company_data %>%
        filter(nameCompany %>% is.na()) %>%
        select(-c(nameCompany, idCIK)) %>%
        inner_join(all_tickers %>% select(idTicker, nameCompany, idCIK)) %>%
        suppressMessages()

      count_df <-
        missing_name_df %>%
        count(idTicker, sort = TRUE)

      dup_tickers <-
        count_df %>%
        filter(n > 1) %>%
        .$idTicker %>%
        unique()

      fine_df <-
        missing_name_df %>%
        filter(!idTicker %in% dup_tickers)

      dup_df <-
        missing_name_df %>%
        filter(idTicker %in% dup_tickers)

      dup_general_df <-
        dup_tickers %>%
        future_map_dfr(function(x) {
          .parse_company_general_safe(ticker = x)
        }) %>%
        arrange(idTicker) %>%
        suppressWarnings()

      dup_df <-
        dup_df %>%
        select(-c(nameCompany, idCIK)) %>%
        distinct() %>%
        left_join(dup_general_df %>% select(idTicker, nameCompany = nameEntity, idCIK)) %>%
        suppressMessages()

      missing_name_df <-
        fine_df %>%
        bind_rows(dup_df) %>%
        select(-c(idSIC, classificationSIC)) %>%
        left_join(all_tickers %>%
                    select(idCIK, idSIC, classificationSIC)) %>%
        suppressMessages()

      company_data <-
        match_df %>%
        bind_rows(missing_name_df) %>%
        arrange(desc(amountEquityMarketCap)) %>%
        select(
          idCIK,
          idTicker,
          nameCompany,
          idExchange,
          idSector,
          nameSector,
          idSIC,
          classificationSIC,
          everything()
        )

      company_data <-
        company_data %>%
        mutate_at(
          company_data %>% select(dplyr::matches("price")) %>% names(),
          funs(. %>% formattable::currency(digits = 2))
        ) %>%
        mutate_at(
          company_data %>% select(dplyr::matches("amount")) %>% names(),
          funs(. %>% formattable::currency(digits = 0))
        ) %>%
        mutate_at(
          company_data %>% select(dplyr::matches("pct")) %>% names(),
          funs(. %>% formattable::percent(digits = 2))
        )
      if (return_message) {
        list(
          "Acquired data for ",
          company_data %>% nrow() %>% formattable::comma(digits = 0),
          ' US Stocks with a combined market capitalization of ',
          company_data$amountEquityMarketCap %>% sum(na.rm = TRUE) %>% formattable::currency(digits = 0)
        ) %>%
          purrr::invoke(paste0, .) %>%
          cat(fill = T)
      }

      company_data <-
        company_data %>%
        resolve_names_to_upper()

      return(company_data)

    }
  }

# SEC - Subsidiary --------------------------------------------------------

.parse_sec_url_for_cik <-
  function(url) {
    url %>%
      str_replace_all("https://www.sec.gov/Archives/edgar/data/", '') %>%
      str_split('\\/') %>%
      flatten_chr() %>%
      .[[1]] %>%
      as.numeric()
  }

.get_loc_df <-
  function() {
    tibble(
      nameLocation = c(
        "AFGHANISTAN",
        "ALAND ISLANDS",
        "ALBANIA",
        "ALGERIA",
        "AMERICAN SAMOA",
        "ANDORRA",
        "ANGOLA",
        "ANGUILLA",
        "ANTARCTICA",
        "ANTIGUA AND BARBUDA",
        "ARGENTINA",
        "ARMENIA",
        "ARUBA",
        "AUSTRALIA",
        "AUSTRIA",
        "AUSTRIA-HUNGARY",
        "AZERBAIJAN",
        "BADEN",
        "BAHAMAS",
        "BAHRAIN",
        "BANGLADESH",
        "BARBADOS",
        "BAVARIA",
        "BELARUS",
        "BELGIUM",
        "BELIZE",
        "BENIN",
        "BERMUDA",
        "BHUTAN",
        "BOLIVIA, PLURINATIONAL STATE OF",
        "BONAIRE, SINT EUSTATIUS AND SABA",
        "BOSNIA AND HERZEGOVINA",
        "BOTSWANA",
        "BOUVET ISLAND",
        "BRAZIL",
        "BRITISH INDIAN OCEAN TERRITORY",
        "BRUNEI DARUSSALAM",
        "BULGARIA",
        "BURKINA FASO",
        "BURUNDI",
        "CAMBODIA",
        "CAMEROON",
        "CANADA",
        "CABO VERDE",
        "CAYMAN ISLANDS",
        "CENTRAL AFRICAN REPUBLIC",
        "CHAD",
        "CHILE",
        "CHINA",
        "CHRISTMAS ISLAND",
        "COCOS (KEELING) ISLANDS",
        "COLOMBIA",
        "COMOROS",
        "CONGO, THE DEMOCRATIC REPUBLIC OF THE",
        "CONGO",
        "COOK ISLANDS",
        "COSTA RICA",
        "COTE D'IVOIRE",
        "CROATIA",
        "CUBA",
        "CURACAO",
        "CYPRUS",
        "CZECH REPUBLIC",
        "CZECHOSLOVAKIA",
        "DENMARK",
        "DJIBOUTI",
        "DOMINICA",
        "DOMINICAN REPUBLIC",
        "ECUADOR",
        "EGYPT",
        "EL SALVADOR",
        "EQUATORIAL GUINEA",
        "ERITREA",
        "ESTONIA",
        "ETHIOPIA",
        "FALKLAND ISLANDS (MALVINAS)",
        "FAROE ISLANDS",
        "FIJI",
        "FINLAND",
        "FRANCE",
        "FRENCH GUIANA",
        "FRENCH POLYNESIA",
        "FRENCH SOUTHERN TERRITORIES",
        "GABON",
        "GAMBIA",
        "GEORGIA",
        "GERMAN DEMOCRATIC REPUBLIC",
        "FEDERAL REPUBLIC OF GERMANY",
        "GERMANY",
        "GHANA",
        "GIBRALTAR",
        "GREECE",
        "GREENLAND",
        "GRENADA",
        "GUADELOUPE",
        "GUAM",
        "GUATEMALA",
        "GUERNSEY",
        "GUINEA",
        "GUINEA-BISSAU",
        "GUYANA",
        "HAITI",
        "HANOVER",
        "HEARD ISLAND AND MCDONALD ISLANDS",
        "HESSE ELECTORAL",
        "HESSE GRAND DUCAL",
        "HOLY SEE (VATICAN CITY STATE)",
        "HONDURAS",
        "HONG KONG",
        "HUNGARY",
        "ICELAND",
        "INDIA",
        "INDONESIA",
        "IRAN, ISLAMIC REPUBLIC OF",
        "IRAQ",
        "IRELAND",
        "ISLE OF MAN",
        "ISRAEL",
        "ITALY",
        "JAMAICA",
        "JAPAN",
        "JERSEY",
        "JORDAN",
        "KAZAKHSTAN",
        "KENYA",
        "KIRIBATI",
        "KOREA",
        "KOREA, DEMOCRATIC PEOPLE'S REPUBLIC OF",
        "KOREA, REPUBLIC OF",
        "KOSOVO",
        "KUWAIT",
        "KYRGYZSTAN",
        "LAO PEOPLE'S DEMOCRATIC REPUBLIC",
        "LATVIA",
        "LEBANON",
        "LESOTHO",
        "LIBERIA",
        "LIBYA",
        "LIECHTENSTEIN",
        "LITHUANIA",
        "LUXEMBOURG",
        "MACAO",
        "MACEDONIA, THE FORMER YUGOSLAV REPUBLIC OF",
        "MADAGASCAR",
        "MALAWI",
        "MALAYSIA",
        "MALDIVES",
        "MALI",
        "MALTA",
        "MARSHALL ISLANDS",
        "MARTINIQUE",
        "MAURITANIA",
        "MAURITIUS",
        "MAYOTTE",
        "MECKLENBURG SCHWERIN",
        "MEXICO",
        "MICRONESIA, FEDERATED STATES OF",
        "MODENA",
        "MOLDOVA, REPUBLIC OF",
        "MONACO",
        "MONGOLIA",
        "MONTENEGRO",
        "MONTSERRAT",
        "MOROCCO",
        "MOZAMBIQUE",
        "MYANMAR",
        "NAMIBIA",
        "NAURU",
        "NEPAL",
        "NETHERLANDS",
        "NETHERLANDS ANTILLES",
        "NEW CALEDONIA",
        "NEW ZEALAND",
        "NICARAGUA",
        "NIGER",
        "NIGERIA",
        "NIUE",
        "NORFOLK ISLAND",
        "NORTHERN MARIANA ISLANDS",
        "NORWAY",
        "OMAN",
        "PAKISTAN",
        "PALAU",
        "PALESTINE, STATE OF",
        "PANAMA",
        "PAPUA NEW GUINEA",
        "PARAGUAY",
        "PARMA",
        "PERU",
        "PHILIPPINES",
        "PITCAIRN",
        "POLAND",
        "PORTUGAL",
        "PUERTO RICO",
        "QATAR",
        "REPUBLIC OF VIETNAM",
        "REUNION",
        "ROMANIA",
        "RUSSIAN FEDERATION",
        "RWANDA",
        "SAINT BARTHELEMY",
        "SAINT HELENA, ASCENSION AND TRISTAN DA CUNHA",
        "SAINT KITTS AND NEVIS",
        "SAINT LUCIA",
        "SAINT MARTIN (FRENCH PART)",
        "SAINT PIERRE AND MIQUELON",
        "SAINT VINCENT AND THE GRENADINES",
        "SAMOA",
        "SAN MARINO",
        "SAO TOME AND PRINCIPE",
        "SAUDI ARABIA",
        "SAXONY",
        "SENEGAL",
        "SERBIA",
        "SEYCHELLES",
        "SIERRA LEONE",
        "SINGAPORE",
        "SINT MAARTEN (DUTCH PART)",
        "SLOVAKIA",
        "SLOVENIA",
        "SOLOMON ISLANDS",
        "SOMALIA",
        "SOUTH AFRICA",
        "SOUTH GEORGIA AND THE SOUTH SANDWICH ISLANDS",
        "SOUTH SUDAN",
        "SPAIN",
        "SRI LANKA",
        "SUDAN",
        "SURINAME",
        "SVALBARD AND JAN MAYEN",
        "SWAZILAND",
        "SWEDEN",
        "SWITZERLAND",
        "SYRIAN ARAB REPUBLIC",
        "TAIWAN, PROVINCE OF CHINA",
        "TAJIKISTAN",
        "TANZANIA, UNITED REPUBLIC OF",
        "THAILAND",
        "TIMOR-LESTE",
        "TOGO",
        "TOKELAU",
        "TONGA",
        "TRINIDAD AND TOBAGO",
        "TUNISIA",
        "TURKEY",
        "TURKMENISTAN",
        "TURKS AND CAICOS ISLANDS",
        "TUSCANY",
        "TUVALU",
        "TWO SICILIES",
        "UGANDA",
        "UKRAINE",
        "UNITED ARAB EMIRATES",
        "UNITED KINGDOM",
        "UNITED STATES",
        "UNITED STATES MINOR OUTLYING ISLANDS",
        "URUGUAY",
        "UZBEKISTAN",
        "VANUATU",
        "VENEZUELA, BOLIVARIAN REPUBLIC OF",
        "VIET NAM",
        "VIRGIN ISLANDS, BRITISH",
        "VIRGIN ISLANDS, U.S.",
        "WALLIS AND FUTUNA",
        "WESTERN SAHARA",
        "WUERTTEMBURG",
        "YEMEN",
        "YEMEN ARAB REPUBLIC",
        "YEMEN PEOPLE'S REPUBLIC",
        "YUGOSLAVIA",
        "ZAMBIA",
        "ZANZIBAR",
        "ZIMBABWE",
        "ALABAMA",
        "ALASKA",
        "ARIZONA",
        "ARKANSAS",
        "CALIFORNIA",
        "COLORADO",
        "CONNECTICUT",
        "DELAWARE",
        "FLORIDA",
        "GEORGIA",
        "HAWAII",
        "IDAHO",
        "ILLINOIS",
        "INDIANA",
        "IOWA",
        "KANSAS",
        "KENTUCKY",
        "LOUISIANA",
        "MAINE",
        "MARYLAND",
        "MASSACHUSETTS",
        "MICHIGAN",
        "MINNESOTA",
        "MISSISSIPPI",
        "MISSOURI",
        "MONTANA",
        "NEBRASKA",
        "NEVADA",
        "NEW HAMPSHIRE",
        "NEW JERSEY",
        "NEW MEXICO",
        "NEW YORK",
        "NORTH CAROLINA",
        "NORTH DAKOTA",
        "OHIO",
        "OKLAHOMA",
        "OREGON",
        "PENNSYLVANIA",
        "RHODE ISLAND",
        "SOUTH CAROLINA",
        "SOUTH DAKOTA",
        "TENNESSEE",
        "TEXAS",
        "UTAH",
        "VERMONT",
        "VIRGINIA",
        "WASHINGTON",
        "WEST VIRGINIA",
        "WISCONSIN",
        "WYOMING",
        "DISTRICT OF COLUMBIA",
        "ENGLAND",
        "BRITISH VIRGIN ISLANDS",
        "NETHERLAND ANTILLES",
        "RUSSIA",
        "SOUTH KOREA",
        'TAIWAN',
        "VENEZUELA",
        'CHANNEL ISLANDS'
      )
    )
  }

.parse_page_sub_multi_item_html <-
  function(page) {
    locations <-
      .get_loc_df() %>%
      .$nameLocation
    subsidiaries <-
      page %>%
      html_nodes('td div') %>%
      html_text() %>%
      str_replace_all('\u0095 |\u0096|\u0095\n', '') %>%
      str_trim()

    subsidiaries <-
      subsidiaries[!subsidiaries == '']

    data_nodes <-
      page %>%
      html_nodes('td') %>%
      html_text() %>%
      str_replace_all('\u0095 |\u0096|\u0095\n', '') %>%
      str_trim() %>%
      str_to_upper()

    data_nodes <-
      data_nodes[!data_nodes == '']

    location_items <-
      data_nodes[data_nodes %in% locations]

    pct_vals <-
      tibble(value = data_nodes) %>%
      filter(!value %>% str_detect("\\([(1-9)]\\)")) %>%
      mutate(pctSubsidiaryOwned = value %>% as.numeric()) %>%
      filter(!pctSubsidiaryOwned %>% is.na()) %>%
      slice(seq_along(subsidiaries)) %>%
      .$pctSubsidiaryOwned / 100 %>%
      suppressWarnings() %>%
      suppressMessages()

    all_data <-
      tibble(
        nameSubsidiary = subsidiaries,
        nameLocationSubsidiary = location_items,
        pctSubsidiaryOwned = pct_vals
      ) %>%
      mutate(nameSubsidiary = nameSubsidiary %>% str_to_upper())

    return(all_data)
  }

.parse_page_subsidiary_table_html <-
  function(page,
           numbers = 1:10,
           hit_terms = c(
             "Organized",
             "STATE OR|STATE OF|JURISDICTION OF|JURISDICTION OF INCORPORATION OR ORGANIZATION|JURISDICTION|JURISDICTION OF INCORPORATION OR\nORGANIZATION",
             "NAME|ORGANIZED UNDER THE LAWS OF",
             'STATE OF ORGANIZATION',
             'STATE OR COUNTRY OF ORGANIZATION',
             'NAME OF SUBSIDIARY',
             'NAME',
             'ENTITY NAME',
             'the laws of',
             'Percentage of voting',
             'securities owned by',
             'immediate parent',
             'CERTAIN INTERMEDIARY SUBSIDIARIES',
             'Note:',
             'Organized',
             'Under the',
             'Laws of',
             'OWNED BY',
             'IMMEDIATE',
             'PARENT',
             "OWNS",
             "CERTAIN INTERMEDIARY SUBSIDIARIES",
             'PERCENTAGE',
             'OF VOTING',
             'SECURITIES'
           )) {
    is_ib1 <-
      page %>%
      html_nodes('b font') %>%
      html_text() %>% length() > 0

    if (is_ib1) {
      items_bold <-
        page %>%
        html_nodes('b font') %>%
        html_text() %>%
        str_to_upper() %>%
        str_replace_all('\n', ' ')
      items_bold <-
        stringi::stri_trans_general(items_bold, "Latin-ASCII")
      items_bold <-
        items_bold %>%
        str_split('\\-') %>%
        flatten_chr() %>%
        str_trim()
    } else {
      items_bold <-
        page %>%
        html_nodes('b') %>%
        html_text() %>%
        str_to_upper() %>%
        str_replace_all('\n', ' ') %>%
        stringi::stri_trans_general("Latin-ASCII")
      items_bold <-
        items_bold %>%
        str_split('\\-') %>%
        flatten_chr() %>%
        str_trim() %>%
        unique()
    }

    has_date <-
      items_bold %>% grep(month.name %>% str_to_upper() %>% paste(collapse = '|'), .) %>% length > 0

    if (has_date) {
      date_data <-
        items_bold[items_bold %>% grep(month.name %>% str_to_upper() %>% paste(collapse = '|'), .)] %>%
        lubridate::mdy()
    } else {
      date_data <-
        NA
    }

    hit_terms <-
      hit_terms %>%
      append(items_bold) %>%
      str_to_upper() %>%
      unique() %>%
      append(list('(', letters, ')') %>%
               purrr::invoke(paste0, .)) %>%
      paste0(collapse = '|')


    hit_terms_in <-
      hit_terms %>% str_split('\\|') %>%
      flatten_chr()

    locations <-
      .get_loc_df() %>%
      .$nameLocation

    all_data <-
      numbers %>%
      future_map_dfr(function(x) {
        css_selector <-
          paste0('td:nth-child(', x, ')')
        has_length <-
          page %>%
          html_nodes(css_selector) %>% length() > 0
        if (has_length) {
          item <-
            paste0("X" , x)

          value <-
            page %>%
            html_nodes(css_selector) %>%
            html_text() %>%
            str_trim()
          tibble(item, value)
        }
      }) %>%
      mutate(
        value = value %>% str_to_upper() %>% str_replace_all('\n  ', ' ') %>% str_replace_all('\u0096 ', '')
      ) %>%
      filter(!value == '')

    has_loc_key <-
      all_data %>%
      filter(value %in% locations) %>%
      nrow() > 0

    if (has_loc_key) {
      loc_cols <-
        all_data %>%
        filter(value %in% locations) %>%
        .$item %>%
        unique()
      if (loc_cols %>% length == 1) {
        loc_col <-
          loc_cols[[1]]
      }
    }

    has_pct <-
      all_data %>%
      filter(value %>% str_detect("PERCENT")) %>%
      .$item %>% unique() %>% length() > 0

    if (has_pct) {
      pct_col <-
        all_data %>%
        filter(value %>% str_detect("PERCENT")) %>%
        .$item %>% unique()
    } else {
      pct_col <-
        NA
    }

    is_whack <-
      pct_col[[1]] %in% loc_cols

    if (is_whack) {
      all_data <-
        page %>%
        .parse_page_sub_multi_item_html() %>%
        mutate(dateSubsidiaryAsOf = date_data)

      return(all_data)
    }

    all_data <-
      all_data %>%
      filter(!value %in% items_bold) %>%
      filter(!value %>% str_detect(paste0(items_bold %>% unique(), collapse = '|'))) %>%
      filter(!value %in% hit_terms_in) %>%
      filter(!value %>% str_detect(hit_terms))

    count_df <-
      all_data %>% count(item, sort = T) %>%
      arrange(item) %>%
      spread(item, n)

    off_one <-
      (count_df[, 2] %>% extract2(1)) - (count_df[, 1] %>% extract2(1)) == 1

    min_item <-
      count_df %>% gather(item, value) %>% filter(value == min(value)) %>% .$item

    change_pct <-
      has_pct & (pct_col == min_item) %>% sum() > 0

    if (change_pct) {
      pct_col <-
        names(count_df)[[3]]
    }

    if (off_one) {
      df <-
        all_data$item %>% unique() %>%
        future_map_dfr(function(x) {
          has_data <-
            all_data %>%
            filter(item == x) %>%
            filter(!value %>% is.na()) %>%
            filter(!value == '') %>%
            nrow()

          if (has_data) {
            all_data %>%
              filter(item == x) %>%
              filter(!value %>% is.na()) %>%
              filter(!value == '') %>%
              filter(!value %>% str_detect(hit_terms)) %>%
              mutate(idSubsidiary = 1:n())
          }
        }) %>%
        filter(!value %>% str_detect(hit_terms)) %>%
        spread(item, value)

      if (change_pct) {
        df <-
          df %>%
          select(-one_of(min_item))
      }
    }

    if (!off_one) {
      has_property <-
        items_bold %>% str_detect('PROPERTY') %>% sum() > 0
      if (has_property) {
        tables <-
          page %>%
          html_table(fill = T)
        df <-
          seq_along(tables) %>%
          future_map_dfr(function(x) {
            table_df <-
              tables[[x]] %>%
              data.frame(stringsAsFactors = FALSE) %>%
              as_tibble()

            column_df <-
              table_df %>% slice(1) %>%
              gather(column, value) %>%
              mutate(idColumn = 1:n()) %>%
              filter(!value %>% is.na()) %>%
              left_join(tibble(
                value = c(
                  "PROPERTY",
                  "ENTITIES",
                  "STATE OF FORMATION",
                  "DATE OF FORMATION",
                  " ",
                  'General Information:'
                ),
                nameItem = c(
                  'nameProperty',
                  'nameSubsidiary',
                  'locationOrganizationSubsidiary',
                  'dateSubsidiaryFormed',
                  'locationOrganizationSubsidiary',
                  'nameSubsidiary'
                )
              )) %>%
              suppressMessages()
            two_col <-
              column_df %>% nrow() == 2
            if (two_col) {
              column_df$nameItem[[2]] <-
                'locationOrganizationSubsidiary'
            }

            columns_keep <-
              column_df$idColumn

            table_df <-
              table_df <-
              table_df %>%
              select(columns_keep) %>%
              slice(-1) %>%
              purrr::set_names(column_df$nameItem)

            table_df <-
              table_df %>%
              mutate_all(funs(. %>% str_trim() %>% str_to_upper())) %>%
              mutate(nameSubsidiary = ifelse(nameSubsidiary == '', NA, nameSubsidiary)) %>%
              filter(!nameSubsidiary %>% is.na())


            if (two_col) {
              table_df <-
                table_df %>%
                tidyr::separate(
                  locationOrganizationSubsidiary,
                  into = c(
                    'locationOrganizationSubsidiary',
                    'dateSubsidiaryFormed'
                  ),
                  sep = 'FORMED'
                ) %>%
                suppressWarnings() %>%
                mutate(locationOrganizationSubsidiary = locationOrganizationSubsidiary %>% str_replace_all('\\,', '')) %>%
                mutate_all(funs(. %>% str_replace('\n', '') %>% str_trim()))
            }


            if ('nameProperty' %in% names(table_df)) {
              table_df <-
                table_df %>%
                mutate(nameProperty = ifelse(nameProperty == '', NA, nameProperty)) %>%
                mutate_all(funs(. %>% str_replace('\n|\n  |\n  ', '') %>% str_trim())) %>%
                mutate_all(funs(. %>% str_replace('\n', '') %>% str_trim())) %>%
                mutate_all(funs(. %>% str_replace('  ', ' ') %>% str_trim())) %>%
                fill(nameProperty)

            }

            return(table_df)
          })

        if ('dateSubsidiaryFormed' %in% names(df)) {
          df <-
            df %>%
            mutate(dateSubsidiaryFormed = dateSubsidiaryFormed %>% lubridate::mdy())
        }

        df <-
          df %>%
          mutate(idCIK = cik, urlSEC = url) %>%
          select(idCIK, nameSubsidiary, everything()) %>%
          mutate(
            locationOrganizationSubsidiary = locationOrganizationSubsidiary %>% str_replace_all(
              'A |LIMITED LIABILITY COMPANY|CORPORATION|LIMITED PARTNERSHIP'
            ) %>% str_trim()
          )

        return(df)
      }
      if (!has_property) {
        df <-
          all_data %>%
          mutate(value = ifelse(value == '', NA, value)) %>%
          filter(!value %>% is.na()) %>%
          group_by(item) %>%
          mutate(idSubsidiary = 1:n()) %>%
          spread(item, value) %>%
          filter(!X1 == '') %>%
          mutate(idSubsidiary = 1:n()) %>%
          gather(item, value, -c(X1, idSubsidiary)) %>%
          ungroup() %>%
          filter(!value %>% str_detect(hit_terms)) %>%
          spread(item, value)
      }

    }

    df <-
      df %>%
      dplyr::rename(nameSubsidiary = X1) %>%
      tidyr::separate(nameSubsidiary,
                      sep = '\\(',
                      into = c('nameSubsidiary', 'remove')) %>%
      select(-dplyr::matches("remove")) %>%
      mutate(nameSubsidiary = nameSubsidiary %>% str_trim()) %>%
      suppressWarnings() %>%
      select(-dplyr::matches("idSubsidiary"))

    if (has_pct) {
      names(df)[names(df) %>% grep(pct_col, .)] <-
        'pctSubsidiaryOwned'

      df <-
        df %>%
        mutate_at(df %>% select(dplyr::matches('pct')) %>% names(),
                  funs(. %>% as.numeric() / 100)) %>%
        suppressWarnings()
    }

    if (has_loc_key) {
      names(df)[names(df) %>% grep(loc_col, .)] <-
        'locationOrganizationSubsidiary'
    }

    df <-
      df %>%
      select(-dplyr::matches("X"))

    return(df)
  }

.parse_sec_subsidiary_url_html <-
  function(url = "https://www.sec.gov/Archives/edgar/data/34088/000003408816000065/xomexhibit21.htm",
           return_message = TRUE) {
    cik <-
      url %>%
      .parse_sec_url_for_cik()

    page <-
      url %>%
      read_html()

    is_zero <-
      page %>%
      html_nodes(paste0('td:nth-child(', 1, ')')) %>%
      length() == 0
    locations <-
      .get_loc_df() %>%
      .$nameLocation

    if (is_zero) {
      data <-
        page %>%
        html_nodes('font') %>%
        html_text() %>%
        str_replace_all('\\ ', ' ')

      data <-
        data[!data == '']


      is_parenth <-
        data %>% str_detect('\\(') %>% sum() / length(data) > .25

      if (is_parenth) {
        data <-
          data[data %>% str_detect('\\(')]

        df <-
          tibble(data) %>%
          separate(
            data,
            sep = '\\(',
            into = c('nameSubsidiary', 'locationOrganizationSubsidiary')
          ) %>%
          separate(
            locationOrganizationSubsidiary,
            sep = '\\)',
            into = c('locationOrganizationSubsidiary', 'remove')
          ) %>%
          select(-remove) %>%
          mutate_all(funs(. %>% str_trim() %>% str_to_upper())) %>%
          mutate(idCIK = cik, urlSEC = url) %>%
          select(-dplyr::matches("idSubsidiary"))

        if (return_message) {
          list("Parsed: ", url) %>%
            purrr::invoke(paste0, .) %>% cat(fill = T)
        }

        return(df)
      }

      is_nested <-
        page %>%
        html_nodes('b font') %>%
        html_text() %>% length() > 2

      if (is_nested) {
        locations_raw <-
          page %>%
          html_nodes('b font') %>%
          html_text() %>%
          str_replace_all('\\:', '') %>%
          str_to_upper()

        locations <-
          locations_raw[!locations_raw %>% str_detect('EXHIBIT|SUBSIDIARY|SUBSIDIARIES')]

        data <-
          data[data %>% nchar() > 3] %>% str_to_upper()

        df <-
          tibble(nameSubsidiary = data) %>%
          mutate(idRow = 1:n())

        .loc_df <-
          tibble(nameSubsidiary = locations) %>%
          inner_join(df %>% select(idRow, nameSubsidiary)) %>%
          mutate(idRow = idRow + 1) %>%
          select(locationOrganizationSubsidiary = nameSubsidiary, idRow) %>%
          suppressMessages()

        df <-
          df %>%
          filter(!nameSubsidiary %>% str_detect('SUBSIDIARY|SUBSIDIARIES')) %>%
          filter(!nameSubsidiary %>% str_detect(paste0(locations_raw, collapse = '|'))) %>%
          suppressWarnings()

        df <-
          df %>%
          left_join(.loc_df) %>%
          fill(locationOrganizationSubsidiary) %>%
          mutate(urlSEC = url, idCIK = cik) %>%
          select(idCIK,
                 nameSubsidiary,
                 locationOrganizationSubsidiary,
                 everything()) %>%
          select(-idRow) %>%
          suppressMessages() %>%
          select(-dplyr::matches("idSubsidiary"))
        if (return_message) {
          list("Parsed: ", url) %>%
            purrr::invoke(paste0, .) %>% cat(fill = T)
        }

        return(df)
      }
    }

    is_font_table <-
      page %>%
      html_nodes('b') %>%
      html_text() %>% length() == 0

    if (is_font_table) {
      all_data <-
        1:10 %>%
        future_map_dfr(function(x) {
          css_selector <-
            paste0('td:nth-child(', x, ')')
          has_length <-
            page %>%
            html_nodes(css_selector) %>% length() > 0
          if (has_length) {
            item <-
              paste0("X" , x)

            value <-
              page %>%
              html_nodes(css_selector) %>%
              html_text() %>%
              str_trim()
            tibble(item, value)
          }
        }) %>%
        mutate(
          value = value %>% str_to_upper() %>% str_replace_all('\n  ', ' ') %>% str_replace_all('\u0096 ', '')
        ) %>%
        filter(!value == '')


      has_loc_key <-
        all_data %>%
        filter(value %in% locations) %>%
        nrow() > 0

      if (has_loc_key) {
        loc_col <-
          all_data %>%
          filter(value %in% locations) %>%
          .$item %>%
          unique()
      }

      hit_terms_in <-
        c(
          "Organized",
          "STATE OR|STATE OF|JURISDICTION OF|JURISDICTION OF INCORPORATION OR ORGANIZATION|JURISDICTION|JURISDICTION OF INCORPORATION OR\nORGANIZATION",
          "NAME|ORGANIZED UNDER THE LAWS OF",
          'STATE OF ORGANIZATION',
          'STATE OR COUNTRY OF ORGANIZATION',
          'NAME OF SUBSIDIARY',
          'NAME',
          'ENTITY NAME',
          'the laws of',
          'Percentage of voting',
          'securities owned by',
          'immediate parent',
          'CERTAIN INTERMEDIARY SUBSIDIARIES',
          'PERCENT OWNED'
        )
      hit_terms <-
        hit_terms %>%
        str_to_upper() %>%
        paste0(collapse = '|')

      hit_terms_in <-
        hit_terms %>% str_split('\\|') %>%
        flatten_chr()

      has_pct_col <-
        all_data %>%
        filter(value %in% "100") %>%
        nrow() > 0 |
        (all_data %>% filter(value %>% str_detect('PERCENT')) %>% nrow() > 0)

      if (has_pct_col) {
        pct_col <-
          all_data %>%
          filter((value %in% "100") |
                   (value %>% str_detect("PERCENT"))) %>%
          .$item %>%
          unique() %>%
          .[[1]]
      }

      all_data <-
        all_data %>%
        filter(!value %in% hit_terms_in) %>%
        filter(!value %>% str_detect(hit_terms)) %>%
        filter(!value == '') %>%
        mutate(valueNC = value %>% nchar()) %>%
        filter(!value %>% str_detect("PERCENT"))

      if (!has_pct_col) {
        all_data <-
          all_data %>%
          filter(valueNC > 3)
      }
      all_data <-
        all_data %>%
        select(-valueNC) %>%
        group_by(item) %>%
        mutate(idSubsidiary = 1:n()) %>%
        spread(item, value) %>%
        ungroup() %>%
        dplyr::rename(nameSubsidiary = X1)

      if (has_loc_key) {
        names(all_data)[names(all_data) %in% loc_col] <-
          'locationOrganizationSubsidiary'
      }

      if (has_pct_col) {
        names(all_data)[names(all_data) %in% pct_col] <-
          'pctSubsidiaryOwned'

        all_data <-
          all_data %>%
          mutate(pctSubsidiaryOwned = pctSubsidiaryOwned %>% as.numeric() / 100)
      }

      all_data <-
        all_data %>%
        mutate(idCIK = cik,
               dateSubsidiaryAsOf = NA,
               urlSEC = url) %>%
        select(-dplyr::matches("idSubsidiary|^X"))

      if (return_message) {
        list("Parsed: ", url) %>%
          purrr::invoke(paste0, .) %>% cat(fill = T)
      }

      return(all_data)

    }

    df <-
      page %>%
      .parse_page_subsidiary_table_html() %>%
      suppressWarnings()

    df <-
      df %>%
      filter(!nameSubsidiary == '') %>%
      mutate(idCIK = cik, urlSEC = url) %>%
      select(-dplyr::matches("idSubsidiary")) %>%
      select(idCIK, everything())

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% cat(fill = T)
    }

    return(df %>% select(-dplyr::matches("idSubsidiary")))

  }

# url = 'https://www.sec.gov/Archives/edgar/data/19617/000095012301002499/y46253ex21-1.txt'
.parse_sec_subsidiary_url_text <-
  function(url = "https://www.sec.gov/Archives/edgar/data/899689/000104746903007996/a2104897zex-21.txt",
           return_message = TRUE) {
    cik <-
      url %>%
      .parse_sec_url_for_cik()
    data <-
      url %>%
      read_lines()

    data <-
      data[!data == '']
    has_s <-
      data %>% str_detect("<S>") %>% sum() > 0

    if (has_s) {
      data <-
        data[(data %>% grep("<S>", .) %>% .[[1]] + 1):length(data)]
    }

    data <-
      data[!data %>% str_detect("STATE OF|NAME OF|---|NAME OF SUBSIDIARY|ORGANIZED UNDER|THE LAWS OF|<")]

    data <-
      data[data %>% nchar() > 3]

    df <-
      seq_along(data) %>%
      future_map_dfr(function(x) {
        item <-
          data[[x]]

        items <-
          item %>%
          str_replace_all('\\   ', '\\:') %>%
          str_split('\\:') %>%
          flatten_chr() %>%
          str_trim() %>%
          str_to_upper()

        items <-
          items[!items == '']

        if (items %>% length() == 1) {
          return(tibble())
        }

        two_items <-
          items %>% length() == 2
        if (two_items) {
          table_data <-
            tibble(
              idSubsidiary = x,
              nameSubsidiary = items[[1]],
              locationOrganizationSubsidiary = items[[2]]
            )
        }
        three_items <-
          items %>% length() == 3
        if (three_items) {
          table_data <-
            tibble(
              idSubsidiary = x,
              nameSubsidiary = items[[1]],
              locationOrganizationSubsidiary = items[[2]],
              pctSubsidiaryOwned = items[[3]] %>% as.numeric() / 100
            )
        }

        table_data <-
          table_data %>%
          mutate(
            isChildSubsidiary = ifelse(nameSubsidiary %>% substr(1, 1) == "-", TRUE, FALSE),
            nameSubsidiary = nameSubsidiary %>% str_replace('\\-', '') %>% str_trim()
          )
        return(table_data)
      }) %>%
      mutate(idCIK = cik, urlSEC = url) %>%
      select(-dplyr::matches("idSubsidiary")) %>%
      select(idCIK,
             nameSubsidiary,
             locationOrganizationSubsidiary,
             everything()) %>%
      filter(!nameSubsidiary %in% c('NAME', 'ORGANIZED UNDER'))

    df <-
      df %>%
      filter(!nameSubsidiary == '')

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% cat(fill = T)
    }

    return(df)

  }

.parse_sec_subsidiary_url  <-
  function(url = "https://www.sec.gov/Archives/edgar/data/34088/000003408816000065/xomexhibit21.htm",
           return_message = TRUE)  {
    is_text <-
      url %>%
      str_detect("txt")

    is_html <-
      url %>%
      str_detect("html|htm")
    parse_sec_subsidiary_url_text_safe <-
      purrr::possibly(.parse_sec_subsidiary_url_text, tibble())

    parse_sec_subsidiary_url_html_safe <-
      purrr::possibly(.parse_sec_subsidiary_url_html, tibble())

    if (is_text) {
      data <-
        url %>%
        parse_sec_subsidiary_url_text_safe()
    }

    if (is_html) {
      data <-
        url %>%
        parse_sec_subsidiary_url_html_safe()
    }
    return(data)
  }


# form_parsing ------------------------------------------------------------


.parse_full_form_names <-
  function(sec_names) {
    df_names <-
      seq_along(sec_names) %>%
      future_map_dfr(function(x) {
        sec_name <-
          sec_names[[x]]

        name_pieces <-
          sec_name %>% str_replace_all('\\.value|\\.item', '')

        pieces <-
          name_pieces %>%
          str_split('\\.') %>%
          flatten_chr()

        pieces_no_num <-
          pieces[!pieces %>% str_detect("[0-9]")]
        peice_length <-
          pieces_no_num %>% length()

        is_street <-
          pieces %>% str_detect("street1|street2|Street1|Street2") %>% sum(na.rm = T) > 0

        name_item <-
          pieces_no_num[length(pieces_no_num)]

        if (sec_name %>% str_detect('filingManager')) {
          name_item <-
            pieces %>% paste0(collapse = '')

          df <-
            tibble(nameSECFull = sec_name,
                   nameSEC = name_item)
          return(df)
        }

        if (is_street) {
          name_item <-
            pieces[pieces %>% str_detect("street1|street2|Street1|Street2")]
        }

        is_sig <-
          name_pieces %>% str_detect('signature') & peice_length == 1

        is_footnote <-
          sec_name %>% str_detect('footnote')

        is_issuer <-
          sec_name %>% str_detect('\\issuer.[A-Z]')

        is_federal <-
          sec_name %>% str_detect(pattern = "federalExemptionsExclusions")

        if (is_federal) {
          df <-
            tibble(
              nameSECFull = sec_name,
              nameTable = pieces[[1]],
              nameSEC = name_item
            )

          return(df)
        }

        if (is_issuer) {

          items <-
            sec_name %>% str_split('\\.') %>% flatten_chr()

          countItem <-
            pieces[2] %>% as.character() %>% readr::parse_number() %>% suppressWarnings()

          name_item <-
            items[length(items)]

          df <-
            tibble(
              nameSECFull = sec_name,
              nameTable = 'issuer',
              countItem,
              nameSEC = name_item
            )
          return(df)
        }

        if (is_footnote) {
          if (pieces %>% length() == 1) {
            countItem <-
              0
            item <-
              pieces[[1]]
          } else {
            item <-
              pieces[[1]]
            countItem <-
              pieces[2] %>%as.character() %>%  readr::parse_number() %>% suppressWarnings()
          }
          return(tibble(nameTable = 'footnotes', nameSECFull = sec_name, nameSEC = item, countItem))
        }

        if (is_sig) {
          df <-
            tibble(nameTable = 'signatures', nameSECFull = sec_name, nameSEC = name_item)
          return(df)
        }

        if (peice_length == 1) {
          df <-
            tibble(nameSECFull = sec_name, nameSEC = name_item)
          return(df)
        }

        piece_count <-
          length(pieces)

        if (piece_count == 1) {
          df <-
            tibble(nameSECFull = sec_name, nameSEC = sec_name)
          return(df)
        }

        if (piece_count == 2 &!is_footnote) {


          df <-
            tibble(nameSECFull = sec_name,
                   nameTable = pieces[[1]] ,
                   nameSEC = name_item)

          return(df)
        }

        if (piece_count > 2) {
          countItem <-
            pieces[2] %>%as.character() %>%  readr::parse_number() %>% suppressWarnings()

          df <-
            tibble(
              nameSECFull = sec_name,
              nameTable = pieces[[1]] ,
              countItem,
              nameSEC = name_item
            )

          return(df)
        }

      }) %>%
      filter(!nameSEC == '')

    df_dictionary <-
      .sec_form_title_df()

    has_missing_names <-
      df_names$nameSEC[!df_names$nameSEC %in% df_dictionary$nameSEC] %>%
      length() > 0
    if (has_missing_names) {
      missing <-
        df_names$nameSEC[!df_names$nameSEC %in% df_dictionary$nameSEC] %>%
        unique()

      missing_names <-
        missing %>%
        paste0(collapse = '\n')
      stop(list("Missing:\n", missing_names) %>%
             purrr::reduce(paste0))
    }

    df_names <-
      df_names %>%
      left_join(df_dictionary) %>%
      suppressWarnings() %>%
      suppressMessages()

    if (!'nameTable' %in% names(df_names)) {
      df_names <-
        df_names %>%
        mutate(nameTable = 'asset')
    }

    df_names <-
      df_names %>%
      select(nameTable, nameSECFull, nameSEC, nameActual, everything()) %>%
      mutate(nameTable = nameTable %>% str_replace('Id',''),
             nameTable = ifelse(nameTable %in% c('issuerCredentials','securitiesIssued'), NA, nameTable)) %>%
      suppressWarnings() %>%
      suppressMessages()
  }

.parse_xml_tables <-
  function(url = "https://www.sec.gov/Archives/edgar/data/61004/000114036117000046/doc1.xml"){
    page <-
      url %>%
      xml2::read_xml()

    tables <-
      page %>%
      xml_contents() %>%
      xml_name() %>%
      unique()

    data <-
      seq_along(tables) %>%
      future_map_dfr(function(x){
        table <-
          tables[[x]]

        if (table %in% c('headerData', 'formData')) {
          form_tables <-
            page %>% xml_contents() %>% xml_name()

          table_loc <-
            table %>% grep(form_tables)
          xml_nodes <-
            page %>%
            xml_contents() %>% .[[table_loc]]
        }

        if (table %in% c('infoTable' , 'assets')) {
          xml_nodes <-
            page %>%
            xml_contents()
        }

        if (table == 'comment') {
          value <-
            page %>% xml_contents() %>% xml_text()

          df <-
            tibble(idTable = x, nameSECFull = table, value)
          return(df)

        }

        tables_special <- c('headerData', 'formData', 'infoTable', 'assets')

        if (!table %in% tables_special) {

          value_search <-
            list('//', table) %>% purrr::reduce(paste0)

          xml_nodes <-
            page %>%
            xml_contents() %>%
            xml_find_all(value_search)
        }
        if (xml_nodes %>% length() > 100) {
          list("Be patient there are ", xml_nodes %>% length() %>% formattable::comma(digits = 0), ' nodes to parse') %>%
            purrr::reduce(paste0) %>% cat(fill = T)
        }
        value_list <-
          xml_nodes %>% as_list()

        value_list <-
          value_list[value_list %>% future_map(length) %>% flatten_dbl() > 0]

        json_data <-
          value_list %>%
          jsonlite::toJSON(force = FALSE, dataframe = 'values') %>%
          jsonlite::fromJSON(simplifyDataFrame = TRUE, flatten = TRUE)

        wrong_output <-
          json_data %>% class() == 'array'

        if (wrong_output) {
          item <-
            xml_nodes %>% xml_name()
          value <-
            xml_nodes %>% xml_text()
          json_data <-
            tibble(item, value) %>%
            spread(item, value)
        }

        if (json_data %>% length() == 0) {
          return(tibble())
        }
        if ('summaryInfo' %in% names(json_data)) {
          json_data <-
            seq_along(json_data) %>% map(
              function(x){
                js_d <- json_data[x]
                if ('summaryInfo' %in% names(js_d)) {
                  if (js_d$summaryInfo$clarificationResponses %>% length() == 0) {
                    js_d$summaryInfo$clarificationResponses <-
                      NULL
                  }
                }
                return(js_d)
              }) %>%
            flatten()

          json_data <-
            json_data[json_data %>% future_map(function(x){data.frame(x, stringsAsFactors = F)} %>% nrow()) > 0]
        }

        json_data <-
          json_data %>%
          data.frame(stringsAsFactors = FALSE) %>%
          as_tibble() %>%
          mutate_all(as.character) %>%
          mutate(idTable = x) %>%
          gather(nameSECFull, value, -idTable) %>%
          arrange(idTable)
        return(json_data)
      })

    data <-
      data %>%
      mutate(isList = value %>% str_detect('list')) %>%
      filter(!isList) %>%
      select(-isList) %>%
      mutate(
        nameSECFull = nameSECFull %>% str_replace_all(
          "filerInfo.flags.|filerInfo.filer.|coverPage.|.filer.|\\flags.|filer.credentials.",
          ''
        ),
        nameSECFull = nameSECFull %>% str_replace_all('filerInfo.|issuerCredentials.', '')
      )


    rm(tables)
    rm(page)
    rm(url)
    return(data)
  }

.parse_sec_form <-
  function(url = "https://www.sec.gov/Archives/edgar/data/61004/000114036117000046/doc1.xml",
           return_message = TRUE) {
    data <-
      .parse_xml_tables(url = url)

    if (!'nameSECFull' %in% names(data)) {
      data <-
        data %>%
        mutate(nameSECFull = nameSEC)
    }

    cik <-
      url %>% str_replace_all('https://www.sec.gov/Archives/edgar/data/', '') %>% str_split('/') %>% flatten_chr() %>% .[[1]] %>% as.character() %>% readr::parse_number() %>% suppressMessages()

    df_title <-
      .sec_form_title_df()

    is_13FInfo <-
      url %>% str_detect('form13fInfoTable.xml|infotable.xml')
    sec_names <-
      data$nameSECFull %>% unique()

    df_names <-
      .parse_full_form_names(sec_names = sec_names)

    df_names <-
      df_names %>%
      mutate(nameTable = ifelse(
        nameSECFull %>% str_detect("issuerAddress"),
        "issuerAddress",
        nameTable),
        nameTable =  ifelse(
          nameSECFull %>% str_detect("reportingOwner"),
          "reportingOwner",
          nameTable)
      ) %>%
      mutate(nameTable = ifelse(nameSECFull %>% str_detect("issuerInfo."), 'issuerInfo', nameTable),
             nameTable = ifelse(nameSECFull %>% str_detect("securitiesIssued."), 'securitiesIssued', nameTable),
             nameTable = ifelse(nameSECFull %>% str_detect("summaryInfo."), 'summaryInfo', nameTable),
             nameTable = ifelse(nameSECFull %>% str_detect("^comment[A-Z]"), 'Comments', nameTable)
      )

    if (is_13FInfo) {
      df_names <-
        df_names %>%
        mutate(nameTable = 'holdingsInformation')
    }
    if (!'nameSEC' %in% names(data)) {
      data <- data %>%
        mutate(nameSEC = nameSECFull)
    }
    data <-
      data %>%
      select(-nameSEC) %>%
      left_join(df_names) %>%
      mutate(nameActual = ifelse(nameSECFull == "X.1.A.A.", 'idForm', nameActual)) %>%
      suppressMessages()

    if ('countItem' %in% names(data)) {
      data <-
        data %>%
        select(nameTable, countItem, nameSECFull, nameActual, everything()) %>%
        mutate(countItem = countItem - 1) %>%
        suppressMessages()
    }

    if ('property' %in% data$nameTable) {
      data <-
        data %>%
        mutate(nameTable = ifelse(nameTable %>% is.na(), 'Asset', nameTable))
    }

    has_metadata <-
      data %>%
      filter(nameTable %>% is.na()) %>% nrow() > 0

    if (has_metadata) {
      df_metadata <-
        data %>%
        filter(nameTable %>% is.na()) %>%
        select(nameActual, value) %>%
        group_by(nameActual) %>%
        mutate(countItem = 1:n() - 1) %>%
        arrange(countItem) %>%
        ungroup() %>%
        filter(!nameActual %>% str_detect('idCCC')) %>%
        mutate(nameActual = ifelse(countItem == 0, nameActual, nameActual %>% paste0(countItem))) %>%
        select(-countItem)

      col_order <-
        df_metadata$nameActual

      df_metadata <-
        df_metadata %>%
        spread(nameActual, value) %>%
        select(one_of(col_order)) %>%
        mutate(urlSECFiling = url) %>%
        .resolve_form_columns()
    } else {
      df_metadata <-
        tibble(idCIKFiler = cik,
               urlSECFiling = url)
    }

    tables <-
      data %>%
      filter(!nameTable %>% is.na()) %>%
      .$nameTable %>%
      unique()

    data <-
      seq_along(tables) %>%
      future_map(function(x) {
        table <-
          tables[[x]]
        table_name <-
          list('data',
               table %>% substr(1, 1) %>% str_to_upper(),
               table %>% substr(2, nchar(table))) %>%
          purrr::reduce(paste0)

        table_df <-
          data %>%
          filter(nameTable == table) %>%
          select(dplyr::matches("countItem"), nameActual, value) %>%
          select(which(colMeans(is.na(.)) < 1)) %>%
          group_by(nameActual) %>%
          mutate(countItem = 1:n() - 1) %>%
          ungroup()

        has_counts <-
          table_df$countItem %>% max(na.rm = TRUE) > 0

        if (has_counts) {
          table_df <-
            table_df %>%
            arrange(countItem)

          col_order <- c('countItem', table_df$nameActual)

          table_df <-
            table_df %>%
            spread(nameActual, value) %>%
            select(one_of(col_order)) %>%
            mutate(urlSECFiling = url) %>%
            .resolve_form_columns()

          table_df <-
            table_df %>%
            nest(-urlSECFiling, .key = data)
        } else {
          table_df <-
            table_df %>%
            select(-countItem)
          col_order <- c(table_df$nameActual)

          table_df <-
            table_df %>%
            spread(nameActual, value) %>%
            select(one_of(col_order)) %>%
            .resolve_form_columns() %>%
            mutate(urlSECFiling = url)

          table_df <-
            table_df %>%
            nest(-urlSECFiling, .key = data)
        }
        names(table_df)[[2]] <-
          table_name

        df_metadata <-
          df_metadata %>%
          left_join(table_df) %>%
          suppressMessages()

      }) %>%
      reduce(left_join) %>%
      suppressMessages()

    ## maybe add IDCK

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% cat(fill = T)
    }

    rm(df_metadata)
    return(data)
  }

.parse_form_data <-
  function(.all_filings, filter_parameter = 'isXBRLInstanceFile', return_message = TRUE) {
    df_search <-
      .all_filings %>%
      filter_(.dots = filter_parameter)

    if (filter_parameter == 'isXBRLInstanceFile') {
      if (df_search %>% nrow() == 0) {
        return(tibble())
      }
      parse_xbrl_filer_url_safe <-
        purrr::possibly(.parse_xbrl_filer_url, tibble())
      all_data <-
        df_search$urlSECFiling %>%
        unique() %>%
        future_map_dfr(function(x) {
          .parse_xbrl_filer_url(url = x, return_message = return_message)
        })
      all_data <-
        all_data %>%
        select(-dplyr::matches("idCIK1|nameFiler1")) %>%
        left_join(df_search %>% select(idForm, idAccession, nameFile, dateFiling, urlSECFiling)) %>%
        select(
          dplyr::matches("idCIK"),
          dplyr::matches("name[Entity]|name[Filer]"),
          dateFiling,
          idForm,
          idAccession,
          nameFile,
          everything()
        ) %>%
        suppressMessages()

      return(all_data)
    }

    if (filter_parameter == 'isFormD') {
      if ('idForm' %in% names(df_search)){
        df_search <-
          df_search %>%
          filter(!idForm %>% str_detect("10"))
      }
    }
    if (df_search %>% nrow() == 0) {
      return(tibble())
    }
    all_data <-
      df_search$urlSECFiling %>%
      unique() %>%
      future_map_dfr(function(x) {
        .parse_sec_form(url = x, return_message = return_message)
      })

    all_data <-
      all_data %>%
      select(-dplyr::matches("idCIK1|nameFiler1")) %>%
      left_join(df_search %>% select(dplyr::matches("idForm"), dplyr::matches("idAccession"), dplyr::matches("nameFile"), dplyr::matches("dateFiling"), urlSECFiling)) %>%
      select(
        dplyr::matches("idCIK"),
        dplyr::matches("name[Entity]|name[Filer]"),
        dateFiling,
        dplyr::matches("idForm"),
        dplyr::matches("idAccession"),
        dplyr::matches("nameFile"),
        everything()
      ) %>%
      suppressMessages()

    if (filter_parameter == 'hasAssetFile') {
      if('dataComments' %in% names(all_data)) {
        df_comments <-
          all_data %>%
          select(idCIKFiler, idAccession, dataComments) %>%
          mutate(isNULL = dataComments %>% map_lgl(is_null)) %>%
          filter(!isNULL) %>%
          distinct() %>%
          select(-isNULL)

        all_data <-
          all_data %>%
          select(-dataComments) %>%
          mutate(isNULL = dataAsset %>% map_lgl(is_null)) %>%
          filter(!isNULL) %>%
          filter(!nameFile == "ASSET RELATED DOCUMENT") %>%
          distinct() %>%
          select(-isNULL) %>%
          left_join(df_comments) %>%
          suppressMessages()
      }
    }
    return(all_data)
  }


# XBRL Finder -------------------------------------------------------------
.parse_xbrl_filer_url <-
  function(url = "https://www.sec.gov/Archives/edgar/data/1037540/000165642316000023/bxp-20160930.xml",
           return_message = TRUE) {
    options(stringsAsFactors = FALSE, scipen = 999999)
    cik <-
      url %>%
      str_split('data/') %>%
      flatten_chr() %>%
      .[[2]] %>%
      str_split('/') %>%
      flatten_chr() %>%
      .[[1]] %>%
      as.numeric()
    td <-
      tempdir()
    tf <-
      tempfile(tmpdir = td, fileext = ".xml")

    url %>%
      curl::curl_download(destfile = tf)

    doc <-
      tf %>%
      XBRL::xbrlParse()


    ## Get a data frame with facts:
    df_fct <-
      XBRL::xbrlProcessFacts(doc) %>%
      as_tibble()

    df_fct <-
      df_fct %>%
      mutate(
        isNumber = ifelse(!fact %>% as.character() %>% readr::parse_number() %>% is.na(), TRUE, FALSE),
        amountFact = ifelse(isNumber == TRUE, fact %>% as.character() %>% readr::parse_number(), NA)
      ) %>%
      separate(elementId,
               c('codeElement', 'nameElement'),
               sep = '\\_',
               remove = FALSE) %>%
      suppressWarnings()
    ## Get a data frame with contexts:
    df_cts <-
      XBRL::xbrlProcessContexts(doc) %>%
      as_tibble()
    ## Get a data frame with units:
    df_unt <-
      XBRL::xbrlProcessUnits(doc) %>%
      as_tibble()

    df_sch <-
      XBRL::xbrlGetSchemaName(doc) %>%
      as_tibble()

    df_footnotes <-
      XBRL::xbrlProcessFootnotes(doc) %>%
      as_tibble()


    ## Free the external memory used:
    XBRL::xbrlFree(doc)
    url_xsd <-
      url %>% str_replace(".xml", ".xsd")
    url_xsd %>%
      curl_download(destfile = tf)

    ## Parse the schema file:
    docS <-
      tf %>%
      XBRL::xbrlParse()
    ## Get roles:
    df_rls <-
      docS %>%
      XBRL::xbrlProcessRoles() %>%
      as_tibble()

    ## calculation
    url_cal <-
      url %>% str_replace(".xml", "_cal.xml")
    if (httr::url_ok(url_cal) %>% suppressWarnings()){
      url_cal %>%
        curl_download(destfile = tf)

      docS <-
        tf %>%
        XBRL::xbrlParse()

      df_calcs <-
        docS %>%
        XBRL::xbrlProcessArcs(arcType = 'calculation') %>%
        as_tibble()
    } else {
      df_calcs <-
        tibble()
    }

    ## definition
    url_def <-
      url %>% str_replace(".xml", "_def.xml")

    url_def %>%
      curl_download(destfile = tf)

    docS <-
      tf %>%
      XBRL::xbrlParse()

    df_defs <-
      docS %>%
      XBRL::xbrlProcessArcs(arcType = 'definition') %>%
      as_tibble()

    ## labels
    url_lab <-
      url %>% str_replace(".xml", "_lab.xml")

    url_lab %>%
      curl_download(destfile = tf)

    docS <-
      tf %>%
      XBRL::xbrlParse()

    df_labels <-
      docS %>%
      XBRL::xbrlProcessLabels() %>%
      as_tibble()

    ## presentation
    url_pre <-
      url %>% str_replace(".xml", "_pre.xml")

    url_pre %>%
      curl_download(destfile = tf)

    docS <-
      tf %>%
      XBRL::xbrlParse()

    ## Free the external memory used:
    tf %>%
      unlink()
    data <-
      tibble(
        idCIK = cik,
        urlSECFiling = url,
        dataFacts = list(df_fct),
        dataContexts = list(df_cts),
        dataUnits = list(df_unt),
        dataFootnotes = list(df_footnotes),
        dataRoles = list(df_rls),
        dataCalculations = list(df_calcs) ,
        dataDefinitions = list(df_defs),
        dataLabel = list(df_labels)
      )
    td %>% unlink()
    tf %>% unlink()
    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% cat(fill = T)
    }
    return(data)
  }

# dictionaries ------------------------------------------------------------
.sec_form_title_df <-
  function() {
    tibble(
      nameSEC = c(
        "conversionOrExercisePrice",
        "deemedExecutionDate",
        "directOrIndirectOwnership",
        "documentType",
        "equitySwapInvolved",
        "exerciseDate",
        "expirationDate",
        "footnote",
        "isDirector",
        "isOfficer",
        "isOther",
        "issuerCik",
        "issuerName",
        "issuerTradingSymbol",
        "isTenPercentOwner",
        "natureOfOwnership",
        "noSecuritiesOwned",
        "notSubjectToSection16",
        "officerTitle",
        "otherText",
        "periodOfReport",
        "postTransactionAmountsOwnedFollowingTransaction",
        "remarks",
        "rptOwnerCik",
        "rptOwnerCity",
        "rptOwnerName",
        "rptOwnerState",
        "rptOwnerStateDescription",
        "rptOwnerStreet1",
        "rptOwnerStreet2",
        "rptOwnerZipCode",
        "schemaVersion",
        "securityTitle",
        "sharesOwnedFollowingTransaction",
        "signatureDate",
        "signatureName",
        "transactionAcquiredDisposedCode",
        "transactionCode",
        "transactionDate",
        "transactionFormType",
        "transactionPricePerShare",
        "transactionShares",
        "transactionTimeliness",
        "transactionTotalValue",
        "underlyingSecurityShares",
        "underlyingSecurityTitle",
        "clarificationOfResponse", "isBusinessCombinationTransaction",
        "cik", "moreThanOneYear", "previousName", "edgarPreviousNameList",
        "entityName", "entityType", "entityTypeOtherDesc", "federalExemptionsExclusions",
        "industryGroupType", "investmentFundType", "investmentFundInfo",
        "hasNonAccreditedInvestors", "numberNonAccreditedInvestors",
        "totalNumberAlreadyInvested", "city", "stateOrCountry", "stateOrCountryDescription",
        "street1", "street2", "zipCode", "issuerPhoneNumber", "issuerPreviousNameList",
        "jurisdictionOfInc", "overFiveYears", "yearOfInc", "withinFiveYears",
        "yetToBeFormed", "aggregateNetAssetValueRange", "revenueRange",
        "minimumInvestmentAccepted", "totalAmountSold", "totalOfferingAmount",
        "totalRemaining", "firstName", "lastName", "middleName", "relationship",
        "relationshipClarification", "dollarAmount", "isEstimate", "associatedBDCRDNumber",
        "associatedBDName", "foreignSolicitation", "recipientCRDNumber",
        "recipientName", "description", "state", "statesOfSolicitationList",
        "authorizedRepresentative", "nameOfSigner", "signatureTitle",
        "submissionType", "testOrLive", "dateOfFirstSale", "yetToOccur",
        "isAmendment", "descriptionOfOtherType", "isDebtType", "isEquityType",
        "isMineralPropertyType", "isOptionToAcquireType", "isOtherType",
        "isPooledInvestmentFundType", "isSecurityToBeAcquiredType", "isTenantInCommonType",
        'notSubjectToSection16', 'rptOwnerStreet1', 'rptOwnerStreet2',

        "liveTestFlag", "confirmingCopyFlag", "returnCopyFlag", "overrideInternetFlag",
        "ccc", "reportCalendarOrQuarter", "filingManagername", "filingManageraddressstreet1",
        "filingManageraddressstreet2", "filingManageraddresscity", "filingManageraddressstateOrCountry",
        'filingManagerstateOrCountryDescription',
        "filingManageraddresszipCode", "reportType", "form13FFileNumber",
        "provideInfoForInstruction5", "name", "title", "phone", "signature",
        "otherIncludedManagersCount", "tableEntryTotal", "tableValueTotal",
        "isConfidentialOmitted",
        "nameOfIssuer", "titleOfClass", "cusip", "value", "investmentDiscretion",
        "otherManager", "putCall", "sshPrnamt", "sshPrnamtType", "Sole",
        "Shared", "None",

        "offeringFileNumber", "sinceLastFiling", "jurisdictionOrganization",
        "yearIncorporation", "sicCode", "irsNum", "fullTimeEmployees",
        "partTimeEmployees", "phoneNumber", "connectionName", "industryGroup",
        "cashEquivalents", "investmentSecurities", "accountsReceivable",
        "propertyPlantEquipment", "totalAssets", "accountsPayable", "longTermDebt",
        "totalLiabilities", "totalStockholderEquity", "totalLiabilitiesAndEquity",
        "totalRevenues", "costAndExpensesApplToRevenues", "depreciationAndAmortization",
        "netIncome", "earningsPerShareBasic", "earningsPerShareDiluted",
        "nameAuditor", "commonEquityClassName", "outstandingCommonEquity",
        "commonCusipEquity", "publiclyTradedCommonEquity", "preferredEquityClassName",
        "outstandingPreferredEquity", "preferredCusipEquity", "publiclyTradedPreferredEquity",
        "debtSecuritiesClassName", "outstandingDebtSecurities", "cusipDebtSecurities",
        "publiclyTradedDebtSecurities", "certifyIfTrue", "certifyIfNotDisqualified",
        "summaryInfo", "financialStatementAuditStatus", "securitiesOfferedTypes",
        "offerDelayedContinuousFlag", "offeringYearFlag", "offeringAfterQualifFlag",
        "offeringBestEffortsFlag", "solicitationProposedOfferingFlag",
        "resaleSecuritiesAffiliatesFlag", "securitiesOffered", "outstandingSecurities",
        "pricePerSecurity", "issuerAggregateOffering", "securityHolderAggegate",
        "qualificationOfferingAggregate", "concurrentOfferingAggregate",
        "totalAggregateOffering", "underwritersServiceProviderName",
        "underwritersFees", "auditorServiceProviderName", "auditorFees",
        "legalServiceProviderName", "legalFees", "promotersServiceProviderName",
        "promotersFees", "brokerDealerCrdNumber", "estimatedNetAmount",
        "clarificationResponses", "jurisdictionsOfSecOfferedSame", "issueJuridicationSecuritiesOffering",
        "dealersJuridicationSecuritiesOffering", "securitiesIssuerName",
        "securitiesIssuerTitle", "securitiesIssuedTotalAmount", "securitiesPrincipalHolderAmount",
        "securitiesIssuedAggregateAmount", "securitiesActExcemption",
        "certifyIfBadActor", "salesCommissionsServiceProviderName",
        "salesCommissionsServiceProviderFees", "jurisdictionsOfSecOfferedNone",
        "ifUnregsiteredNone", "blueSkyServiceProviderName", "blueSkyFees",
        'indicateTier1Tier2Offering', 'X.1.A.A.', 'X.1.A.A.', 'aggregateConsiderationBasis',
        'findersFeesServiceProviderName' , 'finderFeesFee',
        'loans', 'propertyAndEquipment', 'deposits', 'totalInterestIncome',
        'totalInterestExpenses', 'securitiesOfferedOtherDesc', 'comment',
        "assetTypeNumber",
        "assetNumber",
        "assetGroupNumber",
        "reportPeriodBeginningDate",
        "reportPeriodEndDate",
        "issuerName",
        "originalIssuanceDate",
        "originalSecurityAmount",
        "originalSecurityTermNumber",
        "securityMaturityDate",
        "originalAmortizationTermNumber",
        "originalInterestRatePercentage",
        "accrualTypeCode",
        "interestRateTypeCode",
        "originalInterestOnlyTermNumber",
        "firstPaymentDate",
        "underwritingIndicator",
        "securityTitleName",
        "denominationNumber",
        "currencyName",
        "trusteeName",
        "secFileNumber",
        "cik",
        "callableIndicator",
        "paymentFrequencyCode",
        "zeroCouponIndicator",
        "assetAddedIndicator",
        "assetModifiedIndicator",
        "reportPeriodBeginningAssetBalanceAmount",
        "reportPeriodBeginningScheduledAssetBalanceAmount",
        "reportPeriodScheduledPaymentAmount",
        "reportPeriodInterestRatePercentage",
        "totalActualPaidAmount",
        "actualInterestCollectionPercentage",
        "actualPrincipalCollectedAmount",
        "actualOtherCollectionAmount",
        "otherPrincipalAdjustmentAmount",
        "otherInterestAdjustmentAmount",
        "scheduledInterestAmount",
        "scheduledPrincipalAmount",
        "endReportingPeriodActualBalanceAmount",
        "endReportingPeriodScheduledBalanceAmount",
        "servicingFeePercentage",
        "servicingFlatFeeAmount",
        "zeroBalanceCode",
        "zeroBalanceEffectiveDate",
        "remainingTermToMaturityNumber",
        "currentDelinquentStatusNumber",
        "paymentPastDueDaysNumber",
        "paymentPastDueNumber",
        "nextReportPeriodPaymentDueAmount",
        "nextDueDate",
        "primaryLoanServicerName",
        "mostRecentServicingTransferReceivedDate",
        "assetSubjectToDemandIndicator",
        "statusAssetSubjectToDemandCode",
        "repurchaseAmount",
        "demandResolutionDate",
        "repurchaserName",
        "repurchaseReplacementReasonCode",
        "reportPeriodBeginDate",
        "originalLoanPurposeCode",
        "originatorName",
        "originalLoanAmount",
        "originalLoanMaturityDate",
        "originalInterestRateTypeCode",
        "originalLienPositionCode",
        "mostRecentJuniorLoanBalanceAmount",
        "mostRecentJuniorLoanBalanceDate",
        "mostRecentSeniorLoanAmount",
        "mostRecentSeniorLoanAmountDate",
        "loanTypeMostSeniorLienCode",
        "mostSeniorLienHybridPeriodNumber",
        "mostSeniorLienNegativeAmortizationLimitPercentage",
        "mostSeniorLienOriginationDate",
        "prepaymentPenaltyIndicator",
        "negativeAmortizationIndicator",
        "modificationIndicator",
        "modificationNumber",
        "mortgageInsuranceRequirementIndicator",
        "balloonIndicator",
        "coveredHighCostCode",
        "servicerHazardInsuranceCode",
        "refinanceCashOutAmount",
        "totalOriginationDiscountAmount",
        "brokerIndicator",
        "channelCode",
        "nationalMortgageLicenseSystemCompanyNumber",
        "buyDownNumber",
        "loanDelinquencyAdvanceNumber",
        "originationARMIndexCode",
        "armMarginPercentage",
        "fullyIndexedRatePercentage",
        "initialFixedRatePeriodHybridARMNumber",
        "initialInterestRateDecreasePercentage",
        "initialInterestRateIncreasePercentage",
        "indexLookbackNumber",
        "subsequentInterestRateResetNumber",
        "lifetimeRateCeilingPercentage",
        "lifetimeRateFloorPercentage",
        "subsequentInterestRateDecreasePercentage",
        "subsequentInterestRateIncreasePercentage",
        "subsequentPaymentResetNumber",
        "armRoundCode",
        "armRoundPercentage",
        "optionArmIndicator",
        "paymentMethodAfterRecastCode",
        "initialMinimumPaymentAmount",
        "convertibleIndicator",
        "HELOCIndicator",
        "HELOCDrawNumber",
        "prepaymentPenaltyCalculationCode",
        "prepaymentPenaltyTypeCode",
        "prepaymentPenaltyTotalTermNumber",
        "prepaymentPenaltyHardTermNumber",
        "negativeAmortizationLimitAmount",
        "negativeAmortizationInitialRecastNumber",
        "negativeAmortizationSubsequentRecastNumber",
        "negativeAmortizationBalanceAmount",
        "initialFixedPaymentNumber",
        "initialPaymentCapPercentage",
        "subsequentPaymentCapPercentage",
        "initialMinimumPaymentResetNumber",
        "subsequentMinimumPaymentResetNumber",
        "minimumPaymentAmount",
        "geographicalLocation",
        "occupancyStatusCode",
        "mostRecentOccupancyStatusCode",
        "propertyTypeCode",
        "mostRecentPropertyValueAmount",
        "mostRecentPropertyValueTypeCode",
        "mostRecentPropertyValueDate",
        "mostRecentAVMModelCode",
        "mostRecentAVMConfidenceNumber",
        "originalCLTVPercentage",
        "originalLTVPercentage",
        "originalObligorNumber",
        "originalObligorCreditScoreNumber",
        "originalObligorCreditScoreType",
        "mostRecentObligorCreditScoreNumber",
        "mostRecentObligorCreditScoreType",
        "mostRecentObligorCreditScoreDate",
        "obligorIncomeVerificationLevelCode",
        "IRSForm4506TIndicator",
        "originatorFrontEndDTIPercentage",
        "originatorBackEndDTIPercentage",
        "obligorEmploymentVerificationCode",
        "obligorEmploymentLengthCode",
        "obligorAssetVerificationCode",
        "originalPledgedAssetsAmount",
        "qualificationMethodCode",
        "mortgageInsuranceCompanyName",
        "mortgageInsuranceCoveragePercentage",
        "poolInsuranceCompanyName",
        "poolInsuranceStopLossPercentage",
        "mortgageInsuranceCoverageTypeCode",
        "modificationIndicatorReportingPeriod",
        "nextPaymentDueDate",
        "advancingMethodCode",
        "servicingAdvanceMethodologyCode",
        "stopPrincipalInterestAdvancingDate",
        "reportingPeriodBeginningLoanBalanceAmount",
        "reportingPeriodBeginningScheduledLoanBalanceAmount",
        "nextReportingPeriodPaymentDueAmount",
        "reportingPeriodInterestRatePercentage",
        "nextInterestRatePercentage",
        "otherAssessedUncollectedServicerFeeamount",
        "otherServicingFeeRetainedByServicerAmount",
        "reportingPeriodEndActualBalanceAmount",
        "reportingPeriodEndScheduledBalanceAmount",
        "reportingPeriodScheduledPaymentAmount",
        "actualInterestCollectedAmount",
        "actualOtherCollectedAmount",
        "paidThroughDate",
        "interestPaidThroughDate",
        "paidFullAmount",
        "servicerAdvancedPrincipalAmount",
        "servicerAdvancedRepaidPrincipalAmount",
        "servicerAdvancedCumulativePrincipalAmount",
        "servicerAdvanceInterestAmount",
        "servicerAdvanceRepaidInterestAmount",
        "servicerAdvanceCumulativeInterestAmount",
        "servicerAdvanceTaxesInsuranceAmount",
        "servicerAdvanceRepaidTaxesInsuranceAmount",
        "servicerAdvanceCumulativeTaxesInsuranceAmount",
        "servicerAdvanceCorporateAmount",
        "servicerAdvanceRepaidCorporateAmount",
        "servicerAdvanceCumulativeCorporateAmount",
        "mostRecentTwelveMonthHistoryCode",
        "nextResetRatePercentage",
        "nextPaymentChangeDate",
        "nextInterestRateChangeDate",
        "nextResetPaymentAmount",
        "exercisedArmConversionOptionIndicator",
        "primaryServicerName",
        "masterServicerName",
        "specialServicerName",
        "subServicerName",
        "assetSubjectDemandIndicator",
        "assetSubjectDemandStatusCode",
        "repurchaseReplacementCode",
        "chargeOffPrincipalAmount",
        "chargeOffInterestAmount",
        "lossMitigationTypeCode",
        "mostRecentLoanModificationEventCode",
        "mostRecentLoanModificationEffectiveDate",
        "postModificationMaturityDate",
        "postModificationInterestRateTypeCode",
        "postModificationAmortizationTypeCode",
        "postModificationInterestPercentage",
        "postModificationFirstPaymentDate",
        "postModificationLoanBalanceAmount",
        "postModificationPrincipalInterestPaymentAmount",
        "totalCapAmount",
        "incomeVerificationIndicatorAtModification",
        "modificationFrontEndDebtToIncomePercentage",
        "modificationBackEndDebtToIncomePercentage",
        "totalDeferredAmount",
        "forgivenPrincipalCumulativeAmount",
        "forgivenPrincipalReportingPeriodAmount",
        "forgivenInterestCumulativeAmount",
        "forgivenInterestReportingPeriodAmount",
        "actualEndingBalanceTotalDebtAmount",
        "scheduledEndingBalanceTotalDebtAmount",
        "postModificationARMCode",
        "postModificationARMIndexCode",
        "postModificationMarginPercentage",
        "postModificationInterestResetNumber",
        "postModificationNextResetDate",
        "postModificationIndexLookbackNumber",
        "postModificationARMRoundingCode",
        "postModificationARMRoundingPercentage",
        "postModificationInitialMinimumPayment",
        "postModificationNextPaymentAdjustmentDate",
        "postModificationARMPaymentRecastFrequency",
        "postModificationLifetimeFloorPercentage",
        "postModificationLifetimeCeilingPercentage",
        "postModificationInitialInterestRateIncreasePercentage",
        "postModificationInitialInterestRateDecreasePercentage",
        "postModificationSubsequentInterestIncreasePercentage",
        "postModificationSubsequentInterestRateDecreasePercentage",
        "postModificationPaymentCapPercentage",
        "postModificationPaymentMethodAfterRecastCode",
        "postModificationARMInterestRateTeaserNumber",
        "postModificationARMPaymentTeaserNumber",
        "postModificationARMNegativeAmortizationIndicator",
        "postModificationARMNegativeAmortizationCapPercentage",
        "postModificationInterestOnlyTermNumber",
        "postModificationInterestOnlyLastPaymentDate",
        "postModificationBalloonAmount",
        "postModificationInterestRateStepIndicator",
        "postModificationStepInterestPercentage",
        "postModificationStepDate",
        "postModificationStepPrincipalInterestPaymentAmount",
        "postModificationStepNumber",
        "postModificationMaximumFutureStepAgreementPercentage",
        "postModificationMaximumStepAgreementRateDate",
        "nonInterestBearingDeferredPrincipalCumulativeAmount",
        "nonInterestBearingDeferredPrincipalReportingPeriodAmount",
        "recoveryDeferredPrincipalReportingPeriodAmount",
        "nonInterestBearingDeferredPaidFullAmount",
        "nonInterestBearingDeferredInterestFeeReportingPeriodAmount",
        "nonInterestBearingDeferredInterestFeeCumulativeAmount",
        "recoveryDeferredInterestFeeReportingPeriodAmount",
        "mostRecentForbearancePlanOrTrialModificationStartDate",
        "mostRecentForbearancePlanOrTrialModificationScheduledEndDate",
        "mostRecentTrialModificationViolatedDate",
        "mostRecentRepaymentPlanStartDate",
        "mostRecentRepaymentPlanScheduledEndDate",
        "mostRecentRepaymentPlanViolatedDate",
        "shortSaleAcceptedOfferAmount",
        "mostRecentLossMitigationExitDate",
        "mostRecentLossMitigationExitCode",
        "attorneyReferralDate",
        "foreclosureDelayReasonCode",
        "foreclosureExitDate",
        "foreclosureExitReasonCode",
        "noticeOfIntentDate",
        "mostRecentAcceptedREOOfferAmount",
        "mostRecentAcceptedREOOfferDate",
        "grossLiquidationProceedsAmount",
        "netSalesProceedsAmount",
        "reportingPeriodLossPassedToIssuingEntityAmount",
        "cumulativeTotalLossPassedToIssuingEntityAmount",
        "subsequentRecoveryAmount",
        "evictionIndicator",
        "reoExitDate",
        "reoExitReasonCode",
        "UPBLiquidationAmount",
        "servicingFeesClaimedAmount",
        "servicerAdvanceReimbursedPrincipalAmount",
        "servicerAdvanceReimbursedInterestAmount",
        "servicerAdvanceReimbursedTaxesInsuranceAmount",
        "servicerAdvanceReimbursedCorporateAmount",
        "REOManagementFeesAmount",
        "cashKeyDeedAmount",
        "performanceIncentiveFeesAmount",
        "mortgageInsuranceClaimFiledDate",
        "mortgageInsuranceClaimAmount",
        "mortgageInsuranceClaimPaidDate",
        "mortgageInsuranceClaimPaidAmount",
        "mortgageInsuranceClaimDeniedRescindedDate",
        "marketableTitleTransferDate",
        "nonPayStatusCode",
        "reportingActionCode",
        "GroupID",
        "reportingPeriodBeginningDate",
        "reportingPeriodEndDate",
        "originationDate",
        "originalTermLoanNumber",
        "maturityDate",
        "interestRateSecuritizationPercentage",
        "interestAccrualMethodCode",
        "firstLoanPaymentDueDate",
        "lienPositionSecuritizationCode",
        "loanStructureCode",
        "paymentTypeCode",
        "periodicPrincipalAndInterestPaymentSecuritizationAmount",
        "scheduledPrincipalBalanceSecuritizationAmount",
        "NumberPropertiesSecuritization",
        "NumberProperties",
        "graceDaysAllowedNumber",
        "interestOnlyIndicator",
        "prepaymentPremiumIndicator",
        "modifiedIndicator",
        "armIndexCode",
        "firstRateAdjustmentDate",
        "firstPaymentAdjustmentDate",
        "armMarginNumber",
        "lifetimeRateCapPercentage",
        "periodicRateIncreaseLimitPercentage",
        "periodicRateDecreaseLimitPercentage",
        "periodicPaymentAdjustmentMaximumAmount",
        "periodicPaymentAdjustmentMaximumPercent",
        "rateResetFrequencyCode",
        "paymentResetFrequencyCode",
        "indexLookbackDaysNumber",
        "prepaymentLockOutEndDate",
        "yieldMaintenanceEndDate",
        "prepaymentPremiumsEndDate",
        "maximumNegativeAmortizationAllowedPercentage",
        "maximumNegativeAmortizationAllowedAmount",
        "negativeAmortizationDeferredInterestCapAmount",
        "deferredInterestCumulativeAmount",
        "deferredInterestCollectedAmount",
        "property",
        "reportPeriodModificationIndicator",
        "reportPeriodBeginningScheduleLoanBalanceAmount",
        "totalScheduledPrincipalInterestDueAmount",
        "servicerTrusteeFeeRatePercentage",
        "unscheduledPrincipalCollectedAmount",
        "reportPeriodEndActualBalanceAmount",
        "reportPeriodEndScheduledLoanBalanceAmount",
        "hyperAmortizingDate",
        "servicingAdvanceMethodCode",
        "nonRecoverabilityIndicator",
        "totalPrincipalInterestAdvancedOutstandingAmount",
        "totalTaxesInsuranceAdvancesOutstandingAmount",
        "otherExpensesAdvancedOutstandingAmount",
        "paymentStatusLoanCode",
        "armIndexRatePercentage",
        "nextInterestRateChangeAdjustmentDate",
        "nextPaymentAdjustmentDate",
        "mostRecentSpecialServicerTransferDate",
        "mostRecentMasterServicerReturnDate",
        "realizedLossToTrustAmount",
        "liquidationPrepaymentCode",
        "liquidationPrepaymentDate",
        "prepaymentPremiumYieldMaintenanceReceivedAmount",
        "workoutStrategyCode",
        "lastModificationDate",
        "modificationCode",
        "postModificationPaymentAmount",
        "postModificationAmortizationPeriodAmount",
        "propertyName",
        "propertyAddress",
        "propertyCity",
        "propertyState",
        "propertyZip",
        "propertyCounty",
        "netRentableSquareFeetNumber",
        "netRentableSquareFeetSecuritizationNumber",
        "unitsBedsRoomsNumber",
        "unitsBedsRoomsSecuritizationNumber",
        "yearBuiltNumber",
        "yearLastRenovated",
        "valuationSecuritizationAmount",
        "valuationSourceSecuritizationCode",
        "valuationSecuritizationDate",
        "mostRecentValuationAmount",
        "mostRecentValuationDate",
        "mostRecentValuationSourceCode",
        "physicalOccupancySecuritizationPercentage",
        "mostRecentPhysicalOccupancyPercentage",
        "propertyStatusCode",
        "defeasanceOptionStartDate",
        "DefeasedStatusCode",
        "largestTenant",
        "squareFeetLargestTenantNumber",
        "leaseExpirationLargestTenantDate",
        "secondLargestTenant",
        "squareFeetSecondLargestTenantNumber",
        "leaseExpirationSecondLargestTenantDate",
        "thirdLargestTenant",
        "squareFeetThirdLargestTenantNumber",
        "leaseExpirationThirdLargestTenantDate",
        "financialsSecuritizationDate",
        "mostRecentFinancialsStartDate",
        "mostRecentFinancialsEndDate",
        "revenueSecuritizationAmount",
        "mostRecentRevenueAmount",
        "operatingExpensesSecuritizationAmount",
        "operatingExpensesAmount",
        "netOperatingIncomeSecuritizationAmount",
        "mostRecentNetOperatingIncomeAmount",
        "netCashFlowFlowSecuritizationAmount",
        "mostRecentNetCashFlowAmount",
        "netOperatingIncomeNetCashFlowSecuritizationCode",
        "netOperatingIncomeNetCashFlowCode",
        "mostRecentDebtServiceAmount",
        "debtServiceCoverageNetOperatingIncomeSecuritizationPercentage",
        "mostRecentDebtServiceCoverageNetOperatingIncomePercentage",
        "debtServiceCoverageNetCashFlowSecuritizationPercentage",
        "mostRecentDebtServiceCoverageNetCashFlowpercentage",
        "debtServiceCoverageSecuritizationCode",
        "mostRecentDebtServiceCoverageCode",
        "mostRecentAnnualLeaseRolloverReviewDate",
        "reportingPeriodEndingDate",
        "originalLoanTerm",
        "loanMaturityDate",
        "interestCalculationTypeCode",
        "originalFirstPaymentDate",
        "gracePeriodNumber",
        "subvented",
        "vehicleManufacturerName",
        "vehicleModelName",
        "vehicleNewUsedCode",
        "vehicleModelYear",
        "vehicleTypeCode",
        "vehicleValueAmount",
        "vehicleValueSourceCode",
        "obligorCreditScoreType",
        "obligorCreditScore",
        "coObligorIndicator",
        "paymentToIncomePercentage",
        "obligorGeographicLocation",
        "reportingPeriodModificationIndicator",
        "nextReportingPeriodPaymentAmountDue",
        "otherServicerFeeRetainedByServicer",
        "otherAssessedUncollectedServicerFeeAmount",
        "reportingPeriodActualEndBalanceAmount",
        "totalActualAmountPaid",
        "servicerAdvancedAmount",
        "currentDelinquencyStatus",
        "chargedoffPrincipalAmount",
        "recoveredAmount",
        "modificationTypeCode",
        "paymentExtendedNumber",
        "repossessedIndicator",
        "repossessedProceedsAmount",
        "reportingPeriodBeginDate",
        "acquisitionCost",
        "originalLeaseTermNumber",
        "scheduledTerminationDate",
        "gracePeriod",
        "baseResidualValue",
        "baseResidualSourceCode",
        "contractResidualValue",
        "lesseeCreditScoreType",
        "lesseeCreditScore",
        "lesseeIncomeVerificationLevelCode",
        "lesseeEmploymentVerificationCode",
        "coLesseePresentIndicator",
        "lesseeGeographicLocation",
        "remainingTermNumber",
        "reportingPeriodSecuritizationValueAmount",
        "securitizationDiscountRate",
        "otherLeaseLevelServicingFeesRetainedAmount",
        "reportingPeriodEndingActualBalanceAmount",
        "reportingPeriodEndActualSecuritizationAmount",
        "primaryLeaseServicerName",
        "DemandResolutionDate",
        "repurchaseOrReplacementReasonCode",
        "chargedOffAmount",
        "leaseExtended",
        "terminationIndicator",
        "excessFeeAmount",
        "liquidationProceedsAmount",
        "commentNumber", "commentColumn", "commentDescription",
        'previousAccessionNumber', 'itemNumber', 'fieldName', 'notes'
      ),
      nameActual = c(
        "priceExerciseConversion",
        "dateDeemedExecution",
        "codeOwnershipDirectIndirect",
        "idDocument",
        "isEquitySwapInvolved",
        "dateExercised",
        "dateExpiration",
        "descriptionFootnote",
        "isDirector",
        "isOfficer",
        "isOther",
        "idCIKIssuer",
        "nameIssuer",
        "idTickerIssuer",
        "isTenPercentOwner",
        "descriptionNatureOfOwnership",
        "isNoSecuritiesOwned",
        "isNotSubjectToSection16",
        "titleOfficer",
        "descriptionOtherText",
        "dateReport",
        "countSharesOwnedPostTransaction",
        "descriptionRemarks",
        "idCIKOwner",
        "cityOwenr",
        "nameOwner",
        "stateOwner",
        "descriptionStateOwner",
        "addressStreet1Owner",
        "addressStreet2Owner",
        "zipcodeOwner",
        "idSchema",
        "titleSecurity",
        "countSharesOwnedPostTransaction",
        "dateSignature",
        "nameSignature",
        "codeTransactionAcquiredDisposed",
        "codeTransaction",
        "dateTransaction",
        "idFormTransaction",
        "pricePerShareTransaction",
        "countSharesTransaction",
        "idCodeTimelinessTransaction",
        "amountTransaction",
        "countSharesUnderlying",
        "titleSecurityUnderlying",
        "descriptionResponse", "isBusinessCombinationTransaction",
        "idCIK", "isMoreThanOneYear", "nameEntityPrevius", "listNameEntityPreviousEDGAR",
        "nameEntity", "typeEntity", "descriptionEntityTypeOther", "idFederalExemptionsExclusions",
        "typeIndustryGroup", "typeInvestmentFund", "descriptionInvestmentFund",
        "hasNonAccreditedInvestors", "countInvestorsNonAccredited",
        "countInvestorsActive", "cityEntity", "stateEntity", "descriptionStateEntity",
        "addressStreet1Entity", "addressStreet2Entity", "zipcodeEntity", "phoneNumberEntity", "listIssuerPreviousName",
        "jurisdictionOfInc", "isOverFiveYearsOld", "hasYearOfInc", "isFormedWithinFiveYears",
        "isYetToBeFormed", "rangeAgregateNetAssetValue", "rangeRevenue",
        "amountInvestmentMinimum", "amountSoldTotal", "amountOfferingTotal",
        "amountRemaining", "nameFirst", "nameLast", "nameMiddle", "relationshipEntity",
        "descriptionRelationship", "amountDollars", "isEstimate", "idCRDBroker",
        "nameBroker", "isForeignSolicitation", "idCRDRecipient",
        "nameRecipient", "stateDescription", "state", "listStatesSolicitation",
        "isAuthorizedRepresentative", "nameSignatory", "titleSignatory",
        "idForm", "codeTestOrLive", "dateFirstSale", "isYetToOccur",
        "isAmendment", "descriptionOtherType", "isDebtType", "isEquityType",
        "isMineralPropertyType", "isOptionToAcquireType", "isOtherType",
        "isPooledInvestmentFundType", "isSecurityToBeAcquiredType", "isTenantInCommonType",
        'isNotSubjectToSection16', 'addressStreet1Owner', 'addressStreet2Owner',
        "isLiveTestFlag", "isConfirmingCopyFlag", "isReturnCopyFlag", "isOverrideInternetFlag",
        "idCCC", "dateReportCalendarOrQuarter", "nameFilingManager", "addressStreet1FilingManager",
        "addressStreet2FilingManager", "cityFilingManager", "stateFilingManager",
        'descriptionStateFilingManager',
        "zipcodeFilingManager", "typeReport", "idSEC",
        "codeProvideInfoForInstruction5", "nameEntity", "titleEntity", "phoneEntity", "signatureEntity",
        "countOtherIncludedManagers", "countTableEntries", "amountValueHoldings",
        "isConfidentialOmitted", "nameIssuer", "classSecurities", "idCUSIP", "valueSecurities", "typeInvestmentDiscretion",
        "descriptionOtherManager", "codePutCall", "countSharesPrincipal", "codeSharesPrincipal", "countSharesVotingSole",
        "countSharesVotingShared", "countSharesVotingNone",

        "idSEC", "isSinceLastFiling", "codeJurisdictionOrganization",
        "yearIncorporation", "idSIC", "idIRS", "countEmployeesFullTime",
        "countEmployeesPartTime", "phoneEntity", "nameConnection", "nameIndustry",
        "amountCashEquivalents", "amountInvestmentSecurities", "amountAccountsReceivable",
        "amountPropertyPlantEquipment", "amountAssetsTotal", "amountAccountsPayable", "amountLongTermDebt",
        "amountLiabilitiesTotal", "amountStockholderEquityTotal", "amountLiabilitiesAndEquityTotal",
        "amountRevenuesTotal", "amountCostAndExpensesOfRevenue", "amountDepreciationAndAmortization",
        "amountNetIncome", "pershareEarningsBasic", "pershareEarningsDiluted",
        "nameAuditor", "nameCommonEquityClass", "amountCommonEquityOutstanding",
        "idCUSIPCommonEquity", "isCommonEquityPublic", "namePreferredEquityClass",
        "amountPreferredEquityOutstanding", "idCusipPreferrdEquity", "isdPreferredEquityPublic",
        "nameDebtSecuritiesClass", "amountOutstandingDebtSecurities", "idCUSIPDebtSecurities",
        "isDebtSecuritiesPublic", "isCertifyIfTrue", "isCertifyIfNotDisqualified",
        "codeTier1Tier2Offering", "codeFinancialStatementAuditStatus", "codeSecuritiesOfferedTypes",
        "codeOfferDelayedContinuous", "codeOfferingYearFlag", "codeOfferingAfterQualifFlag",
        "codeOfferingBestEffortsFlag", "codeSolicitationProposedOfferingFlag",
        "codeResaleSecuritiesAffiliates", "countSecuritiesOffered", "countSecuritiesOutstanding",
        "persharePrice", "amountOfferingIssuer", "amountOfferingExistingShareholdersSelling",
        "amountOfferingSold12MonthQualifiedOffering", "amountOfferingSoldConcurrent",
        "amountOfferingTotal", "nameUnderwritr",
        "amountUnderwritersFees", "nameAuditor", "amountAuditorFees",
        "nameLegal", "amountLegalFees", "namePromoter",
        "amountPromotersFees", "idCRDBroker", "amountOfferringProceedsNet",
        "descriptionResponse", "isJurisdictionsOfSecOfferedSame", "locatonJuridicationSecuritiesOffering",
        "locationDealersJuridicationSecuritiesOffering", "nameSecuritiesIssuer",
        "titleSecuritiesOffered", "amountSecuritiesIssued", "amountSecuritiesPrincipalHolder",
        "amountSecuritiesIssuedTotal", "nameSecuritiesActExemption",
        "isBadActor", "nameSalesCommissionsServiceProvider",
        "amountSalesCommissionsFees", "isJurisdictionsSecuritiesOfferingNone",
        "isUnRegisteredNone",
        "nameBlueSkyServiceProvider", "amountBlueSkyFees",
        'isTier1Tier2Offering', 'idForm', 'idForm', 'amountOfferingConsiderationBasis',
        'nameFindersFeeProvider' , 'amountFindersFee',
        'amountLoans', 'amountPropertyAndEquipment', 'amountDeposits', 'amountInterestIncomeTotal',
        'amountInterestExpenseTotal', 'descriptionOtherSecuritiesOffered',
        'commentFiling',
        "numberAssetType",
        "numberAsset",
        "numberAssetGroup",
        "dateReportPeriodBeginning",
        "dateReportPeriodEnd",
        "nameIssuer",
        "dateOriginalIssuance",
        "amountOriginalSecurity",
        "numberOriginalSecurityTerm",
        "dateSecurityMaturity",
        "numberOriginalAmortizationTerm",
        "percentageOriginalInterestRate",
        "codeAccrualType",
        "codeInterestRateType",
        "numberOriginalInterestOnlyTerm",
        "dateFirstPayment",
        "hasUnderwriting",
        "nameSecurityTitle",
        "numberDenomination",
        "nameCurrency",
        "nameTrustee",
        "numberSecFile",
        "idCIK",
        "hasCallable",
        "codePaymentFrequency",
        "hasZeroCoupon",
        "hasAssetAdded",
        "hasAssetModified",
        "amountReportPeriodBeginningAssetBalance",
        "amountReportPeriodBeginningScheduledAssetBalance",
        "amountReportPeriodScheduledPayment",
        "percentageReportPeriodInterestRate",
        "amountTotalActualPaid",
        "percentageActualInterestCollection",
        "amountActualPrincipalCollected",
        "amountActualOtherCollection",
        "amountOtherPrincipalAdjustment",
        "amountOtherInterestAdjustment",
        "amountScheduledInterest",
        "amountScheduledPrincipal",
        "amountEndReportingPeriodActualBalance",
        "amountEndReportingPeriodScheduledBalance",
        "percentageServicingFee",
        "amountServicingFlatFee",
        "codeZeroBalance",
        "dateZeroBalanceEffective",
        "numberRemainingTermToMaturity",
        "numberCurrentDelinquentStatus",
        "numberPaymentPastDueDays",
        "numberPaymentPastDue",
        "amountNextReportPeriodPaymentDue",
        "dateNextDue",
        "namePrimaryLoanServicer",
        "dateMostRecentServicingTransferReceived",
        "hasAssetSubjectToDemand",
        "codeStatusAssetSubjectToDemand",
        "amountRepurchase",
        "dateDemandResolution",
        "nameRepurchaser",
        "codeRepurchaseReplacementReason",
        "dateReportPeriodBegin",
        "codeOriginalLoanPurpose",
        "nameOriginator",
        "amountOriginalLoan",
        "dateOriginalLoanMaturity",
        "codeOriginalInterestRateType",
        "codeOriginalLienPosition",
        "amountMostRecentJuniorLoanBalance",
        "dateMostRecentJuniorLoanBalance",
        "amountMostRecentSeniorLoan",
        "dateMostRecentSeniorLoanAmount",
        "codeLoanTypeMostSeniorLien",
        "numberMostSeniorLienHybridPeriod",
        "percentageMostSeniorLienNegativeAmortizationLimit",
        "dateMostSeniorLienOrigination",
        "hasPrepaymentPenalty",
        "hasNegativeAmortization",
        "hasModification",
        "numberModification",
        "hasMortgageInsuranceRequirement",
        "hasBalloon",
        "codeCoveredHighCost",
        "codeServicerHazardInsurance",
        "amountRefinanceCashOut",
        "amountTotalOriginationDiscount",
        "hasBroker",
        "codeChannel",
        "numberNationalMortgageLicenseSystemCompany",
        "numberBuyDown",
        "numberLoanDelinquencyAdvance",
        "codeOriginationARMIndex",
        "percentageArmMargin",
        "percentageFullyIndexedRate",
        "numberInitialFixedRatePeriodHybridARM",
        "percentageInitialInterestRateDecrease",
        "percentageInitialInterestRateIncrease",
        "numberIndexLookback",
        "numberSubsequentInterestRateReset",
        "percentageLifetimeRateCeiling",
        "percentageLifetimeRateFloor",
        "percentageSubsequentInterestRateDecrease",
        "percentageSubsequentInterestRateIncrease",
        "numberSubsequentPaymentReset",
        "codeArmRound",
        "percentageArmRound",
        "hasOptionArm",
        "codePaymentMethodAfterRecast",
        "amountInitialMinimumPayment",
        "hasConvertible",
        "hasHELOC",
        "numberHELOCDraw",
        "codePrepaymentPenaltyCalculation",
        "codePrepaymentPenaltyType",
        "numberPrepaymentPenaltyTotalTerm",
        "numberPrepaymentPenaltyHardTerm",
        "amountNegativeAmortizationLimit",
        "numberNegativeAmortizationInitialRecast",
        "numberNegativeAmortizationSubsequentRecast",
        "amountNegativeAmortizationBalance",
        "numberInitialFixedPayment",
        "percentageInitialPaymentCap",
        "percentageSubsequentPaymentCap",
        "numberInitialMinimumPaymentReset",
        "numberSubsequentMinimumPaymentReset",
        "amountMinimumPayment",
        "locationGeographical",
        "codeOccupancyStatus",
        "codeMostRecentOccupancyStatus",
        "codePropertyType",
        "amountMostRecentPropertyValue",
        "codeMostRecentPropertyValueType",
        "dateMostRecentPropertyValue",
        "codeMostRecentAVMModel",
        "numberMostRecentAVMConfidence",
        "percentageOriginalCLTV",
        "percentageOriginalLTV",
        "numberOriginalObligor",
        "numberOriginalObligorCreditScore",
        "typeOriginalObligorCreditScore",
        "numberMostRecentObligorCreditScore",
        "typeMostRecentObligorCreditScore",
        "dateMostRecentObligorCreditScore",
        "codeObligorIncomeVerificationLevel",
        "hasIRSForm4506T",
        "percentageOriginatorFrontEndDTI",
        "percentageOriginatorBackEndDTI",
        "codeObligorEmploymentVerification",
        "codeObligorEmploymentLength",
        "codeObligorAssetVerification",
        "amountOriginalPledgedAssets",
        "codeQualificationMethod",
        "nameMortgageInsuranceCompany",
        "percentageMortgageInsuranceCoverage",
        "namePoolInsuranceCompany",
        "percentagePoolInsuranceStopLoss",
        "codeMortgageInsuranceCoverageType",
        "periodModificationHasReporting",
        "dateNextPaymentDue",
        "codeAdvancingMethod",
        "codeServicingAdvanceMethodology",
        "dateStopPrincipalInterestAdvancing",
        "amountReportingPeriodBeginningLoanBalance",
        "amountReportingPeriodBeginningScheduledLoanBalance",
        "amountNextReportingPeriodPaymentDue",
        "percentageReportingPeriodInterestRate",
        "percentageNextInterestRate",
        "feeamountOtherAssessedUncollectedServicer",
        "amountOtherServicingFeeRetainedByServicer",
        "amountReportingPeriodEndActualBalance",
        "amountReportingPeriodEndScheduledBalance",
        "amountReportingPeriodScheduledPayment",
        "amountActualInterestCollected",
        "amountActualOtherCollected",
        "datePaidThrough",
        "dateInterestPaidThrough",
        "amountPaidFull",
        "amountServicerAdvancedPrincipal",
        "amountServicerAdvancedRepaidPrincipal",
        "amountServicerAdvancedCumulativePrincipal",
        "amountServicerAdvanceInterest",
        "amountServicerAdvanceRepaidInterest",
        "amountServicerAdvanceCumulativeInterest",
        "amountServicerAdvanceTaxesInsurance",
        "amountServicerAdvanceRepaidTaxesInsurance",
        "amountServicerAdvanceCumulativeTaxesInsurance",
        "amountServicerAdvanceCorporate",
        "amountServicerAdvanceRepaidCorporate",
        "amountServicerAdvanceCumulativeCorporate",
        "codeMostRecentTwelveMonthHistory",
        "percentageNextResetRate",
        "dateNextPaymentChange",
        "dateNextInterestRateChange",
        "amountNextResetPayment",
        "hasExercisedArmConversionOption",
        "namePrimaryServicer",
        "nameMasterServicer",
        "nameSpecialServicer",
        "nameSubServicer",
        "hasAssetSubjectDemand",
        "codeAssetSubjectDemandStatus",
        "codeRepurchaseReplacement",
        "amountChargeOffPrincipal",
        "amountChargeOffInterest",
        "codeLossMitigationType",
        "codeMostRecentLoanModificationEvent",
        "dateMostRecentLoanModificationEffective",
        "datePostModificationMaturity",
        "codePostModificationInterestRateType",
        "codePostModificationAmortizationType",
        "percentagePostModificationInterest",
        "datePostModificationFirstPayment",
        "amountPostModificationLoanBalance",
        "amountPostModificationPrincipalInterestPayment",
        "amountTotalCap",
        "modificationIncomeVerificationHasAt",
        "percentageModificationFrontEndDebtToIncome",
        "percentageModificationBackEndDebtToIncome",
        "amountTotalDeferred",
        "amountForgivenPrincipalCumulative",
        "amountForgivenPrincipalReportingPeriod",
        "amountForgivenInterestCumulative",
        "amountForgivenInterestReportingPeriod",
        "amountActualEndingBalanceTotalDebt",
        "amountScheduledEndingBalanceTotalDebt",
        "codePostModificationARM",
        "codePostModificationARMIndex",
        "percentagePostModificationMargin",
        "numberPostModificationInterestReset",
        "datePostModificationNextReset",
        "numberPostModificationIndexLookback",
        "codePostModificationARMRounding",
        "percentagePostModificationARMRounding",
        "paymentPostModificationInitialMinimum",
        "datePostModificationNextPaymentAdjustment",
        "frequencyPostModificationARMPaymentRecast",
        "percentagePostModificationLifetimeFloor",
        "percentagePostModificationLifetimeCeiling",
        "percentagePostModificationInitialInterestRateIncrease",
        "percentagePostModificationInitialInterestRateDecrease",
        "percentagePostModificationSubsequentInterestIncrease",
        "percentagePostModificationSubsequentInterestRateDecrease",
        "percentagePostModificationPaymentCap",
        "codePostModificationPaymentMethodAfterRecast",
        "numberPostModificationARMInterestRateTeaser",
        "numberPostModificationARMPaymentTeaser",
        "hasPostModificationARMNegativeAmortization",
        "percentagePostModificationARMNegativeAmortizationCap",
        "numberPostModificationInterestOnlyTerm",
        "datePostModificationInterestOnlyLastPayment",
        "amountPostModificationBalloon",
        "hasPostModificationInterestRateStep",
        "percentagePostModificationStepInterest",
        "datePostModificationStep",
        "amountPostModificationStepPrincipalInterestPayment",
        "numberPostModificationStep",
        "percentagePostModificationMaximumFutureStepAgreement",
        "datePostModificationMaximumStepAgreementRate",
        "amountNonInterestBearingDeferredPrincipalCumulative",
        "amountNonInterestBearingDeferredPrincipalReportingPeriod",
        "amountRecoveryDeferredPrincipalReportingPeriod",
        "amountNonInterestBearingDeferredPaidFull",
        "amountNonInterestBearingDeferredInterestFeeReportingPeriod",
        "amountNonInterestBearingDeferredInterestFeeCumulative",
        "amountRecoveryDeferredInterestFeeReportingPeriod",
        "dateMostRecentForbearancePlanOrTrialModificationStart",
        "dateMostRecentForbearancePlanOrTrialModificationScheduledEnd",
        "dateMostRecentTrialModificationViolated",
        "dateMostRecentRepaymentPlanStart",
        "dateMostRecentRepaymentPlanScheduledEnd",
        "dateMostRecentRepaymentPlanViolated",
        "amountShortSaleAcceptedOffer",
        "dateMostRecentLossMitigationExit",
        "codeMostRecentLossMitigationExit",
        "dateAttorneyReferral",
        "codeForeclosureDelayReason",
        "dateForeclosureExit",
        "codeForeclosureExitReason",
        "dateNoticeOfIntent",
        "amountMostRecentAcceptedREOOffer",
        "dateMostRecentAcceptedREOOffer",
        "amountGrossLiquidationProceeds",
        "amountNetSalesProceeds",
        "amountReportingPeriodLossPassedToIssuingEntity",
        "amountCumulativeTotalLossPassedToIssuingEntity",
        "amountSubsequentRecovery",
        "hasEviction",
        "dateReoExit",
        "codeReoExitReason",
        "amountUPBLiquidation",
        "amountServicingFeesClaimed",
        "amountServicerAdvanceReimbursedPrincipal",
        "amountServicerAdvanceReimbursedInterest",
        "amountServicerAdvanceReimbursedTaxesInsurance",
        "amountServicerAdvanceReimbursedCorporate",
        "amountREOManagementFees",
        "amountCashKeyDeed",
        "amountPerformanceIncentiveFees",
        "dateMortgageInsuranceClaimFiled",
        "amountMortgageInsuranceClaim",
        "dateMortgageInsuranceClaimPaid",
        "amountMortgageInsuranceClaimPaid",
        "dateMortgageInsuranceClaimDeniedRescinded",
        "dateMarketableTitleTransfer",
        "codeNonPayStatus",
        "codeReportingAction",
        "idGroup",
        "dateReportingPeriodBeginning",
        "dateReportingPeriodEnd",
        "dateOrigination",
        "numberOriginalTermLoan",
        "dateMaturity",
        "percentageInterestRateSecuritization",
        "codeInterestAccrualMethod",
        "dateFirstLoanPaymentDue",
        "codeLienPositionSecuritization",
        "codeLoanStructure",
        "codePaymentType",
        "amountPeriodicPrincipalAndInterestPaymentSecuritization",
        "amountScheduledPrincipalBalanceSecuritization",
        "securitizationNumberProperties",
        "propertiesNumber",
        "numberGraceDaysAllowed",
        "hasInterestOnly",
        "hasPrepaymentPremium",
        "hasModified",
        "codeArmIndex",
        "dateFirstRateAdjustment",
        "dateFirstPaymentAdjustment",
        "numberArmMargin",
        "percentageLifetimeRateCap",
        "percentagePeriodicRateIncreaseLimit",
        "percentagePeriodicRateDecreaseLimit",
        "amountPeriodicPaymentAdjustmentMaximum",
        "percentPeriodicPaymentAdjustmentMaximum",
        "codeRateResetFrequency",
        "codePaymentResetFrequency",
        "numberIndexLookbackDays",
        "datePrepaymentLockOutEnd",
        "dateYieldMaintenanceEnd",
        "datePrepaymentPremiumsEnd",
        "percentageMaximumNegativeAmortizationAllowed",
        "amountMaximumNegativeAmortizationAllowed",
        "amountNegativeAmortizationDeferredInterestCap",
        "amountDeferredInterestCumulative",
        "amountDeferredInterestCollected",
        "propertyProperty",
        "hasReportPeriodModification",
        "amountReportPeriodBeginningScheduleLoanBalance",
        "amountTotalScheduledPrincipalInterestDue",
        "percentageServicerTrusteeFeeRate",
        "amountUnscheduledPrincipalCollected",
        "amountReportPeriodEndActualBalance",
        "amountReportPeriodEndScheduledLoanBalance",
        "dateHyperAmortizing",
        "codeServicingAdvanceMethod",
        "hasNonRecoverability",
        "amountTotalPrincipalInterestAdvancedOutstanding",
        "amountTotalTaxesInsuranceAdvancesOutstanding",
        "amountOtherExpensesAdvancedOutstanding",
        "codePaymentStatusLoan",
        "percentageArmIndexRate",
        "dateNextInterestRateChangeAdjustment",
        "dateNextPaymentAdjustment",
        "dateMostRecentSpecialServicerTransfer",
        "dateMostRecentMasterServicerReturn",
        "amountRealizedLossToTrust",
        "codeLiquidationPrepayment",
        "dateLiquidationPrepayment",
        "amountPrepaymentPremiumYieldMaintenanceReceived",
        "codeWorkoutStrategy",
        "dateLastModification",
        "codeModification",
        "amountPostModificationPayment",
        "amountPostModificationAmortizationPeriod",
        "nameProperty",
        "addressProperty",
        "cityProperty",
        "stateProperty",
        "zipcodeProperty",
        "countyProperty",
        "numberNetRentableSquareFeet",
        "numberNetRentableSquareFeetSecuritization",
        "numberUnitsBedsRooms",
        "numberUnitsBedsRoomsSecuritization",
        "yearBuilt",
        "yearLastRenovated",
        "amountValuationSecuritization",
        "codeValuationSourceSecuritization",
        "dateValuationSecuritization",
        "amountMostRecentValuation",
        "dateMostRecentValuation",
        "codeMostRecentValuationSource",
        "percentagePhysicalOccupancySecuritization",
        "percentageMostRecentPhysicalOccupancy",
        "codePropertyStatus",
        "dateDefeasanceOptionStart",
        "codeDefeasedStatus",
        "tenantLargest",
        "numberSquareFeetLargestTenant",
        "dateLeaseExpirationLargestTenant",
        "tenantSecondLargest",
        "numberSquareFeetSecondLargestTenant",
        "dateLeaseExpirationSecondLargestTenant",
        "tenantThirdLargest",
        "numberSquareFeetThirdLargestTenant",
        "dateLeaseExpirationThirdLargestTenant",
        "dateFinancialsSecuritization",
        "dateMostRecentFinancialsStart",
        "dateMostRecentFinancialsEnd",
        "amountRevenueSecuritization",
        "amountMostRecentRevenue",
        "amountOperatingExpensesSecuritization",
        "amountOperatingExpenses",
        "amountNetOperatingIncomeSecuritization",
        "amountMostRecentNetOperatingIncome",
        "amountNetCashFlowFlowSecuritization",
        "amountMostRecentNetCashFlow",
        "codeNetOperatingIncomeNetCashFlowSecuritization",
        "codeNetOperatingIncomeNetCashFlow",
        "amountMostRecentDebtService",
        "percentageDebtServiceCoverageNetOperatingIncomeSecuritization",
        "percentageMostRecentDebtServiceCoverageNetOperatingIncome",
        "percentageDebtServiceCoverageNetCashFlowSecuritization",
        "percentageMostRecentDebtServiceCoverageNetCash",
        "codeDebtServiceCoverageSecuritization",
        "codeMostRecentDebtServiceCoverage",
        "dateMostRecentAnnualLeaseRolloverReview",
        "dateReportingPeriodEnding",
        "termOriginalLoan",
        "dateLoanMaturity",
        "codeInterestCalculationType",
        "dateOriginalFirstPayment",
        "numberGracePeriod",
        "subventedSubvented",
        "nameVehicleManufacturer",
        "nameVehicleModel",
        "codeVehicleNewUsed",
        "yearVehicleModel",
        "codeVehicleType",
        "amountVehicleValue",
        "codeVehicleValueSource",
        "typeObligorCreditScore",
        "scoreObligorCredit",
        "hasCoObligor",
        "percentagePaymentToIncome",
        "locationObligorGeographic",
        "hasReportingPeriodModification",
        "amountPaymentDueNextReportingPeriod",
        "servicerOtherServicerFeeRetainedBy",
        "amountOtherAssessedUncollectedServicerFee",
        "amountReportingPeriodActualEndBalance",
        "amountPaidTotalActual",
        "amountServicerAdvanced",
        "isDelinquent",
        "amountChargedoffPrincipal",
        "amountRecovered",
        "codeModificationType",
        "numberPaymentExtended",
        "hasRepossessed",
        "amountRepossessedProceeds",
        "dateReportingPeriodBegin",
        "costAcquisition",
        "numberOriginalLeaseTerm",
        "dateScheduledTermination",
        "periodGrace",
        "valueBaseResidual",
        "codeBaseResidualSource",
        "valueContractResidual",
        "typeLesseeCreditScore",
        "scoreLesseeCredit",
        "codeLesseeIncomeVerificationLevel",
        "codeLesseeEmploymentVerification",
        "hasCoLesseePresent",
        "locationLesseeGeographic",
        "numberRemainingTerm",
        "amountReportingPeriodSecuritizationValue",
        "rateSecuritizationDiscount",
        "amountOtherLeaseLevelServicingFeesRetained",
        "amountReportingPeriodEndingActualBalance",
        "amountReportingPeriodEndActualSecuritization",
        "namePrimaryLeaseServicer",
        "dateDemandResolution",
        "codeRepurchaseOrReplacementReason",
        "amountChargedOff",
        "extendedLease",
        "hasTermination",
        "amountExcessFee",
        "amountLiquidationProceeds",
        "detailNumberComment", "columnComment", "descriptionComment",
        'idAccessionPrevious',
        'numberItem', 'nameField', 'descriptionNotes'
      )
    )}

.filer_type_df <-
  function() {
    tibble(
      idTypeFilerOwner = c(
        'insider',
        'private' ,
        'broker_dealer',
        'transfer_agent',
        'ia',
        'msd',
        'bank',
        'inv_co'
      ),
      typeFilerOwner = c(
        'Insider',
        'Private Placement',
        'Broker Dealer',
        'Transfer Agent',
        'Investment Advisor',
        'Bank',
        'Municipal Securities Dealer',
        'Investment Company'
      )
    ) %>%
      mutate_all(str_to_upper)
  }

#' Form-D dictionary
#'
#' This function returns searchable
#' industries for parsed SEC Form-D
#' filings
#'
#' @return a \code{tibble}
#' @export
#' @import dplyr
#' @examples
#' dictionary_form_d_categories()
dictionary_form_d_categories <-
  function() {
    category_df <-
      dplyr::tibble(
        idIndustry = 1:35,
        nameIndustry = c(
          "AGRICULTURE",
          "AIRLINES AND AIRPORTS",
          "BIOTECHNOLOGY",
          "BUSINESS SERVICES",
          "COAL MINING",
          "COMMERCIAL REAL ESTATE",
          "COMMERCIAL BANKING",
          "COMPUTERS",
          "CONSTRUCTION",
          "ELECTRIC UTILITIES",
          "ENERGY CONSERVATION",
          "ENVIORNMENTAL SERVICES",
          "HEALTH INSURANCE",
          "HOSPITALS AND PHYSICIANS",
          "INSURANCE",
          "INVESTING",
          "INVESTMENT BANKING",
          "LODGING AND CONVETION",
          "MANUFACTURING",
          "OIL AND GAS",
          "OTHER",
          "OTHER BANKING AND FINANCIAL SERVICES",
          "OTHER ENERGY",
          "OTHER HEALTH CARE",
          "OTHER REAL ESTATE",
          "OTHER TECHNOLOGY",
          "OTHER TRAVEL",
          "PHARMACEUTICALS",
          "POOLED INVESTMENT FUND",
          "REITS AND FINANCE",
          "RESIDENTIAL REAL ESTATE",
          "RESTAURANTS",
          "RETAIL",
          "TELECOMMUNICATIONS",
          "TRAVEL AND TOURISM"
        ),
        codeIndustryParent = c(
          "OTHER",
          "TRAVEL",
          "HEALTH",
          "OTHER",
          "ENERGY",
          "REAL",
          "FINANCE",
          "TECH",
          "REAL",
          "ENERGY",
          "ENERGY",
          "ENERGY",
          "HEALTH",
          "HEALTH",
          "FINANCE",
          "FINANCE",
          "FINANCE",
          "TRAVEL",
          "OTHER",
          "ENERGY",
          "OTHER",
          "FINANCE",
          "ENERGY",
          "HEALTH",
          "REAL",
          "TECH",
          "TRAVEL",
          "HEALTH",
          "FINANCE",
          "REAL",
          "REAL",
          "OTHER",
          "OTHER",
          "TECH",
          "TRAVEL"
        ),
        nameIndustryParent = c(
          "OTHER",
          "TRAVEL AND LEISURE",
          "HEALTHCARE",
          "OTHER",
          "ENERGY",
          "REAL ESTATE",
          "FINANCIAL",
          "TECHNOLOGY",
          "REAL ESTATE",
          "ENERGY",
          "ENERGY",
          "ENERGY",
          "HEALTHCARE",
          "HEALTHCARE",
          "FINANCIAL",
          "FINANCIAL",
          "FINANCIAL",
          "TRAVEL AND LEISURE",
          "OTHER",
          "ENERGY",
          "OTHER",
          "FINANCIAL",
          "ENERGY",
          "HEALTHCARE",
          "REAL ESTATE",
          "TECHNOLOGY",
          "TRAVEL AND LEISURE",
          "HEALTHCARE",
          "FINANCIAL",
          "REAL ESTATE",
          "REAL ESTATE",
          "OTHER",
          "OTHER",
          "TECHNOLOGY",
          "TRAVEL AND LEISURE"
        )
      )
    return(category_df)
  }

.insider_code_df <-
  function() {
    insider_df <-
      tibble(
        idInsiderTransaction =
          c(
            "A",
            "C",
            "D",
            "F",
            "G",
            "H",
            "I",
            "J",
            "K",
            "L",
            "M",
            "NONE",
            "O",
            "P",
            "S",
            "U",
            "V",
            "W",
            "X",
            "Z"
          ),
        nameInsiderTransaction = c(
          "AWARD",
          "CONVEYANCE",
          "DISPOSITION TO ISSUER",
          "PAYMENT WITH SECURITIES",
          "GIFT",
          "EXPIRATION OF LONG DERIVATIVE POSITION",
          "DISCRETIONARY TRANSACTION",
          "OTHER",
          "EQUITY SWAP OR SIMILAR",
          "SMALL ACQUISITIONS",
          "EXEMPT",
          NA,
          "OTM EXERCISE",
          "PURCHASE",
          "SALE",
          "MERGER AND ACQUISITION",
          "REPORTED EARLY",
          "WILL OR LAWS OF DESCENT",
          "ITM OR ATM EXERCISE",
          "DEPOSIT INTO/WITHDRAWAL FROM VOTING TRUST"
        ),
        idTypeInsiderTransaction = c(
          "A",
          "D",
          "D",
          "D",
          "D",
          NA,
          NA,
          NA,
          NA,
          "A",
          "A",
          NA,
          "A",
          "A",
          "D",
          NA,
          NA,
          "D",
          "A",
          "D"
        )
      )
    return(insider_df)
  }

#' SEC filing code dictionary
#'
#' This function returns a
#' dictionary of SEC form filing types
#'
#' @return a \code{tibble}
#' @export
#' @import dplyr stringr
#' @family SEC
#' @family dictionary
#'
#' @examples
#' dictionary_sec_filing_codes()
dictionary_sec_filing_codes <-
  function() {
    tibble(
      idFormType = c(
        "1.01",
        "1.02",
        "1.03",
        "1.04",
        "2.01",
        "2.02",
        "2.03",
        "2.04",
        "2.05",
        "2.06",
        "3.01",
        "3.02",
        "3.03",
        "4.01",
        "4.02",
        "5.01",
        "5.02",
        "5.03",
        "5.04",
        "5.05",
        "5.06",
        "5.07",
        "5.08",
        "6.01",
        "6.02",
        "6.03",
        "6.04",
        "6.05",
        "7.01",
        "8.01",
        "9.01"
      ),
      nameFormType = c(
        "Entry into a Material Definitive Agreement",
        "Termination of a Material Definitive Agreement",
        "Bankruptcy or Receivership",
        "Mine Safety Reporting of Shutdowns and Patterns of Violations",
        "Completion of Acquisition or Disposition of Assets",
        "Results of Operations and Financial Condition",
        "Creation of a Direct Financial Obligation or an Obligation under an Off-Balance Sheet Arrangement of a Registrant",
        "Triggering Events That Accelerate or Increase a Direct Financial Obligation or an Obligation under an Off-Balance Sheet Arrangement",
        "Costs Associated with Exit or Disposal Activities",
        "Material Impairments",
        "Notice of Delisting or Failure to Satisfy a Continued Listing Rule or Standard; Transfer of Listing",
        "Unregistered Sales of Equity Securities",
        "Material Modification to Rights of Security Holders",
        "Changes in Registrant's Certifying Accountant",
        "Non-Reliance on Previously Issued Financial Statements or a Related Audit Report or Completed Interim Review",
        "Changes in Control of Registrant",
        "Departure of Directors or Certain Officers; Election of Directors; Appointment of Certain Officers; Compensatory Arrangements of Certain Officers",
        "Amendments to Articles of Incorporation or Bylaws; Change in Fiscal Year",
        "Temporary Suspension of Trading Under Registrant's Employee Benefit Plans",
        "Amendments to the Registrant's Code of Ethics, or Waiver of a Provision of the Code of Ethics",
        "Change in Shell Company Status",
        "Submission of Matters to a Vote of Security Holders",
        "Shareholder Director Nominations",
        "ABS Informational and Computational Material",
        "Change of Servicer or Trustee",
        "Change in Credit Enhancement or Other External Support",
        "Failure to Make a Required Distribution",
        "Securities Act Updating Disclosure",
        "Regulation FD Disclosure",
        "Other Events",
        "Financial Statements and Exhibits"
      ) %>% stringr::str_to_upper()
    )

  }

#' SEC form codes
#'
#' This function returns a
#' dictionary of SEC form codes
#'
#' @return a \code{tibble}
#' @export
#' @family SEC
#' @family dictionary
#'
#' @examples
#' dictionary_sec_form_codes()
dictionary_sec_form_codes <-
  function() {
    tibble(
      idForm = c(
        "R",
        "A",
        "Q",
        "CR",
        "REG",
        "REGX",
        "O",
        "P",
        "X",
        "W",
        "SEC",
        "PROXY",
        "CT",
        "IS",
        "CO",
        "T"
      ),
      nameForm = c(
        "Other Report",
        "Annual Report",
        "Quarterly Report",
        "Current Report",
        "Registration",
        "Private Offering",
        "Ownership",
        "Prospectus",
        "Exemption",
        "Withdrawal",
        "SEC Correspondence",
        "Proxy Statement",
        "Confidential Treatment",
        "Initial Statement",
        "Change in Ownership",
        "Trades"
      ) %>% stringr::str_to_upper()
    )
  }

.company_type_df <-
  function() {
    tibble(
      idCompanyType = c(
        "ic",
        "i",
        "ia",
        "bd",
        "m",
        "t",
        "b",
        "c",
        "p",
        "etf",
        "mmf",
        "mf",
        "uit",
        "cef"
      ),
      nameCompanyType = c(
        "Investment Company",
        "Insider",
        "Investment Adviser",
        "Broker-dealer",
        "Municipal Securities Dealer",
        "Transfer Agent",
        "Bank",
        "Company",
        "Private Issuer",
        "ETF",
        "Money Market Fund",
        "Mutual Fund",
        "UIT",
        "Closed-end Fund"
      )
    )
  }

#' SEC Rule dictionary
#'
#' This function retuns a
#' dictionary of SEC rules
#'
#' @return a \code{tibble}
#' @export
#' @import dplyr stringr
#'
#' @examples
#' dictionary_sec_rules()
dictionary_sec_rules <-
  function() {
    tibble(
      idRule = c(
        "06",
        "3C",
        "3C.7",
        "3C.1",
        "06b",
        "04",
        "46",
        "04.1",
        "04.2",
        "04.3",
        "05",
        "3C.6",
        "3C.5",
        "06c",
        "4a5",
        "3C.11",
        "3C.2",
        "3C.3",
        "3C.9",
        "3C.10",
        "3C.4",
        "3C.12",
        "3C.",
        "3C.14",
        "3"
      ),
      nameRule = c(
        "Rule 506",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Rule 506b",
        "Rule 504",
        "Rule 506c",
        "Rule 504b(1)(i)",
        "Rule 504b(1)(ii)",
        "Rule 504b(1)(iii)",
        "Rule 505",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Rule 506c",
        "Securities Act Section 4(a)(5)",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c"
      )
    ) %>%
      mutate_all(str_to_upper)
  }



# form_parsing ------------------------------------------------------------


.parse_full_form_names <-
  function(sec_names) {
    df_names <-
      seq_along(sec_names) %>%
      future_map_dfr(function(x) {
        sec_name <-
          sec_names[[x]]

        name_pieces <-
          sec_name %>% str_replace_all('\\.value|\\.item', '')

        pieces <-
          name_pieces %>%
          str_split('\\.') %>%
          flatten_chr()

        pieces_no_num <-
          pieces[!pieces %>% str_detect("[0-9]")]
        peice_length <-
          pieces_no_num %>% length()

        is_street <-
          pieces %>% str_detect("street1|street2|Street1|Street2") %>% sum(na.rm = T) > 0

        name_item <-
          pieces_no_num[length(pieces_no_num)]

        if (sec_name %>% str_detect('filingManager')) {
          name_item <-
            pieces %>% paste0(collapse = '')

          df <-
            tibble(nameSECFull = sec_name,
                   nameSEC = name_item)
          return(df)
        }

        if (is_street) {
          name_item <-
            pieces[pieces %>% str_detect("street1|street2|Street1|Street2")]
        }

        is_sig <-
          name_pieces %>% str_detect('signature') & peice_length == 1

        is_footnote <-
          sec_name %>% str_detect('footnote')

        is_issuer <-
          sec_name %>% str_detect('\\issuer.[A-Z]')

        is_federal <-
          sec_name %>% str_detect(pattern = "federalExemptionsExclusions")

        if (is_federal) {
          df <-
            tibble(
              nameSECFull = sec_name,
              nameTable = pieces[[1]],
              nameSEC = name_item
            )

          return(df)
        }

        if (is_issuer) {

          items <-
            sec_name %>% str_split('\\.') %>% flatten_chr()

          countItem <-
            pieces[2] %>% as.character() %>%  readr::parse_number() %>% suppressWarnings()

          name_item <-
            items[length(items)]

          df <-
            tibble(
              nameSECFull = sec_name,
              nameTable = 'issuer',
              countItem,
              nameSEC = name_item
            )
          return(df)
        }

        if (is_footnote) {
          if (pieces %>% length() == 1) {
            countItem <-
              0
            item <-
              pieces[[1]]
          } else {
            item <-
              pieces[[1]]
            countItem <-
              pieces[2] %>% as.character() %>% readr::parse_number() %>% suppressWarnings()
          }
          return(tibble(nameTable = 'footnotes', nameSECFull = sec_name, nameSEC = item, countItem))
        }

        if (is_sig) {
          df <-
            tibble(nameTable = 'signatures', nameSECFull = sec_name, nameSEC = name_item)
          return(df)
        }

        if (peice_length == 1) {
          df <-
            tibble(nameSECFull = sec_name, nameSEC = name_item)
          return(df)
        }

        piece_count <-
          length(pieces)

        if (piece_count == 1) {
          df <-
            tibble(nameSECFull = sec_name, nameSEC = sec_name)
          return(df)
        }

        if (piece_count == 2 &!is_footnote) {


          df <-
            tibble(nameSECFull = sec_name,
                   nameTable = pieces[[1]] ,
                   nameSEC = name_item)

          return(df)
        }

        if (piece_count > 2) {
          countItem <-
            pieces[2] %>%
            as.character() %>%
            readr::parse_number() %>% suppressWarnings()

          df <-
            tibble(
              nameSECFull = sec_name,
              nameTable = pieces[[1]] ,
              countItem,
              nameSEC = name_item
            )

          return(df)
        }

      }) %>%
      filter(!nameSEC == '')

    df_dictionary <-
      .sec_form_title_df()

    has_missing_names <-
      df_names$nameSEC[!df_names$nameSEC %in% df_dictionary$nameSEC] %>%
      length() > 0
    if (has_missing_names) {
      missing <-
        df_names$nameSEC[!df_names$nameSEC %in% df_dictionary$nameSEC] %>%
        unique()

      missing_names <-
        missing %>%
        paste0(collapse = '\n')
      stop(list("Missing:\n", missing_names) %>%
             purrr::reduce(paste0))
    }

    df_names <-
      df_names %>%
      left_join(df_dictionary) %>%
      suppressWarnings() %>%
      suppressMessages()

    if (!'nameTable' %in% names(df_names)) {
      df_names <-
        df_names %>%
        mutate(nameTable = 'asset')
    }

    df_names <-
      df_names %>%
      select(nameTable, nameSECFull, nameSEC, nameActual, everything()) %>%
      mutate(nameTable = nameTable %>% str_replace('Id',''),
             nameTable = ifelse(nameTable %in% c('issuerCredentials','securitiesIssued'), NA, nameTable)) %>%
      suppressWarnings() %>%
      suppressMessages()
  }

.parse_xml_tables <-
  function(url = "https://www.sec.gov/Archives/edgar/data/61004/000114036117000046/doc1.xml"){
    page <-
      url %>%
      xml2::read_xml()

    tables <-
      page %>%
      xml_contents() %>%
      xml_name() %>%
      unique()

    data <-
      seq_along(tables) %>%
      future_map_dfr(function(x){
        table <-
          tables[[x]]

        if (table %in% c('headerData', 'formData')) {
          form_tables <-
            page %>% xml_contents() %>% xml_name()

          table_loc <-
            table %>% grep(form_tables)
          xml_nodes <-
            page %>%
            xml_contents() %>% .[[table_loc]]
        }

        if (table %in% c('infoTable' , 'assets')) {
          xml_nodes <-
            page %>%
            xml_contents()
        }

        if (table == 'comment') {
          value <-
            page %>% xml_contents() %>% xml_text()

          df <-
            tibble(idTable = x, nameSECFull = table, value)
          return(df)

        }

        tables_special <-
          c('headerData', 'formData', 'infoTable', 'assets')

        if (!table %in% tables_special) {

          value_search <-
            list('//', table) %>% purrr::reduce(paste0)

          xml_nodes <-
            page %>%
            xml_contents() %>%
            xml_find_all(value_search)
        }
        if (xml_nodes %>% length() > 100) {
          list("Be patient there are ", xml_nodes %>% length() %>% formattable::comma(digits = 0), ' nodes to parse') %>%
            purrr::reduce(paste0) %>% cat(fill = T)
        }
        value_list <-
          xml_nodes %>%
          as_list()

        value_list <-
          value_list[value_list %>% future_map(length) %>% flatten_dbl() > 0]

        json_data <-
          value_list %>%
          jsonlite::toJSON(force = FALSE, dataframe = 'values') %>%
          jsonlite::fromJSON(simplifyDataFrame = TRUE, flatten = TRUE)

        wrong_output <-
          json_data %>% class() == 'array'

        if (wrong_output) {
          item <-
            xml_nodes %>% xml_name()
          value <-
            xml_nodes %>% xml_text()
          json_data <-
            tibble(item, value) %>%
            spread(item, value)
        }

        if (json_data %>% length() == 0) {
          return(tibble())
        }
        if ('summaryInfo' %in% names(json_data)) {
          json_data <-
            seq_along(json_data) %>% map(
              function(x){
                js_d <- json_data[x]
                if ('summaryInfo' %in% names(js_d)) {
                  if (js_d$summaryInfo$clarificationResponses %>% length() == 0) {
                    js_d$summaryInfo$clarificationResponses <-
                      NULL
                  }
                }
                return(js_d)
              }) %>%
            flatten()

          json_data <-
            json_data[json_data %>% future_map(function(x){data.frame(x, stringsAsFactors = F)} %>% nrow()) > 0]
        }

        json_data <-
          json_data %>%
          data.frame(stringsAsFactors = FALSE) %>%
          as_tibble() %>%
          mutate_all(as.character) %>%
          mutate(idTable = x) %>%
          gather(nameSECFull, value, -idTable) %>%
          arrange(idTable)
        return(json_data)
      })

    data <-
      data %>%
      mutate(isList = value %>% str_detect('list')) %>%
      filter(!isList) %>%
      select(-isList) %>%
      mutate(
        nameSECFull = nameSECFull %>% str_replace_all(
          "filerInfo.flags.|filerInfo.filer.|coverPage.|.filer.|\\flags.|filer.credentials.",
          ''
        ),
        nameSECFull = nameSECFull %>% str_replace_all('filerInfo.|issuerCredentials.', '')
      )


    rm(tables)
    rm(page)
    rm(url)
    return(data)
  }

.parse_sec_form <-
  function(url = "https://www.sec.gov/Archives/edgar/data/61004/000114036117000046/doc1.xml",
           return_message = TRUE) {
    data <-
      .parse_xml_tables(url = url)

    if (!'nameSECFull' %in% names(data)) {
      data <-
        data %>%
        mutate(nameSECFull = nameSEC)
    }

    cik <-
      url %>% str_replace_all('https://www.sec.gov/Archives/edgar/data/', '') %>% str_split('/') %>% flatten_chr() %>% .[[1]] %>% as.character() %>% readr::parse_number() %>% suppressMessages()

    df_title <-
      .sec_form_title_df()

    is_13FInfo <-
      url %>% str_detect('form13fInfoTable.xml|infotable.xml')
    sec_names <-
      data$nameSECFull %>% unique()

    df_names <-
      .parse_full_form_names(sec_names = sec_names)

    df_names <-
      df_names %>%
      mutate(nameTable = ifelse(
        nameSECFull %>% str_detect("issuerAddress"),
        "issuerAddress",
        nameTable),
        nameTable =  ifelse(
          nameSECFull %>% str_detect("reportingOwner"),
          "reportingOwner",
          nameTable)
      ) %>%
      mutate(nameTable = ifelse(nameSECFull %>% str_detect("issuerInfo."), 'issuerInfo', nameTable),
             nameTable = ifelse(nameSECFull %>% str_detect("securitiesIssued."), 'securitiesIssued', nameTable),
             nameTable = ifelse(nameSECFull %>% str_detect("summaryInfo."), 'summaryInfo', nameTable),
             nameTable = ifelse(nameSECFull %>% str_detect("^comment[A-Z]"), 'Comments', nameTable)
      )

    if (is_13FInfo) {
      df_names <-
        df_names %>%
        mutate(nameTable = 'holdingsInformation')
    }
    if (!'nameSEC' %in% names(data)) {
      data <- data %>%
        mutate(nameSEC = nameSECFull)
    }
    data <-
      data %>%
      select(-nameSEC) %>%
      left_join(df_names) %>%
      mutate(nameActual = ifelse(nameSECFull == "X.1.A.A.", 'idForm', nameActual)) %>%
      suppressMessages()

    if ('countItem' %in% names(data)) {
      data <-
        data %>%
        select(nameTable, countItem, nameSECFull, nameActual, everything()) %>%
        mutate(countItem = countItem - 1) %>%
        suppressMessages()
    }

    if ('property' %in% data$nameTable) {
      data <-
        data %>%
        mutate(nameTable = ifelse(nameTable %>% is.na(), 'Asset', nameTable))
    }

    has_metadata <-
      data %>%
      filter(nameTable %>% is.na()) %>% nrow() > 0

    if (has_metadata) {
      df_metadata <-
        data %>%
        filter(nameTable %>% is.na()) %>%
        select(nameActual, value) %>%
        group_by(nameActual) %>%
        mutate(countItem = 1:n() - 1) %>%
        arrange(countItem) %>%
        ungroup() %>%
        filter(!nameActual %>% str_detect('idCCC')) %>%
        mutate(nameActual = ifelse(countItem == 0, nameActual, nameActual %>% paste0(countItem))) %>%
        select(-countItem)

      col_order <-
        df_metadata$nameActual

      df_metadata <-
        df_metadata %>%
        spread(nameActual, value) %>%
        select(one_of(col_order)) %>%
        mutate(urlSECFiling = url) %>%
        .resolve_form_columns()
    } else {
      df_metadata <-
        tibble(idCIKFiler = cik,
               urlSECFiling = url)
    }

    tables <-
      data %>%
      filter(!nameTable %>% is.na()) %>%
      .$nameTable %>%
      unique()

    data <-
      seq_along(tables) %>%
      future_map(function(x) {
        table <-
          tables[[x]]
        table_name <-
          list('data',
               table %>% substr(1, 1) %>% str_to_upper(),
               table %>% substr(2, nchar(table))) %>%
          purrr::reduce(paste0)

        table_df <-
          data %>%
          filter(nameTable == table) %>%
          select(dplyr::matches("countItem"), nameActual, value) %>%
          select(which(colMeans(is.na(.)) < 1)) %>%
          group_by(nameActual) %>%
          mutate(countItem = 1:n() - 1) %>%
          ungroup()

        has_counts <-
          table_df$countItem %>% max(na.rm = TRUE) > 0

        if (has_counts) {
          table_df <-
            table_df %>%
            arrange(countItem)

          col_order <- c('countItem', table_df$nameActual)

          table_df <-
            table_df %>%
            spread(nameActual, value) %>%
            select(one_of(col_order)) %>%
            mutate(urlSECFiling = url) %>%
            .resolve_form_columns()

          table_df <-
            table_df %>%
            nest(-urlSECFiling, .key = data)
        } else {
          table_df <-
            table_df %>%
            select(-countItem)
          col_order <- c(table_df$nameActual)

          table_df <-
            table_df %>%
            spread(nameActual, value) %>%
            select(one_of(col_order)) %>%
            .resolve_form_columns() %>%
            mutate(urlSECFiling = url)

          table_df <-
            table_df %>%
            nest(-urlSECFiling, .key = data)
        }
        names(table_df)[[2]] <-
          table_name

        df_metadata <-
          df_metadata %>%
          left_join(table_df) %>%
          suppressMessages()

      }) %>%
      reduce(left_join) %>%
      suppressMessages()

    ## maybe add IDCK


    rm(df_metadata)
    return(data)
  }

.parse_form_data <-
  function(.all_filings, filter_parameter = 'isXBRLInstanceFile', return_message = TRUE) {
    df_search <-
      .all_filings %>%
      filter_(.dots = filter_parameter)

    if (filter_parameter == 'isXBRLInstanceFile') {
      if (df_search %>% nrow() == 0) {
        return(tibble())
      }
      parse_xbrl_filer_url_safe <-
        purrr::possibly(.parse_xbrl_filer_url, tibble())
      all_data <-
        df_search$urlSECFiling %>%
        unique() %>%
        future_map_dfr(function(x) {
          .parse_xbrl_filer_url(url = x, return_message = return_message)
        })
      all_data <-
        all_data %>%
        select(-dplyr::matches("idCIK1|nameFiler1")) %>%
        left_join(df_search %>% select(dplyr::matches("idForm"), dplyr::matches("idAccession"), dplyr::matches("nameFile"), dateFiling, urlSECFiling)) %>%
        select(
          dplyr::matches("idCIK"),
          dplyr::matches("name[Entity]|name[Filer]"),
          dateFiling,
          dplyr::matches("idForm"),
          dplyr::matches("idAccession"),
          dplyr::matches("nameFile"),
          everything()
        ) %>%
        suppressMessages()

      return(all_data)
    }

    if (filter_parameter == 'is13FFiling') {
      urls_df <-
        df_search %>% select(urlSECFiling, urlSECFilingDirectory)
      df_13f_urls <-
        1:nrow(urls_df) %>%
        future_map_dfr(function(x){

          row_df <-
            urls_df %>% slice(x)

          url <- row_df$urlSECFiling
          urlSECFilingDirectory <-
            row_df$urlSECFilingDirectory
          parts <-
            url %>%
            str_replace_all("https://www.sec.gov/Archives/edgar/data/", '') %>%
            str_split('\\/') %>%
            flatten_chr()
          idCIKFiler <-
            parts[[1]] %>% as.numeric()
          slugAccession <-
            parts[[2]]

          isPrimary <-
            parts[[3]] %>% str_detect("primary")

          tibble(idCIKFiler, slugAccession, isPrimary, urlSECFiling = url, urlSECFilingDirectory)
        })

      slugs <-
        df_13f_urls$slugAccession %>% unique()

      df_13fs <-
        seq_along(slugs) %>%
        future_map_dfr(function(x){
          slug <-
            slugs[[x]]
          df_period <-
            df_13f_urls %>%
            filter(slugAccession == slug)
          if (df_period %>% nrow() == 2) {
            primary_url <-
              df_period %>% filter(isPrimary) %>%
              .$urlSECFiling
            df_primary <-
              .parse_sec_form(url = primary_url, return_message = return_message) %>%
              mutate(urlSECFiling = primary_url)

            df_primary <-
              df_primary %>%
              left_join(df_13f_urls) %>%
              suppressWarnings()

            no_primary_url <-
              df_period %>% filter(!isPrimary) %>%
              .$urlSECFiling

            urlSECFilingDirectory <-
              df_period %>% filter(!isPrimary) %>%
              .$urlSECFilingDirectory

            df_primary_no <-
              .parse_sec_form(url = no_primary_url, return_message = return_message) %>%
              mutate(urlSECFiling = no_primary_url)

            data <-
              df_primary %>%
              select(-dplyr::matches("urlSECFiling")) %>%
              left_join(df_primary_no %>% select(-dplyr::matches("urlSECFiling"))) %>%
              mutate(urlSECFilingDirectory = urlSECFilingDirectory) %>%
              suppressMessages()
            return(data)
          } else {
            period_url <-
              df_period$urlFiling
            urlSECFilingDirectory <-
              df_period$urlSECFilingDirectory
            data <-
              .parse_sec_form(url = period_url, return_message = return_message) %>%
              mutate(urlFiling = period_url) %>%
              left_join(df_period) %>%
              mutate(urlSECFilingDirectory = urlSECFilingDirectory)
            return(data)
          }
        })

      df_13fs <-
        df_13fs %>%
        left_join(urls_df) %>%
        left_join(df_search %>% select(dateFiling, datePeriodReport, datetimeAccepted, urlSECFilingDirectory, dplyr::matches("urlTextFilingFull"))) %>%
        select(-dplyr::matches("slugAcession")) %>%
        select(dplyr::matches("idCIKFiler"), dplyr::matches("nameFilingManager"), everything()) %>%
        select(dateFiling, everything()) %>%
        suppressMessages()

      return(df_13fs)
    }

    if (filter_parameter == 'isFormD') {
      if ('idForm' %in% names(df_search)){
        df_search <-
          df_search %>%
          filter(!idForm %>% str_detect("10"))
      }
    }
    if (df_search %>% nrow() == 0) {
      return(tibble())
    }

    parse_sec_form_safe <-
      purrr::possibly(.parse_sec_form, tibble())
    all_data <-
      df_search$urlSECFiling %>%
      unique() %>%
      future_map_dfr(function(x) {
        parse_sec_form_safe(url = x, return_message = return_message)
      })

    if (all_data %>% nrow() == 0) {
      return(all_data)
    }

    all_data <-
      all_data %>%
      select(-dplyr::matches("idCIK1|nameFiler1")) %>%
      left_join(df_search %>% select(dplyr::matches("idForm"), dplyr::matches("idAccession"), dplyr::matches("nameFile"), dateFiling, urlSECFiling)) %>%
      select(
        dplyr::matches("idCIK"),
        dplyr::matches("name[Entity]|name[Filer]"),
        dateFiling,
        dplyr::matches("idForm"),
        dplyr::matches("idAccession"),
        dplyr::matches("nameFile"),
        everything()
      ) %>%
      suppressMessages()

    if (filter_parameter == 'hasAssetFile') {
      if('dataComments' %in% names(all_data)) {
        df_comments <-
          all_data %>%
          select(idCIKFiler, dplyr::matches("idAccession"), dplyr::matches("dataComments")) %>%
          mutate(isNULL = dataComments %>% map_lgl(is_null)) %>%
          filter(!isNULL) %>%
          distinct() %>%
          select(-isNULL)

        all_data <-
          all_data %>%
          select(-dataComments) %>%
          mutate(isNULL = dataAsset %>% map_lgl(is_null)) %>%
          filter(!isNULL) %>%
          filter(!nameFile == "ASSET RELATED DOCUMENT") %>%
          distinct() %>%
          select(-isNULL) %>%
          left_join(df_comments) %>%
          suppressMessages()
      }
    }

    all_data <-
      all_data %>%
      select(which(colMeans(is.na(.)) < 1))
    return(all_data)
  }


# index_parsing -----------------------------------------------------------
.parse_sec_filing_index <-
  function(urls, return_message = TRUE) {
    df <-
      tibble()
    success <- function(res) {
      if (return_message) {
        list("Parsing: ", res$url) %>% purrr::reduce(paste0) %>% cat(fill = T)
      }
      page <-
        res$content %>%
        read_html()

      not_503 <-
        !res$status_code == 503

      cik <-
        res$url %>%
        str_split('data/') %>%
        flatten_chr() %>%
        .[[2]] %>%
        str_split('/') %>%
        flatten_chr() %>%
        .[[1]] %>%
        as.numeric()
      if (not_503){
        values <-
          page %>%
          html_nodes('.info') %>%
          html_text()

        if (length(values) == 0) {
          return(tibble())
        }

        items <-
          page %>%
          html_nodes('.infoHead') %>%
          html_text()

        all_items <-
          items %>%
          map_chr(function(x) {
            is_zero <-
              x %>% str_count('\\ ') == 0

            if (x == 'Accepted') {
              return("datetimeAccepted")
            }

            if (x == 'Documents') {
              return('countDocuments')
            }
            if (x == "items") {
              return('descriptionItems')
            }

            if (is_zero) {
              return('item' %>% paste0(x))
            }

            if (x == "Period of Report") {
              return("datePeriodReport")
            }

            if (x == "429 Reference" | x %>% str_detect("Reference")) {
              return("reference429")
            }

            name_items <-
              x %>% str_split('\\ ') %>%
              flatten_chr()

            first <-
              name_items[name_items %>% length()] %>% str_to_lower()

            end <-
              name_items[1:(name_items %>% length() - 1)] %>%
              paste0(collapse = '') %>%
              str_to_title()

            final_name <-
              list(first, end) %>% purrr::invoke(paste0, .)
            return(final_name)
          })
        search_url <-
          res$url
        df_metadata <-
          tibble(item = all_items,
                 value = values) %>%
          mutate(urlSECFilingDirectory = search_url) %>%
          spread(item, value)

        df_metadata <-
          df_metadata %>%
          mutate_at(df_metadata %>% select(dplyr::matches('count')) %>% names(),
                    funs(. %>% as.numeric())) %>%
          mutate_at(
            df_metadata %>% select(dplyr::matches('^date[A-Z]')) %>%  select(-dplyr::matches("datetime"))  %>% names(),
            funs(. %>% lubridate::ymd())
          ) %>%
          mutate_at(
            df_metadata %>% select(dplyr::matches('^datetime')) %>%  select(-dplyr::matches("datetime"))  %>% names(),
            funs(. %>% lubridate::ymd_hms())
          )

        urlSECFiling <-
          page %>%
          html_nodes('#formDiv a') %>%
          html_attr('href') %>%
          paste0('https://www.sec.gov', .)

        namehref <-
          page %>%
          html_nodes('#formDiv a') %>%
          html_text()

        files <-
          page %>%
          html_nodes('#formDiv td:nth-child(2)') %>%
          html_text() %>%
          str_to_upper()

        wrong_length <-
          !(namehref %>% length() == files %>% length())

        if (wrong_length) {
          namehref <-
            namehref[namehref %>% str_detect("\\.")]
          urlSECFiling <-
            urlSECFiling[2:length(urlSECFiling)]
        }

        files <-
          files %>%
          str_trim()

        types_form <-
          page %>%
          html_nodes('td:nth-child(4)') %>%
          html_text() %>%
          str_trim()

        types_form[types_form == ''] <-
          NA

        types_form <-
          types_form[seq_along(files)]

        files[files == ''] <-
          NA
        search_url <-
          res$url
        data <-
          tibble(
            nameFile = files,
            nameHref = namehref,
            typeForm = types_form,
            urlSECFiling
          ) %>%
          mutate(
            isXML = nameHref %>% str_detect("xml"),
            isForm3_4 = nameHref %>% str_detect('doc3.xml|doc4.xml'),
            isFormD = ifelse(
              isXML & typeForm %in% c("D", "D/A"),
              TRUE,
              FALSE
            ),
            is13FFiling = ifelse(
              isXML& typeForm %>% str_detect("13F-HR|INFORMATION TABLE"),
              TRUE,
              FALSE
            ),
            hasSmallOfferingData = ifelse(isXML &
                                            typeForm %>% str_detect("1-A|1-A/A"),
                                          TRUE,
                                          FALSE),
            hasSmallOfferingData = ifelse(typeForm == "C" & isXML, TRUE, hasSmallOfferingData),
            hasAssetFile = typeForm %>% str_detect("EX-102|EX-103")
          ) %>%
          tidyr::separate(nameHref,
                          into = c('nameHREF', 'typeFile'),
                          sep = '\\.') %>%
          mutate(urlSECFilingDirectory = search_url) %>%
          mutate(
            nameFile = ifelse(nameFile == '', NA, nameFile %>% str_to_upper()),
            isCompleteTextFiling = nameFile %>% str_detect("COMPLETE SUBMISSION"),
            isXBRLInstanceFile = ifelse(nameFile %>% str_detect("XBRL INSTANCE"), TRUE, FALSE),
            isImage = ifelse(typeFile %in% c('jpg', 'gif', 'tiff', 'png'), TRUE, FALSE),
            isPDF = ifelse(typeFile %in% c('pdf'), TRUE, FALSE)
          )

        data <-
          data %>%
          left_join(df_metadata) %>%
          mutate(idCIK = cik) %>%
          select(idCIK, dplyr::matches("date"), dplyr::matches("count"), everything()) %>%
          suppressWarnings() %>%
          suppressMessages()
      } else {
        search_url <-
          res$url
        data <-
          tibble(idCIK = cik,
                 urlSECFilingDirectory = search_url)
      }

      df <<-
        df %>%
        bind_rows(data)
    }
    failure <- function(msg){
      tibble()
    }
    urls %>%
      walk(function(x){
        curl_fetch_multi(url = x, success, failure)
      })
    multi_run()
    df
  }

.all_filings <-
  function(urls, return_message = TRUE)  {
  df_filings <-
    urls %>%
    future_map_dfr(function(x){
      .parse_sec_filing_index(urls = x, return_message = return_message)
    })

  return(df_filings)
}

.all_filing_urls <-
  function(data, nest_data = TRUE,
           return_message = TRUE) {
    if (!'urlSECFilingDirectory' %in% names(data)) {
      stop("urlSECFilingDirectory needs to be in the data fields")
    }
    if (!'idAccession' %in% names(data)) {
      df_accession <-
        data$urlSECFilingDirectory %>%
        unique() %>%
        future_map_dfr(function(x){
          urlSECFilingDirectory <-
            x

          idAccession <-
            x %>% str_replace_all('https://www.sec.gov/Archives/edgar/data/', '') %>%
            str_split('\\/') %>%
            flatten_chr() %>% {
              .[length(.)] %>% str_replace_all('-index.htm', '')
            }
          tibble(idAccession, urlSECFilingDirectory)
        })

      data <-
        data %>%
        left_join(df_accession) %>%
        suppressMessages()
    }
    data <-
      data %>%
      select(-dplyr::matches("hasAssetFile|isFormD|is13F|isForm3_4|hasSmallOfferingData")) %>%
      filter(typeFile %>% str_detect("htm")) %>%
      group_by(idAccession) %>%
      mutate(countAccension = 1:n()) %>%
      filter(countAccension == max(countAccension)) %>%
      ungroup() %>%
      arrange(dateFiling)

    urls <-
      data$urlSECFilingDirectory

    df_all_filings <-
      .all_filings(urls = urls, return_message = return_message)

    df_all_filings <-
      df_all_filings %>%
      left_join(data %>% select(urlSECFilingDirectory, countAccension, idAccession)) %>%
      suppressMessages()

    if (nest_data) {
      df_all_filings <-
        df_all_filings %>%
        nest(-c(idAccession, countAccension, urlSECFilingDirectory), .key = dataFilings)

    }
    df_all_filings
  }

# Text Form ---------------------------------------------------------------
.header_names <-
  function() {
    tibble(
      nameSEC = c(
        "ACCEPTANCE-DATETIME",
        "ACCESSION NUMBER",
        "CONFORMED SUBMISSION TYPE",
        "PUBLIC DOCUMENT COUNT",
        "FILED AS OF DATE",
        "DATE AS OF CHANGE",
        "COMPANY CONFORMED NAME",
        "CENTRAL INDEX KEY",
        "STANDARD INDUSTRIAL CLASSIFICATION",
        "IRS NUMBER",
        "STATE OF INCORPORATION",
        "FISCAL YEAR END",
        "FORM TYPE",
        "SEC ACT",
        "SEC FILE NUMBER",
        "FILM NUMBER",
        "STREET 1",
        "CITY",
        "STATE",
        "ZIP",
        "BUSINESS PHONE",
        "FORMER CONFORMED NAME",
        "DATE OF NAME CHANGE",
        "STREET 2",
        "CONFORMED PERIOD OF REPORT",
        "ITEM INFORMATION"
      ),
      nameActual = c(
        "datetimeAccepted",
        "idAccession",
        "idForm",
        "countPublicDocuments",
        "dateFiling",
        "dateFilingChange",
        "nameCompany",
        "idCIK",
        "nameCodeSIC",
        "idIRS",
        "stateIncorporation",
        "monthdayFiscalYearEnd",
        "typeForm",
        "idSECAct",
        "idSEC",
        "idFilm",
        "addressStreet1",
        "city",
        "state",
        "zipcode",
        "telephone",
        "nameCompanyFormer",
        "dateNameChange",
        'addressStreet2',
        'dateReportPeriod',
        'descriptionItem'
      )
    )
  }

.section_names <-
  function() {
    tibble(nameSectionSEC = c(NA, "SUBJECT COMPANY", "FILED BY", 'ISSUER', 'REPORTING-OWNER'),
           nameSectionActual = c('', '', 'FilingEntity', 'Issuer', 'ownerReporting')
    )
  }

.parent_names <-
  function() {
    tibble(nameParentSEC = c(NA, "COMPANY DATA", "FILING VALUES", "BUSINESS ADDRESS", "MAIL ADDRESS",
                             "FORMER COMPANY"),
           nameParentActual = c('', '', '', 'Business', 'Mailing', ''))
  }

.parse_text_headers <-
  function(text_blob){
  header_start <-
    text_blob %>% grep("<SEC-HEADER>",.) + 1

  header_end <-
    text_blob %>% grep("</SEC-HEADER>",.) - 1

  header_text <-
    text_blob[header_start:header_end]

  header_text <-
    header_text %>% str_replace_all('\\<','') %>% str_replace_all('\\>',':')

  df_headers <- tibble(text = header_text) %>%
    tidyr::separate(col = text, into = c('nameSEC', 'value'), sep = '\\:') %>%
    mutate(value = value %>% str_replace_all("\t", '')) %>%
    mutate(idRow = 1:n())

  df_parents <-
    df_headers %>%
    filter(value == '') %>%
    mutate(idRow = idRow + 1) %>%
    dplyr::rename(nameParentSEC = nameSEC) %>%
    select(-value)

  df_section <-
    df_parents %>%
    filter(nameParentSEC %in% c("SUBJECT COMPANY", "FILED BY")) %>%
    select(nameSectionSEC = nameParentSEC, idRow) %>%
    mutate(idRow = idRow + 1)

  df_parents <-
    df_parents %>%
    filter(!nameParentSEC %in% c("SUBJECT COMPANY", "FILED BY")) %>%
    left_join(df_section) %>%
    fill(nameSectionSEC) %>%
    select(nameSectionSEC, nameParentSEC, idRow) %>%
    suppressMessages()

  data <-
    df_headers %>%
    filter(!value == '') %>%
    left_join(df_parents) %>%
    select(idRow, nameSectionSEC, nameParentSEC, everything()) %>%
    tidyr::fill(nameSectionSEC) %>%
    tidyr::fill(nameParentSEC) %>%
    select(-idRow) %>%
    distinct() %>%
    suppressWarnings() %>%
    suppressMessages()

  df_parents <-
    .parent_names()

  df_names <-
    .header_names()
  df_sections <-
    .section_names()

  has_missing_names <-
    data$nameSEC[!data$nameSEC %in% df_names$nameSEC] %>% length() > 0

  if (has_missing_names) {
    df_missing <-
      data$nameSEC[!data$nameSEC %in% df_names$nameSEC] %>% unique() %>%
      future_map_dfr(function(x){
        parts <-
          x %>% str_replace_all('\\-', ' ') %>%
          str_split('\\ ') %>% flatten_chr()


        first <-
          parts[length(parts)] %>%
          str_to_lower()

        is_cik <-
          first %>% str_detect('cik') %>% sum(na.rm = TRUE) > 0

        if (is_cik) {
          first <-
            'idCIK'
        }

        other <-
          list(parts[1:(length(parts) - 1)] %>% str_to_title) %>%
          purrr::reduce(paste0) %>%
          paste0(collapse = '')

        actual <-
          list(first,other) %>%
          purrr::reduce(paste0)

        tibble(nameSEC = x, nameActual = actual)
      })

    df_names <- df_names %>%
      bind_rows(df_missing)
  }

  data <-
    data %>%
    left_join(df_parents) %>%
    left_join(df_sections) %>%
    left_join(df_names) %>%
    mutate(nameParentActual = ifelse(nameParentActual %>% is.na(), '', nameParentActual)) %>%
    suppressMessages() %>%
    unite(nameItem, nameActual, nameParentActual, nameSectionActual, sep = '') %>%
    select(nameItem, value) %>%
    suppressWarnings() %>%
    group_by(nameItem) %>%
    mutate(countItem = 1:n() - 1) %>%
    ungroup() %>%
    mutate(nameItem = ifelse(countItem == 0, nameItem, paste0(nameItem, countItem))) %>%
    suppressMessages() %>%
    select(-countItem)


  col_order <-
    data$nameItem

  data <-
    data %>%
    spread(nameItem, value) %>%
    select(one_of(col_order))

  data <-
    data %>%
    mutate_at(data %>% select(dplyr::matches("datetime")) %>% names(),
              funs(. %>% lubridate::ymd_hms())) %>%
    mutate_at(data %>% select(dplyr::matches("^date[A-Z]")) %>% select(-dplyr::matches("datetime")) %>% names(),
              funs(. %>% lubridate::ymd())) %>%
    mutate_at(data %>% select(dplyr::matches("idCIK|count|monthdayFiscalYearEnd")) %>% names(),
              funs(. %>% as.numeric())) %>%
    mutate_at(data %>% select(dplyr::matches("name[A-Z]|type[A-Z]|description|class")) %>% names(),
              funs(. %>% stringr::str_to_upper()))

  if ('nameCodeSIC' %in% names(data)) {
    data <-
      data %>%
      separate(nameCodeSIC, into = c('nameIndustry', 'idSIC'), sep = '\\[') %>%
      mutate(nameIndustry = nameIndustry %>% str_trim() %>% str_to_upper(),
             idSIC = idSIC %>% as.character() %>% readr::parse_number()) %>%
      suppressWarnings()
  }
  return(data)
}

.parse_for_text <-
  function(text_blob) {
  text_start <-
    text_blob %>% grep("<TEXT>",.) %>% .[[1]] + 1

  text_end <-
    text_blob %>% grep("</TEXT>",.)

  text_end <-
    text_end %>% max() - 1

  df_text <-
    tibble(textRow = text_blob[text_start:text_end]) %>%
    mutate(idRow = 1:n()) %>%
    select(idRow, everything())

  return(df_text)
}

.parse_text_filing <-
  function(url = "https://www.sec.gov/Archives/edgar/data/732712/000119312517025716/0001193125-17-025716.txt") {
    text_blob <-
      url %>%
      readr::read_lines() %>% {
        .[!. == ''] %>%
          str_trim()
      }

    has_html <-
      text_blob %>% str_count("<HTML>") %>% sum(na.rm = TRUE) > 0
    has_xml <-
      text_blob %>% str_count("<XML>") %>% sum(na.rm = TRUE) > 0

    df_headers <-
      .parse_text_headers(text_blob = text_blob)

    df_text <-
      .parse_for_text(text_blob = text_blob) %>%
      mutate(idAccession = df_headers$idAccession) %>%
      nest(-idAccession, .key = textFiling)

    data <-
      df_headers %>%
      left_join(df_text) %>%
      mutate(urlSECFiling = url,
             hasHTML = has_html,
             hasXML = has_xml) %>%
      select(dplyr::matches("idCIK"), dplyr::matches("dateFiling"), idAccession, dplyr::matches("idForm"), dplyr::matches("nameCompany"), everything())

    return(data)

  }


.sec_complete_filings <-
  function(urls = c("https://www.sec.gov/Archives/edgar/data/732712/000119312517030264/0001193125-17-030264.txt", "https://www.sec.gov/Archives/edgar/data/732712/000161159317000024/0001611593-17-000024.txt", "https://www.sec.gov/Archives/edgar/data/1629703/000161159317000025/0001611593-17-000025.txt", "https://www.sec.gov/Archives/edgar/data/1284999/000161159317000014/0001611593-17-000014.txt"),
           return_message =  TRUE) {
    df <-
      tibble()
    success <- function(res) {
      url <-
        res$url
      if (return_message) {
        list("Parsing: ", url, "\n") %>% purrr::reduce(paste0) %>% cat(fill = T)
      }

      data <-
        .parse_text_filing(url = url)
      df <<-
        df %>%
        bind_rows(data)
    }
    failure <- function(msg) {
      tibble()
    }
    urls %>%
      walk(function(x) {
        curl_fetch_multi(url = x, success, failure)
      })
    multi_run()
    df
  }


# XBRL Finder -------------------------------------------------------------
.parse_xbrl_filer_url <-
  function(url = "https://www.sec.gov/Archives/edgar/data/1037540/000165642316000023/bxp-20160930.xml",
           return_message = TRUE) {
    options(stringsAsFactors = FALSE, scipen = 999999)
    cik <-
      url %>%
      str_split('data/') %>%
      flatten_chr() %>%
      .[[2]] %>%
      str_split('/') %>%
      flatten_chr() %>%
      .[[1]] %>%
      as.numeric()
    td <-
      tempdir()
    tf <-
      tempfile(tmpdir = td, fileext = ".xml")

    url %>%
      curl::curl_download(destfile = tf)

    doc <-
      tf %>%
      XBRL::xbrlParse()


    ## Get a data frame with facts:
    df_fct <-
      XBRL::xbrlProcessFacts(doc) %>%
      as_tibble()

    df_fct <-
      df_fct %>%
      mutate(
        isNumber = ifelse(!fact %>% readr::parse_number() %>% is.na(), TRUE, FALSE),
        amountFact = ifelse(isNumber == TRUE, fact %>%as.character() %>%  readr::parse_number(), NA)
      ) %>%
      separate(elementId,
               c('codeElement', 'nameElement'),
               sep = '\\_',
               remove = FALSE) %>%
      suppressWarnings()
    ## Get a data frame with contexts:
    df_cts <-
      XBRL::xbrlProcessContexts(doc) %>%
      as_tibble()
    ## Get a data frame with units:
    df_unt <-
      XBRL::xbrlProcessUnits(doc) %>%
      as_tibble()

    df_sch <-
      XBRL::xbrlGetSchemaName(doc) %>%
      as_tibble()

    df_footnotes <-
      XBRL::xbrlProcessFootnotes(doc) %>%
      as_tibble()


    ## Free the external memory used:
    XBRL::xbrlFree(doc)
    url_xsd <-
      url %>% str_replace(".xml", ".xsd")
    url_xsd %>%
      curl_download(destfile = tf)

    ## Parse the schema file:
    docS <-
      tf %>%
      XBRL::xbrlParse()
    ## Get roles:
    df_rls <-
      docS %>%
      XBRL::xbrlProcessRoles() %>%
      as_tibble()

    ## calculation
    url_cal <-
      url %>% str_replace(".xml", "_cal.xml")
    if (suppressWarnings(httr::url_ok(url_cal))){
      url_cal %>%
        curl_download(destfile = tf)

      docS <-
        tf %>%
        XBRL::xbrlParse()

      df_calcs <-
        docS %>%
        XBRL::xbrlProcessArcs(arcType = 'calculation') %>%
        as_tibble()
    } else {
      df_calcs <-
        tibble()
    }

    ## definition
    url_def <-
      url %>% str_replace(".xml", "_def.xml")

    url_def %>%
      curl_download(destfile = tf)

    docS <-
      tf %>%
      XBRL::xbrlParse()

    df_defs <-
      docS %>%
      XBRL::xbrlProcessArcs(arcType = 'definition') %>%
      as_tibble()

    ## labels
    url_lab <-
      url %>% str_replace(".xml", "_lab.xml")

    url_lab %>%
      curl_download(destfile = tf)

    docS <-
      tf %>%
      XBRL::xbrlParse()

    df_labels <-
      docS %>%
      XBRL::xbrlProcessLabels() %>%
      as_tibble()

    ## presentation
    url_pre <-
      url %>% str_replace(".xml", "_pre.xml")

    url_pre %>%
      curl_download(destfile = tf)

    docS <-
      tf %>%
      XBRL::xbrlParse()

    ## Free the external memory used:
    tf %>%
      unlink()
    data <-
      tibble(
        idCIK = cik,
        urlSECFiling = url,
        dataFacts = list(df_fct),
        dataContexts = list(df_cts),
        dataUnits = list(df_unt),
        dataFootnotes = list(df_footnotes),
        dataRoles = list(df_rls),
        dataCalculations = list(df_calcs) ,
        dataDefinitions = list(df_defs),
        dataLabel = list(df_labels)
      )
    td %>% unlink()
    tf %>% unlink()

    return(data)
  }


# dictionaries ------------------------------------------------------------
.sec_form_title_df <-
  function() {
    tibble(
      nameSEC = c(
        "conversionOrExercisePrice",
        "deemedExecutionDate",
        "directOrIndirectOwnership",
        "documentType",
        "equitySwapInvolved",
        "exerciseDate",
        "expirationDate",
        "footnote",
        "isDirector",
        "isOfficer",
        "isOther",
        "issuerCik",
        "issuerName",
        "issuerTradingSymbol",
        "isTenPercentOwner",
        "natureOfOwnership",
        "noSecuritiesOwned",
        "notSubjectToSection16",
        "officerTitle",
        "otherText",
        "periodOfReport",
        "postTransactionAmountsOwnedFollowingTransaction",
        "remarks",
        "rptOwnerCik",
        "rptOwnerCity",
        "rptOwnerName",
        "rptOwnerState",
        "rptOwnerStateDescription",
        "rptOwnerStreet1",
        "rptOwnerStreet2",
        "rptOwnerZipCode",
        "schemaVersion",
        "securityTitle",
        "sharesOwnedFollowingTransaction",
        "signatureDate",
        "signatureName",
        "transactionAcquiredDisposedCode",
        "transactionCode",
        "transactionDate",
        "transactionFormType",
        "transactionPricePerShare",
        "transactionShares",
        "transactionTimeliness",
        "transactionTotalValue",
        "underlyingSecurityShares",
        "underlyingSecurityTitle",
        "clarificationOfResponse", "isBusinessCombinationTransaction",
        "cik", "moreThanOneYear", "previousName", "edgarPreviousNameList",
        "entityName", "entityType", "entityTypeOtherDesc", "federalExemptionsExclusions",
        "industryGroupType", "investmentFundType", "investmentFundInfo",
        "hasNonAccreditedInvestors", "numberNonAccreditedInvestors",
        "totalNumberAlreadyInvested", "city", "stateOrCountry", "stateOrCountryDescription",
        "street1", "street2", "zipCode", "issuerPhoneNumber", "issuerPreviousNameList",
        "jurisdictionOfInc", "overFiveYears", "yearOfInc", "withinFiveYears",
        "yetToBeFormed", "aggregateNetAssetValueRange", "revenueRange",
        "minimumInvestmentAccepted", "totalAmountSold", "totalOfferingAmount",
        "totalRemaining", "firstName", "lastName", "middleName", "relationship",
        "relationshipClarification", "dollarAmount", "isEstimate", "associatedBDCRDNumber",
        "associatedBDName", "foreignSolicitation", "recipientCRDNumber",
        "recipientName", "description", "state", "statesOfSolicitationList",
        "authorizedRepresentative", "nameOfSigner", "signatureTitle",
        "submissionType", "testOrLive", "dateOfFirstSale", "yetToOccur",
        "isAmendment", "descriptionOfOtherType", "isDebtType", "isEquityType",
        "isMineralPropertyType", "isOptionToAcquireType", "isOtherType",
        "isPooledInvestmentFundType", "isSecurityToBeAcquiredType", "isTenantInCommonType",
        'notSubjectToSection16', 'rptOwnerStreet1', 'rptOwnerStreet2',

        "liveTestFlag", "confirmingCopyFlag", "returnCopyFlag", "overrideInternetFlag",
        "ccc", "reportCalendarOrQuarter", "filingManagername", "filingManageraddressstreet1",
        "filingManageraddressstreet2", "filingManageraddresscity", "filingManageraddressstateOrCountry",
        'filingManagerstateOrCountryDescription',
        "filingManageraddresszipCode", "reportType", "form13FFileNumber",
        "provideInfoForInstruction5", "name", "title", "phone", "signature",
        "otherIncludedManagersCount", "tableEntryTotal", "tableValueTotal",
        "isConfidentialOmitted",
        "nameOfIssuer", "titleOfClass", "cusip", "value", "investmentDiscretion",
        "otherManager", "putCall", "sshPrnamt", "sshPrnamtType", "Sole",
        "Shared", "None",

        "offeringFileNumber", "sinceLastFiling", "jurisdictionOrganization",
        "yearIncorporation", "sicCode", "irsNum", "fullTimeEmployees",
        "partTimeEmployees", "phoneNumber", "connectionName", "industryGroup",
        "cashEquivalents", "investmentSecurities", "accountsReceivable",
        "propertyPlantEquipment", "totalAssets", "accountsPayable", "longTermDebt",
        "totalLiabilities", "totalStockholderEquity", "totalLiabilitiesAndEquity",
        "totalRevenues", "costAndExpensesApplToRevenues", "depreciationAndAmortization",
        "netIncome", "earningsPerShareBasic", "earningsPerShareDiluted",
        "nameAuditor", "commonEquityClassName", "outstandingCommonEquity",
        "commonCusipEquity", "publiclyTradedCommonEquity", "preferredEquityClassName",
        "outstandingPreferredEquity", "preferredCusipEquity", "publiclyTradedPreferredEquity",
        "debtSecuritiesClassName", "outstandingDebtSecurities", "cusipDebtSecurities",
        "publiclyTradedDebtSecurities", "certifyIfTrue", "certifyIfNotDisqualified",
        "summaryInfo", "financialStatementAuditStatus", "securitiesOfferedTypes",
        "offerDelayedContinuousFlag", "offeringYearFlag", "offeringAfterQualifFlag",
        "offeringBestEffortsFlag", "solicitationProposedOfferingFlag",
        "resaleSecuritiesAffiliatesFlag", "securitiesOffered", "outstandingSecurities",
        "pricePerSecurity", "issuerAggregateOffering", "securityHolderAggegate",
        "qualificationOfferingAggregate", "concurrentOfferingAggregate",
        "totalAggregateOffering", "underwritersServiceProviderName",
        "underwritersFees", "auditorServiceProviderName", "auditorFees",
        "legalServiceProviderName", "legalFees", "promotersServiceProviderName",
        "promotersFees", "brokerDealerCrdNumber", "estimatedNetAmount",
        "clarificationResponses", "jurisdictionsOfSecOfferedSame", "issueJuridicationSecuritiesOffering",
        "dealersJuridicationSecuritiesOffering", "securitiesIssuerName",
        "securitiesIssuerTitle", "securitiesIssuedTotalAmount", "securitiesPrincipalHolderAmount",
        "securitiesIssuedAggregateAmount", "securitiesActExcemption",
        "certifyIfBadActor", "salesCommissionsServiceProviderName",
        "salesCommissionsServiceProviderFees", "jurisdictionsOfSecOfferedNone",
        "ifUnregsiteredNone", "blueSkyServiceProviderName", "blueSkyFees",
        'indicateTier1Tier2Offering', 'X.1.A.A.', 'X.1.A.A.', 'aggregateConsiderationBasis',
        'findersFeesServiceProviderName' , 'finderFeesFee',
        'loans', 'propertyAndEquipment', 'deposits', 'totalInterestIncome',
        'totalInterestExpenses', 'securitiesOfferedOtherDesc', 'comment',
        "assetTypeNumber",
        "assetNumber",
        "assetGroupNumber",
        "reportPeriodBeginningDate",
        "reportPeriodEndDate",
        "issuerName",
        "originalIssuanceDate",
        "originalSecurityAmount",
        "originalSecurityTermNumber",
        "securityMaturityDate",
        "originalAmortizationTermNumber",
        "originalInterestRatePercentage",
        "accrualTypeCode",
        "interestRateTypeCode",
        "originalInterestOnlyTermNumber",
        "firstPaymentDate",
        "underwritingIndicator",
        "securityTitleName",
        "denominationNumber",
        "currencyName",
        "trusteeName",
        "secFileNumber",
        "cik",
        "callableIndicator",
        "paymentFrequencyCode",
        "zeroCouponIndicator",
        "assetAddedIndicator",
        "assetModifiedIndicator",
        "reportPeriodBeginningAssetBalanceAmount",
        "reportPeriodBeginningScheduledAssetBalanceAmount",
        "reportPeriodScheduledPaymentAmount",
        "reportPeriodInterestRatePercentage",
        "totalActualPaidAmount",
        "actualInterestCollectionPercentage",
        "actualPrincipalCollectedAmount",
        "actualOtherCollectionAmount",
        "otherPrincipalAdjustmentAmount",
        "otherInterestAdjustmentAmount",
        "scheduledInterestAmount",
        "scheduledPrincipalAmount",
        "endReportingPeriodActualBalanceAmount",
        "endReportingPeriodScheduledBalanceAmount",
        "servicingFeePercentage",
        "servicingFlatFeeAmount",
        "zeroBalanceCode",
        "zeroBalanceEffectiveDate",
        "remainingTermToMaturityNumber",
        "currentDelinquentStatusNumber",
        "paymentPastDueDaysNumber",
        "paymentPastDueNumber",
        "nextReportPeriodPaymentDueAmount",
        "nextDueDate",
        "primaryLoanServicerName",
        "mostRecentServicingTransferReceivedDate",
        "assetSubjectToDemandIndicator",
        "statusAssetSubjectToDemandCode",
        "repurchaseAmount",
        "demandResolutionDate",
        "repurchaserName",
        "repurchaseReplacementReasonCode",
        "reportPeriodBeginDate",
        "originalLoanPurposeCode",
        "originatorName",
        "originalLoanAmount",
        "originalLoanMaturityDate",
        "originalInterestRateTypeCode",
        "originalLienPositionCode",
        "mostRecentJuniorLoanBalanceAmount",
        "mostRecentJuniorLoanBalanceDate",
        "mostRecentSeniorLoanAmount",
        "mostRecentSeniorLoanAmountDate",
        "loanTypeMostSeniorLienCode",
        "mostSeniorLienHybridPeriodNumber",
        "mostSeniorLienNegativeAmortizationLimitPercentage",
        "mostSeniorLienOriginationDate",
        "prepaymentPenaltyIndicator",
        "negativeAmortizationIndicator",
        "modificationIndicator",
        "modificationNumber",
        "mortgageInsuranceRequirementIndicator",
        "balloonIndicator",
        "coveredHighCostCode",
        "servicerHazardInsuranceCode",
        "refinanceCashOutAmount",
        "totalOriginationDiscountAmount",
        "brokerIndicator",
        "channelCode",
        "nationalMortgageLicenseSystemCompanyNumber",
        "buyDownNumber",
        "loanDelinquencyAdvanceNumber",
        "originationARMIndexCode",
        "armMarginPercentage",
        "fullyIndexedRatePercentage",
        "initialFixedRatePeriodHybridARMNumber",
        "initialInterestRateDecreasePercentage",
        "initialInterestRateIncreasePercentage",
        "indexLookbackNumber",
        "subsequentInterestRateResetNumber",
        "lifetimeRateCeilingPercentage",
        "lifetimeRateFloorPercentage",
        "subsequentInterestRateDecreasePercentage",
        "subsequentInterestRateIncreasePercentage",
        "subsequentPaymentResetNumber",
        "armRoundCode",
        "armRoundPercentage",
        "optionArmIndicator",
        "paymentMethodAfterRecastCode",
        "initialMinimumPaymentAmount",
        "convertibleIndicator",
        "HELOCIndicator",
        "HELOCDrawNumber",
        "prepaymentPenaltyCalculationCode",
        "prepaymentPenaltyTypeCode",
        "prepaymentPenaltyTotalTermNumber",
        "prepaymentPenaltyHardTermNumber",
        "negativeAmortizationLimitAmount",
        "negativeAmortizationInitialRecastNumber",
        "negativeAmortizationSubsequentRecastNumber",
        "negativeAmortizationBalanceAmount",
        "initialFixedPaymentNumber",
        "initialPaymentCapPercentage",
        "subsequentPaymentCapPercentage",
        "initialMinimumPaymentResetNumber",
        "subsequentMinimumPaymentResetNumber",
        "minimumPaymentAmount",
        "geographicalLocation",
        "occupancyStatusCode",
        "mostRecentOccupancyStatusCode",
        "propertyTypeCode",
        "mostRecentPropertyValueAmount",
        "mostRecentPropertyValueTypeCode",
        "mostRecentPropertyValueDate",
        "mostRecentAVMModelCode",
        "mostRecentAVMConfidenceNumber",
        "originalCLTVPercentage",
        "originalLTVPercentage",
        "originalObligorNumber",
        "originalObligorCreditScoreNumber",
        "originalObligorCreditScoreType",
        "mostRecentObligorCreditScoreNumber",
        "mostRecentObligorCreditScoreType",
        "mostRecentObligorCreditScoreDate",
        "obligorIncomeVerificationLevelCode",
        "IRSForm4506TIndicator",
        "originatorFrontEndDTIPercentage",
        "originatorBackEndDTIPercentage",
        "obligorEmploymentVerificationCode",
        "obligorEmploymentLengthCode",
        "obligorAssetVerificationCode",
        "originalPledgedAssetsAmount",
        "qualificationMethodCode",
        "mortgageInsuranceCompanyName",
        "mortgageInsuranceCoveragePercentage",
        "poolInsuranceCompanyName",
        "poolInsuranceStopLossPercentage",
        "mortgageInsuranceCoverageTypeCode",
        "modificationIndicatorReportingPeriod",
        "nextPaymentDueDate",
        "advancingMethodCode",
        "servicingAdvanceMethodologyCode",
        "stopPrincipalInterestAdvancingDate",
        "reportingPeriodBeginningLoanBalanceAmount",
        "reportingPeriodBeginningScheduledLoanBalanceAmount",
        "nextReportingPeriodPaymentDueAmount",
        "reportingPeriodInterestRatePercentage",
        "nextInterestRatePercentage",
        "otherAssessedUncollectedServicerFeeamount",
        "otherServicingFeeRetainedByServicerAmount",
        "reportingPeriodEndActualBalanceAmount",
        "reportingPeriodEndScheduledBalanceAmount",
        "reportingPeriodScheduledPaymentAmount",
        "actualInterestCollectedAmount",
        "actualOtherCollectedAmount",
        "paidThroughDate",
        "interestPaidThroughDate",
        "paidFullAmount",
        "servicerAdvancedPrincipalAmount",
        "servicerAdvancedRepaidPrincipalAmount",
        "servicerAdvancedCumulativePrincipalAmount",
        "servicerAdvanceInterestAmount",
        "servicerAdvanceRepaidInterestAmount",
        "servicerAdvanceCumulativeInterestAmount",
        "servicerAdvanceTaxesInsuranceAmount",
        "servicerAdvanceRepaidTaxesInsuranceAmount",
        "servicerAdvanceCumulativeTaxesInsuranceAmount",
        "servicerAdvanceCorporateAmount",
        "servicerAdvanceRepaidCorporateAmount",
        "servicerAdvanceCumulativeCorporateAmount",
        "mostRecentTwelveMonthHistoryCode",
        "nextResetRatePercentage",
        "nextPaymentChangeDate",
        "nextInterestRateChangeDate",
        "nextResetPaymentAmount",
        "exercisedArmConversionOptionIndicator",
        "primaryServicerName",
        "masterServicerName",
        "specialServicerName",
        "subServicerName",
        "assetSubjectDemandIndicator",
        "assetSubjectDemandStatusCode",
        "repurchaseReplacementCode",
        "chargeOffPrincipalAmount",
        "chargeOffInterestAmount",
        "lossMitigationTypeCode",
        "mostRecentLoanModificationEventCode",
        "mostRecentLoanModificationEffectiveDate",
        "postModificationMaturityDate",
        "postModificationInterestRateTypeCode",
        "postModificationAmortizationTypeCode",
        "postModificationInterestPercentage",
        "postModificationFirstPaymentDate",
        "postModificationLoanBalanceAmount",
        "postModificationPrincipalInterestPaymentAmount",
        "totalCapAmount",
        "incomeVerificationIndicatorAtModification",
        "modificationFrontEndDebtToIncomePercentage",
        "modificationBackEndDebtToIncomePercentage",
        "totalDeferredAmount",
        "forgivenPrincipalCumulativeAmount",
        "forgivenPrincipalReportingPeriodAmount",
        "forgivenInterestCumulativeAmount",
        "forgivenInterestReportingPeriodAmount",
        "actualEndingBalanceTotalDebtAmount",
        "scheduledEndingBalanceTotalDebtAmount",
        "postModificationARMCode",
        "postModificationARMIndexCode",
        "postModificationMarginPercentage",
        "postModificationInterestResetNumber",
        "postModificationNextResetDate",
        "postModificationIndexLookbackNumber",
        "postModificationARMRoundingCode",
        "postModificationARMRoundingPercentage",
        "postModificationInitialMinimumPayment",
        "postModificationNextPaymentAdjustmentDate",
        "postModificationARMPaymentRecastFrequency",
        "postModificationLifetimeFloorPercentage",
        "postModificationLifetimeCeilingPercentage",
        "postModificationInitialInterestRateIncreasePercentage",
        "postModificationInitialInterestRateDecreasePercentage",
        "postModificationSubsequentInterestIncreasePercentage",
        "postModificationSubsequentInterestRateDecreasePercentage",
        "postModificationPaymentCapPercentage",
        "postModificationPaymentMethodAfterRecastCode",
        "postModificationARMInterestRateTeaserNumber",
        "postModificationARMPaymentTeaserNumber",
        "postModificationARMNegativeAmortizationIndicator",
        "postModificationARMNegativeAmortizationCapPercentage",
        "postModificationInterestOnlyTermNumber",
        "postModificationInterestOnlyLastPaymentDate",
        "postModificationBalloonAmount",
        "postModificationInterestRateStepIndicator",
        "postModificationStepInterestPercentage",
        "postModificationStepDate",
        "postModificationStepPrincipalInterestPaymentAmount",
        "postModificationStepNumber",
        "postModificationMaximumFutureStepAgreementPercentage",
        "postModificationMaximumStepAgreementRateDate",
        "nonInterestBearingDeferredPrincipalCumulativeAmount",
        "nonInterestBearingDeferredPrincipalReportingPeriodAmount",
        "recoveryDeferredPrincipalReportingPeriodAmount",
        "nonInterestBearingDeferredPaidFullAmount",
        "nonInterestBearingDeferredInterestFeeReportingPeriodAmount",
        "nonInterestBearingDeferredInterestFeeCumulativeAmount",
        "recoveryDeferredInterestFeeReportingPeriodAmount",
        "mostRecentForbearancePlanOrTrialModificationStartDate",
        "mostRecentForbearancePlanOrTrialModificationScheduledEndDate",
        "mostRecentTrialModificationViolatedDate",
        "mostRecentRepaymentPlanStartDate",
        "mostRecentRepaymentPlanScheduledEndDate",
        "mostRecentRepaymentPlanViolatedDate",
        "shortSaleAcceptedOfferAmount",
        "mostRecentLossMitigationExitDate",
        "mostRecentLossMitigationExitCode",
        "attorneyReferralDate",
        "foreclosureDelayReasonCode",
        "foreclosureExitDate",
        "foreclosureExitReasonCode",
        "noticeOfIntentDate",
        "mostRecentAcceptedREOOfferAmount",
        "mostRecentAcceptedREOOfferDate",
        "grossLiquidationProceedsAmount",
        "netSalesProceedsAmount",
        "reportingPeriodLossPassedToIssuingEntityAmount",
        "cumulativeTotalLossPassedToIssuingEntityAmount",
        "subsequentRecoveryAmount",
        "evictionIndicator",
        "reoExitDate",
        "reoExitReasonCode",
        "UPBLiquidationAmount",
        "servicingFeesClaimedAmount",
        "servicerAdvanceReimbursedPrincipalAmount",
        "servicerAdvanceReimbursedInterestAmount",
        "servicerAdvanceReimbursedTaxesInsuranceAmount",
        "servicerAdvanceReimbursedCorporateAmount",
        "REOManagementFeesAmount",
        "cashKeyDeedAmount",
        "performanceIncentiveFeesAmount",
        "mortgageInsuranceClaimFiledDate",
        "mortgageInsuranceClaimAmount",
        "mortgageInsuranceClaimPaidDate",
        "mortgageInsuranceClaimPaidAmount",
        "mortgageInsuranceClaimDeniedRescindedDate",
        "marketableTitleTransferDate",
        "nonPayStatusCode",
        "reportingActionCode",
        "GroupID",
        "reportingPeriodBeginningDate",
        "reportingPeriodEndDate",
        "originationDate",
        "originalTermLoanNumber",
        "maturityDate",
        "interestRateSecuritizationPercentage",
        "interestAccrualMethodCode",
        "firstLoanPaymentDueDate",
        "lienPositionSecuritizationCode",
        "loanStructureCode",
        "paymentTypeCode",
        "periodicPrincipalAndInterestPaymentSecuritizationAmount",
        "scheduledPrincipalBalanceSecuritizationAmount",
        "NumberPropertiesSecuritization",
        "NumberProperties",
        "graceDaysAllowedNumber",
        "interestOnlyIndicator",
        "prepaymentPremiumIndicator",
        "modifiedIndicator",
        "armIndexCode",
        "firstRateAdjustmentDate",
        "firstPaymentAdjustmentDate",
        "armMarginNumber",
        "lifetimeRateCapPercentage",
        "periodicRateIncreaseLimitPercentage",
        "periodicRateDecreaseLimitPercentage",
        "periodicPaymentAdjustmentMaximumAmount",
        "periodicPaymentAdjustmentMaximumPercent",
        "rateResetFrequencyCode",
        "paymentResetFrequencyCode",
        "indexLookbackDaysNumber",
        "prepaymentLockOutEndDate",
        "yieldMaintenanceEndDate",
        "prepaymentPremiumsEndDate",
        "maximumNegativeAmortizationAllowedPercentage",
        "maximumNegativeAmortizationAllowedAmount",
        "negativeAmortizationDeferredInterestCapAmount",
        "deferredInterestCumulativeAmount",
        "deferredInterestCollectedAmount",
        "property",
        "reportPeriodModificationIndicator",
        "reportPeriodBeginningScheduleLoanBalanceAmount",
        "totalScheduledPrincipalInterestDueAmount",
        "servicerTrusteeFeeRatePercentage",
        "unscheduledPrincipalCollectedAmount",
        "reportPeriodEndActualBalanceAmount",
        "reportPeriodEndScheduledLoanBalanceAmount",
        "hyperAmortizingDate",
        "servicingAdvanceMethodCode",
        "nonRecoverabilityIndicator",
        "totalPrincipalInterestAdvancedOutstandingAmount",
        "totalTaxesInsuranceAdvancesOutstandingAmount",
        "otherExpensesAdvancedOutstandingAmount",
        "paymentStatusLoanCode",
        "armIndexRatePercentage",
        "nextInterestRateChangeAdjustmentDate",
        "nextPaymentAdjustmentDate",
        "mostRecentSpecialServicerTransferDate",
        "mostRecentMasterServicerReturnDate",
        "realizedLossToTrustAmount",
        "liquidationPrepaymentCode",
        "liquidationPrepaymentDate",
        "prepaymentPremiumYieldMaintenanceReceivedAmount",
        "workoutStrategyCode",
        "lastModificationDate",
        "modificationCode",
        "postModificationPaymentAmount",
        "postModificationAmortizationPeriodAmount",
        "propertyName",
        "propertyAddress",
        "propertyCity",
        "propertyState",
        "propertyZip",
        "propertyCounty",
        "netRentableSquareFeetNumber",
        "netRentableSquareFeetSecuritizationNumber",
        "unitsBedsRoomsNumber",
        "unitsBedsRoomsSecuritizationNumber",
        "yearBuiltNumber",
        "yearLastRenovated",
        "valuationSecuritizationAmount",
        "valuationSourceSecuritizationCode",
        "valuationSecuritizationDate",
        "mostRecentValuationAmount",
        "mostRecentValuationDate",
        "mostRecentValuationSourceCode",
        "physicalOccupancySecuritizationPercentage",
        "mostRecentPhysicalOccupancyPercentage",
        "propertyStatusCode",
        "defeasanceOptionStartDate",
        "DefeasedStatusCode",
        "largestTenant",
        "squareFeetLargestTenantNumber",
        "leaseExpirationLargestTenantDate",
        "secondLargestTenant",
        "squareFeetSecondLargestTenantNumber",
        "leaseExpirationSecondLargestTenantDate",
        "thirdLargestTenant",
        "squareFeetThirdLargestTenantNumber",
        "leaseExpirationThirdLargestTenantDate",
        "financialsSecuritizationDate",
        "mostRecentFinancialsStartDate",
        "mostRecentFinancialsEndDate",
        "revenueSecuritizationAmount",
        "mostRecentRevenueAmount",
        "operatingExpensesSecuritizationAmount",
        "operatingExpensesAmount",
        "netOperatingIncomeSecuritizationAmount",
        "mostRecentNetOperatingIncomeAmount",
        "netCashFlowFlowSecuritizationAmount",
        "mostRecentNetCashFlowAmount",
        "netOperatingIncomeNetCashFlowSecuritizationCode",
        "netOperatingIncomeNetCashFlowCode",
        "mostRecentDebtServiceAmount",
        "debtServiceCoverageNetOperatingIncomeSecuritizationPercentage",
        "mostRecentDebtServiceCoverageNetOperatingIncomePercentage",
        "debtServiceCoverageNetCashFlowSecuritizationPercentage",
        "mostRecentDebtServiceCoverageNetCashFlowpercentage",
        "debtServiceCoverageSecuritizationCode",
        "mostRecentDebtServiceCoverageCode",
        "mostRecentAnnualLeaseRolloverReviewDate",
        "reportingPeriodEndingDate",
        "originalLoanTerm",
        "loanMaturityDate",
        "interestCalculationTypeCode",
        "originalFirstPaymentDate",
        "gracePeriodNumber",
        "subvented",
        "vehicleManufacturerName",
        "vehicleModelName",
        "vehicleNewUsedCode",
        "vehicleModelYear",
        "vehicleTypeCode",
        "vehicleValueAmount",
        "vehicleValueSourceCode",
        "obligorCreditScoreType",
        "obligorCreditScore",
        "coObligorIndicator",
        "paymentToIncomePercentage",
        "obligorGeographicLocation",
        "reportingPeriodModificationIndicator",
        "nextReportingPeriodPaymentAmountDue",
        "otherServicerFeeRetainedByServicer",
        "otherAssessedUncollectedServicerFeeAmount",
        "reportingPeriodActualEndBalanceAmount",
        "totalActualAmountPaid",
        "servicerAdvancedAmount",
        "currentDelinquencyStatus",
        "chargedoffPrincipalAmount",
        "recoveredAmount",
        "modificationTypeCode",
        "paymentExtendedNumber",
        "repossessedIndicator",
        "repossessedProceedsAmount",
        "reportingPeriodBeginDate",
        "acquisitionCost",
        "originalLeaseTermNumber",
        "scheduledTerminationDate",
        "gracePeriod",
        "baseResidualValue",
        "baseResidualSourceCode",
        "contractResidualValue",
        "lesseeCreditScoreType",
        "lesseeCreditScore",
        "lesseeIncomeVerificationLevelCode",
        "lesseeEmploymentVerificationCode",
        "coLesseePresentIndicator",
        "lesseeGeographicLocation",
        "remainingTermNumber",
        "reportingPeriodSecuritizationValueAmount",
        "securitizationDiscountRate",
        "otherLeaseLevelServicingFeesRetainedAmount",
        "reportingPeriodEndingActualBalanceAmount",
        "reportingPeriodEndActualSecuritizationAmount",
        "primaryLeaseServicerName",
        "DemandResolutionDate",
        "repurchaseOrReplacementReasonCode",
        "chargedOffAmount",
        "leaseExtended",
        "terminationIndicator",
        "excessFeeAmount",
        "liquidationProceedsAmount",
        "commentNumber", "commentColumn", "commentDescription",
        'previousAccessionNumber', 'itemNumber', 'fieldName', 'notes', 'sequenceNumber',
        "amendmentNo",
        "amendmentType",
        "confDeniedExpired",
        'additionalInformation',
        'fileNumber'
      ),
      nameActual = c(
        "priceExerciseConversion",
        "dateDeemedExecution",
        "codeOwnershipDirectIndirect",
        "idDocument",
        "isEquitySwapInvolved",
        "dateExercised",
        "dateExpiration",
        "descriptionFootnote",
        "isDirector",
        "isOfficer",
        "isOther",
        "idCIKIssuer",
        "nameIssuer",
        "idTickerIssuer",
        "isTenPercentOwner",
        "descriptionNatureOfOwnership",
        "isNoSecuritiesOwned",
        "isNotSubjectToSection16",
        "titleOfficer",
        "descriptionOtherText",
        "dateReport",
        "countSharesOwnedPostTransaction",
        "descriptionRemarks",
        "idCIKOwner",
        "cityOwenr",
        "nameOwner",
        "stateOwner",
        "descriptionStateOwner",
        "addressStreet1Owner",
        "addressStreet2Owner",
        "zipcodeOwner",
        "idSchema",
        "titleSecurity",
        "countSharesOwnedPostTransaction",
        "dateSignature",
        "nameSignature",
        "codeTransactionAcquiredDisposed",
        "codeTransaction",
        "dateTransaction",
        "idFormTransaction",
        "pricePerShareTransaction",
        "countSharesTransaction",
        "idCodeTimelinessTransaction",
        "amountTransaction",
        "countSharesUnderlying",
        "titleSecurityUnderlying",
        "descriptionResponse", "isBusinessCombinationTransaction",
        "idCIK", "isMoreThanOneYear", "nameEntityPrevius", "listNameEntityPreviousEDGAR",
        "nameEntity", "typeEntity", "descriptionEntityTypeOther", "idFederalExemptionsExclusions",
        "typeIndustryGroup", "typeInvestmentFund", "descriptionInvestmentFund",
        "hasNonAccreditedInvestors", "countInvestorsNonAccredited",
        "countInvestorsActive", "cityEntity", "stateEntity", "descriptionStateEntity",
        "addressStreet1Entity", "addressStreet2Entity", "zipcodeEntity", "phoneNumberEntity", "listIssuerPreviousName",
        "jurisdictionOfInc", "isOverFiveYearsOld", "hasYearOfInc", "isFormedWithinFiveYears",
        "isYetToBeFormed", "rangeAgregateNetAssetValue", "rangeRevenue",
        "amountInvestmentMinimum", "amountSoldTotal", "amountOfferingTotal",
        "amountRemaining", "nameFirst", "nameLast", "nameMiddle", "relationshipEntity",
        "descriptionRelationship", "amountDollars", "isEstimate", "idCRDBroker",
        "nameBroker", "isForeignSolicitation", "idCRDRecipient",
        "nameRecipient", "stateDescription", "state", "listStatesSolicitation",
        "isAuthorizedRepresentative", "nameSignatory", "titleSignatory",
        "idForm", "codeTestOrLive", "dateFirstSale", "isYetToOccur",
        "isAmendment", "descriptionOtherType", "isDebtType", "isEquityType",
        "isMineralPropertyType", "isOptionToAcquireType", "isOtherType",
        "isPooledInvestmentFundType", "isSecurityToBeAcquiredType", "isTenantInCommonType",
        'isNotSubjectToSection16', 'addressStreet1Owner', 'addressStreet2Owner',
        "isLiveTestFlag", "isConfirmingCopyFlag", "isReturnCopyFlag", "isOverrideInternetFlag",
        "idCCC", "dateReportCalendarOrQuarter", "nameFilingManager", "addressStreet1FilingManager",
        "addressStreet2FilingManager", "cityFilingManager", "stateFilingManager",
        'descriptionStateFilingManager',
        "zipcodeFilingManager", "typeReport", "idSEC",
        "codeProvideInfoForInstruction5", "nameEntity", "titleEntity", "phoneEntity", "signatureEntity",
        "countOtherIncludedManagers", "countTableEntries", "amountValueHoldings",
        "isConfidentialOmitted", "nameIssuer", "classSecurities", "idCUSIP", "valueSecurities", "typeInvestmentDiscretion",
        "descriptionOtherManager", "codePutCall", "countSharesPrincipal", "codeSharesPrincipal", "countSharesVotingSole",
        "countSharesVotingShared", "countSharesVotingNone",

        "idSEC", "isSinceLastFiling", "codeJurisdictionOrganization",
        "yearIncorporation", "idSIC", "idIRS", "countEmployeesFullTime",
        "countEmployeesPartTime", "phoneEntity", "nameConnection", "nameIndustry",
        "amountCashEquivalents", "amountInvestmentSecurities", "amountAccountsReceivable",
        "amountPropertyPlantEquipment", "amountAssetsTotal", "amountAccountsPayable", "amountLongTermDebt",
        "amountLiabilitiesTotal", "amountStockholderEquityTotal", "amountLiabilitiesAndEquityTotal",
        "amountRevenuesTotal", "amountCostAndExpensesOfRevenue", "amountDepreciationAndAmortization",
        "amountNetIncome", "pershareEarningsBasic", "pershareEarningsDiluted",
        "nameAuditor", "nameCommonEquityClass", "amountCommonEquityOutstanding",
        "idCUSIPCommonEquity", "isCommonEquityPublic", "namePreferredEquityClass",
        "amountPreferredEquityOutstanding", "idCusipPreferrdEquity", "isdPreferredEquityPublic",
        "nameDebtSecuritiesClass", "amountOutstandingDebtSecurities", "idCUSIPDebtSecurities",
        "isDebtSecuritiesPublic", "isCertifyIfTrue", "isCertifyIfNotDisqualified",
        "codeTier1Tier2Offering", "codeFinancialStatementAuditStatus", "codeSecuritiesOfferedTypes",
        "codeOfferDelayedContinuous", "codeOfferingYearFlag", "codeOfferingAfterQualifFlag",
        "codeOfferingBestEffortsFlag", "codeSolicitationProposedOfferingFlag",
        "codeResaleSecuritiesAffiliates", "countSecuritiesOffered", "countSecuritiesOutstanding",
        "persharePrice", "amountOfferingIssuer", "amountOfferingExistingShareholdersSelling",
        "amountOfferingSold12MonthQualifiedOffering", "amountOfferingSoldConcurrent",
        "amountOfferingTotal", "nameUnderwritr",
        "amountUnderwritersFees", "nameAuditor", "amountAuditorFees",
        "nameLegal", "amountLegalFees", "namePromoter",
        "amountPromotersFees", "idCRDBroker", "amountOfferringProceedsNet",
        "descriptionResponse", "isJurisdictionsOfSecOfferedSame", "locatonJuridicationSecuritiesOffering",
        "locationDealersJuridicationSecuritiesOffering", "nameSecuritiesIssuer",
        "titleSecuritiesOffered", "amountSecuritiesIssued", "amountSecuritiesPrincipalHolder",
        "amountSecuritiesIssuedTotal", "nameSecuritiesActExemption",
        "isBadActor", "nameSalesCommissionsServiceProvider",
        "amountSalesCommissionsFees", "isJurisdictionsSecuritiesOfferingNone",
        "isUnRegisteredNone",
        "nameBlueSkyServiceProvider", "amountBlueSkyFees",
        'isTier1Tier2Offering', 'idForm', 'idForm', 'amountOfferingConsiderationBasis',
        'nameFindersFeeProvider' , 'amountFindersFee',
        'amountLoans', 'amountPropertyAndEquipment', 'amountDeposits', 'amountInterestIncomeTotal',
        'amountInterestExpenseTotal', 'descriptionOtherSecuritiesOffered',
        'commentFiling',
        "numberAssetType",
        "numberAsset",
        "numberAssetGroup",
        "dateReportPeriodBeginning",
        "dateReportPeriodEnd",
        "nameIssuer",
        "dateOriginalIssuance",
        "amountOriginalSecurity",
        "numberOriginalSecurityTerm",
        "dateSecurityMaturity",
        "numberOriginalAmortizationTerm",
        "percentageOriginalInterestRate",
        "codeAccrualType",
        "codeInterestRateType",
        "numberOriginalInterestOnlyTerm",
        "dateFirstPayment",
        "hasUnderwriting",
        "nameSecurityTitle",
        "numberDenomination",
        "nameCurrency",
        "nameTrustee",
        "numberSecFile",
        "idCIK",
        "hasCallable",
        "codePaymentFrequency",
        "hasZeroCoupon",
        "hasAssetAdded",
        "hasAssetModified",
        "amountReportPeriodBeginningAssetBalance",
        "amountReportPeriodBeginningScheduledAssetBalance",
        "amountReportPeriodScheduledPayment",
        "percentageReportPeriodInterestRate",
        "amountTotalActualPaid",
        "percentageActualInterestCollection",
        "amountActualPrincipalCollected",
        "amountActualOtherCollection",
        "amountOtherPrincipalAdjustment",
        "amountOtherInterestAdjustment",
        "amountScheduledInterest",
        "amountScheduledPrincipal",
        "amountEndReportingPeriodActualBalance",
        "amountEndReportingPeriodScheduledBalance",
        "percentageServicingFee",
        "amountServicingFlatFee",
        "codeZeroBalance",
        "dateZeroBalanceEffective",
        "numberRemainingTermToMaturity",
        "numberCurrentDelinquentStatus",
        "numberPaymentPastDueDays",
        "numberPaymentPastDue",
        "amountNextReportPeriodPaymentDue",
        "dateNextDue",
        "namePrimaryLoanServicer",
        "dateMostRecentServicingTransferReceived",
        "hasAssetSubjectToDemand",
        "codeStatusAssetSubjectToDemand",
        "amountRepurchase",
        "dateDemandResolution",
        "nameRepurchaser",
        "codeRepurchaseReplacementReason",
        "dateReportPeriodBegin",
        "codeOriginalLoanPurpose",
        "nameOriginator",
        "amountOriginalLoan",
        "dateOriginalLoanMaturity",
        "codeOriginalInterestRateType",
        "codeOriginalLienPosition",
        "amountMostRecentJuniorLoanBalance",
        "dateMostRecentJuniorLoanBalance",
        "amountMostRecentSeniorLoan",
        "dateMostRecentSeniorLoanAmount",
        "codeLoanTypeMostSeniorLien",
        "numberMostSeniorLienHybridPeriod",
        "percentageMostSeniorLienNegativeAmortizationLimit",
        "dateMostSeniorLienOrigination",
        "hasPrepaymentPenalty",
        "hasNegativeAmortization",
        "hasModification",
        "numberModification",
        "hasMortgageInsuranceRequirement",
        "hasBalloon",
        "codeCoveredHighCost",
        "codeServicerHazardInsurance",
        "amountRefinanceCashOut",
        "amountTotalOriginationDiscount",
        "hasBroker",
        "codeChannel",
        "numberNationalMortgageLicenseSystemCompany",
        "numberBuyDown",
        "numberLoanDelinquencyAdvance",
        "codeOriginationARMIndex",
        "percentageArmMargin",
        "percentageFullyIndexedRate",
        "numberInitialFixedRatePeriodHybridARM",
        "percentageInitialInterestRateDecrease",
        "percentageInitialInterestRateIncrease",
        "numberIndexLookback",
        "numberSubsequentInterestRateReset",
        "percentageLifetimeRateCeiling",
        "percentageLifetimeRateFloor",
        "percentageSubsequentInterestRateDecrease",
        "percentageSubsequentInterestRateIncrease",
        "numberSubsequentPaymentReset",
        "codeArmRound",
        "percentageArmRound",
        "hasOptionArm",
        "codePaymentMethodAfterRecast",
        "amountInitialMinimumPayment",
        "hasConvertible",
        "hasHELOC",
        "numberHELOCDraw",
        "codePrepaymentPenaltyCalculation",
        "codePrepaymentPenaltyType",
        "numberPrepaymentPenaltyTotalTerm",
        "numberPrepaymentPenaltyHardTerm",
        "amountNegativeAmortizationLimit",
        "numberNegativeAmortizationInitialRecast",
        "numberNegativeAmortizationSubsequentRecast",
        "amountNegativeAmortizationBalance",
        "numberInitialFixedPayment",
        "percentageInitialPaymentCap",
        "percentageSubsequentPaymentCap",
        "numberInitialMinimumPaymentReset",
        "numberSubsequentMinimumPaymentReset",
        "amountMinimumPayment",
        "locationGeographical",
        "codeOccupancyStatus",
        "codeMostRecentOccupancyStatus",
        "codePropertyType",
        "amountMostRecentPropertyValue",
        "codeMostRecentPropertyValueType",
        "dateMostRecentPropertyValue",
        "codeMostRecentAVMModel",
        "numberMostRecentAVMConfidence",
        "percentageOriginalCLTV",
        "percentageOriginalLTV",
        "numberOriginalObligor",
        "numberOriginalObligorCreditScore",
        "typeOriginalObligorCreditScore",
        "numberMostRecentObligorCreditScore",
        "typeMostRecentObligorCreditScore",
        "dateMostRecentObligorCreditScore",
        "codeObligorIncomeVerificationLevel",
        "hasIRSForm4506T",
        "percentageOriginatorFrontEndDTI",
        "percentageOriginatorBackEndDTI",
        "codeObligorEmploymentVerification",
        "codeObligorEmploymentLength",
        "codeObligorAssetVerification",
        "amountOriginalPledgedAssets",
        "codeQualificationMethod",
        "nameMortgageInsuranceCompany",
        "percentageMortgageInsuranceCoverage",
        "namePoolInsuranceCompany",
        "percentagePoolInsuranceStopLoss",
        "codeMortgageInsuranceCoverageType",
        "periodModificationHasReporting",
        "dateNextPaymentDue",
        "codeAdvancingMethod",
        "codeServicingAdvanceMethodology",
        "dateStopPrincipalInterestAdvancing",
        "amountReportingPeriodBeginningLoanBalance",
        "amountReportingPeriodBeginningScheduledLoanBalance",
        "amountNextReportingPeriodPaymentDue",
        "percentageReportingPeriodInterestRate",
        "percentageNextInterestRate",
        "feeamountOtherAssessedUncollectedServicer",
        "amountOtherServicingFeeRetainedByServicer",
        "amountReportingPeriodEndActualBalance",
        "amountReportingPeriodEndScheduledBalance",
        "amountReportingPeriodScheduledPayment",
        "amountActualInterestCollected",
        "amountActualOtherCollected",
        "datePaidThrough",
        "dateInterestPaidThrough",
        "amountPaidFull",
        "amountServicerAdvancedPrincipal",
        "amountServicerAdvancedRepaidPrincipal",
        "amountServicerAdvancedCumulativePrincipal",
        "amountServicerAdvanceInterest",
        "amountServicerAdvanceRepaidInterest",
        "amountServicerAdvanceCumulativeInterest",
        "amountServicerAdvanceTaxesInsurance",
        "amountServicerAdvanceRepaidTaxesInsurance",
        "amountServicerAdvanceCumulativeTaxesInsurance",
        "amountServicerAdvanceCorporate",
        "amountServicerAdvanceRepaidCorporate",
        "amountServicerAdvanceCumulativeCorporate",
        "codeMostRecentTwelveMonthHistory",
        "percentageNextResetRate",
        "dateNextPaymentChange",
        "dateNextInterestRateChange",
        "amountNextResetPayment",
        "hasExercisedArmConversionOption",
        "namePrimaryServicer",
        "nameMasterServicer",
        "nameSpecialServicer",
        "nameSubServicer",
        "hasAssetSubjectDemand",
        "codeAssetSubjectDemandStatus",
        "codeRepurchaseReplacement",
        "amountChargeOffPrincipal",
        "amountChargeOffInterest",
        "codeLossMitigationType",
        "codeMostRecentLoanModificationEvent",
        "dateMostRecentLoanModificationEffective",
        "datePostModificationMaturity",
        "codePostModificationInterestRateType",
        "codePostModificationAmortizationType",
        "percentagePostModificationInterest",
        "datePostModificationFirstPayment",
        "amountPostModificationLoanBalance",
        "amountPostModificationPrincipalInterestPayment",
        "amountTotalCap",
        "modificationIncomeVerificationHasAt",
        "percentageModificationFrontEndDebtToIncome",
        "percentageModificationBackEndDebtToIncome",
        "amountTotalDeferred",
        "amountForgivenPrincipalCumulative",
        "amountForgivenPrincipalReportingPeriod",
        "amountForgivenInterestCumulative",
        "amountForgivenInterestReportingPeriod",
        "amountActualEndingBalanceTotalDebt",
        "amountScheduledEndingBalanceTotalDebt",
        "codePostModificationARM",
        "codePostModificationARMIndex",
        "percentagePostModificationMargin",
        "numberPostModificationInterestReset",
        "datePostModificationNextReset",
        "numberPostModificationIndexLookback",
        "codePostModificationARMRounding",
        "percentagePostModificationARMRounding",
        "paymentPostModificationInitialMinimum",
        "datePostModificationNextPaymentAdjustment",
        "frequencyPostModificationARMPaymentRecast",
        "percentagePostModificationLifetimeFloor",
        "percentagePostModificationLifetimeCeiling",
        "percentagePostModificationInitialInterestRateIncrease",
        "percentagePostModificationInitialInterestRateDecrease",
        "percentagePostModificationSubsequentInterestIncrease",
        "percentagePostModificationSubsequentInterestRateDecrease",
        "percentagePostModificationPaymentCap",
        "codePostModificationPaymentMethodAfterRecast",
        "numberPostModificationARMInterestRateTeaser",
        "numberPostModificationARMPaymentTeaser",
        "hasPostModificationARMNegativeAmortization",
        "percentagePostModificationARMNegativeAmortizationCap",
        "numberPostModificationInterestOnlyTerm",
        "datePostModificationInterestOnlyLastPayment",
        "amountPostModificationBalloon",
        "hasPostModificationInterestRateStep",
        "percentagePostModificationStepInterest",
        "datePostModificationStep",
        "amountPostModificationStepPrincipalInterestPayment",
        "numberPostModificationStep",
        "percentagePostModificationMaximumFutureStepAgreement",
        "datePostModificationMaximumStepAgreementRate",
        "amountNonInterestBearingDeferredPrincipalCumulative",
        "amountNonInterestBearingDeferredPrincipalReportingPeriod",
        "amountRecoveryDeferredPrincipalReportingPeriod",
        "amountNonInterestBearingDeferredPaidFull",
        "amountNonInterestBearingDeferredInterestFeeReportingPeriod",
        "amountNonInterestBearingDeferredInterestFeeCumulative",
        "amountRecoveryDeferredInterestFeeReportingPeriod",
        "dateMostRecentForbearancePlanOrTrialModificationStart",
        "dateMostRecentForbearancePlanOrTrialModificationScheduledEnd",
        "dateMostRecentTrialModificationViolated",
        "dateMostRecentRepaymentPlanStart",
        "dateMostRecentRepaymentPlanScheduledEnd",
        "dateMostRecentRepaymentPlanViolated",
        "amountShortSaleAcceptedOffer",
        "dateMostRecentLossMitigationExit",
        "codeMostRecentLossMitigationExit",
        "dateAttorneyReferral",
        "codeForeclosureDelayReason",
        "dateForeclosureExit",
        "codeForeclosureExitReason",
        "dateNoticeOfIntent",
        "amountMostRecentAcceptedREOOffer",
        "dateMostRecentAcceptedREOOffer",
        "amountGrossLiquidationProceeds",
        "amountNetSalesProceeds",
        "amountReportingPeriodLossPassedToIssuingEntity",
        "amountCumulativeTotalLossPassedToIssuingEntity",
        "amountSubsequentRecovery",
        "hasEviction",
        "dateReoExit",
        "codeReoExitReason",
        "amountUPBLiquidation",
        "amountServicingFeesClaimed",
        "amountServicerAdvanceReimbursedPrincipal",
        "amountServicerAdvanceReimbursedInterest",
        "amountServicerAdvanceReimbursedTaxesInsurance",
        "amountServicerAdvanceReimbursedCorporate",
        "amountREOManagementFees",
        "amountCashKeyDeed",
        "amountPerformanceIncentiveFees",
        "dateMortgageInsuranceClaimFiled",
        "amountMortgageInsuranceClaim",
        "dateMortgageInsuranceClaimPaid",
        "amountMortgageInsuranceClaimPaid",
        "dateMortgageInsuranceClaimDeniedRescinded",
        "dateMarketableTitleTransfer",
        "codeNonPayStatus",
        "codeReportingAction",
        "idGroup",
        "dateReportingPeriodBeginning",
        "dateReportingPeriodEnd",
        "dateOrigination",
        "numberOriginalTermLoan",
        "dateMaturity",
        "percentageInterestRateSecuritization",
        "codeInterestAccrualMethod",
        "dateFirstLoanPaymentDue",
        "codeLienPositionSecuritization",
        "codeLoanStructure",
        "codePaymentType",
        "amountPeriodicPrincipalAndInterestPaymentSecuritization",
        "amountScheduledPrincipalBalanceSecuritization",
        "securitizationNumberProperties",
        "propertiesNumber",
        "numberGraceDaysAllowed",
        "hasInterestOnly",
        "hasPrepaymentPremium",
        "hasModified",
        "codeArmIndex",
        "dateFirstRateAdjustment",
        "dateFirstPaymentAdjustment",
        "numberArmMargin",
        "percentageLifetimeRateCap",
        "percentagePeriodicRateIncreaseLimit",
        "percentagePeriodicRateDecreaseLimit",
        "amountPeriodicPaymentAdjustmentMaximum",
        "percentPeriodicPaymentAdjustmentMaximum",
        "codeRateResetFrequency",
        "codePaymentResetFrequency",
        "numberIndexLookbackDays",
        "datePrepaymentLockOutEnd",
        "dateYieldMaintenanceEnd",
        "datePrepaymentPremiumsEnd",
        "percentageMaximumNegativeAmortizationAllowed",
        "amountMaximumNegativeAmortizationAllowed",
        "amountNegativeAmortizationDeferredInterestCap",
        "amountDeferredInterestCumulative",
        "amountDeferredInterestCollected",
        "propertyProperty",
        "hasReportPeriodModification",
        "amountReportPeriodBeginningScheduleLoanBalance",
        "amountTotalScheduledPrincipalInterestDue",
        "percentageServicerTrusteeFeeRate",
        "amountUnscheduledPrincipalCollected",
        "amountReportPeriodEndActualBalance",
        "amountReportPeriodEndScheduledLoanBalance",
        "dateHyperAmortizing",
        "codeServicingAdvanceMethod",
        "hasNonRecoverability",
        "amountTotalPrincipalInterestAdvancedOutstanding",
        "amountTotalTaxesInsuranceAdvancesOutstanding",
        "amountOtherExpensesAdvancedOutstanding",
        "codePaymentStatusLoan",
        "percentageArmIndexRate",
        "dateNextInterestRateChangeAdjustment",
        "dateNextPaymentAdjustment",
        "dateMostRecentSpecialServicerTransfer",
        "dateMostRecentMasterServicerReturn",
        "amountRealizedLossToTrust",
        "codeLiquidationPrepayment",
        "dateLiquidationPrepayment",
        "amountPrepaymentPremiumYieldMaintenanceReceived",
        "codeWorkoutStrategy",
        "dateLastModification",
        "codeModification",
        "amountPostModificationPayment",
        "amountPostModificationAmortizationPeriod",
        "nameProperty",
        "addressProperty",
        "cityProperty",
        "stateProperty",
        "zipcodeProperty",
        "countyProperty",
        "numberNetRentableSquareFeet",
        "numberNetRentableSquareFeetSecuritization",
        "numberUnitsBedsRooms",
        "numberUnitsBedsRoomsSecuritization",
        "yearBuilt",
        "yearLastRenovated",
        "amountValuationSecuritization",
        "codeValuationSourceSecuritization",
        "dateValuationSecuritization",
        "amountMostRecentValuation",
        "dateMostRecentValuation",
        "codeMostRecentValuationSource",
        "percentagePhysicalOccupancySecuritization",
        "percentageMostRecentPhysicalOccupancy",
        "codePropertyStatus",
        "dateDefeasanceOptionStart",
        "codeDefeasedStatus",
        "tenantLargest",
        "numberSquareFeetLargestTenant",
        "dateLeaseExpirationLargestTenant",
        "tenantSecondLargest",
        "numberSquareFeetSecondLargestTenant",
        "dateLeaseExpirationSecondLargestTenant",
        "tenantThirdLargest",
        "numberSquareFeetThirdLargestTenant",
        "dateLeaseExpirationThirdLargestTenant",
        "dateFinancialsSecuritization",
        "dateMostRecentFinancialsStart",
        "dateMostRecentFinancialsEnd",
        "amountRevenueSecuritization",
        "amountMostRecentRevenue",
        "amountOperatingExpensesSecuritization",
        "amountOperatingExpenses",
        "amountNetOperatingIncomeSecuritization",
        "amountMostRecentNetOperatingIncome",
        "amountNetCashFlowFlowSecuritization",
        "amountMostRecentNetCashFlow",
        "codeNetOperatingIncomeNetCashFlowSecuritization",
        "codeNetOperatingIncomeNetCashFlow",
        "amountMostRecentDebtService",
        "percentageDebtServiceCoverageNetOperatingIncomeSecuritization",
        "percentageMostRecentDebtServiceCoverageNetOperatingIncome",
        "percentageDebtServiceCoverageNetCashFlowSecuritization",
        "percentageMostRecentDebtServiceCoverageNetCash",
        "codeDebtServiceCoverageSecuritization",
        "codeMostRecentDebtServiceCoverage",
        "dateMostRecentAnnualLeaseRolloverReview",
        "dateReportingPeriodEnding",
        "termOriginalLoan",
        "dateLoanMaturity",
        "codeInterestCalculationType",
        "dateOriginalFirstPayment",
        "numberGracePeriod",
        "subventedSubvented",
        "nameVehicleManufacturer",
        "nameVehicleModel",
        "codeVehicleNewUsed",
        "yearVehicleModel",
        "codeVehicleType",
        "amountVehicleValue",
        "codeVehicleValueSource",
        "typeObligorCreditScore",
        "scoreObligorCredit",
        "hasCoObligor",
        "percentagePaymentToIncome",
        "locationObligorGeographic",
        "hasReportingPeriodModification",
        "amountPaymentDueNextReportingPeriod",
        "servicerOtherServicerFeeRetainedBy",
        "amountOtherAssessedUncollectedServicerFee",
        "amountReportingPeriodActualEndBalance",
        "amountPaidTotalActual",
        "amountServicerAdvanced",
        "isDelinquent",
        "amountChargedoffPrincipal",
        "amountRecovered",
        "codeModificationType",
        "numberPaymentExtended",
        "hasRepossessed",
        "amountRepossessedProceeds",
        "dateReportingPeriodBegin",
        "costAcquisition",
        "numberOriginalLeaseTerm",
        "dateScheduledTermination",
        "periodGrace",
        "valueBaseResidual",
        "codeBaseResidualSource",
        "valueContractResidual",
        "typeLesseeCreditScore",
        "scoreLesseeCredit",
        "codeLesseeIncomeVerificationLevel",
        "codeLesseeEmploymentVerification",
        "hasCoLesseePresent",
        "locationLesseeGeographic",
        "numberRemainingTerm",
        "amountReportingPeriodSecuritizationValue",
        "rateSecuritizationDiscount",
        "amountOtherLeaseLevelServicingFeesRetained",
        "amountReportingPeriodEndingActualBalance",
        "amountReportingPeriodEndActualSecuritization",
        "namePrimaryLeaseServicer",
        "dateDemandResolution",
        "codeRepurchaseOrReplacementReason",
        "amountChargedOff",
        "extendedLease",
        "hasTermination",
        "amountExcessFee",
        "amountLiquidationProceeds",
        "detailNumberComment", "columnComment", "descriptionComment",
        'idAccessionPrevious',
        'numberItem', 'nameField', 'descriptionNotes', 'idSequence',
        "numberAmendment",
        "typeAmendmentType",
        "confDeniedExpired",
        'descriptionInformationAdditional',
        'numberFile'
      )
    )}

.filer_type_df <-
  function() {
    tibble(
      idTypeFilerOwner = c(
        'insider',
        'private' ,
        'broker_dealer',
        'transfer_agent',
        'ia',
        'msd',
        'bank',
        'inv_co'
      ),
      typeFilerOwner = c(
        'Insider',
        'Private Placement',
        'Broker Dealer',
        'Transfer Agent',
        'Investment Advisor',
        'Bank',
        'Municipal Securities Dealer',
        'Investment Company'
      )
    ) %>%
      mutate_all(str_to_upper)
  }

#' Form-D dictionary
#'
#' This function returns searchable
#' industries for parsed SEC Form-D
#' filings
#'
#' @return a \code{tibble}
#' @export
#' @import dplyr
#' @examples
#' dictionary_form_d_categories()
dictionary_form_d_categories <-
  function() {
    category_df <-
      dplyr::tibble(
        idIndustry = 1:35,
        nameIndustry = c(
          "AGRICULTURE",
          "AIRLINES AND AIRPORTS",
          "BIOTECHNOLOGY",
          "BUSINESS SERVICES",
          "COAL MINING",
          "COMMERCIAL REAL ESTATE",
          "COMMERCIAL BANKING",
          "COMPUTERS",
          "CONSTRUCTION",
          "ELECTRIC UTILITIES",
          "ENERGY CONSERVATION",
          "ENVIORNMENTAL SERVICES",
          "HEALTH INSURANCE",
          "HOSPITALS AND PHYSICIANS",
          "INSURANCE",
          "INVESTING",
          "INVESTMENT BANKING",
          "LODGING AND CONVETION",
          "MANUFACTURING",
          "OIL AND GAS",
          "OTHER",
          "OTHER BANKING AND FINANCIAL SERVICES",
          "OTHER ENERGY",
          "OTHER HEALTH CARE",
          "OTHER REAL ESTATE",
          "OTHER TECHNOLOGY",
          "OTHER TRAVEL",
          "PHARMACEUTICALS",
          "POOLED INVESTMENT FUND",
          "REITS AND FINANCE",
          "RESIDENTIAL REAL ESTATE",
          "RESTAURANTS",
          "RETAIL",
          "TELECOMMUNICATIONS",
          "TRAVEL AND TOURISM"
        ),
        codeIndustryParent = c(
          "OTHER",
          "TRAVEL",
          "HEALTH",
          "OTHER",
          "ENERGY",
          "REAL",
          "FINANCE",
          "TECH",
          "REAL",
          "ENERGY",
          "ENERGY",
          "ENERGY",
          "HEALTH",
          "HEALTH",
          "FINANCE",
          "FINANCE",
          "FINANCE",
          "TRAVEL",
          "OTHER",
          "ENERGY",
          "OTHER",
          "FINANCE",
          "ENERGY",
          "HEALTH",
          "REAL",
          "TECH",
          "TRAVEL",
          "HEALTH",
          "FINANCE",
          "REAL",
          "REAL",
          "OTHER",
          "OTHER",
          "TECH",
          "TRAVEL"
        ),
        nameIndustryParent = c(
          "OTHER",
          "TRAVEL AND LEISURE",
          "HEALTHCARE",
          "OTHER",
          "ENERGY",
          "REAL ESTATE",
          "FINANCIAL",
          "TECHNOLOGY",
          "REAL ESTATE",
          "ENERGY",
          "ENERGY",
          "ENERGY",
          "HEALTHCARE",
          "HEALTHCARE",
          "FINANCIAL",
          "FINANCIAL",
          "FINANCIAL",
          "TRAVEL AND LEISURE",
          "OTHER",
          "ENERGY",
          "OTHER",
          "FINANCIAL",
          "ENERGY",
          "HEALTHCARE",
          "REAL ESTATE",
          "TECHNOLOGY",
          "TRAVEL AND LEISURE",
          "HEALTHCARE",
          "FINANCIAL",
          "REAL ESTATE",
          "REAL ESTATE",
          "OTHER",
          "OTHER",
          "TECHNOLOGY",
          "TRAVEL AND LEISURE"
        )
      )
    return(category_df)
  }

.insider_code_df <-
  function() {
    insider_df <-
      tibble(
        idInsiderTransaction =
          c(
            "A",
            "C",
            "D",
            "F",
            "G",
            "H",
            "I",
            "J",
            "K",
            "L",
            "M",
            "NONE",
            "O",
            "P",
            "S",
            "U",
            "V",
            "W",
            "X",
            "Z"
          ),
        nameInsiderTransaction = c(
          "AWARD",
          "CONVEYANCE",
          "DISPOSITION TO ISSUER",
          "PAYMENT WITH SECURITIES",
          "GIFT",
          "EXPIRATION OF LONG DERIVATIVE POSITION",
          "DISCRETIONARY TRANSACTION",
          "OTHER",
          "EQUITY SWAP OR SIMILAR",
          "SMALL ACQUISITIONS",
          "EXEMPT",
          NA,
          "OTM EXERCISE",
          "PURCHASE",
          "SALE",
          "MERGER AND ACQUISITION",
          "REPORTED EARLY",
          "WILL OR LAWS OF DESCENT",
          "ITM OR ATM EXERCISE",
          "DEPOSIT INTO/WITHDRAWAL FROM VOTING TRUST"
        ),
        idTypeInsiderTransaction = c(
          "A",
          "D",
          "D",
          "D",
          "D",
          NA,
          NA,
          NA,
          NA,
          "A",
          "A",
          NA,
          "A",
          "A",
          "D",
          NA,
          NA,
          "D",
          "A",
          "D"
        )
      )
    return(insider_df)
  }

#' SEC filing code dictionary
#'
#' This function returns a
#' dictionary of SEC form filing types
#'
#' @return a \code{tibble}
#' @export
#' @import dplyr stringr
#' @family SEC
#' @family dictionary
#'
#' @examples
#' dictionary_sec_filing_codes()
dictionary_sec_filing_codes <-
  function() {
    tibble(
      idFormType = c(
        "1.01",
        "1.02",
        "1.03",
        "1.04",
        "2.01",
        "2.02",
        "2.03",
        "2.04",
        "2.05",
        "2.06",
        "3.01",
        "3.02",
        "3.03",
        "4.01",
        "4.02",
        "5.01",
        "5.02",
        "5.03",
        "5.04",
        "5.05",
        "5.06",
        "5.07",
        "5.08",
        "6.01",
        "6.02",
        "6.03",
        "6.04",
        "6.05",
        "7.01",
        "8.01",
        "9.01"
      ),
      nameFormType = c(
        "Entry into a Material Definitive Agreement",
        "Termination of a Material Definitive Agreement",
        "Bankruptcy or Receivership",
        "Mine Safety Reporting of Shutdowns and Patterns of Violations",
        "Completion of Acquisition or Disposition of Assets",
        "Results of Operations and Financial Condition",
        "Creation of a Direct Financial Obligation or an Obligation under an Off-Balance Sheet Arrangement of a Registrant",
        "Triggering Events That Accelerate or Increase a Direct Financial Obligation or an Obligation under an Off-Balance Sheet Arrangement",
        "Costs Associated with Exit or Disposal Activities",
        "Material Impairments",
        "Notice of Delisting or Failure to Satisfy a Continued Listing Rule or Standard; Transfer of Listing",
        "Unregistered Sales of Equity Securities",
        "Material Modification to Rights of Security Holders",
        "Changes in Registrant's Certifying Accountant",
        "Non-Reliance on Previously Issued Financial Statements or a Related Audit Report or Completed Interim Review",
        "Changes in Control of Registrant",
        "Departure of Directors or Certain Officers; Election of Directors; Appointment of Certain Officers; Compensatory Arrangements of Certain Officers",
        "Amendments to Articles of Incorporation or Bylaws; Change in Fiscal Year",
        "Temporary Suspension of Trading Under Registrant's Employee Benefit Plans",
        "Amendments to the Registrant's Code of Ethics, or Waiver of a Provision of the Code of Ethics",
        "Change in Shell Company Status",
        "Submission of Matters to a Vote of Security Holders",
        "Shareholder Director Nominations",
        "ABS Informational and Computational Material",
        "Change of Servicer or Trustee",
        "Change in Credit Enhancement or Other External Support",
        "Failure to Make a Required Distribution",
        "Securities Act Updating Disclosure",
        "Regulation FD Disclosure",
        "Other Events",
        "Financial Statements and Exhibits"
      ) %>% stringr::str_to_upper()
    )

  }

#' SEC form codes
#'
#' This function returns a
#' dictionary of SEC form codes
#'
#' @return a \code{tibble}
#' @export
#' @family SEC
#' @family dictionary
#'
#' @examples
#' dictionary_sec_form_codes()
dictionary_sec_form_codes <-
  function() {
    tibble(
      idForm = c(
        "R",
        "A",
        "Q",
        "CR",
        "REG",
        "REGX",
        "O",
        "P",
        "X",
        "W",
        "SEC",
        "PROXY",
        "CT",
        "IS",
        "CO",
        "T"
      ),
      nameForm = c(
        "Other Report",
        "Annual Report",
        "Quarterly Report",
        "Current Report",
        "Registration",
        "Private Offering",
        "Ownership",
        "Prospectus",
        "Exemption",
        "Withdrawal",
        "SEC Correspondence",
        "Proxy Statement",
        "Confidential Treatment",
        "Initial Statement",
        "Change in Ownership",
        "Trades"
      ) %>% stringr::str_to_upper()
    )
  }

.company_type_df <-
  function() {
    tibble(
      idCompanyType = c(
        "ic",
        "i",
        "ia",
        "bd",
        "m",
        "t",
        "b",
        "c",
        "p",
        "etf",
        "mmf",
        "mf",
        "uit",
        "cef"
      ),
      nameCompanyType = c(
        "Investment Company",
        "Insider",
        "Investment Adviser",
        "Broker-dealer",
        "Municipal Securities Dealer",
        "Transfer Agent",
        "Bank",
        "Company",
        "Private Issuer",
        "ETF",
        "Money Market Fund",
        "Mutual Fund",
        "UIT",
        "Closed-end Fund"
      )
    )
  }

#' SEC Rule dictionary
#'
#' This function retuns a
#' dictionary of SEC rules
#'
#' @return a \code{tibble}
#' @export
#' @import dplyr stringr
#'
#' @examples
#' dictionary_sec_rules()
dictionary_sec_rules <-
  function() {
    tibble(
      idRule = c(
        "06",
        "3C",
        "3C.7",
        "3C.1",
        "06b",
        "04",
        "46",
        "04.1",
        "04.2",
        "04.3",
        "05",
        "3C.6",
        "3C.5",
        "06c",
        "4a5",
        "3C.11",
        "3C.2",
        "3C.3",
        "3C.9",
        "3C.10",
        "3C.4",
        "3C.12",
        "3C.",
        "3C.14",
        "3"
      ),
      nameRule = c(
        "Rule 506",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Rule 506b",
        "Rule 504",
        "Rule 506c",
        "Rule 504b(1)(i)",
        "Rule 504b(1)(ii)",
        "Rule 504b(1)(iii)",
        "Rule 505",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Rule 506c",
        "Securities Act Section 4(a)(5)",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c",
        "Investment Company Act Section 3c"
      )
    ) %>%
      mutate_all(str_to_upper)
  }




# Company Tickers ---------------------------------------------------------



#' SEC listed public companys
#'
#' @param include_ticker_information if \code{TRUE} returns ticker information
#' @param return_message
#'
#' @return
#' @export
#' @import jsonlite dplyr purrr stringr dplyr
#' @family SEC EDGAR
#' @examples
#' edgar_tickers()
edgar_tickers <-
  function(include_ticker_information = F,
           join_sic = T,
           snake_names = F,

           return_message = T) {
    json_data <-
      "https://www.sec.gov/data/company_tickers.json" %>%
      jsonlite::fromJSON(simplifyDataFrame = TRUE, flatten = F)

    data <-
      seq_along(json_data) %>%
      map_dfr(function(x) {
        json_data[[x]] %>% flatten_dfr()
      }) %>%
      setNames(c('idCIK',
                 'idTicker',
                 "nameCompany")) %>%
      distinct() %>%
      mutate(nameCompany = nameCompany %>% str_to_upper())

    if (include_ticker_information) {
      "\n\nAcquiring ticker information\n\n" %>% cat(fill = T)
      sec_tickers_info_safe <-
        possibly(sec_tickers_info, tibble())

      df_tickers <-
        sec_tickers_info(tickers = data$idTicker, return_message = return_message, join_sic = join_sic, unformat = T, snake_names = F, convert_case = T, include_address = T)

      df_tickers <-
        df_tickers %>%
        rename(nameCompanyTicker = nameCompany,
               idCIKTicker = idCIK)

      data <-
        data %>%
        left_join(
          df_tickers, by = "idTicker"
        )

    }

    data <- data %>% munge_tbl(snake_names = snake_names)

    data
  }


# EDGAR Counts ------------------------------------------------------------

.cik_filing_count <-
  function(cik = 886982,
           return_message = TRUE) {
    code_cik  <-
      cik %>%
      pad_cik()

    url <-
      list("https://www.sec.gov/cgi-bin/srch-edgar?text=CIK%3D",
           code_cik,'&first=1994&last=',
           Sys.Date() %>% lubridate::year() %>% as.numeric()
      ) %>%
      purrr::reduce(paste0)

    page <-
      url %>%
      read_html()

    no_data <-
      page %>%
      html_nodes(css = 'p+ b') %>%
      html_text() %>% length() == 0

    if (no_data) {
      return(tibble(idCIK = cik))
    }

    filings <-
      page %>%
      html_nodes(css = 'p+ b') %>%
      html_text() %>%
      as.character() %>%
      readr::parse_number()

    pages <-
      ceiling(filings/100)

    df <-
      tibble(idCIK = cik,
             countFilings = filings,
             countPages = pages) %>%
      mutate(isMultiSearch = pages > 20)

    if (return_message) {
      list("CIK: ", cik, " has ", filings %>% formattable::comma(digits = 0), ' Filings') %>%
        purrr::reduce(paste0) %>%
        cat(fill = T)
    }
    df
  }

#' CIK Filing Counts
#'
#' @param cik CIK codes
#' @param return_message
#'
#' @return
#' @export
#'
#' @examples
cik_filing_counts <-
  function(cik, return_message = T) {
    .cik_filing_count_safe <-
      possibly(.cik_filing_count, tibble())
    cik %>%
      future_map_dfr(function(x){
        .cik_filing_count_safe(cik = x,
                               return_message = return_message)
      })
  }

.sic_filing_count <-
  function(sic = 800,
           return_message = TRUE) {
    code_sic  <-
      sic %>%
      pad_sic()

    url <-
      list("https://www.sec.gov/cgi-bin/srch-edgar?text=ASSIGNED-SIC%3D",
           code_sic,'&first=1994&last=',
           Sys.Date() %>% lubridate::year() %>% as.numeric()
      ) %>%
      purrr::reduce(paste0)

    page <-
      url %>%
      read_html()

    no_data <-
      page %>%
      html_nodes(css = 'p+ b') %>%
      html_text() %>% length() == 0

    if (no_data) {
      return(tibble(idCIK = cik))
    }

    filings <-
      page %>%
      html_nodes(css = 'p+ b') %>%
      html_text() %>%
      as.character() %>%
      readr::parse_number()

    pages <-
      ceiling(filings/100)

    df <-
      tibble(idSIC = sic,
             countFilings = filings,
             countPages = pages) %>%
      mutate(isMultiSearch = pages > 20)

    if (return_message) {
      list("SIC: ", sic, " has ", filings %>% formattable::comma(digits = 0), ' Filings') %>%
        purrr::reduce(paste0) %>%
        cat(fill = T)
    }
    df
  }



#' SIC Counts
#'
#' @param sic vector of SIC codes
#' @param join_sec if \code{TRUE} joins SIC data
#' @param use_all_sice_codes uses all SIC codes
#' @param return_message
#'
#'
#' @return
#' @export
#' @import dplyr purrr curl formattable tidyr stringr lubridate rvest httr xml2 jsonlite readr stringi
#' @examples
sic_filing_count <-
  function(sic = NULL, join_sic = T, snake_names = F,
           use_all_sice_codes = F,
           return_message = T,
           unformat = F) {
    if (use_all_sice_codes) {
      sic <-
        dictionary_sic_codes() %>%
        pull(idSIC)
    }
    if (length(sic) == 0) {
      "Enter SIC Codes"
    }
    .sic_filing_count_safe <- possibly(.sic_filing_count, tibble())

    data <-
      sic %>%
      future_map_dfr(function(x){
        .sic_filing_count_safe(sic = x, return_message = return_message)
      })

    if (join_sic) {
      data <-
        data %>%
        left_join(dictionary_sic_codes(), by = "idSIC")
    }

    data <-
      data %>%
      munge_tbl(snake_names = snake_names, unformat = F)

    data
  }


# SEC Dictionaries --------------------------------------------------------

.resolve_form_columns <-
  function(data) {
    data %>%
      mutate_if(is.character,
                funs(ifelse(. %in% c('_', "NULL"), NA, .))) %>%
      mutate_at(data %>% select(
        dplyr::matches(
          "^name|^description|^idDay|^type|^title|^description|^code|^address|^city|^state|^relationship"
        )
      ) %>% names(),
      funs(. %>% str_to_upper())) %>%
      mutate_at(data %>% select(
        dplyr::matches("^price|^count|^amount|^value|^idCIK|^yearIncorporation|^idSIC|^pershare|^number|^percent|^term|^pct|^score|^year")
      ) %>% names(),
      funs(. %>% as.character() %>% readr::parse_number())) %>%
      mutate_at(data %>% select(dplyr::matches("^is|^has")) %>% names(),
                funs(
                  ifelse(
                    . %in% c('true', 'false'),
                    . %>% as.logical(),
                    . %>% as.numeric() %>% as.logical()
                  )
                )) %>%
      mutate_at(data %>% select(dplyr::matches("^date")) %>% names(),
                funs(. %>% lubridate::ymd())) %>%
      mutate_at(data %>% select(dplyr::matches("^amountValueHoldings|^valueSecurities")) %>% names(),
                funs(. * 1000)) %>%
      suppressWarnings() %>%
      suppressMessages() %>%
      select(which(colMeans(is.na(.)) < 1))
  }


# SIC codes
# https://www.sec.gov/info/edgar/siccodes.htm

#' SIC Code dictionary
#'
#' @return
#' @export
#' @import rvest stringr dplyr purrr tidyr xml2
#' @family SEC
#' @family dictionary
#'
#'
#' @examples
#' dictionary_sic_codes()

dictionary_sic_codes <-
  memoise::memoise(function() {
    page <-
      "https://www.sec.gov/info/edgar/siccodes.htm" %>%
      read_html()

    page %>% html_table(fill = T) %>% first() %>% as_tibble() %>%
      setNames(c("idSIC", "nameOfficeAD", "nameIndustry")) %>%
      munge_tbl(convert_case = T)
  })

# Form Descriptions

#' SEC form dictionary
#'
#' @return
#' @export
#' @import dplyr purrr curl formattable tidyr stringr lubridate rvest httr xml2 jsonlite readr stringi
#' @examples
dictionary_sec_forms <-
  function() {
    page <-
      "https://www.sec.gov/forms" %>%
      read_html()

    forms <-
      page %>%
      html_nodes('.release-number-content') %>%
      html_text() %>%
      str_trim() %>%
      str_to_upper() %>%
      str_replace_all('NUMBER:', '')

    form_names <-
      page %>%
      html_nodes('.views-field-field-display-title a') %>%
      html_text() %>%
      str_to_upper() %>%
      str_trim() %>%
      str_replace_all('\r|\n|\u0092|\u0097', '') %>%
      str_replace_all('(PDF)', '') %>%
      str_replace_all('\\(', '') %>%
      str_replace_all('\\)', '') %>%
      str_trim()

    url_description_form <-
      page %>%
      html_nodes('.views-field-field-display-title a') %>%
      html_attr('href') %>%
      paste0('https://www.sec.gov', .)

    date_updated <-
      page %>%
      html_nodes('.datetime') %>%
      html_text() %>%
      list("01-", .) %>%
      purrr::reduce(paste0) %>%
      lubridate::dmy()

    sec_ids <-
      page %>%
      html_nodes('.list-page-detail-content') %>%
      html_text() %>%
      str_trim() %>%
      str_replace_all('SEC Number:', '') %>%
      str_trim()

    sec_ids[sec_ids == ''] <-
      NA


    reference <-
      page %>%
      html_nodes('td.views-field-term-node-tid') %>%
      html_text() %>%
      str_trim() %>%
      str_to_upper() %>%
      str_replace_all('\\TOPIC(S):','') %>%
      str_split('\\:') %>%
      future_map(function(x){
        x %>% str_split('\\:') %>% purrr::flatten_chr() %>% .[[2]]
      }) %>%
      purrr::flatten_chr()

    data <-
      tibble(
        idForm = forms,
        nameForm = form_names,
        urlFormDescription = url_description_form,
        dateFormUpdate = date_updated,
        idSECNumber = sec_ids,
        referenceForm = reference
      ) %>%
      arrange(desc(dateFormUpdate))


    data

  }


# General -----------------------------------------------------------------

#' Parse an EDGAR data frame for underlying tables
#'
#' @param all_data
#' @param table_name_initial
#' @param parse_all_filings
#' @param parse_complete_text_filings
#' @param parse_form_d
#' @param parse_13F
#' @param parse_small_offerings
#' @param parse_form_3_4s
#' @param parse_asset_files
#' @param parse_xbrl
#' @param assign_to_environment
#' @param nest_data
#' @param return_message
#'
#' @return
#' @export
#'
#' @examples
parse_for_tables <-
  function(all_data,
           table_name_initial = "All Filings",
           parse_all_filings = TRUE,
           parse_complete_text_filings = TRUE,
           parse_form_d = TRUE,
           parse_13F = TRUE,
           parse_small_offerings = TRUE,
           parse_form_3_4s = TRUE,
           parse_asset_files = TRUE,
           parse_xbrl = TRUE,
           assign_to_environment = FALSE,
           nest_data = TRUE,
           return_message = TRUE) {
    all_tables <-
      tibble()

    parse_form_data_safe <-
      purrr::possibly(.parse_form_data, tibble())
    parse_all_filings <-
      c(
        parse_complete_text_filings,
        parse_form_d,
        parse_13F,
        parse_small_offerings,
        parse_form_3_4s,
        parse_asset_files,
        parse_xbrl
      ) %>%
      sum() > 0
    if ('termSearch' %in% names(all_data)) {
      df_general <-
        all_data %>%
        select(termSearch, countFilings) %>%
        distinct()

      all_tables <-
        all_tables %>%
        bind_rows(tibble(nameTable = 'Summary', dataTable = list(df_general)))

      all_data <-
        all_data %>% select(-c(termSearch, countFilings))

    } else {
      all_tables <-
        all_tables %>%
        bind_rows(tibble(nameTable = 'Summary', dataTable = list(tibble())))
    }

    if (parse_all_filings) {
      all_data <-
        all_data %>%
        select(-dplyr::matches(
          "hasAssetFile|isFormD|is13F|isForm3_4|hasSmallOfferingData"
        )) %>%
        distinct()

      if (!'typeFile' %in% names(all_data)) {
        all_data <-
          all_data %>%
          mutate(typeFile = ifelse(urlSECFilingDirectory %>% str_detect('htm'),
                                   'html', NA))
      }

      search_df <-
        all_data %>%
        select(dateFiling,
               dplyr::matches("typeFile"),
               dplyr::matches("idForm"),
               urlSECFilingDirectory) %>%
        distinct()

      df_all_filings <-
        search_df$urlSECFilingDirectory %>%
        unique() %>%
        future_map_dfr(function(x){
          .parse_sec_filing_index(urls = x)
        })



      df_all_filings <-
        df_all_filings %>%
        nest(-c(idCIK, urlSECFilingDirectory, dplyr::matches("idAccession")), .key = dataFilings)

      all_data <-
        all_data %>%
        select(-dplyr::matches("dataFilings")) %>%
        left_join(df_all_filings %>% select(-one_of(c('idCIK', 'idAccession')))) %>%
        mutate(hasNoFilings = dataFilings %>% map_lgl(is_null)) %>%
        suppressMessages()

      all_tables <-
        all_tables %>%
        bind_rows(tibble(nameTable = table_name_initial, dataTable = list(all_data)))

      .all_filings <-
        all_data %>%
        filter(!hasNoFilings) %>%
        select(idCIK:typeFile, dataFilings)

      if (!'idCIKFiler' %in% names(.all_filings)) {
        .all_filings <-
          .all_filings %>%
          dplyr::rename(idCIKFiler = idCIK)
      }

      if (!'typeFileFiler' %in% names(.all_filings)) {
        .all_filings <-
          .all_filings %>%
          dplyr::rename(typeFileFiler = typeFile)
      }

      .all_filings <-
        .all_filings %>%
        select(dplyr::matches("idCIK|data")) %>%
        unnest() %>%
        distinct()

      all_tables <-
        all_tables %>%
        bind_rows(tibble(nameTable = 'All Filing URLS', dataTable = list(.all_filings)))

      if (parse_complete_text_filings) {
        if (!'urlTextFilingFull' %in% names(all_data)) {
          all_data <-
            all_data %>%
            mutate(urlTextFilingFull = urlSECFilingDirectory %>% str_replace_all("-index.htm", ".txt"))
        }
        urls <-
          all_data$urlTextFilingFull %>%
          unique()
        sec_complete_filings_safe <-
          purrr::possibly(.sec_complete_filings, tibble())
        all_text_df <-
          .sec_complete_filings(urls = urls)

        all_tables <-
          all_tables %>%
          bind_rows(tibble(nameTable = 'Text Filings', dataTable = list(all_text_df)))
      }

      if (parse_form_d) {
        df_form_ds <-
          .all_filings %>%
          parse_form_data_safe(filter_parameter = 'isFormD')
        all_tables <-
          all_tables %>%
          bind_rows(tibble(nameTable = 'FormDs', dataTable = list(df_form_ds)))
      }

      if (parse_13F) {
        df_13F <-
          .all_filings %>%
          parse_form_data_safe(filter_parameter = 'is13FFiling')
        all_tables <-
          all_tables %>%
          bind_rows(tibble(nameTable = '13Fs', dataTable = list(df_13F)))
      }

      if (parse_small_offerings) {
        df_small_offerings <-
          .all_filings %>%
          parse_form_data_safe(filter_parameter = 'hasSmallOfferingData')
        all_tables <-
          all_tables %>%
          bind_rows(tibble(
            nameTable = 'Small Offerings',
            dataTable = list(df_small_offerings)
          ))
      }

      if (parse_form_3_4s) {
        df_form3_4 <-
          .all_filings %>%
          parse_form_data_safe(filter_parameter = 'isForm3_4')

        all_tables <-
          all_tables %>%
          bind_rows(tibble(nameTable = 'Form 3 and 4', dataTable = list(df_form3_4)))
      }

      if (parse_asset_files) {
        df_assets <-
          .all_filings %>%
          .parse_form_data(filter_parameter = 'hasAssetFile')

        all_tables <-
          all_tables %>%
          bind_rows(tibble(nameTable = 'Asset Data', dataTable = list(df_assets)))
      }

      if (parse_xbrl) {
        df_xbrl <-
          .all_filings %>%
          parse_form_data_safe(filter_parameter = 'isXBRLInstanceFile')

        all_tables <-
          all_tables %>%
          bind_rows(tibble(nameTable = 'XBRL', dataTable = list(df_xbrl)))
      }

    } else {
      all_tables <-
        all_tables %>%
        bind_rows(tibble(nameTable = 'TermsFilings', dataTable = list(all_data)))

    }

    all_tables <-
      all_tables %>%
      mutate(countCols = dataTable %>% map_dbl(ncol)) %>%
      filter(countCols > 0) %>%
      select(-countCols)

    if (assign_to_environment) {
      table_name_df <-
        all_tables %>%
        select(nameTable) %>%
        distinct() %>%
        mutate(
          nameDF =
            list('data', nameTable %>% str_replace_all('\\ ', ''), 'EDGAR') %>% purrr::invoke(paste0, .)
        )

      1:nrow(table_name_df) %>%
        walk(function(x) {
          df_data <-
            all_tables %>%
            slice(x) %>%
            select(dataTable) %>%
            unnest()

          df_name <-
            table_name_df %>% slice(x) %>% .$nameDF

          df_data <-
            df_data %>%
            mutate_at(.vars =
                        df_data %>% select(dplyr::matches("^amount|^price|^value")) %>% names(),
                      funs(. %>% formattable::currency(digits = 2))) %>%
            mutate_at(
              .vars =
                df_data %>% select(dplyr::matches("^count[A-Z]|^number")) %>% select(-dplyr::matches("country")) %>% names(),
              funs(. %>% as.numeric() %>%  formattable::comma(digits = 0))
            ) %>%
            mutate_at(
              .vars = df_data %>% select(dplyr::matches("^percent|^pct")) %>% select(-dplyr::matches("country")) %>% names(),
              funs(. %>% as.numeric() %>% formattable::percent(digits = 0))
            ) %>%
            select(which(colMeans(is.na(.)) < 1)) %>%
            tidy_column_formats()

          assign(x = df_name,
                 eval(df_data),
                 envir = .GlobalEnv)
        })
    }
    return(all_tables)
  }


# SEC Free Text Search ----------------------------------------------------


.generate_ft_search_urls <-
  function(search_term = c('"Rockwood Capital"'),
           return_message = TRUE) {
    term <-
      search_term %>%
      URLencode()

    base_url <-
      list("https://searchwww.sec.gov/EDGARFSClient/jsp/EDGAR_MainAccess.jsp?search_text=", term, "&sort=Date&startDoc=0&numResults=100&isAdv=true&formType=1&fromDate=mm/dd/yyyy&toDate=mm/dd/yyyy&stemming=true") %>%
      purrr::reduce(paste0)

    page <-
      base_url %>%
      read_html()

    page_total <-
      page %>%
      html_nodes('#header .normal+ .normalbold') %>%
      html_text() %>%
      as.character() %>%
      readr::parse_number() %>%
      max(na.rm = TRUE)

    length_out <-
      ceiling(page_total/100)

    times <-
      seq(0,by = 100, length.out = length_out)

    urls <-
      list("https://searchwww.sec.gov/EDGARFSClient/jsp/EDGAR_MainAccess.jsp?search_text=", term, "&sort=Date&startDoc=", times,"&numResults=100&isAdv=true&formType=1&fromDate=mm/dd/yyyy&toDate=mm/dd/yyyy&stemming=true") %>%
      purrr::reduce(paste0)
    if (return_message) {
      glue("Found SEC free text urls for {search_term}") %>% cat()
    }
    tibble(termSearch = search_term, urlSECSearch = urls)
  }

.parse_ft_filing_page <-
  function(urls, return_message = TRUE) {
    df <-
      tibble()
    success <- function(res) {
      if (return_message) {
        list("Parsing: ", res$url) %>% purrr::reduce(paste0) %>% cat(fill = T)
      }
      page <-
        res$content %>%
        read_html()
      search_url <-
        res$url
      dates <-
        page %>%
        html_nodes('i.blue') %>%
        html_text() %>%
        lubridate::mdy()

      search_items <-
        page %>%
        html_nodes('.infoBorder+ tr td+ td #viewFiling') %>%
        html_text() %>%
        str_trim() %>%
        str_to_upper()

      urlFiling <-
        page %>%
        html_nodes('.infoBorder+ tr td+ td #viewFiling') %>%
        html_attr('href') %>%
        str_replace_all("javascript:opennew",'') %>%
        str_replace_all("'|\\(",'') %>%
        map_chr(function(x){
          x %>%
            str_split('\\,') %>%
            flatten_chr() %>%
            .[[1]]
        })

      ciks <-
        urlFiling %>%
        map_dbl(function(x){
          x %>% str_replace_all('http://www.sec.gov/Archives/edgar/data/','') %>%
            str_split('/') %>%
            flatten_chr() %>%
            .[[1]] %>%
            as.character() %>%
            readr::parse_number()
        })

      text <-
        page %>%
        html_nodes('.small') %>%
        html_text() %>%
        str_to_upper()

      data <-
        tibble(
          dateFiling = dates[seq_along(ciks)],
          idCIKFiler = ciks,
          nameFilerFilingExhibit = search_items,
          descriptionText = text[seq_along(ciks)],
          urlSECFiling = urlFiling
        ) %>%
        tidyr::separate(nameFilerFilingExhibit, sep = '\\ FOR ',
                        into = c('exhibitFiling', 'nameFiler'), remove = FALSE) %>%
        tidyr::separate(exhibitFiling, sep = '\\ OF ',
                        into = c('idExhibit', 'idForm'), remove = TRUE) %>%
        suppressWarnings()

      data <-
        data %>%
        mutate(idForm = ifelse(idForm %>% is.na(), idExhibit, idForm),
               idExhibit = ifelse(idForm == idExhibit, NA, idExhibit),
               urlSECSearch = search_url) %>%
        select(dateFiling, idCIKFiler, nameFiler, idForm, idExhibit, everything()) %>%
        suppressWarnings() %>%
        suppressMessages()

      df <<-
        df %>%
        bind_rows(data)
    }
    failure <- function(msg){
      tibble()
    }
    urls %>%
      walk(function(x){
        curl_fetch_multi(url = x, success, failure)
      })
    multi_run()
    df
  }

#' Title
#'
#' @param search_terms
#' @param nest_data
#' @param return_message
#' @import dplyr purrr curl formattable tidyr stringr lubridate rvest httr xml2 jsonlite readr stringi
#' @return
#' @export
#' @examples
#' edgar_ft_terms(search_terms = c('"Jared Kushner"', '"EJF Capital"', '"Blackstone Real Estate"'))

edgar_ft_terms <-
  function(search_terms = c('"Jared Kushner"', '"EJF Capital"', '"Blackstone Real Estate"'),
           include_counts = F,
           nest_data = FALSE,
           return_message = TRUE) {
    .generate_ft_search_urls_safe <-
      purrr::possibly(.generate_ft_search_urls, tibble())

    df_urls <-
      search_terms %>%
      future_map_dfr(function(x) {
        .generate_ft_search_urls_safe(search_term = x)
      })

    all_data <-
      .parse_ft_filing_page(urls = df_urls$urlSECSearch, return_message = return_message) %>%
      left_join(df_urls) %>%
      select(termSearch, everything()) %>%
      suppressMessages() %>%
      arrange(desc(termSearch), desc(dateFiling)) %>%
      find_target_filings()

    if (include_counts) {
      .cik_filing_count_safe <-
        purrr::possibly(.cik_filing_count, tibble())
      df_counts <-
        all_data %>%
        pull(idCIKFiler) %>%
        unique() %>%
        future_map_dfr(function(x){
          .cik_filing_count_safe(cik = x)
        })

      all_data <-
        all_data %>%
        left_join(df_counts %>%
                    dplyr::rename(idCIKFiler = idCIK) %>%
                    select(idCIKFiler, countFilings)) %>%
        select(termSearch:idCIKFiler, countFilings, everything()) %>%
        suppressMessages() %>%
        arrange(dateFiling, termSearch)
    }

    if (return_message) {
      results <-
        all_data %>% group_by(termSearch) %>% count(termSearch) %>% mutate(n = n %>% formattable::comma(digits = 0)) %>%
        unite(termMessage, termSearch, n, sep = ': ') %>%
        .$termMessage

      list(
        "\nSEC free text search filing mentions in the last 4 years:\n",
        results %>% paste0(collapse = '\n')
      ) %>%
        purrr::reduce(paste0) %>% cat(fill = T)
    }

    if (nest_data) {
      all_data <-
        all_data %>%
        nest(-c(termSearch), .key = dataFilings)
    }

    return(all_data)
  }

# Boolean Archive Search --------------------------------------------------
.sec_parameter_df <- function() {
  tibble(
    nameParameter = c(
      "Company Name",
      "Company CIK",
      "Public Document Count",
      "Accession Number",
      "Form Type",
      "Period",
      "Filing Date",
      "Company Name Confirmed",
      "CIK",
      "SIC",
      "IRS Number",
      "State of Incorporation",
      "Fiscal Year End",
      "Form Type Exact",
      "SEC Act",
      "File Number",
      "Business Address",
      "Mailing Address",
      "Former Company Name",
      "Date of Company Name Change",
      "Company",
      "form"
    ),
    slugParameter = c(
      "company-name",
      "company-cik",
      "Public-Document-Count",
      "Accession-Number",
      "type",
      "period",
      "Filing-Date",
      "Company-Name-Confirmed",
      "cik",
      "ASSIGNED-SIC",
      "irs-number",
      "STATE-OF-INCORPORATION",
      "Fiscal-Year-End",
      "Form-Type",
      "Act",
      "File-Number",
      "Business-Address",
      "Mailing-Address",
      "FORMER-CONFORMED-NAME",
      "DATE-CHANGED",
      'company-name',
      "type"
    )
  )
}


# https://www.sec.gov/edgar/searchedgar/edgarzones.htm
# https://www.sec.gov/cgi-bin/srch-edgar?ASSIGNED-SIC%3D0800&first=1994&last=2017
# https://www.sec.gov/cgi-bin/srch-edgar?text=Rockwood&start=1901&count=100&first=1994&last=2017
# https://www.sec.gov/edgar/searchedgar/search_help.htm
.parse_boolean_search_page <-
  function(urls, return_message = TRUE) {
    df <-
      tibble()
    success <- function(res){
      if (return_message) {
        list("Parsing: ", res$url, "\n") %>% purrr::reduce(paste0) %>% cat(fill = T)
      }
      page <-
        res$content %>%
        read_html()

      use_url <-
        page %>%
        html_nodes('div td:nth-child(2) a') %>%
        html_text() %>%
        str_to_upper() %>% length() == 0
      if (use_url) {
        page <-
          res$url %>%
          read_html()

      }

      entities <-
        page %>%
        html_nodes('div td:nth-child(2) a') %>%
        html_text() %>%
        str_to_upper()

      stems <-
        page %>%
        html_nodes('div td:nth-child(2) a') %>%
        html_attr('href')

      if (stems %>% length() > 0 ) {
        data <-
          seq_along(stems) %>%
          future_map_dfr(function(x){
            stem <-
              stems[[x]]

            url_filing <-
              'https://www.sec.gov' %>% paste0(stem)

            items <-
              stem %>%
              str_replace_all('/Archives/edgar/data/','') %>%
              str_split('/') %>%
              flatten_chr()

            cik <-
              items[[1]] %>% as.numeric()

            accession <-
              items[length(items)]

            is_html <-
              accession %>% str_detect(".htm|.html")

            tibble(idRow = x, idCIK = cik,
                   isHTML = is_html,
                   slugAccension = accession,
                   urlSECFilingDirectory = url_filing)

          })
      } else {
        data <-
          tibble(idRow = x, idCIK = NA)
      }

      form <-
        page %>%
        html_nodes('td:nth-child(4)') %>%
        html_text() %>%
        str_to_upper()

      if (!length(form) == nrow(data)) {
        form <-
          form[2:length(form)]
      }

      date_filing <-
        page %>%
        html_nodes('td:nth-child(5)') %>%
        html_text() %>%
        lubridate::mdy()

      file_size <-
        page %>%
        html_nodes('td:nth-child(6)') %>%
        html_text() %>%
        as.character() %>%
        readr::parse_number()

      data <-
        data %>%
        mutate(
          nameEntityLegal = entities,
          idForm = form,
          dateFiling = date_filing,
          sizeFile = file_size
        ) %>%
        resolve_legal_name() %>%
        select(-idRow) %>%
        select(dateFiling, idCIK, nameEntity, idForm, everything()) %>%
        find_target_filings()
      search_url <-
        res$url
      data <-
        data %>%
        separate(slugAccension,
                 sep = '\\.',
                 into = c('idAccession', 'typeFile'),
                 extra = "merge",
                 fill = "right"
        ) %>%
        mutate(idAccession = idAccession %>% str_replace_all('-index', '')) %>%
        separate(
          idAccession,
          into = c('idCIKFilerSubmission', 'codeYear', 'countFilerYearFilings'),
          sep = '\\-',
          remove = FALSE
        ) %>%
        mutate_at(
          c('idCIKFilerSubmission', 'codeYear', 'countFilerYearFilings'),
          funs(. %>% as.character() %>% readr::parse_number())
        ) %>%
        suppressMessages() %>%
        suppressWarnings() %>%
        mutate(
          urlSECSearch = search_url,
          isSameFiler = ifelse(idCIK == idCIKFilerSubmission, TRUE, FALSE),
          urlTextFilingFull = ifelse(
            typeFile %>% str_detect('htm'),
            urlSECFilingDirectory %>% str_replace_all("-index.htm", ".txt"),
            urlSECFilingDirectory
          )
        )

      df <<-
        df %>%
        bind_rows(data)
    }
    failure <- function(msg){
      tibble()
    }
    urls %>%
      walk(function(x){
        curl_fetch_multi(url = x, success, failure)
      })
    multi_run()
    df
  }



.generate_edgar_search_url <-
  function(search_term = '"Corona Virus"',
           parameter = NULL,
           year_start = NULL,
           year_end = NULL,
           page_start = 0) {
    if (length(search_term) == 0) {
      stop("Please enter a search term")
    }

    base <-
      'https://www.sec.gov/cgi-bin/srch-edgar?text='

    is_non_numeric <-
      class(search_term) != "numeric"
    if (is_non_numeric) {
      term <-
        search_term %>%
        URLencode()
    } else {
      term <-
        search_term
    }
    term <-
      term %>% str_replace_all('\\=', '%3D')
    has_parameter <-
      length(parameter) > 0
    if (has_parameter) {
      df_params <-
        .sec_parameter_df() %>%
        mutate_all(str_to_lower)

      parameter <-
        parameter %>% str_to_lower()

      wrong_param <-
        !parameter %in% df_params$nameParameter

      if (wrong_param) {
        stop(
          list(
            "SEC boolean search parameters can only be\n",
            paste0(df_params$nameParameter, collapse = '\n')
          ) %>% purrr::reduce(paste0)
        )
      }

      param_slug <-
        df_params %>%
        filter(nameParameter == parameter) %>%
        .$slugParameter

      if (parameter %>% str_to_lower() %in% c('cik', 'company cik')) {
        term <-
          term %>%
          pad_cik()
      }

      if (parameter %>% str_to_lower() %>% str_detect('date')) {
        term <-
          term <-
          lubridate::ymd() %>% as.character() %>% str_replace_all('\\-', '')
      }

      if (parameter %>% str_to_lower() == 'sic') {
        term <-
          term %>%
          pad_sic()
      }

      slug_term <-
        list(param_slug, '%3D', term) %>%
        purrr::reduce(paste0)
    } else {
      slug_term <-
        term
    }

    if (length(year_start) == 0) {
      year_start <-
        1994
    }

    if (length(year_end) == 0) {
      year_end <-
        Sys.Date() %>% lubridate::year() %>%
        as.numeric()
    }

    url <-
      list(base, slug_term, '&start=', page_start, '&count=100',
           '&first=', year_start, '&last=', year_end) %>%
      purrr::reduce(paste0)

    url
  }

.generate_search_term_urls <-
  function(search_term = c('"Rockwood Capital"'),
           parameter = NULL,
           year_start = NULL,
           year_end = NULL){
    url <-
      .generate_edgar_search_url(
        search_term = search_term,
        parameter = parameter,
        year_start = year_start,
        year_end = year_end,
        page_start = 0
      )

    page <-
      url %>%
      read_html()

    filings <-
      page %>%
      html_nodes(css = 'p+ b') %>%
      html_text() %>%
      as.character() %>%
      readr::parse_number()

    if (length(parameter) == 0){
      search_message <-
        search_term

    } else {
      search_message <-
        list(parameter, ' = ', search_term) %>% purrr::reduce(paste0)
    }

    pages <-
      ceiling(filings / 100)

    list('\n',filings %>% formattable::comma(digits = 0), " total filings for search term: ",
         search_message, ' to parse'
    ) %>%
      purrr::reduce(paste0) %>%
      cat(fill = T)


    page_count <-
      seq(0, by = 100, length.out = pages)

    if (page_count %>% length() == 0) {
      page_count <-
        0
    }

    urls <-
      page_count %>%
      map_chr(function(x) {
        .generate_edgar_search_url(
          search_term = search_term,
          parameter = parameter,
          year_start = year_start,
          year_end = year_end,
          page_start = x
        )
      })
    rm(page)

    df_urls <-
      tibble(termSearch = search_term,
             countFilings = filings, urlSECSearch = urls)
    return(df_urls)
  }

.sec_search_term <-
  function(search_term = "Boston Properties",
           parameter = NULL,
           year_start = NULL,
           year_end = NULL,
           return_message = TRUE){
    url_df <-
      .generate_search_term_urls(
        search_term = search_term,
        parameter = parameter,
        year_start = year_start,
        year_end = year_end
      )
    urls <-
      url_df$urlSECSearch

    all_data <-
      .parse_boolean_search_page(urls = urls)


    all_data <-
      all_data %>%
      left_join(url_df, by = "urlSECSearch") %>%
      suppressMessages() %>%
      select(termSearch, countFilings, everything())

    if (return_message) {
      list(
        "\nFound ",
        all_data %>% nrow() %>% formattable::comma(digits = 0),
        ' SEC filings for ',
        search_term,
        '\n'
      ) %>%
        purrr::reduce(paste0) %>%
        cat(fill = T)
    }

    all_data <-
      all_data %>%
      arrange(desc(dateFiling))

    all_data
  }


#' EDGAR Search for Terms
#'
#' @param search_terms
#' @param parameter
#' @param year_start
#' @param year_end
#' @param parse_all_filings
#' @param parse_form_d
#' @param parse_13F
#' @param parse_small_offerings
#' @param parse_form_3_4s
#' @param parse_asset_files
#' @param parse_xbrl
#' @param assign_to_environment
#' @param nest_data
#' @param return_message
#'
#' @return
#' @export
#' @import dplyr purrr curl formattable tidyr stringr lubridate rvest httr xml2 jsonlite readr stringi XBRL jsonlite
#' @importFrom jsonlite fromJSON
#' @examples
#' edgar_search_terms(search_terms = "China", year_start = 2020)

edgar_search_terms <-
  function(search_terms = NULL,
           parameter = NULL,
           year_start = NULL,
           year_end = NULL,
           table_name_initial = "All Filings",
           parse_all_filings = TRUE,
           parse_complete_text_filings = FALSE,
           parse_form_d = FALSE,
           parse_13F = FALSE,
           parse_small_offerings = FALSE,
           parse_form_3_4s = FALSE,
           parse_asset_files = FALSE,
           parse_xbrl = FALSE,
           assign_to_environment = TRUE,
           nest_data = TRUE,
           return_message = TRUE) {

    sec_search_term_safe <-
      purrr::possibly(.sec_search_term, tibble())

    all_data <-
      search_terms %>%
      future_map_dfr(function(x) {
        sec_search_term_safe(
          search_term = x,
          parameter = parameter,
          year_start = year_start,
          year_end = year_end
        )
      }) %>%
      dplyr::select(-dplyr::matches("urlSECSearch")) %>%
      distinct()


    if (all_data %>% nrow() == 0) {
      return(tibble())
    }

    parse_for_tables_safe <-
      purrr::possibly(parse_for_tables, tibble())

    all_tables <-
      parse_for_tables_safe(
        all_data = all_data,
        table_name_initial = table_name_initial,
        parse_all_filings = parse_all_filings,
        parse_complete_text_filings = parse_complete_text_filings,
        parse_form_d = parse_form_d,
        parse_13F = parse_13F,
        parse_small_offerings = parse_small_offerings,
        parse_form_3_4s = parse_form_3_4s,
        parse_asset_files = parse_asset_files,
        parse_xbrl = parse_xbrl,
        nest_data = nest_data,
        return_message = return_message
      )

    if (all_tables %>% nrow() == 0) {
      return(all_data)
    }

    all_tables <-
      all_tables %>%
      bind_rows(tibble(nameTable = 'Search Filings', dataTable = list(all_data)))

    if (assign_to_environment) {
      table_name_df <-
        all_tables %>%
        select(nameTable) %>%
        distinct() %>%
        mutate(
          nameDF =
            list('dataFiler', nameTable %>% str_replace_all('\\ ', '')) %>% purrr::invoke(paste0, .)
        )

      1:nrow(table_name_df) %>%
        walk(function(x) {
          df_name <-
            table_name_df %>% slice(x) %>% .$nameDF

          df_data <-
            all_tables %>%
            filter(nameTable == table_name_df$nameTable[[x]]) %>%
            select(dplyr::matches(c('idCIK|nameEntity|dataTable'))) %>%
            unnest() %>%
            suppressWarnings()

          has_unnest <-
            names(df_data) %>% str_detect('data') %>% sum(na.rm = TRUE) > 1

          if (has_unnest) {
            base_names <-
              df_data %>% select(-dplyr::matches("data")) %>% names()

            df_data_names <-
              names(df_data)[names(df_data) %>% str_detect('data')]

            for (df_data_name in df_data_names) {
              table <-
                df_data %>%
                select(one_of(c(base_names, df_data_name))) %>%
                unnest() %>%
                select(which(
                  colMeans(is.na(.)) < 1
                ))

              df_table_name <-
                list(df_name, df_data_name %>% str_replace_all('data', '')) %>% purrr::reduce(paste0)
              assign(x = df_table_name,
                     eval(table),
                     envir = .GlobalEnv)
            }

          } else {
            has_unnest <-
              df_data %>% names() %>% str_detect('data') %>% sum(na.rm = TRUE) > 0
            if (has_unnest) {
              df_data <-
                df_data %>%
                unnest()

              select_cols <- tibble(nameData = names(df_data)) %>%
                mutate(idColumn = 1:n()) %>%
                group_by(nameData) %>%
                mutate(countColumn = 1:n()) %>%
                ungroup() %>%
                filter(countColumn == min(countColumn)) %>%
                .$idColumn

              df_data <-
                df_data[, select_cols]

              table <-
                df_data %>%
                select(which(
                  colMeans(is.na(.)) < 1
                ))
              assign(x = df_name,
                     eval(table),
                     envir = .GlobalEnv)
            } else {
              table <-
                df_data %>%
                select(which(
                  colMeans(is.na(.)) < 1
                ))
              assign(x = df_name,
                     eval(table),
                     envir = .GlobalEnv)
            }
          }
        })
    }
    return(all_tables)
  }



# Most Recent Filings -----------------------------------------------------

.parse_most_recent_filing_form_page <-
  function(url = "https://www.sec.gov/cgi-bin/current?q1=0&q2=6&q3=10-D", return_message = F) {
    page <-
      url %>%
      read_html()

    data <-
      page %>%
      html_nodes(css = 'td pre') %>%
      html_text() %>%
      str_replace_all('Date Filed   Form        CIK Code     Company Name','') %>%
      read_table(col_names = FALSE) %>%
      purrr::set_names(c('dateFiling', 'idForm', 'idCIK', 'nameFiler')) %>%
      mutate(nameFiler = nameFiler %>% str_to_upper()) %>%
      suppressWarnings() %>%
      suppressMessages()

    urls <-
      page %>%
      html_nodes('pre a') %>%
      html_attr('href') %>%
      paste0('https://www.sec.gov',.)
    data_match <-
      urls %>% length() / 2 == (nrow(data))
    if (data_match) {
      data <-
        data %>%
        mutate(idRow = 1:n()) %>%
        left_join(
          tibble(urlCIKFiler = urls[c(FALSE,TRUE)] %>% paste0('&start=0&count=100'),
                 urlSECFilingDirectory = urls[c(FALSE, TRUE)]) %>%
            mutate(idRow = 1:n())

        ) %>%
        suppressWarnings() %>%
        suppressMessages()
    }

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% cat(fill = T)}
    data
  }

.parse_most_recent_stream <-
  function(url = "https://www.sec.gov/cgi-bin/browse-edgar?company=&CIK=&type=&owner=include&count=100&action=getcurrent",
           return_message = TRUE) {
    page <-
      url %>%
      xml2::read_html()

    url_directory <-
      page %>%
      html_nodes('a+ table td:nth-child(2) a:nth-child(1)') %>%
      html_attr(name = 'href') %>%
      paste0('https://www.sec.gov', .)

    urlTextFilingFull <-
      page %>%
      html_nodes('div a+ a') %>%
      html_attr(name = 'href') %>%
      paste0('https://www.sec.gov', .)

    forms <-
      page %>%
      html_nodes('a+ table td:nth-child(1)') %>%
      html_text() %>%
      str_trim()

    forms <-
      forms[!forms == '']

    url_directory <-
      page %>%
      html_nodes('a+ table td:nth-child(2) a:nth-child(1)') %>%
      html_attr('href') %>%
      paste0('https://www.sec.gov',.)

    filing_descriptions <-
      page %>%
      html_nodes('.small') %>%
      html_text() %>%
      str_trim()

    df_descriptions <-
      seq_along(filing_descriptions) %>%
      future_map_dfr(function(x){
        description <-
          filing_descriptions[[x]] %>%
          str_to_upper()

        has_act <-
          description %>% str_detect("ACT:")

        if (has_act) {
          items <-
            description %>%
            str_split("ACCESSION NUMBER: ") %>%
            flatten_chr() %>%
            str_replace_all('\n','')

          filing_description <-
            items[[1]]

          items <-
            items[[2]] %>%
            str_split('ACT:') %>%
            flatten_chr() %>%
            str_trim()

          accession <-
            items[1]

          items <-
            items[[2]] %>%
            str_split("SIZE:") %>%
            flatten_chr() %>%
            str_trim()

          df <-
            tibble(idRow = x,
                   descriptionFiling = filing_description,
                   idAccession = accession,
                   idSECAct = items[[1]],
                   descriptionFileSize = items[[2]])
          return(df)

        }

        items <-
          description %>%
          str_split("ACCESSION NUMBER: ") %>%
          flatten_chr() %>%
          str_replace_all('\n','')

        filing_description <-
          items[[1]]

        items <-
          items[[2]] %>%
          str_split('SIZE:') %>%
          flatten_chr() %>%
          str_trim()

        df <-
          tibble(idRow = x,
                 descriptionFiling = filing_description,
                 idAccession = items[[1]],
                 descriptionFileSize = items[[2]])
        return(df)
      })

    filer_description <-
      page %>%
      html_nodes('td:nth-child(3) a') %>%
      html_text()

    df_filers <-
      seq_along(filer_description) %>%
      future_map_dfr(function(x){
        filer <-
          filer_description[[x]] %>%
          str_to_upper()

        is_messed <-
          filer %>% str_count("\\(") > 2

        if (!is_messed) {
          values <-
            filer %>%
            str_split('\\(') %>%
            flatten_chr() %>%
            str_replace_all('[\\)]','') %>%
            str_trim()

          df <-
            tibble(idRow = x, item = c('nameEntityLegal', 'idCIK', 'typeSECEntity'), value = values) %>%
            spread(item, value) %>%
            mutate(idCIK = idCIK %>% as.numeric())

          df <-
            df %>%
            resolve_legal_name() %>%
            select(idCIK, nameEntity, everything())
          return(df)
        }

        if (is_messed) {
          values <-
            filer %>%
            str_split('\\(') %>%
            flatten_chr() %>%
            str_replace_all('[\\)]','') %>%
            str_trim()

          values <-
            c(list(values[1], values[2]) %>%
                purrr::reduce(paste), values[3], values[4])

          df <-
            tibble(idRow = x, item = c('nameEntityLegal', 'idCIK', 'typeSECEntity'), value = values) %>%
            spread(item, value) %>%
            mutate(idCIK = idCIK %>% as.numeric())

          df <-
            df %>%
            resolve_legal_name() %>%
            select(idCIK, nameEntity, everything())
          return(df)
        }
      })

    url_cik_filer <-
      page %>%
      html_nodes('td:nth-child(3) a') %>%
      html_attr('href') %>%
      paste0('https://www.sec.gov',.) %>%
      paste0(., '&start=0')

    datetime_accepted <-
      page %>%
      html_nodes(css = '.small+ td') %>%
      html_text() %>%
      lubridate::ymd_hms()

    date_filed <-
      page %>%
      html_nodes('td:nth-child(5)') %>%
      html_text() %>%
      lubridate::ymd()

    file_film <- page %>%
      html_nodes('td:nth-child(6)') %>%
      html_text()

    df_films <-
      seq_along(file_film) %>%
      future_map_dfr(function(x){
        parts <-
          file_film[[x]] %>% str_split('\n') %>% flatten_chr()
        tibble(
          idRow = x,
          idFile = parts[[1]] %>% stringr::str_trim(),
          idSECFiling = parts[[2]] %>% as.numeric()
        ) %>%
          mutate(
            urlSECFile = list(
              "https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&filenum=",
              idFile,
              '&owner=include&count=100000'
            ) %>% purrr::reduce(paste0)
          )
      })

    data <-
      df_filers %>%
      left_join(df_descriptions, by = "idRow") %>%
      select(-idRow) %>%
      mutate(urlTextFilingFull,
             dateFiling = date_filed,
             datetimeAccepted = datetime_accepted,
             idForm = forms,
             urlSECFilingDirectory = url_directory,
             urlCIKFIler = url_cik_filer,
             urlSearch = url) %>%
      select(idForm, everything()) %>%

      find_target_filings()

    if (df_films %>% nrow() == data %>% nrow()) {
      data <-
        data %>%
        mutate(idRow = 1:n()) %>%
        left_join(df_films, by = "idRow") %>%
        select(-idRow)
    }

    if(return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% cat(fill = T)
    }
    data <-
      data %>%
      mutate_at(
        data %>% select_if(is.character) %>% select(-dplyr::matches("url")) %>% names(),
        funs(ifelse(. == '', NA, .) %>% str_to_upper())
      )
    if ('descriptionFileSize' %in% names(data)) {
      data <-
        data %>%
        mutate(
          typeFileDocument = descriptionFileSize %>% map_chr(stringi::stri_extract_last_boundaries),
          sizeFile = readr::parse_number(as.character(descriptionFileSize)),
          sizeFileBytes = ifelse(typeFileDocument == "MB", sizeFile * 1024, 1048576 * sizeFile)
        ) %>%
        select(-c(typeFileDocument, descriptionFileSize, sizeFile))
    }

    return(data)
  }

.get_most_recent_filing_urls <-
  function(filing_type = NULL, pages_out = 20) {
    start_pages <-
      seq(0, by = 100, length.out = pages_out)
    if ('dfEnd' %>% exists()) {
      eval(rm(dfEnd))
    }

    if (length(filing_type) == 0) {
      slug_filing <-
        ''
    } else {
      if (filing_type %>% str_to_lower() == 'all') {
        slug_filing <-
          ''
      } else {
        slug_filing <-
          filing_type
      }
    }

    urls <-
      list('https://www.sec.gov/cgi-bin/browse-edgar?action=getcurrent&datea=&dateb=&company=&type=',slug_filing, '&SIC=&State=&Country=&CIK=&owner=include&accno=&start=',
           start_pages, '&count=100') %>%
      purrr::reduce(paste0)

    is_on <-
      TRUE
    for (url in urls) {
      if (!is_on) {
        invisible()
      }
      if('dfEnd' %>% exists()) {
        invisible()
      } else {
        df_end <-
          .guess_page_ongoing(url = url, override = FALSE)
        is_over_zero <-
          df_end %>% length() > 0
        if (is_over_zero) {
          assign('dfEnd', eval(df_end), envir = .GlobalEnv)
          assign('is_on', eval(FALSE), envir = .GlobalEnv)
          rm(is_over_zero)
        }
      }
    }
    still_none <-
      df_end %>% length() == 0
    if (still_none)  {
      df_end <-
        urls %>%
        future_map_dfr(function(x){
          .guess_page_ongoing(url = x, override = TRUE)
        })
      df_end <-
        df_end %>%
        slice(nrow(df_end))
    }

    if ('countPage' %in% names(df_end)) {
      df_end <-
        df_end %>%
        dplyr::rename(countStart = countPage)
    }

    if (df_end$countStart < 0) {
      df_end <-
        df_end %>%
        mutate(countStart = 0)
    }

    if (slug_filing == '') {
      df_end <-
        df_end %>%
        mutate(countStart = 2000)
    }

    length_actual_pages <-
      ceiling(df_end$countStart/100)
    length_actual <-
      seq(0, by = 100, length.out =  length_actual_pages)
    urls <-
      list('https://www.sec.gov/cgi-bin/browse-edgar?action=getcurrent&datea=&dateb=&company=&type=',slug_filing, '&SIC=&State=&Country=&CIK=&owner=include&accno=&start=',
           length_actual, '&count=100') %>%
      purrr::reduce(paste0)

    df_mr_urls <-
      tibble(urlPageFiling = urls) %>%
      mutate(countPage = 1:n())

    if (length(filing_type) > 0) {
      df_mr_urls <-
        df_mr_urls %>%
        mutate(idForm = filing_type) %>%
        select(idForm, everything())
    }
    if('dfEnd' %>% exists()){
      rm(list = c('dfEnd'), pos = ".GlobalEnv")
    }
    return(df_mr_urls)
  }


.sec_filing_most_recent <-
  function(filing_type = NULL, return_message = TRUE) {
    get_most_recent_filing_urls_safe <-
      purrr::possibly(.get_most_recent_filing_urls, tibble())
    url_df <-
      get_most_recent_filing_urls_safe(filing_type = filing_type)

    if (length(filing_type) == 0) {
      filing_name <-
        'all'
    } else {
      filing_name <-
        filing_type
    }

    parse_most_recent_stream_safe <-
      purrr::possibly(.parse_most_recent_stream, tibble())

    all_data <-
      url_df$urlPageFiling %>%
      future_map_dfr(function(x){
        parse_most_recent_stream_safe(url = x, return_message = return_message)
      }) %>%
      mutate(idFormName = filing_name) %>%
      select(idFormName, everything())

    if (return_message) {
      list("\nReturned ", all_data %>% nrow() %>% formattable::comma(digits = 0),
           ' of the most recent filings from ', filing_type, ' forms\n') %>%
        purrr::reduce(paste0) %>%
        cat(fill = T)
    }
    return(all_data)
  }

#' Most recent EDGAR filings by type
#'
#' @param forms
#' @param nest_data
#' @param return_message
#'
#' @return
#' @export
#' @import dplyr tidyr purrr stringr formattable readr lubridate XBRL curl jsonlite lazyeval
#' @importFrom jsonlite fromJSON
#' @examples
edgar_recent_filings <-
  function(forms = c("All", "10-D", "10-K"),
           table_name_initial = "Recent Filings",
           parse_all_filings = TRUE,
           parse_form_d = FALSE,
           parse_complete_text_filings = FALSE,
           parse_13F = FALSE,
           parse_small_offerings =  FALSE,
           parse_form_3_4s =  FALSE,
           parse_asset_files = FALSE,
           parse_xbrl =  FALSE,
           assign_to_environment =  TRUE,
           nest_data = FALSE,
           return_message = TRUE) {
    sec_filing_most_recent_safe <-
      purrr::possibly(.sec_filing_most_recent, tibble())

    all_data <-
      forms %>%
      future_map_dfr(function(x) {
        x %>% message()
        .sec_filing_most_recent(filing_type = x,
                                return_message = return_message)
      }) %>%
      select(dplyr::matches("dateFiling"),
             idCIK,
             nameEntity,
             idForm,
             everything())

    all_data <-
      all_data %>%
      select(-dplyr::matches("datetimeAccepted|^is[A-Z]|^has[A-Z]|is13FFiling")) %>%
      parse_for_tables(
        table_name_initial = table_name_initial,
        parse_all_filings = parse_all_filings,
        parse_complete_text_filings = parse_complete_text_filings,
        parse_form_d = parse_form_d,
        parse_13F = parse_13F,
        parse_small_offerings = parse_small_offerings,
        parse_form_3_4s = parse_form_3_4s,
        parse_asset_files = parse_asset_files,
        parse_xbrl = parse_xbrl,
        assign_to_environment = assign_to_environment,
        nest_data = nest_data,
        return_message = return_message
      )
    return(all_data)

  }

# SEC index logs ----------------------------------------------------------

.get_year_index_urls <-
  function(url = "https://www.sec.gov/Archives/edgar/daily-index/2016/") {
    yearData <-
      url %>%
      str_replace_all('https://www.sec.gov/Archives/edgar/daily-index|/','') %>%
      as.character() %>%
      readr::parse_number()

    page <-
      url %>%
      read_html()

    quarters <-
      page %>%
      html_nodes('td a') %>%
      html_attr('href') %>%
      str_replace_all('\\QTR|/','') %>%
      as.character() %>%
      readr::parse_number()

    urls <-
      page %>%
      html_nodes('td a') %>%
      html_attr('href') %>%
      list(url, .) %>%
      purrr::reduce(paste0)

    url_df <-
      tibble(idQuarter =quarters, yearData,
             urlQuarter = urls)
    return(url_df)
  }

.parse_quarter_urls <-
  function(url = "https://www.sec.gov/Archives/edgar/daily-index/2012/QTR4/",
           index_type = 'master',
           return_message = TRUE) {
    page <-
      url %>%
      read_html()

    slugs <-
      page %>%
      html_nodes('td a') %>%
      html_attr('href')

    slugs <-
      slugs[!slugs %>% str_detect("xml")]

    urls <-
      list(url, slugs) %>%
      purrr::reduce(paste0)

    df_urls <-
      tibble(slugs, urlSECIndex = urls) %>%
      tidyr::separate(slugs,
                      into = c('typeIndex', 'dateData', 'remove'),
                      sep = '\\.')

    if (df_urls$dateData[[1]] %>% lubridate::ymd() %>% is.na()) {
      df_urls <-
        df_urls %>%
        mutate(dateIndex = dateData %>% lubridate::mdy()) %>%
        select(-c(remove)) %>%
        mutate(urlQuarter = url)

    } else {
      df_urls <-
        df_urls %>%
        mutate(dateIndex = dateData %>% lubridate::ymd()) %>%
        select(-c(remove)) %>%
        mutate(urlQuarter = url)

    }


    if (length(index_type) == 0) {
      df_urls <-
        df_urls %>%
        filter(typeIndex == 'master')
    } else {
      df_urls <-
        df_urls %>%
        filter(typeIndex == index_type)
    }

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% cat(fill = T)
    }

    return(df_urls)

  }

.parse_index_filing_page <-
  function(url = "https://www.sec.gov/Archives/edgar/daily-index/1994/QTR3/company.070194.idx",
           return_message = TRUE) {
    start_skip <-
      url %>%
      read_lines() %>%
      grep('------', .)

    url_slug <-
      url %>%
      str_split('\\/') %>%
      flatten_chr() %>%
      .[length(.)] %>%
      str_replace_all('\\.idx', '') %>%
      str_split('\\.') %>%
      flatten_chr()

    index_type <-
      url_slug[[1]]

    index_date <-
      url_slug[[2]] %>%
      as.numeric() %>%
      lubridate::ymd() %>%
      suppressMessages()

    if (index_date %>% is.na()) {
      index_date <-
        url_slug[[2]] %>%
        as.numeric() %>%
        lubridate::mdy()
    }

    df <-
      url %>%
      readr::read_table(skip = start_skip,
                        col_names = FALSE,
                        progress = FALSE) %>%
      suppressMessages() %>%
      suppressWarnings()

    if (df %>% ncol() == 1) {
      df <-
        df %>%
        separate(X1, sep = '\\|',
                 c(
                   'idCIK',
                   'nameEntityLegal',
                   'idForm',
                   'dateFiling',
                   'slugAccension'
                 ))

      df <-
        df %>%
        mutate(idCIK = idCIK %>% as.numeric()) %>%
        mutate(nameEntity = nameEntityLegal %>% str_to_upper() %>% str_replace_all('\\.|\\,', '') %>% str_trim(),
               dateIndex = index_date,
               typeIndex = index_type) %>%
        separate(nameEntity,
                 sep = '\\ /',
                 into = c('nameEntity', 'idLocationEntity')) %>%
        mutate(
          dateFiling = dateFiling %>% lubridate::ymd(),
          idLocationEntity = idLocationEntity %>% str_replace_all('\\/', '') %>% str_trim()
        ) %>%
        suppressWarnings() %>%
        suppressMessages() %>%
        mutate(
          urlSECFilingText = list("https://www.sec.gov/Archives/", slugAccension) %>%
            purrr::reduce(paste0),
          urlSECFilingDirectory = urlSECFilingText %>% str_replace_all(".txt", '-index.html')
        ) %>%
        select(nameEntity, idLocationEntity, everything()) %>%
        suppressWarnings()

      df <-
        df %>%
        mutate(
          dataAccension = slugAccension %>% str_replace_all('edgar/data/|.txt', ''),
          urlSECIndex = url
        ) %>%
        tidyr::separate(dataAccension,
                        sep = '\\/',
                        into = c('remove', 'idAccession')) %>%
        select(-remove) %>%
        tidyr::separate(
          idAccession,
          into = c('idCIKFiler', 'codeYear', 'countFilerYearFilings'),
          remove = FALSE,
          sep = '\\-'
        ) %>%
        mutate_at(
          c('idCIKFiler', 'codeYear', 'countFilerYearFilings'),
          funs(. %>% as.numeric())
        ) %>%
        mutate(hasDifferentSECFiler = ifelse(!idCIK == idCIKFiler, TRUE, FALSE)) %>%
        select(
          typeIndex,
          dateIndex,
          dateFiling,
          idCIK,
          nameEntity,
          idForm,
          idAccession,
          countFilerYearFilings,
          hasDifferentSECFiler,
          everything()
        ) %>%
        suppressWarnings() %>%
        suppressMessages()

      if (return_message) {
        list("Parsed: ", url) %>%
          purrr::invoke(paste0, .) %>% cat(fill = T)
      }
      return(df)
    }


    is_form <-
      index_type == 'form'
    if (is_form) {
      if (df %>% ncol() == 6) {
        df <-
          df %>%
          tidyr::unite(X2, X2, X3, sep = ' ')
      }

      df <-
        df %>%
        purrr::set_names(c(
          'idForm',
          'nameEntityLegal',
          'idCIK',
          'dateFiling',
          'slugAccension'
        ))
    }

    if (!is_form) {
      if (df %>% ncol() == 6) {
        df <-
          df %>%
          tidyr::unite(X1, X1, X2, sep = ' ')
      }
      df <-
        df %>%
        purrr::set_names(c(
          'nameEntityLegal',
          'idForm',
          'idCIK',
          'dateFiling',
          'slugAccension'
        ))
    }

    df <-
      df %>%
      mutate(nameEntity = nameEntityLegal %>% str_to_upper() %>% str_replace_all('\\.|\\,', '') %>% str_trim(),
             dateIndex = index_date,
             typeIndex = index_type) %>%
      separate(nameEntity,
               sep = '\\ /',
               into = c('nameEntity', 'idLocationEntity')) %>%
      mutate(
        dateFiling = dateFiling %>% lubridate::ymd(),
        idLocationEntity = idLocationEntity %>% str_replace_all('\\/', '') %>% str_trim()
      ) %>%
      suppressWarnings() %>%
      suppressMessages()

    is_http <-
      df$slugAccension %>% str_count('\\.htm') %>% sum() / nrow(df) > .5

    if (is_http) {
      df <-
        df %>%
        dplyr::rename(urlSECFilingDirectory = slugAccension) %>%
        select(
          typeIndex,
          dateIndex,
          dateFiling,
          idCIK,
          nameEntity,
          everything())

      if (return_message) {
        list("Parsed: ", url) %>%
          purrr::invoke(paste0, .) %>% cat(fill = T)
      }

      return(df)
    }

    df <-
      df %>%
      mutate(
        urlSECFilingText = list("https://www.sec.gov/Archives/", slugAccension) %>%
          purrr::reduce(paste0),
        urlSECFilingDirectory = urlSECFilingText %>% str_replace_all(".txt", '-index.html')
      ) %>%
      select(nameEntity, idLocationEntity, everything()) %>%
      suppressWarnings()

    df <-
      df %>%
      mutate(
        dataAccension = slugAccension %>% str_replace_all('edgar/data/|.txt', ''),
        urlSECIndex = url
      ) %>%
      tidyr::separate(dataAccension,
                      sep = '\\/',
                      into = c('remove', 'idAccession')) %>%
      select(-remove) %>%
      tidyr::separate(
        idAccession,
        into = c('idCIKFiler', 'codeYear', 'countFilerYearFilings'),
        remove = FALSE,
        sep = '\\-'
      ) %>%
      mutate_at(
        c('idCIKFiler', 'codeYear', 'countFilerYearFilings'),
        funs(. %>% as.numeric())
      ) %>%
      mutate(hasDifferentSECFiler = ifelse(!idCIK == idCIKFiler, TRUE, FALSE)) %>%
      select(
        typeIndex,
        dateIndex,
        dateFiling,
        idCIK,
        nameEntity,
        idAccession,
        countFilerYearFilings,
        hasDifferentSECFiler,
        everything()
      ) %>%
      suppressWarnings() %>%
      suppressMessages()

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% cat(fill = T)
    }

    return(df)
  }

.get_years_page_urls <-
  function(years = 1994:2017,
           index_type = 'master',
           return_message = TRUE) {

    wrong_years <-
      (years < 1993) %>% as.numeric() %>% sum(na.rm = TRUE) > 0

    if (wrong_years) {
      stop("Years to search start in 1994")
    }

    urls <-
      list('https://www.sec.gov/Archives/edgar/daily-index/',
           years,
           '/') %>%
      purrr::reduce(paste0)

    df_urls <-
      urls %>%
      future_map_dfr(function(x) {
        .get_year_index_urls(url = x)
      })

    all_url_df <-
      df_urls$urlQuarter %>%
      future_map_dfr(function(x) {
        .parse_quarter_urls(url = x,
                           index_type = index_type,
                           return_message = return_message)
      }) %>%
      suppressWarnings()

    all_url_df <-
      all_url_df %>%
      left_join(df_urls) %>%
      select(yearData, idQuarter, everything()) %>%
      suppressMessages() %>%
      select(-dateData)

    return(all_url_df)
  }

#' SEC filing streams
#'
#' This function parses daily
#' SEC filing log starting in 1994
#' for specified periods
#'
#' @param start_date starting date in year-month-date form
#' @param end_date ending date starting in year-month-date form
#' @param only_most_recent_data \code{TRUE} return only most recent day's filing stream
#' @param index_type type of index to parse \itemize{
#' \item \code{master}: parses master log (default)
#' \item \code{compamy}: parses company log
#' \item \code{filer}: parses filer log
#' }
#' @param return_message \code{TRUE} return a message after data import
#' @param nest_data \code{TRUE} return nested data frame
#' @return nested \code{tibble} or \code{tibble} if \code{nest_data = FALSE}
#' @references \href{http://sec.gov}{The Securities and Exchange Commission}
#' @import dplyr tidyr purrr stringr formattable readr lubridate XBRL curl jsonlite lazyeval
#' @importFrom jsonlite fromJSON
#' @export
#' @family SEC
#' @family filing search
#' @examples
#' \dontrun{
#' edgar_filing_streams(start_date = "2016-01-01",
#' end_date = Sys.Date(), only_most_recent_data = FALSE, index_type = 'master',
#' nest_data = TRUE,
#' return_message = TRUE)
#' }

edgar_filing_streams <-
  function(start_date = "2017-02-15",
           end_date = Sys.Date(),
           only_most_recent_data = FALSE,
           index_type = 'master',
           table_name_initial = "Filing Logs",
           parse_all_filings = FALSE,
           parse_complete_text_filings = FALSE,
           parse_form_d = FALSE,
           parse_13F = FALSE,
           parse_small_offerings =  FALSE,
           parse_form_3_4s =  FALSE,
           parse_asset_files = FALSE,
           parse_xbrl =  FALSE,
           assign_to_environment =  TRUE,
           nest_data = TRUE,
           return_message = TRUE) {

    start_date <-
      start_date %>%
      lubridate::ymd()

    end_date <-
      end_date %>%
      lubridate::ymd()

    start_year <-
      lubridate::year(start_date)

    end_year <-
      end_date %>% lubridate::year()

    search_years <-
      start_year:end_year

    if (only_most_recent_data) {
      search_years <-
        Sys.Date() %>% lubridate::year()

      df_urls <-
        .get_years_page_urls(years = search_years,
                            index_type = index_type,
                            return_message = return_message)

      urls <-
        df_urls %>%
        slice(nrow(df_urls)) %>%
        .$urlSECIndex
    }

    if (!only_most_recent_data) {
      df_urls <-
        .get_years_page_urls(years = search_years,
                            index_type = index_type,
                            return_message = return_message)

      urls <-
        df_urls %>%
        filter(dateIndex >= start_date) %>%
        filter(dateIndex <= end_date) %>%
        .$urlSECIndex
    }

    parse_index_filing_page_safe <-
      purrr::possibly(.parse_index_filing_page, tibble())

    all_data <-
      seq_along(urls) %>%
      future_map_dfr(function(x) {
        parse_index_filing_page_safe(url = urls[[x]], return_message = return_message)
      })

    all_data <-
      all_data %>%
      left_join(df_urls %>% select(urlSECIndex, yearData, idQuarter)) %>%
      select(yearData, idQuarter, dateIndex, everything())
    all_data <-
      all_data %>%
      mutate_at(all_data %>% select(dplyr::matches("count")) %>% names(),
                funs(. %>% formattable::comma(digits = 0))) %>%
      select(-dplyr::matches("slugAccension"))

    if (return_message) {
      list(
        "Parsed ",
        all_data %>% nrow() %>% formattable::comma(digits = 0),
        " SEC filings from ",
        all_data$dateIndex %>% min(na.rm = T),
        ' to ',
        all_data$dateIndex %>% max(na.rm = TRUE)
      ) %>%
        purrr::reduce(paste0) %>%
        cat(fill = T)
    }

    all_data <-
      all_data %>%
      parse_for_tables(
        table_name_initial = table_name_initial,
        parse_all_filings = parse_all_filings,
        parse_form_d = parse_form_d,
        parse_13F = parse_13F,
        parse_small_offerings = parse_small_offerings,
        parse_form_3_4s = parse_form_3_4s,
        parse_asset_files = parse_asset_files,
        parse_xbrl = parse_xbrl,
        nest_data = nest_data,
        return_message = return_message
      )

    all_data
  }




# CIK Search --------------------------------------------------------------


.guess_page_ongoing <-
  function(url = "https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=1184765&type=&dateb=&owner=include&start=0&count=100",
           override = FALSE) {
    page <-
      url %>%
      read_html()

    page_count <-
      url %>% str_split('start=') %>%
      flatten_chr() %>%
      .[[2]] %>%
      str_split('&') %>%
      flatten_chr() %>%
      .[[1]] %>%
      as.character() %>%
      readr::parse_number()

    items <-
      page %>%
      html_nodes('input') %>%
      html_attr('value') %>% str_to_upper() %>%
      unique()

    if (items %>% length() == 0){
      return(invisible())
    }

    no_page <-
      page %>%
      html_nodes('h1') %>%
      html_text() %>%
      str_to_lower() == 'invalid parameter'

    no_page <-
      no_page %>%
      length() > 0
    is_end <-
      !items %>% str_detect("NEXT 100") %>% sum(na.rm = T) > 0

    if (is_end & (!no_page)) {
      return(tibble(isEnd = TRUE, countStart = page_count))
    }
    if (!override) {
      if (!is_end) {
        return(tibble())
      }
    } else {
      return(tibble(countStart = page_count))
    }
    tibble(isEnd = is_end, countPage = page_count -100)
  }

.parse_search_page <-
  function(urls = "https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&company=Bank&owner=exclude&match=&start=500&count=100&hidefilings=0",
           return_message = TRUE) {
    df <-
      tibble()
    success <- function(res){
      if (return_message) {
        list("Parsing: ", res$url, "\n") %>% purrr::reduce(paste0) %>% cat(fill = T)
      }
      page <-
        res$content %>%
        read_html()
      cik <-
        page %>%
        html_nodes('td:nth-child(1) a') %>%
        html_text() %>%
        as.numeric()

      entities <-
        page %>%
        html_nodes('td:nth-child(2)') %>%
        html_text() %>%
        str_to_upper()

      locations <-
        page %>%
        html_nodes('td:nth-child(3)') %>%
        html_text() %>%
        str_to_title() %>%
        str_trim()

      locations[locations == ''] <-
        NA

      data <-
        tibble(
          idCIK = cik,
          nameEntityLegal = entities,
          codeLocationBusiness = locations
        ) %>%
        mutate(codeLocationBusiness = codeLocationBusiness %>% str_to_upper()) %>%
        separate(nameEntityLegal,
                 into = c('nameEntityLegal', 'sic'),
                 sep = 'SIC: ') %>%
        separate(sic,
                 into = c('idSIC', 'nameIndustry'),
                 sep = '-') %>%
        mutate(
          idSIC = idSIC %>% str_trim() %>% as.numeric(),
          nameIndustry = nameIndustry %>% str_trim()
        ) %>%
        suppressWarnings() %>%
        suppressMessages() %>%
        select(which(colMeans(is.na(.)) < 1)) %>%
        mutate(nameEntity = nameEntityLegal %>% str_to_upper() %>% str_replace_all('\\.|\\,', '') %>% str_trim()) %>%
        select(idCIK, nameEntity, everything()) %>%
        separate(nameEntity,
                 sep = '\\ /',
                 into = c('nameEntity', 'idLocationEntity')) %>%
        mutate(
          nameEntity = nameEntity %>% gsub('/', '', .),
          idLocationEntity = idLocationEntity %>% str_replace_all('\\/', '') %>% str_trim()
        ) %>%
        suppressWarnings() %>%
        suppressMessages() %>%
        select(-dplyr::matches("idLocationEntity"))
      df <<-
        df %>%
        bind_rows(data)
    }
    failure <- function(msg){
      tibble()
    }
    urls %>%
      walk(function(x) {
        curl_fetch_multi(url = x, success, failure)
      })
    multi_run()
    df
  }

.parse_search_page_length <-
  function(search_term = "BREA", pages_out = 5) {
    term <-
      search_term %>% URLencode()

    start_pages <-
      seq(0, by = 100, length.out = pages_out)
    if ('dfEnd' %>% exists()) {
      eval(rm(dfEnd))
    }
    urls <-
      list('https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&company=',term, '&type=&dateb=&owner=include&start=',
           start_pages, '&count=100') %>%
      purrr::reduce(paste0)

    is_on <-
      TRUE
    for (url in urls) {
      if (!is_on) {
        invisible()
      }
      if('dfEnd' %>% exists()) {
        invisible()
      } else {
        df_end <-
          .guess_page_ongoing(url = url, override = FALSE)
        is_over_zero <-
          df_end %>% length() > 0
        if (is_over_zero) {
          assign('dfEnd', eval(df_end), envir = .GlobalEnv)
          assign('is_on', eval(FALSE), envir = .GlobalEnv)
          rm(is_over_zero)
        }
      }
    }
    still_none <-
      df_end %>% length() == 0
    if (still_none)  {
      df_end <-
        urls %>%
        future_map_dfr(function(x){
          .guess_page_ongoing(url = x, override = TRUE)
        })
      df_end <-
        df_end %>%
        slice(nrow(df_end))
    }

    if (df_end %>% ncol() == 0) {
      df_end <-
        tibble(countStart = 0)
    }
    length_actual_pages <-
      ceiling(df_end$countStart/100)

    length_actual <-
      seq(0, by = 100, length.out =  length_actual_pages)
    urls <-
      list('https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&company=',term, '&type=&dateb=&owner=include&start=',
           length_actual, '&count=100') %>%
      purrr::reduce(paste0)

    df_filing_urls <-
      tibble(nameSearch = search_term, urlCIKPageFiling = urls) %>%
      mutate(countPage = 1:n())
    if('dfEnd' %>% exists()){
      rm(list = c('dfEnd'), pos = ".GlobalEnv")
    }

    if ('is_on' %>% exists()) {
      rm(list = c('is_on'), pos = ".GlobalEnv")
    }
    return(df_filing_urls)
  }

.entity_ciks <-
  function(search_term = "BREA", return_message = TRUE) {
    url_df <-
      .parse_search_page_length(search_term = search_term)

    data <-
      url_df$urlCIKPageFiling %>%
      future_map_dfr(function(x) {
        .parse_search_page(url = x, return_message = FALSE)
      }) %>%
      mutate(nameSearch = search_term) %>%
      select(nameSearch, everything())

    if (return_message) {
      list("Returned ", nrow(data) %>% formattable::comma(digits = 0),
           ' SEC registered entities for the term ', search_term) %>%
        purrr::reduce(paste0) %>%
        cat(fill = T)
    }

    return(data)
  }

#' SEC registered entity search
#'
#' @param search_names
#' @param nest_data
#' @param return_message
#'
#' @return
#' @export
#' @import dplyr tidyr purrr stringr formattable readr lubridate XBRL curl jsonlite lazyeval
#' @importFrom jsonlite fromJSON
#' @examples
#' edgar_entities_cik( c("Rockwood", "BREA", 'EJF'))
edgar_entities_cik <-
  function(search_names,
           nest_data = FALSE,
           return_message = TRUE) {

    .sec_entity_safe <-
      purrr::possibly(.entity_ciks, tibble())

    all_data <-
      search_names %>%
      future_map_dfr(function(x) {
        .sec_entity_safe(search_term = x, return_message = return_message)
      }) %>%
      select(which(colMeans(is.na(.)) < 1))


    if (data %>% hasName("nameEntity")) {
      all_data <-
        all_data %>%
        mutate(nameEntity = nameEntityLegal %>% str_to_upper() %>% str_replace_all('\\.|\\,', '') %>% str_trim())
    }

    all_data <-
      all_data %>%
      select(-dplyr::matches("idLocationEntity")) %>%
      separate(nameEntity,
               sep = '\\ /',
               into = c('nameEntity', 'idLocationEntity')) %>%
      mutate(idLocationEntity = idLocationEntity %>% str_replace_all('\\/', '') %>% str_trim()) %>%
      select(nameSearch, idCIK, nameEntity, everything())



    all_data <-
      all_data %>%
      separate(nameEntity, sep = 'FORMERLY: ', c('nameEntity', 'nameEntityFormer')) %>%
      dplyr::select(which(colMeans(is.na(.)) < 1))


    if (nest_data) {
      all_data <-
        all_data %>%
        nest(-c(nameSearch), .key = dataSearch)
    }


  }


# sec_metadata  -----------------------------------------------------------



.extract_info <- function(page, css_node) {
  page %>%
    html_nodes(css = css_node) %>%
    html_text()
}

.parse_city_state <-
  function(x = "MENLO PARK CA 94025") {
    parts <- x %>% str_split('\\ ') %>% flatten_chr()
    over_2 <- parts %>% length() > 2
    if (over_2) {
      zipcode <- parts[parts %>% length()]
      state_city <- parts[!parts %in% c(zipcode)]
      state <- state_city[length(state_city)]
      city <-
        state_city[!state_city %in% state] %>% str_c(collapse = ' ')
      data <-
        tibble(
          cityCompany = city,
          stateCompany = state,
          zipcodeCompany = zipcode
        )
      return(data)
    }
    tibble()
  }

.generate_url <- function(ticker = "FB") {
  glue::glue("https://www.sec.gov/cgi-bin/browse-edgar?CIK={ticker}&owner=exclude&action=getcompany&Find=Search")
}

.parse_company_info <-
  function(url = "https://www.sec.gov/cgi-bin/browse-edgar?CIK=FB&owner=exclude&action=getcompany&Find=Search") {
    page <-
      url %>%
      read_html()

    name_parts <-
      page %>%
      .extract_info(css_node = '.companyName') %>%
      str_split('\\ CIK') %>%
      flatten_chr()

    if (length(name_parts) == 0) {
      stop("Invalid company symbol")
    }
    company_name <- name_parts[[1]]

    cik <-
      page %>% .extract_info(".companyName a") %>% str_split("\\(") %>%
      flatten_chr() %>%
      str_trim() %>%
      .[[1]]

    SIC <-
      page %>%
      .extract_info(".identInfo acronym+ a") %>%
      as.character() %>%
      readr::parse_number()

    street.address <-
      page %>%
      .extract_info(".mailer:nth-child(1) .mailerAddress:nth-child(1)")

    city.state.raw <-
      page %>%
      .extract_info(".mailer:nth-child(1) .mailerAddress+ .mailerAddress") %>%
      str_trim()
    city.state <- sub("\\s+$", "", city.state.raw)
    city.state <- gsub("\n", "", city.state)

    if (length(city.state) == 2) {
      street.address <- paste(street.address, city.state[1])
      city.state <- city.state[2]
    }
    df_city_state <-
      city.state %>% .parse_city_state() %>%
      mutate(addressStreetCompany = street.address) %>%
      dplyr::select(addressStreetCompany, everything())

    company.details <-
      page %>%
      .extract_info(".identInfo")
    fiscal.year.end <-
      gsub("^.*Fiscal Year End: ", "", company.details) %>%
      substr(1, 4)
    if (fiscal.year.end == "SIC:") {
      fiscal.year.end <- NA
    }
    state <- gsub("^.*State location: ", "", company.details) %>%
      substr(1, 2)
    state.inc <- gsub("^.*State of Inc.: ", "", company.details) %>%
      substr(1, 2)
    if (state.inc == "SI") {
      state.inc <- NA
    }
    data <-
      tibble(
        nameCompany = company_name,
        slugCIK = cik,
        idCIK = readr::parse_number(as.character(cik)),
        idSIC = SIC,
        stateIncorporated = state.inc,
        monthDayFiscalYearEnd = fiscal.year.end
      ) %>%
      bind_cols(df_city_state)
    data
  }


.parse_company_pages <-
  function(urls,
           return_message = TRUE) {
    df <-
      tibble()
    success <- function(res) {
      parse_company_info_safe <-
        purrr::possibly(.parse_company_info, tibble())

      data <-
        .parse_company_info(url = res$url)


      df <<-
        df %>%
        bind_rows(data)
    }
    failure <- function(msg) {
      cat(sprintf("Fail: %s (%s)\n", res$url, msg))
    }
    urls %>%
      walk(function(x) {
        curl_fetch_multi(url = x, success, failure)
      })
    multi_run()

    df
  }

.sec_ticker_info <-
  function(ticker = "VNO",
           return_message = TRUE) {
    if (return_message) {
      glue::glue("Acquiring company information for {ticker}") %>% cat(fill = T)
    }
    .parse_company_pages_safe <-
      purrr::possibly(.parse_company_pages, tibble())
    url <- ticker %>%
      .generate_url()

    data <-
      url %>%
      .parse_company_pages_safe() %>%
      mutate(idTicker = ticker) %>%
      dplyr::select(idTicker, everything()) %>%
      mutate_if(is.character,
                str_to_upper)

    data
  }

#' Get Ticker SEC company information
#'
#' @param tickers character vector of ticker symbols
#' @param return_message if \code{true} return a message
#'
#' @return a \code{tibble}
#' @export
#' @import curl glue dplyr purrr stringr rvest xml2
#'
#' @examples
#' sec_tickers_info(tickers = c("BXP", "AVB", "AAPL"))
sec_tickers_info <-
  function(tickers = c("VNO", "NVDA", "FB"),
           join_sic = T,
           snake_names = F,
           unformat = F,
           convert_case = T,
           amount_digits = 2,
           include_address = T,
           return_message = TRUE) {
    all_data <-
      tickers %>%
      future_map_dfr(function(x) {
        .sec_ticker_info(ticker = x, return_message = return_message)
      }) %>%
      dplyr::select(which(colMeans(is.na(.)) < 1))

    if (join_sic) {
      all_data <-
        all_data %>%
        left_join(dictionary_sic_codes(), by = "idSIC")
    }

    all_data %>%
      munge_tbl(
        snake_names = snake_names,
        unformat = unformat,
        convert_case = convert_case,
        amount_digits = amount_digits,
        include_address = include_address
      )

    all_data
  }

# page_guess --------------------------------------------------------------
.sec_filer_name_page_df <-
  function(){
    tibble(
      nameSEC = c("dateFiled", "filingHREF", "formName", "type", "XBRLREF"),
      nameActual = c(
        "dateFiling",
        "urlSECFilingDirectory",
        "nameForm",
        "idForm",
        "urlXBRL"
      )
    )
  }

.guess_page_ongoing <-
  function(url = "https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=1184765&type=&dateb=&owner=include&start=0&count=100",
           override = FALSE) {
    page <-
      url %>%
      read_html()

    page_count <-
      url %>%
      str_split('count=') %>%
      flatten_chr() %>%
      .[[2]] %>%
      str_split('&') %>%
      flatten_chr() %>%
      .[[1]] %>%
      as.character() %>%
      readr::parse_number()

    items <-
      page %>%
      html_nodes('input') %>%
      html_attr('value') %>% str_to_upper() %>%
      unique()

    if (items %>% length() == 0){
      return(invisible())
    }

    no_page <-
      page %>%
      html_nodes('h1') %>%
      html_text() %>%
      str_to_lower() == 'invalid parameter'

    no_page <-
      no_page %>%
      length() > 0
    is_end <-
      !items %>% str_detect("NEXT 100") %>% sum(na.rm = T) > 0

    if (is_end & (!no_page)) {
      return(tibble(isEnd = TRUE, countStart = page_count))
    }
    if (!override) {
      if (!is_end) {
        return(tibble())
      }
    } else {
      return(tibble(countStart = page_count))
    }
    tibble(isEnd = is_end, countPage = page_count -100)
  }


# FIler Parsing -----------------------------------------------------------


.cik_filer_page_urls <-
  function(cik = 1184765, pages_out = 20) {
    start_pages <-
      seq(0, by = 100, length.out = pages_out)
    if ('dfEnd' %>% exists()) {
      eval(rm(dfEnd))
    }
    urls <-
      list('https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=',cik, '&type=&dateb=&owner=include&start=',
           start_pages, '&count=100') %>%
      purrr::reduce(paste0)

    is_on <-
      TRUE
    for (url in urls) {
      if (!is_on) {
        invisible()
      }
      if('dfEnd' %>% exists()) {
        invisible()
      } else {
        df_end <-
          .guess_page_ongoing(url = url, override = FALSE)
        is_over_zero <-
          df_end %>% length() > 0
        if (is_over_zero) {
          assign('dfEnd', eval(df_end), envir = .GlobalEnv)
          assign('is_on', eval(FALSE), envir = .GlobalEnv)
          rm(is_over_zero)
        }
      }
    }
    still_none <-
      df_end %>% length() == 0
    if (still_none)  {
      df_end <-
        urls %>%
        future_map_dfr(function(x){
          .guess_page_ongoing(url = x, override = TRUE)
        })
      df_end <-
        df_end %>%
        slice(nrow(df_end))
    }
    length_actual_pages <-
      ceiling(df_end$countStart/100)
    length_actual <-
      seq(0, by = 100, length.out =  length_actual_pages)
    urls <-
      list('https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=',cik, '&type=&dateb=&owner=include&start=',
           length_actual, '&count=100', '&output=xml') %>%
      purrr::reduce(paste0)

    df_filing_urls <-
      tibble(idCIK = cik, urlCIKPageFiling = urls) %>%
      mutate(countPage = 1:n())
    if('dfEnd' %>% exists()){
      rm(list = c('dfEnd'), pos = ".GlobalEnv")
    }
    return(df_filing_urls)
  }

.parse_cik_filer_page <-
  function(url = "https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=899689&type=&dateb=&owner=include&start=600&count=100&output=xml",
           return_message = TRUE) {
    page <-
      url %>%
      read_xml()

    xml_nodes <-
      page %>%
      xml_contents() %>%
      .[[2]]

    filing_count <-
      xml_nodes %>%
      xml_contents() %>%
      xml_name()

    df_page_items <-
      seq_along(filing_count) %>%
      future_map_dfr(function(x) {
        xml_node <-
          xml_nodes %>%
          xml_contents() %>%
          .[[x]]

        items <-
          xml_node %>%
          xml_children() %>%
          xml_name()

        values <-
          xml_node %>%
          xml_children() %>%
          xml_text()

        return(tibble(
          countPageFiling = x,
          nameSEC = items,
          value = values
        ))
      }) %>%
      left_join(.sec_filer_name_page_df()) %>%
      suppressWarnings() %>%
      suppressMessages() %>%
      select(-nameSEC)

    df_page_items <-
      df_page_items %>%
      spread(nameActual, value) %>%
      .resolve_form_columns() %>%
      mutate(urlCIKPageFiling = url)

    df_page_items <-
      df_page_items %>%
      find_target_filings()

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% cat(fill = T)
    }

    return(df_page_items)
  }
.df_general_name_df <-
  function() {
    tibble(nameSEC = c("CIK", "CIKHREF", "Location", "SIC", "SICDescription", "SICHREF",
                       "businessAddresscity", "businessAddressphoneNumber", "businessAddressstate",
                       "businessAddressstreet", "businessAddresszipCode", "fiscalYearEnd",
                       "mailingAddresscity", "mailingAddressstate", "mailingAddressstreet",
                       "mailingAddresszipCode", "name", "stateOfIncorporation", "businessAddressstreet2", "mailingAddressstreet2", 'formerNames',
                       'businessAddress', 'formerNamedate', 'formerNamename',
                       'mailingAddress'),
           nameActual = c("idCIK", "urlCIKFiling", "locationEntity", "idSICEntity", "nameIndustry", "urlSICMembers",
                          "cityAddressBusiness", "phoneAddressBusiness", "stateAddressBusiness",
                          "addressStreet1Business", "zipcodeBusiness", "periodFiscalYearEnd",
                          "cityAddressMailing", "stateAddressMailing", "addressStreet1Mailing",
                          "zipcodeMailing", "nameEntity", "stateIncorporation",
                          "addressStreet2Mailing", "addressStreet2Business", 'nameEntity',
                          'addressBusiness', 'dateFormerName', 'nameEntity',
                          'addressMailing')

    )
  }

.parse_cik_filer_general_info <-
  function(url = "https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&CIK=1326801&type=&dateb=&owner=include&start=0&count=100&output=xml") {
    page <-
      url %>%
      read_xml()

    xml_nodes <-
      page %>%
      xml_contents() %>%
      .[[1]]

    items <-
      xml_nodes %>%
      xml_children() %>%
      xml_name()

    df_names <-
      .df_general_name_df()

    df_general <-
      seq_along(items) %>%
      future_map_dfr(function(x){

        xml_node <-
          xml_nodes %>%
          xml_contents() %>%
          .[[x]]

        item <-
          items[[x]]
        xml_search <-
          list('//', item, '/value') %>%
          purrr::reduce(paste0)

        value <-
          xml_node %>%
          xml_find_all(xml_search) %>%
          xml_text()
        no_val <-
          value %>% length() == 0

        if (item == 'formerNames') {
          value_search <-
            list('//', item) %>% purrr::reduce(paste0)

          item_parent <-
            xml_node %>%
            xml_find_all(value_search) %>%
            xml_children() %>%
            xml_name()

          items <-
            xml_node %>%
            xml_find_all(value_search) %>%
            xml_children() %>%
            xml_children() %>%
            xml_name()

          values <-
            xml_node %>%
            xml_find_all(value_search) %>%
            xml_children() %>%
            xml_children() %>%
            xml_text()

          df <-
            tibble(
              countItem = x,
              itemParent = item_parent[seq_along(item)],
              nameSEC = items,
              value = values
            ) %>%
            unite(nameSEC, itemParent, nameSEC, sep = '')
          return(df)
        }

        if (item == 'name') {
          value_search <-
            list('//', item) %>% purrr::reduce(paste0)
          items <-
            xml_node %>%
            xml_find_all(value_search) %>%
            xml_name()

          items <-
            items[length(items)]

          values <-
            xml_node %>%
            xml_find_all(value_search) %>%
            xml_text()

          values <-
            values[length(values)]

          df <-
            tibble(
              countItem = x,
              nameSEC = items,
              value = values
            )
          return(df)
        }

        if (no_val) {
          value_search <-
            list('//', item) %>% purrr::reduce(paste0)
          has_children <-
            xml_node %>%
            xml_find_all(value_search) %>% xml_children() %>% xml_length() %>% length() > 1
          if (has_children) {
            has_more_children <-
              xml_node %>%
              xml_find_all(value_search) %>%
              xml_children() %>%
              xml_children() %>% xml_length() %>% length() > 1

            if (has_more_children) {
              item_parent <-
                xml_node %>%
                xml_find_all(value_search) %>%
                xml_children() %>%
                xml_name()

              items <-
                xml_node %>%
                xml_find_all(value_search) %>%
                xml_children() %>%
                xml_children() %>%
                xml_name()

              values <-
                xml_node %>%
                xml_find_all(value_search) %>%
                xml_children() %>%
                xml_children() %>%
                xml_text()

              df <-
                tibble(
                  countItem = x,
                  itemParent = item_parent[seq_along(item)],
                  nameSEC = items,
                  value = values
                ) %>%
                unite(nameSEC, itemParent, nameSEC, sep = '')
              return(df)
            }

            item_parent <-
              xml_node %>%
              xml_find_all(value_search) %>%
              xml_name()

            item <-
              xml_node %>%
              xml_find_all(value_search) %>%
              xml_children() %>%
              xml_name()

            values <-
              xml_node %>%
              xml_find_all(value_search) %>%
              xml_children() %>%
              xml_text()

            df <-
              tibble(
                countItem = x,
                itemParent = item_parent,
                nameSEC = item,
                value = values
              ) %>%
              unite(nameSEC, itemParent, nameSEC, sep = '')
            return(df)
          }

          if (!has_children) {
            values <-
              xml_node %>%
              xml_find_all(value_search) %>%
              xml_text()
          }
          df <-
            tibble(
              countItem = x,
              nameSEC = item,
              value = values
            )
          return(df)
        }
        nameSEC <-
          xml_node %>%
          xml_find_all(list('//', item) %>% purrr::reduce(paste0)) %>%
          xml_name()
        tibble(idRow = countRow, nameSEC, value = value)
      })

    df_general <-
      df_general %>%
      left_join(df_names) %>%
      suppressMessages()

    missing_names <-
      df_general$nameSEC[!df_general$nameSEC %in% df_names$nameSEC] %>%
      length() >0

    if (missing_names) {
      missing_n <-
        df_general$nameSEC[!df_general$nameSEC %in% df_names$nameSEC]
      stop(list("Missing ", missing_n) %>%
             purrr::reduce(paste0))
    }

    df_general <-
      df_general %>%
      select(-c(nameSEC,countItem)) %>%
      group_by(nameActual) %>%
      mutate(idRow = 1:n()) %>%
      filter(idRow == min(idRow)) %>%
      ungroup() %>%
      suppressMessages()

    col_order <-
      df_general$nameActual

    df_general <-
      df_general %>%
      spread(nameActual, value) %>%
      select(one_of(col_order)) %>%
      dplyr::rename(nameEntityLegal = nameEntity) %>%
      mutate(nameEntity = nameEntityLegal %>% str_to_upper() %>% str_replace_all('\\.|\\,', '') %>% str_trim()) %>%
      separate(nameEntity,
               sep = '\\ /',
               into = c('nameEntity', 'idLocationEntity')) %>%
      mutate(
        idLocationEntity = idLocationEntity %>% str_replace_all('\\/', '') %>% str_trim()
      ) %>%
      select(nameEntity, everything()) %>%
      suppressWarnings() %>%
      suppressMessages()

    return(df_general)
  }


.cik_filer_filings <-
  function(cik = 899689) {
    df_urls <-
      .cik_filer_page_urls(cik = cik) %>%
      suppressWarnings() %>%
      suppressMessages()

    parse_cik_filer_page_safe <-
      purrr::possibly(.parse_cik_filer_page, tibble())

    df_filings <-
      df_urls$urlCIKPageFiling %>%
      future_map_dfr(function(x) {
        parse_cik_filer_page_safe(url = x)
      }) %>%
      mutate(idCIK = cik) %>%
      select(idCIK, everything())

    df_general <-
      df_urls$urlCIKPageFiling[[1]] %>%
      .parse_cik_filer_general_info() %>%
      mutate_all(funs(ifelse(. == '', NA, .))) %>%
      .resolve_form_columns() %>%
      select(which(colMeans(is.na(.)) < 1))

    df_filings <-
      df_filings %>%
      left_join(df_general %>% select(idCIK, nameEntity)) %>%
      suppressMessages()

    df_filings <-
      df_filings %>%
      select(-countPageFiling) %>%
      arrange(dateFiling) %>%
      mutate(countFilingEntity = 1:n()) %>%
      arrange(desc(dateFiling)) %>%
      select(countFilingEntity, idCIK, nameEntity, everything())

    if ('urlXBRL' %in% names(df_filings)) {
      df_filings <-
        df_filings %>%
        mutate(hasXBRL = ifelse(!urlXBRL %>% is.na(), TRUE, FALSE))
    }

    df_filings
  }

# SIC Search --------------------------------------------------------------


.sic_filer_page_urls <-
  function(sic = 6798, pages_out = 20) {
    start_pages <-
      seq(0, by = 100, length.out = pages_out)
    if ('dfEnd' %>% exists()) {
      eval(rm(dfEnd))
    }

    urls <-
      list('https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&SIC=',sic, '&type=&dateb=&owner=include&start=',
           start_pages, '&count=100') %>%
      purrr::reduce(paste0)

    is_on <-
      TRUE
    for (url in urls) {
      if (!is_on) {
        invisible()
      }
      if('dfEnd' %>% exists()) {
        invisible()
      } else {
        df_end <-
          .guess_page_ongoing(url = url, override = FALSE)
        is_over_zero <-
          df_end %>% length() > 0
        if (is_over_zero) {
          assign('dfEnd', eval(df_end), envir = .GlobalEnv)
          assign('is_on', eval(FALSE), envir = .GlobalEnv)
          rm(is_over_zero)
        }
      }
    }
    still_none <-
      df_end %>% length() == 0
    if (still_none)  {
      df_end <-
        tibble(countStart = 0)
    }
    length_actual_pages <-
      ceiling(df_end$countStart/100)
    length_actual <-
      seq(0, by = 100, length.out =  length_actual_pages)
    urls <-
      list('https://www.sec.gov/cgi-bin/browse-edgar?action=getcompany&SIC=',sic, '&type=&dateb=&owner=include&start=',
           length_actual, '&count=100', '&output=xml') %>%
      purrr::reduce(paste0)

    df_sic_urls <-
      tibble(idSIC = sic, urlSICPageFiling = urls) %>%
      mutate(countPage = 1:n())
    if('dfEnd' %>% exists()){
      rm(list = c('dfEnd'), pos = ".GlobalEnv")
    }
    return(df_sic_urls)
  }

.sic_code_filer <-
  function(sic = 6798,
           return_message = TRUE) {

    sic_filer_page_urls_safe <-
      purrr::possibly(.sic_filer_page_urls, tibble())

    url_df <-
      sic_filer_page_urls_safe(sic = sic)

    parse_search_page_safe <-
      purrr::possibly(.parse_search_page, tibble())

    all_data <-
      url_df$urlSICPageFiling %>%
      future_map_dfr(function(x){
        parse_search_page_safe(url = x, return_message = return_message)
      }) %>%
      mutate(idSIC = sic) %>%
      select(idSIC, everything())



    if (return_message) {
      list('\nReturned ', all_data %>% nrow() %>% formattable::comma(digits = 0),
           ' SEC registered entities for SIC industry code ', sic,'\n') %>%
        purrr::reduce(paste0) %>%
        cat(fill = T)
    }
    return(all_data)
  }

#' SIC Cod Companies
#'
#' @param sic_codes
#' @param merge_names
#' @param return_message
#' @param nest_data
#'
#' @return
#' @export
#' @import dplyr tidyr purrr stringr formattable readr lubridate XBRL curl jsonlite lazyeval
#' @importFrom jsonlite fromJSON
#' @examples
#' edgar_sic_filers(sic_codes = c(3949, 3690, 3711))
edgar_sic_filers <-
  function(sic_codes = NULL,
           merge_names = TRUE,
           return_message = TRUE,
           nest_data = FALSE) {

    if (length(sic_codes) == 0) {
      stop("Please enter SIC codes to search")
    }

    sic_code_filer_safe <-
      purrr::possibly(.sic_code_filer, tibble())

    all_data <-
      sic_codes %>%
      future_map_dfr(function(x){
        sic_code_filer_safe(sic = x, return_message = return_message)
      })

    if (merge_names) {
      if (!'dataSICCodes' %>% exists()) {
        assign(x = 'dataSICCodes', value = eval(dictionary_sic_codes()),
               envir = .GlobalEnv)
      }

      all_data <-
        all_data %>%
        left_join(
          dataSICCodes %>% select(-nameOfficeAD)
        ) %>%
        select(idSIC, nameIndustry, everything()) %>%
        suppressMessages()
    }
    return(all_data)
  }

# SEC - Subsidiary --------------------------------------------------------

.parse_sec_url_for_cik <-
  function(url) {
    url %>%
      str_replace_all("https://www.sec.gov/Archives/edgar/data/", '') %>%
      str_split('\\/') %>%
      flatten_chr() %>%
      .[[1]] %>%
      as.numeric()
  }

.loc_df <-
  function() {
    tibble(
      nameLocation = c(
        "AFGHANISTAN",
        "ALAND ISLANDS",
        "ALBANIA",
        "ALGERIA",
        "AMERICAN SAMOA",
        "ANDORRA",
        "ANGOLA",
        "ANGUILLA",
        "ANTARCTICA",
        "ANTIGUA AND BARBUDA",
        "ARGENTINA",
        "ARMENIA",
        "ARUBA",
        "AUSTRALIA",
        "AUSTRIA",
        "AUSTRIA-HUNGARY",
        "AZERBAIJAN",
        "BADEN",
        "BAHAMAS",
        "BAHRAIN",
        "BANGLADESH",
        "BARBADOS",
        "BAVARIA",
        "BELARUS",
        "BELGIUM",
        "BELIZE",
        "BENIN",
        "BERMUDA",
        "BHUTAN",
        "BOLIVIA, PLURINATIONAL STATE OF",
        "BONAIRE, SINT EUSTATIUS AND SABA",
        "BOSNIA AND HERZEGOVINA",
        "BOTSWANA",
        "BOUVET ISLAND",
        "BRAZIL",
        "BRITISH INDIAN OCEAN TERRITORY",
        "BRUNEI DARUSSALAM",
        "BULGARIA",
        "BURKINA FASO",
        "BURUNDI",
        "CAMBODIA",
        "CAMEROON",
        "CANADA",
        "CABO VERDE",
        "CAYMAN ISLANDS",
        "CENTRAL AFRICAN REPUBLIC",
        "CHAD",
        "CHILE",
        "CHINA",
        "CHRISTMAS ISLAND",
        "COCOS (KEELING) ISLANDS",
        "COLOMBIA",
        "COMOROS",
        "CONGO, THE DEMOCRATIC REPUBLIC OF THE",
        "CONGO",
        "COOK ISLANDS",
        "COSTA RICA",
        "COTE D'IVOIRE",
        "CROATIA",
        "CUBA",
        "CURACAO",
        "CYPRUS",
        "CZECH REPUBLIC",
        "CZECHOSLOVAKIA",
        "DENMARK",
        "DJIBOUTI",
        "DOMINICA",
        "DOMINICAN REPUBLIC",
        "ECUADOR",
        "EGYPT",
        "EL SALVADOR",
        "EQUATORIAL GUINEA",
        "ERITREA",
        "ESTONIA",
        "ETHIOPIA",
        "FALKLAND ISLANDS (MALVINAS)",
        "FAROE ISLANDS",
        "FIJI",
        "FINLAND",
        "FRANCE",
        "FRENCH GUIANA",
        "FRENCH POLYNESIA",
        "FRENCH SOUTHERN TERRITORIES",
        "GABON",
        "GAMBIA",
        "GEORGIA",
        "GERMAN DEMOCRATIC REPUBLIC",
        "FEDERAL REPUBLIC OF GERMANY",
        "GERMANY",
        "GHANA",
        "GIBRALTAR",
        "GREECE",
        "GREENLAND",
        "GRENADA",
        "GUADELOUPE",
        "GUAM",
        "GUATEMALA",
        "GUERNSEY",
        "GUINEA",
        "GUINEA-BISSAU",
        "GUYANA",
        "HAITI",
        "HANOVER",
        "HEARD ISLAND AND MCDONALD ISLANDS",
        "HESSE ELECTORAL",
        "HESSE GRAND DUCAL",
        "HOLY SEE (VATICAN CITY STATE)",
        "HONDURAS",
        "HONG KONG",
        "HUNGARY",
        "ICELAND",
        "INDIA",
        "INDONESIA",
        "IRAN, ISLAMIC REPUBLIC OF",
        "IRAQ",
        "IRELAND",
        "ISLE OF MAN",
        "ISRAEL",
        "ITALY",
        "JAMAICA",
        "JAPAN",
        "JERSEY",
        "JORDAN",
        "KAZAKHSTAN",
        "KENYA",
        "KIRIBATI",
        "KOREA",
        "KOREA, DEMOCRATIC PEOPLE'S REPUBLIC OF",
        "KOREA, REPUBLIC OF",
        "KOSOVO",
        "KUWAIT",
        "KYRGYZSTAN",
        "LAO PEOPLE'S DEMOCRATIC REPUBLIC",
        "LATVIA",
        "LEBANON",
        "LESOTHO",
        "LIBERIA",
        "LIBYA",
        "LIECHTENSTEIN",
        "LITHUANIA",
        "LUXEMBOURG",
        "MACAO",
        "MACEDONIA, THE FORMER YUGOSLAV REPUBLIC OF",
        "MADAGASCAR",
        "MALAWI",
        "MALAYSIA",
        "MALDIVES",
        "MALI",
        "MALTA",
        "MARSHALL ISLANDS",
        "MARTINIQUE",
        "MAURITANIA",
        "MAURITIUS",
        "MAYOTTE",
        "MECKLENBURG SCHWERIN",
        "MEXICO",
        "MICRONESIA, FEDERATED STATES OF",
        "MODENA",
        "MOLDOVA, REPUBLIC OF",
        "MONACO",
        "MONGOLIA",
        "MONTENEGRO",
        "MONTSERRAT",
        "MOROCCO",
        "MOZAMBIQUE",
        "MYANMAR",
        "NAMIBIA",
        "NAURU",
        "NEPAL",
        "NETHERLANDS",
        "NETHERLANDS ANTILLES",
        "NEW CALEDONIA",
        "NEW ZEALAND",
        "NICARAGUA",
        "NIGER",
        "NIGERIA",
        "NIUE",
        "NORFOLK ISLAND",
        "NORTHERN MARIANA ISLANDS",
        "NORWAY",
        "OMAN",
        "PAKISTAN",
        "PALAU",
        "PALESTINE, STATE OF",
        "PANAMA",
        "PAPUA NEW GUINEA",
        "PARAGUAY",
        "PARMA",
        "PERU",
        "PHILIPPINES",
        "PITCAIRN",
        "POLAND",
        "PORTUGAL",
        "PUERTO RICO",
        "QATAR",
        "REPUBLIC OF VIETNAM",
        "REUNION",
        "ROMANIA",
        "RUSSIAN FEDERATION",
        "RWANDA",
        "SAINT BARTHELEMY",
        "SAINT HELENA, ASCENSION AND TRISTAN DA CUNHA",
        "SAINT KITTS AND NEVIS",
        "SAINT LUCIA",
        "SAINT MARTIN (FRENCH PART)",
        "SAINT PIERRE AND MIQUELON",
        "SAINT VINCENT AND THE GRENADINES",
        "SAMOA",
        "SAN MARINO",
        "SAO TOME AND PRINCIPE",
        "SAUDI ARABIA",
        "SAXONY",
        "SENEGAL",
        "SERBIA",
        "SEYCHELLES",
        "SIERRA LEONE",
        "SINGAPORE",
        "SINT MAARTEN (DUTCH PART)",
        "SLOVAKIA",
        "SLOVENIA",
        "SOLOMON ISLANDS",
        "SOMALIA",
        "SOUTH AFRICA",
        "SOUTH GEORGIA AND THE SOUTH SANDWICH ISLANDS",
        "SOUTH SUDAN",
        "SPAIN",
        "SRI LANKA",
        "SUDAN",
        "SURINAME",
        "SVALBARD AND JAN MAYEN",
        "SWAZILAND",
        "SWEDEN",
        "SWITZERLAND",
        "SYRIAN ARAB REPUBLIC",
        "TAIWAN, PROVINCE OF CHINA",
        "TAJIKISTAN",
        "TANZANIA, UNITED REPUBLIC OF",
        "THAILAND",
        "TIMOR-LESTE",
        "TOGO",
        "TOKELAU",
        "TONGA",
        "TRINIDAD AND TOBAGO",
        "TUNISIA",
        "TURKEY",
        "TURKMENISTAN",
        "TURKS AND CAICOS ISLANDS",
        "TUSCANY",
        "TUVALU",
        "TWO SICILIES",
        "UGANDA",
        "UKRAINE",
        "UNITED ARAB EMIRATES",
        "UNITED KINGDOM",
        "UNITED STATES",
        "UNITED STATES MINOR OUTLYING ISLANDS",
        "URUGUAY",
        "UZBEKISTAN",
        "VANUATU",
        "VENEZUELA, BOLIVARIAN REPUBLIC OF",
        "VIET NAM",
        "VIRGIN ISLANDS, BRITISH",
        "VIRGIN ISLANDS, U.S.",
        "WALLIS AND FUTUNA",
        "WESTERN SAHARA",
        "WUERTTEMBURG",
        "YEMEN",
        "YEMEN ARAB REPUBLIC",
        "YEMEN PEOPLE'S REPUBLIC",
        "YUGOSLAVIA",
        "ZAMBIA",
        "ZANZIBAR",
        "ZIMBABWE",
        "ALABAMA",
        "ALASKA",
        "ARIZONA",
        "ARKANSAS",
        "CALIFORNIA",
        "COLORADO",
        "CONNECTICUT",
        "DELAWARE",
        "FLORIDA",
        "GEORGIA",
        "HAWAII",
        "IDAHO",
        "ILLINOIS",
        "INDIANA",
        "IOWA",
        "KANSAS",
        "KENTUCKY",
        "LOUISIANA",
        "MAINE",
        "MARYLAND",
        "MASSACHUSETTS",
        "MICHIGAN",
        "MINNESOTA",
        "MISSISSIPPI",
        "MISSOURI",
        "MONTANA",
        "NEBRASKA",
        "NEVADA",
        "NEW HAMPSHIRE",
        "NEW JERSEY",
        "NEW MEXICO",
        "NEW YORK",
        "NORTH CAROLINA",
        "NORTH DAKOTA",
        "OHIO",
        "OKLAHOMA",
        "OREGON",
        "PENNSYLVANIA",
        "RHODE ISLAND",
        "SOUTH CAROLINA",
        "SOUTH DAKOTA",
        "TENNESSEE",
        "TEXAS",
        "UTAH",
        "VERMONT",
        "VIRGINIA",
        "WASHINGTON",
        "WEST VIRGINIA",
        "WISCONSIN",
        "WYOMING",
        "DISTRICT OF COLUMBIA",
        "ENGLAND",
        "BRITISH VIRGIN ISLANDS",
        "NETHERLAND ANTILLES",
        "RUSSIA",
        "SOUTH KOREA",
        'TAIWAN',
        "VENEZUELA",
        'CHANNEL ISLANDS'
      )
    )
  }

.parse_page_sub_multi_item_html <-
  function(page) {
    locations <-
      .loc_df() %>%
      .$nameLocation
    subsidiaries <-
      page %>%
      html_nodes('td div') %>%
      html_text() %>%
      str_replace_all('\u0095 |\u0096|\u0095\n', '') %>%
      str_trim()

    subsidiaries <-
      subsidiaries[!subsidiaries == '']

    data_nodes <-
      page %>%
      html_nodes('td') %>%
      html_text() %>%
      str_replace_all('\u0095 |\u0096|\u0095\n', '') %>%
      str_trim() %>%
      str_to_upper()

    data_nodes <-
      data_nodes[!data_nodes == '']

    location_items <-
      data_nodes[data_nodes %in% locations]

    pct_vals <-
      tibble(value = data_nodes) %>%
      filter(!value %>% str_detect("\\([(1-9)]\\)")) %>%
      mutate(pctSubsidiaryOwned = value %>% as.numeric()) %>%
      filter(!pctSubsidiaryOwned %>% is.na()) %>%
      slice(seq_along(subsidiaries)) %>%
      .$pctSubsidiaryOwned / 100 %>%
      suppressWarnings() %>%
      suppressMessages()

    all_data <-
      tibble(
        nameSubsidiary = subsidiaries,
        nameLocationSubsidiary = location_items,
        pctSubsidiaryOwned = pct_vals
      ) %>%
      mutate(nameSubsidiary = nameSubsidiary %>% str_to_upper())

    return(all_data)
  }

.parse_page_subsidiary_table_html <-
  function(page,
           numbers = 1:10,
           hit_terms = c(
             "Organized",
             "STATE OR|STATE OF|JURISDICTION OF|JURISDICTION OF INCORPORATION OR ORGANIZATION|JURISDICTION|JURISDICTION OF INCORPORATION OR\nORGANIZATION",
             "NAME|ORGANIZED UNDER THE LAWS OF",
             'STATE OF ORGANIZATION',
             'STATE OR COUNTRY OF ORGANIZATION',
             'NAME OF SUBSIDIARY',
             'NAME',
             'ENTITY NAME',
             'the laws of',
             'Percentage of voting',
             'securities owned by',
             'immediate parent',
             'CERTAIN INTERMEDIARY SUBSIDIARIES',
             'Note:',
             'Organized',
             'Under the',
             'Laws of',
             'OWNED BY',
             'IMMEDIATE',
             'PARENT',
             "OWNS",
             "CERTAIN INTERMEDIARY SUBSIDIARIES",
             'PERCENTAGE',
             'OF VOTING',
             'SECURITIES'
           )) {
    is_ib1 <-
      page %>%
      html_nodes('b font') %>%
      html_text() %>% length() > 0

    if (is_ib1) {
      items_bold <-
        page %>%
        html_nodes('b font') %>%
        html_text() %>%
        str_to_upper() %>%
        str_replace_all('\n', ' ') %>%
        stringi::stri_trans_general("Latin-ASCII")
      str_split('\\-') %>%
        flatten_chr() %>%
        str_trim()
    } else {
      items_bold <-
        page %>%
        html_nodes('b') %>%
        html_text() %>%
        str_to_upper() %>%
        str_replace_all('\n', ' ') %>%
        stringi::stri_trans_general("Latin-ASCII") %>%
        str_split('\\-') %>%
        flatten_chr() %>%
        str_trim() %>%
        unique()
    }

    has_date <-
      items_bold %>% grep(month.name %>% str_to_upper() %>% paste(collapse = '|'), .) %>% length > 0

    if (has_date) {
      date_data <-
        items_bold[items_bold %>% grep(month.name %>% str_to_upper() %>% paste(collapse = '|'), .)] %>%
        lubridate::mdy()
    } else {
      date_data <-
        NA
    }

    hit_terms <-
      hit_terms %>%
      append(items_bold) %>%
      str_to_upper() %>%
      unique() %>%
      append(list('(', letters, ')') %>%
               purrr::invoke(paste0, .)) %>%
      paste0(collapse = '|')


    hit_terms_in <-
      hit_terms %>% str_split('\\|') %>%
      flatten_chr()

    locations <-
      .loc_df() %>%
      .$nameLocation

    all_data <-
      numbers %>%
      future_map_dfr(function(x) {
        css_selector <-
          paste0('td:nth-child(', x, ')')
        has_length <-
          page %>%
          html_nodes(css_selector) %>% length() > 0
        if (has_length) {
          item <-
            paste0("X" , x)

          value <-
            page %>%
            html_nodes(css_selector) %>%
            html_text() %>%
            str_trim()
          tibble(item, value)
        }
      }) %>%
      mutate(
        value = value %>% str_to_upper() %>% str_replace_all('\n  ', ' ') %>% str_replace_all('\u0096 ', '')
      ) %>%
      filter(!value == '')

    has_loc_key <-
      all_data %>%
      filter(value %in% locations) %>%
      nrow() > 0

    if (has_loc_key) {
      loc_cols <-
        all_data %>%
        filter(value %in% locations) %>%
        .$item %>%
        unique()
      if (loc_cols %>% length == 1) {
        loc_col <-
          loc_cols[[1]]
      }
    }

    has_pct <-
      all_data %>%
      filter(value %>% str_detect("PERCENT")) %>%
      .$item %>% unique() %>% length() > 0

    if (has_pct) {
      pct_col <-
        all_data %>%
        filter(value %>% str_detect("PERCENT")) %>%
        .$item %>% unique()
    } else {
      pct_col <-
        NA
    }

    is_whack <-
      pct_col[[1]] %in% loc_cols

    if (is_whack) {
      all_data <-
        page %>%
        .parse_page_sub_multi_item_html() %>%
        mutate(dateSubsidiaryAsOf = date_data)

      return(all_data)
    }

    all_data <-
      all_data %>%
      filter(!value %in% items_bold) %>%
      filter(!value %>% str_detect(paste0(items_bold %>% unique(), collapse = '|'))) %>%
      filter(!value %in% hit_terms_in) %>%
      filter(!value %>% str_detect(hit_terms))

    count_df <-
      all_data %>% count(item, sort = T) %>%
      arrange(item) %>%
      spread(item, n)

    off_one <-
      (count_df[, 2] %>% extract2(1)) - (count_df[, 1] %>% extract2(1)) == 1

    min_item <-
      count_df %>% gather(item, value) %>% filter(value == min(value)) %>% .$item

    change_pct <-
      has_pct & (pct_col == min_item) %>% sum() > 0

    if (change_pct) {
      pct_col <-
        names(count_df)[[3]]
    }

    if (off_one) {
      df <-
        all_data$item %>% unique() %>%
        future_map_dfr(function(x) {
          has_data <-
            all_data %>%
            filter(item == x) %>%
            filter(!value %>% is.na()) %>%
            filter(!value == '') %>%
            nrow()

          if (has_data) {
            all_data %>%
              filter(item == x) %>%
              filter(!value %>% is.na()) %>%
              filter(!value == '') %>%
              filter(!value %>% str_detect(hit_terms)) %>%
              mutate(idSubsidiary = 1:n())
          }
        }) %>%
        filter(!value %>% str_detect(hit_terms)) %>%
        spread(item, value)

      if (change_pct) {
        df <-
          df %>%
          select(-one_of(min_item))
      }
    }

    if (!off_one) {
      has_property <-
        items_bold %>% str_detect('PROPERTY') %>% sum() > 0
      if (has_property) {
        tables <-
          page %>%
          html_table(fill = T)
        df <-
          seq_along(tables) %>%
          future_map_dfr(function(x) {
            table_df <-
              tables[[x]] %>%
              data.frame(stringsAsFactors = FALSE) %>%
              as_tibble()

            column_df <-
              table_df %>% slice(1) %>%
              gather(column, value) %>%
              mutate(idColumn = 1:n()) %>%
              filter(!value %>% is.na()) %>%
              left_join(tibble(
                value = c(
                  "PROPERTY",
                  "ENTITIES",
                  "STATE OF FORMATION",
                  "DATE OF FORMATION",
                  " ",
                  'General Information:'
                ),
                nameItem = c(
                  'nameProperty',
                  'nameSubsidiary',
                  'locationOrganizationSubsidiary',
                  'dateSubsidiaryFormed',
                  'locationOrganizationSubsidiary',
                  'nameSubsidiary'
                )
              )) %>%
              suppressMessages()
            two_col <-
              column_df %>% nrow() == 2
            if (two_col) {
              column_df$nameItem[[2]] <-
                'locationOrganizationSubsidiary'
            }

            columns_keep <-
              column_df$idColumn

            table_df <-
              table_df <-
              table_df %>%
              select(columns_keep) %>%
              slice(-1) %>%
              purrr::set_names(column_df$nameItem)

            table_df <-
              table_df %>%
              mutate_all(funs(. %>% str_trim() %>% str_to_upper())) %>%
              mutate(nameSubsidiary = ifelse(nameSubsidiary == '', NA, nameSubsidiary)) %>%
              filter(!nameSubsidiary %>% is.na())


            if (two_col) {
              table_df <-
                table_df %>%
                tidyr::separate(
                  locationOrganizationSubsidiary,
                  into = c(
                    'locationOrganizationSubsidiary',
                    'dateSubsidiaryFormed'
                  ),
                  sep = 'FORMED'
                ) %>%
                suppressWarnings() %>%
                mutate(locationOrganizationSubsidiary = locationOrganizationSubsidiary %>% str_replace_all('\\,', '')) %>%
                mutate_all(funs(. %>% str_replace('\n', '') %>% str_trim()))
            }


            if ('nameProperty' %in% names(table_df)) {
              table_df <-
                table_df %>%
                mutate(nameProperty = ifelse(nameProperty == '', NA, nameProperty)) %>%
                mutate_all(funs(. %>% str_replace('\n|\n  |\n  ', '') %>% str_trim())) %>%
                mutate_all(funs(. %>% str_replace('\n', '') %>% str_trim())) %>%
                mutate_all(funs(. %>% str_replace('  ', ' ') %>% str_trim())) %>%
                fill(nameProperty)

            }

            return(table_df)
          })

        if ('dateSubsidiaryFormed' %in% names(df)) {
          df <-
            df %>%
            mutate(dateSubsidiaryFormed = dateSubsidiaryFormed %>% lubridate::mdy())
        }

        df <-
          df %>%
          mutate(idCIK = cik, urlSEC = url) %>%
          select(idCIK, nameSubsidiary, everything()) %>%
          mutate(
            locationOrganizationSubsidiary = locationOrganizationSubsidiary %>% str_replace_all(
              'A |LIMITED LIABILITY COMPANY|CORPORATION|LIMITED PARTNERSHIP'
            ) %>% str_trim()
          )

        return(df)
      }
      if (!has_property) {
        df <-
          all_data %>%
          mutate(value = ifelse(value == '', NA, value)) %>%
          filter(!value %>% is.na()) %>%
          group_by(item) %>%
          mutate(idSubsidiary = 1:n()) %>%
          spread(item, value) %>%
          filter(!X1 == '') %>%
          mutate(idSubsidiary = 1:n()) %>%
          gather(item, value, -c(X1, idSubsidiary)) %>%
          ungroup() %>%
          filter(!value %>% str_detect(hit_terms)) %>%
          spread(item, value)
      }

    }

    df <-
      df %>%
      dplyr::rename(nameSubsidiary = X1) %>%
      tidyr::separate(nameSubsidiary,
                      sep = '\\(',
                      into = c('nameSubsidiary', 'remove')) %>%
      select(-dplyr::matches("remove")) %>%
      mutate(nameSubsidiary = nameSubsidiary %>% str_trim()) %>%
      suppressWarnings() %>%
      select(-dplyr::matches("idSubsidiary"))

    if (has_pct) {
      names(df)[names(df) %>% grep(pct_col, .)] <-
        'pctSubsidiaryOwned'

      df <-
        df %>%
        mutate_at(df %>% select(dplyr::matches('pct')) %>% names(),
                  funs(. %>% as.numeric() / 100)) %>%
        suppressWarnings()
    }

    if (has_loc_key) {
      names(df)[names(df) %>% grep(loc_col, .)] <-
        'locationOrganizationSubsidiary'
    }

    df <-
      df %>%
      select(-dplyr::matches("X"))

    return(df)
  }

.parse_sec_subsidiary_url_html <-
  function(url = "https://www.sec.gov/Archives/edgar/data/34088/000003408816000065/xomexhibit21.htm",
           return_message = TRUE) {
    cik <-
      url %>%
      .parse_sec_url_for_cik()

    page <-
      url %>%
      read_html()

    is_zero <-
      page %>%
      html_nodes(paste0('td:nth-child(', 1, ')')) %>%
      length() == 0
    locations <-
      .loc_df() %>%
      .$nameLocation

    if (is_zero) {
      data <-
        page %>%
        html_nodes('font') %>%
        html_text() %>%
        str_replace_all('\\ ', ' ')

      data <-
        data[!data == '']


      is_parenth <-
        data %>% str_detect('\\(') %>% sum() / length(data) > .25

      if (is_parenth) {
        data <-
          data[data %>% str_detect('\\(')]

        df <-
          tibble(data) %>%
          separate(
            data,
            sep = '\\(',
            into = c('nameSubsidiary', 'locationOrganizationSubsidiary')
          ) %>%
          separate(
            locationOrganizationSubsidiary,
            sep = '\\)',
            into = c('locationOrganizationSubsidiary', 'remove')
          ) %>%
          select(-remove) %>%
          mutate_all(funs(. %>% str_trim() %>% str_to_upper())) %>%
          mutate(idCIK = cik, urlSEC = url) %>%
          select(-dplyr::matches("idSubsidiary"))

        if (return_message) {
          list("Parsed: ", url) %>%
            purrr::invoke(paste0, .) %>% cat(fill = T)
        }

        return(df)
      }

      is_nested <-
        page %>%
        html_nodes('b font') %>%
        html_text() %>% length() > 2

      if (is_nested) {
        locations_raw <-
          page %>%
          html_nodes('b font') %>%
          html_text() %>%
          str_replace_all('\\:', '') %>%
          str_to_upper()

        locations <-
          locations_raw[!locations_raw %>% str_detect('EXHIBIT|SUBSIDIARY|SUBSIDIARIES')]

        data <-
          data[data %>% nchar() > 3] %>% str_to_upper()

        df <-
          tibble(nameSubsidiary = data) %>%
          mutate(idRow = 1:n())

        .loc_df <-
          tibble(nameSubsidiary = locations) %>%
          inner_join(df %>% select(idRow, nameSubsidiary)) %>%
          mutate(idRow = idRow + 1) %>%
          select(locationOrganizationSubsidiary = nameSubsidiary, idRow) %>%
          suppressMessages()

        df <-
          df %>%
          filter(!nameSubsidiary %>% str_detect('SUBSIDIARY|SUBSIDIARIES')) %>%
          filter(!nameSubsidiary %>% str_detect(paste0(locations_raw, collapse = '|'))) %>%
          suppressWarnings()

        df <-
          df %>%
          left_join(.loc_df) %>%
          fill(locationOrganizationSubsidiary) %>%
          mutate(urlSEC = url, idCIK = cik) %>%
          select(idCIK,
                 nameSubsidiary,
                 locationOrganizationSubsidiary,
                 everything()) %>%
          select(-idRow) %>%
          suppressMessages() %>%
          select(-dplyr::matches("idSubsidiary"))
        if (return_message) {
          list("Parsed: ", url) %>%
            purrr::invoke(paste0, .) %>% cat(fill = T)
        }

        return(df)
      }
    }

    is_font_table <-
      page %>%
      html_nodes('b') %>%
      html_text() %>% length() == 0

    if (is_font_table) {
      all_data <-
        1:10 %>%
        future_map_dfr(function(x) {
          css_selector <-
            paste0('td:nth-child(', x, ')')
          has_length <-
            page %>%
            html_nodes(css_selector) %>% length() > 0
          if (has_length) {
            item <-
              paste0("X" , x)

            value <-
              page %>%
              html_nodes(css_selector) %>%
              html_text() %>%
              str_trim()
            tibble(item, value)
          }
        }) %>%
        mutate(
          value = value %>% str_to_upper() %>% str_replace_all('\n  ', ' ') %>% str_replace_all('\u0096 ', '')
        ) %>%
        filter(!value == '')


      has_loc_key <-
        all_data %>%
        filter(value %in% locations) %>%
        nrow() > 0

      if (has_loc_key) {
        loc_col <-
          all_data %>%
          filter(value %in% locations) %>%
          .$item %>%
          unique()
      }

      hit_terms_in <-
        c(
          "Organized",
          "STATE OR|STATE OF|JURISDICTION OF|JURISDICTION OF INCORPORATION OR ORGANIZATION|JURISDICTION|JURISDICTION OF INCORPORATION OR\nORGANIZATION",
          "NAME|ORGANIZED UNDER THE LAWS OF",
          'STATE OF ORGANIZATION',
          'STATE OR COUNTRY OF ORGANIZATION',
          'NAME OF SUBSIDIARY',
          'NAME',
          'ENTITY NAME',
          'the laws of',
          'Percentage of voting',
          'securities owned by',
          'immediate parent',
          'CERTAIN INTERMEDIARY SUBSIDIARIES',
          'PERCENT OWNED'
        )
      hit_terms <-
        hit_terms %>%
        str_to_upper() %>%
        paste0(collapse = '|')

      hit_terms_in <-
        hit_terms %>% str_split('\\|') %>%
        flatten_chr()

      has_pct_col <-
        all_data %>%
        filter(value %in% "100") %>%
        nrow() > 0 |
        (all_data %>% filter(value %>% str_detect('PERCENT')) %>% nrow() > 0)

      if (has_pct_col) {
        pct_col <-
          all_data %>%
          filter((value %in% "100") |
                   (value %>% str_detect("PERCENT"))) %>%
          .$item %>%
          unique() %>%
          .[[1]]
      }

      all_data <-
        all_data %>%
        filter(!value %in% hit_terms_in) %>%
        filter(!value %>% str_detect(hit_terms)) %>%
        filter(!value == '') %>%
        mutate(valueNC = value %>% nchar()) %>%
        filter(!value %>% str_detect("PERCENT"))

      if (!has_pct_col) {
        all_data <-
          all_data %>%
          filter(valueNC > 3)
      }
      all_data <-
        all_data %>%
        select(-valueNC) %>%
        group_by(item) %>%
        mutate(idSubsidiary = 1:n()) %>%
        spread(item, value) %>%
        ungroup() %>%
        dplyr::rename(nameSubsidiary = X1)

      if (has_loc_key) {
        names(all_data)[names(all_data) %in% loc_col] <-
          'locationOrganizationSubsidiary'
      }

      if (has_pct_col) {
        names(all_data)[names(all_data) %in% pct_col] <-
          'pctSubsidiaryOwned'

        all_data <-
          all_data %>%
          mutate(pctSubsidiaryOwned = pctSubsidiaryOwned %>% as.numeric() / 100)
      }

      all_data <-
        all_data %>%
        mutate(idCIK = cik,
               dateSubsidiaryAsOf = NA,
               urlSEC = url) %>%
        select(-dplyr::matches("idSubsidiary|^X"))

      if (return_message) {
        list("Parsed: ", url) %>%
          purrr::invoke(paste0, .) %>% cat(fill = T)
      }

      return(all_data)

    }

    df <-
      page %>%
      .parse_page_subsidiary_table_html() %>%
      suppressWarnings()

    df <-
      df %>%
      filter(!nameSubsidiary == '') %>%
      mutate(idCIK = cik, urlSEC = url) %>%
      select(-dplyr::matches("idSubsidiary")) %>%
      select(idCIK, everything())

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% cat(fill = T)
    }

    return(df %>% select(-dplyr::matches("idSubsidiary")))

  }

# url = 'https://www.sec.gov/Archives/edgar/data/19617/000095012301002499/y46253ex21-1.txt'
.parse_sec_subsidiary_url_text <-
  function(url = "https://www.sec.gov/Archives/edgar/data/899689/000104746903007996/a2104897zex-21.txt",
           return_message = TRUE) {
    cik <-
      url %>%
      .parse_sec_url_for_cik()
    data <-
      url %>%
      read_lines()

    data <-
      data[!data == '']
    has_s <-
      data %>% str_detect("<S>") %>% sum() > 0

    if (has_s) {
      data <-
        data[(data %>% grep("<S>", .) %>% .[[1]] + 1):length(data)]
    }

    data <-
      data[!data %>% str_detect("STATE OF|NAME OF|---|NAME OF SUBSIDIARY|ORGANIZED UNDER|THE LAWS OF|<")]

    data <-
      data[data %>% nchar() > 3]

    df <-
      seq_along(data) %>%
      future_map_dfr(function(x) {
        item <-
          data[[x]]

        items <-
          item %>%
          str_replace_all('\\   ', '\\:') %>%
          str_split('\\:') %>%
          flatten_chr() %>%
          str_trim() %>%
          str_to_upper()

        items <-
          items[!items == '']

        if (items %>% length() == 1) {
          return(tibble())
        }

        two_items <-
          items %>% length() == 2
        if (two_items) {
          table_data <-
            tibble(
              idSubsidiary = x,
              nameSubsidiary = items[[1]],
              locationOrganizationSubsidiary = items[[2]]
            )
        }
        three_items <-
          items %>% length() == 3
        if (three_items) {
          table_data <-
            tibble(
              idSubsidiary = x,
              nameSubsidiary = items[[1]],
              locationOrganizationSubsidiary = items[[2]],
              pctSubsidiaryOwned = items[[3]] %>% as.numeric() / 100
            )
        }

        table_data <-
          table_data %>%
          mutate(
            isChildSubsidiary = ifelse(nameSubsidiary %>% substr(1, 1) == "-", TRUE, FALSE),
            nameSubsidiary = nameSubsidiary %>% str_replace('\\-', '') %>% str_trim()
          )
        return(table_data)
      }) %>%
      mutate(idCIK = cik, urlSEC = url) %>%
      select(-dplyr::matches("idSubsidiary")) %>%
      select(idCIK,
             nameSubsidiary,
             locationOrganizationSubsidiary,
             everything()) %>%
      filter(!nameSubsidiary %in% c('NAME', 'ORGANIZED UNDER'))

    df <-
      df %>%
      filter(!nameSubsidiary == '')

    if (return_message) {
      list("Parsed: ", url) %>%
        purrr::invoke(paste0, .) %>% cat(fill = T)
    }

    return(df)

  }

.parse_sec_subsidiary_url  <-
  function(url = "https://www.sec.gov/Archives/edgar/data/34088/000003408816000065/xomexhibit21.htm",
           return_message = TRUE)  {
    is_text <-
      url %>%
      str_detect("txt")

    is_html <-
      url %>%
      str_detect("html|htm")
    parse_sec_subsidiary_url_text_safe <-
      purrr::possibly(.parse_sec_subsidiary_url_text, tibble())

    parse_sec_subsidiary_url_html_safe <-
      purrr::possibly(.parse_sec_subsidiary_url_html, tibble())

    if (is_text) {
      data <-
        url %>%
        parse_sec_subsidiary_url_text_safe()
    }

    if (is_html) {
      data <-
        url %>%
        parse_sec_subsidiary_url_html_safe()
    }
    data
  }

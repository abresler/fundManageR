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
  function(return_message = T) {
    url <- 'https://www.sec.gov/edgar/NYU/cik.coleft.c'
    cik_data <-
      url %>%
      readr::read_table(col_names = F)

    cik_data <-
      cik_data %>%
      tidyr::separate(X1,
                      into = c('nameEntity', 'codeCIK'),
                      sep = '\\:[0][0]') %>%
      mutate(codeCIK = codeCIK %>% stringr::str_replace('\\:', ''),
             codeCIK = "00" %>% paste0(codeCIK),
             idCIK = codeCIK %>% as.numeric) %>%
      mutate(datetimeData = Sys.time()) %>%
      dplyr::select(idCIK, nameEntity, everything())

    if (return_message) {
      "You returned " %>%
        paste0(cik_data %>% nrow,' entities with registered CIK codes')
    }
    return(cik_data)
  }


# munging -----------------------------------------------------------------

select_start_vars <-
  function(data){
    data <-
      data %>%
      dplyr::select(idCRD, nameEntityManager, everything())
    return(data)
  }


mutate_adv_data <-
  function(data) {
    has_dates <-
      data %>% dplyr::select(matches("^date")) %>% names %>% length > 0
    if (has_dates) {
    data <-
      data %>%
      mutate_at(.cols =
                  data %>% dplyr::select(matches("^date")) %>% names,
                funs(. %>% lubridate::ymd()))
    }
    has_counts <-
      data %>% dplyr::select(matches("^count[A-Z]|^amount[A-Z]|idCRD")) %>% dplyr::select(-matches("country")) %>%
      names %>% length > 0

    if (has_counts) {
    data <-
      data %>%
      mutate_at(.cols = data %>% dplyr::select(matches("^count[A-Z]|^amount[A-Z]|idCRD")) %>%
                  dplyr::select(-matches("country")) %>%
                  names,
                funs(. %>% as.numeric()))
    }
    has_logical <-
      data %>% dplyr::select(matches("^is[A-Z]|^has[A-Z]")) %>% names %>% length > 0

    if (has_logical) {
    data <-
      data %>%
      mutate_at(.cols =
                  data %>% dplyr::select(matches("^is[A-Z]|^has[A-Z]")) %>% names,
                funs(. %>% as.logical()))
    }

    return(data)

  }


widen_adv_data <-
  function(data) {
    data <-
      data %>%
      mutate_at(.cols =
                  data %>% dplyr::select(matches("^date[A-Z]")) %>% names,
                funs(. %>% as.character())) %>%
      gather(nameItem, value, -c(countItem, nameEntityManager)) %>%
      suppressWarnings()

    data <-
      data %>%
      mutate(
        countItem = countItem - 1,
        countItem = countItem %>% as.character(),
        countItem = ifelse(countItem == "0", '', countItem)
      ) %>%
      unite(item, nameItem, countItem, sep = '') %>%
      distinct() %>%
      suppressWarnings()

    col_order <-
      c('nameEntityManager', data$item)

    data <-
      data %>%
      spread(item, value) %>%
      dplyr::select(one_of(col_order))

    data <-
      data %>%
      mutate_adv_data() %>%
      suppressWarnings()

    return(data)
  }


get_item_name_yes_no_df <-
  function(item_name = 'hasCustodyClientCash') {
    item_name_df <-
      1:length(item_name) %>%
      map_df(function(x) {
        data_frame(nameItem = rep(item_name[x], 2),
                   valueItem = c(T, F)) %>%
          unite(fullnameItem,
                nameItem,
                valueItem,
                sep = '.',
                remove = F)
      })
    return(item_name_df)
  }

has_item_check_name <-
  function(item_name = 'hasQuarterlyStatemnt') {
    item_name_df <-
      1:length(item_name) %>%
      map_df(function(x) {
        data_frame(nameItem = rep(item_name[x], 1),
                   valueItem = T) %>%
          mutate(fullnameItem = nameItem)
      })
    return(item_name_df)
  }



# finra_broker ------------------------------------------------------------
get_html_page <-
  function(url = 'http://www.adviserinfo.sec.gov/IAPD/IAPDFirmSummary.aspx?ORG_PK=160080') {
    httr::set_config(config(ssl_verifypeer = 0L))
    page <-
      url %>%
      GET() %>%
      read_html()
    return(page)
  }


check_html_node <-
  function(page, node_css = '#ctl00_cphMain_landing_p2BrochureLink') {
    if (page %>% html_nodes(css = node_css) %>% length > 0) {
      node_exists <-
        T
    } else {
      node_exists <-
        F
    }
    return(node_exists)
  }


get_html_node_text <-
  function(page, node_css = '#ctl00_cphMain_landing_lblActiveOrgName') {
    parse_html_for_text <-
      function(page, node_css = '#ctl00_cphMain_landing_lblActiveOrgName') {
        node_text <-
          page %>%
          html_nodes(css = node_css) %>%
          html_text

        return(node_text)
      }

    node_exists <-
      page %>%
      check_html_node(node_css = node_css)

    if (node_exists) {
      node_text <-
        page %>% parse_html_for_text(node_css = node_css)
    } else {
      node_text <-
        NA
    }
    return(node_text)
  }


get_entity_manager_name <-
  function(page) {
    manager_name <-
      page %>%
      get_html_node_text(node_css = '#ctl00_ctl00_cphMainContent_ucADVHeader_lblPrimaryBusinessName')

    if (manager_name %>% is.na) {
      manager_name <-
        page %>%
        html_nodes('.summary-displayname') %>%
        html_text
    }

    return(manager_name)
  }

get_html_node_attributes <-
  function(page,
           node_css = '#ctl00_cphMain_landing_p2BrochureLink',
           html_attribute = 'href',
           base_url = NULL) {
    parse_for_html_attribute <-
      function(page,
               node_css = '#ctl00_cphMain_landing_p2BrochureLink',
               html_attribute = 'href',
               base_url = NULL) {
        node_attribute <-
          page %>%
          html_nodes(css = node_css) %>%
          html_attr(name = html_attribute)

        if (!base_url %>% is_null) {
          node_attribute <-
            base_url %>%
            paste0(node_attribute)
        }
        return(node_attribute)

      }

    node_exists <-
      page %>%
      check_html_node(node_css = node_css)

    if (node_exists) {
      node_attribute <-
        page %>%
        parse_for_html_attribute(
          node_css = node_css,
          html_attribute = html_attribute,
          base_url = base_url
        )
    } else {
      node_attribute <-
        NA
    }

    return(node_attribute)
  }


parse_node_table_to_text <-
  function(page, css_node = '#ctl00_ctl00_cphMainContent_cphAdvFormContent_ClientCompensation_ctl00_trIAPDHeader + tr + tr') {
    node_text <-
      page %>%
      get_html_node_text(node_css = css_node) %>%
      str_replace_all('\r|\n|\t', '') %>%
      stri_replace_all_charclass("\\p{WHITE_SPACE}", " ") %>%
      stri_trim_both() %>%
      gsub("^\\s+|\\s+$", "", .) %>%
      str_split("   +") %>%
      flatten_chr

    node_text <-
      node_text[!node_text == '']
    return(node_text)
  }


parse_finra_c_url <-
  function(finra_c_url = "curl 'http://brokercheck.finra.org/Search/GenericSearch' -H 'Pragma: no-cache' -H 'Origin: http://brokercheck.finra.org' -H 'Accept-Encoding: gzip, deflate' -H 'Accept-Language: en-US,en;q=0.8' -H 'Upgrade-Insecure-Requests: 1' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/53.0.2785.80 Safari/537.36' -H 'Content-Type: application/x-www-form-urlencoded' -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8' -H 'Cache-Control: no-cache' -H 'Referer: http://brokercheck.finra.org/Search/GenericSearch' -H 'Cookie: __RequestVerificationToken=rSB02up7WU1YiLtAmLvoqbrC4heA7LXKidX0GkSiEjprktiE0qFDfkGKbQkTQVd94ShQynDI5BsmpIJeB4idW39G2QE6hRtgA9M_ejnQoHc1; ASP.NET_SessionId=kjzpjdpm1p0ltmm1xcuafmoz' -H 'Connection: keep-alive' -H 'DNT: 1' --data '__RequestVerificationToken=1jsNCQ-1KyYFIfKZuaJc_-CFqVaBiSi1MQ_WyvUrMKk9bEh1ux4s_te11rbeAEHH23ncmfbN8Ndf9gIQvYb0gGEuSabJ1PKIhKi0_AJac9Q1&GenericSearch.StartRow=1&GenericSearch.PageSize=15&GenericSearch.System=BC&GenericSearch.SearchType=2&GenericSearch.IndividualSearchText=&GenericSearch.EmploymingFirmSearchText=&GenericSearch.FirmSearchText=EJF&GenericSearch.Within=25&GenericSearch.ZipCode=' --compressed") {
    clean_url <-
      finra_c_url %>%
      curlconverter::straighten() %>%
      suppressMessages()

    res <-
      clean_url %>%
      make_req()

    html_page <-
      res[[1]]() %>%
      content(as = "parsed")

    return(html_page)
  }

find_text_node <-
  function(node_text,
           hit_words = "Total Number of Clients",
           off_set = 4,
           is_numeric_node = T) {
    if (hit_words %>% length > 1) {
      hit_words <-
        hit_words %>% str_c(collapse = '|')
    }

    node_exists <-
      (node_text %>% grep(hit_words, .)) %>% length > 0

    if (!node_exists) {
      stop("Sorry " %>%
             paste0(hit_words, ' is not found in the text nodes'))
    }
    node_location <-
      (node_text %>% grep(hit_words, .)) %>% + off_set

    text_node <-
      node_text[node_location] %>%
      str_trim

    if (is_numeric_node) {
      text_node <-
        text_node %>%
        str_replace_all('\\$', '') %>% str_trim %>%
        readr::parse_number()

      text_node <-
        text_node[!text_node %>% is.na]
    }
    return(text_node)
  }


get_finra_managers_metadata <-
  function(search_names = c('Rockwood Capital',
                            'Blackstone Real Estate',
                            'Fir Tree',
                            'Fortress'),
           return_message = T) {
    get_finra_manager_metadata <-
      function(search_name = "Blackstone Real Estate",
               return_message = T) {
        get_finra_search_term_c_urls <-
          function(search_name = "Blackstone") {
            get_finra_c_url <-
              function(search_name = "EJF Capital",
                       page_no = 1) {
                base_curl <-
                  "curl 'http://brokercheck.finra.org/Search/GenericSearch' -H 'Pragma: no-cache' -H 'Origin: http://brokercheck.finra.org' -H 'Accept-Encoding: gzip, deflate' -H 'Accept-Language: en-US,en;q=0.8' -H 'Upgrade-Insecure-Requests: 1' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/53.0.2785.80 Safari/537.36' -H 'Content-Type: application/x-www-form-urlencoded' -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8' -H 'Cache-Control: no-cache' -H 'Referer: http://brokercheck.finra.org/Search/GenericSearch' -H 'Cookie: __RequestVerificationToken=rSB02up7WU1YiLtAmLvoqbrC4heA7LXKidX0GkSiEjprktiE0qFDfkGKbQkTQVd94ShQynDI5BsmpIJeB4idW39G2QE6hRtgA9M_ejnQoHc1; ASP.NET_SessionId=kjzpjdpm1p0ltmm1xcuafmoz' -H 'Connection: keep-alive' -H 'DNT: 1' --data '__RequestVerificationToken=1jsNCQ-1KyYFIfKZuaJc_-CFqVaBiSi1MQ_WyvUrMKk9bEh1ux4s_te11rbeAEHH23ncmfbN8Ndf9gIQvYb0gGEuSabJ1PKIhKi0_AJac9Q1&GenericSearch.StartRow=1&GenericSearch.PageSize=15&GenericSearch.System=BC&GenericSearch.SearchType=2&GenericSearch.IndividualSearchText=&GenericSearch.EmploymingFirmSearchText=&GenericSearch.FirmSearchText="

                end_curl_1 <-
                  "&ZipCode=&Within=25&PageSize=15&StartRow=1&CurrentPage=1"

                end_curl_2 <-
                  "&IsPagination=false' --compressed"

                search_encode <-
                  search_name %>%
                  URLencode()
                c_url <-
                  base_curl %>%
                  paste0(search_encode, end_curl_1, page_no, end_curl_2)
                return(c_url)
              }

            page <-
              search_name %>%
              get_finra_c_url(search_name = .) %>%
              parse_finra_c_url()

            count_results <-
              page %>%
              html_nodes('.searchresultscriteriacol1') %>%
              html_text() %>%
              str_trim %>%
              str_replace_all(' of | to ', '\\-') %>%
              str_split('\\-') %>%
              flatten_chr %>%
              readr::parse_number() %>%
              max()

            pages <-
              ceiling(count_results / 15)

            term_c_urls <-
              seq_len(pages) %>%
              map_chr(function(x) {
                get_finra_c_url(search_name = search_name, page_no = x)
              })

            return(term_c_urls)
          }

        get_finra_search_term_c_urls_safe <-
          possibly(get_finra_search_term_c_urls, NULL)
        parse_finra_manager_search_data <-
          function(manager_c_url = "curl 'http://brokercheck.finra.org/Search/GenericSearch' -H 'Pragma: no-cache' -H 'Origin: http://brokercheck.finra.org' -H 'Accept-Encoding: gzip, deflate' -H 'Accept-Language: en-US,en;q=0.8' -H 'Upgrade-Insecure-Requests: 1' -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/53.0.2785.80 Safari/537.36' -H 'Content-Type: application/x-www-form-urlencoded' -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8' -H 'Cache-Control: no-cache' -H 'Referer: http://brokercheck.finra.org/Search/GenericSearch' -H 'Cookie: __RequestVerificationToken=rSB02up7WU1YiLtAmLvoqbrC4heA7LXKidX0GkSiEjprktiE0qFDfkGKbQkTQVd94ShQynDI5BsmpIJeB4idW39G2QE6hRtgA9M_ejnQoHc1; ASP.NET_SessionId=kjzpjdpm1p0ltmm1xcuafmoz' -H 'Connection: keep-alive' -H 'DNT: 1' --data '__RequestVerificationToken=1jsNCQ-1KyYFIfKZuaJc_-CFqVaBiSi1MQ_WyvUrMKk9bEh1ux4s_te11rbeAEHH23ncmfbN8Ndf9gIQvYb0gGEuSabJ1PKIhKi0_AJac9Q1&GenericSearch.StartRow=1&GenericSearch.PageSize=15&GenericSearch.System=BC&GenericSearch.SearchType=2&GenericSearch.IndividualSearchText=&GenericSearch.EmploymingFirmSearchText=&GenericSearch.FirmSearchText=Blackstone%20Real%20&ZipCode=&Within=25&PageSize=15&StartRow=1&CurrentPage=11&IsPagination=false' --compressed")
          {
            page <-
              manager_c_url %>%
              parse_finra_c_url()

            nameEntityManager <-
              page %>%
              get_html_node_text(node_css = '.displayname')

            idSECCRD <-
              page %>%
              get_html_node_text(node_css = '.displaycrd') %>%
              str_replace_all('\\(|\\)|CRD#|SEC# ', '') %>%
              str_trim

            nameRelatedEntities <-
              page %>%
              get_html_node_text(node_css = '#contentTable i') %>%
              str_replace_all('Alternate Names: ', '')

            typeEntityRegulation <-
              page %>%
              get_html_node_text(node_css = '.TextBold div')

            addressEntity <-
              page %>%
              get_html_node_text(node_css = '.searchresulttext .bcsearchresultfirstcol div div div') %>%
              str_trim()

            if ((addressEntity %>% length) == (nameEntityManager %>% length)) {
              use_addresses <-
                T
            } else {
              use_addresses <-
                F
            }
            manager_data <-
              data_frame(nameEntityManager,
                         idSECCRD,
                         nameRelatedEntities) %>%
              separate(idSECCRD, sep = '\\ / ', into = c('idCRD', 'idSEC')) %>%
              suppressWarnings() %>%
              mutate(
                idCRD = idCRD %>% as.numeric,
                urlManagerSummaryADV = 'http://www.adviserinfo.sec.gov/IAPD/IAPDFirmSummary.aspx?ORG_PK=' %>% paste0(idCRD),
                urlPDFManagerADVBrochure = 'http://www.adviserinfo.sec.gov/IAPD/content/ViewForm/crd_iapd_stream_pdf.aspx?ORG_PK=' %>% paste0(idCRD)
              )

            if (use_addresses == T) {
              manager_data <-
                manager_data %>%
                mutate(addressEntity)
            }

            if (typeEntityRegulation %>% length() == nameEntityManager %>% length()) {
              manager_data <-
                manager_data %>%
                mutate(typeEntityRegulation)
            }
            return(manager_data)
          }


        search_c_urls <-
          get_finra_search_term_c_urls_safe(search_name = search_name)
        if (search_c_urls %>% is.null) {
          stop("No Data")
        }
        manager_data <-
          search_c_urls %>%
          map_df(function(x) {
            parse_finra_manager_search_data(manager_c_url = x)
          }) %>%
          mutate(nameSearch = search_name) %>%
          dplyr::select(nameSearch, everything())
        if (return_message) {
          "Extracted " %>%
            paste0(manager_data %>% nrow,
                   ' SEC regulated managers with a name containing ',
                   search_name) %>%
            message
        }
        return(manager_data)

      }

    get_finra_manager_metadata_safe <-
      possibly(get_finra_manager_metadata, NULL)

    managers_data <-
      search_names %>%
      map_df(function(x) {
        get_finra_manager_metadata_safe(search_name = x, return_message = return_message)
      }) %>%
      distinct()
    return(managers_data)
  }


# sec_adv_data ------------------------------------------------------------

parse_sec_manager_pdf_url <-
  function(url = 'http://www.adviserinfo.sec.gov/IAPD/Part2Brochures.aspx?ORG_PK=135952') {
    page <-
      url %>%
      get_html_page

    idCRD <-
      url %>%
      get_pk_url_crd()

    nameEntityBrochure <-
      page %>%
      get_html_node_text(node_css = 'td:nth-child(1) a') %>%
      str_trim()

    dateBrochureSubmitted <-
      page %>%
      get_html_node_text(node_css = 'td:nth-child(2)') %>%
      str_trim() %>%
      lubridate::mdy() %>%
      suppressWarnings()

    dateLastConfirmed <-
      page %>%
      get_html_node_text(node_css = 'td:nth-child(3)') %>%
      str_trim() %>%
      lubridate::mdy() %>%
      suppressWarnings()

    urlPDFManagerADVBrochure <-
      page %>%
      get_html_node_attributes(node_css = 'td:nth-child(1) a',
                               html_attribute = 'href',
                               base_url = 'http://www.adviserinfo.sec.gov')

    pdf_data <-
      data_frame(
        idCRD,
        nameEntityBrochure,
        urlPDFManagerADVBrochure,
        dateBrochureSubmitted,
        dateLastConfirmed
      )


    if (pdf_data %>% nrow > 1) {
      pdf_data <-
        pdf_data %>%
        gather(item,
               value,
               -c(idCRD, dateBrochureSubmitted, dateLastConfirmed)) %>%
        group_by(item) %>%
        mutate(idItem = (1:n() - 1),
               nameItem = if_else(idItem > 0, item %>% paste0(idItem), item)) %>%
        ungroup %>%
        dplyr::select(-c(item, idItem)) %>%
        spread(nameItem, value)
    }

    return(pdf_data)

  }


get_url_crd <-
  function(url = 'http://www.adviserinfo.sec.gov/IAPD/IAPDFirmSummary.aspx?ORG_PK=135952') {
    idCRD <-
      url %>%
      str_split('\\?ORG_PK=') %>%
      flatten_chr %>%
      .[2] %>%
      as.numeric()
    return(idCRD)
  }


get_manager_sec_page <-
  function(url = 'http://www.adviserinfo.sec.gov/IAPD/IAPDFirmSummary.aspx?ORG_PK=156663') {
    httr::set_config(config(ssl_verifypeer = 0L))
    page_status <-
      url %>%
      GET()

    if (page_status$url == 'http://www.adviserinfo.sec.gov/IAPD/SearchNoResult.aspx') {
      idCRD <-
        url %>%
        get_url_crd()

      manager_df <-
        data_frame(idCRD,
                   urlManagerSummaryADV = NA)
      "No data" %>% message
    } else {
      page <-
        url %>%
        get_html_page()

      name_entity_manager <-
        page %>%
        get_html_node_text(node_css = '#ctl00_cphMain_landing_lblActiveOrgName')

      idCRDSEC <-
        page %>%
        get_html_node_text(node_css = '#ctl00_cphMain_landing_lblActiveOrgCrd') %>%
        str_replace_all('\\(|\\)|CRD# |SEC#', '')

      get_all_status_data <-
        function(page) {
          parse_era_table <-
            function(page) {
              nodes_exist <-
                page %>%
                html_nodes('#tbERAStatus td') %>% length > 0

              if (nodes_exist) {
                statusJurisdictionERA <-
                  page %>%
                  html_nodes('#tbERAStatus td:nth-child(1)') %>%
                  html_text()

                statusReportingERA <-
                  page %>%
                  html_nodes('#tbERAStatus td:nth-child(2)') %>%
                  html_text()

                dateEffectiveERA <-
                  page %>%
                  html_nodes('#tbERAStatus td:nth-child(3)') %>%
                  html_text() %>%
                  lubridate::mdy() %>%
                  as.character()


                table_df <-
                  data_frame(statusJurisdictionERA,
                             statusReportingERA,
                             dateEffectiveERA) %>%
                  mutate(nameEntityManager = name_entity_manager,
                         countItem = 1:n()) %>%
                  dplyr::select(nameEntityManager, everything()) %>%
                  widen_adv_data() %>%
                  mutate_adv_data()

              } else {
                table_df <-
                  data_frame(nameEntityManager = name_entity_manager, isExempt = F)
              }
              return(table_df)
            }

          parse_state_table <-
            function(page) {
              nodes_exist <-
                page %>%
                html_nodes('#tbNtcStatus td') %>% length > 0

              if (nodes_exist) {
                stateRegistration <-
                  page %>%
                  html_nodes('#tbNtcStatus td:nth-child(1)') %>%
                  html_text()

                dateEffectiveState <-
                  page %>%
                  html_nodes('#tbNtcStatus td:nth-child(2)') %>%
                  html_text() %>%
                  lubridate::mdy() %>%
                  as.character()

                table_df <-
                  data_frame(stateRegistration, dateEffectiveState) %>%
                  mutate(nameEntityManager = name_entity_manager) %>%
                  dplyr::select(nameEntityManager, everything()) %>%
                  mutate(countItem = 1:n()) %>%
                  arrange(countItem) %>%
                  widen_adv_data() %>%
                  mutate_adv_data()

              } else {
                table_df <-
                  data_frame(nameEntityManager = name_entity_manager)
              }
              return(table_df)
            }

          parse_reg_status <-
            function(page) {
              nodes_exist <-
                page %>%
                html_nodes('#tbRegStatus td') %>% length > 0

              if (nodes_exist) {
                statusJurisdiction <-
                  page %>%
                  html_nodes('#tbRegStatus td:nth-child(1)') %>%
                  html_text()

                statusReporting <-
                  page %>%
                  html_nodes('#tbRegStatus td:nth-child(2)') %>%
                  html_text()

                dateEffective <-
                  page %>%
                  html_nodes('#tbRegStatus td:nth-child(3)') %>%
                  html_text() %>%
                  lubridate::mdy() %>%
                  as.character()

                table_df <-
                  data_frame(statusJurisdiction, statusReporting, dateEffective) %>%
                  mutate(nameEntityManager = name_entity_manager,
                         countItem = 1:n()) %>%
                  dplyr::select(nameEntityManager, everything()) %>%
                  widen_adv_data() %>%
                  mutate_adv_data()

              } else {
                table_df <-
                  data_frame(nameEntityManager = name_entity_manager, isExempt = F)
              }
              return(table_df)
            }

          era_df <-
            page %>%
            parse_era_table()

          reg_df <-
            parse_reg_status(page)

          state_df <-
            parse_state_table(page)

          all_df <-
            era_df %>%
            left_join(reg_df) %>%
            left_join(state_df) %>%
            suppressMessages()
          return(all_df)
        }

      manager_df <-
        page %>%
        get_all_status_data()

      urlManagerADV <-
        page %>%
        get_html_node_attributes(
          node_css = '#aspnetForm > div.container-fluid > div > nav > ul > li:nth-child(7) > a',
          html_attribute = 'href',
          base_url = 'http://www.adviserinfo.sec.gov'
        )

      manager_df <-
        manager_df %>%
        mutate(
          nameEntityManager = name_entity_manager,
          idCRDSEC,
          urlManagerSummaryADV = url,
          urlManagerADV
        ) %>%
        separate(idCRDSEC, sep = ' / ', into = c('idCRD', 'idSEC')) %>%
        mutate(idCRD = idCRD %>% as.numeric) %>%
        dplyr::select(nameEntityManager, idCRD, idSEC, everything())

      brochure_exists <-
        page %>%
        check_html_node(node_css = '#ctl00_cphMain_landing_p2BrochureLink')

      if (brochure_exists) {
        urlPDFManager <-
          page %>%
          get_html_node_attributes(
            node_css = '#aspnetForm > div.container-fluid > div > nav > ul > li:nth-child(8) > a',
            html_attribute = 'href',
            base_url = 'http://www.adviserinfo.sec.gov'
          )

        if (!'nameEntityManager' %in% names(manager_df)) {
          nameEntityManager <-
            page %>%
            get_entity_manager_name()
          manager_df <-
            manager_df %>%
            mutate(nameEntityManager)

        }

        manager_df <-
          manager_df %>%
          left_join(urlPDFManager %>%
                      parse_sec_manager_pdf_url()) %>%
          suppressMessages()
      }
      manager_df <-
        manager_df %>%
        dplyr::select(idCRD, nameEntityManager, everything())
    }
    return(manager_df)
  }


#' Get ADV metadata for specified ADV or search name
#'
#' @param search_names Names of the entities you want to search
#' @param crd_ids CRD ids you want to search
#' @param return_message
#'
#' @return
#' @export
#'
#' @examples
#' get_data_adv_managers_metadata(search_names = c('Divco'))
get_data_adv_managers_metadata <-
  function(search_names =  'Divco',
           crd_ids = NULL,
           return_message = T) {
    if (!crd_ids %>% is_null) {
      crd_urls <-
        'http://www.adviserinfo.sec.gov/IAPD/IAPDFirmSummary.aspx?ORG_PK=' %>%
        paste0(crd_ids)
    }
    if (!search_names %>% is_null) {
      finra_data <-
        search_names %>%
        get_finra_managers_metadata(return_message = return_message)
      adv_urls <-
        finra_data$urlManagerSummaryADV
    }

    if ((!crd_ids %>% is_null) & (!search_names %>% is_null)) {
      adv_urls <-
        c(adv_urls, crd_urls)
    }

    if ((!crd_ids %>% is_null) & (search_names %>% is_null)) {
      adv_urls <-
        c(crd_urls)
    }

    get_manager_sec_page_safe <-
      possibly(get_manager_sec_page, NULL)

    sec_summary_data <-
      adv_urls %>%
      map_df(get_manager_sec_page_safe) %>%
      suppressWarnings()

    search_data <-
      sec_summary_data

    return(search_data)
  }

get_manager_sec_adv_actual_url <-
  function(url = 'http://www.adviserinfo.sec.gov/IAPD/crd_iapd_AdvVersionSelector.aspx?ORG_PK=146629') {
    page <-
      url %>%
      GET

    actual_url <-
      page$url
    return(actual_url)
  }


get_url_primary_key <-
  function(url = 'http://www.adviserinfo.sec.gov/IAPD/content/viewform/adv/sections/iapd_AdvIdentifyingInfoSection.aspx?ORG_PK=146629&FLNG_PK=01285F220008018901086F5100319405056C8CC0') {
    url_primary_key <-
      url %>%
      str_split(pattern = 'FLNG_PK=') %>%
      flatten_chr() %>%
      .[2]
    return(url_primary_key)
  }


get_sec_sitemap_df <-
  function() {
    sitemap_df <-
      data_frame(
        nameSection = c(
          'Registration',
          "Part 1A Item 11 Disclosure Information",
          "DRPs",
          "Item 1 Identifying Information",
          "Item 2 SEC Registration/Reporting",
          "Item 3 Form Of Organization",
          "Item 4 Successions",
          "Item 5 Information About Your Advisory Business",
          "Item 6 Other Business Activities",
          "Item 7.A Financial Industry Affiliations",
          "Item 7.B Private Fund Reporting",
          "Item 8 Participation or Interest in Client Transactions",
          "Item 9 Custody",
          "Item 10 Control Persons",
          "Item 11 Disclosure Information",
          "Item 12 Small Businesses",
          "Schedule A",
          "Schedule B",
          "Schedule D",
          "Signature Page"
        ),
        idSection =
          c(
            'sectionRegistration',
            "sectionDisclosures",
            "sectionDRP",
            "section1IdentifyingInfo",
            "section2SECRegistration",
            "section3OrganizationForm",
            "section4Successions",
            "section5AdvisoryBusinessInformation",
            "section6OtherBusinessInformation",
            "section7AFinanceAffiliations",
            "section7BPrivateFundReporting",
            "section8ClientConflicts",
            "section9Custody",
            "section10ControlPersons",
            "section11Disclosures",
            "section12SmallBusiness",
            "sectionScheduleA",
            "sectionScheduleB",
            "sectionScheduleD",
            "sectionSignaturePage"
          ),
        nameData =
          c("data_registration", "data_disclosures", "data_drp", "data_1identifyinginfo",
            "data_2secregistration", "data_3organizationform", "data_4successions",
            "data_5advisorybusinessinformation", "data_6otherbusinessinformation",
            "data_7afinanceaffiliations", "data_7bprivatefundreporting",
            "data_8clientconflicts", "data_9custody", "data_10controlpersons",
            "data_11disclosures", "data_12smallbusiness", "data_schedulea",
            "data_scheduleb", "data_scheduled", "data_signaturepage"),
        nameFunction = c("get_manager_sec_page_safe", NA, "get_section_drp_safe",
                         "get_section_1_data_safe", "get_section_2_data_safe", "get_section_3_data_safe",
                         "get_section_4_data_safe", "get_section_5_data_safe", "get_section_6_data_safe",
                         "get_section_7a_data_safe", "get_section_7b_data_safe", "get_section_8_data_safe",
                         "get_section_9_data_safe", "get_section_10_data_safe", "get_section_11_data_safe",
                         "get_section_12_data_safe", "get_schedule_a_data_safe", "get_schedule_b_data_safe",
                         "get_schedule_d_data_safe", "get_manager_signatory_data_safe"
        ),
        nameSectionActual = c(
          "Registration",
          "Part 1A Item 11 Disclosure Information",
          "DRPs",
          "Identifying Information",
          "SEC Reporting",
          "Organization",
          "Successions",
          "Advisory Business Information",
          "Other Business Activities",
          "Financial Industry Affiliations",
          "Private Fund Reporting",
          "Participation or Interest in Client Transactions",
          "Custody",
          "Control Persons",
          "Disclosure Information",
          "Small Businesses",
          "Direct Manager Owners",
          "Indirect Manager Owners",
          "Other Manager Information",
          "Manager Signatories"
        )

      )
    return(sitemap_df)
  }


#' Returns data frame of SEC ADV sitemap
#'
#' @return
#' @export
#' @import dplyr
#' @examples
get_data_sec_adv_manager_sitemap <-
  function() {
    sitemap_df <-
      data_frame(
        idSection =
          c(
            'sectionRegistration',
            "sectionDisclosures",
            "sectionDRP",
            "section1IdentifyingInfo",
            "section2SECRegistration",
            "section3OrganizationForm",
            "section4Successions",
            "section5AdvisoryBusinessInformation",
            "section6OtherBusinessInformation",
            "section7AFinanceAffiliations",
            "section7BPrivateFundReporting",
            "section8ClientConflicts",
            "section9Custody",
            "section10ControlPersons",
            "section11Disclosures",
            "section12SmallBusiness",
            "sectionScheduleA",
            "sectionScheduleB",
            "sectionScheduleD",
            "sectionSignaturePage"
          ),
        nameSection = c(
          'Registration',
          "Part 1A Item 11 Disclosure Information",
          "DRPs",
          "Item 1 Identifying Information",
          "Item 2 SEC Registration/Reporting",
          "Item 3 Form Of Organization",
          "Item 4 Successions",
          "Item 5 Information About Your Advisory Business",
          "Item 6 Other Business Activities",
          "Item 7.A Financial Industry Affiliations",
          "Item 7.B Private Fund Reporting",
          "Item 8 Participation or Interest in Client Transactions",
          "Item 9 Custody",
          "Item 10 Control Persons",
          "Item 11 Disclosure Information",
          "Item 12 Small Businesses",
          "Schedule A",
          "Schedule B",
          "Schedule D",
          "Signature Page"
        ),
        nameSectionActual = c(
          "Registration",
          "Part 1A Item 11 Disclosure Information",
          "DRPs",
          "Identifying Information",
          "SEC Reporting",
          "Organization",
          "Successions",
          "Advisory Business Information",
          "Other Business Activities",
          "Financial Industry Affiliations",
          "Private Fund Reporting",
          "Participation or Interest in Client Transactions",
          "Custody",
          "Control Persons",
          "Disclosure Information",
          "Small Businesses",
          "Direct Manager Owners",
          "Indirect Manager Owners",
          "Other Manager Information",
          "Manager Signatories"
        )

      )
    return(sitemap_df)
  }


get_location_name_df <-
  function() {
    location_name_df <-
      data_frame(
        itemNode = c(
          "Number and Street 1",
          'Number and Street 2',
          "City",
          "State",
          "Country",
          "ZIP+4/Postal Code"
        ),
        nameNode = c(
          "addressStreet1",
          'addressStreet2',
          "city",
          "state",
          "country",
          "zip"
        )
      )
    return(location_name_df)
  }

get_sitemap_urls <-
  function(site_map_df, section_name = 'section7BPrivateFundReporting') {
    urls_exist <-
      site_map_df %>%
      dplyr::filter(idSection %in% section_name) %>%
      .$urlADVSection %>% length > 0
    if (urls_exist) {
      urls <-
        site_map_df %>%
        dplyr::filter(idSection %in% section_name) %>%
        .$urlADVSection
      return(urls)
    }
  }


parse_adv_manager_sitemap_df <-
  function(url = 'http://www.adviserinfo.sec.gov/IAPD/crd_iapd_AdvVersionSelector.aspx?ORG_PK=135952',
           return_wide = F) {

    idCRD <-
      url %>%
      get_url_crd()

    if (idCRD %>% is.na) {
      idCRD <-
        url %>%
        str_split('\\?ORG_PK=') %>%
        flatten_chr %>%
        .[2] %>%
        str_split('\\&') %>%
        flatten_chr %>%
        .[[1]] %>%
        as.numeric() %>%
        suppressWarnings()
    }

    actual_url <-
      url %>%
      get_manager_sec_adv_actual_url()

    url_primary_key <-
      actual_url %>%
      get_url_primary_key()

    page <-
      actual_url %>%
      get_html_page()

    base_url <-
      actual_url %>%
      str_split('sections|Sections') %>%
      flatten_chr() %>%
      .[[1]]

    name_entity_manager <-
      page %>%
      get_entity_manager_name()

    if (name_entity_manager %>% is.na) {
      name_entity_manager <-
        page %>%
        html_nodes('.summary-displayname') %>%
        html_text
    }

    items <-
      page %>%
      get_html_node_text('.sidebar a[href^=".."]') %>%
      str_trim()

    values <-
      page %>%
      get_html_node_attributes(node_css = '.sidebar a[href^=".."]',
                               'href') %>%
      str_trim() %>%
      str_replace('../', '') %>%
      paste0(base_url,
             .) %>%
      unique

    adv_sitemap_df <-
      data_frame(idCRD, nameSection = 'Registration', urlADVSection = idCRD %>% paste0('http://www.adviserinfo.sec.gov/IAPD/IAPDFirmSummary.aspx?ORG_PK=',.)) %>%
      bind_rows(data_frame(idCRD,
                           nameSection = items,
                           urlADVSection = values)) %>%
      left_join(get_sec_sitemap_df()) %>%
      distinct() %>%
      dplyr::filter(!nameFunction %>% is.na) %>%
      dplyr::select(-nameSection) %>%
      mutate(nameEntityManager = name_entity_manager,
             idRow = 1:n()) %>%
      group_by(idSection) %>%
      dplyr::filter(idRow == min(idRow)) %>%
      ungroup %>%
      dplyr::select(-idRow) %>%
      dplyr::select(idCRD, nameEntityManager, everything()) %>%
      suppressMessages()

    if (return_wide) {
      adv_sitemap_df <-
        adv_sitemap_df %>%
        spread(idSection, urlADVSection)

    }
    return(adv_sitemap_df)
  }

get_managers_adv_sitemap_adv <-
  function(idCRDs = c(109110),
           search_names =  NULL) {
    get_data_adv_managers_metadata_safe <-
      possibly(get_data_adv_managers_metadata, NULL)
    parse_adv_manager_sitemap_df_safe <-
      possibly(parse_adv_manager_sitemap_df, NULL)

    if (!idCRDs %>% is_null) {
      urls <-
        idCRDs %>%
        paste0('http://www.adviserinfo.sec.gov/IAPD/crd_iapd_AdvVersionSelector.aspx?ORG_PK=',.)
    }

    if (!search_names %>% is_null) {
      manager_data <-
        search_names %>%
        map_df(function(x) {
          get_data_adv_managers_metadata_safe(crd_ids = idCRDs, search_names = x, return_message = T)
        })

      manager_data <-
        manager_data %>%
        dplyr::filter(!urlManagerADV %>% is.na)
    }

    if ('urls' %>% exists & 'manager_data' %>% exists) {
      urls <-
        c(urls, manager_data$urlManagerADV %>% unique) %>% unique
    }

    if (idCRDs %>% is_null & 'manager_data' %>% exists) {
      urls <-
        manager_data$urlManagerADV %>% unique
    }

    if ('urls' %>% exists & search_names %>% is_null) {
      manager_data <-
        get_data_adv_managers_metadata_safe(crd_ids = idCRDs, search_names = NULL)

      name_entity_manager <-
        manager_data$nameEntityManager
    }
    sitemap_dfs <-
      urls %>%
      unique %>%
      map_df(function(x) {
        parse_adv_manager_sitemap_df_safe(url = x, return_wide = F)
      })

    sitemap_dfs <-
      sitemap_dfs %>%
      dplyr::filter(!nameEntityManager %>% is.na) %>%
      left_join(manager_data %>% dplyr::select(idCRD, nameEntityManager)) %>%
      dplyr::select(idCRD,
                    nameSectionActual,
                    nameEntityManager,
                    idSection,
                    nameData,
                    nameFunction,
                    urlADVSection) %>%
      suppressMessages()

    return(sitemap_dfs)
  }


get_type_manager_entity_owner_df <-
  function() {
    type_df <-
      data_frame(
        idTypeEntityManagerOwner = c("I", "DE", "FE"),
        typeEntityManagerOwner = c('Individual', 'Domestic Entity', 'Foreign Entity'),
        isEntityOwnerManagerEntity = c(F, T, T)
      )
    return(type_df)
  }

get_range_entity_owner_df <-
  function() {
    range_df <-
      data_frame(
        idRangeManagerEntityOwnership = c(NA, LETTERS[1:6]),
        rangeManagerEntityOwnership = c(
          "< 5%",
          ">= 5%, < 10%",
          ">= 10%, < 25%",
          ">= 25%, < 50%",
          ">= 50%, < 75%",
          ">= 75%",
          "Other (general partner, trustee, or elected manager)"
        )
      )
    return(range_df)
  }

parse_manager_owner_name <-
  function(x = "GRAY, ROBERT, LAWRENCE") {
    count_commas <-
      x %>% str_count('\\, ')

    full_name <-
      x %>% str_split('\\, ') %>% flatten_chr

    if (count_commas == 1) {
      name_first <-
        full_name[2]
      name_middle <-
        NA
      name_last <-
        full_name[1]

      name_full <-
        name_first %>% paste(name_last)

      name_common <-
        name_full
    }

    if (count_commas == 2) {
      name_first <-
        full_name[2]

      name_middle <-
        full_name[3]

      name_last <-
        full_name[1]

      name_full <-
        name_first %>% paste(name_middle, name_last)

      name_common <-
        name_first %>% paste(name_last)
    }

    if (count_commas == 3) {
      name_first <-
        full_name[3]

      name_middle <-
        full_name[4]

      name_last <-
        full_name[1] %>%
        paste(full_name[2], collapse = '')

      name_full <-
        name_first %>% paste(name_middle, name_last)

      name_common <-
        name_first %>% paste(name_last)
    }

    name_df <-
      data_frame(
        nameEntityManagerOwner = x,
        nameCommonEntityOwnerManager = name_common,
        nameFullEntityOwnerManager = name_full,
        nameFirstEntityManagerOwner = name_first,
        nameMiddleEntityManagerOwner = name_middle,
        nameLastEntityManagerOwner = name_last
      ) %>%
      mutate_all(str_to_upper)

    return(name_df)
  }

get_pk_url_crd <-
  function(url = 'http://www.adviserinfo.sec.gov/IAPD/content/viewform/adv/Sections/iapd_AdvPrivateFundReportingSection.aspx?ORG_PK=162351&FLNG_PK=052DAAB400080184043CE66005E35E29056C8CC0') {
    idCRD <-
      url %>%
      str_split('=') %>%
      flatten_chr() %>%
      .[2] %>% str_replace_all('&FLNG_PK', '') %>%
      as.numeric
    return(idCRD)
  }


get_sitemap_filter_url <-
  function(site_map_df, filter_name = 'sectionScheduleA') {
    urls <-
      site_map_df %>%
      dplyr::filter(idSection == filter_name) %>%
      .$urlADVSection
    return(urls)
  }

# private fund data -------------------------------------------------------
get_check_box_value_df <-
  function() {
    node_check_df <-
      data_frame(
        nodeName = c(
          "Checkbox checked, changed",
          "Checkbox not checked",
          "Radio button not selected",
          "Radio button selected, changed"
        ),
        isNodeChecked = c(TRUE, FALSE, FALSE,
                          TRUE)
      )
    return(node_check_df)
  }

get_form_check_box_df <-
  function(modify = T) {
    form_value_df <-
      data_frame(
        idRow = 1:52,
        nameItem = c(
          'hasICA1940S31Exclusion.TRUE',
          'hasICA1940S37Exclusion.TRUE',
          'isMasterFund.TRUE',
          'isMasterFund.FALSE',
          'isFeederFund.TRUE',
          'isFeederFund.FALSE',
          'isFundOfFund.TRUE',
          'isFundOfFund.FALSE',
          'isFundOfFundRelatedInvestment.TRUE',
          'isFundOfFundRelatedInvestment.FALSE',
          'isICA1940PrivateSecuritiesInvestor.TRUE',
          'isICA1940PrivateSecuritiesInvestor.FALSE',
          'isHedgeFund.TRUE',
          'isLiquidityFund.TRUE',
          'isPrivateEquityFund.TRUE',
          'isRealEstateFund.TRUE',
          'isSecuritizedAssetFund.TRUE',
          'isVentureFund.TRUE',
          'isOtherFund.TRUE',
          'hasPrivateFundSubAdviser.TRUE',
          'hasPrivateFundSubAdviser.FALSE',
          'hasOtherPrivateFundAdviser.TRUE',
          'hasOtherPrivateFundAdviser.FALSE',
          'hasSolicitedClientsForFund.TRUE',
          'hasSolicitedClientsForFund.FALSE',
          'hasREGDExemption.TRUE',
          'hasREGDExemption.FALSE',
          'isFundAuditedAnnually.TRUE',
          'isFundAuditedAnnually.FALSE',
          'isFundAuditGAAPCompliant.TRUE',
          'isFundAuditGAAPCompliant.FALSE',
          'isFundAuditorIndependent.TRUE',
          'isFundAuditorIndependent.FALSE',
          'isFundAuditorPCAOBRegistered.TRUE',
          'isFundAuditorPCAOBRegistered.FALSE',
          'isFundAuditorPCAOBCompliant.TRUE',
          'isFundAuditorPCAOBCompliant.FALSE',
          'hasInvestorAnnualFinancialStatement.TRUE',
          'hasInvestorAnnualFinancialStatement.FALSE',
          'hasAuditorUnqualifiedOpinion.TRUE',
          'hasAuditorUnqualifiedOpinion.FALSE',
          'hasAuditorUnqualifiedOpinion.NA',
          'hasFundPrimeBroker.TRUE',
          'hasFundPrimeBroker.FALSE',
          'hasFundCustodian.TRUE',
          'hasFundCustodian.FALSE',
          'isCustodianRelatedPerson.TRUE',
          'isCustodianRelatedPerson.FALSE',
          'hasFundAdministrator.TRUE',
          'hasFundAdministrator.FALSE',
          'hasFundPlacementAgent.TRUE',
          'hasFundPlacementAgent.FALSE'
        )
      )
    if (modify) {
      form_value_df <-
        form_value_df <-
        data_frame(
          nameItem = c(
            'isMasterFund.TRUE',
            'isMasterFund.FALSE',
            'isFeederFund.TRUE',
            'isFeederFund.FALSE',
            'isFundOfFund.TRUE',
            'isFundOfFund.FALSE',
            'isFundOfFundRelatedInvestment.TRUE',
            'isFundOfFundRelatedInvestment.FALSE',
            'isFundICASecuritiesInvestor.TRUE',
            'isFundICASecuritiesInvestor.FALSE',
            'isHedgeFund.TRUE',
            'isLiquidityFund.TRUE',
            'isPrivateEquityFund.TRUE',
            'isRealEstateFund.TRUE',
            'isSecuritizedAssetFund.TRUE',
            'isVentureFund.TRUE',
            'isOtherFund.TRUE',
            'hasPrivateFundSubAdviser.TRUE',
            'hasPrivateFundSubAdviser.FALSE',
            'hasOtherPrivateFundAdviser.TRUE',
            'hasOtherPrivateFundAdviser.FALSE',
            'hasSolicitedClientsForFund.TRUE',
            'hasSolicitedClientsForFund.FALSE',
            'hasREGDExemption.TRUE',
            'hasREGDExemption.FALSE',
            'isFundAuditedAnnually.TRUE',
            'isFundAuditedAnnually.FALSE',
            'isFundAuditGAAPCompliant.TRUE',
            'isFundAuditGAAPCompliant.FALSE',
            'isFundAuditorIndependent.TRUE',
            'isFundAuditorIndependent.FALSE',
            'isFundAuditorPCAOBRegistered.TRUE',
            'isFundAuditorPCAOBRegistered.FALSE',
            'isFundAuditorPCAOBCompliant.TRUE',
            'isFundAuditorPCAOBCompliant.FALSE',
            'hasInvestorAnnualFinancialStatement.TRUE',
            'hasInvestorAnnualFinancialStatement.FALSE',
            'hasAuditorUnqualifiedOpinion.TRUE',
            'hasAuditorUnqualifiedOpinion.FALSE',
            'hasAuditorUnqualifiedOpinion.NA',
            'hasFundPrimeBroker.TRUE',
            'hasFundPrimeBroker.FALSE'
          )
        ) %>%
        mutate(idRow = 1:n()) %>%
        dplyr::select(idRow, nameItem)
    }
    return(form_value_df)
  }



get_fund_table_html <-
  function(page = page,
           table_number = 1,
           table_css_node_df = table_css_node_df) {
    node_css <-
      table_css_node_df %>%
      dplyr::filter(numberTable == table_number) %>%
      .$tableCSSNode

    table_html <-
      page %>%
      html_nodes(css = node_css)

    return(table_html)
  }

get_table_check_box_df <-
  function(page = page,
           table_number = 1,
           table_css_node_df,
           only_radio = T) {
    node_table <-
      get_fund_table_html(
        page = page,
        table_number = table_number,
        table_css_node_df = table_css_node_df
      )

    table_checked_nodes <-
      node_table %>%
      html_nodes('img') %>%
      html_attr('alt') %>%
      str_trim()

    if (only_radio) {
      table_checked_nodes <-
        table_checked_nodes[table_checked_nodes %>%
                              str_detect("Radio")]
    }

    check_box_df <-
      data_frame(nodeName = table_checked_nodes) %>%
      mutate(idTable = table_number,
             idImageNode = 1:n()) %>%
      left_join(get_check_box_value_df()) %>%
      suppressMessages() %>%
      dplyr::select(idTable, idImageNode, nodeName, isNodeChecked)
    return(check_box_df)
  }

get_tables_check_boxes_df <-
  function(page = page,
           table_number = 1,
           table_css_node_df = table_css_node_df) {
    tables_check_boxes_df <-
      table_css_node_df$numberTable %>%
      map_df(function(x) {
        get_table_check_box_df(
          page = page,
          table_number = x,
          table_css_node_df = table_css_node_df
        )
      })
    return(tables_check_boxes_df)
  }

get_table_check_box_data <-
  function(page = page,
           table_number = 1,
           table_css_node_df,
           only_radio = T) {
    form_value_df <-
      get_form_check_box_df(modify = T)

    check_box_df <-
      get_table_check_box_df(
        page = page,
        table_number = table_number,
        table_css_node_df = table_css_node_df,
        only_radio = T
      )

    form_value_df <-
      form_value_df %>%
      separate(nameItem,
               into = c('nameItem', 'logicalResponse'),
               sep = '\\.') %>%
      suppressWarnings() %>%
      mutate(logicalResponse = logicalResponse %>% as.logical)

    response_df <-
      check_box_df %>%
      slice(1:nrow(form_value_df)) %>%
      left_join(form_value_df %>% dplyr::rename(idImageNode = idRow)) %>%
      dplyr::filter(isNodeChecked == T) %>%
      dplyr::filter(logicalResponse == T) %>%
      dplyr::select(nameItem, logicalResponse) %>%
      suppressWarnings() %>%
      suppressMessages()

    x <-
      response_df$nameItem

    fund_type <-
      case_when(
        x == 'isHedgeFund' ~ "Hedge Fund",
        x == 'isLiquidityFund' ~ "Liquidity",
        x == 'isPrivateEquityFund' ~ "Private Equity",
        x == 'isRealEstateFund' ~ "Real Estate",
        x == 'isSecuritizedAssetFund' ~ "Securitized Asset",
        x == 'isVentureFund' ~ "Venture",
        x == 'isOtherFund' ~ "Other",
        FALSE ~ as.character(x)
      )

    fund_type <-
      fund_type[!fund_type %>% is.na()]

    response_df <-
      response_df %>%
      dplyr::filter(
        !nameItem %in% c(
          'isHedgeFund',
          'isLiquidityFund',
          'isPrivateEquityFund',
          'isRealEstateFund',
          'isSecuritizedAssetFund',
          'isVentureFund',
          'isOtherFund'
        )
      )

    column_order <-
      response_df$nameItem

    response_df <-
      response_df %>%
      spread(nameItem, logicalResponse) %>%
      dplyr::select(one_of(column_order)) %>%
      mutate(numberFund = table_number,
             typeFund = fund_type) %>%
      dplyr::select(numberFund, typeFund, everything())
    return(response_df)
  }

parse_table_node_df <-
  function(page = page,
           table_number = 1,
           table_css_node_df = table_css_node_df) {
    base_table_html <-
      get_fund_table_html(
        page = page,
        table_number = table_number,
        table_css_node_df = table_css_node_df
      ) %>%
      html_nodes('.PaperFormTableData tr td')

    table_data_nodes <-
      base_table_html %>%
      html_text(trim = T) %>%
      str_replace_all('\r|\n|\t', '') %>%
      stri_replace_all_charclass("\\p{WHITE_SPACE}", " ") %>%
      stri_trim_both() %>%
      gsub("^\\s+|\\s+$", "", .)

    table_data_nodes <-
      table_data_nodes[!table_data_nodes == '']

    table_node_df <-
      1:length(table_data_nodes) %>%
      map_df(function(x) {
        data_nodes <-
          table_data_nodes[[x]] %>%
          str_split('  ') %>%
          flatten_chr

        if (data_nodes[!data_nodes == ''] %>% length() > 0) {
          data_nodes <-
            data_nodes[!data_nodes == '']

          nodeText <-
            data_nodes %>% paste0(collapse = ' ')

          nodeText <-
            nodeText %>%
            str_replace_all(
              'Name of the marketer: |Legal name of custodian: |Primary business name of custodian: |CRD Number (if any):|Name of the prime broker: ',
              ''
            )

          isLetter <-
            (nodeText %>% str_detect("^\\([a-z]")) &
            (nodeText %>% nchar < 10)

          isNumberSection <-
            (nodeText %>% str_detect("^[1-9].")) &
            (nodeText %>% nchar < 4) &
            (nodeText %>% str_detect('\\.'))

          if (isNumberSection) {
            numberSection <-
              nodeText %>% str_extract_all("^[1-9].") %>% flatten_chr %>% as.numeric
          } else {
            numberSection <-
              NA
          }

          if (isLetter) {
            letterSection <-
              nodeText %>% str_extract_all("^\\([a-z]") %>% flatten_chr %>%
              str_replace_all('\\(', '')
          } else {
            letterSection <-
              NA
          }

          remove_node <-
            nodeText %>% str_detect('Include only those assets where')

          if (remove_node) {
            letterSection <-
              "g"
            isLetter <-
              T
          }

          if (nodeText == 'B. SERVICE PROVIDERS') {
            letterSection <-
              "c"
            isLetter <-
              T
          }
          if (nodeText == 'Your Advisory Services') {
            letterSection <-
              "q"
            isLetter <-
              T
          }

          if (nodeText %>% str_detect('\\CRD Number')) {
            nodeText <-
              nodeText %>%
              gsub('\\CRD Number', '', .) %>%
              gsub('\\(if any):', '', .) %>%
              gsub('and', '', .) %>%
              str_trim()

            letterSection <-
              "cc"

          }

          if (nodeText == 'Private Offering') {
            letterSection <-
              "c"
            isLetter <-
              T
          }

          if (nodeText == 'Administrator') {
            letterSection <-
              "g"
            isLetter <-
              T
          }

          if (nodeText == 'No Information Filed') {
            nodeText <-
              NA
          }

          if (isLetter | isNumberSection) {
            nodeText <-
              NA
          }
          data_frame(idNode = x, numberSection, letterSection, nodeText)
        }
      }) %>%
      fill(numberSection, letterSection) %>%
      dplyr::filter(!nodeText %>% is.na) %>%
      dplyr::filter(!numberSection %>% is.na) %>%
      dplyr::filter(!nodeText %>% str_detect("^NOTE:|Ownership")) %>%
      unite(
        numberLetterSection,
        numberSection,
        letterSection,
        sep = '',
        remove = F
      ) %>%
      mutate(numberTable = table_number) %>%
      dplyr::select(numberTable, everything())
    return(table_node_df)
  }

extract_node_data <-
  function(table_node_df,
           variable_name = 'pctFundNonUSCitizens',
           section_letter = "16b",
           method = "max") {
    options(scipen = 99999)
    letter_section_location <-
      c('7c', '23c', '24d', '25d', '26c', '28e')
    letter_section_other <-
      c(
        '7a',
        '7b',
        '7d',
        '23b',
        '24a',
        '24b',
        '24c',
        '24cc',
        '25a',
        '25b',
        '25c',
        '25f',
        '25cc',
        '26a',
        '26b',
        '28c',
        '28d',
        '28cc',
        '28g'
      )

    is_location <-
      variable_name %>% str_detect("location")
    if (is_location) {
      method <-
        'location'
    } else {
      method <-
        'max'
    }
    if (!method %>% str_to_lower() %in% c('max', 'location')) {
      stop('Method can only be ')
    }

    if (section_letter %in% table_node_df$numberLetterSection) {
      node_df <-
        table_node_df %>%
        dplyr::filter(numberLetterSection == section_letter)

      if ((method == 'max') &
          (!section_letter %in% letter_section_other)) {
        node_value <-
          node_df %>%
          dplyr::filter(idNode == max(idNode)) %>%
          .$nodeText
        if (variable_name %in% c('nameFundGPManagerTrusteeDirector'))  {
          node_value <-
            node_df %>% slice(-c(1:2)) %>%
            .$nodeText

          if (node_value %>% length > 1) {
            node_value <-
              node_value %>% paste0(collapse = ' - ')
          }
        }

        if (variable_name %in% c('idSECCRDPrimeBroker', 'idSECCRDCustodian'))  {
          node_value <-
            node_df %>%
            dplyr::filter(!nodeText %>% str_detect('If')) %>%
            .$nodeText

          if (node_value %>% length > 1) {
            node_value <-
              node_value %>% paste0(collapse = ' & ')
          }
        }
        has_currency <-
          node_value %>% str_detect('\\$') %>% unique
        if (has_currency) {
          node_value <-
            node_value %>% str_replace('\\$', '') %>%
            str_trim %>%
            readr::parse_number
        }
        if (variable_name %>% str_detect("pct")) {
          node_value <-
            node_value %>% str_replace('\\%', '') %>% str_trim %>% as.numeric
        }
        if (node_value %>% str_detect('\\$')) {
          node_value <-
            node_value %>% str_replace('\\$|\\', '')
        }
        if ((variable_name == 'idFundFormD') &
            (node_value %>% str_detect("-") == F)) {
          node_value <-
            NA
        }
      }
      if ((method == 'location') &
          (!section_letter %in% c('7c', '23c', '24d', '25d', '26c'))) {
        node_value <-
          node_df %>%
          dplyr::filter(nodeText %>% str_detect('City:|State:|Country')) %>%
          .$nodeText %>%
          str_replace_all('City:|State:|Country:', '')

        node_value <-
          node_value[!node_value == ''] %>% str_trim
        if (node_value %>% length > 1) {
          node_value <-
            node_value %>%
            paste0(collapse = ', ') %>%
            str_trim %>% str_to_upper
        } else {
          node_value <-
            node_value %>% str_trim %>% str_to_upper
        }
      }
      if (section_letter %in% letter_section_other) {
        hitwords <-
          'Name|Private fund|If you are filing |Additional Feeder Fund|If the|Yes|No|The location|Additional|If the marketer is registered|Website Address|If the answer to 28(f)|Click the button|If you or another adviser'
        has_node_length <-
          node_df %>%
          dplyr::filter(!nodeText %>% str_detect(hitwords)) %>%
          .$nodeText %>% length > 0
        if (has_node_length) {
          node_value <-
            node_df %>%
            dplyr::filter(!nodeText %>% str_detect(hitwords)) %>%
            .$nodeText

          if ((node_value %>% length) > 1) {
            vars_names <-
              c(variable_name,
                1:(length(node_value) - 1) %>% paste0(variable_name, .))
          }
        } else {
          node_value <-
            NA
        }
      }
      if (section_letter %in% letter_section_location) {
        hit_words <-
          c("The",
            "Under",
            'Location of prime',
            'The location of',
            'Location of') %>%
          paste0(collapse = '|')

        locations_df <-
          node_df %>%
          dplyr::select(idNode, nodeText) %>%
          dplyr::filter(!nodeText %>% str_detect('Yes|No')) %>%
          mutate(idLocationNode = ifelse(nodeText %>% str_detect(hit_words), idNode, NA))

        count_df <-
          locations_df %>%
          dplyr::filter(nodeText %>% str_detect(hit_words)) %>%
          mutate(countLocation = 1:n()) %>%
          dplyr::select(countLocation, idLocationNode, nodeText)

        locations_df <-
          locations_df %>% left_join(count_df) %>%
          fill(countLocation) %>%
          dplyr::filter(!nodeText %>% str_detect("The")) %>%
          dplyr::select(countLocation, nodeText) %>%
          suppressMessages()

        count_locations <-
          locations_df$countLocation %>% unique

        node_value <-
          count_locations %>%
          map_chr(function(x) {
            node_vals <-
              locations_df %>%
              dplyr::filter(countLocation == x) %>%
              dplyr::filter(nodeText %>% str_detect('City:|State:|Country')) %>%
              .$nodeText %>%
              str_replace_all('City:|State:|Country:', '')
            node_vals <-
              node_vals[!node_vals == ''] %>% str_trim
            if (node_vals %>% length > 1) {
              node_vals <-
                node_vals %>%
                paste0(collapse = ', ') %>%
                str_trim
            } else {
              node_vals <-
                node_vals %>% str_trim
            }
            return(node_vals %>% str_to_upper)
          })

        if ((node_value %>% length) > 1) {
          vars_names <-
            c(variable_name,
              1:(length(node_value) - 1) %>% paste0(variable_name, .))
        }
      }

    } else {
      node_value <-
        NA
    }

    if ('vars_names' %>% exists()) {
      variable_name <-
        vars_names
      rm(vars_names) %>% suppressWarnings()
    }

    node_df <-
      data_frame(nameVariable = variable_name,
                 valueNode = node_value) %>%
      mutate(sectionLetter = section_letter) %>%
      dplyr::select(sectionLetter, everything())

    return(node_df)
  }

get_section_matrix_df <-
  function() {
    section_matrix_df <-
      data_frame(
        sectionLetter = c(
          "1a",
          "1b",
          "2b",
          "3b",
          "7a",
          "7b",
          "7c",
          "7d",
          "11b",
          "12b",
          "13b",
          "14b",
          "15b",
          "16b",
          "20b",
          "22c",
          "23b",
          "23c",
          "24b",
          "24c",
          '24cc',
          "24d",
          "25b",
          "25c",
          "25d",
          "25f",
          '25cc',
          "26b",
          "26c",
          "27f",
          '28c',
          '28d',
          '28cc',
          '28e',
          '28g'
        ),
        variableName =  c(
          "nameFund",
          "idPrivateFund",
          "locationFundIncorporation",
          "nameFundGPManagerTrusteeDirector",
          "nameFeederFund",
          "idPrivateFundFeeder",
          "locationFeederFund",
          "nameFundFeederGPManagerTrusteeDirector",
          "amountFundGrossAUM",
          "amountFundMinimumInvestment",
          "countFundOwners",
          "pctFundManagerOwned",
          "pctFundFundOfFunds",
          "pctFundNonUSCitizens",
          "pctClientsFundInvestor",
          "idFundFormD",
          "nameAuditorFund",
          "locationAuditorFund",
          "namePrimeBroker",
          "idSECPrimeBroker",
          'idCRDPrimeBroker',
          "locationPrimeBroker",
          "nameCustodianLegal",
          "nameCustodianBusiness",
          "locationCustodian",
          "idSECCustodian",
          'idCRDCustodian',
          "nameAdministratorFund",
          "locationAdministratorFund",
          "pctFund3rdPartyValued",
          'nameFundMarketer',
          'idSECFundMarketer',
          'idCRDFundMarketer',
          'locationFundMarkter',
          'urlFundMarketer'

        )
      )
    return(section_matrix_df)
  }

parse_funds_tables <-
  function(page,
           table_css_node_df = table_css_node_df,
           section_matrix_df = section_matrix_df,
           return_message = T) {

    parse_fund_table <-
      function(page,
               table_number = 1,
               table_css_node_df = table_css_node_df,
               section_matrix_df = section_matrix_df) {
        table_node_df <-
          parse_table_node_df(
            page = page,
            table_number = table_number,
            table_css_node_df = table_css_node_df
          )

        response_df <-
          get_table_check_box_data(
            page = page,
            table_number = table_number,
            table_css_node_df = table_css_node_df,
            only_radio = T
          )

        fund_table_data <-
          1:nrow(section_matrix_df) %>%
          map_df(function(x) {
            table_node_df %>% extract_node_data(
              variable_name = section_matrix_df$variableName[x],
              section_letter = section_matrix_df$sectionLetter[x],
              method = 'max'
            ) %>%
              mutate(valueNode = valueNode %>% as.character)
          }) %>%
          suppressWarnings() %>%
          mutate(valueNode = ifelse(valueNode %in% c('', '-'), NA, valueNode)) %>%
          dplyr::filter(!valueNode %>% is.na)

        col_order <-
          fund_table_data$nameVariable

        fund_table_data <-
          fund_table_data %>%
          dplyr::select(-sectionLetter) %>%
          spread(nameVariable, valueNode) %>%
          dplyr::select(one_of(col_order)) %>%
          mutate(numberFund = table_number) %>%
          dplyr::select(numberFund, everything())

        fund_table_data <-
          fund_table_data %>%
          mutate_at(.cols =
                      fund_table_data %>% dplyr::select(matches("^count|amount")) %>% names,
                    .funs = as.numeric) %>%
          mutate_at(
            .cols =
              fund_table_data %>% dplyr::select(matches("^pct")) %>% names,
            .funs = funs(. %>% as.numeric() / 100)
          )
        fund_table_data <-
          fund_table_data %>%
          left_join(response_df) %>%
          dplyr::select(numberFund:nameFund, typeFund, everything()) %>%
          suppressMessages()

        return(fund_table_data)
      }

    parse_fund_table_safe <-
      possibly(parse_fund_table, NULL)

    table_numbers <-
      table_css_node_df$numberTable

    all_table_data <-
      table_numbers %>%
      map_df(function(x) {
        parse_fund_table_safe(
          page = page,
          table_number = x,
          table_css_node_df = table_css_node_df,
          section_matrix_df = section_matrix_df
        )
      })
    return(all_table_data)
  }


# form_sections -----------------------------------------------------------
get_section_1_data <-
  function(url = 'http://www.adviserinfo.sec.gov/IAPD/content/viewform/adv/sections/iapd_AdvIdentifyingInfoSection.aspx?ORG_PK=165609&FLNG_PK=011CBE92000801870387C7310019743D056C8CC0') {

    idCRD <-
      url %>%
      get_pk_url_crd()

    get_node_item_df <- function() {
      node_item_df <-
        c(
          'hasChangedLegalName',
          'hasChangedBusinessName',
          'isPrivateResidence',
          'isOfficeOpenMondayFriday',
          'isOfficeOpenOther',
          'isPrivateResidence2'
        ) %>%
        has_item_check_name() %>%
        bind_rows(
          c(
            'hasMultipleWebsites',
            'hasBooksOutsidePrimaryOffice',
            'hasForeignFinancialRegulation',
            'hasCIKNumber',
            'hasAssetsOver1B'
          ) %>%
            get_item_name_yes_no_df()
        )
      return(node_item_df)
    }

    page <-
      url %>%
      get_html_page()

    name_entity_manager <-
      page %>%
      get_entity_manager_name()

    find_text_node_safe <-
      possibly(find_text_node, NULL )

    parse_node_df <-
      function(page) {
        check_nodes <-
          page %>%
          html_nodes('.main td img') %>%
          html_attr('alt') %>%
          str_trim()

        node_df <-
          data_frame(nodeName = check_nodes) %>%
          left_join(get_check_box_value_df()) %>%
          bind_cols(get_node_item_df()) %>%
          mutate(valueItem = T) %>%
          dplyr::filter(isNodeChecked == T) %>%
          dplyr::select(nameItem, valueItem) %>%
          suppressMessages()

        column_order <-
          node_df$nameItem

        node_df <-
          node_df %>%
          spread(nameItem, valueItem) %>%
          dplyr::select(one_of(column_order))
        return(node_df)
      }

    parse_value_nodes <-
      function(page) {
        node_text <-
          page %>%
          parse_node_table_to_text(css_node = '#ctl00_ctl00_cphMainContent_cphAdvFormContent_IdentInfoPHSection_ctl00_trIAPDHeader + tr +tr td td')

        business_name_df <-
          data_frame(
            nameItem = c(
              'nameEntityManagerLegal',
              'nameEntityManagerBusiness',
              'idSEC',
              'addressStreet1OfficePrimary',
              'addressStreet2OfficePrimary',
              'cityOfficePrimary',
              'stateOfficePrimary',
              'countryOfficePrimary',
              'zipOfficePrimary',
              'hoursOffice',
              'phoneOfficePrimary',
              'idLEI'
            ),
            hit_words = c(
              'Your full legal name',
              'Name under which you primarily conduct your advisory business, if different from',
              'egistered with the SEC as an investment adviser',
              'Number and Street 1:',
              'Number and Street 2:',
              'City:',
              'State:',
              'Country:',
              'Postal Code:',
              'Normal business hours at this location:',
              'Telephone number at this location:',
              'Provide your Legal Entity Identifier'
            ),
            off_set = c(1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1),
            is_numeric_node = rep(F, 12)
          )

        business_data_df <-
          1:nrow(business_name_df) %>%
          map_df(function(x) {
            data_frame(
              nameItem =
                business_name_df$nameItem[[x]],
              value =
                find_text_node_safe(
                  node_text = node_text,
                  hit_words = business_name_df$hit_words[[x]],
                  off_set = business_name_df$off_set[[x]],
                  is_numeric_node = business_name_df$is_numeric_node[[x]]
                )
            )
          }) %>%
          mutate(value = value %>% sub('\\:', '>', .)) %>%
          separate(value, into = c('remove', 'value'), sep = "\\>") %>%
          suppressWarnings()

        business_data_df <-
          business_data_df %>%
          mutate(value = if_else(value %>% is.na, remove, value),
                 value = value %>% str_trim) %>%
          dplyr::select(nameItem, value) %>%
          dplyr::filter(!value == '') %>%
          mutate(idRow = 1:n()) %>%
          group_by(nameItem) %>%
          dplyr::filter(idRow == min(idRow)) %>%
          ungroup %>%
          dplyr::select(-idRow) %>%
          distinct()

        column_order <-
          business_data_df$nameItem

        business_data_df <-
          business_data_df %>%
          spread(nameItem, value) %>%
          dplyr::select(one_of(column_order))
        has_secondary_office <-
          'addressStreet2OfficePrimary' %in% names(business_data_df)
        if (has_secondary_office) {
          business_data_df <-
            business_data_df %>%
            unite(addressStreet1OfficePrimary, addressStreet1OfficePrimary, addressStreet2OfficePrimary, sep = ' ')
        }

        has_office_location <-
          names(business_data_df) %>% str_count('^address|^city|^country|^state') %>% sum >= 4
        if (has_office_location) {
          business_data_df <-
            business_data_df %>%
            mutate(locationOfficePrimary = addressStreet1OfficePrimary %>% paste0(' ', cityOfficePrimary, ', ', stateOfficePrimary, ' ', countryOfficePrimary, ' ', zipOfficePrimary) %>% str_to_upper())
        }
        if (business_data_df$idLEI == 'A legal entity identifier') {
          business_data_df <-
            business_data_df %>%
            dplyr::select(-idLEI)
        }
        return(business_data_df)
      }

    section_data <-
      page %>%
      parse_value_nodes() %>%
      mutate(idCRD, nameEntityManager = name_entity_manager) %>%
      left_join(
        page %>%
          parse_node_df() %>% mutate(idCRD, nameEntityManager = name_entity_manager)
      ) %>%
      select_start_vars() %>%
      suppressMessages()

    section_data <-
      section_data %>%
      mutate_at(.cols =
                  section_data %>% dplyr::select(matches("^address|^city|^state|^country")) %>% names,
                funs(. %>% str_to_upper()))

    return(section_data)
  }

get_section_2_data <-
  function(url = 'http://www.adviserinfo.sec.gov/IAPD/content/viewform/adv/Sections/iapd_AdvSecRegistrationSection.aspx?ORG_PK=135952&FLNG_PK=00D23FD4000801840427FED005E328A5056C8CC0') {
    idCRD <-
      url %>%
      get_pk_url_crd()


    get_node_item_df <- function() {
      node_item_df <-
        c(
          "hasAUMGreater100M",
          "hasAUMUnder100MOver25m",
          "hasPrincipalOfficeWY",
          "hasPrincipalOfficeForeign",
          "isAdviser1940InvestmentActCompany",
          "isAdviserBusinessDevelopmentCompany25MInCapital",
          "isAdviserPensionCapitalGreater200M",
          "isAdviserRelated203A",
          "isAdviserNew203A",
          "isAdviserMultiState203A",
          "isAdviserInternet",
          "hasSECOrderProhibitingRegistration",
          "isAdviserSECIneligible"
        ) %>%
        has_item_check_name()

      state_exists <-
        page %>%
        html_nodes('.main') %>%
        html_nodes(
          '#ctl00_ctl00_cphMainContent_cphAdvFormContent_SECRegisteredPHSection_ctl00_FourthColumn'
        ) %>%
        length() > 0

      if (state_exists) {
        states <-
          c(
            "AL",
            "AK",
            "AZ",
            "AR",
            "CA",
            "CO",
            "CT",
            "DE",
            "DC",
            "FL",
            "GA",
            "GU",
            "HI",
            "ID",
            "IL",
            "IN",
            "IA",
            "KS",
            "KY",
            "LA",
            "ME",
            "MD",
            "MA",
            "MI",
            "MN",
            "MS",
            "MO",
            "MT",
            "NE",
            "NV",
            "NH",
            "NJ",
            "NM",
            "NY",
            "NC",
            "ND",
            "OH",
            "OK",
            "OR",
            "PA",
            "PR",
            "RI",
            "SC",
            "SD",
            "TN",
            "TX",
            "UT",
            "VT",
            "VI",
            "VA",
            "WA",
            "WV",
            "WI"
          )
        item_name <-
          'stateRegistered'
        state_df <-
          1:length(states) %>%
          map_df(function(x) {
            data_frame(nameItem = item_name,
                       valueItem = states[x]) %>%
              unite(fullnameItem,
                    nameItem,
                    valueItem,
                    sep = '.',
                    remove = F)
          })

        node_item_df <-
          node_item_df %>% mutate(valueItem = valueItem %>% as.character()) %>%
          bind_rows(state_df)
      }

      return(node_item_df)
    }

    page <-
      url %>%
      get_html_page()

    name_entity_manager <-
      page %>%
      get_entity_manager_name()

    parse_node_df <-
      function(page) {
        check_nodes <-
          page %>%
          html_nodes('.main td img') %>%
          html_attr('alt') %>%
          str_trim()

        check_nodes <-
          check_nodes[!check_nodes == '']

        if (check_nodes %>% length == 3) {
          node_item_df <-
            data_frame(
              nameItem = c(
                'hasExemeptionAsSolelyVentureAdviser',
                'hasExemptionAsPrivateFundManagerUnder150MAUM',
                'hasExemptionSoleyPrivateFundManagrAUMOver150M'
              ),
              valueItem = T
            )
          node_df <-
            data_frame(nodeName = check_nodes) %>%
            left_join(get_check_box_value_df()) %>%
            mutate(isNodeChecked = if_else(nodeName %>% str_detect('Checkbox not checked'), F, T)) %>%
            bind_cols(node_item_df) %>%
            mutate(valueItem = T) %>%
            dplyr::filter(isNodeChecked == T) %>%
            dplyr::select(nameItem, valueItem) %>%
            suppressMessages()
        }

        if (!check_nodes %>% length == 3) {
          node_item_df <-
            get_node_item_df()

          if (check_nodes %>% length == 56 ){
            node_item_df <-
              node_item_df %>%
              dplyr::filter(nameItem %>% str_detect("^state")) %>%
              dplyr::select(nameItem,valueItem)

            node_item_df <-
              data_frame(
                nameItem = c(
                  'hasExemeptionAsSolelyVentureAdviser',
                  'hasExemptionAsPrivateFundManagerUnder150MAUM',
                  'hasExemptionSoleyPrivateFundManagrAUMOver150M'
                ),
                valueItem = T %>% as.character()
              ) %>% bind_rows(node_item_df)

          }

          if (check_nodes %>% length == 66 ){
            node_item_df <-
              node_item_df %>%
              dplyr::filter(nameItem %>% str_detect("^state")) %>%
              dplyr::select(nameItem,valueItem)

            node_item_df <-
              data_frame(
                nameItem = c(
                  "hasAUMGreater100M",
                  "hasAUMUnder100MOver25m",
                  "hasPrincipalOfficeWY",
                  "hasPrincipalOfficeForeign",
                  "isAdviser1940InvestmentActCompany",
                  "isAdviserBusinessDevelopmentCompany25MInCapital",
                  "isAdviserPensionCapitalGreater200M",
                  "isAdviserRelated203A",
                  "isAdviserNew203A",
                  "isAdviserMultiState203A",
                  "isAdviserInternet",
                  "hasSECOrderProhibitingRegistration",
                  "isAdviserSECIneligible"),
                valueItem = T %>% as.character()
              ) %>% bind_rows(node_item_df)

          }

          node_df <-
            data_frame(nodeName = check_nodes) %>%
            left_join(get_check_box_value_df()) %>%
            tidyr::replace_na(list(isNodeChecked = F)) %>%
            mutate(isNodeChecked = if_else(nodeName %>% str_detect('Checkbox not checked'), F, T)) %>%
            bind_cols(node_item_df) %>%
            dplyr::filter(isNodeChecked == T) %>%
            dplyr::select(nameItem, valueItem) %>%
            suppressMessages()
        }

        node_df <-
          node_df %>%
          group_by(nameItem) %>%
          mutate(countItem = 1:n()) %>%
          ungroup %>%
          mutate(nameEntityManager = name_entity_manager) %>%
          mutate(
            countItem = countItem - 1,
            countItem = countItem %>% as.character,
            countItem = ifelse(countItem == "0", '', countItem)
          ) %>%
          unite(item, nameItem, countItem, sep = '') %>%
          distinct() %>%
          suppressWarnings()

        col_order <-
          c('nameEntityManager', node_df$item)

        node_df <-
          node_df %>%
          spread(item, valueItem) %>%
          dplyr::select(one_of(col_order)) %>%
          mutate_adv_data()
        return(node_df)
      }

    section_data <-
      page %>% parse_node_df() %>%
      mutate(idCRD, nameEntityManager = name_entity_manager) %>%
      select_start_vars()

    return(section_data)
  }

get_section_3_data <-
  function(url = 'http://www.adviserinfo.sec.gov/IAPD/content/viewform/adv/Sections/iapd_AdvFormOfOrgSection.aspx?ORG_PK=162771&FLNG_PK=05F0FE9C0008018504231CD005F25E65056C8CC0') {
    idCRD <-
      url %>%
      get_pk_url_crd()

    page <-
      url %>%
      get_html_page()

    name_entity_manager <-
      page %>%
      get_entity_manager_name()

    parse_organizational_form_data <-
      function(page) {
        boxes <-
          page %>%
          html_nodes('img') %>%
          html_attr('alt') %>%
          str_trim()

        boxes <-
          boxes[boxes %>%
                  str_detect("Radio")]

        typeEntityManager <-
          data_frame(
            typeEntityManager = c(
              'Corporation',
              'Sole Proprietorship',
              'LLP',
              'Partnership',
              'LLC',
              'LP',
              'Other'
            ),
            nodeName = boxes
          ) %>%
          left_join(get_check_box_value_df()) %>%
          dplyr::filter(isNodeChecked == T) %>%
          suppressMessages() %>%
          .$typeEntityManager

        nodes <-
          page %>%
          html_nodes('.PrintHistRed') %>%
          html_text

        monthFiscalYearEnd <-
          nodes[1]

        if (nodes %>% length == 3) {
          locationEntityOrganized <-
            nodes[2:3] %>% str_to_upper() %>% paste0(collapse = ', ')
        } else {
          locationEntityOrganized <-
            NA
        }
        org_data <-
          data_frame(typeEntityManager,
                     monthFiscalYearEnd,
                     locationEntityOrganized)
        return(org_data)
      }

    section_data <-
      page %>%
      parse_organizational_form_data() %>%
      mutate(idCRD, nameEntityManager = name_entity_manager) %>%
      select_start_vars()

    return(section_data)

  }


get_section_4_data <-
  function(url = 'http://www.adviserinfo.sec.gov/IAPD/content/viewform/adv/Sections/iapd_AdvSuccessionsSection.aspx?ORG_PK=150510&FLNG_PK=00B175BA0008018601B35551000582C5056C8CC0',
           return_wide = T
  ) {
    idCRD <-
      url %>%
      get_pk_url_crd()

    get_node_item_df <-
      function() {
        node_item_df <-
          'isSucceedingRIABusiness' %>%
          get_item_name_yes_no_df()
        return(node_item_df)
      }

    page <-
      url %>%
      get_html_page()

    name_entity_manager <-
      page %>%
      get_entity_manager_name()

    parse_node_df <-
      function(page) {
        check_nodes <-
          page %>%
          html_nodes('.main td img') %>%
          html_attr('alt') %>%
          str_trim()

        node_df <-
          data_frame(nodeName = check_nodes) %>%
          left_join(get_check_box_value_df()) %>%
          bind_cols(get_node_item_df()) %>%
          dplyr::filter(isNodeChecked == T) %>%
          dplyr::select(nameItem, valueItem) %>%
          suppressMessages()

        column_order <-
          node_df$nameItem

        node_df <-
          node_df %>%
          spread(nameItem, valueItem) %>%
          dplyr::select(one_of(column_order))

        has_ria_node <-
          'isSucceedingRIABusiness' %in% names(node_df)

        if (has_ria_node) {
          if (node_df$isSucceedingRIABusiness == T) {
            node_text <-
              page %>%
              parse_node_table_to_text(
                '#ctl00_ctl00_cphMainContent_cphAdvFormContent_SuccessionPHSection_ctl00_trIAPDHeader+ tr td'
              )

            dateSuccession <-
              node_text %>%
              find_text_node(
                hit_words = "Date of Succession",
                off_set = 0,
                is_numeric_node = F
              ) %>%
              lubridate::mdy()
            node_df <-
              node_df %>%
              mutate(dateSuccession)
          }
        }
        return(node_df)
      }

    name_entity_manager <-
      page %>%
      get_entity_manager_name()

    section_data <-
      page %>% parse_node_df() %>%
      mutate(idCRD, nameEntityManager = name_entity_manager) %>%
      select_start_vars()

    if (!return_wide) {
      section_data <-
        section_data %>%
        gather(itemName, value, -c(idCRD, nameEntityManager))
    }

    return(section_data)
  }

get_section_5_data <-
  function(url = 'http://www.adviserinfo.sec.gov/IAPD/content/viewform/adv/Sections/iapd_AdvAdvisoryBusinessSection.aspx?ORG_PK=150510&FLNG_PK=00B175BA0008018601B35551000582C5056C8CC0'
  ) {
    section_name <-
      'section5AdvisoryBusinessInformation'
    idCRD <-
      url %>%
      get_pk_url_crd()

    page <-
      url %>%
      get_html_page()

    name_entity_manager <-
      page %>%
      get_entity_manager_name()

    if (!'sitemap_df' %>% exists()) {
      sitemap_df <-
        parse_adv_manager_sitemap_df(url = 'http://www.adviserinfo.sec.gov/IAPD/crd_iapd_AdvVersionSelector.aspx?ORG_PK=' %>%
                                       paste0(idCRD))
    }

    section_exists <-
      section_name %in% sitemap_df$idSection

    if (!section_exists) {
      stop(section_name %>% paste0(" does not exists for ", idCRD))
    }

    get_section_5_node_name_df <-
      function() {
        name_df <-
          data_frame(
            fullnameItem =
              c(
                "rangeClientsZero",
                "rangeClients1to10",
                "rangeClients11to25",
                "rangeClients26to100",
                "rangeClientsOver100",
                "rangeClientsIndividualNonHighNetWorthZero",
                "rangeClientsIndividualNonHighNetWorth1to10pct",
                "rangeClientsIndividualNonHighNetWorth11to25pct",
                "rangeClientsIndividualNonHighNetWorth26to50pct",
                "rangeClientsIndividualNonHighNetWorth51to75pct",
                "rangeClientsIndividualNonHighNetWorth76to99pct",
                "rangeClientsIndividualNonHighNetWorth100pct",
                "rangeClientsIndividualHighNetWorthZero",
                "rangeClientsIndividualHighNetWorth1to10pct",
                "rangeClientsIndividualHighNetWorth11to25pct",
                "rangeClientsIndividualHighNetWorth26to50pct",
                "rangeClientsIndividualHighNetWorth51to75pct",
                "rangeClientsIndividualHighNetWorth76to99pct",
                "rangeClientsIndividualHighNetWorth100pct",
                "rangeClientsBankThriftZero",
                "rangeClientsBankThrift1to10pct",
                "rangeClientsBankThrift11to25pct",
                "rangeClientsBankThrift26to50pct",
                "rangeClientsBankThrift51to75pct",
                "rangeClientsBankThrift76to99pct",
                "rangeClientsBankThrift100pct",
                "rangeClientsInvestmentCompanyZero",
                "rangeClientsInvestmentCompany1to10pct",
                "rangeClientsInvestmentCompany11to25pct",
                "rangeClientsInvestmentCompany26to50pct",
                "rangeClientsInvestmentCompany51to75pct",
                "rangeClientsInvestmentCompany76to99pct",
                "rangeClientsInvestmentCompany100pct",
                "rangeClientsBusinessDevelopmentCompanyZero",
                "rangeClientsBusinessDevelopmentCompany1to10pct",
                "rangeClientsBusinessDevelopmentCompany11to25pct",
                "rangeClientsBusinessDevelopmentCompany26to50pct",
                "rangeClientsBusinessDevelopmentCompany51to75pct",
                "rangeClientsBusinessDevelopmentCompany76to99pct",
                "rangeClientsBusinessDevelopmentCompany100pct",
                "rangeClientsPooledInvestmentVehicleZero",
                "rangeClientsPooledInvestmentVehicle1to10pct",
                "rangeClientsPooledInvestmentVehicle11to25pct",
                "rangeClientsPooledInvestmentVehicle26to50pct",
                "rangeClientsPooledInvestmentVehicle51to75pct",
                "rangeClientsPooledInvestmentVehicle76to99pct",
                "rangeClientsPooledInvestmentVehicle100pct",
                "rangeClientsPensionPlanZero",
                "rangeClientsPensionPlan1to10pct",
                "rangeClientsPensionPlan11to25pct",
                "rangeClientsPensionPlan26to50pct",
                "rangeClientsPensionPlan51to75pct",
                "rangeClientsPensionPlan76to99pct",
                "rangeClientsPensionPlan100pct",
                "rangeClientsCharitableOrganizationZero",
                "rangeClientsCharitableOrganization1to10pct",
                "rangeClientsCharitableOrganization11to25pct",
                "rangeClientsCharitableOrganization26to50pct",
                "rangeClientsCharitableOrganization51to75pct",
                "rangeClientsCharitableOrganization76to99pct",
                "rangeClientsCharitableOrganization100pct",
                "rangeClientsCorporationOtherZero",
                "rangeClientsCorporationOther1to10pct",
                "rangeClientsCorporationOther11to25pct",
                "rangeClientsCorporationOther26to50pct",
                "rangeClientsCorporationOther51to75pct",
                "rangeClientsCorporationOther76to99pct",
                "rangeClientsCorporationOther100pct",
                "rangeClientsStateMunicipalGovernmentZero",
                "rangeClientsStateMunicipalGovernment1to10pct",
                "rangeClientsStateMunicipalGovernment11to25pct",
                "rangeClientsStateMunicipalGovernment26to50pct",
                "rangeClientsStateMunicipalGovernment51to75pct",
                "rangeClientsStateMunicipalGovernment76to99pct",
                "rangeClientsStateMunicipalGovernment100pct",
                "rangeClientsInvestmentAdviserOtherZero",
                "rangeClientsInvestmentAdviserOther1to10pct",
                "rangeClientsInvestmentAdviserOther11to25pct",
                "rangeClientsInvestmentAdviserOther26to50pct",
                "rangeClientsInvestmentAdviserOther51to75pct",
                "rangeClientsInvestmentAdviserOther76to99pct",
                "rangeClientsInvestmentAdviserOther100pct",
                "rangeClientsInsuranceCompanyZero",
                "rangeClientsInsuranceCompany1to10pct",
                "rangeClientsInsuranceCompany11to25pct",
                "rangeClientsInsuranceCompany26to50pct",
                "rangeClientsInsuranceCompany51to75pct",
                "rangeClientsInsuranceCompany76to99pct",
                "rangeClientsInsuranceCompany100pct",
                "rangeClientsOtherZero",
                "rangeClientsOther1to10pct",
                "rangeClientsOther11to25pct",
                "rangeClientsOther26to50pct",
                "rangeClientsOther51to75pct",
                "rangeClientsOther76to99pct",
                "rangeClientsOther100pct",
                "rangeAUMIndividualNonHighNetWorthZero",
                "rangeAUMIndividualNonHighNetWorthUpto25pct",
                "rangeAUMIndividualNonHighNetWorthUpto50pct",
                "rangeAUMIndividualNonHighNetWorthUpto75pct",
                "rangeAUMIndividualNonHighNetWorthOver75pct",
                "rangeAUMIndividualHighNetWorkZero",
                "rangeAUMIndividualHighNetWorkUpto25pct",
                "rangeAUMIndividualHighNetWorkUpto50pct",
                "rangeAUMIndividualHighNetWorkUpto75pct",
                "rangeAUMIndividualHighNetWorkOver75pct",
                "rangeAUMBankThriftZero",
                "rangeAUMBankThriftUpto25pct",
                "rangeAUMBankThriftUpto50pct",
                "rangeAUMBankThriftUpto75pct",
                "rangeAUMBankThriftOver75pct",
                "rangeAUMInvestmentCompanyZero",
                "rangeAUMInvestmentCompanyUpto25pct",
                "rangeAUMInvestmentCompanyUpto50pct",
                "rangeAUMInvestmentCompanyUpto75pct",
                "rangeAUMInvestmentCompanyOver75pct",
                "rangeAUMBusinessDevelopmentCompanyZero",
                "rangeAUMBusinessDevelopmentCompanyUpto25pct",
                "rangeAUMBusinessDevelopmentCompanyUpto50pct",
                "rangeAUMBusinessDevelopmentCompanyUpto75pct",
                "rangeAUMBusinessDevelopmentCompanyOver75pct",
                "rangeAUMPooledInvestmentVehicleZero",
                "rangeAUMPooledInvestmentVehicleUpto25pct",
                "rangeAUMPooledInvestmentVehicleUpto50pct",
                "rangeAUMPooledInvestmentVehicleUpto75pct",
                "rangeAUMPooledInvestmentVehicleOver75pct",
                "rangeAUMPensionPlanZero",
                "rangeAUMPensionPlanUpto25pct",
                "rangeAUMPensionPlanUpto50pct",
                "rangeAUMPensionPlanUpto75pct",
                "rangeAUMPensionPlanOver75pct",
                "rangeAUMCharitableOrganizationZero",
                "rangeAUMCharitableOrganizationUpto25pct",
                "rangeAUMCharitableOrganizationUpto50pct",
                "rangeAUMCharitableOrganizationUpto75pct",
                "rangeAUMCharitableOrganizationOver75pct",
                "rangeAUMCorporationOthrZero",
                "rangeAUMCorporationOthrUpto25pct",
                "rangeAUMCorporationOthrUpto50pct",
                "rangeAUMCorporationOthrUpto75pct",
                "rangeAUMCorporationOthrOver75pct",
                "rangeAUMStateMunicipalGovernmentZero",
                "rangeAUMStateMunicipalGovernmentUpto25pct",
                "rangeAUMStateMunicipalGovernmentUpto50pct",
                "rangeAUMStateMunicipalGovernmentUpto75pct",
                "rangeAUMStateMunicipalGovernmentOver75pct",
                "rangeAUMInvestmentAdviserOtherZero",
                "rangeAUMInvestmentAdviserOtherUpto25pct",
                "rangeAUMInvestmentAdviserOtherUpto50pct",
                "rangeAUMInvestmentAdviserOtherUpto75pct",
                "rangeAUMInvestmentAdviserOtherOver75pct",
                "rangeAUMInsuranceCompanyZero",
                "rangeAUMInsuranceCompanyUpto25pct",
                "rangeAUMInsuranceCompanyUpto50pct",
                "rangeAUMInsuranceCompanyUpto75pct",
                "rangeAUMInsuranceCompanyOver75pct",
                "rangeAUMOtherZero",
                "rangeAUMOtherUpto25pct",
                "rangeAUMOtherUpto50pct",
                "rangeAUMOtherUpto75pct",
                "rangeAUMOtherOver75pct",
                "hasFeeAUM",
                "hasFeeHourlyCharge",
                "hasFeeSubscription",
                "hasFeeFixed",
                "hasFeeCommission",
                "hasFeePerformance",
                "hasFeeOther",
                "hasSecuritiesPortfolioManagement.TRUE",
                "hasSecuritiesPortfolioManagement.FALSE",
                "hasFinancialPlanning",
                "hasPortfolioManagementIndividualSmallBusiness",
                "hasPortfolioManagementInvestmentCompanies",
                "hasPortfolioManagementPooledInvestmentVehicles",
                "hasPortfolioManagementInstitutionalClients",
                "hasServicePensionConsulting",
                "hasServiceInvestmentAdviserSelection",
                "hasServicePeriodicalPublication",
                "hasServiceSecurityRating",
                "hasServiceMarketTiming",
                "hasServiceEducationSeminars",
                "hasServiceOther",
                "rangeClientsFinancialPlanningZero",
                "rangeClientsFinancialPlanning1to10",
                "rangeClientsFinancialPlanning11to25",
                "rangeClientsFinancialPlanning26to50",
                "rangeClientsFinancialPlanning51to100",
                "rangeClientsFinancialPlanning101to250",
                "rangeClientsFinancialPlanning251to500",
                "rangeClientsFinancialPlanningOver500",
                "hasFeeWrapSponsor",
                "hasFeeWrapPortfolioManager",
                "isAdviserLimitedInvestmentTypes.TRUE",
                "isAdviserLimitedInvestmentTypes.FALSE"
              ),
            nameItem =
              c(
                "rangeClients",
                "rangeClients",
                "rangeClients",
                "rangeClients",
                "rangeClients",
                "rangeClientsIndividualNonHighNetWorth",
                "rangeClientsIndividualNonHighNetWorth",
                "rangeClientsIndividualNonHighNetWorth",
                "rangeClientsIndividualNonHighNetWorth",
                "rangeClientsIndividualNonHighNetWorth",
                "rangeClientsIndividualNonHighNetWorth",
                "rangeClientsIndividualNonHighNetWorth",
                "rangeClientsIndividualHighNetWorth",
                "rangeClientsIndividualHighNetWorth",
                "rangeClientsIndividualHighNetWorth",
                "rangeClientsIndividualHighNetWorth",
                "rangeClientsIndividualHighNetWorth",
                "rangeClientsIndividualHighNetWorth",
                "rangeClientsIndividualHighNetWorth",
                "rangeClientsBankThrift",
                "rangeClientsBankThrift",
                "rangeClientsBankThrift",
                "rangeClientsBankThrift",
                "rangeClientsBankThrift",
                "rangeClientsBankThrift",
                "rangeClientsBankThrift",
                "rangeClientsInvestmentCompany",
                "rangeClientsInvestmentCompany",
                "rangeClientsInvestmentCompany",
                "rangeClientsInvestmentCompany",
                "rangeClientsInvestmentCompany",
                "rangeClientsInvestmentCompany",
                "rangeClientsInvestmentCompany",
                "rangeClientsBusinessDevelopmentCompany",
                "rangeClientsBusinessDevelopmentCompany",
                "rangeClientsBusinessDevelopmentCompany",
                "rangeClientsBusinessDevelopmentCompany",
                "rangeClientsBusinessDevelopmentCompany",
                "rangeClientsBusinessDevelopmentCompany",
                "rangeClientsBusinessDevelopmentCompany",
                "rangeClientsPooledInvestmentVehicle",
                "rangeClientsPooledInvestmentVehicle",
                "rangeClientsPooledInvestmentVehicle",
                "rangeClientsPooledInvestmentVehicle",
                "rangeClientsPooledInvestmentVehicle",
                "rangeClientsPooledInvestmentVehicle",
                "rangeClientsPooledInvestmentVehicle",
                "rangeClientsPensionPlan",
                "rangeClientsPensionPlan",
                "rangeClientsPensionPlan",
                "rangeClientsPensionPlan",
                "rangeClientsPensionPlan",
                "rangeClientsPensionPlan",
                "rangeClientsPensionPlan",
                "rangeClientsCharitableOrganization",
                "rangeClientsCharitableOrganization",
                "rangeClientsCharitableOrganization",
                "rangeClientsCharitableOrganization",
                "rangeClientsCharitableOrganization",
                "rangeClientsCharitableOrganization",
                "rangeClientsCharitableOrganization",
                "rangeClientsCorporationOther",
                "rangeClientsCorporationOther",
                "rangeClientsCorporationOther",
                "rangeClientsCorporationOther",
                "rangeClientsCorporationOther",
                "rangeClientsCorporationOther",
                "rangeClientsCorporationOther",
                "rangeClientsStateMunicipalGovernment",
                "rangeClientsStateMunicipalGovernment",
                "rangeClientsStateMunicipalGovernment",
                "rangeClientsStateMunicipalGovernment",
                "rangeClientsStateMunicipalGovernment",
                "rangeClientsStateMunicipalGovernment",
                "rangeClientsStateMunicipalGovernment",
                "rangeClientsInvestmentAdviserOther",
                "rangeClientsInvestmentAdviserOther",
                "rangeClientsInvestmentAdviserOther",
                "rangeClientsInvestmentAdviserOther",
                "rangeClientsInvestmentAdviserOther",
                "rangeClientsInvestmentAdviserOther",
                "rangeClientsInvestmentAdviserOther",
                "rangeClientsInsuranceCompany",
                "rangeClientsInsuranceCompany",
                "rangeClientsInsuranceCompany",
                "rangeClientsInsuranceCompany",
                "rangeClientsInsuranceCompany",
                "rangeClientsInsuranceCompany",
                "rangeClientsInsuranceCompany",
                "rangeClientsOther",
                "rangeClientsOther",
                "rangeClientsOther",
                "rangeClientsOther",
                "rangeClientsOther",
                "rangeClientsOther",
                "rangeClientsOther",
                "rangeAUMIndividualNonHighNetWorth",
                "rangeAUMIndividualNonHighNetWorth",
                "rangeAUMIndividualNonHighNetWorth",
                "rangeAUMIndividualNonHighNetWorth",
                "rangeAUMIndividualNonHighNetWorth",
                "rangeAUMIndividualHighNetWork",
                "rangeAUMIndividualHighNetWork",
                "rangeAUMIndividualHighNetWork",
                "rangeAUMIndividualHighNetWork",
                "rangeAUMIndividualHighNetWork",
                "rangeAUMBankThrift",
                "rangeAUMBankThrift",
                "rangeAUMBankThrift",
                "rangeAUMBankThrift",
                "rangeAUMBankThrift",
                "rangeAUMInvestmentCompany",
                "rangeAUMInvestmentCompany",
                "rangeAUMInvestmentCompany",
                "rangeAUMInvestmentCompany",
                "rangeAUMInvestmentCompany",
                "rangeAUMBusinessDevelopmentCompany",
                "rangeAUMBusinessDevelopmentCompany",
                "rangeAUMBusinessDevelopmentCompany",
                "rangeAUMBusinessDevelopmentCompany",
                "rangeAUMBusinessDevelopmentCompany",
                "rangeAUMPooledInvestmentVehicle",
                "rangeAUMPooledInvestmentVehicle",
                "rangeAUMPooledInvestmentVehicle",
                "rangeAUMPooledInvestmentVehicle",
                "rangeAUMPooledInvestmentVehicle",
                "rangeAUMPensionPlan",
                "rangeAUMPensionPlan",
                "rangeAUMPensionPlan",
                "rangeAUMPensionPlan",
                "rangeAUMPensionPlan",
                "rangeAUMCharitableOrganization",
                "rangeAUMCharitableOrganization",
                "rangeAUMCharitableOrganization",
                "rangeAUMCharitableOrganization",
                "rangeAUMCharitableOrganization",
                "rangeAUMCorporationOthr",
                "rangeAUMCorporationOthr",
                "rangeAUMCorporationOthr",
                "rangeAUMCorporationOthr",
                "rangeAUMCorporationOthr",
                "rangeAUMStateMunicipalGovernment",
                "rangeAUMStateMunicipalGovernment",
                "rangeAUMStateMunicipalGovernment",
                "rangeAUMStateMunicipalGovernment",
                "rangeAUMStateMunicipalGovernment",
                "rangeAUMInvestmentAdviserOther",
                "rangeAUMInvestmentAdviserOther",
                "rangeAUMInvestmentAdviserOther",
                "rangeAUMInvestmentAdviserOther",
                "rangeAUMInvestmentAdviserOther",
                "rangeAUMInsuranceCompany",
                "rangeAUMInsuranceCompany",
                "rangeAUMInsuranceCompany",
                "rangeAUMInsuranceCompany",
                "rangeAUMInsuranceCompany",
                "rangeAUMOther",
                "rangeAUMOther",
                "rangeAUMOther",
                "rangeAUMOther",
                "rangeAUMOther",
                "hasFeeAUM",
                "hasFeeHourlyCharge",
                "hasFeeSubscription",
                "hasFeeFixed",
                "hasFeeCommission",
                "hasFeePerformance",
                "hasFeeOther",
                "hasSecuritiesPortfolioManagement",
                "hasSecuritiesPortfolioManagement",
                "hasFinancialPlanning",
                "hasPortfolioManagementIndividualSmallBusiness",
                "hasPortfolioManagementInvestmentCompanies",
                "hasPortfolioManagementPooledInvestmentVehicles",
                "hasPortfolioManagementInstitutionalClients",
                "hasServicePensionConsulting",
                "hasServiceInvestmentAdviserSelection",
                "hasServicePeriodicalPublication",
                "hasServiceSecurityRating",
                "hasServiceMarketTiming",
                "hasServiceEducationSeminars",
                "hasServiceOther",
                "rangeClientsFinancialPlanning",
                "rangeClientsFinancialPlanning",
                "rangeClientsFinancialPlanning",
                "rangeClientsFinancialPlanning",
                "rangeClientsFinancialPlanning",
                "rangeClientsFinancialPlanning",
                "rangeClientsFinancialPlanning",
                "rangeClientsFinancialPlanning",
                "hasFeeWrapSponsor",
                "hasFeeWrapPortfolioManager",
                "isAdviserLimitedInvestmentTypes",
                "isAdviserLimitedInvestmentTypes"
              ),
            valueItem =
              c(
                "Zero",
                "1to10",
                "11to25",
                "26to100",
                "Over100",
                "Zero",
                "1to10pct",
                "11to25pct",
                "26to50pct",
                "51to75pct",
                "76to99pct",
                "100pct",
                "Zero",
                "1to10pct",
                "11to25pct",
                "26to50pct",
                "51to75pct",
                "76to99pct",
                "100pct",
                "Zero",
                "1to10pct",
                "11to25pct",
                "26to50pct",
                "51to75pct",
                "76to99pct",
                "100pct",
                "Zero",
                "1to10pct",
                "11to25pct",
                "26to50pct",
                "51to75pct",
                "76to99pct",
                "100pct",
                "Zero",
                "1to10pct",
                "11to25pct",
                "26to50pct",
                "51to75pct",
                "76to99pct",
                "100pct",
                "Zero",
                "1to10pct",
                "11to25pct",
                "26to50pct",
                "51to75pct",
                "76to99pct",
                "100pct",
                "Zero",
                "1to10pct",
                "11to25pct",
                "26to50pct",
                "51to75pct",
                "76to99pct",
                "100pct",
                "Zero",
                "1to10pct",
                "11to25pct",
                "26to50pct",
                "51to75pct",
                "76to99pct",
                "100pct",
                "Zero",
                "1to10pct",
                "11to25pct",
                "26to50pct",
                "51to75pct",
                "76to99pct",
                "100pct",
                "Zero",
                "1to10pct",
                "11to25pct",
                "26to50pct",
                "51to75pct",
                "76to99pct",
                "100pct",
                "Zero",
                "1to10pct",
                "11to25pct",
                "26to50pct",
                "51to75pct",
                "76to99pct",
                "100pct",
                "Zero",
                "1to10pct",
                "11to25pct",
                "26to50pct",
                "51to75pct",
                "76to99pct",
                "100pct",
                "Zero",
                "1to10pct",
                "11to25pct",
                "26to50pct",
                "51to75pct",
                "76to99pct",
                "100pct",
                "Zero",
                "Upto25pct",
                "Upto50pct",
                "Upto75pct",
                "Over75pct",
                "Zero",
                "Upto25pct",
                "Upto50pct",
                "Upto75pct",
                "Over75pct",
                "Zero",
                "Upto25pct",
                "Upto50pct",
                "Upto75pct",
                "Over75pct",
                "Zero",
                "Upto25pct",
                "Upto50pct",
                "Upto75pct",
                "Over75pct",
                "Zero",
                "Upto25pct",
                "Upto50pct",
                "Upto75pct",
                "Over75pct",
                "Zero",
                "Upto25pct",
                "Upto50pct",
                "Upto75pct",
                "Over75pct",
                "Zero",
                "Upto25pct",
                "Upto50pct",
                "Upto75pct",
                "Over75pct",
                "Zero",
                "Upto25pct",
                "Upto50pct",
                "Upto75pct",
                "Over75pct",
                "Zero",
                "Upto25pct",
                "Upto50pct",
                "Upto75pct",
                "Over75pct",
                "Zero",
                "Upto25pct",
                "Upto50pct",
                "Upto75pct",
                "Over75pct",
                "Zero",
                "Upto25pct",
                "Upto50pct",
                "Upto75pct",
                "Over75pct",
                "Zero",
                "Upto25pct",
                "Upto50pct",
                "Upto75pct",
                "Over75pct",
                "Zero",
                "Upto25pct",
                "Upto50pct",
                "Upto75pct",
                "Over75pct",
                "TRUE",
                "TRUE",
                "TRUE",
                "TRUE",
                "TRUE",
                "TRUE",
                "TRUE",
                "TRUE",
                "FALSE",
                "TRUE",
                "TRUE",
                "TRUE",
                "TRUE",
                "TRUE",
                "TRUE",
                "TRUE",
                "TRUE",
                "TRUE",
                "TRUE",
                "TRUE",
                "TRUE",
                "Zero",
                "1to10",
                "11to25",
                "26to50",
                "51to100",
                "101to250",
                "251to500",
                "Over500",
                "TRUE",
                "TRUE",
                "TRUE",
                "FALSE"
              )
          )
        return(name_df)
      }

    section_exists <-
      'section5AdvisoryBusinessInformation' %in% sitemap_df$idSection

    if (!section_exists) {
      stop("Company structure data does not exists for " %>% paste0(idCRD))
    }

    parse_company_structure_page <-
      function(page)  {
        parse_section_5_check_nodes <-
          function(page) {
            check_nodes <-
              page %>%
              html_nodes('.main td img') %>%
              html_attr('alt') %>%
              str_trim()

            client_summary_image_df <-
              data_frame(nodeName = check_nodes) %>%
              left_join(get_check_box_value_df()) %>%
              bind_cols(get_section_5_node_name_df()) %>%
              dplyr::select(nameItem,
                            fullnameItem,
                            nameItem,
                            valueItem,
                            nodeName,
                            isNodeChecked) %>%
              suppressMessages() %>%
              dplyr::filter(isNodeChecked == T) %>%
              dplyr::select(nameItem, valueItem)

            column_order <-
              client_summary_image_df$nameItem

            client_summary_df <-
              client_summary_image_df %>%
              spread(nameItem, valueItem) %>%
              dplyr::select(one_of(column_order))

            client_summary_df <-
              client_summary_df %>%
              mutate_at(.cols =
                          client_summary_df %>% dplyr::select(matches("^is|^has")) %>% names,
                        .funs = as.logical)

            return(client_summary_df)
          }
        find_text_node_safe <-
          possibly(find_text_node, NULL)

        name_entity_manager <-
          page %>%
          get_entity_manager_name()

        node_text <-
          page %>%
          parse_node_table_to_text(css_node = '#ctl00_ctl00_cphMainContent_cphAdvFormContent_ClientCompensation_ctl00_trIAPDHeader + tr + tr')

        employee_node_df <-
          data_frame(
            nameItem = c(
              "countEmployeesTotal",
              "countEmployeesInvestmentAdvisory",
              "countEmployeesBrokerDealer",
              "countEmployeesStateRegisteredInvestmentAdviser",
              "countEmployeesStateRegisteredInvestmentAdviserMultipleEntities",
              "countEmployeesLicensedInsuranceAgents",
              "countEmployeesSolicitAdvisoryClients",
              "pctClientsNonUS"
            ),
            hit_words = c(
              'but do not include any clerical workers.',
              ' perform investment advisory',
              'registered representatives of a broker-dealer',
              'investment adviser representatives',
              'for an investment adviser other than you',
              'licensed agents of an insurance company or agency',
              'Approximately how many firms or other',
              'are non-United States persons'
            ),
            off_set = c(1, 1, 1, 1, 1, 1, 5, 1),
            is_numeric_node = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, T)
          )

        employee_count_df <-
          1:nrow(employee_node_df) %>%
          map_df(function(x) {
            data_frame(
              nameItem =
                employee_node_df$nameItem[[x]],
              value =
                find_text_node_safe(
                  node_text = node_text,
                  hit_words = employee_node_df$hit_words[[x]],
                  off_set = employee_node_df$off_set[[x]],
                  is_numeric_node = employee_node_df$is_numeric_node[[x]]
                )
            )
          }) %>%
          spread(nameItem, value) %>%
          dplyr::select(one_of(employee_node_df$nameItem)) %>%
          mutate_at(.cols = 'pctClientsNonUS',
                    .funs = funs(. / 100)) %>%
          suppressWarnings()

        node_text <-
          page %>%
          parse_node_table_to_text(
            '#ctl00_ctl00_cphMainContent_cphAdvFormContent_AssetsUnderMgnmt_ctl00_trIAPDHeader + tr table td tr td'
          )

        aum_value_name_df <-
          data_frame(
            nameItem = c(
              "amountAUMDiscretionary",
              "amountAUMNonDiscretionary",
              "amountAUMTotal",
              "countAccountsDiscretionary",
              "countAccountsNonDiscretionary",
              "countAccountsTotal"
            ),
            hit_words = c(
              c(
                'Discretionary',
                'Non-Discretionary',
                'Total:',
                'Discretionary',
                'Non-Discretionary',
                'Total:'
              )
            ),
            off_set = c(2, 2, 2, 4, 4, 4),
            is_numeric_node = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
          )

        has_aum_df <-
          node_text %>% str_count(aum_value_name_df$hit_words %>% paste0(collapse = "|")) %>% sum > 0

        if (has_aum_df) {
          aum_value_df <-
            1:nrow(aum_value_name_df) %>%
            map_df(function(x) {
              has_value <-
                find_text_node_safe(
                  node_text = node_text,
                  hit_words = aum_value_name_df$hit_words[[x]],
                  off_set = aum_value_name_df$off_set[[x]],
                  is_numeric_node = aum_value_name_df$is_numeric_node[[x]]
                ) %>% length > 0

              if (has_value) {
                val <-
                  find_text_node_safe(
                    node_text = node_text,
                    hit_words = aum_value_name_df$hit_words[[x]],
                    off_set = aum_value_name_df$off_set[[x]],
                    is_numeric_node = aum_value_name_df$is_numeric_node[[x]]
                  )
              } else {
                val <-
                  NA
              }

              data_frame(
                nameItem =
                  aum_value_name_df$nameItem[[x]],
                value =
                  val
              )
            }) %>%
            distinct() %>%
            group_by(nameItem) %>%
            dplyr::filter(value == max(value)) %>%
            ungroup %>%
            spread(nameItem, value) %>%
            suppressWarnings()
          aum_value_df <-
            aum_value_df %>%
            dplyr::select(matches(aum_value_name_df$nameItem %>% paste0(collapse = '|'))) %>%
            mutate(nameEntityManager = name_entity_manager)

        } else {
          aum_value_df <-
            data_frame(nameEntityManager = name_entity_manager)
        }

        check_node_df <-
          page %>% parse_section_5_check_nodes()

        section_5_data <-
          data_frame(idCRD,
                     nameEntityManager = name_entity_manager) %>%
          bind_cols(list(employee_count_df, check_node_df)) %>%
          left_join(aum_value_df) %>%
          suppressMessages()
        node_text <-
          page %>%
          parse_node_table_to_text(css_node = '#ctl00_ctl00_cphMainContent_cphAdvFormContent_ClientCompensation_ctl00_trIAPDHeader + tr + tr')
        if (node_text[grepl("[[:upper:]]+$", node_text)] %>% unique() %>% length == 1) {
          other_value <-
            node_text[grepl("[[:upper:]]+$", node_text)] %>% unique()
          section_5_data <-
            section_5_data %>%
            mutate(typeOther = other_value) %>%
            dplyr::select(idCRD,
                          nameEntityManager,
                          matches("typeOther"),
                          everything())
        }

        if (node_text[grepl("[[:upper:]]+$", node_text)] %>% unique() %>% length > 1) {
          other_value <-
            node_text[grepl("[[:upper:]]+$", node_text)] %>% unique()

          type_df <-
            data_frame(nameItem = rep('typeOther', length(other_value)),
                       value = other_value) %>%
            mutate(
              countItem = (1:n()) - 1,
              fullnameItem = if_else(
                countItem > 0,
                nameItem %>% paste0(".", countItem),
                nameItem
              )
            ) %>%
            dplyr::select(fullnameItem, value) %>%
            spread(fullnameItem, value) %>%
            mutate(nameEntityManager = name_entity_manager)
          section_5_data <-
            section_5_data %>%
            left_join(type_df) %>%
            suppressMessages() %>%
            dplyr::select(idCRD,
                          nameEntityManager,
                          matches("typeOther"),
                          everything())
        }
        return(section_5_data)

      }

    section_5_data <-
      page %>%
      parse_company_structure_page() %>%
      mutate(nameEntityManager = name_entity_manager) %>%
      select_start_vars()

    return(section_5_data)
  }


get_section_6_data <-
  function(url = 'http://www.adviserinfo.sec.gov/IAPD/content/viewform/adv/Sections/iapd_AdvOtherBusinessSection.aspx?ORG_PK=150510&FLNG_PK=00B175BA0008018601B35551000582C5056C8CC0',
           return_wide = T
  ) {

    idCRD <-
      url %>%
      get_pk_url_crd()


    page <-
      url %>%
      get_html_page()

    get_node_item_df <-
      function() {
        node_item_df <-
          c(
            'isBusinessActiveNonListedActivity',
            'typeBusinessActiveNonListedActivity',
            'hasProductNonInvestmentAdvice'
          ) %>%
          get_item_name_yes_no_df()
        return(node_item_df)
      }

    parse_section_6 <-
      function(page, return_wide)  {
        parse_section_6_check_nodes <-
          function(page, return_wide) {
            check_nodes <-
              page %>%
              html_nodes('.main td img') %>%
              html_attr('alt') %>%
              str_trim()

            check_nodes <-
              check_nodes[check_nodes %>% str_detect("Radio")]

            node_df <-
              data_frame(nodeName = check_nodes) %>%
              left_join(get_check_box_value_df()) %>%
              bind_cols(get_node_item_df()) %>%
              dplyr::filter(isNodeChecked == T) %>%
              dplyr::select(nameItem, valueItem) %>%
              suppressMessages()

            if (return_wide){
              column_order <-
                node_df$nameItem

              node_df <-
                node_df %>%
                spread(nameItem, valueItem) %>%
                dplyr::select(one_of(column_order))
            }

            return(node_df)
          }

        name_entity_manager <-
          page %>%
          get_entity_manager_name()

        section_data <-
          page %>%
          parse_section_6_check_nodes(return_wide = return_wide) %>%
          mutate(nameEntityManager = name_entity_manager)
        return(section_data)

      }

    section_data <-
      page %>%
      parse_section_6(return_wide = return_wide) %>%
      mutate(idCRD) %>%
      select_start_vars()
    return(section_data)
  }

get_section_7a_data <-
  function(url = 'https://adviserinfo.sec.gov/IAPD/content/viewform/adv/Sections/iapd_AdvFinancialAffiliationsSection.aspx?ORG_PK=145652&FLNG_PK=01A04E4200080189047A8BA1003A6639056C8CC0',
           return_wide = T
  ) {

    idCRD <-
      url %>%
      get_pk_url_crd()
    page <-
      url %>%
      get_html_page()

    name_entity_manager <-
      page %>%
      get_entity_manager_name()

    get_node_item_df <-
      function() {
        node_item_df <-
          data_frame(
            nameItem = c(
              'isBrokerDealer',
              'isOtherInvestmentAdvisor',
              'isRegisteredMunicipalAdvisor',
              'isRegisteredSwapDealer',
              'isMajorSwapParticipant',
              'isCommodityPoolOperator',
              'isFutureCommissionMerchant',
              'isBankThrift',
              'isTrustCompany',
              'isAccountingFirm',
              'isLawFirm',
              'isInsuranceCompany',
              'isPensionConsultant',
              'isRealEstateBrokerDealer',
              'isNonPooledInvestmentSponsor',
              'isPooledInvestmentSponsor'
            )
          )
        return(node_item_df)
      }

    parse_affliations <-
      function(page, return_wide)  {
        parse_section_7_check_nodes <-
          function(page, return_wide) {
            check_nodes <-
              page %>%
              html_nodes('.main td img') %>%
              html_attr('alt') %>%
              str_trim()
            has_nodes <-
              data_frame(nodeName = check_nodes) %>%
              left_join(get_check_box_value_df()) %>%
              bind_cols(get_node_item_df()) %>%
              mutate(valueItem = T) %>%
              suppressMessages() %>%
              dplyr::filter(isNodeChecked == T) %>% nrow > 0 %>%
              suppressMessages()

            if (has_nodes){
              affiliation_df <-
                data_frame(nodeName = check_nodes) %>%
                left_join(get_check_box_value_df()) %>%
                bind_cols(get_node_item_df()) %>%
                mutate(valueItem = T) %>%
                dplyr::filter(isNodeChecked == T) %>%
                dplyr::select(nameItem, valueItem) %>%
                suppressMessages()
              if (return_wide) {
                column_order <-
                  affiliation_df$nameItem

                affiliation_df <-
                  affiliation_df %>%
                  spread(nameItem, valueItem) %>%
                  dplyr::select(one_of(column_order))
              }
            } else {
              affiliation_df <-
                data_frame(idCRD, nameEntityManager = name_entity_manager)
            }
            return(affiliation_df)
          }
        parse_section_7_check_nodes_safe <-
          possibly(parse_section_7_check_nodes, NULL)
        section_data <-
          page %>%
          parse_section_7_check_nodes(return_wide = return_wide) %>%
          mutate(nameEntityManager = name_entity_manager)

        return(section_data)

      }

    section_data <-
      page %>%
      parse_affliations(return_wide = return_wide) %>%
      mutate(idCRD) %>%
      dplyr::select(idCRD, nameEntityManager, everything())

    return(section_data)
  }

get_section_7b_data <-
  function(url = 'http://www.adviserinfo.sec.gov/IAPD/content/viewform/adv/Sections/iapd_AdvPrivateFundReportingSection.aspx?ORG_PK=162351&FLNG_PK=052DAAB400080184043CE66005E35E29056C8CC0',
           return_wide = F,
           return_message = T) {

    idCRD <-
      url %>%
      get_pk_url_crd()


    page <-
      url %>%
      get_html_page()

    parse_private_fund_data <-
      function(page, return_wide, return_message = T) {
        name_entity_manager <-
          page %>%
          get_entity_manager_name()

        has_fund_data <-
          page %>%
          html_nodes(
            '#ctl00_ctl00_cphMainContent_cphAdvFormContent_PrvtFundReportingListPH_grdFunds_ctl01_lblTotalRecords'
          ) %>%
          html_text() %>%
          str_replace_all("Total Funds: ", '') %>%
          as.numeric %>% length > 0
        if (has_fund_data) {
          fund_count <-
            page %>%
            html_nodes(
              '#ctl00_ctl00_cphMainContent_cphAdvFormContent_PrvtFundReportingListPH_grdFunds_ctl01_lblTotalRecords'
            ) %>%
            html_text() %>%
            str_replace_all("Total Funds: ", '') %>%
            as.numeric
          page_sequences <-
            seq(3, length.out = fund_count) %>% as.character %>%
            map_chr(function(x) {
              if (x %>% nchar == 1) {
                paste0('0', x)
              } else {
                x
              }
            })

          page_table_nodes <-
            '#ctl00_ctl00_cphMainContent_cphAdvFormContent_PrvtFundReportingListPH_grdFunds_ctl' %>%
            paste0(page_sequences, '_pnlFund')

          table_css_node_df <-
            data_frame(
              numberTable = 1:fund_count,
              pageSequenceNode = page_sequences,
              tableCSSNode = page_table_nodes
            )
          section_matrix_df <-
            get_section_matrix_df()

          all_data <-
            parse_funds_tables(
              page = page,
              return_message = return_message,
              table_css_node_df = table_css_node_df,
              section_matrix_df = section_matrix_df
            )

          all_data <-
            all_data %>%
            mutate(nameEntityManager = name_entity_manager) %>%
            dplyr::select(nameEntityManager, everything())

          if (return_message) {
            total_aum <-
              all_data$amountFundGrossAUM %>% sum %>% formattable::currency()
          }

          if (return_wide) {
            all_data <-
              all_data %>%
              dplyr::rename(countItem = numberFund) %>%
              widen_adv_data() %>%
              mutate_adv_data()

          }

          if (return_message) {
            "Parsed " %>%
              paste0(
                name_entity_manager,
                ' they have ',
                fund_count,
                ' fund vehicles and ',
                total_aum,
                ' in private fund AUM'
              ) %>%
              message()
          }

        } else {
          all_data <-
            data_frame(nameEntityManager = name_entity_manager)
        }
        return(all_data)
      }

    parse_private_fund_data_safe <-
      possibly(parse_private_fund_data, NULL)
    section_data <-
      page %>%
      parse_private_fund_data_safe(return_wide = return_wide,
                                   return_message = return_message)

    section_data <-
      section_data %>%
      mutate(idCRD) %>%
      select_start_vars()

    return(section_data)
  }

get_section_8_data <-
  function(url = 'http://www.adviserinfo.sec.gov/IAPD/content/viewform/adv/Sections/iapd_AdvClientTransSection.aspx?ORG_PK=150510&FLNG_PK=00B175BA0008018601B35551000582C5056C8CC0',
           return_wide = T
  ) {
    idCRD <-
      url %>%
      get_pk_url_crd()


    page <-
      url %>%
      get_html_page()

    get_node_item_df <-
      function() {
        node_item_df <-
          c(
            "isSecuritiesBuyerFromClientsForSelfToClientsFromOwned",
            "isSecuritiesFirmBoughSoldClientRecommended",
            "isSecuritiesClientRecommendedFirmOwnedSecurity",
            "hasTradeExecutionClient",
            "isSecuritiesUnderwriterPurchaserManagerClientRecommendedSecurity",
            "hasRecommendedPurchaseSaleFirmOwnedSecurity",
            "hasClientDiscretionBuySell",
            "hasClientDiscretionBuySellAmount",
            "hasClientDiscretionBrokerSelection",
            "hasClientDiscretionCommisionCost",
            "isClientBrokerRelatedParty",
            "hasClientBrokerRecommendation",
            "isClientBrokerRecommenationRelatedParty",
            "isBrokerSoftDollarRecipient",
            "isBrokerSoftDollarEligibleResearchService",
            "hasCompensationForClientReferrals",
            "isCompensatedForClientReferrals"
          ) %>%
          get_item_name_yes_no_df()
        return(node_item_df)
      }

    parse_section_8 <-
      function(page, return_wide)  {
        name_entity_manager <-
          page %>%
          get_entity_manager_name()
        parse_section_8_nodes <-
          function(page, return_wide) {
            check_nodes <-
              page %>%
              html_nodes('.main td img') %>%
              html_attr('alt') %>%
              str_trim()

            section_data <-
              data_frame(nodeName = check_nodes) %>%
              left_join(get_check_box_value_df()) %>%
              bind_cols(get_node_item_df()) %>%
              dplyr::filter(isNodeChecked == T) %>%
              mutate(nameEntityManager = name_entity_manager) %>%
              dplyr::select(nameEntityManager, nameItem, valueItem) %>%
              suppressMessages()

            if (return_wide) {

              column_order <-
                c('nameEntityManager', section_data$nameItem)

              section_data <-
                section_data %>%
                spread(nameItem, valueItem) %>%
                dplyr::select(one_of(column_order))
            }

            return(section_data)
          }
        section_data <-
          page %>%
          parse_section_8_nodes(return_wide = return_wide)

        return(section_data)

      }

    section_data <-
      page %>%
      parse_section_8(return_wide = return_wide) %>%
      mutate(idCRD) %>%
      dplyr::select(idCRD, everything())
    return(section_data)
  }

get_section_9_data <-
  function(url = 'https://adviserinfo.sec.gov/IAPD/content/viewform/adv/Sections/iapd_AdvCustodySection.aspx?ORG_PK=134600&FLNG_PK=04189D5A00080188013572C10022C501056C8CC0',
           return_wide = T) {
    idCRD <-
      url %>%
      get_pk_url_crd()

    page <-
      url %>%
      get_html_page()

    name_entity_manager <-
      page %>%
      get_entity_manager_name()

    get_node_item_df <-
      function() {
        node_item_df <-
          data_frame(
            namefullItem =
              c(
                "hasCustodyClientCash.TRUE",
                "hasCustodyClientCash.FALSE",
                "hasCustodyClientSecurities.TRUE",
                "hasCustodyClientSecurities.FALSE",
                "hasCustodyClientCashRelatdPerson.TRUE",
                "hasCustodyClientCashRelatdPerson.FALSE",
                "hasCustodyClientSecuritiesRelatedPerson.TRUE",
                "hasCustodyClientSecuritiesRelatedPerson.FALSE",
                "hasQuarterlyStatementsFromCustodian",
                "hasAnnualIndependentFundAudit",
                "hasSurpriseIndependentFundAudit",
                "hasControlPersonReport",
                "isQualifiedCustodian.TRUE",
                "isQualifiedCustodian.FALSE",
                "hasRelatedQualifiedCustodian.TRUE",
                "hasRelatedQualifiedCustodian.FALSE"
              ),
            nameItem =
              c(
                "hasCustodyClientCash",
                "hasCustodyClientCash",
                "hasCustodyClientSecurities",
                "hasCustodyClientSecurities",
                "hasCustodyClientCashRelatdPerson",
                "hasCustodyClientCashRelatdPerson",
                "hasCustodyClientSecuritiesRelatedPerson",
                "hasCustodyClientSecuritiesRelatedPerson",
                "hasQuarterlyStatementsFromCustodian",
                "hasAnnualIndependentFundAudit",
                "hasSurpriseIndependentFundAudit",
                "hasControlPersonReport",
                "isQualifiedCustodian",
                "isQualifiedCustodian",
                "hasRelatedQualifiedCustodian",
                "hasRelatedQualifiedCustodian"
              ),
            valueItem =
              c(
                TRUE,
                FALSE,
                TRUE,
                FALSE,
                TRUE,
                FALSE,
                TRUE,
                FALSE,
                TRUE,
                TRUE,
                TRUE,
                TRUE,
                TRUE,
                FALSE,
                TRUE,
                FALSE
              )
          )
        return(node_item_df)
      }

    parse_custody <-
      function(page, return_wide)  {
        parse_custody_nodes <-
          function(page, return_wide) {
            check_nodes <-
              page %>%
              html_nodes('.main td img') %>%
              html_attr('alt') %>%
              str_trim()

            node_df <-
              data_frame(nodeName = check_nodes) %>%
              left_join(get_check_box_value_df()) %>%
              bind_cols(get_node_item_df()) %>%
              mutate(value = T) %>%
              dplyr::filter(isNodeChecked == T) %>%
              mutate(nameEntityManager = name_entity_manager) %>%
              dplyr::select(nameEntityManager, nameItem, value) %>%
              suppressMessages()
            if (return_wide) {
              column_order <-
                c('nameEntityManager', node_df$nameItem)

              node_df <-
                node_df %>%
                spread(nameItem, value) %>%
                dplyr::select(one_of(column_order))
            }
            return(node_df)
          }

        section_data <-
          page %>%
          parse_custody_nodes(return_wide = return_wide)
        return(section_data)

      }

    section_data <-
      page %>%
      parse_custody(return_wide = return_wide) %>%
      mutate(idCRD) %>%
      dplyr::select(idCRD, everything())

    node_text <-
      page %>%
      parse_node_table_to_text(css_node = '#ctl00_ctl00_cphMainContent_cphAdvFormContent_CustodyPH_ctl00_trIAPDHeader + tr + tr')

    has_nodes <-
      node_text %>% str_count('\\$') %>% sum > 0

    if (has_nodes) {
      custody_name_df <-
        data_frame(
          nameItem = c(
            "amountAUMClientFundsInCustody",
            "countClientFundsInCustody",
            "amountAUMClientFundsInCustodyRelatedParty",
            "countClientFundsInCustodyRelatedParty",
            'countQualifiedCustodians',
            'monthYearIndependentInspection'
          ),
          hit_words =
            c(
              '^custody',
              '^custody',
              '^for which your related persons',
              '^for which your related persons',
              'in connection with advisory services you provide to',
              'during your last fiscal year'
            ),
          off_set = c(4, 6, 5, 7, 2, 1),
          is_numeric_node = c(TRUE, TRUE, TRUE, TRUE, TRUE, F)
        )
      find_text_node_safe <-
        possibly(find_text_node, NULL)
      custody_value_df <-
        1:nrow(custody_name_df) %>%
        map_df(function(x) {
          value_exists <-
            find_text_node_safe(
              node_text = node_text,
              hit_words = custody_name_df$hit_words[[x]],
              off_set = custody_name_df$off_set[[x]],
              is_numeric_node = custody_name_df$is_numeric_node[[x]]
            ) %>% length > 0

          if (value_exists) {
            value <-
              find_text_node_safe(
                node_text = node_text,
                hit_words = custody_name_df$hit_words[[x]],
                off_set = custody_name_df$off_set[[x]],
                is_numeric_node = custody_name_df$is_numeric_node[[x]]
              ) %>%
              as.character() %>%
              suppressWarnings()
          } else {
            value <- NA
          }

          val_df <-
            data_frame(nameItem =
                         custody_name_df$nameItem[[x]],
                       value = value)
          return(val_df)
        }) %>%
        distinct() %>%
        dplyr::filter(!value %>% is.na) %>%
        spread(nameItem, value) %>%
        mutate(nameEntityManager = name_entity_manager) %>%
        mutate_adv_data() %>%
        dplyr::select(nameEntityManager, everything()) %>%
        suppressWarnings()

      section_data <-
        custody_value_df %>%
        left_join(section_data) %>%
        suppressMessages()
    }
    section_data <-
      section_data %>%
      dplyr::select(idCRD, nameEntityManager, everything())
    return(section_data)
  }

get_section_10_data <-
  function(url = 'http://www.adviserinfo.sec.gov/IAPD/content/viewform/adv/Sections/iapd_AdvControlPersonsSection.aspx?ORG_PK=142979&FLNG_PK=00AB630A0008018801764C5100236B05056C8CC0'
  ) {
    idCRD <-
      url %>%
      get_pk_url_crd()

    page <-
      url %>%
      get_html_page()

    get_node_item_df <-
      function() {
        node_item_df <-
          c('hasControlPersonUnnamed') %>%
          get_item_name_yes_no_df()
        return(node_item_df)
      }

    parse_section_10 <-
      function(page)  {
        parse_section_10_nodes <-
          function(page) {
            check_nodes <-
              page %>%
              html_nodes('.main td img') %>%
              html_attr('alt') %>%
              str_trim()

            section_data <-
              data_frame(nodeName = check_nodes) %>%
              left_join(get_check_box_value_df()) %>%
              bind_cols(get_node_item_df()) %>%
              dplyr::filter(isNodeChecked == T) %>%
              dplyr::select(nameItem, valueItem) %>%
              suppressMessages()

            column_order <-
              section_data$nameItem

            section_data <-
              section_data %>%
              spread(nameItem, valueItem) %>%
              dplyr::select(one_of(column_order))

            return(section_data)
          }

        name_entity_manager <-
          page %>%
          get_entity_manager_name()

        section_data <-
          page %>%
          parse_section_10_nodes()

        section_data <-
          data_frame(nameEntityManager = name_entity_manager) %>%
          bind_cols(list(section_data))

        return(section_data)

      }

    section_data <-
      page %>%
      parse_section_10()

    section_data <-
      data_frame(idCRD) %>%
      bind_cols(section_data)

    return(section_data)
  }

get_section_11_data <-
  function(url = 'http://www.adviserinfo.sec.gov/IAPD/content/viewform/adv/Sections/iapd_AdvDisciplinarySection.aspx?ORG_PK=142979&FLNG_PK=00AB630A0008018801764C5100236B05056C8CC0',
           return_wide = T) {
    idCRD <-
      url %>%
      get_pk_url_crd()

    page <-
      url %>%
      get_html_page()

    get_node_item_df <-
      function() {
        node_item_df <-
          c(
            "hasManagementSupervisedPersonEvent",
            "hasManagerFelonyPleaConviction",
            "hasManagerFelonyCharge",
            "hasManagerMisdemeanorPleaConviction",
            "hasManagerMisdemeanrCharge",
            "hasManagerSEC_CFTCFalseStatementOmission",
            "hasManagerSEC_CFTCStatuteViolation",
            "hasManagerSEC_CFTCAuthorizationAction",
            "hasManagerSEC_CFTCOrderAgainst",
            "hasManagerSEC_CFCPenaltyCeaseDesist",
            "hasManagerFederalStateForeignFalseStatement",
            "hasManagerFederalStateForeignInvestmentViolation",
            "hasManagerFederalStateForeignBusinessRevokeSuspended",
            "hasManagerFederalStateForeignOrderAgainst",
            "hasManagerFederalStateForeignLicenseRevoked",
            "hasManagerSelfRegulatedBodyFalseStatement",
            "hasManagerSelfRegulatedBodyRuleViolation",
            "hasManagerSelfRegulatedBodyBusinessRevokeSuspension",
            "hasManagerSelfRegulatedBodyActivityBan",
            "hasManagerAttorneyAccountantFederalContractorPriorBanRevoke",
            "isManagerSubjectToRegulatoryProceeding",
            "hasManagerDomesticForeignCourtEnjoinedInvestmentActivity",
            "hasManagerDomesticForeignCourtGuiltyStatuteViolation",
            "hasManagerDomesticForeignCourtDismissedActionSettlementPursuant",
            "isManagerDomesticForeignCourtSubjectToProceeding"
          ) %>%
          get_item_name_yes_no_df()
        return(node_item_df)
      }

    parse_section <-
      function(page, return_wide)  {
        name_entity_manager <-
          page %>%
          get_entity_manager_name()

        parse_section_nodes <-
          function(page) {
            check_nodes <-
              page %>%
              html_nodes('.main td img') %>%
              html_attr('alt') %>%
              str_trim()

            section_data <-
              data_frame(nodeName = check_nodes) %>%
              left_join(get_check_box_value_df()) %>%
              bind_cols(get_node_item_df()) %>%
              dplyr::filter(isNodeChecked == T) %>%
              mutate(nameEntityManager = name_entity_manager) %>%
              dplyr::select(nameEntityManager, nameItem, value = valueItem) %>%
              suppressMessages()

            if (return_wide) {
              col_order <-
                c('nameEntityManager', section_data$nameItem)

              section_data <-
                section_data %>%
                spread(nameItem, value) %>%
                suppressWarnings() %>%
                dplyr::select(one_of(col_order))
            }
            return(section_data)
          }

        section_data <-
          page %>%
          parse_section_nodes()

        return(section_data)

      }

    section_data <-
      page %>%
      parse_section(return_wide = return_wide) %>%
      mutate(idCRD) %>%
      dplyr::select(idCRD, everything())

    return(section_data)
  }

get_section_12_data <-
  function(url = 'http://www.adviserinfo.sec.gov/IAPD/content/viewform/adv/Sections/iapd_AdvSmallBusinessSection.aspx?ORG_PK=150510&FLNG_PK=00B175BA0008018601B35551000582C5056C8CC0'
  ) {
    idCRD <-
      url %>%
      get_pk_url_crd()

    page <-
      url %>%
      get_html_page()

    get_node_item_df <-
      function() {
        node_item_df <-
          c(
            'hasAssetsOver5mFiscalYearEnd',
            'hasControlRelatedAssetManagerAssetsOver25mFiscalYearEnd',
            'hasControlRelatedAssetManagerAssetsOver5mFiscalYearEnd',
            'isControlledByAssetManagerAssetsOver25mFiscalYearEnd',
            'isControlledByAssetManagerAssetsOver5mFiscalYearEnd'
          ) %>%
          get_item_name_yes_no_df()
        return(node_item_df)
      }

    parse_section <-
      function(page)  {
        parse_section_nodes <-
          function(page) {
            name_entity_manager <-
              page %>%
              get_entity_manager_name()

            check_nodes <-
              page %>%
              html_nodes('.main td img') %>%
              html_attr('alt') %>%
              str_trim()

            has_nodes <-
              data_frame(nodeName = check_nodes) %>%
              left_join(get_check_box_value_df()) %>%
              suppressMessages() %>%
              bind_cols(get_node_item_df()) %>%
              dplyr::filter(isNodeChecked == T) %>% nrow > 0


            if (has_nodes) {
              section_data <-
                data_frame(nodeName = check_nodes) %>%
                left_join(get_check_box_value_df()) %>%
                bind_cols(get_node_item_df()) %>%
                dplyr::filter(isNodeChecked == T) %>%
                dplyr::select(nameItem, valueItem) %>%
                dplyr::select(nameEntityManager, everything) %>%
                mutate(nameEntityManager = name_entity_manager) %>%
                dplyr::rename(value = valueItem) %>%
                group_by(nameItem) %>%
                mutate(countItem = 1:n()) %>%
                ungroup %>%
                suppressMessages() %>%
                mutate(
                  countItem = countItem - 1,
                  countItem = countItem %>% as.character,
                  countItem = ifelse(countItem == "0", '', countItem)
                ) %>%
                unite(item, nameItem, countItem, sep = '') %>%
                distinct() %>%
                suppressWarnings()

              col_order <-
                c('nameEntityManager', section_data$item)

              section_data <-
                section_data %>%
                spread(item, value) %>%
                dplyr::select(one_of(col_order))
            } else {
              section_data <-
                data_frame(nameEntityManager = name_entity_manager)
            }

            return(section_data)
          }

        section_data <-
          page %>%
          parse_section_nodes() %>%
          mutate(idCRD) %>%
          dplyr::select(idCRD, everything())


        return(section_data)

      }

    section_data <-
      page %>%
      parse_section() %>%
      mutate(idCRD) %>%
      dplyr::select(idCRD, everything())

    return(section_data)
  }

get_schedule_a_data <-
  function(url = 'http://www.adviserinfo.sec.gov/IAPD/content/viewform/adv/Sections/iapd_AdvScheduleASection.aspx?ORG_PK=150510&FLNG_PK=00B175BA0008018601B35551000582C5056C8CC0',
           return_wide = F) {
    idCRD <-
      url %>%
      get_pk_url_crd()

    page <-
      url %>%
      get_html_page()

    parse_section <-
      function(page)  {
        name_entity_manager <-
          page %>%
          get_entity_manager_name()

        table_exists <-
          page %>% html_nodes(css = '#ctl00_ctl00_cphMainContent_cphAdvFormContent_ScheduleAPHSection_ctl00_ownersGrid') %>% length > 0

        if (table_exists) {
          table_data <-
            page %>%
            html_nodes(css = '#ctl00_ctl00_cphMainContent_cphAdvFormContent_ScheduleAPHSection_ctl00_ownersGrid') %>%
            html_table(fill = T, header = F) %>%
            data.frame(stringsAsFactors = F) %>%
            as_data_frame() %>%
            slice(-1) %>%
            mutate(X1 = X1 %>% str_to_upper())

          names(table_data) <-
            c(
              'nameEntityManagerOwner',
              'idTypeEntityManagerOwner',
              'statusEntityManagerOwner',
              'monthYearEntityManagerOwnerPurchased',
              'idRangeManagerEntityOwnership',
              'isControlPerson',
              'isPublicReportingEntity',
              'idEntityManagerOwner'
            )

          table_data <-
            table_data %>%
            mutate(
              idCRD,
              isControlPerson = if_else(isControlPerson == "Y", TRUE, FALSE),
              isPublicReportingEntity = if_else(isPublicReportingEntity == "Y", TRUE, FALSE),
              dateEntityManagerOwnerPurchased = '01/' %>% paste0(monthYearEntityManagerOwnerPurchased) %>% lubridate::dmy %>% as.Date
            ) %>%
            left_join(get_type_manager_entity_owner_df()) %>%
            left_join(get_range_entity_owner_df()) %>%
            dplyr::select(-monthYearEntityManagerOwnerPurchased) %>%
            suppressMessages()
          has_individual_owners <-
            table_data %>%
            dplyr::filter(idTypeEntityManagerOwner == "I") %>% nrow > 0
          if (has_individual_owners) {
            individual_data <-
              table_data %>%
              dplyr::filter(idTypeEntityManagerOwner == "I")

            individual_data <-
              individual_data$nameEntityManagerOwner %>%
              map_df(parse_manager_owner_name) %>%
              right_join(individual_data) %>%
              suppressMessages() %>%
              mutate(
                countDash = idEntityManagerOwner %>% str_count('\\-'),
                typeIDEntityManagerOwner = if_else(countDash == 0, 'idEmployee', 'idSSN')
              ) %>%
              dplyr::select(-countDash)
          }

          has_entity_owners <-
            table_data %>% dplyr::filter(isEntityOwnerManagerEntity == T) %>% nrow > 0
          if (has_entity_owners) {
            entity_df <-
              table_data %>% dplyr::filter(isEntityOwnerManagerEntity == T) %>%
              mutate(
                countDash = idEntityManagerOwner %>% str_count('\\-'),
                typeIDEntityManagerOwner = if_else(countDash == 0, 'idCRD', 'idEIN')
              ) %>%
              dplyr::select(-countDash)
            if ('individual_data' %>% exists) {
              table_data <-
                individual_data %>%
                bind_rows(entity_df) %>%
                mutate(
                  nameCommonEntityOwnerManager = if_else(
                    nameCommonEntityOwnerManager %>% is.na,
                    nameEntityManagerOwner,
                    nameCommonEntityOwnerManager
                  ),
                  nameEntityManagerOwner = if_else(
                    nameFullEntityOwnerManager %>% is.na,
                    nameEntityManagerOwner,
                    nameFullEntityOwnerManager
                  )
                ) %>%
                suppressMessages() %>%
                dplyr::select(idCRD, everything())
            } else {
              table_data <-
                entity_df %>%
                mutate(nameFullEntityOwnerManager = nameEntityManagerOwner,
                       nameCommonEntityOwnerManager = nameEntityManagerOwner)
            }
          } else {
            table_data <-
              individual_data %>%
              mutate(
                nameCommonEntityOwnerManager = if_else(
                  nameCommonEntityOwnerManager %>% is.na,
                  nameEntityManagerOwner,
                  nameCommonEntityOwnerManager
                ),
                nameEntityManagerOwner = if_else(
                  nameFullEntityOwnerManager %>% is.na,
                  nameFullEntityOwnerManager,
                  nameFullEntityOwnerManager
                )
              ) %>%
              dplyr::select(idCRD, everything())
          }

          table_data <-
            table_data %>%
            mutate(
              nameFullEntityOwnerManager = if_else(
                nameFullEntityOwnerManager %>% is.na,
                nameEntityManagerOwner,
                nameFullEntityOwnerManager
              )
            )

          table_data <-
            table_data %>%
            mutate(nameEntityManager = name_entity_manager) %>%
            dplyr::select(
              nameEntityManager,
              nameCommonEntityOwnerManager,
              statusEntityManagerOwner,
              isEntityOwnerManagerEntity,
              dateEntityManagerOwnerPurchased,
              rangeManagerEntityOwnership,
              idEntityManagerOwner,
              typeEntityManagerOwner,
              isControlPerson,
              idTypeEntityManagerOwner:typeIDEntityManagerOwner,
              everything()
            )
        } else {
          table_data <-
            data_frame(nameEntityManager = name_entity_manager)
        }
        return(table_data)
      }

    section_data <-
      page %>%
      parse_section()

    section_data <-
      section_data %>%
      mutate(idCRD) %>%
      dplyr::select(idCRD, everything())

    if (return_wide) {
      section_data <-
        section_data %>%
        mutate_all(.funs = as.character) %>%
        mutate(countItem = 1:n(),
               idCRD = idCRD %>% as.numeric) %>%
        gather(item, value, -c(nameEntityManager, countItem, idCRD)) %>%
        mutate(
          countItem = countItem - 1,
          countItem = countItem %>% as.character,
          countItem = ifelse(countItem == "0", '', countItem)
        ) %>%
        unite(item, item, countItem, sep = '') %>%
        dplyr::filter(!value %>% is.na()) %>%
        suppressWarnings()

      column_order <-
        c('idCRD', 'nameEntityManager', section_data$item)

      section_data <-
        section_data %>%
        spread(item, value) %>%
        dplyr::select(one_of(column_order))

      section_data <-
        section_data %>%
        mutate_at(.cols =
                    section_data %>% dplyr::select(matches("^amount|^count")) %>% names,
                  .funs = as.numeric) %>%
        mutate_at(.cols =
                    section_data %>% dplyr::select(matches("^has|^is")) %>% names,
                  .funs = as.logical) %>%
        mutate_at(.cols =
                    section_data %>% dplyr::select(matches("^date")) %>% names,
                  funs(. %>% lubridate::ymd()))
    }

    return(section_data %>% distinct())
  }

get_schedule_b_data <-
  function(url = 'http://www.adviserinfo.sec.gov/IAPD/content/viewform/adv/Sections/iapd_AdvScheduleBSection.aspx?ORG_PK=142979&FLNG_PK=00AB630A0008018801764C5100236B05056C8CC0',
           return_wide = F) {
    idCRD <-
      url %>%
      get_pk_url_crd()
    page <-
      url %>%
      get_html_page()

    parse_section <-
      function(page)  {
        name_entity_manager <-
          page %>%
          get_entity_manager_name()

        table_exists <-
          page %>% html_nodes(css = '#ctl00_ctl00_cphMainContent_cphAdvFormContent_ScheduleBPHSection_ctl00_ownersGrid') %>% length > 0

        if (table_exists) {
          table_data <-
            page %>%
            html_nodes(css = '#ctl00_ctl00_cphMainContent_cphAdvFormContent_ScheduleBPHSection_ctl00_ownersGrid') %>%
            html_table(fill = T, header = F) %>%
            data.frame(stringsAsFactors = F) %>%
            as_data_frame() %>%
            slice(-1) %>%
            mutate(X1 = X1 %>% str_to_upper())

          names(table_data) <-
            c(
              'nameEntityManagerOwner',
              'idTypeEntityManagerOwner',
              'nameEntityManagerOwned',
              'statusEntityManagerOwner',
              'monthYearEntityManagerOwnerPurchased',
              'idRangeManagerEntityOwnership',
              'isControlPerson',
              'isPublicReportingEntity',
              'idEntityManagerOwner'
            )

          table_data <-
            table_data %>%
            mutate(
              idCRD,
              isControlPerson = if_else(isControlPerson == "Y", TRUE, FALSE),
              isPublicReportingEntity = if_else(isPublicReportingEntity == "Y", TRUE, FALSE),
              dateEntityManagerOwnerPurchased = '01/' %>% paste0(monthYearEntityManagerOwnerPurchased) %>% lubridate::dmy %>% as.Date
            ) %>%
            left_join(get_type_manager_entity_owner_df()) %>%
            left_join(get_range_entity_owner_df()) %>%
            dplyr::select(-monthYearEntityManagerOwnerPurchased) %>%
            suppressMessages()

          has_individual_data <-
            table_data %>%
            dplyr::filter(idTypeEntityManagerOwner == "I") %>% nrow > 0


          if (has_individual_data) {
            has_entity_df <-
              table_data %>% dplyr::filter(isEntityOwnerManagerEntity == T) %>% nrow > 0
            if (has_entity_df) {
              entity_df <-
                table_data %>% dplyr::filter(isEntityOwnerManagerEntity == T)

              entity_df <-
                entity_df %>%
                mutate(
                  countDash = idEntityManagerOwner %>% str_count('\\-'),
                  typeIDEntityManagerOwner = if_else(countDash == 0, 'idCRD', 'idEIN')
                ) %>%
                dplyr::select(-countDash)

              individual_data <-
                table_data %>%
                dplyr::filter(idTypeEntityManagerOwner == "I")


              individual_data <-
                individual_data$nameEntityManagerOwner %>%
                map_df(parse_manager_owner_name) %>%
                right_join(individual_data) %>%
                suppressMessages() %>%
                mutate(
                  countDash = idEntityManagerOwner %>% str_count('\\-'),
                  typeIDEntityManagerOwner = if_else(countDash == 0, 'idEmployee', 'idSSN')
                ) %>%
                dplyr::select(-countDash)

              table_data <-
                individual_data %>%
                bind_rows(entity_df) %>%
                mutate(
                  nameCommonEntityOwnerManager = if_else(
                    nameCommonEntityOwnerManager %>% is.na,
                    nameEntityManagerOwner,
                    nameCommonEntityOwnerManager
                  ),
                  nameEntityManagerOwner = if_else(
                    nameFullEntityOwnerManager %>% is.na,
                    nameEntityManagerOwner,
                    nameFullEntityOwnerManager
                  )
                ) %>%
                suppressMessages() %>%
                mutate(
                  countDash = idEntityManagerOwner %>% str_count('\\-'),
                  typeIDEntityManagerOwner = if_else(countDash == 0, 'idEmployee', 'idSSN')
                ) %>%
                dplyr::select(-countDash) %>%
                dplyr::select(idCRD, everything())
            } else {
              individual_data <-
                table_data %>%
                dplyr::filter(idTypeEntityManagerOwner == "I")

              individual_data <-
                individual_data$nameEntityManagerOwner %>%
                map_df(parse_manager_owner_name) %>%
                right_join(individual_data) %>%
                suppressMessages()

              table_data <-
                individual_data %>%
                mutate(
                  nameCommonEntityOwnerManager = if_else(
                    nameCommonEntityOwnerManager %>% is.na,
                    nameEntityManagerOwner,
                    nameCommonEntityOwnerManager
                  ),
                  nameEntityManagerOwner = if_else(
                    nameFullEntityOwnerManager %>% is.na,
                    nameEntityManagerOwner,
                    nameFullEntityOwnerManager
                  )
                ) %>%
                suppressMessages() %>%
                dplyr::select(idCRD, everything()) %>%
                mutate(
                  countDash = idEntityManagerOwner %>% str_count('\\-'),
                  typeIDEntityManagerOwner = if_else(countDash == 0, 'idEmployee', 'idSSN')
                ) %>%
                dplyr::select(-countDash)
            }
          } else {
            table_data <-
              table_data %>%
              mutate(
                countDash = idEntityManagerOwner %>% str_count('\\-'),
                typeIDEntityManagerOwner = if_else(countDash == 0, 'idCRD', 'idEIN')
              ) %>%
              dplyr::select(-countDash) %>%
              dplyr::select(idCRD, everything())
          }


          if ('nameFullEntityOwnerManager' %in% names(table_data)) {
            table_data <-
              table_data %>%
              mutate(
                nameFullEntityOwnerManager = if_else(
                  nameFullEntityOwnerManager %>% is.na,
                  nameEntityManagerOwner,
                  nameFullEntityOwnerManager
                )
              )
          } else {
            table_data <-
              table_data %>%
              mutate(nameFullEntityOwnerManager = nameEntityManagerOwner)
          }

          if ('nameCommonEntityOwnerManager' %in% names(table_data)) {
            table_data <-
              table_data %>%
              mutate(
                nameCommonEntityOwnerManager = if_else(
                  nameCommonEntityOwnerManager %>% is.na,
                  nameEntityManagerOwner,
                  nameCommonEntityOwnerManager
                )
              )
          } else {
            table_data <-
              table_data %>%
              mutate(nameCommonEntityOwnerManager = nameEntityManagerOwner)
          }

          table_data <-
            table_data %>%
            mutate(nameEntityManager = name_entity_manager) %>%
            dplyr::select(
              nameEntityManager,
              nameEntityManagerOwner,
              statusEntityManagerOwner,
              isEntityOwnerManagerEntity,
              dateEntityManagerOwnerPurchased,
              rangeManagerEntityOwnership,
              idEntityManagerOwner,
              typeEntityManagerOwner,
              isControlPerson,
              idTypeEntityManagerOwner:typeIDEntityManagerOwner,
              everything()
            )
        } else {
          table_data <-
            data_frame(nameEntityManager = name_entity_manager)
        }
        return(table_data)
      }

    section_data <-
      page %>%
      parse_section()

    section_data <-
      section_data %>%
      mutate(idCRD) %>%
      dplyr::select(idCRD, everything())

    if (return_wide) {
      section_data <-
        section_data %>%
        mutate_all(.funs = as.character) %>%
        mutate(countItem = 1:n(),
               idCRD = idCRD %>% as.numeric) %>%
        gather(item, value, -c(nameEntityManager, countItem, idCRD)) %>%
        mutate(
          countItem = countItem - 1,
          countItem = countItem %>% as.character,
          countItem = ifelse(countItem == "0", '', countItem)
        ) %>%
        unite(item, item, countItem, sep = '') %>%
        dplyr::filter(!value %>% is.na()) %>%
        suppressWarnings()

      column_order <-
        c('idCRD', 'nameEntityManager', section_data$item)

      section_data <-
        section_data %>%
        spread(item, value) %>%
        dplyr::select(one_of(column_order))

      section_data <-
        section_data %>%
        mutate_at(.cols =
                    section_data %>% dplyr::select(matches("^amount|^count")) %>% names,
                  .funs = as.numeric) %>%
        mutate_at(.cols =
                    section_data %>% dplyr::select(matches("^has|^is")) %>% names,
                  .funs = as.logical) %>%
        mutate_at(.cols =
                    section_data %>% dplyr::select(matches("^date")) %>% names,
                  funs(. %>% lubridate::ymd()))
    }

    return(section_data %>% distinct())
  }

get_schedule_d_data <-
  function(url = 'http://www.adviserinfo.sec.gov/IAPD/content/viewform/adv/Sections/iapd_AdvScheduleDSection.aspx?ORG_PK=284340&FLNG_PK=0573726A0008018802C736510026C985056C8CC0',
           join_data = F,
           return_wide = F) {
    idCRD <-
      url %>%
      get_pk_url_crd()

    page <-
      url %>%
      get_html_page()

    name_entity_manager <-
      page %>% get_entity_manager_name()

    parse_schedule_d <-
      function(page, join_data, return_wide)  {
        table_ids <-
          page %>%
          html_nodes('.main div') %>%
          html_attr('id')

        table_ids <-
          table_ids[!table_ids %>% is.na()]

        table_ids <-
          table_ids[table_ids %>% str_detect('PFID|pageMessages|Lookup|Header') == F]

        table_node_df <-
          data_frame(idCSSTable = table_ids %>% paste0('#', .)) %>%
          mutate(idTable = 1:n())

        table_nodes <-
          page %>%
          html_nodes('.flatBorderTable tr td .PaperFormTableData')

        all_table_node_df <-
          1:length(table_nodes) %>%
          map_df(function(x) {
            raw_nodes <-
              table_nodes[[x]] %>%
              html_text(trim = T) %>%
              str_replace_all('\r|\n|\t', '') %>%
              stri_replace_all_charclass("\\p{WHITE_SPACE}", " ") %>%
              stri_trim_both() %>%
              gsub("^\\s+|\\s+$", "", .) %>%
              str_split('\\  ') %>%
              flatten_chr %>%
              str_trim

            raw_nodes <-
              raw_nodes[!raw_nodes == '']

            if (raw_nodes %>% length > 0) {
              data_frame(idTable = x, nodeText = raw_nodes)
            }
          }) %>%
          mutate(idRow = 1:n())

        parse_other_office_locations <-
          function(page, return_wide) {
            table_nodes <-
              page %>%
              html_nodes('.flatBorderTable tr td .PaperFormTableData')

            end_table_df <-
              1:length(table_nodes) %>%
              map_df(function(x) {
                raw_nodes <-
                  table_nodes[[x]] %>%
                  html_text(trim = T) %>%
                  str_replace_all('\r|\n|\t', '') %>%
                  stri_replace_all_charclass("\\p{WHITE_SPACE}", " ") %>%
                  stri_trim_both() %>%
                  gsub("^\\s+|\\s+$", "", .) %>%
                  str_split('\\  ') %>%
                  flatten_chr

                raw_nodes <-
                  raw_nodes[!raw_nodes == '']

                is_end_table <-
                  raw_nodes %>% grep("A. PRIVATE FUND|SECTION 6", .) %>% length > 0
                data_frame(idTable = x, isEndTable = is_end_table, nodesText = raw_nodes)
              })

            has_end_table <-
              end_table_df %>% dplyr::filter(isEndTable == T) %>% nrow > 0
            if (has_end_table) {
              end_table_no <-
                end_table_df %>%
                dplyr::filter(isEndTable == T) %>%
                slice(1) %>%
                .$idTable - 1
            } else {
              end_table_no <-
                table_nodes %>% length
            }

            location_df <-
              1:(end_table_no) %>%
              map_df(function(x) {
                raw_nodes <-
                  table_nodes[[x]] %>%
                  html_text(trim = T) %>%
                  str_replace_all('\r|\n|\t', '') %>%
                  stri_replace_all_charclass("\\p{WHITE_SPACE}", " ") %>%
                  stri_trim_both() %>%
                  gsub("^\\s+|\\s+$", "", .) %>%
                  str_split('\\  ') %>%
                  flatten_chr

                raw_nodes <-
                  raw_nodes[!raw_nodes == '']

                raw_nodes <-
                  raw_nodes[raw_nodes %>% str_detect(":[A-Z a-z 1-9]")] %>% str_trim
                has_nodes <-
                  raw_nodes %>% length > 0
                if (has_nodes) {
                  data_frame(idTable = x, itemvalueNode = raw_nodes)
                }
              }) %>%
              dplyr::filter(
                !itemvalueNode %>% str_detect(
                  'Name of entity where books|Facis|Facsimile|NAME OF THE INDEPENDENT|SUCH NON-US FUNDS|ITEM|item|Item|SCHEDULE|NVESTMENT ACQUISITION|THE ANALYSIS AND MANAGEMEN|MAIL STOP'
                )
              ) %>%
              dplyr::filter(!itemvalueNode %>% str_detect('NOTE: |Name of the')) %>%
              dplyr::filter(itemvalueNode %>% str_detect('\\:'))
            name_entity_manager <-
              page %>%
              get_entity_manager_name()
            has_other_locations <-
              location_df %>% nrow > 0
            if (has_other_locations) {
              if (location_df$idTable %>% unique %>% min > 1) {
                offset_value <-
                  location_df$idTable %>% unique %>% min - 1

                location_df <-
                  location_df %>%
                  mutate(idTable = idTable - offset_value)
              }
              all_locations <-
                location_df$idTable %>% unique %>%
                map_df(function(x) {
                  locationOfficeSecondary <-
                    location_df %>%
                    dplyr::filter(idTable == x)

                  locationOfficeSecondary %>%
                    separate(itemvalueNode, c('itemNode', 'valueNode'), '\\:') %>%
                    left_join(get_location_name_df()) %>%
                    dplyr::select(idTable, nameNode, valueNode) %>%
                    mutate(
                      valueNode = valueNode %>% str_trim %>% str_to_upper(),
                      nameNode = nameNode %>% paste0('ManagerOfficeSecondary')
                    ) %>%
                    suppressMessages()
                }) %>%
                dplyr::rename(countItem = idTable,
                              item = nameNode,
                              value = valueNode) %>%
                dplyr::filter(!value %>% is.na) %>%
                mutate(nameEntityManager = name_entity_manager) %>%
                dplyr::select(nameEntityManager, everything()) %>%
                arrange(countItem) %>%
                spread(item, value)

              if (names(all_locations) %>% str_count('^address|^city|^country|^state') %>% sum >= 4) {
                if(names(all_locations) %>% str_count('stateManagerOfficeSecondary|zipManagerOfficeSecondary|addressStreet1ManagerOfficeSecondary|cityManagerOfficeSecondary') %>% sum == 4) {
                  all_locations <-
                    all_locations %>%
                    mutate(locationSecondary = addressStreet1ManagerOfficeSecondary %>% paste0(' ', cityManagerOfficeSecondary, ', ', stateManagerOfficeSecondary, ' ', countryManagerOfficeSecondary, ' ', zipManagerOfficeSecondary))
                }
              }

              if (return_wide) {
                all_locations <-
                  all_locations %>%
                  widen_adv_data()
              }
            } else {
              all_locations <-
                data_frame(nameEntityManager = name_entity_manager)
            }
            return(all_locations)
          }

        parse_for_manager_website_data <-
          function(page) {
            nodes <-
              page %>%
              get_html_node_text('.PrintHistRed') %>%
              str_to_lower

            name_entity_manager <-
              page %>% get_entity_manager_name()

            if (nodes %>% grep('http', .) %>% length > 0) {
              urlManager <-
                nodes[nodes %>% grep('http', .)]

              url_df <-
                data_frame(urlManager) %>%
                mutate(countItem = 1:n()) %>%
                gather(item, value, -countItem) %>%
                dplyr::filter(!value %>% is.na) %>%
                spread(item, value) %>%
                mutate(nameEntityManager = name_entity_manager) %>%
                dplyr::select(nameEntityManager, everything())
              rm(nodes)

              url_df <-
                url_df %>%
                widen_adv_data()
            } else {
              url_df <-
                url_df <-
                data_frame(nameEntityManager = name_entity_manager)
            }
            return(url_df)
          }

        parse_related_advisor <-
          function(page, return_wide) {
            name_entity_manager <-
              page %>%
              get_entity_manager_name()

            page_input_ids <-
              page %>%
              html_nodes('input') %>%
              html_attr('id')
            has_adviser_nodes <-
              page_input_ids %>% str_count('ctl00_ctl00_cphMainContent_cphAdvFormContent_AffiliatedAdvisersList_rptrAfflAdvisers_') %>% sum > 0
            if (has_adviser_nodes) {
              node_id_value <-
                page_input_ids[page_input_ids %>% str_detect(
                  'ctl00_ctl00_cphMainContent_cphAdvFormContent_AffiliatedAdvisersList_rptrAfflAdvisers_'
                )] %>%
                paste0('#', ., ' + table')

              related_adviser_node_df <-
                data_frame(cssNode = node_id_value) %>%
                mutate(idTable = 1:n()) %>%
                dplyr::select(idTable, everything())

              related_advisor_df <-
                related_adviser_node_df$idTable %>%
                map_df(function(x) {
                  table_nodes <-
                    page %>%
                    html_nodes(css = related_adviser_node_df$cssNode[x]) %>%
                    html_text(trim = T) %>%
                    str_replace_all('\r|\n|\t', '') %>%
                    stri_replace_all_charclass("\\p{WHITE_SPACE}", " ") %>%
                    stri_trim_both() %>%
                    gsub("^\\s+|\\s+$", "", .) %>%
                    str_split("   +") %>%
                    flatten_chr

                  nameLegalRelatedEntity <-
                    table_nodes[table_nodes %>% grep('Legal Name of', .) + 1] %>% str_replace_all('Related Person: ', '')

                  nameBusinessRelatedEntity <-
                    table_nodes[table_nodes %>% grep('Primary Business Name of', .) + 2]

                  if (table_nodes %>% grep('CRD Number', .) %>% length > 0) {
                    idCRDRelatedEntity <-
                      table_nodes[table_nodes %>% grep('CRD Number', .)] %>% gsub('\\CRD Number', '', .) %>%
                      gsub('\\(|\\)', '', .) %>% gsub(' if any:', '', .)

                    if (idCRDRelatedEntity == '') {
                      idCRDRelatedEntity <-
                        NA
                    } else {
                      idCRDRelatedEntity <-
                        idCRDRelatedEntity %>% str_trim %>% as.numeric
                    }

                  }

                  data_df <-
                    data_frame(
                      idTable = x,
                      nameBusinessRelatedEntity,
                      nameLegalRelatedEntity,
                      idCRDRelatedEntity
                    ) %>%
                    mutate(nameBusinessRelatedEntity = if_else(nameBusinessRelatedEntity == "SAME", nameLegalRelatedEntity, nameBusinessRelatedEntity),
                           idCRDRelatedEntity = idCRDRelatedEntity %>% as.numeric)
                  has_image_check_box <-
                    page %>%
                    html_nodes(related_adviser_node_df$cssNode[x]) %>%
                    html_nodes('img') %>% length == 33
                  if (has_image_check_box) {
                    check_nodes <-
                      page %>%
                      html_nodes(related_adviser_node_df$cssNode[x]) %>%
                      html_nodes('img') %>%
                      html_attr('alt') %>%
                      str_trim()

                    node_df <-
                      data_frame(
                        nameItem = c(
                          'isBrokerDealer',
                          'isOtherInvestmentAdvisor',
                          'isRegisteredMunicipalAdvisor',
                          'isRegisteredSwapDealer',
                          'isMajorSwapParticipant',
                          'isCommodityPoolOperator',
                          'isFutureCommissionMerchant',
                          'isBankThrift',
                          'isTrustCompany',
                          'isAccountingFirm',
                          'isLawFirm',
                          'isInsuranceCompany',
                          'isPensionConsultant',
                          'isRealEstateBrokerDealer',
                          'isNonPooledInvestmentSponsor',
                          'isPooledInvestmentSponsor'
                        ),
                        valueItem = T
                      ) %>%
                      mutate(fullnameItem = nameItem) %>%
                      bind_rows(
                        list(
                          c(
                            'isRelatedPersonControlled',
                            'hasRelatedPersonCommonControl',
                            'isRelatedPersonClientCustodian',
                            'isRelatedPersonNotOperationIndependent'
                          ) %>%
                            get_item_name_yes_no_df(),
                          'isPrivateResidence' %>%
                            has_item_check_name(),
                          c(
                            'isRelatedPersonExemptInvestmentAdviser',
                            'isRelatedPersonForeignEntityRegistered',
                            'hasRelatedPersonSharedSupervisedPerson',
                            'hasRelatedPersonSharedAddress'
                          ) %>%
                            get_item_name_yes_no_df()
                        )
                      )

                    box_df <-
                      data_frame(nodeName = check_nodes) %>%
                      mutate(idTable = x) %>%
                      left_join(get_check_box_value_df()) %>%
                      bind_cols(node_df) %>%
                      dplyr::filter(isNodeChecked == T) %>%
                      dplyr::select(idTable, nameItem, valueItem) %>%
                      suppressMessages() %>%
                      spread(nameItem, valueItem)

                    data_df <-
                      data_df %>%
                      left_join(box_df) %>%
                      suppressMessages()
                  }

                  return(data_df)
                }) %>%
                dplyr::rename(countItem = idTable) %>%
                gather(item, value, -countItem) %>%
                dplyr::filter(!value %>% is.na) %>%
                mutate(nameEntityManager = name_entity_manager) %>%
                dplyr::select(nameEntityManager, everything()) %>%
                arrange(countItem, item) %>%
                spread(item, value) %>%
                dplyr::select(c(nameEntityManager, countItem, nameBusinessRelatedEntity, nameLegalRelatedEntity, everything() ))

              if (return_wide) {
                related_advisor_df <-
                  related_advisor_df %>%
                  widen_adv_data() %>%
                  mutate_adv_data()
              }
            } else {
              related_advisor_df <-
                data_frame(nameEntityManager = name_entity_manager)
            }
            return(related_advisor_df)
          }

        parse_record_locations <-
          function(page, all_table_node_df, return_wide) {
            has_book_keeping_data <-
              all_table_node_df %>%
              dplyr::filter(nodeText %>% str_detect('Briefly describe the books')) %>% nrow > 0

            if (has_book_keeping_data) {
              node_locations <-
                all_table_node_df$nodeText %>% grep('Briefly describe the books', .)

              record_df <-
                c(-1, 5, 7, 10, 12, 14, 16, 18, 19, 20, 21) %>%
                map_df(function(x) {
                  has_value <-
                    all_table_node_df$nodeText[node_locations - x] %>% length >0

                  if (has_value) {
                    val <- all_table_node_df$nodeText[node_locations - x]
                  } else {
                    val <- NA
                  }

                  data_frame(value = val,
                             idNode = x)
                }) %>%
                dplyr::filter(!value %>% is.na()) %>%
                dplyr::filter(
                  !value %>% str_detect('CANCELLED CHECKS ARE HELD BY THE BANK AND KEPT OFFSITE|Number and Street 2:|City:|Country|Number and Street 1')
                )
              record_df <-
                record_df %>%
                mutate(
                  value = value %>% str_replace_all('Name of entity where books and records are kept:', '')
                ) %>%
                left_join(data_frame(
                  idNode = c(-1, 5, 7, 10, 12, 14, 16, 18, 19, 20, 21) ,
                  nameItem = c(
                    'descriptionRecordsKept',
                    'faxRecordKeeper',
                    'phoneRecordKeeper',
                    'zipRecordKeeper',
                    'countryRecordKeeper',
                    'stateRecordKeeper',
                    'cityRecordKeeper',
                    'addressStreet2RecordKeeper',
                    'addressStreet1RecordKeeper',
                    'addressStreet1RecordKeeper',
                    'nameEntityRecordKeeper'
                  )
                )) %>%
                group_by(nameItem) %>%
                mutate(countItem = 1:n()) %>%
                ungroup %>%
                suppressMessages() %>%
                dplyr::select(-idNode) %>%
                dplyr::select(countItem, nameItem, everything()) %>%
                arrange(countItem, nameItem) %>%
                mutate(value = value %>% str_to_upper) %>%
                spread(nameItem, value) %>%
                mutate(nameEntityManager = name_entity_manager) %>%
                dplyr::select(nameEntityManager,  countItem, everything())

              if ('addressStreet2RecordKeeper' %in% names(record_df)) {
                record_df <-
                  record_df %>%
                  replace_na(list(addressStreet2RecordKeeper = '')) %>%
                  unite(addressStreet1RecordKeeper, addressStreet1RecordKeeper, addressStreet2RecordKeeper, sep = ' ')
              }

              if (names(record_df) %>% str_count('^addressStreet1|^city|^state|^country') %>% sum >= 4) {
                record_df <-
                  record_df %>%
                  mutate(locationRecordKeeper = addressStreet1RecordKeeper %>% paste0(' ', cityRecordKeeper, ', ', stateRecordKeeper, ' ', countryRecordKeeper, ' ', zipRecordKeeper))
              }

              if (return_wide) {
                record_df <-
                  record_df %>%
                  widen_adv_data()

              }

              if (record_df %>% ncol > 100 | record_df %>% nrow > 8) {
                record_df <-
                  data_frame(nameEntityManager = name_entity_manager)
              }
            } else {
              record_df <-
                data_frame(nameEntityManager = name_entity_manager)
            }

            return(record_df)


          }

        parse_control_person_data <-
          function(page, return_wide) {
            nodes <-
              page %>%
              get_html_node_text('.PrintHistRed')

            name_entity_manager <-
              page %>% get_entity_manager_name()

            if (nodes[nodes %>% str_detect('INDIRECTLY CONTROLS')] %>% length > 0) {
              node_locations <-
                nodes %>% grep('INDIRECTLY CONTROLS', .)

              control_person_df <-
                8:0 %>%
                map_df(function(x) {
                  data_frame(value = nodes[node_locations - x],
                             idNode = x)
                }) %>%
                left_join(data_frame(
                  idNode = 8:0,
                  nameItem = c(
                    'nameControlPerson',
                    'idCRDControlPerson',
                    'dateEffectiveControlPerson',
                    'addressStreet1ControlPerson',
                    'cityControlPerson',
                    'stateControlPerson',
                    'countryControlPerson',
                    'zipControlPerson',
                    'descriptionControlPerson'
                  )
                )) %>%
                group_by(nameItem) %>%
                mutate(countItem = 1:n()) %>%
                ungroup %>%
                suppressMessages() %>%
                dplyr::select(-idNode)

              control_person_df <-
                control_person_df %>%
                spread(nameItem, value) %>%
                mutate(nameEntityManager = name_entity_manager) %>%
                dplyr::select(nameEntityManager, everything())

              control_person_df <-
                control_person_df %>%
                mutate_at(.cols =
                            control_person_df %>% dplyr::select(matches("^date")) %>% names,
                          funs(. %>% lubridate::mdy())) %>%
                mutate_at(.cols =
                            control_person_df %>% dplyr::select(matches("^country[A-Z]|^name|^state|^city")) %>% names,
                          .funs = str_to_upper) %>%
                dplyr::select(nameEntityManager, nameControlPerson, descriptionControlPerson, everything())

              if (names(control_person_df) %>% str_count('^address|^city|^country|^state') %>% sum >= 4) {
                control_person_df <-
                  control_person_df %>%
                  mutate(locationControlPerson = addressStreet1ControlPerson %>% paste0(' ', cityControlPerson, ', ', stateControlPerson, ' ', countryControlPerson, ' ', zipControlPerson)) %>%
                  dplyr::select(nameEntityManager:countItem, locationControlPerson, everything())
              }

              if ('nameControlPerson' %in% names(control_person_df)) {
                name_df <-
                  control_person_df$nameControlPerson %>%
                  map_df(parse_manager_owner_name)

                names(name_df) <-
                  names(name_df) %>%
                  str_replace_all('EntityManagerOwner|EntityOwnerManager','ControlPerson')

                control_person_df <-
                  control_person_df %>%
                  left_join(name_df) %>%
                  suppressMessages() %>%
                  dplyr::select(nameEntityManager, nameCommonControlPerson, descriptionControlPerson, everything())

              }
              if (return_wide) {
                control_person_df <-
                  control_person_df %>%
                  mutate(
                    countItem = countItem - 1,
                    countItem = countItem %>% as.character,
                    countItem = ifelse(countItem == "0", '', countItem)
                  ) %>%
                  unite(item, nameItem, countItem, sep = '') %>%
                  mutate(nameEntityManager = name_entity_manager) %>%
                  dplyr::select(nameEntityManager, everything())

                col_order <-
                  c('nameEntityManager', control_person_df$item)

                control_person_df <-
                  control_person_df %>%
                  spread(item, value) %>%
                  dplyr::select(one_of(col_order))

                control_person_df <-
                  control_person_df %>%
                  mutate_at(.cols =
                              control_person_df %>% dplyr::select(matches("^idCRD")) %>% names,
                            .funs = as.numeric) %>%
                  mutate_at(
                    .cols =
                      control_person_df %>% dplyr::select(matches("^date")) %>% names,
                    funs(. %>% lubridate::mdy())
                  )
              }
            } else {
              control_person_df <-
                data_frame(nameEntityManager = name_entity_manager)
            }
            return(control_person_df)
          }

        parse_public_control_persons <-
          function(page, all_table_node_df) {
            has_public_control_data <-
              all_table_node_df %>% dplyr::filter(nodeText == 'Full legal name of the public reporting company:') %>% nrow > 0
            if (has_public_control_data) {
              namePublicCompanyLegal <-
                all_table_node_df %>%
                dplyr::filter(
                  idRow  == all_table_node_df %>% dplyr::filter(
                    nodeText == 'Full legal name of the public reporting company:'
                  ) %>% .$idRow + 1
                ) %>% .$nodeText

              idCIKPublicCompany <-
                all_table_node_df %>%
                dplyr::filter(
                  idRow  == all_table_node_df %>% dplyr::filter(
                    nodeText %>% str_detect("The public reporting company's CIK number")
                  ) %>% .$idRow + 1
                ) %>% .$nodeText %>%
                as.numeric()

              public_control_df <-
                data_frame(nameEntityManager = name_entity_manager) %>%
                mutate(idCIKPublicCompany, namePublicCompanyLegal)
            } else {
              public_control_df <-
                data_frame(nameEntityManager = name_entity_manager)
            }

            return(public_control_df)


          }

        parse_other_disclosures <-
          function(page, return_wide) {
            has_other_text_node <-
              !page %>%
              html_nodes(
                '#ctl00_ctl00_cphMainContent_cphAdvFormContent_SchedDMisc_ctl00_trIAPDHeader + tr span'
              ) %>%
              html_text %>% str_trim() == ''

            if (has_other_text_node) {
              node_text <-
                page %>%
                html_nodes(
                  '#ctl00_ctl00_cphMainContent_cphAdvFormContent_SchedDMisc_ctl00_trIAPDHeader + tr span'
                ) %>%
                html_text

              node_text <-
                node_text %>%
                str_split('\n|\r') %>%
                flatten_chr %>%
                str_to_upper() %>%
                str_trim

              node_text <-
                node_text[!node_text == '']

              other_node_text_df <-
                data_frame(
                  nameEntityManager = name_entity_manager,
                  item = 'descriptionOtherDisclosures',
                  value = node_text
                ) %>%
                mutate(countItem = 1:n()) %>%
                spread(item, value)

              if (return_wide) {
                other_node_text_df <-
                  other_node_text_df %>%
                  widen_adv_data()
              }
            } else {
              other_node_text_df <-
                data_frame(nameEntityManager = name_entity_manager)
            }
            return(other_node_text_df)
          }


        parse_other_office_locations_safe <-
          possibly(parse_other_office_locations, NULL)

        parse_record_locations_safe <-
          possibly(parse_record_locations, NULL)

        other_office_location_df <-
          page %>%
          parse_other_office_locations_safe(return_wide = return_wide)

        record_location_df <-
          page %>%
          parse_record_locations_safe(all_table_node_df = all_table_node_df,
                                      return_wide = return_wide)
        parse_related_advisor_safe <-
          possibly(parse_related_advisor, NULL)

        related_adviser_df <-
          page %>%
          parse_related_advisor_safe(return_wide = F)

        parse_control_person_data_safe <-
          possibly(parse_control_person_data, NULL)

        control_person_df <-
          page %>%
          parse_control_person_data_safe(return_wide = return_wide)

        parse_for_manager_website_data_safe <-
          possibly(parse_for_manager_website_data, NULL)

        website_df <-
          page %>%
          parse_for_manager_website_data_safe()

        parse_public_control_persons_safe <-
          possibly(parse_public_control_persons, NULL)

        public_control_df <-
          page %>%
          parse_public_control_persons_safe(all_table_node_df = all_table_node_df)

        parse_other_disclosures_safe <-
          possibly(parse_other_disclosures, NULL)

        other_data_df <-
          page %>%
          parse_other_disclosures_safe(return_wide = return_wide)

        if (join_data) {
          section_data <-
            other_office_location_df %>%
            left_join(related_adviser_df) %>%
            left_join(record_location_df) %>%
            left_join(website_df) %>%
            left_join(control_person_df) %>%
            left_join(public_control_df) %>%
            left_join(other_data_df) %>%
            suppressMessages()
        } else {
          section_data <-
            data_frame(
              nameTable = c(
                'Other Office Locations',
                'Record Locations',
                'Related Advisers',
                'Website',
                'Control Persons',
                'Control Public Entities',
                'Other Disclosures'
              ),
              dataTable = list(
                other_office_location_df,
                record_location_df,
                related_adviser_df,
                website_df,
                control_person_df,
                public_control_df,
                other_data_df
              )
            ) %>%
            mutate(nameEntityManager = name_entity_manager,
                   countColumns = dataTable %>% map_dbl(ncol))
        }

        return(section_data)
      }

    section_data <-
      page %>%
      parse_schedule_d(join_data = join_data, return_wide = return_wide)

    section_data <-
      section_data %>%
      mutate(idCRD) %>%
      dplyr::select(idCRD, everything())

    if (!'nameEntityManager' %in% names(section_data)) {
      section_data <-
        section_data %>%
        mutate(nameEntityManager = name_entity_manager) %>%
        dplyr::select(idCRD, nameEntityManager, everything())
    }

    if ((!return_wide) & join_data == T) {
      section_data <-
        section_data %>%
        gather(item, value, -c(nameEntityManager, idCRD)) %>%
        mutate(
          countItem = item %>% readr::parse_number,
          countItem = if_else(countItem %>% is.na, 0, countItem),
          countItem = countItem + 1,
          item = item %>% str_replace_all('[1-9]', '')
        ) %>%
        arrange(countItem) %>%
        suppressWarnings() %>%
        suppressMessages()
    }

    section_data <-
      section_data %>%
      dplyr::select(idCRD, nameEntityManager, everything())

    return(section_data)
  }

get_manager_signatory_data <-
  function(url = 'http://www.adviserinfo.sec.gov/IAPD/content/viewform/adv/Sections/iapd_AdvSignatureSection.aspx?ORG_PK=160489&FLNG_PK=02FF0ECC00080185033F257005F016CD056C8CC0'
  ) {

    idCRD <-
      url %>%
      get_pk_url_crd()

    page <-
      url %>%
      get_html_page()

    name_entity_manager <-
      page %>%
      get_entity_manager_name()

    parse_signature_page <-
      function(page) {
        is_old <-
          (page %>%
             get_html_node_text('.PrintHistRed') %>% is.na == T) %>% as.numeric %>% sum >= 1
        if(is_old){
          nodes <-
            page %>%
            get_html_node_text('font') %>%
            unique %>%
            .[1:3]
        } else {
          nodes <-
            page %>%
            get_html_node_text('.PrintHistRed') %>%
            str_replace_all('&', 'AND') %>%
            unique
        }
        if (nodes %>% length == 3) {
          item_names <-
            c('nameEntityManagerSignatory',
              'dateADVFiling',
              'titleSignatory')

          signature_df <-
            data_frame(item = item_names,
                       value = nodes) %>%
            spread(item, value) %>%
            mutate(dateADVFiling = dateADVFiling %>% lubridate::mdy())
        }

        if (nodes %>% length == 2) {
          item_names <-
            c('nameEntityManagerSignatory',
              'dateADVFiling')

          signature_df <-
            data_frame(item = item_names,
                       value = nodes) %>%
            spread(item, value)
        }
        rm(nodes)
        rm(page)
        return(signature_df)
      }

    signature_df <-
      page %>%
      parse_signature_page() %>%
      mutate(idCRD, nameEntityManager = name_entity_manager) %>%
      dplyr::select(idCRD, nameEntityManager, everything())
    return(signature_df)
  }

get_section_drp <-
  function(url = 'http://www.adviserinfo.sec.gov/IAPD/content/viewform/adv/Sections/iapd_AdvDrpSection.aspx?ORG_PK=103863&FLNG_PK=05CBF6920008018902B1D9910035D515056C8CC0',
           return_wide = F) {
    idCRD <-
      url %>%
      get_pk_url_crd()

    page <-
      url %>%
      get_html_page()

    name_entity_manager <-
      page %>%
      get_entity_manager_name()

    clean_adv_drp_data <-
      function(data, widen_data = F) {
        has_data <-
          data %>% ncol > 1
        if (has_data) {
          data <-
            data %>%
            dplyr::select(-countItem) %>%
            mutate_at(.cols =
                        data %>% dplyr::select(matches("^date")) %>% names,
                      .funs = as.character) %>%
            gather(itemName, value, -c(nameEntityManager, typeCharges)) %>%
            unite(item, itemName, typeCharges, sep = '') %>%
            dplyr::filter(!value %>% is.na) %>%
            distinct() %>%
            group_by(item) %>%
            mutate(countItem = 1:n()) %>%
            ungroup %>%
            spread(item, value) %>%
            mutate_adv_data()

          if (widen_data) {
            data <-
              data %>%
              widen_adv_data
          }
        }
        return(data)
      }

    generate_table_name_df <-
      function(item_name = 'dateCharged',
               hit_word = 'Charged',
               offset = 1,
               is_numeric = F,
               is_date = T,
               is_employment_firm = F,
               replace_words = NA,
               filter_words = NA) {
        name_df <-
          data_frame(
            nameItem = item_name,
            hit_word,
            offset,
            is_numeric,
            is_date,
            is_employment_firm,
            replace_words,
            filter_words
          )
        return(name_df)
      }

    parse_drp_table_data <-
      function(all_table_node_df,
               item_name = 'descriptionPersonEntityViolator',
               hit_word = 'Other Product Types:',
               offset = 1,
               is_numeric = F,
               is_date = F,
               is_employment_firm = F,
               replace_words = NA,
               filter_words = '^7.') {

        row_locs <-
          all_table_node_df %>%
          dplyr::filter(nodeText %>% str_detect(hit_word)) %>%
          .$idRow + offset

        values <-
          all_table_node_df %>% slice(row_locs) %>% .$nodeText
        if (!filter_words %>% is.na) {
          has_values <-
            values[!values %>% str_detect(filter_words)] %>% length > 0
        } else {
          has_values <-
            values %>% length > 0
        }

        if (has_values) {
          if (!filter_words %>% is.na) {
            values <-
              values[!values %>% str_detect(filter_words)]
          }
        } else {
          values <-
            NA
        }

        if (is_date) {
          values <-
            1:length(values) %>%
            map(function(x) {
              date_values <-
                values[[x]] %>%
                str_split('\\: ') %>%
                flatten_chr()
              val_length <-
                date_values %>% length
              if (val_length == 1) {
                date_value <-
                  date_values[[1]]
              }
              if (val_length == 2) {
                date_value <-
                  date_values %>%
                  .[[2]]
              }
              if (!val_length %in% c(1, 2)) {
                date_value <-
                  NA
              }
              return(date_value)
            }) %>%
            flatten_chr %>%
            lubridate::mdy() %>%
            as.character() %>%
            suppressWarnings()

        }

        if (is_employment_firm) {
          values <-
            1:length(values) %>%
            map_chr(function(x) {
              emp_length <-
                values[x] %>%
                str_split('\\:') %>%
                flatten_chr
              if (emp_length %>% length == 1) {
                emp_val <-
                  emp_length %>%
                  .[[1]]
              }
              if (emp_length %>% length == 2) {
                emp_val <-
                  emp_length %>%
                  .[[2]]
              }
              if (!emp_length %>% length %in% c(1, 2)) {
                emp_val <-
                  NA
              }
              return(emp_val)
            })

        }

        if (!replace_words %>% is.na) {
          values <-
            values %>%
            str_replace_all(replace_words, '')
        }
        if (is_numeric) {
          values <-
            values %>%
            str_replace_all('\\$', '') %>%
            readr::parse_number() %>%
            suppressWarnings()
        }
        values <-
          values[!values == '']
        values <-
          values %>% gsub('N/A', NA, .)
        values <-
          values[!values %>% is.na]
        has_values <-
          values %>% length > 0
        if (has_values) {
          table_df  <-
            data_frame(value = values) %>%
            dplyr::filter(!value %>% is.na) %>%
            mutate(countItem = 1:n(),
                   nameItem = item_name) %>%
            dplyr::select(countItem, nameItem, value)

          if (!filter_words %>% is.na) {
            table_df <-
              table_df %>%
              dplyr::filter(!value %>% str_detect(filter_words))
          }

          table_df <-
            table_df %>%
            group_by(nameItem, value) %>%
            mutate(
              lagValue = countItem - dplyr::lag(countItem),
              lagValue = ifelse(lagValue %>% is.na, 0, lagValue)
            ) %>%
            ungroup %>%
            dplyr::filter(!lagValue == 1) %>%
            mutate(countItem = 1:n()) %>%
            dplyr::select(-lagValue)

        } else {
          table_df <-
            data_frame(nameItem = item_name)
        }
        return(table_df)
      }

    parse_drp_table_data_safe <-
      possibly(parse_drp_table_data, NULL)

    parse_criminal_data <-
      function(page) {
        has_no_filings <-
          page %>%
          html_nodes('#aspnetForm a[name*="Criminal"] + table') %>%
          html_text %>% str_trim %>% nchar < 130

        if (!has_no_filings) {
          table_nodes <-
            page %>%
            html_nodes('#aspnetForm a[name*="Criminal"] + .flatBorderTable') %>%
            html_text

          all_table_node_df <-
            1:length(table_nodes) %>%
            map_df(function(x) {
              raw_nodes <-
                table_nodes[[x]] %>%
                str_replace_all('\r|\n|\t', '') %>%
                stri_replace_all_charclass("\\p{WHITE_SPACE}", " ") %>%
                stri_trim_both() %>%
                gsub("^\\s+|\\s+$", "", .) %>%
                str_split('\\  ') %>%
                flatten_chr %>%
                str_trim

              raw_nodes <-
                raw_nodes[!raw_nodes == '']

              if (raw_nodes %>% length > 0) {
                data_frame(idTable = x, nodeText = raw_nodes)
              }
            }) %>%
            dplyr::select(nodeText) %>%
            mutate(idRow = 1:n())

          drp_name_df <-
            generate_table_name_df(
              item_name = 'idCRDViolator',
              hit_word = 'CRD Number:',
              offset = 1,
              is_numeric = T,
              is_date = F,
              is_employment_firm = F,
              replace_words = NA,
              filter_words = NA
            ) %>%
            bind_rows(
              list(
                generate_table_name_df(
                  item_name = 'namePersonEntityViolator',
                  hit_word = 'Name:',
                  offset = 1,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'descriptionPersonEntityViolator',
                  hit_word = 'relatedbusiness and your o',
                  offset = 2,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'descriptionLocationCharges',
                  hit_word = 'Formal Charge(s) were brought in:',
                  offset = 1,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'dateCharged',
                  hit_word = 'Charged',
                  offset = 1,
                  is_numeric = F,
                  is_date = T,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'dateEventStatus',
                  hit_word = 'Event Status Date',
                  offset = 1,
                  is_numeric = F,
                  is_date = T,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'descriptionDispositionDisclosure',
                  hit_word = 'Disposition Disclosure',
                  offset = 2,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'descriptionViolationCircumstances',
                  hit_word = 'Disposition Disclosure',
                  offset = 2,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = NA
                )

              )
            )

          all_drp_data <-
            1:nrow(drp_name_df) %>%
            map_df(function(x) {
              parse_drp_table_data_safe(
                all_table_node_df = all_table_node_df,
                item_name = drp_name_df$nameItem[x],
                hit_word = drp_name_df$hit_word[x],
                replace_words = drp_name_df$replace_words[x],
                filter_words = drp_name_df$filter_words[x],
                offset = drp_name_df$offset[x],
                is_numeric = drp_name_df$is_numeric[x],
                is_date = drp_name_df$is_date[x],
                is_employment_firm =  drp_name_df$is_employment_firm[x]
              )
            }) %>%
            mutate(nameEntityManager = name_entity_manager) %>%
            dplyr::select(nameEntityManager, everything()) %>%
            distinct() %>%
            arrange(countItem) %>%
            dplyr::filter(!countItem %>% is.na) %>%
            spread(nameItem, value) %>%
            mutate_adv_data() %>%
            dplyr::select(c(
              nameEntityManager,
              countItem,
              matches('namePerson'),
              everything()
            )) %>%
            mutate(typeCharges = 'Criminal') %>%
            suppressWarnings()
        } else{
          all_drp_data <-
            data_frame(nameEntityManager = name_entity_manager)
        }
        return(all_drp_data)
      }

    parse_regulary_action_data <-
      function(page) {
        has_no_filings <-
          page %>%
          html_nodes('#aspnetForm a[name*="Regulatory"] + table') %>%
          html_text %>% str_trim %>% nchar < 200

        if (!has_no_filings) {
          table_nodes <-
            page %>%
            html_nodes('#aspnetForm a[name*="Regulatory"] + .flatBorderTable') %>%
            html_text

          all_table_node_df <-
            1:length(table_nodes) %>%
            map_df(function(x) {
              raw_nodes <-
                table_nodes[[x]] %>%
                str_replace_all('\r|\n|\t', '') %>%
                stri_replace_all_charclass("\\p{WHITE_SPACE}", " ") %>%
                stri_trim_both() %>%
                gsub("^\\s+|\\s+$", "", .) %>%
                str_split('\\  ') %>%
                flatten_chr %>%
                str_trim

              raw_nodes <-
                raw_nodes[!raw_nodes == '']

              if (raw_nodes %>% length > 0) {
                data_frame(idTable = x, nodeText = raw_nodes)
              }
            }) %>%
            dplyr::select(nodeText) %>%
            mutate(idRow = 1:n())

          drp_name_df <-
            generate_table_name_df(
              item_name = 'idCRDViolator',
              hit_word = 'CRD Number:',
              offset = 1,
              is_numeric = T,
              is_date = F,
              is_employment_firm = F,
              replace_words = NA,
              filter_words = NA
            ) %>%
            bind_rows(
              list(
                generate_table_name_df(
                  item_name = 'namePersonEntityViolator',
                  hit_word = 'Name:',
                  offset = 1,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'entityEnforcmentAgencyViolation',
                  hit_word = 'foreign financial regulatory authority,',
                  offset = 3,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'sanctionPrincpalViolation',
                  hit_word = 'Principal Sanction:',
                  offset = 0,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = 'Principal Sanction:',
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'descriptionSanctions',
                  hit_word = 'Other Sanctions:',
                  offset = 1,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = c('^3.')
                ),
                generate_table_name_df(
                  item_name = 'dateInitiated',
                  hit_word = 'Date Initiated',
                  offset = 0,
                  is_numeric = F,
                  is_date = T,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'idDocket',
                  hit_word = 'Docket/Case Number',
                  offset = 0,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = 'Docket/Case Number:',
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'nameEmployingFirmViolationTime',
                  hit_word = 'Employing Firm when activity ',
                  offset = 0,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = T,
                  replace_words = NA,
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'typeProductViolation',
                  hit_word = 'Principal Product Type:',
                  offset = 0,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = 'Principal Product Type:',
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'descriptionProductViolation',
                  hit_word = 'Other Product Types:',
                  offset = 1,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words =  c('^7.')
                ),
                generate_table_name_df(
                  item_name = 'descriptionAllegationViolation',
                  hit_word = 'Describe the allegations',
                  offset = 1,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = c('^8.')
                ),
                generate_table_name_df(
                  item_name = 'dateResolution',
                  hit_word = 'Resolution Date',
                  offset = 1,
                  is_numeric = F,
                  is_date = T,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'amountFineViolation',
                  hit_word = 'Monetary/Fine Amount:',
                  offset = 1,
                  is_numeric = T,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'detailOrderViolation',
                  hit_word = 'Ordered:',
                  offset = 1,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'descriptionSatisfactionConditions',
                  hit_word = 'waived:',
                  offset = 1,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'descriptionViolationCircumstances',
                  hit_word = 'Provide a brief summary of details related to the action status:',
                  offset = 2,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = NA
                )

              )
            )

          all_drp_data <-
            1:nrow(drp_name_df) %>%
            map_df(function(x) {
              parse_drp_table_data_safe(
                all_table_node_df = all_table_node_df,
                item_name = drp_name_df$nameItem[x],
                hit_word = drp_name_df$hit_word[x],
                replace_words = drp_name_df$replace_words[x],
                filter_words = drp_name_df$filter_words[x],
                offset = drp_name_df$offset[x],
                is_numeric = drp_name_df$is_numeric[x],
                is_date = drp_name_df$is_date[x],
                is_employment_firm =  drp_name_df$is_employment_firm[x]
              )
            }) %>%
            mutate(nameEntityManager = name_entity_manager) %>%
            dplyr::select(nameEntityManager, everything()) %>%
            distinct() %>%
            dplyr::filter(!countItem %>% is.na) %>%
            arrange(countItem) %>%
            spread(nameItem, value) %>%
            mutate_adv_data() %>%
            dplyr::select(c(
              nameEntityManager,
              countItem,
              everything()
            )) %>%
            mutate(typeCharges = 'Regulatory') %>%
            suppressWarnings()
        } else{
          all_drp_data <-
            data_frame(nameEntityManager = name_entity_manager)
        }
        return(all_drp_data)
      }

    parse_civil_action_data <-
      function(page) {
        has_no_filings <-
          page %>%
          html_nodes('#aspnetForm a[name*="Civil"] + table') %>%
          html_text %>% str_trim %>% nchar < 200

        if (!has_no_filings) {
          table_nodes <-
            page %>%
            html_nodes('#aspnetForm a[name*="Civil"] + .flatBorderTable') %>%
            html_text


          all_table_node_df <-
            1:length(table_nodes) %>%
            map_df(function(x) {
              raw_nodes <-
                table_nodes[[x]] %>%
                str_replace_all('\r|\n|\t', '') %>%
                stri_replace_all_charclass("\\p{WHITE_SPACE}", " ") %>%
                stri_trim_both() %>%
                gsub("^\\s+|\\s+$", "", .) %>%
                str_split('\\  ') %>%
                flatten_chr %>%
                str_trim

              raw_nodes <-
                raw_nodes[!raw_nodes == '']

              if (raw_nodes %>% length > 0) {
                data_frame(idTable = x, nodeText = raw_nodes)
              }
            }) %>%
            dplyr::select(nodeText) %>%
            mutate(idRow = 1:n())

          drp_name_df <-
            generate_table_name_df(
              item_name = 'idCRDViolator',
              hit_word = 'CRD Number:',
              offset = 1,
              is_numeric = T,
              is_date = F,
              is_employment_firm = F,
              replace_words = NA,
              filter_words = NA
            ) %>%
            bind_rows(
              list(
                generate_table_name_df(
                  item_name = 'namePersonEntityViolator',
                  hit_word = 'Name:',
                  offset = 1,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'entityEnforcmentAgencyViolation',
                  hit_word = 'foreign financial regulatory authority,',
                  offset = 3,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'sanctionPrincpalViolation',
                  hit_word = 'Principal Sanction:',
                  offset = 0,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = 'Principal Sanction:',
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'descriptionSanctions',
                  hit_word = 'Other Sanctions:',
                  offset = 1,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = c('^3.')
                ),
                generate_table_name_df(
                  item_name = 'dateInitiated',
                  hit_word = 'Date Initiated',
                  offset = 0,
                  is_numeric = F,
                  is_date = T,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'idDocket',
                  hit_word = 'Docket/Case Number',
                  offset = 0,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = 'Docket/Case Number:',
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'nameEmployingFirmViolationTime',
                  hit_word = 'Employing Firm when activity ',
                  offset = 0,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = T,
                  replace_words = NA,
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'typeProductViolation',
                  hit_word = 'Principal Product Type',
                  offset = 0,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = 'Principal Product Type:',
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'descriptionProductViolation',
                  hit_word = 'Other Product Types',
                  offset = 1,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words =  c('^7.')
                ),
                generate_table_name_df(
                  item_name = 'descriptionAllegationViolation',
                  hit_word = 'Describe the allegations',
                  offset = 1,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = c('\\^8.')
                ),
                generate_table_name_df(
                  item_name = 'dateResolution',
                  hit_word = 'Resolution Date',
                  offset = 1,
                  is_numeric = F,
                  is_date = T,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'amountFineViolation',
                  hit_word = 'Monetary/Fine Amount:',
                  offset = 1,
                  is_numeric = T,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'detailOrderViolation',
                  hit_word = 'Ordered:',
                  offset = 1,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'descriptionSatisfactionConditions',
                  hit_word = 'waived:',
                  offset = 1,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = NA
                ),
                generate_table_name_df(
                  item_name = 'descriptionViolationCircumstances',
                  hit_word = 'Provide a brief summary of details related to the action status:',
                  offset = 2,
                  is_numeric = F,
                  is_date = F,
                  is_employment_firm = F,
                  replace_words = NA,
                  filter_words = NA
                )

              )
            )

          all_drp_data <-
            1:nrow(drp_name_df) %>%
            map_df(function(x) {
              parse_drp_table_data_safe(
                all_table_node_df = all_table_node_df,
                item_name = drp_name_df$nameItem[x],
                hit_word = drp_name_df$hit_word[x],
                replace_words = drp_name_df$replace_words[x],
                filter_words = drp_name_df$filter_words[x],
                offset = drp_name_df$offset[x],
                is_numeric = drp_name_df$is_numeric[x],
                is_date = drp_name_df$is_date[x],
                is_employment_firm =  drp_name_df$is_employment_firm[x]
              )
            }) %>%
            mutate(nameEntityManager = name_entity_manager) %>%
            dplyr::select(nameEntityManager, everything()) %>%
            distinct()

          all_drp_data <-
            all_drp_data %>%
            dplyr::filter(!countItem %>% is.na) %>%
            arrange(countItem) %>%
            spread(nameItem, value) %>%
            mutate_adv_data() %>%
            dplyr::select(c(
              nameEntityManager,
              countItem,
              everything()
            )) %>%
            mutate(typeCharges = 'Civil') %>%
            suppressWarnings()
        } else{
          all_drp_data <-
            data_frame(nameEntityManager = name_entity_manager)
        }
        return(all_drp_data)
      }

    criminal_df <-
      page %>%
      parse_criminal_data() %>%
      clean_adv_drp_data(widen_data = return_wide)

    regulatory_df <-
      page %>%
      parse_regulary_action_data() %>%
      clean_adv_drp_data(widen_data = return_wide)

    civil_df <-
      page %>%
      parse_civil_action_data() %>%
      clean_adv_drp_data(widen_data = return_wide)

    if (return_wide) {
      all_drp_data <-
        criminal_df %>% dplyr::select(-matches("^countItem")) %>%
        left_join(regulatory_df %>%
                    dplyr::select(-matches("^countItem"))) %>%
        left_join(civil_df %>%
                    dplyr::select(-matches("^countItem"))) %>%
        suppressMessages()
      if ('nameEntityManager' %in% names(all_drp_data )) {
        all_drp_data <-
          all_drp_data %>%
          mutate(nameEntityManager = name_entity_manager)
      }
      all_drp_data <-
        all_drp_data %>%
        distinct() %>%
        mutate(idCRD) %>%
        dplyr::select(idCRD, nameEntityManager, everything())
    } else {
      all_drp_data <-
        data_frame(
          nameTable = c('Criminal DRP', 'Regulatory CRD', 'Civil CRD'),
          dataTable = list(criminal_df, regulatory_df, civil_df)
        ) %>%
        mutate(idCRD)

      if (!'nameEntityManager' %in% names(all_drp_data )) {
        all_drp_data <-
          all_drp_data %>%
          mutate(nameEntityManager = name_entity_manager)
      }
      all_drp_data <-
        all_drp_data %>%
        dplyr::select(idCRD, nameEntityManager, everything())

      all_drp_data <-
        all_drp_data %>%
        mutate(countColumns = dataTable %>% map_dbl(ncol)) %>%
        dplyr::select(-countColumns)
    }
    return(all_drp_data)

  }

get_crd_sections_data <-
  function(id_crd = 124529,
           all_sections = T,
           section_names = c(
             "Registration",
             "Identifying Information",
             "Organization",
             "Successions",
             "Private Fund Reporting",
             "Direct Manager Owners",
             "Indirect Manager Owners",
             "Other Manager Information",
             "Manager Signatories"
           ),
           flatten_tables = T) {

    sitemap_df <-
      get_managers_adv_sitemap_adv(idCRDs = id_crd) %>%
      distinct() %>%
      dplyr::filter(!idSection %>% str_detect('section12SmallBusiness')) %>%
      suppressWarnings() %>%
      suppressMessages()

    section_null <-
      section_names %>% is_null

    if (all_sections) {
      section_names <-
        sitemap_df$nameSectionActual
    }

    if (section_null &
        all_sections == F) {
      stop("You must select a section, possibilties for this search are:\n" %>%
             paste0(paste0(
               sitemap_df$nameSectionActual, collapse = '\n'
             )))
    }

    get_adv_sections <-
      function(sitemap_df, all_sections) {
        get_manager_sec_page_safe <-
          possibly(get_manager_sec_page, NULL)
        get_section_drp_safe <-
          possibly(get_section_drp, NULL)
        get_section_1_data_safe <-
          possibly(get_section_1_data, NULL)
        get_section_2_data_safe <-
          possibly(get_section_2_data, NULL)
        get_section_3_data_safe <-
          possibly(get_section_3_data, NULL)
        get_section_4_data_safe <-
          possibly(get_section_4_data, NULL)
        get_section_5_data_safe <-
          possibly(get_section_5_data, NULL)
        get_section_6_data_safe <-
          possibly(get_section_6_data, NULL)
        get_section_7a_data_safe <-
          possibly(get_section_7a_data, NULL)
        get_section_7b_data_safe <-
          possibly(get_section_7b_data, NULL)
        get_section_8_data_safe <-
          possibly(get_section_8_data, NULL)
        get_section_9_data_safe <-
          possibly(get_section_9_data, NULL )
        get_section_10_data_safe <-
          possibly(get_section_10_data, NULL)
        get_section_11_data_safe <-
          possibly(get_section_11_data, NULL)
        get_section_12_data_safe <-
          possibly(get_section_12_data, NULL)
        get_schedule_a_data_safe <-
          possibly(get_schedule_a_data, NULL)
        get_schedule_b_data_safe <-
          possibly(get_schedule_b_data, NULL)
        get_schedule_d_data_safe <-
          possibly(get_schedule_d_data, NULL)
        get_manager_signatory_data_safe <-
          possibly(get_manager_signatory_data, NULL )

        if (!all_sections) {
          no_rows <-
            sitemap_df %>%
            dplyr::filter(nameSectionActual %in% section_names) %>% nrow == 0

          if (no_rows) {
            stop(
              "Sorry tables can only be:\n",
              sitemap_df$nameSectionActual %>% paste0(collapse = '\n')
            )
          }

          sitemap_df <-
            sitemap_df %>%
            dplyr::filter(nameSectionActual %in% section_names)
        }

        all_data <-
          1:nrow(sitemap_df) %>%
          map_df(function(x) {
            f <-
              sitemap_df$nameFunction[[x]] %>% lazyeval::as_name() %>% eval
            url <-
              sitemap_df$urlADVSection[[x]]
            df_name <-
              sitemap_df$nameData[[x]]
            nameADVPage <-
              sitemap_df$nameSectionActual[[x]]
            paste0('idCRD: ', id_crd, ' - ', nameADVPage) %>% message
            g <-
              f %>%
              list(quote(url)) %>%
              as.call()

            assign(x = df_name, eval(g)) %>%
              suppressWarnings()
            data <-
              data_frame(nameADVPage = nameADVPage,
                         dataTable = list(df_name %>% as_name() %>% eval))
            return(data)
          })
        return(all_data)
      }

    get_adv_sections_safe <-
      possibly(get_adv_sections, NULL)

    all_data <-
      get_adv_sections_safe(sitemap_df = sitemap_df, all_sections = all_sections) %>%
      suppressWarnings()

    all_data <-
      all_data %>% mutate(isNULL = dataTable %>% map_lgl(is_null)) %>%
      dplyr::filter(isNULL == F) %>%
      dplyr::select(-isNULL) %>%
      mutate(countColumns = dataTable %>% map_dbl(ncol),
             countRows = dataTable %>% map_dbl(nrow)) %>%
      suppressWarnings %>%
      suppressMessages()

    name_entity_manager <-
      all_data %>%
      unnest %>%
      .$nameEntityManager %>%
      unique %>%
      .[[1]] %>%
      suppressWarnings()

    name_entity_manager <-
      name_entity_manager[!name_entity_manager %>% is.na]

    not_wide_tables <-
      c('Indirect Manager Owners')

    all_data <-
      all_data %>%
      mutate(
        idCRD = id_crd,
        nameEntityManager = name_entity_manager
      ) %>%
      dplyr::select(idCRD, nameEntityManager, nameADVPage, everything()) %>%
      dplyr::filter(countColumns > 2) %>%
      dplyr::filter(countRows > 0) %>%
      mutate(isDataWide = if_else(countRows == 1, T, F)) %>%
      mutate(isDataWide = if_else(nameADVPage %in% not_wide_tables, F, isDataWide))

    if ('Advisory Business Information' %in% all_data$nameADVPage){
      has_aum_total <-
        all_data %>%
        dplyr::filter(nameADVPage == 'Advisory Business Information') %>%
        dplyr::select(dataTable) %>%
        unnest %>%
        dplyr::select(matches("amountAUMTotal")) %>% ncol == 1

      if (has_aum_total) {
        total_aum <-
          all_data %>%
          dplyr::filter(nameADVPage == 'Advisory Business Information') %>%
          dplyr::select(dataTable) %>%
          unnest %>%
          .$amountAUMTotal %>%
          formattable::currency(digits = 0)
        "Parsed " %>%
          paste0(name_entity_manager, '\nThey have ',total_aum, ' in Total Assets Under Management') %>% message
      }
    }  else {
      "Parsed " %>%
        paste0(name_entity_manager) %>%
        message()

    }

    for (x in 1:nrow(all_data)) {
      all_data$dataTable[[x]]$nameEntityManager <-
        name_entity_manager

      if (all_data$nameADVPage[x] %in% c('Other Manager Information','DRPs')) {
        count_rows <-
          all_data$dataTable[[x]]$dataTable %>% length

        for (count in 1:(count_rows)) {
          all_data$dataTable[[x]]$dataTable[[count]]$nameEntityManager <-
            name_entity_manager
        }
      }
    }
    if (flatten_tables) {
      get_all_manager_description_data <-
        function(all_data) {
          select_nesting_vars <-
            function(data) {
              data <-
                data %>%
                dplyr::select(nameADVPage, dataTable)
              return(data)
            }
          widen_data <-
            all_data %>%
            dplyr::filter(isDataWide == T)

          widened_data <-
            1:nrow(widen_data) %>%
            map_df(function(x) {
              data <-
                widen_data %>%
                select_nesting_vars() %>%
                dplyr::select(-nameADVPage) %>%
                slice(x) %>%
                unnest %>%
                mutate_all(as.character) %>%
                gather(nameItem, value, -c(idCRD, nameEntityManager))
              return(data)
            }) %>%
            distinct %>%
            group_by(nameItem) %>%
            mutate(countItem = 1:n()) %>%
            ungroup %>%
            suppressWarnings() %>%
            dplyr::filter(!value %>% is.na) %>%
            mutate(idCRD = idCRD %>% as.numeric) %>%
            mutate(
              countItem = countItem - 1,
              countItem = countItem %>% as.character,
              countItem = ifelse(countItem == "0", '', countItem)
            ) %>%
            unite(item, nameItem, countItem, sep = '') %>%
            distinct() %>%
            suppressWarnings()

          col_order <-
            c('idCRD', 'nameEntityManager', widened_data$item)

          data <-
            widened_data %>%
            spread(item, value) %>%
            dplyr::select(one_of(col_order)) %>%
            mutate_adv_data()
          return(data)
        }

      manager_description_data <-
        all_data %>%
        get_all_manager_description_data()

      all_data <-
        data_frame(
          idCRD = id_crd,
          nameEntityManager = name_entity_manager,
          nameTable = 'Manager Description',
          dataTable = list(manager_description_data)
        ) %>%
        bind_rows(
          all_data %>% dplyr::filter(isDataWide == F) %>%
            dplyr::select(idCRD, nameEntityManager,
                          nameTable = nameADVPage,
                          dataTable)
        ) %>%
        dplyr::select(idCRD, nameEntityManager, nameTable, dataTable)
    }

    return(all_data)
  }


get_search_crd_ids <-
  function(search_names = c('EJF Capital', '137 Ventures'),
           crd_ids = NULL) {
    if (search_names %>% is_null & (crd_ids %>% is_null)) {
      stop("Please enter search names or CRD IDs")
    }

    crd_df <-
      data_frame(idCRD = NA)

    if (!search_names %>% is_null) {
      search_name_df <-
        search_names %>%
        map_df(function(x) {
          get_data_adv_managers_metadata(
            crd_ids = NULL,
            search_names = x,
            return_message = T
          )
        })

      id_crds <-
        search_name_df %>%
        .$idCRD

      crd_df <-
        crd_df %>%
        bind_rows(data_frame(idCRD = id_crds))

    }

    if (!crd_ids %>% is_null) {
      crd_df <-
        crd_df %>%
        bind_rows(data_frame(idCRD = crd_ids))
    }
    crds <-
      crd_df %>%
      dplyr::filter(!idCRD %>% is.na) %>%
      distinct() %>%
      .$idCRD

    return(crds)
  }

return_selected_adv_tables <-
  function(data,
           all_sections,
           table_names,
           gather_data) {
    table_names <-
      data$nameTable %>% unique
    return_selected_adv_table <-
      function(data,
               table_name,
               gather_data) {
        table_names <-
          c('Manager Description', data$nameTable %>% unique)

        if (table_name %>% str_count(table_names) %>% sum == 0) {
          stop("Names can only be\n" %>% paste0(paste0(table_names, collapse = '\n')))
        }

        data_selected <-
          data %>%
          dplyr::filter(nameTable %>% str_detect(table_name)) %>%
          dplyr::select(idCRD, nameTable, dataTable) %>%
          mutate(idRow = 1:n())

        crd_df <-
          data_selected %>%
          dplyr::select(idRow, idCRD)

        has_nested_list <-
          data_selected %>%
          dplyr::select(dataTable) %>%
          unnest %>%
          map_df(class) %>%
          gather(column, valueCol) %>%
          .$valueCol %>% str_count('list') %>% sum() >= 1

        if (!has_nested_list) {
          data_selected <-
            data_selected %>%
            mutate(countColumns = dataTable %>% map_dbl(ncol)) %>%
            dplyr::filter(countColumns > 1) %>%
            dplyr::select(-countColumns) %>%
            dplyr::filter(nameTable == table_name) %>%
            dplyr::select(dataTable) %>%
            unnest

          data_selected <-
            data_selected %>%
            mutate_at(.cols = data_selected %>% dplyr::select(matches("^amount[A-Z]")) %>% names,
                      funs(. %>% currency(digits = 0))) %>%
            mutate_at(.cols = data_selected %>% dplyr::select(matches("^pct[A-Z]")) %>% names,
                      funs(. %>% percent(digits = 2)))

          section_df <-
            get_sec_sitemap_df() %>%
            dplyr::select(nameSectionActual, idSection) %>%
            bind_rows(
              data_frame(
                nameSectionActual = 'Manager Description',
                idSection = 'managerDescription'
              )
            )
          df_name <-
            data_frame(nameSectionActual = table_name) %>%
            left_join(section_df) %>%
            suppressMessages() %>%
            .$idSection

          if (gather_data) {
            data_selected <-
              data_selected %>%
              dplyr::select(-matches('^count[I]|^numberFund')) %>%
              mutate_at(
                .cols =
                  data_selected %>% dplyr::select(-idCRD) %>% dplyr::select(-matches('^count[I]|^numberFund')) %>% names,
                .funs = as.character
              ) %>%
              gather(nameItem, valueItem, -c(idCRD, nameEntityManager)) %>%
              arrange(idCRD) %>%
              mutate(nameItem = nameItem %>% str_replace_all('[0-9]', '')) %>%
              mutate(
                nameTable = table_names[x],
                nameItem = nameItem %>% str_replace_all('[0-9]', '')
              ) %>%
              group_by(idCRD, nameEntityManager, nameItem) %>%
              mutate(countItemManager = 1:n()) %>%
              ungroup %>%
              dplyr::select(nameTable,
                            idCRD:nameItem,
                            countItemManager,
                            valueItem) %>%
              suppressWarnings %>%
              arrange(idCRD, nameItem, countItemManager)
          }

          assign(x = df_name, eval(data_selected), env = .GlobalEnv)
          section_df %>%
            dplyr::filter(nameSectionActual == table_name) %>%
            .$idSection %>% message
        }
        if (has_nested_list) {
          has_no_count_col <-
            data_selected %>%
            unnest %>% names %>% str_count('countColumns') %>% sum == 0
          if (has_no_count_col) {
            count_values <-
              data_selected %>%
              .$dataTable %>% map_dbl(ncol)

            data_selected <-
              data_selected %>%
              mutate(countColumns = count_values)
          }
          data_selected <-
            data_selected %>%
            dplyr::select(idRow, dataTable) %>%
            unnest %>%
            dplyr::select(idRow, nameTable, dataTable) %>%
            mutate(countColumn = map_dbl(dataTable, ncol)) %>%
            dplyr::filter(countColumn > 1) %>%
            dplyr::select(-countColumn)


          if (data_selected %>% nrow >= 1) {
            table_names <-
              data_selected$nameTable %>%
              str_replace_all(' ', '') %>%
              paste0('manager', .) %>%
              unique

            table_name_df <-
              data_frame(idTable = table_names,
                         nameTable = data_selected$nameTable %>% unique)

            data_selected <-
              data_selected %>%
              dplyr::select(idRow, dataTable, nameTable) %>%
              left_join(crd_df) %>%
              left_join(table_name_df) %>%
              suppressMessages() %>%
              dplyr::select(idCRD, idTable, nameTable, dataTable)

            1:length(table_names) %>%
              map(function(x) {
                table_names[x] %>% message
                table_data <-
                  data_selected %>%
                  dplyr::filter(idTable == table_names[x]) %>%
                  dplyr::select(idCRD, dataTable) %>%
                  unnest

                table_data <-
                  table_data %>%
                  mutate_at(.cols = table_data %>% dplyr::select(matches("^amount[A-Z]")) %>% names,
                            funs(. %>% currency(digits = 0))) %>%
                  mutate_at(.cols = table_data %>% dplyr::select(matches("^pct[A-Z]")) %>% names,
                            funs(. %>% percent(digits = 0)))

                if (gather_data) {
                  table_data <-
                    table_data %>%
                    dplyr::select(-matches('^count[I]|^numberFund')) %>%
                    mutate_at(
                      .cols =
                        table_data %>% dplyr::select(-idCRD) %>% dplyr::select(-matches(
                          '^count[I]|^numberFund'
                        )) %>% names,
                      .funs = as.character
                    ) %>%
                    gather(nameItem,
                           valueItem,
                           -c(idCRD, nameEntityManager)) %>%
                    arrange(idCRD) %>%
                    dplyr::filter(!valueItem %>% is.na) %>%
                    mutate(
                      nameTable = table_names[x],
                      nameItem = nameItem %>% str_replace_all('[0-9]', '')
                    ) %>%
                    group_by(idCRD, nameEntityManager, nameItem) %>%
                    mutate(countItemManager = 1:n()) %>%
                    ungroup %>%
                    dplyr::select(nameTable,
                                  idCRD:nameItem,
                                  countItemManager,
                                  valueItem) %>%
                    suppressWarnings %>%
                    arrange(idCRD, nameItem, countItemManager)
                }

                df_name <-
                  table_names[x]
                assign(x = df_name, eval(table_data), env = .GlobalEnv)
              })
          }
        }
        invisible()
      }

    return_selected_adv_table_safe <-
      possibly(return_selected_adv_table, NULL)
    table_names %>%
      map(function(x) {
        return_selected_adv_table_safe(data = data,
                                       table_name = x,
                                       gather_data = gather_data)
      })
    invisible()
  }

#' Get detailed ADV form data for specified search name or CRD id
#'
#' @param search_names Names of the entities you want to search
#' @param crd_ids CRD ids you want to search
#' @param all_sections Do you want to search all ADV Sections
#' @param section_names If not all sections, which sections
#' @param flatten_tables Do you want the data the data with singular values flattened into a single data frame
#' @param gather_data Do you want the data in gathered form
#' @param assign_to_enviornment Do you want to save the invidual data frames to your global environment
#'
#' @return
#' @export
#' @import dplyr formattable httr purrr readr rvest stringi stringr tibble tidyr curlconverter lubridate
#' @importFrom lazyeval as_name
#' @importFrom curl curl_download
#' @importFrom magrittr %>%
#' @importFrom lubridate mdy
#' @importFrom lubridate ymd
#' @examples
#' get_data_adv_managers_filings(search_names = c('Blackstone Real Estate'), crd_ids = NULL, all_sections = T,  section_names = NULL, flatten_tables = T, gather_data = F, assign_to_enviornment = T)
get_data_adv_managers_filings <-
  function(search_names = NULL,
           crd_ids = NULL,
           all_sections = T,
           section_names = c(
             "Registration",
             "Identifying Information",
             "Organization",
             "Successions",
             "Private Fund Reporting",
             "Direct Manager Owners",
             "Indirect Manager Owners",
             "Other Manager Information",
             "Manager Signatories"
           ),
           flatten_tables = T,
           gather_data = F,
           assign_to_enviornment = T) {
    packages <-
      c(
        'tidyverse',
        "curl",
        "dplyr",
        "formattable",
        "httr",
        "lubridate",
        "magrittr",
        "purrr",
        "readr",
        'lazyeval',
        "rvest",
        "stringi",
        "stringr",
        "tibble",
        "tidyr",
        'curlconverter'
      )
    suppressMessages(lapply(packages, library, character.only = T))

    nothing_entered <-
      (crd_ids %>% is_null()) & (search_names %>% is_null())
    if (nothing_entered) {
      stop("Please enter a CRD ID or a search name")
    }

    get_search_crd_ids_safe <-
      possibly(get_search_crd_ids, NULL)

    crds <-
      get_search_crd_ids_safe(search_names = search_names, crd_ids = crd_ids)

    get_crd_sections_data_safe <-
      possibly(get_crd_sections_data, NULL)

    all_data <-
      crds %>%
      map_df(function(x) {
        get_crd_sections_data_safe(
          id_crd = x,
          all_sections = all_sections,
          section_names = section_names,
          flatten_tables = flatten_tables
        )
      })
    return_selected_adv_tables_safe <-
      possibly(return_selected_adv_tables, NULL)

    if (assign_to_enviornment) {
      all_data %>%
        return_selected_adv_tables(all_sections = all_sections,
                                   gather_data = gather_data)

    }
    return(all_data)

  }


# pdf ---------------------------------------------------------------------

parse_manager_brochure_data <-
  function(url = 'https://adviserinfo.sec.gov/IAPD/IAPDFirmSummary.aspx?ORG_PK=156663') {
    idCRD <-
      url %>%
      get_pk_url_crd()

    page <-
      url %>%
      get_html_page()

    name_entity_manager <-
      page %>%
      get_entity_manager_name()

    brochure_exists <-
      page %>%
      check_html_node(node_css = '#ctl00_cphMain_landing_p2BrochureLink')

    parse_sec_manager_pdf_url <-
      function(page) {
        brochure_url <-
          page %>%
          html_nodes('a[href*="Part2Brochures.aspx"]') %>%
          html_attr('href') %>%
          unique() %>%
          paste0('https://www.adviserinfo.sec.gov',.)

        brochure_page <-
          brochure_url %>%
          get_html_page()

        has_brochure <-
          brochure_page %>%
          html_nodes('.main td a') %>%
          html_attr('href') %>% length > 0
        if (has_brochure) {
          url_brochure_pdf <-
            brochure_page %>%
            html_nodes('.main td a') %>%
            html_attr('href') %>%
            paste0('https://www.adviserinfo.sec.gov',.)
        } else {
          url_brochure_pdf <-
            NA
        }
        return(url_brochure_pdf)
      }
    pdf_urls <-
      page %>% parse_sec_manager_pdf_url()
    no_brochure <-
      pdf_urls %>% is.na() %>% as.numeric %>% sum >= 1

    if (no_brochure) {
      brochure_exists <-
        F
    }
    parse_pdf_brochure <-
      function(url = 'https://adviserinfo.sec.gov/IAPD/Content/Common/crd_iapd_Brochure.aspx?BRCHR_VRSN_ID=379240') {
        info <-
          url %>%
          pdf_info()

        info$created <-
          info$created %>% as.character

        info$modified <-
          info$modified %>% as.character

        info <-
          info %>%
          flatten_df

        sec_name_df <-
          data_frame(
            nameSEC = c(
              "version",
              "pages",
              "encrypted",
              "linearized",
              "Author",
              "Creator",
              "Producer",
              "created",
              "modified",
              "metadata",
              "locked",
              "attachments",
              "layout",
              'Title'
            ),
            nameActual = c(
              "idVersion",
              "countPages",
              "isEncrypted",
              "isLinearized",
              "nameAuthor",
              "nameCreator",
              "nameProducer",
              "datetimeCreated",
              "datetimeModified",
              "detailsMetadata",
              "isLocked",
              "hasAttachments",
              "detaillayout",
              'titleDocument'
            )
          )

        sec_names <-
          names(info)

        actual_name_df <-
          1:length(sec_names) %>%
          map_df(function(x){
            name_exists <-
              sec_name_df %>%
              dplyr::filter(nameSEC == sec_names[x]) %>% nrow > 0
            if (name_exists)  {
              nameActual <-
                sec_name_df %>%
                dplyr::filter(nameSEC == sec_names[x]) %>%
                .$nameActual
            } else {
              nameActual <-
                NA
            }
            data_frame(idColumn = x, nameActual)
          })

        columns_selected <-
          actual_name_df %>%
          dplyr::filter(!nameActual %>% is.na) %>%
          .$idColumn

        info <-
          info %>%
          dplyr::select(columns_selected)

        actual_names <-
          actual_name_df %>%
          dplyr::filter(!nameActual %>% is.na) %>%
          .$nameActual

        names(info) <-
          actual_names

        info <-
          info %>%
          dplyr::select(matches("countPages|idVersion|dateTime|titleDocument|nameAuthor|nameProducer")) %>%
          mutate(urlPDFManagerADVBrochure = url)

        pdf_pages <-
          url %>%
          pdf_text() %>%
          str_split('\n')

        pdf_text_df <-
          1:length(pdf_pages) %>%
          map_df(function(x) {
            page_text <-
              pdf_pages[[x]] %>%
              str_trim()

            page_text <-
              page_text %>% str_replace_all('Form ADV Part 2A: |Form ADV Part 2A Brochure|Part 2A of ADV: ', '') %>%
              str_replace_all('Firm Brochure', '') %>%
              str_trim

            page_text <-
              page_text[!page_text == '']

            remove_last_line <-
              !page_text[page_text %>% length %>% max] %>% readr::parse_number() %>% is.na %>% suppressWarnings()

            if (remove_last_line) {
              page_text <-
                page_text[1:(page_text %>% length - 1)]
            }

            clean_text <-
              function (text.var) {
                text.var <-
                  gsub("\\s+", " ", gsub("\\\\r|\\\\n|\\n|\\\\t", " ", text.var))
                return(text.var)
              }

            page_text <-
              page_text %>%
              stri_replace_all_charclass("\\p{WHITE_SPACE}", " ") %>%
              stri_trim_both() %>%
              clean_text

            page_text <-
              page_text %>% paste0(collapse = ' ') %>%
              clean_text

            page_text <-
              page_text %>%
              stringi::stri_trans_general("latin-ascii")

            page_data <-
              data_frame(numberPage = x, textPage = page_text)
            return(page_data)
          }) %>%
          dplyr::select(textPage) %>%
          summarise(textBrochure = textPage %>% paste0(collapse = '\n')) %>%
          mutate(urlPDFManagerADVBrochure = url) %>%
          right_join(info) %>%
          suppressMessages() %>%
          dplyr::select(datetimeCreated,
                        textBrochure,
                        everything())

        return(pdf_text_df)

      }

    if (brochure_exists) {
      urlPDFManagerADVBrochure <-
        page %>%
        parse_sec_manager_pdf_url()

      brochure_data <-
        urlPDFManagerADVBrochure %>%
        map_df(function(x) {
          parse_pdf_brochure(url = x)
        }) %>%
        mutate(nameEntityManager = name_entity_manager, idCRD)

      brochure_data <-
        brochure_data %>%
        mutate_at(.cols = brochure_data %>% dplyr::select(matches("datetime")) %>% names,
                  .funs = ymd_hms) %>%
        arrange(desc(datetimeCreated))
    } else {
      brochure_data <-
        data_frame(nameEntityManager = name_entity_manager, idCRD)
    }

    brochure_data <-
      brochure_data %>%
      select_start_vars()

    return(brochure_data)

  }

get_manager_brochure_data <-
  function(id_crd = 124529,
           split_pages = T) {
    url <-
      get_managers_adv_sitemap_adv(idCRDs = id_crd) %>%
      distinct() %>%
      dplyr::filter(nameSectionActual == 'Registration') %>%
      .$urlADVSection %>%
      suppressWarnings() %>%
      suppressMessages() %>%
      str_replace('http', 'https')

    pdf_data <-
      url %>%
      parse_manager_brochure_data()

    if ('textBrochure' %in% names(pdf_data)) {
      if (split_pages) {
        pdf_data <-
          pdf_data %>%
          separate_rows(textBrochure, sep = '\n')
      }
    }

    return(pdf_data)

  }

#' Get manager ADV brochure
#'
#' @param search_names Names of the companies you want to search
#' @param crd_ids CRD IDs you want to search
#' @param split_pages Do you want the brochure as 1 text blob or multiple pages
#'
#' @return
#' @export
#' @import pdftools stringr stringi dplyr purrr tidyr
#' @examples
#' get_data_adv_managers_brochures(search_names = c('137 Ventures', 'Divco'), crd_ids = NULL, split_pages = T))
get_data_adv_managers_brochures <-
  function(search_names = c('137 Ventures', 'Divco'),
           crd_ids = NULL,
           split_pages = T) {
    packages <-
      c(
        "curl",
        "dplyr",
        "formattable",
        "httr",
        "lubridate",
        "magrittr",
        "purrr",
        "readr",
        'lazyeval',
        "rvest",
        "stringi",
        "stringr",
        "tibble",
        'pdftools',
        "tidyr",
        'curlconverter'
      )
    suppressMessages(lapply(packages, library, character.only = T))

    get_search_crd_ids_safe <-
      possibly(get_search_crd_ids, NULL)

    crds <-
      get_search_crd_ids_safe(search_names = search_names, crd_ids = crd_ids)

    get_manager_brochure_data_safe <-
      possibly(get_manager_brochure_data, NULL)

    all_data <-
      crds %>%
      map_df(function(x) {
        manager_pdf <-
          get_manager_brochure_data_safe(id_crd = x,
                                         split_pages = split_pages)
        paste0('idCRD: ', x, ' - ', 'Manager Brochure') %>% message
        return(manager_pdf)
      })
    return(all_data)
  }


# Registered_Advisor_Data Downloands -------------------------------------------------

#' Get ADV Summary Filing Data URLs for All Applicable Periods
#'
#' @param return_wide
#'
#' @return
#' @export
#' @import rvest stringr dplyr tibble httr
#' @importFrom lubridate mdy
#' @examples
get_data_adv_period_urls <-
  function(return_wide = T) {
    httr::set_config(config(ssl_verifypeer = 0L))
    page <-
      'https://www.sec.gov/foia/iareports/inva-archive.htm' %>%
      GET %>%
      content() %>%
      suppressMessages()

    url_zip <-
      page %>%
      html_nodes('#main-content a:nth-child(1)') %>%
      html_attr('href') %>%
      paste0('https://www.sec.gov', .)

    date_data <-
      url_zip %>% basename() %>% str_replace_all('ia|.zip', '') %>% lubridate::mdy()

    url_exempt_zip <-
      page %>%
      html_nodes("#main-content a:nth-child(2)") %>%
      html_attr("href") %>%
      paste0('https://www.sec.gov', .)

    data_data_exempt <-
      url_exempt_zip %>% basename() %>% str_replace_all('ia|.zip|-exempt', '') %>% lubridate::mdy()

    if (return_wide) {
      url_df <-
        tibble(dateData = date_data,
               urlZip = url_zip) %>%
        left_join(tibble(dateData = data_data_exempt,
                         urlZipExempt = url_exempt_zip)) %>%
        mutate(periodData = dateData %>% format('%Y-%m')) %>%
        suppressMessages()
    } else {
      url_df <-
        tibble(dateData = date_data,
               isExempt = F,
               urlZip = url_zip) %>%
        bind_rows(tibble(
          dateData = data_data_exempt,
          isExempt = T,
          urlZip = url_exempt_zip
        )) %>%
        mutate(periodData = dateData %>% format('%Y-%m'))
    }


    return(url_df)

  }

get_sec_adv_name_df <-
  function() {
    sec_name_df <-
      data_frame(
        nameSEC = c(
          "SEC Region",
          "Organization CRD#",
          "SEC#",
          "Firm Type",
          "Primary Business Name",
          "Legal Name",
          "Main Office Street Address 1",
          "Main Office Street Address 2",
          "Main Office City",
          "Main Office State",
          "Main Office Country",
          "Main Office Postal Code",
          "Main Office Telephone Number",
          "Main Office Facsimile Number",
          "Mail Office Street Address 1",
          "Mail Office Street Address 2",
          "Mail Office City",
          "Mail Office State",
          "Mail Office Country",
          "Mail Office Postal Code",
          "SEC Current Status",
          "SEC Status Effective Date",
          "Jurisdiction Notice Filed-Effective Date",
          "Latest ADV Filing Date",
          "Form Version",
          "1I",
          "Website Address",
          "1M",
          "1N",
          "CIK#",
          "1O",
          "1P",
          "2A(1)",
          "2A(2)",
          "2A(3)",
          "2A(4)",
          "2A(5)",
          "2A(6)",
          "2A(7)",
          "2A(8)",
          "2A(9)",
          "2A(10)",
          "2A(11)",
          "2A(12)",
          "2A(13)",
          "3A",
          "3A-Other",
          "3B",
          "3C-State",
          "3C-Country",
          "5A",
          "5B(1)",
          "5B(2)",
          "5B(3)",
          "5B(4)",
          "5B(5)",
          "5B(6)",
          "5C(1)",
          "5C(1)-If more than 100, how many",
          "5C(2)",
          "5D(1)(a)",
          "5D(1)(b)",
          "5D(1)(c)",
          "5D(1)(d)",
          "5D(1)(e)",
          "5D(1)(f)",
          "5D(1)(g)",
          "5D(1)(h)",
          "5D(1)(i)",
          "5D(1)(j)",
          "5D(1)(k)",
          "5D(1)(l)",
          "5D(1)(m)",
          "5D(1)(m)-Other",
          "5D(2)(a)",
          "5D(2)(b)",
          "5D(2)(c)",
          "5D(2)(d)",
          "5D(2)(e)",
          "5D(2)(f)",
          "5D(2)(g)",
          "5D(2)(h)",
          "5D(2)(i)",
          "5D(2)(j)",
          "5D(2)(k)",
          "5D(2)(l)",
          "5D(2)(m)",
          "5D(2)(m)-Other",
          "5E(1)",
          "5E(2)",
          "5E(3)",
          "5E(4)",
          "5E(5)",
          "5E(6)",
          "5E(7)",
          "5E(7)-Other",
          "5F(1)",
          "5F(2)(a)",
          "5F(2)(b)",
          "5F(2)(c)",
          "5F(2)(d)",
          "5F(2)(e)",
          "5F(2)(f)",
          "5G(1)",
          "5G(2)",
          "5G(3)",
          "5G(4)",
          "5G(5)",
          "5G(6)",
          "5G(7)",
          "5G(8)",
          "5G(9)",
          "5G(10)",
          "5G(11)",
          "5G(12)",
          "5G(12)-Other",
          "5H",
          "5H-If more than 500, how many",
          "5I(1)",
          "5I(2)",
          "5J",
          "6A(1)",
          "6A(2)",
          "6A(3)",
          "6A(4)",
          "6A(5)",
          "6A(6)",
          "6A(7)",
          "6A(8)",
          "6A(9)",
          "6A(10)",
          "6A(11)",
          "6A(12)",
          "6A(13)",
          "6A(14)",
          "6A(14)-Other",
          "6B(1)",
          "6B(2)",
          "6B(3)",
          "7A(1)",
          "7A(2)",
          "7A(3)",
          "7A(4)",
          "7A(5)",
          "7A(6)",
          "7A(7)",
          "7A(8)",
          "7A(9)",
          "7A(10)",
          "7A(11)",
          "7A(12)",
          "7A(13)",
          "7A(14)",
          "7A(15)",
          "7A(16)",
          "7B",
          "8A(1)",
          "8A(2)",
          "8A(3)",
          "8B(1)",
          "8B(2)",
          "8B(3)",
          "8C(1)",
          "8C(2)",
          "8C(3)",
          "8C(4)",
          "8D",
          "8E",
          "8F",
          "8G(1)",
          "8G(2)",
          "8H",
          "8I",
          "9A(1)(a)",
          "9A(1)(b)",
          "9A(2)(a)",
          "9A(2)(b)",
          "9B(1)(a)",
          "9B(1)(b)",
          "9B(2)(a)",
          "9B(2)(b)",
          "9C(1)",
          "9C(2)",
          "9C(3)",
          "9C(4)",
          "9D(1)",
          "9D(2)",
          "9E",
          "9F",
          "10A",
          "11",
          "11A(1)",
          "11A(2)",
          "11B(1)",
          "11B(2)",
          "11C(1)",
          "11C(2)",
          "11C(3)",
          "11C(4)",
          "11C(5)",
          "11D(1)",
          "11D(2)",
          "11D(3)",
          "11D(4)",
          "11D(5)",
          "11E(1)",
          "11E(2)",
          "11E(3)",
          "11E(4)",
          "11F",
          "11G",
          "11H(1)(a)",
          "11H(1)(b)",
          "11H(1)(c)",
          "11H(2)",
          "SEC Region Name",
          "Organization CRD #",
          "SEC #",
          "Main Street Address 1",
          "Main Street Address 2",
          "Main Office City, State, Postal Code",
          "Mail Office City, State, Postal Code",
          "Contact Name",
          "Telephone Number",
          "Legal Status",
          "Types of Advisory Activities",
          "Regulator Status",
          "Effective Date",
          "Criminal Disclosures",
          "Regulatory Action Disclosures",
          "Civil Judicial Disclosures",
          "Bankruptcy Disclosures",
          "Judgment/Lien Disclosures",
          "Bond Payout Disclosures",
          "SEC Registration Status",
          "Status Effective Date",
          "World Wide Web Site Address",
          "1L",
          "5A-If more than 1,000, how many",
          "5B(1)-If more than 1,000, how many",
          "5B(2)-If more than 1,000, how many",
          "5B(3)-If more than 1,000, how many",
          "5C",
          "5C-If more than 500, how many",
          "5D(1)",
          "5D(2)",
          "5D(3)",
          "5D(4)",
          "5D(5)",
          "5D(6)",
          "5D(7)",
          "5D(8)",
          "5D(9)",
          "5D(10)",
          "5D(10)-Other",
          "5G(10)-Other",
          "6A(7)-Other",
          "9A(1)",
          "9A(2)",
          "9B(1)",
          "9B(2)",
          "9C",
          "10",
          'Current Status',
          'FINRA BD Status',
          "2B(1)", "2B(2)", "2B(3)"
        ),
        nameActual = c(
          "idRegionSEC",
          "idCRD",
          "idSEC",
          "typeRegulationSEC",
          "nameEntityManager",
          "nameEntityManagerLegal",
          "addressStreet1OfficePrimary",
          "addressStreet2OfficePrimary",
          "cityOfficePrimary",
          "stateOfficePrimary",
          "countryOfficePrimary",
          "zipOfficePrimary",
          "phoneOfficePrimary",
          "faxOfficePrimary",
          "addressStreet1OfficeMail",
          "addressStreet2OfficeMail",
          "cityOfficeMail",
          "stateOfficeMail",
          "countryOfficeMail",
          "zipOfficeMail",
          "statusSEC",
          "dateStatusSEC",
          "stateDateJurisdictionNotice",
          "dateADVLatest",
          "dateFormVersion",
          "hasEntityMultipleURLs",
          "urlManager",
          "isForeignRegisteredEntity",
          "isSECSection12_15Reporter",
          "idCIK",
          "hasAUMGreater1B",
          "idLEI",
          "hasAUMGreater100M",
          "hasAUMUnder100MOver25m",
          "hasPrincipalOfficeWY",
          "hasPrincipalOfficeForeign",
          "isAdviser1940InvestmentActCompany",
          "isAdviserBusinessDevelopmentCompany25MInCapital",
          "isAdviserPensionCapitalGreater200M",
          "isAdviserRelated203A",
          "isAdviserNew203A",
          "isAdviserMultiState203A",
          "isAdviserInternet",
          "hasSECOrderProhibitingRegistration",
          "isAdviserSECIneligible",
          "typeEntity",
          "typeEntityDetail",
          "monthFiscalYearEnd",
          "stateEntityOrganized",
          "countryEntityOrganized",
          "countEmployeesTotal",
          "countEmployeesInvestmentAdvisory",
          "countEmployeesBrokerDealer",
          "countEmployeesStateRegisteredInvestmentAdviser",
          "countEmployeesStateRegisteredInvestmentAdviserMultipleEntities",
          "countEmployeesLicensedInsuranceAgents",
          "countEmployeesSolicitAdvisoryClients",
          "rangeClients",
          "countClientsOver100Rounded",
          "pctClientsForeign",
          "rangeClientsIndividualNonHighNetWorth",
          "rangeClientsIndividualHighNetWorth",
          "rangeClientsBankThrift",
          "rangeClientsInvestmentCompany",
          "rangeClientsBusinessDevelopmentCompany",
          "rangeClientsPooledInvestmentVehicle",
          "rangeClientsPensionPlan",
          "rangeClientsCharitableOrganization",
          "rangeClientsCorporationOther",
          "rangeClientsStateMunicipalGovernment",
          "rangeClientsInvestmentAdviserOther",
          "rangeClientsInsuranceCompany",
          "rangeClientsOther",
          "typeClientsOther",
          "rangeAUMIndividualNonHighNetWorth",
          "rangeAUMIndividualHighNetWork",
          "rangeAUMBankThrift",
          "rangeAUMInvestmentCompany",
          "rangeAUMBusinessDevelopmentCompany",
          "rangeAUMPooledInvestmentVehicle",
          "rangeAUMPensionPlan",
          "rangeAUMCharitableOrganization",
          "rangeAUMCorporationOthr",
          "rangeAUMStateMunicipalGovernment",
          "rangeAUMInvestmentAdviserOther",
          "rangeAUMInsuranceCompany",
          "rangeAUMOther",
          "typeAUMOther",
          "hasFeeAUM",
          "hasFeeHourlyCharge",
          "hasFeeSubscription",
          "hasFeeFixed",
          "hasFeeCommission",
          "hasFeePerformance",
          "hasFeeOther",
          "typeFeeOther",
          "isManagerSecuritiesPortfolio",
          "amountAUMDiscretionary",
          "amountAUMNonDiscretionary",
          "amountAUMTotal",
          "countAccountsDiscretionary",
          "countAccountsNonDiscretionary",
          "countAccountsTotal",
          "hasFinancialPlanning",
          "hasPortfolioManagementIndividualSmallBusiness",
          "hasPortfolioManagementInvestmentCompanies",
          "hasPortfolioManagementPooledInvestmentVehicles",
          "hasPortfolioManagementInstitutionalClients",
          "hasServicePensionConsulting",
          "hasServiceInvestmentAdviserSelection",
          "hasServicePeriodicalPublication",
          "hasServiceSecurityRating",
          "hasServiceMarketTiming",
          "hasServiceEducationSeminars",
          "hasServiceOther",
          "typeServiceOther",
          "rangeClientsFinancialPlanning",
          "amountClientsFinancialPlanningOver500Rounded",
          "hasFeeWrapSponsor",
          "hasFeeWrapPortfolioManager",
          "isAdviserLimitedInvestmentTypes",
          "isBrokerDealer",
          "isBrokerDealerRepresentative",
          "isCommodityPoolOperator",
          "isFuturesMerchant",
          "isRealEstateBrokerDealerAgent",
          "isInsuranceBrokerAgent",
          "isBank",
          "isTrustCompany",
          "isRegisteredMunicipalAdviser",
          "isRegisteredSecuritySwapDealer",
          "isRegistredSecuritySwapParticipant",
          "isAccountingFirm",
          "isLawFirm",
          "isOtherFinancialProductSalesperson",
          "typeOtherFinancialProductSalesperson",
          "isBusinessActiveNonListedActivity",
          "typeBusinessActiveNonListedActivity",
          "hasProductNonInvestmentAdvice",
          "hasRelatedBrokerDealer",
          "hasRelatedInvestmentAdviserOthr",
          "hasRelatedRegisteredMunicipalAdviser",
          "hasRelatedRegisteredSecuritySwapDealer",
          "hasRelatedRegisteredSecuritySwapParticipant",
          "hasRelatedRegisteredCommodityPoolOperator",
          "hasRelatedRegisteredFuturesMerchant",
          "hasRelatedBankThrift",
          "hasRelatedTrust",
          "hasRelatedAccountingFirm",
          "hasRelatedLawFirm",
          "hasRelatedInsuranceCompany",
          "hasRelatedPensionConsultant",
          "hasRelatedRealEstateBrokerDealer",
          "hasRelatedLimitedPartnershipSyndicator",
          "hasRelatedGeneralPartnerManagingMemberSyndicator",
          "hasRelatedPrivateFundAdviser",
          "isSecuritiesBuyerFromClientsForSelfToClientsFromOwned",
          "isSecuritiesFirmBoughSoldClientRecommended",
          "isSecuritiesClientRecommendedFirmOwnedSecurity",
          "hasTradeExecutionClient",
          "isSecuritiesUnderwriterPurchaserManagerClientRecommendedSecurity",
          "hasRecommendedPurchaseSaleFirmOwnedSecurity",
          "hasClientDiscretionBuySell",
          "hasClientDiscretionBuySellAmount",
          "hasClientDiscretionBrokerSelection",
          "hasClientDiscretionCommisionCost",
          "isClientBrokerRelatedParty",
          "hasClientBrokerRecommendation",
          "isClientBrokerRecommenationRelatedParty",
          "isBrokerSoftDollarRecipient",
          "isBrokerSoftDollarEligibleResearchService",
          "hasCompensationForClientReferrals",
          "isCompensatedForClientReferrals",
          "hasCustodyClientCash",
          "hasCustodyClientSecurities",
          "amountAUMClientSecurities",
          "countCustodyClients",
          "hasAdvisoryCustodyClientCash",
          "hasAdvisoryCustodyClientSecurities",
          "amountAUMAdvisoryClientCash",
          "countAdvisoryCustodyClientCash",
          "hasQualifiedCustodianSendInvestorQuarterlyReports",
          "hasIndependentAccountAuditPooledInvestments",
          "hasIndependentAccountSurpriseAuditClientFunds",
          "hasIndependentAccountantPrepareInternalControlReports",
          "isQualifiedCustodian",
          "hasRelatedQualifiedCustodian",
          "monthYearLastSurpriseAudit",
          "countQualifiedCustodians",
          "hasControlPersonUnnamed",
          "hasManagementSupervisedPersonEvent",
          "hasManagerFelonyPleaConviction",
          "hasManagerFelonyCharge",
          "hasManagerMisdemeanorPleaConviction",
          "hasManagerMisdemeanrCharge",
          "hasManagerSEC_CFTCFalseStatementOmission",
          "hasManagerSEC_CFTCStatuteViolation",
          "hasManagerSEC_CFTCAuthorizationAction",
          "hasManagerSEC_CFTCOrderAgainst",
          "hasManagerSEC_CFCPenaltyCeaseDesist",
          "hasManagerFederalStateForeignFalseStatement",
          "hasManagerFederalStateForeignInvestmentViolation",
          "hasManagerFederalStateForeignBusinessRevokeSuspended",
          "hasManagerFederalStateForeignOrderAgainst",
          "hasManagerFederalStateForeignLicenseRevoked",
          "hasManagerSelfRegulatedBodyFalseStatement",
          "hasManagerSelfRegulatedBodyRuleViolation",
          "hasManagerSelfRegulatedBodyBusinessRevokeSuspension",
          "hasManagerSelfRegulatedBodyActivityBan",
          "hasManagerAttorneyAccountantFederalContractorPriorBanRevoke",
          "isManagerSubjectToRegulatoryProceeding",
          "hasManagerDomesticForeignCourtEnjoinedInvestmentActivity",
          "hasManagerDomesticForeignCourtGuiltyStatuteViolation",
          "hasManagerDomesticForeignCourtDismissedActionSettlementPursuant",
          "isManagerDomesticForeignCourtSubjectToProceeding",
          "nameSECRegion",
          "idCRD",
          "idSEC",
          "addressStreet1OfficePrimary",
          "addressStreet1OfficePrimary2",
          "cityStateZipOfficePrimary",
          "cityStateZipOfficeMail",
          "nameContact",
          "phoneOfficePrimary",
          "typeEntity",
          "descriptionManagerServices",
          "statusSEC",
          "dateStatusSEC",
          "countCriminalDisclosures",
          "countRegulatoryActions",
          "countCivilDisclosures",
          "countBankruptcyDisclosures",
          "countJudgementLiens",
          "countBondPayoutDisclosures",
          "statusSEC",
          "dateStatusSEC",
          "urlManager",
          "hasMultipleEntityURLs",
          "countEmployeesTotalOver1000",
          "countEmployeesInvestmentAdvisoryOver1000",
          "countEmployeesBrokerDealerOver1000",
          "countEmployeesStateRegisteredInvestmentAdviserOver1000",
          "rangeClients",
          "countClientsOver500",
          "rangeClientsIndividualNonHighNetWorth",
          "rangeClientsIndividualHighNetWorth",
          "rangeClientsBankThrift",
          "rangeClientsInvestmentCompany",
          "rangeClientsBusinessDevelopmentCompany",
          "rangeClientsPooledInvestmentVehicle",
          "rangeClientsPensionPlan",
          "rangeClientsCharitableOrganization",
          "rangeClientsCorporationOther",
          "rangeClientsStateMunicipalGovernment",
          "rangeClientsInvestmentAdviserOther",
          "typeServicesOther",
          "typeBusinessOther",
          "hasCustodyClientCash",
          "hasCustodyClientSecurities",
          "amountAUMAdvisoryClientCashSecurities",
          "countAdvisoryCustodyClientCashSecurities",
          "hasAdvisoryCustodyClientCashSecurities",
          "hasControlPersonUnnamed",
          'statusSEC',
          'statusFINRA',
          'hasExemeptionAsSolelyVentureAdviser',
          'hasExemptionAsPrivateFundManagerUnder150MAUM',
          'hasExemptionSoleyPrivateFundManagrAUMOver150M'
        )
      )
    return(sec_name_df)
  }

parse_adv_excel_data <-
  function(file_path = "/Users/alexbresler/Desktop/adv_data/ia080116.xlsx") {
    excel_data <-
      file_path %>%
      readxl::read_excel() %>%
      suppressWarnings()

    return(excel_data)
  }

parse_adv_csv <-
  function(file_path = "/Users/alexbresler/Desktop/adv_data/IA FOIA Download 7-30-10.CSV") {
    data <-
      file_path %>%
      read_csv %>%
      suppressWarnings()
    return(data)
  }
parse_adv_txt_data <-
  function(file_path = "/Users/alexbresler/Desktop/adv_data/5010912_10044_00050000_00050000.txt") {
    data <-
      file_path %>%
      read_delim(delim = '|', col_names = T) %>%
      dplyr::select(-matches("X26")) %>%
      suppressWarnings()
    return(data)
  }

parse_sec_adv_data_url <-
  function(url = 'https://www.sec.gov/foia/iareports/ia090116.zip',
           file_directory = NULL,
           folder_name = 'adv_data',
           remove_existing_folder = F,
           remove_files = T,
           empty_trash = T) {
    options(scipen = 999999)

    no_folder_directories <-
      file_directory %>% is_null & folder_name %>% is_null

    if (no_folder_directories) {
      stop("Please identify a file directory and or a folder name")
    }
    only_folder <-
      !folder_name %>% is_null & file_directory %>% is_null
    if (only_folder) {
      file_directory <-
        getwd()
    }

    file_directory <-
      file_directory %>%
      paste0('/', folder_name)

    date_data <-
      url %>% basename() %>% str_replace_all('ia|.zip|-exempt', '') %>% lubridate::mdy()

    is_exempt <-
      url %>% str_detect("exempt")

    file <-
      url %>% basename()

    temp.dir <-
      file_directory

    file_path <-
      temp.dir %>% str_split('/') %>% flatten_chr() %>% .[1:length(.)] %>% paste0(collapse = '/')
    if (remove_existing_folder) {
      if (dir.exists(paths = file_path)) {
        "rm -R " %>%
          paste0(temp.dir) %>%
          system()
        if (empty_trash == T) {
          system('rm -rf ~/.Trash/*')
        }
      }
    }

    if (!dir.exists(paths = file_path)) {
      dir.create(temp.dir)
    }

    file <-
      temp.dir %>%
      paste0('/', file)
    httr::set_config(config(ssl_verifypeer = 0L))

    url %>%
      curl_download(url = ., destfile = file)

    file %>%
      unzip(exdir = paste0(temp.dir, '/'))

    dir_files <-
      temp.dir %>%
      list.files()

    file_name <-
      dir_files %>%
      str_detect('CSV|csv|TXT|txt|XLS|XLSX|xlsx|xls') %>%
      dir_files[.]

    file_name <-
      file_directory %>%
      paste0('/', file_name)

    if (file_name %>% str_detect("XLS|xls|xlsx|XLSX")) {
      adv_data <-
        file_name %>% parse_adv_excel_data()
    }

    if (file_name %>% str_detect("csv|CSV")) {
      adv_data <-
        file_name %>% parse_adv_csv() %>%
        suppressWarnings()
    }

    if (file_name %>% str_detect("txt|TXT")) {
      adv_data <-
        file_name %>% parse_adv_txt_data()
    }

    sec_names_df <-
      get_sec_adv_name_df()

    adv_names <-
      data_frame(nameSEC = names(adv_data)) %>%
      mutate(idRow = 1:n()) %>%
      group_by(nameSEC) %>%
      dplyr::filter(idRow == min(idRow)) %>%
      ungroup %>%
      .$idRow

    adv_data <-
      adv_data[, adv_names]

    df_names <-
      names(adv_data) %>%
      map(function(x) {
        data_frame(nameActual = sec_names_df %>%
                     dplyr::filter(nameSEC == x) %>%
                     .$nameActual)
      }) %>%
      bind_rows %>%
      .$nameActual

    adv_data <-
      adv_data %>%
      set_names(df_names)

    has_columns <-
      (adv_data %>%
         dplyr::select(-matches("country")) %>%
         dplyr::select(matches("^count[A-Z]")) %>% ncol > 0) &
      (adv_data %>%
         dplyr::select(-matches("country")) %>%
         dplyr::select(matches("^count[A-Z]")) %>%
         map_df(class) %>%
         gather(column, class) %>%
         dplyr::filter(class == 'character') %>% nrow > 0)

    if (has_columns) {
      change_to_range_cols <-
        adv_data %>%
        dplyr::select(-matches("country")) %>%
        dplyr::select(matches("^count[A-Z]")) %>%
        map_df(class) %>%
        gather(column, class) %>%
        dplyr::filter(class == 'character') %>%
        .$column
      for (x in 1:length(change_to_range_cols)) {
        name_loc <-
          change_to_range_cols[x] %>% grep(names(adv_data)) %>% min

        names(adv_data)[name_loc] <-
          names(adv_data)[name_loc] %>% str_replace("count", 'range')
      }
    }
    if (adv_data %>% dplyr::select(matches("^has[A-Z]|^is[A-Z]")) %>% names %>% length > 0) {
      adv_data <-
        adv_data %>%
        mutate_at(.cols =
                    adv_data %>% dplyr::select(matches("^has[A-Z]|^is[A-Z]")) %>% names,
                  .funs = str_trim) %>%
        mutate_at(.cols =
                    adv_data %>% dplyr::select(matches("^has[A-Z]|^is[A-Z]")) %>% names,
                  funs(if_else(. == "Y", TRUE, FALSE))) %>%
        suppressWarnings()
    }

    if (adv_data %>% dplyr::select(matches("^url[M]")) %>% names %>% length > 0) {
      adv_data <-
        adv_data %>%
        mutate_at(.cols =
                    adv_data %>% dplyr::select(matches("^url[M]")) %>% names,
                  .funs = str_to_lower) %>%
        suppressWarnings()
    }

    if (adv_data %>% dplyr::select(matches("^status[SEC]")) %>% dplyr::select(-matches("date")) %>% names %>% length > 0) {
      adv_data <-
        adv_data %>%
        mutate_at(
          .cols =
            adv_data %>% dplyr::select(matches("^status[SEC]")) %>% dplyr::select(-matches("date")) %>% names,
          .funs = str_to_lower
        ) %>%
        suppressWarnings()

    }

    if (adv_data %>%
        dplyr::select(matches("^date")) %>%
        keep(is.character) %>%
        names %>% length > 0) {
      char_col <-
        adv_data %>%
        dplyr::select(matches("^date")) %>%
        keep(is.character) %>%
        names

      adv_data <-
        adv_data %>%
        mutate_at(char_col,
                  lubridate::mdy()) %>%
        suppressWarnings()
    }

    adv_data <-
      adv_data %>%
      mutate_at(.cols =
                  adv_data %>% dplyr::select(
                    matches(
                      "^type[A-Z]|^range[A-Z]|^address[A-Z]|^city[A-Z]|^zip[A-Z]|^fax[A-Z]|^name[A-Z]|^state[A-Z]|^status[A-Z]|monthYearLastSurpriseAudit|^date[A-Z]"
                    )
                  ) %>% names,
                .funs = as.character) %>%
      mutate(idCRD = idCRD %>% as.integer)

    if ('idLEI' %in% names(adv_data)) {
      if (adv_data$idLEI %>% class == 'numeric') {
        adv_data <-
          adv_data %>%
          mutate(idLEI = idLEI %>% as.character)
      }
    }

    if ('rangeClientsFinancialPlanning' %in% names(adv_data)) {
      if (adv_data$rangeClientsFinancialPlanning %>% class == 'numeric') {
        adv_data <-
          adv_data %>%
          mutate(rangeClientsFinancialPlanning = rangeClientsFinancialPlanning %>% as.character)
      }
    }

    adv_data <-
      adv_data %>%
      mutate(dateDataADV = date_data,
             isExempt = is_exempt) %>%
      dplyr::select(dateDataADV, isExempt, everything())

    if (remove_files) {
      "rm -R " %>%
        paste0(temp.dir) %>%
        system()
      if (empty_trash) {
        system('rm -rf ~/.Trash/*')
      }
    }
    return(adv_data)
  }

get_period_type_adv_data <-
  function(period = "2016-08",
           is_exempt = F,
           only_most_recent = F,
           file_directory = NULL,
           folder_name = 'adv_data',
           remove_existing_folder = F,
           remove_files = T,
           empty_trash = T) {
    setwd("~")
    if (!'url_df' %>% exists){
      url_df <-
        get_data_adv_period_urls(return_wide = F)
    }

    if (only_most_recent) {
      period <-
        url_df %>% dplyr::filter(dateData == max(dateData)) %>% .$periodData %>% unique
    }

    if (!period %in% url_df$periodData) {
      available_periods <-
        url_df$periodData %>% unique

      "\nSorry periods can only be:\n" %>%
        paste0(paste0(available_periods, collapse = '\n')) %>%
        stop()
    }
    url_data <-
      url_df %>% dplyr::filter(periodData == period, isExempt == is_exempt) %>% .$urlZip

    parse_sec_adv_data_url_safe <-
      possibly(parse_sec_adv_data_url, otherwise = NULL)

    adv_data <-
      url_data %>%
      parse_sec_adv_data_url_safe(
        file_directory = file_directory,
        folder_name = folder_name,
        remove_existing_folder = remove_existing_folder,
        remove_files = remove_files,
        empty_trash = empty_trash
      )
    return(adv_data)
  }


#' Get ADV summary data for specified periods and filing types
#'
#' @param periods Selected periods in Year-Month form
#' @param all_periods Do you want all periods
#' @param is_exempt Do you want to exempt, non-exempt or both types of filers
#' @param only_most_recent Select only the most recent period
#' @param file_directory Location of the directory you want to save your data into
#' @param folder_name Name of the folder you want the data to be downloaded into
#' @param remove_files Remove the files from the folders
#' @param empty_trash Do you wish to empty the trash after being read into R
#' @import dplyr stringr lubridate readr readxl rvest purrr
#' @importFrom curl curl_download
#' @return
#' @export
#'
#' @examples
#' get_data_adv_managers_periods_summaries(periods = c("2006-06"), all_periods = F, is_exempt = c(F,T), only_most_recent = F,
#' file_directory = NULL, folder_name = 'adv_data', remove_existing_folder = F, remove_files = T, empty_trash = T)
get_data_adv_managers_periods_summaries <-
  function(periods = c("2006-06"),
           all_periods = F,
           is_exempt = c(F,T),
           only_most_recent = F,
           file_directory = NULL,
           folder_name = 'adv_data',
           remove_existing_folder = F,
           remove_files = T,
           empty_trash = T) {
    if (all_periods) {
      periods <-
        get_data_adv_period_urls() %>% .$periodData %>% unique
    }

    input_df <-
      expand.grid(period = periods,
                  only_most_recent = only_most_recent,
                  exempt = is_exempt,
                  stringsAsFactors = F) %>%
      as_data_frame()

    get_period_type_adv_data_safe <-
      possibly(get_period_type_adv_data, NULL)

    all_adv_data <-
      1:nrow(input_df) %>%
      purrr::map_df(function(x) {
        get_period_type_adv_data_safe(
          period = input_df$period[x],
          is_exempt = input_df$exempt[x],
          only_most_recent = input_df$only_most_recent[x],
          file_directory = file_directory,
          folder_name = folder_name,
          remove_existing_folder = remove_existing_folder,
          remove_files = remove_files,
          empty_trash = empty_trash
        )
      }) %>%
      suppressWarnings()
    has_data <-
      all_adv_data %>% nrow > 0
    if (has_data) {
      all_adv_data <-
        all_adv_data %>%
        mutate_at(.cols =
                    all_adv_data %>% dplyr::select(matches("^date[A-Z]")) %>% names,
                  funs(. %>% lubridate::ymd())) %>%
        mutate_at(
          .cols =
            all_adv_data %>% dplyr::select(matches(
              "^name[E]|^address|^city|^status|^state|^type|^country[A-Z]"
            )) %>% dplyr::select(-matches("stateEntityOrganized")) %>% names,
          .funs =
            str_to_upper
        )

      all_adv_data <-
        all_adv_data %>%
        mutate_at(
          .cols =
            all_adv_data %>% dplyr::select(matches("^count[A-Z]"), -matches("country")) %>% names,
          .funs =
            formattable::comma
        )

      has_amounts <-
        names(all_adv_data) %>% str_count('^amount') %>% sum > 0

      if (has_amounts) {
        all_adv_data <-
          all_adv_data %>%
          mutate_at(
            .cols =
              all_adv_data %>% dplyr::select(matches("^amount[A-Z]")) %>% names,
            .funs =
              funs(. %>% as.numeric %>% formattable::currency())
          ) %>%
          arrange(dateDataADV, desc(amountAUMTotal)) %>%
          suppressWarnings()
      }

      all_adv_data <-
        all_adv_data %>%
        dplyr::rename(nameEntityManagerBusiness = nameEntityManager) %>%
        mutate(nameEntityManager = nameEntityManagerLegal) %>%
        dplyr::select(dateDataADV:idSEC, nameEntityManager, nameEntityManagerLegal, nameEntityManagerBusiness, everything()) %>%
        suppressMessages()

      all_adv_data <-
        all_adv_data %>%
        mutate_at(.cols =
                    all_adv_data %>% dplyr::select(matches("^address|^country[A-Z]|^city^state")) %>% names,
                  .funs = str_to_upper)

      if (names(all_adv_data) %>% str_count('^addressStreet2OfficePrimary') %>% sum > 0) {
        locationOfficePrimary <-
          all_adv_data %>%
          replace_na(list(addressStreet2OfficePrimary = '', stateOfficePrimary = '')) %>%
          mutate(locationOfficePrimary =
                   addressStreet1OfficePrimary %>% paste0(' ', addressStreet2OfficePrimary, ' ', cityOfficePrimary, ', ', stateOfficePrimary, ', ', countryOfficePrimary) %>% str_trim) %>%
          .$locationOfficePrimary

        all_adv_data <-
          all_adv_data %>%
          mutate(locationOfficePrimary) %>%
          dplyr::select(dateDataADV:typeRegulationSEC, nameEntityManager, nameEntityManagerLegal, nameEntityManagerBusiness, locationOfficePrimary, everything())
      }
    }

    return(all_adv_data)
  }

#' Get ADV filing data for the most recent filing period
#'
#' @param file_directory Location of the directory you want to save your data into
#' @param folder_name Name of the folder you want the data to be downloaded into
#' @param remove_files Remove the files from the folders
#' @param empty_trash Do you wish to empty the trash after being read into R
#' @return
#' @export
#'
#' @examples
#' get_data_adv_managers_current_period_summary(ile_directory = NULL, folder_name = 'adv_data', remove_files = T, empty_trash = T)

get_data_adv_managers_current_period_summary <-
  function(file_directory = NULL,
           folder_name = 'adv_data',
           remove_files = T,
           empty_trash = T
  ) {
    all_data <-
      get_data_adv_managers_periods_summaries(
        only_most_recent = T,
        all_periods = F,
        remove_files = remove_files,
        empty_trash = empty_trash,
        is_exempt = c(F, T),
        file_directory = file_directory,
        folder_name = folder_name
      )
    select_names <-
      c(
        "dateDataADV",
        "isExempt",
        "idRegionSEC",
        "idCRD",
        "idSEC",
        "typeRegulationSEC",
        "nameEntityManager",
        "nameEntityManagerLegal",
        'locationOfficePrimary',
        "addressStreet1OfficePrimary",
        "addressStreet2OfficePrimary",
        "cityOfficePrimary",
        "stateOfficePrimary",
        "countryOfficePrimary",
        "zipOfficePrimary",
        "phoneOfficePrimary",
        "statusSEC",
        "dateStatusSEC",
        "dateADVLatest",
        "urlManager",
        "isForeignRegisteredEntity",
        "stateDateJurisdictionNotice",
        "idCIK",
        "hasAUMGreater1B",
        "idLEI",
        "hasAUMGreater100M",
        "typeEntity",
        "countryEntityOrganized",
        "countEmployeesTotal",
        "countEmployeesInvestmentAdvisory",
        "amountAUMTotal",
        "amountAUMDiscretionary",
        "amountAUMNonDiscretionary",
        "countAccountsDiscretionary",
        "countAccountsNonDiscretionary",
        "countAccountsTotal",
        "isManagerSecuritiesPortfolio",
        "hasFeeAUM",
        "hasFeeHourlyCharge",
        "hasFeeSubscription",
        "hasFeeFixed",
        "hasFeeCommission",
        "hasFeePerformance",
        "hasFeeOther",
        "typeFeeOther",
        "isBrokerDealer",
        "isBrokerDealerRepresentative",
        "isCommodityPoolOperator",
        "isFuturesMerchant",
        "isRealEstateBrokerDealerAgent",
        "isInsuranceBrokerAgent",
        "isBank",
        "isTrustCompany",
        "isRegisteredMunicipalAdviser",
        "isRegisteredSecuritySwapDealer",
        "isRegistredSecuritySwapParticipant",
        "isAccountingFirm",
        "isLawFirm",
        "isOtherFinancialProductSalesperson",
        "typeOtherFinancialProductSalesperson",
        "countEmployeesBrokerDealer",
        "countEmployeesStateRegisteredInvestmentAdviser",
        "countEmployeesStateRegisteredInvestmentAdviserMultipleEntities",
        "countEmployeesLicensedInsuranceAgents",
        "countEmployeesSolicitAdvisoryClients",
        "hasManagerFelonyPleaConviction",
        "hasManagerFelonyCharge",
        "hasManagerMisdemeanorPleaConviction"
      )

    all_data <-
      all_data %>%
      dplyr::select(one_of(select_names))

    return(all_data)
  }
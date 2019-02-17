
function(url = "http://www.economagic.com/em-cgi/data.exe/treas/pubdebt#Data") {
  page <-
    url %>%
    read_html()

  raw_data <-
    page %>%
    html_nodes('pre') %>%
    html_text()

  raw_data <-
    raw_data %>%
    stringr::str_split('\n') %>%
    flatten_chr() %>%
    str_trim() %>% {
      .[!. %in% c('')]
    }

  data <-
    raw_data[raw_data %>% str_detect("^[0-9]")]

  raw_data <-
    raw_data[raw_data %>% nchar() > 10]

  page %>%
    html_nodes("font+ font") %>%
    html_text()

  tibble(data)
    }

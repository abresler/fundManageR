#' IREI Entities
#'
#' Acquires a list of IREI tracked entities
#' from 14 different parts of the "Commercial Real Estate Jungle"
#'
#' @param filter_type  filter entities types if \code{NULL} returns all options \itemize{
#' \item Accounting
#' \item Brokerage Firms
#' \item Construction Companies
#' \item Consultants
#' \item Developers and Operating Companies
#' \item Investment Bankers
#' \item Investment Managers
#' \item Law Firms
#' \item Real Estate Finance
#' \item Real Estate Recruiters
#' \item Real Estate Research & Information Providers
#' \item Seminar & Conference Producers
#' \item Tax-exempt Funds
#' \item Trade Associations & Organizations
#' }
#' @param return_message if \code{TRUE} returns a message
#'
#' @return a \code{tibble}
#' @source IREI
#' @export
#' @import dplyr rvest glue tidyr stringr tibble
#'
#' @examples
#' \dontrun{
#' irei_entitites(filter_type = NULL)
#' irei_entities(filter_type = c("Investment Managers", "Tax-exempt Funds"))
#' }
irei_entitites <- function(filter_type = NULL, return_message = TRUE) {
  page <-
    "https://irei.com/industry-links/" %>%
    read_html()

  entity_nodes <-
    page %>%
    html_nodes('strong , td a')

  headers <-
    page %>%
    html_nodes('strong:nth-child(1)') %>%
    html_text()

  entities <-
    entity_nodes %>%
    html_text()

  urls <-
    entity_nodes %>%
    html_attr('href')

  data <-
    tibble(nameEntity = entities, urlEntity = urls) %>%
    filter(!nameEntity == '') %>%
    mutate(idRow = 1:n())

  df_headers <-
    tibble(nameEntity = headers) %>%
    left_join(data %>% dplyr::select(nameEntity, idRow)) %>%
    suppressMessages()

  skip <- df_headers$idRow

  df_headers <-
    df_headers %>%
    dplyr::rename(typeFirm = nameEntity) %>%
    mutate(idRow = idRow + 1)

  data <-
    data %>%
    left_join(df_headers) %>%
    slice(-skip) %>%
    dplyr::select(typeFirm, nameEntity, urlEntity) %>%
    fill(typeFirm) %>%
    suppressMessages() %>%
    filter(!is.na(urlEntity))

  remove <-
    c(
      '\\, LLP',
      '\\ LLC',
      "\\(Calif.",
      '\\ Ltd.',
      "\\(State of",
      '\\, Inc.',
      '\\, LLC',
      '\\, LP',
      '\\, Ltd.'
    ) %>%
    str_c(collapse = "|")

  data <-
    data %>%
    mutate(
      nameSearch = nameEntity,
      nameSearch = nameSearch %>% str_replace_all('\\, The', '') %>% str_replace_all(remove, '')
    )

  data <-
    data %>%
    tidyr::separate(nameSearch,
                    sep = '\\(',
                    into = c('nameSearch',
                             'acronymEntity')) %>%
    mutate(nameSearch = nameSearch %>% str_replace_all("FIABCI-USA (International Real Estate Federation)",
                                                       "International Real Estate Federation (FIABCI-USA)") %>%
                                                       str_replace_all('\\)', ""),
           acronymEntity = acronymEntity %>% str_replace_all('\\)', '')) %>%

    suppressWarnings() %>%
    dplyr::select(typeFirm, nameSearch, nameEntity, acronymEntity, everything())

  if (!filter_type %>% purrr::is_null()) {
    filter_type <-
      filter_type %>% str_to_lower() %>% str_replace_all('\\-', '')

    data <-
      data %>%
      mutate(typeFilter = typeFirm %>% str_to_lower() %>% str_replace_all('\\-', '')) %>%
      filter(typeFilter %in% filter_type) %>%
      dplyr::select(-typeFilter)
  }

  if (return_message) {
    firm_count <-
      data %>% nrow() %>% formattable::comma(digits = 0)

    segments <-
      data$typeFirm %>% unique() %>% str_c(collapse = ', ')

    glue::glue("Acquired {firm_count} firms from the {segments} sectors of the institutional real estate jungle") %>%
      cat(fill = T)

  }
  data
}
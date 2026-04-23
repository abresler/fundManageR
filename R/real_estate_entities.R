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

  # Categories are h1 elements with id attributes, followed by ul with links
  h1_nodes <- page %>% html_nodes('h1[id]')

  all_data <- purrr::map_dfr(seq_along(h1_nodes), function(i) {
    h1_node <- h1_nodes[[i]]
    category <- h1_node %>% html_text() %>% str_trim()

    # Get the next sibling ul element
    ul_node <- h1_node %>%
      rvest::html_element(xpath = "following-sibling::ul[1]")

    if (is.na(ul_node) || is.null(ul_node)) {
      return(tibble())
    }

    links <- ul_node %>% html_nodes('li a')

    if (length(links) == 0) {
      return(tibble())
    }

    tibble(
      typeFirm = category,
      nameEntity = links %>% html_text() %>% str_trim(),
      urlEntity = links %>% html_attr('href')
    )
  })

  if (nrow(all_data) == 0) {
    warning("No data found from IREI website - site structure may have changed")
    return(tibble(
      typeFirm = character(),
      nameSearch = character(),
      nameEntity = character(),
      acronymEntity = character(),
      urlEntity = character()
    ))
  }

  data <- all_data %>%
    filter(!is.na(.data$urlEntity) & .data$urlEntity != "" & !is.na(.data$nameEntity) & .data$nameEntity != "")

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

  if (!filter_type %>% is.null()) {
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
      cat(fill = TRUE)

  }
  data
}
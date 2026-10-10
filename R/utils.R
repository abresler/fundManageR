#' Import URL with curl
#'
#' Downloads a file from a URL using curl and imports it with rio.
#' More robust than rio::import() directly on URLs, especially for
#' ZIP files and other compressed formats.
#'
#' @param url URL to download and import
#' @param format optional file format hint for rio::import (e.g., "csv", "xlsx")
#' @param ... additional arguments passed to rio::import
#'
#' @return imported data (usually a data frame)
#' @keywords internal
#' @import curl rio
.import_url_curl <- function(url, format = NULL, ...) {
  # Determine file extension for temp file
  url_basename <- basename(url)
  ext <- tools::file_ext(url_basename)
  if (ext == "") ext <- "tmp"

  # Create temp file with appropriate extension

  temp_file <- tempfile(fileext = paste0(".", ext))

  # Download using curl
  tryCatch({
    curl::curl_download(url, destfile = temp_file, quiet = TRUE)
  }, error = function(e) {
    stop(paste0("Failed to download URL: ", url, "\nError: ", e$message))
  })

  # Import using rio
  result <- tryCatch({
    if (!is.null(format)) {
      rio::import(temp_file, format = format, ...)
    } else {
      rio::import(temp_file, ...)
    }
  }, error = function(e) {
    unlink(temp_file)
    stop(paste0("Failed to import file: ", temp_file, "\nError: ", e$message))
  })

  # Clean up temp file
  unlink(temp_file)

  return(result)
}

#' Drop NA columns
#'
#' This function drops NA
#' columns from a specified data frame.
#'
#' @param data a \code{data frame}
#'
#' @return \code{tibble}
#' @export
#' @import dplyr
#' @family utility function
#' @examples
#' tibble(nameFirm = 'Goldman Sachs', countSuperHeros = NA, countCriminals = 0, countFinedEmployees = 10) %>% drop_na_columns()
drop_na_columns <-
  function(data) {
    data %>%
      select(which(colMeans(is.na(.)) < 1)) %>%
      suppressMessages() %>%
      suppressWarnings()
  }

#' Class DF
#'
#' This function returns the
#' column classes of a specified
#' data frame.
#'
#' @param data a \code{tibble}
#'
#' @return \code{tibble}
#' @family utility function
#' @export
#' @import dplyr purrr
#' @examples
#' get_class_df(mtcars)
get_class_df <-
  function(data) {
    class_data <-
      data %>%
      future_map(class)

    class_df <-
      seq_along(class_data) %>%
      future_map_dfr(function(x) {
        tibble(nameColumn = names(data)[[x]],
                   typeColumn = class_data[[x]] %>% .[length(.)])
      })

    return(class_df)
  }


#' Tidy column formatting
#'
#' Tidys a data frame to return unified case names, autoparses logical columns
#' and auto formats counts, amounts and values.
#'
#' @param data \code{tibble}
#' @param drop_na_columns \code{TRUE} drops NA columns
#' @return \code{tibble}
#' @export
#' @import dplyr stringr formattable purrr tidyr
#' @family utility function
#' @examples
#' library(dplyr)
#' tibble(nameFund = "Blackstone Real Estate Fund IX", isNewFund = "N/A",
#' countAssets = 12000, amountAUM = 65000000, isRealEstateFund = 1) %>% tidy_column_formats(drop_na_columns = FALSE)
#' Convert column names to snake_case
#'
#' Converts tibble column names from the package's native camelCase
#' (\code{idCRD}, \code{nameEntityManager}, \code{amountFundGrossAUM})
#' to snake_case (\code{id_crd}, \code{name_entity_manager}, \code{amount_fund_gross_aum})
#' for ingestion into SQL / DuckDB / parquet pipelines where unquoted
#' identifiers and downstream tooling expect snake_case.
#'
#' Acronyms embedded in camelCase (\code{CRD}, \code{SEC}, \code{AUM},
#' \code{CIK}, \code{LEI}, \code{PCAOB}, \code{GP}, \code{REIT},
#' \code{CDO}, \code{LP}, \code{LLC}) are coerced to lower-case runs
#' (e.g. \code{amountFundGrossAUM -> amount_fund_gross_aum}) rather than
#' split letter-by-letter.
#'
#' Idempotent — passing an already-snake-cased tibble is a no-op.
#'
#' @param data a \code{data.frame} / \code{tibble}
#' @param protect character vector of column names to leave untouched
#'   (e.g. when a downstream schema pins specific names)
#' @return tibble with snake_case column names
#' @export
#' @family utility function
#' @examples
#' library(dplyr)
#' tibble::tibble(idCRD = 156663, nameEntityManager = "ROCKWOOD",
#'                amountFundGrossAUM = 1e9) %>% tidy_snake_case()
tidy_snake_case <-
  function(data, protect = character()) {
    if (is.null(data) || !is.data.frame(data) || ncol(data) == 0) {
      return(data)
    }
    nm <- names(data)
    keep_mask <- nm %in% protect
    # Preserve multi-letter ADV/SEC acronyms so they collapse to a single
    # snake token instead of one underscore per letter.
    acronyms <- c("CRD", "CIK", "SEC", "LEI", "AUM", "PCAOB", "GAAP",
                  "GP", "LP", "LLC", "REIT", "CDO", "ETF", "IRR",
                  "MSA", "MSCI", "CUSIP", "SIC", "NAICS", "FINRA",
                  "NAREIT", "DTCC", "CRSP", "XBRL", "FOIA", "ADV",
                  "API", "ID", "URL", "ICA", "RMUP", "LSF", "EU", "UK", "US")
    to_convert <- nm[!keep_mask]
    lowered <- to_convert
    for (ac in acronyms) {
      lowered <- gsub(
        paste0("(?<![A-Za-z])", ac, "(?![A-Za-z])"),
        tolower(ac),
        lowered,
        perl = TRUE
      )
      # Handle acronym at end of token or preceded by lowercase
      # (amountFundGrossAUM -> amountFundGrossaum)
      lowered <- gsub(
        paste0("(?<=[a-z0-9])", ac, "(?![A-Za-z])"),
        paste0("_", tolower(ac)),
        lowered,
        perl = TRUE
      )
      # Handle acronym followed by capital (CRDFirst -> crd_first handled below)
      lowered <- gsub(
        paste0("(?<![A-Za-z])", ac, "(?=[A-Z])"),
        paste0(tolower(ac), "_"),
        lowered,
        perl = TRUE
      )
    }
    converted <- snakecase::to_snake_case(lowered)
    new_nm <- nm
    new_nm[!keep_mask] <- converted
    # dedupe (collision safety: append _N to repeats)
    if (anyDuplicated(new_nm)) {
      dup_ix <- which(duplicated(new_nm))
      for (i in dup_ix) {
        suffix <- 2L
        while (paste0(new_nm[i], "_", suffix) %in% new_nm) suffix <- suffix + 1L
        new_nm[i] <- paste0(new_nm[i], "_", suffix)
      }
    }
    names(data) <- new_nm
    data
  }


tidy_column_formats <-
  function(data, drop_na_columns = TRUE) {
    data <-
      data %>%
      mutate(across(where(is_character), ~ifelse(. == "N/A", NA, .))) %>%
      mutate(across(where(is_character), ~ifelse(. == "", NA, .)))

    data <-
      data %>%
      mutate(across(matches("^idCRD"), ~as.numeric(.))) %>%
      mutate(across(
        matches("^name[A-Z]|^details[A-Z]|^description[A-Z]|^city[A-Z]|^state[A-Z]|^country[A-Z]|^count[A-Z]|^street[A-Z]|^address[A-Z]") & !matches("nameElement"),
        ~str_to_upper(.)
      )) %>%
      mutate(across(
        matches("^amount"),
        ~as.numeric(.) %>% formattable::currency(digits = 0)
      )) %>%
      mutate(across(matches("^is|^has"), ~as.logical(.))) %>%
      mutate(across(
        matches("latitude|longitude"),
        ~as.numeric(.) %>% formattable::digits(digits = 5)
      )) %>%
      mutate(across(
        matches("^price[A-Z]|pershare") & !matches("priceNotation"),
        ~formattable::currency(., digits = 3)
      )) %>%
      mutate(across(
        matches("^count[A-Z]|^number[A-Z]|^year[A-Z]") & !matches("country|county"),
        ~formattable::comma(., digits = 0)
      )) %>%
      mutate(across(
        matches("codeInterestAccrualMethod|codeOriginalInterestRateType|codeLienPositionSecuritization|codePaymentType|codePaymentFrequency|codeServicingAdvanceMethod|codePropertyStatus"),
        ~as.integer(.)
      )) %>%
      mutate(across(matches("^ratio|^multiple|^priceNotation|^value"),
                    ~formattable::comma(., digits = 3))) %>%
      mutate(across(matches("^pct|^percent"),
                    ~formattable::percent(., digits = 3))) %>%
      mutate(across(
        matches("^amountFact"),
        ~as.numeric(.) %>% formattable::currency(digits = 3)
      )) %>%
      suppressWarnings()
    has_dates <-
      data %>% select(dplyr::matches("^date")) %>% ncol() > 0

    if (has_dates) {
      data %>% select(dplyr::matches("^date")) %>% future_map(class)
    }
    if (drop_na_columns ) {
      data <-
      data %>%
      drop_na_columns()
    }
    return(data)
  }


#' Tidy nested and count columns
#'
#' This function unnests any neseted columns and
#' converts any wide columns containing count information
#' into a tidy data frame
#'
#' @param data \code{data frame}
#' @param column_keys column keys
#' @param bind_to_original_df \code{TRUE} bind results to the original data frame in a nested column
#' @param clean_column_formats \code{TRUE} clean the columns
#'
#' @return \code{tibble}
#' @export
#' @import dplyr stringr formattable purrr tidyr
#' @family utility function
#' @examples
#' library(fundManageR)
#' library(dplyr)
#' get_data_sec_filer(entity_names = "8VC")
#' dataFilerRelatedParties %>%
#' tidy_column_relations()

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

    if ('dataAllFilings' %in% names(data)) {
      data <-
        data %>%
        unnest(cols = dataAllFilings) %>%
        tidy_column_formats()

      return(data)
    }

    if ('dataAssetXBRL' %in% names(data)) {
      data <-
        data %>%
        unnest(cols = dataAssetXBRL) %>%
        tidy_column_formats()

      return(data)
    }

    if (has_lists == F & columns_matching %>% length() == 0) {
      return(data)
    }


    data <-
      data %>%
      mutate(idRow = seq_len(n())) %>%
      select(idRow, everything())
    df <-
      tibble()

    if (columns_matching %>% length() > 0) {
      match <-
        columns_matching %>% str_replace_all('[0-9]', '') %>% unique() %>% paste0(collapse = '|')

      df_match <-
        data %>%
        select(idRow, any_of(column_keys), dplyr::matches(match))

      has_dates <-
        names(df_match) %>% str_count("date") %>% sum() > 0

      if (has_dates) {
        df_match <-
          df_match %>%
          mutate(across(matches("^date"), ~as.character(.)))
      }

      key_cols <-
        df_match %>%
        select(c(idRow, any_of(column_keys))) %>%
        names()

      valuecol <-
        'value'

      gathercols <-
        df_match %>%
        select(-c(idRow, any_of(column_keys))) %>%
        names()

      df_match <-
        df_match %>%
        pivot_longer(cols = all_of(gathercols), names_to = "item", values_to = "value", values_drop_na = TRUE) %>%
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
        seq_len(nrow(df_lists)) %>%
        future_map_dfr(function(x) {
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
            select(idRow, any_of(c(column_keys, column)))
          col_length_df <-
            seq_len(nrow(df)) %>%
            future_map_dfr(function(x) {
              value <-
                df[[column]][[x]]

              if (value %>% is.null()) {
                return(tibble(idRow = x))
              }
              columns_matching <-
                names(value)[!names(value) %>% substr(nchar(.), nchar(.)) %>% readr::parse_number() %>% is.na() %>% suppressWarnings()] %>%
                suppressWarnings() %>%
                suppressMessages() %>%
                suppressWarnings()

              if (column == "dataAllFilings") {
                return(tibble(idRow = x, countCols = value %>% ncol()))
              }
              if (columns_matching %>% length() == 0) {
                return(tibble(idRow = x))
              }
              tibble(idRow = x, countCols = value %>% ncol())
            })

          if (col_length_df %>% ncol() == 1) {
            return(tibble())
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
            unnest(cols = all_of(column))

          has_dates <-
            names(df) %>% str_count("date") %>% sum() > 0

          if (has_dates) {
            df <-
              df %>%
              mutate(across(matches("^date"), ~as.character(.)))
          }

          gathercols <-
            df %>%
            select(-c(idRow, any_of(column_keys))) %>%
            names()

          df <-
            df %>%
            pivot_longer(cols = all_of(gathercols), names_to = "item", values_to = "value", values_drop_na = TRUE) %>%
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
      return(data %>% select(-dplyr::matches("idRow")))
    }
    if (has_data) {
      df <-
        df %>%
        arrange(idRow) %>%
        distinct()

      col_order <-
        c(df %>% select(-c(item, value)) %>% names(), df$item)

      df <-
        df %>%
        pivot_wider(names_from = item, values_from = value) %>%
        select(any_of(col_order)) %>%
        suppressWarnings()

      has_dates <-
        data %>% select(dplyr::matches("^date")) %>% ncol() > 0

      if (has_dates) {
        df <-
          df %>%
          mutate(across(matches("^date[A-Z]"), ~lubridate::ymd(.))) %>%
          mutate(across(matches("^datetime[A-Z]"), ~lubridate::ymd_hm(.)))
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
          select(-dplyr::matches(ignore_cols)) %>%
          left_join(df %>%
                      nest(dataResolved = -idRow)) %>%
          suppressMessages()
        return(data)
      }
      if (!bind_to_original_df) {
        return(df)
      }
    }

  }


# ── URL cleaner ──────────────────────────────────────────────────────
# Internal helper used by ADV compilers and any other URL-bearing parquet
# writer. Lowercases scheme + host, adds http:// to bare domains, strips
# trailing slashes on bare-root, drops empty/garbage values, dedupes
# (case-insensitive).
.clean_urls <- function(x) {
  if (is.null(x) || !length(x)) return(character(0))
  x <- as.character(x)
  x <- x[!is.na(x)]
  if (!length(x)) return(character(0))
  # Treat null bytes + control chars as separators (host-spoofing mitigation),
  # not as silent strips that could weld two strings into one fake host.
  x <- gsub("[\\x00-\\x08\\x0b\\x0c\\x0e-\\x1f\\x7f]", " ", x, perl = TRUE)
  # Split on whitespace (newline, tab, space), comma, semicolon, pipe — many filers
  # paste 2+ URLs in one cell. Each component is cleaned independently.
  x <- unlist(strsplit(x, "[\\s,;\\|]+", perl = TRUE), use.names = FALSE)
  x <- trimws(x[!is.na(x) & nzchar(trimws(x))])
  if (!length(x)) return(character(0))
  # Drop common garbage tokens
  bad <- tolower(x) %in% c("none", "n/a", "na", "null", "tbd", "-", ".", "", "http://", "https://")
  x <- x[!bad]
  if (!length(x)) return(character(0))
  # Add scheme to bare domains
  x <- ifelse(grepl("^[a-zA-Z]+://", x),
              x,
              ifelse(grepl("^[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}", x),
                     paste0("http://", x),
                     x))
  # Lowercase scheme
  x <- sub("^([a-zA-Z]+)://", "\\L\\1://", x, perl = TRUE)
  # Lowercase host (everything before first /, ?, or #)
  x <- sub("^(https?://)([^/?#]+)", "\\1\\L\\2", x, perl = TRUE)
  # Strip trailing slash on bare-root URLs
  x <- sub("^(https?://[^/]+)/+$", "\\1", x)
  # Validate: scheme + dotted host + ≥2-letter TLD
  ok <- grepl("^https?://[a-z0-9][a-z0-9.-]*\\.[a-z]{2,}", x)
  x <- x[ok]
  unique(x)
}

#' Clean a character vector of URLs
#'
#' Lowercases scheme + host, adds \code{http://} to bare domains, strips
#' trailing slashes on bare-root URLs, drops empty / garbage entries
#' (\code{NONE}, \code{n/a}, scheme-only, etc.), dedupes case-insensitively.
#' Returns an empty character vector if no values survive.
#'
#' @param x character vector (or anything coercible)
#' @return character vector of cleaned URLs (length may be 0..length(x))
#' @export
#' @examples
#' clean_urls(c("HTTP://WWW.FOO.COM", "http://www.foo.com/", "n/a", "bar.org"))
#' # -> c("http://www.foo.com", "http://bar.org")
clean_urls <- .clean_urls


# ── Placeholder cleaner ──────────────────────────────────────────────
# Convert common placeholder strings ("NONE","N/A","UNK","TBD",...) to NA.
# Idempotent on already-clean character vectors.
.clean_placeholder <- function(x) {
  if (is.null(x) || !length(x)) return(x)
  if (!is.character(x)) return(x)
  bad <- toupper(trimws(x)) %in% c(
    "", "NONE", "N/A", "NA", "NULL", "TBD", "TBA", "UNK", "UNKNOWN",
    "-", ".", "—", "?", "??", "???", "X", "XX", "XXX", "PENDING",
    "NOT APPLICABLE", "NOT AVAILABLE", "NOT KNOWN", "NOT SPECIFIED"
  )
  x[bad] <- NA_character_
  x
}

#' Convert placeholder strings to \code{NA}
#'
#' Replaces common filer placeholders (\code{"NONE"}, \code{"N/A"},
#' \code{"TBD"}, \code{"UNKNOWN"}, dashes, dots, etc.) with \code{NA_character_}
#' so downstream null-handling works correctly.
#' @param x character vector
#' @return character vector with placeholders set to \code{NA}
#' @export
#' @examples
#' clean_placeholder(c("Foo", "N/A", "  TBD ", "Bar"))
#' # -> c("Foo", NA, NA, "Bar")
clean_placeholder <- .clean_placeholder


# ── Phone cleaner ────────────────────────────────────────────────────
# Normalize to E.164-ish format. Default country = US (+1) for 10-digit inputs.
.clean_phone <- function(x, default_cc = "1") {
  if (is.null(x) || !length(x)) return(character(0))
  x <- as.character(x)
  x <- .clean_placeholder(x)
  out <- vapply(x, function(s) {
    if (is.na(s)) return(NA_character_)
    s <- trimws(s)
    if (!nzchar(s)) return(NA_character_)
    # Reject obvious non-phone (email, url)
    if (grepl("@|://", s)) return(NA_character_)
    # Strip extension before digit extraction: "x123", "ext 4", "extension 5"
    s <- sub("(?i)(\\s+(x|ext\\.?|extension)\\.?\\s*\\d+).*$", "", s, perl = TRUE)
    has_plus <- grepl("^\\+", s)
    digits <- gsub("[^0-9]", "", s)
    n <- nchar(digits)
    if (!n) return(NA_character_)
    # Strip leading "1" for 11-digit US numbers if not explicit +
    if (!has_plus && n == 11L && substr(digits, 1, 1) == "1") {
      digits <- substr(digits, 2, 11); n <- 10L
    }
    # Length validation: real phones are 8-15 digits in E.164. Reject < 8.
    if (n < 8L || n > 15L) return(NA_character_)
    # 10-digit unprefixed → default cc (US)
    if (!has_plus && n == 10L) return(paste0("+", default_cc, digits))
    # Plus-prefixed → keep as-is (assume filer knew what they typed)
    if (has_plus) return(paste0("+", digits))
    # 11+ digits unprefixed and not US-style: don't fabricate a CC. Just emit +digits.
    paste0("+", digits)
  }, character(1), USE.NAMES = FALSE)
  out
}

#' Clean a vector of phone numbers
#'
#' Strips formatting, strips leading "1" on 11-digit US numbers, validates
#' length 7..15, returns E.164-ish \code{+CC#######} strings.
#' Empty/garbage/short values become \code{NA}. Default country code is "1".
#' @param x character vector
#' @param default_cc default country code (digits, no plus). Default \code{"1"}.
#' @return character vector
#' @export
#' @examples
#' clean_phone(c("(212) 583-5000", "212.583.5000", "1-212-583-5000",
#'              "+44 1534 754 502", "N/A", "x"))
#' # -> c("+12125835000", "+12125835000", "+12125835000", "+441534754502", NA, NA)
clean_phone <- .clean_phone


# ── Email cleaner ────────────────────────────────────────────────────
.clean_email <- function(x) {
  if (is.null(x) || !length(x)) return(character(0))
  x <- as.character(x)
  x <- .clean_placeholder(x)
  out <- vapply(x, function(s) {
    if (is.na(s)) return(NA_character_)
    s <- trimws(s)
    s <- gsub("[\\x00-\\x1f\\x7f]", "", s, perl = TRUE)
    s <- tolower(s)
    if (!grepl("^[a-z0-9._%+\\-]+@[a-z0-9.\\-]+\\.[a-z]{2,}$", s, perl = TRUE)) return(NA_character_)
    s
  }, character(1), USE.NAMES = FALSE)
  out
}

#' Clean a vector of email addresses
#'
#' Lowercases, trims whitespace and control chars, validates RFC-5322-lite
#' shape. Anything that fails validation becomes \code{NA}.
#' @param x character vector
#' @return character vector
#' @export
clean_email <- .clean_email


# ── Entity-name normalizer ───────────────────────────────────────────
# Mirrors the DuckDB normalize_entity_name MACRO (lock-stepped behavior so
# joins line up across SQL + parquet write paths). Strips legal suffixes,
# punctuation, "THE " prefix, collapses whitespace, uppercases.
.LEGAL_SUFFIX_RE <- paste0(
  "\\s+(",
  paste(c(
    "INCORPORATED", "INC", "LLC", "L\\.L\\.C", "LLP", "L\\.L\\.P", "LP",
    "LIMITED PARTNERSHIP", "LIMITED LIABILITY COMPANY", "PLLC", "PA",
    "P\\.A", "PC", "P\\.C", "CORPORATION", "CORP", "CO", "COMPANY",
    "LIMITED", "LTD", "GMBH", "AG", "SA", "S\\.A", "BV", "B\\.V",
    "PLC", "NV", "N\\.V", "OY", "AB", "SAS", "SRL", "PTY",
    "HOLDINGS", "GROUP", "TRUST"
  ), collapse = "|"),
  ")\\.?$"
)
.normalize_entity_name <- function(x) {
  if (is.null(x) || !length(x)) return(character(0))
  x <- as.character(x)
  x <- .clean_placeholder(x)
  out <- vapply(x, function(s) {
    if (is.na(s)) return(NA_character_)
    s <- toupper(trimws(s))
    if (!nzchar(s)) return(NA_character_)
    s <- sub("^THE\\s+", "", s)
    # Collapse common dotted legal-form abbreviations BEFORE general punctuation strip
    s <- gsub("\\bL\\.\\s*L\\.\\s*C\\.?", "LLC", s, perl = TRUE)
    s <- gsub("\\bL\\.\\s*L\\.\\s*P\\.?", "LLP", s, perl = TRUE)
    s <- gsub("\\bP\\.\\s*L\\.\\s*C\\.?", "PLC", s, perl = TRUE)
    s <- gsub("\\bP\\.\\s*A\\.?",         "PA",  s, perl = TRUE)
    s <- gsub("\\bP\\.\\s*C\\.?",         "PC",  s, perl = TRUE)
    s <- gsub("\\bB\\.\\s*V\\.?",         "BV",  s, perl = TRUE)
    s <- gsub("\\bN\\.\\s*V\\.?",         "NV",  s, perl = TRUE)
    s <- gsub("\\bS\\.\\s*A\\.?",         "SA",  s, perl = TRUE)
    s <- gsub("\\bS\\.\\s*R\\.\\s*L\\.?", "SRL", s, perl = TRUE)
    s <- gsub("\\bA\\.\\s*G\\.?",         "AG",  s, perl = TRUE)
    s <- gsub("[,.&]", " ", s)         # strip light punctuation
    s <- gsub("[^A-Z0-9 ]", "", s)     # nuke other punctuation
    s <- gsub("\\s+", " ", s)          # collapse whitespace
    s <- trimws(s)
    # Strip leading legal-form (e.g., "B.V. Acme Holdings" → "ACME")
    s <- sub("^(BV|NV|SA|AG|GMBH|LLC|LLP|LP|PLC|SARL|SRL|OY|AB|PTY|HOLDINGS)\\s+", "", s)
    # Strip up to 3 trailing legal-suffix occurrences (e.g., "FOO HOLDINGS LLC")
    for (i in 1:3) {
      prev <- s
      s <- sub(.LEGAL_SUFFIX_RE, "", s, perl = TRUE)
      s <- trimws(s)
      if (identical(s, prev)) break
    }
    if (!nzchar(s)) return(NA_character_)
    s
  }, character(1), USE.NAMES = FALSE)
  out
}

#' Normalize an entity legal name
#'
#' Mirrors the DuckDB \code{normalize_entity_name()} MACRO so that joins line
#' up across SQL and parquet-write paths. Uppercases, strips \code{"THE "}
#' prefix, drops legal suffixes (LLC, INC, CORP, GMBH, SA, BV, etc.) up to
#' three deep, collapses whitespace.
#' @param x character vector
#' @return character vector
#' @export
#' @examples
#' normalize_entity_name(c("The Foo Inc.", "FOO INC", "Foo Holdings, LLC",
#'                          "BAR GMBH", "n/a"))
#' # -> c("FOO", "FOO", "FOO", "BAR", NA)
normalize_entity_name <- .normalize_entity_name
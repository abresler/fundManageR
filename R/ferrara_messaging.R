# ferrara_messaging.R
# -----------------------------------------------------------------------------
# Abel Ferrara-Inspired Console Messaging System
# -----------------------------------------------------------------------------
#
# Design Philosophy:
# Inspired by the visual aesthetic of Abel Ferrara's films - the neon-soaked
# streets of "King of New York", the moral shadows of "Bad Lieutenant", and
# the existential darkness of "The Addiction". This messaging system brings
# that noir atmosphere to your R console.
#
# The Ferrara Palette:
#   - Neon Magenta (#FF1493)  : Headlines, emphasis - Times Square sleaze
#   - Electric Cyan (#00D4FF) : Info, data counts - surveillance cold
#   - Amber Gold (#FFB347)    : Success, completion - streetlight glow
#   - Blood Crimson (#DC143C) : Warnings, alerts - violence undertones
#   - Deep Purple (#9932CC)   : Errors, failures - corruption, surreal
#   - Stark White             : Values, numbers - honest, clinical
#   - Smoke Gray              : Subtitles, secondary info - fog, ambiguity
#
# -----------------------------------------------------------------------------

#' @import cli
#' @keywords internal

# Define the Ferrara theme for cli
.ferrara_theme <- list(
  # Headlines - Neon magenta, bold
  span.fm_headline = list(
    color = "magenta",
    "font-weight" = "bold"
  ),
  # Success messages - Amber/gold

  span.fm_success = list(
    color = "yellow"
  ),
  # Info/counts - Cyan
  span.fm_info = list(
    color = "cyan"
  ),
  # Warnings - Red
  span.fm_warning = list(
    color = "red"
  ),

  # Values/numbers - White bold
  span.fm_value = list(
    color = "white",
    "font-weight" = "bold"
  ),
  # Entity names - Bright magenta
  span.fm_entity = list(
    color = "magenta",
    "font-style" = "italic"
  ),
  # URLs - Cyan underline
  span.fm_url = list(
    color = "cyan",
    "font-style" = "italic"
  ),
 # Subtle/secondary - Dim
  span.fm_subtle = list(
    color = "grey"
  ),
  # Dividers
  span.fm_divider = list(
    color = "blue"
  )
)

#' Apply Ferrara theme
#' @keywords internal
.with_ferrara_theme <- function(expr) {
  cli::cli_div(theme = .ferrara_theme)
  on.exit(cli::cli_end(), add = TRUE)
  force(expr)
}

# =============================================================================
# Core Messaging Functions
# =============================================================================

#' Styled success message - The Deal Closes
#'
#' @param ... Message components (glue-style interpolation supported)
#' @keywords internal
#' @examples
#' \dontrun{
#' .fm_success("Acquired {.fm_value 1500} records from {.fm_entity SEC EDGAR}")
#' }
.fm_success <- function(...) {
  .with_ferrara_theme({
    cli::cli_alert_success(...)
  })
}

#' Styled info message - The Surveillance
#'
#' @param ... Message components
#' @keywords internal
.fm_info <- function(...) {
  .with_ferrara_theme({
    cli::cli_alert_info(...)
  })
}

#' Styled warning message - The Danger
#'
#' @param ... Message components
#' @keywords internal
.fm_warning <- function(...) {
  .with_ferrara_theme({
    cli::cli_alert_warning(...)
  })
}

#' Styled danger/error message - The Fall
#'
#' @param ... Message components
#' @keywords internal
.fm_danger <- function(...) {
  .with_ferrara_theme({
    cli::cli_alert_danger(...)
  })
}

#' Styled headline - The Title Card
#'
#' @param ... Message components
#' @keywords internal
.fm_headline <- function(...) {
  .with_ferrara_theme({
    cli::cli_h1(...)
  })
}

#' Styled subheadline
#'
#' @param ... Message components
#' @keywords internal
.fm_subhead <- function(...) {
  .with_ferrara_theme({
    cli::cli_h2(...)
  })
}

#' Styled bullet point
#'
#' @param ... Message components
#' @keywords internal
.fm_bullet <- function(...) {
  .with_ferrara_theme({
    cli::cli_li(...)
  })
}

#' Begin a styled bullet list
#'
#' @keywords internal
.fm_bullets_start <- function() {
  .with_ferrara_theme({
    cli::cli_ul()
  })
}

#' End a styled bullet list
#'
#' @keywords internal
.fm_bullets_end <- function() {
  cli::cli_end()
}

# =============================================================================
# Compound Message Builders
# =============================================================================

#' Data acquisition success message
#'
#' Standard message for successful data retrieval operations
#'
#' @param n_rows Number of rows acquired
#' @param source Data source name
#' @param entity Optional entity name
#' @param extra Optional extra info
#' @keywords internal
.fm_data_acquired <- function(n_rows, source, entity = NULL, extra = NULL) {
  n_formatted <- format(n_rows, big.mark = ",", scientific = FALSE)

  .with_ferrara_theme({
    if (!is.null(entity)) {
      cli::cli_alert_success(
        "Acquired {.fm_value {n_formatted}} records from {.fm_entity {source}} for {.fm_entity {entity}}"
      )
    } else {
      cli::cli_alert_success(
        "Acquired {.fm_value {n_formatted}} records from {.fm_entity {source}}"
      )
    }
    if (!is.null(extra)) {
      cli::cli_alert_info("{.fm_subtle {extra}}")
    }
  })
}

#' Parsing progress message
#'
#' @param url URL being parsed
#' @keywords internal
.fm_parsing <- function(url) {
  .with_ferrara_theme({
    cli::cli_alert_info("Parsing {.fm_url {url}}")
  })
}

#' Entity summary message
#'
#' For functions that return entity data with highlights
#'
#' @param entity_type Type of entity (company, fund, etc.)
#' @param name Entity name
#' @param highlights Named list of key stats to display
#' @keywords internal
.fm_entity_summary <- function(entity_type, name, highlights = list()) {
  .with_ferrara_theme({
    cli::cli_h2("{entity_type}: {.fm_entity {name}}")
    if (length(highlights) > 0) {
      cli::cli_ul()
      for (nm in names(highlights)) {
        cli::cli_li("{nm}: {.fm_value {highlights[[nm]]}}")
      }
      cli::cli_end()
    }
  })
}

#' Random spotlight message (for YC, unicorns, etc.)
#'
#' @param company Company name
#' @param description Company description
#' @param url Optional URL
#' @param extra Named list of additional fields
#' @keywords internal
.fm_spotlight <- function(company, description = NULL, url = NULL, extra = list()) {
  .with_ferrara_theme({
    cli::cli_rule(left = "SPOTLIGHT", right = cli::symbol$star)
    cli::cli_text("{.fm_headline {company}}")
    if (!is.null(description) && !is.na(description)) {
      cli::cli_text("{.fm_subtle {description}}")
    }
    if (length(extra) > 0) {
      for (nm in names(extra)) {
        if (!is.null(extra[[nm]]) && !is.na(extra[[nm]])) {
          cli::cli_text("{.fm_info {nm}}: {.fm_value {extra[[nm]]}}")
        }
      }
    }
    if (!is.null(url) && !is.na(url)) {
      cli::cli_text("{.fm_url {url}}")
    }
    cli::cli_rule()
  })
}

#' Financial summary message
#'
#' @param metric_name Name of the financial metric
#' @param value The value (will be formatted)
#' @param is_currency Whether to format as currency
#' @param is_percent Whether to format as percentage
#' @keywords internal
.fm_financial_result <- function(metric_name, value, is_currency = FALSE, is_percent = FALSE) {
  if (is_currency) {
    formatted <- paste0("$", format(value, big.mark = ",", scientific = FALSE))
  } else if (is_percent) {
    formatted <- paste0(round(value * 100, 2), "%")
  } else {
    formatted <- format(value, big.mark = ",", scientific = FALSE)
  }

  .with_ferrara_theme({
    cli::cli_alert_success("{metric_name}: {.fm_value {formatted}}")
  })
}

#' Section divider - The Scene Break
#'
#' @param title Optional section title
#' @keywords internal
.fm_divider <- function(title = NULL) {
  .with_ferrara_theme({
    if (!is.null(title)) {
      cli::cli_rule(left = title)
    } else {
      cli::cli_rule()
    }
  })
}

#' Progress indicator for multi-step operations
#'
#' @param current Current step
#' @param total Total steps
#' @param message Step description
#' @keywords internal
.fm_progress_step <- function(current, total, message) {
  .with_ferrara_theme({
    cli::cli_alert_info("[{.fm_value {current}}/{.fm_value {total}}] {message}")
  })
}

#' Time range message
#'
#' @param start_year Start year
#' @param end_year End year
#' @param n_records Number of records
#' @param data_type Type of data
#' @keywords internal
.fm_time_range <- function(start_year, end_year, n_records, data_type = "records") {
  n_formatted <- format(n_records, big.mark = ",", scientific = FALSE)
  .with_ferrara_theme({
    cli::cli_alert_success(
      "Acquired {.fm_value {n_formatted}} {data_type} spanning {.fm_value {start_year}} to {.fm_value {end_year}}"
    )
  })
}

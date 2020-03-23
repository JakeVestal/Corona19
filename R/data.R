#' @importFrom rlang .data
region_names_map <- function()(
  paste0(
    "@details Using a region map:\n\n",
    "\\tabular{",
    readLines(file.path("data-raw", "refresh_Corona19.R")) %>% {
      tibble::tibble(
        "data" = .[(
          which(grepl("region,$", .)) + 1
        ):(
          which(grepl("^dplyr::distinct_at\\(", trimws(.))) - 3
        )] %>%
          gsub(",", "", .) %>%
          gsub("`", "", .) %>%
          gsub("\"", "", .) 
      )
    } %>% 
      tidyr::separate(
        col = "data", into = c("Aliases", "Basename"), sep = "="
      ) %>%
      dplyr::mutate(
        "Basename" = trimws(.data$Basename),
        "Aliases" = trimws(.data$Aliases)
      ) %>%
      dplyr::group_by(.data$Basename) %>%
      dplyr::summarize(
        "Aliases"  = paste0("\"", paste(.data$Aliases, collapse = "\", \""), "\"\\cr")
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        "Basename" = paste0("\\code{", .data$Basename, "}: "),
      ) %>% 
      tidyr::unite(col = "data", sep = "\\tab ") %>%
      {
        paste0(
          paste(rep("l", nrow(.) + 1), collapse = ""),
          "}{",
          "\\strong{Region}: \\tab \\strong{Includes data from}: \\cr",
          paste(unlist(.), collapse = "")
        )
      },
    "}"
  )
)
#' Corona Data
#' 
#' @description 
#' \code{corona_data} contains all of the data found in JHU's
#' \href{https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_daily_reports}{\strong{CSSE
#' COVID-19 Daily Reports}} csv files in a single user-friendly \link[tibble]{tibble} having 8 columns:
#' 
#' \tabular{lllllll}{
#'   \strong{date}: Date for which the data apply. Derived from the filename of
#'   the csv file from which the data in the rest of the columns is drawn.\cr
#'   \strong{area}: Same thing as \emph{province/state} in the original JHU
#'   data, renamed so as to get rid of the awkward "/".\cr
#'   \strong{region}: Same thing as \emph{country/region}, renamed for the same
#'   reason as "\emph{area}".\cr
#'   \strong{confirmed}, \strong{recovered}, \strong{deaths}: As (albiet not
#'   very precisely) defined in the
#'   \href{https://github.com/CSSEGISandData/COVID-19/blob/5f3ba461a5a7ed5f72104e499804e31ca594b0b6/csse_covid_19_data/README.md}{JHU
#'   data}.\cr
#'   \strong{last_update}: Date-time stamp denoting when the information in the
#'   row was last created or modified.\cr
#'   \strong{latitude}, \strong{longitude}: Global co-ordinates, supposedly for
#'   the area? Not available for every row.
#' }
#' 
#' @eval region_names_map()
#' @name corona_data
#' 
NULL

#' IVV Historical Data
#' 
#' Historical data (high, low, open, close, volume) and dividends for iShares
#' Core S&P500 ETF. To be used as a proxy for the S&P500 index itself.
#'
#' @name ivv
#' 
NULL

#' Country Codes
#' 
#' A \link[tibble]{tibble} of country names and their abbreviations.
#'
#' @name country_codes
#' 
NULL


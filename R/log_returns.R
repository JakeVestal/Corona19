#' Daily Log Returns
#'
#' Get log returns between two dates for a dividend-paying asset
#'
#' @param asset
#' A list containing prices, dividends and splits (if any)
#'
#' @param price1
#' Price of the FIRST day in time for which you wish to calculate log returns
#'
#' @param price2
#' Price of the FINAL day in time for which you wish to calculate log returns
#'
#' @export
#'
log_returns <- function(asset, price1, price2){
  
  price1 <- match.arg(price1, choices = c("High", "Low", "Open", "Close"))
  price2 <- match.arg(price2, choices = c("High", "Low", "Open", "Close"))
  
  merge(
    asset$price_history,
    asset$div_history[,c("ExDate", "DividendAmount")] %>%
      dplyr::group_by(.data$ExDate) %>%
      dplyr::summarize(
        "DividendAmount" = sum(.data$DividendAmount)
      ) %>%
      dplyr::ungroup(),
    by.x = "Date",
    by.y = "ExDate",
    all  = TRUE
  )[,unique(c("Date", price1, price2, "DividendAmount"))] %>%
    dplyr::arrange(.data$Date) %>% {
      
      numerator <- .[-1, c("Date", price2, "DividendAmount")]
      
      xts::xts(
        log(
          (
            numerator[price2] +
              dplyr::recode(numerator$DividendAmount, .missing = 0)
          ) /
            .[price1][-nrow(.),]
        ),
        order.by = as.Date(numerator$Date)
      ) %>%
        magrittr::set_colnames("daily_log_rtn")
      
    }
  
}

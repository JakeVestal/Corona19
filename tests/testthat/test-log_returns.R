test_that("random dividend date works", {
  
  div_date <- sample(ivv$div_history$ExDate, 1)
  price_1   <- sample(c("High", "Low", "Open", "Close"), 1)
  price_2   <- sample(c("High", "Low", "Open", "Close"), 1)
  
  match(div_date, ivv$price_history$Date) %>% {
    expect_equal(
      log(
        as.numeric(
          ivv$price_history[., price_2] + ivv$div_history[
            which(ivv$div_history$ExDate == div_date), 
            "DividendAmount"
            ]
        ) / as.numeric(
          ivv$price_history[. + 1, price_1]
        )
      ),
      as.numeric(log_returns(ivv, price_1, price_2)[as.Date(div_date)])
    )
  }
  
})

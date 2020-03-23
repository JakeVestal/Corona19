
vapply(
  unique(corona_data$region),
  function(region){
    tolower(region) %in% tolower(country_codes$Country)
  },
  logical(1)
) %>%
  expect(
    ok = all(.),
    failure_message = paste(names(which(!.)), collapse = ", ")
  )


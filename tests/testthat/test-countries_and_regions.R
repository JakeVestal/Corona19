
vapply(
  unique(corona_data$region),
  function(region){
    tolower(region) %in% tolower(country_codes$Country)
  },
  logical(1)
) %>% {
  expect(
    ok = all(.),
    failure_message = paste0(
      "These regions appear in ",
      crayon::italic("corona_data"),
      " but not in ",
      crayon::italic("country_codes"),
      ":\n",
      paste(crayon::bold(names(which(!.))), collapse = ", "),
      "."
    )
  )
  invisible()
}

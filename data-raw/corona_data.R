## code to prepare `generate_corona_data` dataset goes here

corona_data <- list.files(
  file.path("inst", "csse_covid_19_data", "csse_covid_19_daily_reports"),
  full.names = TRUE,
  pattern = "(.*)\\.csv$"
) %>%
  stats::setNames(., gsub("(.*)\\.csv$", "\\1", basename(.))) %>%
  purrr::imap(
    function(file_path, cdata_date){
      readr::read_csv(file_path) %>%
        magrittr::set_colnames(tolower(colnames(.))) %>%
        dplyr::select(-`last update`) %>%
        dplyr::mutate(
          confirmed = tidyr::replace_na(confirmed, 0),
          deaths    = tidyr::replace_na(deaths, 0),
          recovered = tidyr::replace_na(recovered, 0),
          date      = lubridate::mdy(cdata_date)
        )
    }
  ) %>%
  purrr::reduce(dplyr::bind_rows) %>%
  dplyr::rename(area = `province/state`, region = `country/region`) %>%
  dplyr::distinct_at(dplyr::vars(area, region, date), .keep_all = TRUE)

usethis::use_data(corona_data, overwrite = TRUE)

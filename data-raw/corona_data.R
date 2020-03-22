## code to prepare `generate_corona_data` dataset goes here

library(magrittr)

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
  dplyr::mutate(
    region = dplyr::recode(
      region,
      `Mainland China`             = "China",
      `The Bahamas`                = "Bahamas",
      `Bahamas, The`               = "Bahamas",
      `Congo (Brazzaville)`        = "Congo",
      `Congo (Kinshasa)`           = "Congo",
      `The Gambia`                 = "Gambia",
      `Gambia, The`                = "Gambia",
      `Hong Kong`                  = "Hong Kong SAR",
      `Iran (Islamic Republic of)` = "Iran",
      `Korea, South`               = "South Korea",
      `Macao SAR`                  = "Macao",
      `Republic of Ireland`        = "Ireland",
      `Republic of Korea`          = "South Korea",
      `Republic of Moldova`        = "Moldova",
      `Republic of the Congo`      = "Congo",
      `Russian Federation`         = "Russia",
      `Taipei and environs`        = "Taiwan",
      `Taiwan*`                    = "Taiwan",
      `Viet Nam`                   = "Vietnam"
    ) 
  ) %>%
  dplyr::distinct_at(dplyr::vars(area, region, date), .keep_all = TRUE) 

usethis::use_data(corona_data, overwrite = TRUE)

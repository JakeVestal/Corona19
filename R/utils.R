#' Refresh corona_data
refresh_corona_data <- function(){
  
  print(paste0("Active Project: ", usethis::proj_get()))
  print(paste0("WD: )", getwd()))
  
  corona_data <-  list.files(
    file.path(
      "inst", "JHU_Data", "csse_covid_19_data", "csse_covid_19_daily_reports"
    ),
    full.names = TRUE,
    pattern = "(.*)\\.csv$"
  ) %>%
    stats::setNames(., gsub("(.*)\\.csv$", "\\1", basename(.))) %>%
    purrr::imap(
      function(file_path, cdata_date){
        readr::read_csv(file_path) %>%
          magrittr::set_colnames(tolower(colnames(.))) %>% 
          dplyr::mutate(
            "confirmed"   = tidyr::replace_na(.$confirmed, 0),
            "deaths"      = tidyr::replace_na(.$deaths, 0),
            "recovered"   = tidyr::replace_na(.$recovered, 0),
            "last_update" = tryCatch(
              lubridate::as_datetime(.$`last update`),
              warning = function(w){
                lubridate::mdy_hm(.$`last update`)
              }
            ),
            "date"        = lubridate::mdy(cdata_date)
          ) %>%
          dplyr::select(-.$`last update`)
      }
    ) %>%
    purrr::reduce(dplyr::bind_rows) %>%
    dplyr::rename(area = .$`province/state`, region = .$`country/region`) %>%
    dplyr::mutate(
      region = dplyr::recode(
        .$region,
        `Mainland China`             = "China",
        `The Bahamas`                = "Bahamas",
        `Bahamas, The`               = "Bahamas",
        `Congo (Brazzaville)`        = "Congo",
        `Congo (Kinshasa)`           = "Congo",
        `The Gambia`                 = "Gambia",
        `Gambia, The`                = "Gambia",
        `Hong Kong SAR`              = "Hong Kong",
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
    dplyr::distinct_at(
      dplyr::vars(.$area, .$region, .$date), 
      .keep_all = TRUE
    ) %>%
    dplyr::select(.$date, dplyr::everything())
  
  usethis::use_data(corona_data, overwrite = TRUE)
  
}

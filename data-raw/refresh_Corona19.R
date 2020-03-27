print(paste0("Active Project: ", usethis::proj_get()))
print(paste0("WD: )", getwd()))

corona_data <- list.files(
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
          "last update" = tryCatch(
            lubridate::as_datetime(.$`last update`),
            warning = function(w){
              lubridate::mdy_hm(.$`last update`)
            }
          ),
          "date"        = lubridate::mdy(cdata_date)
        )
    }
  ) %>%
  purrr::reduce(dplyr::bind_rows) %>%
  dplyr::rename(
    area        = "province/state",
    region      = "country/region",
    last_update = "last update"
  ) %>%
  dplyr::mutate(
    region = dplyr::recode(
      .$region,
      `Mainland China`             = "China",
      `The Bahamas`                = "Bahamas",
      `Bahamas, The`               = "Bahamas",
      `Congo (Brazzaville)`        = "Congo",
      `Congo (Kinshasa)`           = "Congo",
      `Cote d'Ivoire`              = "Ivory Coast",
      `Czech Republic`             = "Czechia",
      `Holy See`                   = "Vatican City",
      `The Gambia`                 = "Gambia",
      `Gambia, The`                = "Gambia",
      `Hong Kong SAR`              = "Hong Kong",
      `Iran (Islamic Republic of)` = "Iran",
      `Korea, South`               = "South Korea",
      `Kosovo`                     = "Serbia",
      `Macao SAR`                  = "Macao",
      `Macau`                      = "Macao",
      `North Ireland`              = "United Kingdom",
      `occupied Palestinian territory` = "Palestine",
      `Republic of Ireland`        = "Ireland",
      `Republic of Korea`          = "South Korea",
      `Republic of Moldova`        = "Moldova",
      `Republic of the Congo`      = "Congo",
      `Russian Federation`         = "Russia",
      `St. Martin`                 = "Saint Martin",
      `Taipei and environs`        = "Taiwan",
      `Taiwan*`                    = "Taiwan",
      `UK`                         = "United Kingdom",
      `US`                         = "United States",
      `Viet Nam`                   = "Vietnam"
    )
  ) %>%
  dplyr::distinct_at(
    dplyr::vars("area", "region", "date"),
    .keep_all = TRUE
  ) %>%
  dplyr::select("date", dplyr::everything())

usethis::use_data(corona_data, overwrite = TRUE)

country_codes <- xml2::read_html("https://www.iban.com/country-codes") %>%
  rvest::html_nodes("#myTable") %>%
  rvest::html_table() %>%
  .[[1]] %>%
  tibble::as_tibble() %>%
  dplyr::mutate(
    Country = dplyr::recode(
      .$Country,
      `Macau`                          = "Macao",
      `Taiwan (Province of China)`     = "Taiwan",
      `United States of America (the)` = "United States",
      `Korea (the Republic of)`        = "South Korea",
      `Philippines (the)`              = "Philippines",
      `Russian Federation (the)`       = "Russia",
      `Viet Nam`                       = "Vietnam",
      `Côte d'Ivoire`                  = "Ivory Coast",
      `United Arab Emirates (the)`     = "United Arab Emirates",
      `United Kingdom of Great Britain and Northern Ireland (the)` = "United Kingdom",
      `Iran (Islamic Republic of)`     = "Iran",
      `Republic of North Macedonia`    = "North Macedonia",
      `Dominican Republic (the)`       = "Dominican Republic",
      `Saint Barthélemy`               = "Saint Barthelemy",
      `Faroe Islands (the)`            = "Faroe Islands",
      `Palestine, State of`            = "Palestine",
      `Saint Martin (French part)`     = "Saint Martin",
      `Congo (the Democratic Republic of the)` = "Congo",
      `Netherlands (the)`              = "Netherlands",
      `Holy See (the)`                 = "Vatican City",
      `Réunion`                        = "Reunion",
      `Cayman Islands (the)`           = "Cayman Islands",
      `Sudan (the)`                    = "Sudan",
      `Venezuela (Bolivarian Republic of)` = "Venezuela",
      `Curaçao`                        = "Curacao",
      `Central African Republic (the)` = "Central African Republic",
      `Tanzania, United Republic of`   = "Tanzania",
      `Bahamas (the)`                  = "Bahamas",
      `Gambia (the)`                   = "Gambia",
      `Niger (the)`                    = "Niger",
      `Moldova (the Republic of)`      = "Moldova",
      `Brunei Darussalam`              = "Brunei",
      `Bolivia (Plurinational State of)` = "Bolivia"
    )
  ) %>%
  dplyr::bind_rows(
    tibble::tibble(
      "Country" = c("Cruise Ship", "Others", "Channel Islands")
    )
  ) %>%
  dplyr::mutate(
    "flag_url" = paste0(
      "https://cdn.rawgit.com/lipis/flag-icon-css/master/flags/4x3/",
      tolower(.$`Alpha-2 code`),
      ".svg"
    )
  )

usethis::use_data(country_codes, overwrite = TRUE)

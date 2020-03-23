# Corona19
# By Jake Vestal

library(Corona19)
library(magrittr)
library(shiny)
library(shinyWidgets)
library(dygraphs)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Corona 19"),
    
    fluidRow(
        sort(unlist(unique(corona_data$region))) %>% {
            multiInput(
                inputId = "regions",
                label = "Countries :", 
                choices = NULL,
                width = "500px",
                choiceNames = lapply(
                    .,
                    function(region){
                        print(region)
                        HTML(
                            paste(
                                tags$img(
                                    src = country_codes %>% 
                                        dplyr::filter(Country == region) %>%
                                        dplyr::select(flag_url) %>%
                                        unlist(use.names = FALSE),
                                    width = 20, 
                                    height = 15
                                ), 
                                region
                            )
                        )
                    }
                ),
                choiceValues = .
            )
        }
    ),
    dygraphOutput("region_time_series")
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$region_time_series <- renderDygraph({
        
        validate(
            need(input$regions != "", "Please select one or more regions")
        )
        
        dygraph(
            corona_data %>%
                dplyr::filter(region %in% input$regions) %>%
                dplyr::select(confirmed, deaths, recovered, date, region) %>%
                dplyr::group_by(date, region) %>%
                dplyr::summarise_all(sum) %>% 
                dplyr::ungroup() %>%
                dplyr::transmute(
                    "date"   = date,
                    "region" = region,
                    "value"  = (confirmed - deaths - recovered) / 
                        (deaths + recovered)
                ) %>%
                tidyr::pivot_wider(names_from = region, values_from = value) %>%
                tibble::column_to_rownames("date") %>%
                xts::as.xts()
        ) %>%
            dyRangeSelector()
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

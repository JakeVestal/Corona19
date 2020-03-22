# Corona19
# By Jake Vestal

library(shiny)
library(Corona19)
library(dygraphs)
library(magrittr)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Corona 19"),
    
    fluidRow(
        multiInput(
            inputId = "regions",
            label = "Regions", 
            choices = sort(unlist(unique(corona_data$region))),
            options = list(
                `live-search` = TRUE,
                title = "Select a region"
            )
        )
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

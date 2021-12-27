#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(here)
library(tidyverse)
library(magrittr)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Mitch's Rarest Birds"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("year",
                        "Year",
                        min = 2018,
                        max = 2021,
                        value = 2018)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           leafletOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderLeaflet({
        
        ebird_singles <- read.csv(here::here("./shiny_dashboards/db1/MyEBirdData.csv"),
                          header=T, na.strings=c("","NA")) %>%
            janitor::clean_names() %>%
            dplyr::mutate(count = na_if(count, "X")) %>%
            group_by(scientific_name) %>%
            summarise(n = n(),
                      common_name = first(common_name),
                      location = first(location),
                      lat = first(latitude),
                      long = first(longitude),
                      date = first(date),
                      comments = first(checklist_comments),
                      details = first(observation_details)) %>%
            filter(n == 1) %>%
            # remove some observations only at the genus level
            filter(!str_detect(scientific_name, 'sp.')) %>%
            filter(!str_detect(scientific_name, 'Group')) %>%
            dplyr::mutate(year = substr(date, 1, 4))
        
        pal <- colorFactor(palette = 'Dark2',
                           domain = ebird_singles$year)
       
        year <- c(2018, 2019, 2020, 2021)
        
        leaflet(ebird_singles %>%
                    filter(year == input$year)) %>% 
            addTiles(options = providerTileOptions(opacity = 0.55)) %>%
            addCircles(lng = ~long,
                       lat = ~lat,
                       radius = 200,
                       popup = paste0("Scientific name: ",
                                      # italicize
                                      "<i>",
                                      ebird_singles$scientific_name,
                                      "</i>",
                                      ".",
                                      "<br>", # line break
                                      "Common name: ",
                                      ebird_singles$common_name, 
                                      ".",
                                      "<br>", # line break
                                      "Location: ",
                                      ebird_singles$location,
                                      "."),
                       fill = T,
                       fillOpacity = 0.8,
                       color = ~pal(year)
            )  %>%
            addLegend("bottomleft",
                      pal = pal,
                      values = ebird_singles$year,
                      opacity = 0.8)
        
        
        
        # generate bins based on input$bins from ui.R
        # x    <- faithful[, 2]
        # bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        # hist(x, breaks = bins)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

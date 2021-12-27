# load libraries-----
library(shinydashboard)
library(here)
library(tidyverse)
library(janitor)
library(leaflet)


# import data-----
ebird <- read.csv(here::here("./shiny_dashboards/db1/MyEBirdData.csv"),
                  header=T, na.strings=c("","NA")) %>%
  
  # clean it
  # get rid of spaces, slashes, other bad stuff in colnames
  janitor::clean_names() %>%
  # convert "X"s to NAs in bird counts
  dplyr::mutate(count = na_if(count, "X"))


# do some basic data exploration-----

########################################################
############## MITCH'S TOP 20 BIRDS ####################
########################################################

ebird_top20 <- ebird %>%
  group_by(scientific_name, common_name) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(20)

ggplot(ebird_top20,
       aes(
         x = reorder(common_name, -count),
         y = count)) +
  # first time so that x axis stays sorted by size
  geom_point(aes(color = reorder(common_name, -count)),
             size = 5) +
  geom_segment( aes(x=common_name,
                    xend=common_name,
                    y=0,
                    yend=count),
                color="skyblue",
                size = 1) +
  # second time so that the points are in front of segments
  geom_point(aes(color = reorder(common_name, -count)),
             size = 5) +
  theme_light() +
  ylim(0, NA) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 35,
                                   vjust = 1.1,
                                   hjust= 1,
                                   size = 12),
        panel.grid.major.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.border = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        plot.title = element_text(size=16,
                                  hjust = 0.5)) +
  labs(x = "Bird Species",
       y = "Count",
       title = "Mitch's 20 Most Spotted Birds",
       subtitle = "When Mitch goes birding, he is most likely to see these species.")


########################################################
############## MITCH'S RAREST BIRDS ####################
########################################################

ebird_singles <- ebird %>%
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




# Create a palette that maps factor levels to colors
pal <- colorFactor(palette = 'Dark2',
                   domain = ebird_singles$year)

leaflet(ebird_singles %>%
          filter(year = input$slider)) %>% 
  addTiles(options = providerTileOptions(opacity = 0.55)) %>%
  # addProviderTiles(providers$CartoDB.Positron,
  #                  options = providerTileOptions(opacity = 0.55)) %>%
  # fitBounds(-79.6,44,-79.2,43.5) %>% 
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



########################################################
############## MITCH'S COMMENTS: MAP ###################
########################################################

ebird_comments <- ebird %>%
  filter(is.na(observation_details) == F)


leaflet(ebird_comments) %>% 
  # addTiles(options = providerTileOptions(opacity = 0.55)) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  
  #                  options = providerTileOptions(opacity = 0.55)) %>%
  # fitBounds(-79.6,44,-79.2,43.5) %>% 
  addCircles(lng = ~longitude,
             lat = ~latitude,
             radius = 20,
             popup = paste0("Scientific name: ",
                            # italicize
                            "<i>",
                            ebird_comments$scientific_name,
                            "</i>",
                            ".",
                            "<br>", # line break
                            "Common name: ",
                            ebird_comments$common_name, 
                            ".",
                            "<br>", # line break
                            "Location: ",
                            ebird_comments$location,
                            ".",
                            "<br>", # line break
                            "Date: ",
                            ebird_comments$date,
                            ".",
                            "<br>", # line break
                            "Comment: ",
                            ebird_comments$observation_details,
                            "."),
             fill = T,
             fillOpacity = 0.8
  ) 

########################################################
############## MITCH'S COMMENTS: WORDCLOUD #############
########################################################

library(wordcloud)

comments1 <- as.vector(ebird_comments$observation_details) %>%
  paste(collapse=" ") %>%
  strsplit(" ") %>%
  unlist()

frqs <- sample(seq(0,1,0.01), length(comments1), replace=TRUE) 

wordcloud(comments1,
          frqs,
          col=terrain.colors(length(comments1), alpha=0.9),
          rot.per=0.3)


# make ui-----
ui <- dashboardPage(
  dashboardHeader(
    title = "Mitch's eBird Data Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard"),
      menuItem("Widgets", tabName = "widgets")
      )
    ),
  dashboardBody(
    tabItems(
     
       # First tab content
      tabItem(tabName = "dashboard",
              fluidRow(
                box(plotOutput("plot1", height = 250)),
                box(title = "Controls",
                    sliderInput("slider",
                                "Year:",
                                2018, 2021, 1)
                    )
                )
              ),
      
      # Second tab content
      tabItem(tabName = "widgets",
              h2("Widgets tab content")
              )
      )
    )
  )



# make server-----
server <- function(input, output) {

  output$plot1 <- renderPlot({
    leaflet(ebird_singles %>%
              filter(year == input$slider)) %>% 
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
  })
}


# call it-----
shinyApp(ui, server)

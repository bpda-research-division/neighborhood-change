library(dplyr)
library(shiny)
library(leaflet)
library(tidycensus)
library(RColorBrewer)

census_api_key("3910e99aea0a472b50f5cdc422c9a3395b3c87b3")

#v21 <- load_variables(2021, "acs5/subject", cache = TRUE)

my_states = c("MA")
my_vars <- c(
  total_households = "S1901_C01_001"
  , median_household_income = "S1901_C01_012"
)

cs <- get_acs(
  geography = "tract",
  variables = my_vars,
  state = my_states,
  county = "025",
  year = 2021,
  survey="acs5",
  output = "wide",
  geometry = TRUE,
  cache_table = TRUE
)
df <- cs %>% sf::st_transform(4326)

ui <- fluidPage(
  titlePanel(h1("Neighborhood Change Dashboard", align = "center")),
  sidebarLayout(
    sidebarPanel(
      sliderInput("range", "Magnitudes", min(quakes$mag), max(quakes$mag),
                  value = range(quakes$mag), step = 0.1
      ),
      leafletOutput("map")
      , width=6 # will probably go for 6 on the slider + map side...
    ),
    mainPanel(
      selectInput("colors", "Color Scheme",
                  rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
      ),
      checkboxInput("legend", "Show legend", TRUE)
      , width = 6 # and 6 on the bar + line side
    )
  )
)
server <- function(input, output, session) {
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    df
  })
  # TODO: try creating a reactive expression using a hash table: ht <- new.env(hash=TRUE) => ht[[key]] <- df
  # however, we'd need to think about whether to filter or hash if we got to the point of custom time steps
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, df$median_household_incomeE)
  })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    # leaflet(quakes, width="100%", height="100%") %>% addTiles() %>%
    #   fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
    
    leaflet(df, height = "100%", width = "100%") %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      addPolygons(
        stroke = F,
        smoothFactor = 0,
        fillOpacity = 0.7)
        # color = ~ pal(current_data)) %>%
      # addLegend("bottomright",
      #   pal = pal,
      #   values = ~ current_data %>% append(values = c(0, max_val)),
      #   title = legend_label,
      #   opacity = 1,
      #   na.label = 'Tracts with little or no population')
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    pal <- colorpal()
    
    leafletProxy("map", data = filteredData()) %>%
      clearShapes() %>%
      addPolygons(weight = 1, color = "#777777",
                 fillColor = ~pal(median_household_incomeE), fillOpacity = 0.7 #, popup = ~paste(median_household_incomeE)
      )
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = df)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~median_household_incomeE
      )
    }
  })
}

# ui <- bootstrapPage(
#   tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
#   leafletOutput("map", width = "100%", height = "100%"),
#   absolutePanel(top = 10, right = 10,
                # sliderInput("range", "Magnitudes", min(quakes$mag), max(quakes$mag),
                #             value = range(quakes$mag), step = 0.1
                # ),
                # selectInput("colors", "Color Scheme",
                #             rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                # ),
                # checkboxInput("legend", "Show legend", TRUE)
#   )
# )
# 
# server <- function(input, output, session) {
#   
  # # Reactive expression for the data subsetted to what the user selected
  # filteredData <- reactive({
  #   quakes[quakes$mag >= input$range[1] & quakes$mag <= input$range[2],]
  # })
  # # TODO: try creating a reactive expression using a hash table: ht <- new.env(hash=TRUE) => ht[[key]] <- df
  # # however, we'd need to think about whether to filter or hash if we got to the point of custom time steps
  # 
  # # This reactive expression represents the palette function,
  # # which changes as the user makes selections in UI.
  # colorpal <- reactive({
  #   colorNumeric(input$colors, quakes$mag)
  # })
  # 
  # output$map <- renderLeaflet({
  #   # Use leaflet() here, and only include aspects of the map that
  #   # won't need to change dynamically (at least, not unless the
  #   # entire map is being torn down and recreated).
  #   leaflet(quakes) %>% addTiles() %>%
  #     fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
  # })
  # 
  # # Incremental changes to the map (in this case, replacing the
  # # circles when a new color is chosen) should be performed in
  # # an observer. Each independent set of things that can change
  # # should be managed in its own observer.
  # observe({
  #   pal <- colorpal()
  # 
  #   leafletProxy("map", data = filteredData()) %>%
  #     clearShapes() %>%
  #     addCircles(radius = ~10^mag/10, weight = 1, color = "#777777",
  #                fillColor = ~pal(mag), fillOpacity = 0.7, popup = ~paste(mag)
  #     )
  # })
  # 
  # # Use a separate observer to recreate the legend as needed.
  # observe({
  #   proxy <- leafletProxy("map", data = quakes)
  # 
  #   # Remove any existing legend, and only if the legend is
  #   # enabled, create a new one.
  #   proxy %>% clearControls()
  #   if (input$legend) {
  #     pal <- colorpal()
  #     proxy %>% addLegend(position = "bottomright",
  #                         pal = pal, values = ~mag
  #     )
  #   }
  # })
# }

shinyApp(ui, server)
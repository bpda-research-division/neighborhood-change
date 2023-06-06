# Imports and Setup ##############
library(dplyr)
library(shiny)
library(leaflet)
library(tidycensus)
library(RColorBrewer)
options(tigris_use_cache = TRUE)

# Data loading ######################
census_api_key("3910e99aea0a472b50f5cdc422c9a3395b3c87b3")

#v21 <- load_variables(2021, "acs5/subject", cache = TRUE)

my_states = c("MA")
my_vars <- c(
  total_households = "S1901_C01_001"
  , median_household_income = "S1901_C01_012"
)

years <- c(2010, 2012, 2014, 2016, 2018)

get_acs_by_yr <- function(yr) {
  ct <- get_acs(
    geography = "tract",
    variables = my_vars,
    state = my_states,
    county = "025",
    year = yr,
    survey="acs5",
    output = "wide",
    geometry = TRUE,
    cache_table = TRUE
  )
  ct$year <- yr
  ct
}
cs <- lapply(years, get_acs_by_yr)
df <- cs %>% bind_rows() %>% 
  mutate(median_household_incomeE = ifelse(startsWith(NAME, "Census Tract 98"), NaN,median_household_incomeE)) %>%
  sf::st_transform(4326)

df %>% as_tibble() %>% subset(select=-c(geometry)) %>% write.csv(file='income.csv', row.names=FALSE)

# UI ############
ui <- fluidPage(
  titlePanel(h1("Neighborhood Change Dashboard", align = "center")),
  sidebarLayout(
    sidebarPanel(
      sliderInput("yearSelect", "Drag slider to see change over time", 
                  2010, 2018, value = 2018, step = 2, sep = "", ticks=FALSE
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

# Server ##############
server <- function(input, output, session) {
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    # quakes[quakes$mag >= input$range[1] & quakes$mag <= input$range[2],]
    df[df$year == input$yearSelect,]
  })
  # TODO: try creating a reactive expression using a hash table: 
  # ht <- new.env(hash=TRUE) => ht[[key]] <- df
  # however, we'd need to think about whether to filter or hash if we got to 
  # the point of arbitrary time steps for a single variable
  
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
      addProviderTiles(provider = "CartoDB.Positron", group='basemap') %>%
      addPolygons( # data=df[df$year == 2010,], group="2010", fillColor = colorNumeric("Blues", domain = ~median_household_incomeE),
        stroke = F,
        smoothFactor = 0,
        fillOpacity = 0.7) # %>%
      # addPolygons(data=df[df$year == 2018,], group="2018", fillColor = colorNumeric("Blues", domain = ~median_household_incomeE),
      #   stroke = F,
      #   smoothFactor = 0,
      #   fillOpacity = 0.7) %>%
      # addLayersControl(
      #   baseGroups = c("basemap"),
      #   overlayGroups = c("2010", "2018"),
      #   options = layersControlOptions(collapsed = TRUE)
      # ) %>% hideGroup("2010")
    # maybe I can write a function that takes in a leaflet and runs the for loop of addPolygons
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
    # leafletProxy("map") %>% showGroup(input$yearSelect)
  })
  
  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = df)
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend_decreasing(position = "bottomright",
                          pal = pal, values = ~median_household_incomeE,
                          na.label = 'Tracts with little or no population',
                          decreasing = TRUE, title = "Median Household Income ($)"
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

# Extra Functions #############
addLegend_decreasing <- function (map, position = c("topright", "bottomright", "bottomleft","topleft"),
                                  pal, values, na.label = "NA", bins = 7, colors, 
                                  opacity = 0.5, labels = NULL, labFormat = labelFormat(), 
                                  title = NULL, className = "info legend", layerId = NULL, 
                                  group = NULL, data = getMapData(map), decreasing = FALSE) {
  
  position <- match.arg(position)
  type <- "unknown"
  na.color <- NULL
  extra <- NULL
  if (!missing(pal)) {
    if (!missing(colors)) 
      stop("You must provide either 'pal' or 'colors' (not both)")
    if (missing(title) && inherits(values, "formula")) 
      title <- deparse(values[[2]])
    values <- evalFormula(values, data)
    type <- attr(pal, "colorType", exact = TRUE)
    args <- attr(pal, "colorArgs", exact = TRUE)
    na.color <- args$na.color
    if (!is.null(na.color) && col2rgb(na.color, alpha = TRUE)[[4]] == 
        0) {
      na.color <- NULL
    }
    if (type != "numeric" && !missing(bins)) 
      warning("'bins' is ignored because the palette type is not numeric")
    if (type == "numeric") {
      cuts <- if (length(bins) == 1) 
        pretty(values, bins)
      else bins   
      if (length(bins) > 2) 
        if (!all(abs(diff(bins, differences = 2)) <= 
                 sqrt(.Machine$double.eps))) 
          stop("The vector of breaks 'bins' must be equally spaced")
      n <- length(cuts)
      r <- range(values, na.rm = TRUE)
      cuts <- cuts[cuts >= r[1] & cuts <= r[2]]
      n <- length(cuts)
      p <- (cuts - r[1])/(r[2] - r[1])
      extra <- list(p_1 = p[1], p_n = p[n])
      p <- c("", paste0(100 * p, "%"), "")
      if (decreasing == TRUE){
        colors <- pal(rev(c(r[1], cuts, r[2])))
        labels <- rev(labFormat(type = "numeric", cuts))
      }else{
        colors <- pal(c(r[1], cuts, r[2]))
        labels <- rev(labFormat(type = "numeric", cuts))
      }
      colors <- paste(colors, p, sep = " ", collapse = ", ")
    }
    else if (type == "bin") {
      cuts <- args$bins
      n <- length(cuts)
      mids <- (cuts[-1] + cuts[-n])/2
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "bin", cuts))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "bin", cuts)
      }
    }
    else if (type == "quantile") {
      p <- args$probs
      n <- length(p)
      cuts <- quantile(values, probs = p, na.rm = TRUE)
      mids <- quantile(values, probs = (p[-1] + p[-n])/2, na.rm = TRUE)
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "quantile", cuts, p))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "quantile", cuts, p)
      }
    }
    else if (type == "factor") {
      v <- sort(unique(na.omit(values)))
      colors <- pal(v)
      labels <- labFormat(type = "factor", v)
      if (decreasing == TRUE){
        colors <- pal(rev(v))
        labels <- rev(labFormat(type = "factor", v))
      }else{
        colors <- pal(v)
        labels <- labFormat(type = "factor", v)
      }
    }
    else stop("Palette function not supported")
    if (!any(is.na(values))) 
      na.color <- NULL
  }
  else {
    if (length(colors) != length(labels)) 
      stop("'colors' and 'labels' must be of the same length")
  }
  legend <- list(colors = I(unname(colors)), labels = I(unname(labels)), 
                 na_color = na.color, na_label = na.label, opacity = opacity, 
                 position = position, type = type, title = title, extra = extra, 
                 layerId = layerId, className = className, group = group)
  invokeMethod(map, data, "addLegend", legend)
}

# Run the app ########
shinyApp(ui, server)
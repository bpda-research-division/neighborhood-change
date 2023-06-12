# Imports and Setup ##############
library(dplyr)
library(shiny)
library(plotly)
library(leaflet)
library(tidycensus)
library(RColorBrewer)
options(tigris_use_cache = TRUE)
setwd(getSrcDirectory(function(){})[1])

x <- c(1:25)
search <- rnorm(25, mean = 1)
my_bar_color <- '#60809f'
my_light_line_color <- "#c6c6b9"
my_line_skinny <- .75
forms <- rnorm(25, mean = 1)
admin <- rnorm(25, mean = 1)
data <- data.frame(x, search, forms, admin)

inc_bckts <- c(
  "Less than $10,000" = "S1901_C01_002"
  , "$10,000 to $14,999" = "S1901_C01_003"
  , "$15,000 to $24,999" = "S1901_C01_004"
  , "$25,000 to $34,999" = "S1901_C01_005"
  , "$35,000 to $49,999" = "S1901_C01_006"
  , "$50,000 to $74,999" = "S1901_C01_007"
  , "$75,000 to $99,999" = "S1901_C01_008"
  , "$100,000 to $149,999" = "S1901_C01_009"
  , "$150,000 to $199,999" = "S1901_C01_010"
  , "More than $200,000" = "S1901_C01_011"
)

# Data loading ######################
df <- readRDS(file ="./data/tract_hh_income_geo.rds")
bdf <- readRDS(file = "./data/tract_hh_income_brackets_geo.rds")
# t <- subset(bdf, GEOID == '25025010802' & year == 2018)
# d10 <- df[df$year == 2010,]
# d18 <- df[df$year == 2018,]
yrdfs <- split(df, df$year)
pal <- colorNumeric("Purples", domain = df$median_household_incomeE)

# UI ############
ui <- fluidPage(
  headerPanel(h1("Neighborhood Change Dashboard", align = "center")),
  sidebarPanel(style = "height: 90vh;",
    selectInput("variable", "Select Variable", choices = c("Income", "Age")),
    sliderInput("yearSelect", "Drag slider to see change over time",
                2010, 2018, value = 2010, step = 2, sep = "", ticks=FALSE),
    leafletOutput("map", width="100%", height="100%")
    , width=6 # will probably go for 6 on the slider + map side...
  ),
  mainPanel(
    # selectInput("colors", "Color Scheme",
    #             rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
    # ),
    # checkboxInput("legend", "Show legend", TRUE),
    plotlyOutput("bar_chart"),
    plotlyOutput("line_chart"),
    width = 6 # and 6 on the bar + line side
  )
)

addTimedLayers <- function(map) {
  for (yr in names(yrdfs)) {
    map <- map %>% addPolygons(data=yrdfs[[yr]], group=yr, fillColor = ~pal(median_household_incomeE),
                               stroke = F,
                               smoothFactor = 0,
                               fillOpacity = 0.7)
  }
  map
}

# Server ##############
server <- function(input, output, session) {
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    # quakes[quakes$mag >= input$range[1] & quakes$mag <= input$range[2],]
    df[df$year == input$yearSelect,]
  })
  year_str <- reactive({
    as.character(input$yearSelect)
  })
  filteredBar <- reactive({
    subset(bdf, GEOID == '25025010802' & year == input$yearSelect)
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
      # addPolygons(data=d10, group="2010", fillColor = ~pal(median_household_incomeE),
      #   stroke = F,
      #   smoothFactor = 0,
      #   fillOpacity = 0.7) %>%
      # addPolygons(data=d18, group="2018", fillColor = ~pal(median_household_incomeE),
      #   stroke = F,
      #   smoothFactor = 0,
      #   fillOpacity = 0.7) %>%
      addTimedLayers() %>%
      addLayersControl(
        baseGroups = c("basemap"),
        overlayGroups = names(yrdfs), # c("2010", "2018")
        options = layersControlOptions(collapsed = TRUE) ) %>%
    # maybe I can write a function that takes in a leaflet and runs the for loop of addPolygons
        # color = ~ pal(current_data)) %>%
      # addLegend("bottomright",
      #   pal = pal,
      #   values = ~ current_data %>% append(values = c(0, max_val)),
      #   title = legend_label,
      #   opacity = 1,
      #   na.label = 'Tracts with little or no population') %>%
      addLegend_decreasing(position = "bottomright",
         pal = pal, values = df$median_household_incomeE,
         na.label = 'Tracts with little or no population',
         decreasing = TRUE, title = "Median Household Income ($)") %>%
      setView(-71.075, 42.318, zoom = 12)
  })
  
  output$bar_chart <- renderPlotly({
    plot_ly(filteredBar(),
      x = ~variable,
      y = ~estimate,
      # marker = list(color = my_bar_color),
      name = "Household Income",
      # type = "bar",
      source = "bar_plot"
    ) %>% 
      add_bars() %>%
      layout(yaxis = list(title = '% of Households', range = c(0, 25)), 
             xaxis = list(title = '', categoryorder = 'array', categoryarray = names(inc_bckts)))
  })
  
  output$line_chart <- renderPlotly({
    
    plot_ly(df[df$GEOID == '25025010802',], x = ~year,
            y = ~median_household_incomeE
            # name = 'MHI',
            # type = 'scatter',
            # mode = 'lines',
            # line = list(color = my_light_line_color,
            #             width = my_line_skinny)
            ) %>% add_lines()
      # add_trace(y = ~forms,
      #           name = 'forms',
      #           mode = 'lines',
      #           line = list(color = my_light_line_color,
      #                       width = my_line_skinny)) %>%
      # add_trace(y = ~admin,
      #           name = 'admin',
      #           mode = 'lines',
      #           line = list(color = my_light_line_color,
      #                       width = my_line_skinny)) %>% 
      # event_register('plotly_unhover')
    
  })

  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    # pal <- colorpal()
    #
    # leafletProxy("map", data = filteredData()) %>%
    #   clearShapes() %>%
    #   addPolygons(weight = 1, color = "#777777",
    #              fillColor = ~pal(median_household_incomeE), fillOpacity = 0.7 #, popup = ~paste(median_household_incomeE)
    #   )
    leafletProxy("map") %>% hideGroup(names(yrdfs)) %>% showGroup(year_str())
  })

  # Use a separate observer to recreate the legend as needed.
  # observe({
  #   proxy <- leafletProxy("map", data = df)
  #
  #   # Remove any existing legend, and only if the legend is
  #   # enabled, create a new one.
  #   proxy %>% clearControls()
  #   if (input$legend) {
  #     pal <- colorpal()
  #     proxy %>% addLegend_decreasing(position = "bottomright",
  #                         pal = pal, values = ~median_household_incomeE,
  #                         na.label = 'Tracts with little or no population',
  #                         decreasing = TRUE, title = "Median Household Income ($)"
  #     )
  #   }
  # })
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
# Imports and Setup ##############
library(sf)
library(dplyr)
library(shiny)
source('utils.R', local=TRUE) # import helper functions from utils.R within this directory
library(plotly)
library(leaflet)
library(htmltools)
# library(tidycensus)
library(RColorBrewer)

# Data loading ######################

# Probably need to set up a data structure of dfs keyed on variable codes or names,
# and then set up a reactive expression to update which set of dfs is to be used
# although it's not just a set of dfs, it's also an aggregator and potentially prefixes and such
# my python instincts for that are to set up a Variable class with named attributes sb, cb, ss, cs, aggregator, etc

tracts2010 <- read_sf('geoms/boston_tracts_2010.geojson') # only attribute is the GEOID10 on which we do the join
tracts2010$GEOID10 <- as.character(tracts2010$GEOID10)

# For each variable, we have (sub-city / citywide) * (binned data / central tendency)
# each variable has a code, so we read in varcode_<sb/cb/ss/cs>.rds

df <- readRDS(file ="./data/acshhi_ss.rds") %>% 
  mutate(GEOID = as.character(GEOID)) %>%
  merge(tracts2010, by.y = "GEOID10", by.x = "GEOID") %>%
  st_as_sf()
# df_prev <- readRDS(file ="./data/tract_hh_income_geo.rds")

bdf <- readRDS(file = "./data/acshhi_sb.rds") %>%
  mutate(GEOID = as.character(GEOID)) %>%
  merge(tracts2010, by.y = "GEOID10", by.x = "GEOID")

cs_df <- readRDS(file = './data/acshhi_cs.rds')
cb_df <- readRDS(file = './data/acshhi_cb.rds')
# t <- subset(bdf, GEOID %in% c('25025010802', '25025010801') & year == 2018) %>%
#   group_by(variable) %>% summarise(estimate = mean(estimate))
# t <- subset(df, GEOID == '25025010802' & year == 2018)$median_household_incomeE
# d10 <- df[df$year == 2010,]
# d18 <- df[df$year == 2018,]
yrdfs <- split(df, df$YEAR)
# yrdfs_prev <- split(df_prev, df_prev$year)
pal <- colorNumeric("Purples", domain = df$SUMMARY_VALUE)

# # the below variables are used to reformat the map legend to place the NA value below the color
# # palette - default behavior in the current version of Leaflet is for them to be side by side
# css_fix <- "div.info.legend.leaflet-control br {clear: both;}" # CSS to correct spacing
# html_fix <- htmltools::tags$style(type = "text/css", css_fix)  # Convert CSS to HTML

my_bar_color <- '#60809f'
my_light_line_color <- "#c6c6b9"
my_line_skinny <- .75

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

addTimedLayers <- function(map) {
  map <- map %>% addMapPane("layer1", zIndex=420) %>% addMapPane("layer2",zIndex=410)
  for (yr in names(yrdfs)) {
    map <- map %>% 
      addPolygons(data=yrdfs[[yr]], group=yr, layerId = ~paste(GEOID, yr), fillColor = ~pal(SUMMARY_VALUE),
                 weight = 1, color = "gray", smoothFactor = 0, fillOpacity = 0.7, # label = ~htmlEscape(NAME),
                 options = pathOptions(pane = "layer2"), # lower pane
                 # Highlight polygons upon mouseover
                 highlight = highlightOptions(
                   weight = 3,
                   #stroke = 2,
                   fillOpacity = 0.7,
                   color = "black",
                   #opacity = 1.0,
                   #bringToFront = TRUE,
                   #sendToBack = TRUE
                   ), 
                 )
  } 
  map %>% # hidden layer of identical polygons that will be added in response to clicks
    addPolygons(data=yrdfs[[yr]], group=~GEOID, weight = 3, color = "red", fillOpacity=0,
                options = pathOptions(pane = "layer1") # upper pane
                ) %>% hideGroup(group = yrdfs[[yr]]$GEOID) #
}

# Extra Functions #############
# # I grabbed the code for this function from GitHub user mpriem89, who wrote it as a workaround
# # for an open Leaflet issue regarding map legends: https://github.com/rstudio/leaflet/issues/256
# addLegend_decreasing <- function (map, position = c("topright", "bottomright", "bottomleft","topleft"),
#                                   pal, values, na.label = "NA", bins = 7, colors,
#                                   opacity = 0.5, labels = NULL, labFormat = labelFormat(),
#                                   title = NULL, className = "info legend", layerId = NULL,
#                                   group = NULL, data = getMapData(map), decreasing = FALSE) {
#   
#   position <- match.arg(position)
#   type <- "unknown"
#   na.color <- NULL
#   extra <- NULL
#   if (!missing(pal)) {
#     if (!missing(colors))
#       stop("You must provide either 'pal' or 'colors' (not both)")
#     if (missing(title) && inherits(values, "formula"))
#       title <- deparse(values[[2]])
#     values <- evalFormula(values, data)
#     type <- attr(pal, "colorType", exact = TRUE)
#     args <- attr(pal, "colorArgs", exact = TRUE)
#     na.color <- args$na.color
#     if (!is.null(na.color) && col2rgb(na.color, alpha = TRUE)[[4]] ==
#         0) {
#       na.color <- NULL
#     }
#     if (type != "numeric" && !missing(bins))
#       warning("'bins' is ignored because the palette type is not numeric")
#     if (type == "numeric") {
#       cuts <- if (length(bins) == 1)
#         pretty(values, bins)
#       else bins
#       if (length(bins) > 2)
#         if (!all(abs(diff(bins, differences = 2)) <=
#                  sqrt(.Machine$double.eps)))
#           stop("The vector of breaks 'bins' must be equally spaced")
#       n <- length(cuts)
#       r <- range(values, na.rm = TRUE)
#       cuts <- cuts[cuts >= r[1] & cuts <= r[2]]
#       n <- length(cuts)
#       p <- (cuts - r[1])/(r[2] - r[1])
#       extra <- list(p_1 = p[1], p_n = p[n])
#       p <- c("", paste0(100 * p, "%"), "")
#       if (decreasing == TRUE){
#         colors <- pal(rev(c(r[1], cuts, r[2])))
#         labels <- rev(labFormat(type = "numeric", cuts))
#       }else{
#         colors <- pal(c(r[1], cuts, r[2]))
#         labels <- rev(labFormat(type = "numeric", cuts))
#       }
#       colors <- paste(colors, p, sep = " ", collapse = ", ")
#     }
#     else if (type == "bin") {
#       cuts <- args$bins
#       n <- length(cuts)
#       mids <- (cuts[-1] + cuts[-n])/2
#       if (decreasing == TRUE){
#         colors <- pal(rev(mids))
#         labels <- rev(labFormat(type = "bin", cuts))
#       }else{
#         colors <- pal(mids)
#         labels <- labFormat(type = "bin", cuts)
#       }
#     }
#     else if (type == "quantile") {
#       p <- args$probs
#       n <- length(p)
#       cuts <- quantile(values, probs = p, na.rm = TRUE)
#       mids <- quantile(values, probs = (p[-1] + p[-n])/2, na.rm = TRUE)
#       if (decreasing == TRUE){
#         colors <- pal(rev(mids))
#         labels <- rev(labFormat(type = "quantile", cuts, p))
#       }else{
#         colors <- pal(mids)
#         labels <- labFormat(type = "quantile", cuts, p)
#       }
#     }
#     else if (type == "factor") {
#       v <- sort(unique(na.omit(values)))
#       colors <- pal(v)
#       labels <- labFormat(type = "factor", v)
#       if (decreasing == TRUE){
#         colors <- pal(rev(v))
#         labels <- rev(labFormat(type = "factor", v))
#       }else{
#         colors <- pal(v)
#         labels <- labFormat(type = "factor", v)
#       }
#     }
#     else stop("Palette function not supported")
#     if (!any(is.na(values)))
#       na.color <- NULL
#   }
#   else {
#     if (length(colors) != length(labels))
#       stop("'colors' and 'labels' must be of the same length")
#   }
#   legend <- list(colors = I(unname(colors)), labels = I(unname(labels)),
#                  na_color = na.color, na_label = na.label, opacity = opacity,
#                  position = position, type = type, title = title, extra = extra,
#                  layerId = layerId, className = className, group = group)
#   invokeMethod(map, data, "addLegend", legend)
# }

# Server ##############
tabPanelServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {

      year_str <- reactive({
        as.character(input$yearSelect)
      }) # is this necessary, or can I just call as.character on input$yearSelect when i need it?
      
      selectedLine <- reactive({
        if (length(selected$groups) == 0) {cs_df}
        else {
          subset(df, GEOID %in% selected$groups) %>% # & year == input$yearSelect
            group_by(YEAR) %>% summarise(SUMMARY_VALUE = mean(SUMMARY_VALUE))
        }
        # # quakes[quakes$mag >= input$range[1] & quakes$mag <= input$range[2],]
        # s <- 
        # s #$median_household_incomeE
      })
      
      filteredBar <- reactive({
        aggregator <- sum # we need some kind of reactive that changes the aggregator based on the variable.
        # this could be tricky when we get to pareto interpolation because that requires multiple vectors of input
        
        if (length(selected$groups) == 0) {
          subset(cb_df, YEAR == input$yearSelect)
        }
        else {
          subset(bdf, GEOID %in% selected$groups & YEAR == input$yearSelect) %>%
            group_by(CATEGORY) %>% summarise(VALUE = aggregator(VALUE))
        }
        # we'll probably use a different variable for citywide and then move this statement inside the else block
        
      })
      
      barRange <- reactive({
        # we want this to be as high as 
        aggregator <- sum
        if (length(selected$groups) == 0) {
          data <- cb_df
        }
        else {
          data <- subset(bdf, GEOID %in% selected$groups) %>%
            group_by(CATEGORY, YEAR) %>% summarise(VALUE = aggregator(VALUE), .groups="drop")
        }
        c(0, 1.1*max(data$VALUE))
      }) # is this necessary, or should i just call the 0 to max when i need the range?
      # TODO: try creating a reactive expression using a hash table:
      # ht <- new.env(hash=TRUE) => ht[[key]] <- df
      # however, we'd need to think about whether to filter or hash if we got to
      # the point of arbitrary time steps for a single variable
      
      # This reactive expression represents the palette function,
      # which changes as the user makes selections in UI.
      # colorpal <- reactive({
      #   colorNumeric(input$colors, df$median_household_incomeE)
      # })
      
      output$selectionText <- reactive({
        # the word "tract" should eventually be a variable reflecting the geography selection (tract, neighborhood, etc)
        msg <- "Currently viewing data for: " 
        if (length(selected$groups) == 0) {paste(msg, "<b>the whole city<b>")}
        else {paste(msg, sprintf("<b>%s selected tracts<b>", length(selected$groups)))}
      })
      
      output$map <- renderLeaflet({
        # Use leaflet() here, and only include aspects of the map that
        # won't need to change dynamically (at least, not unless the
        # entire map is being torn down and recreated).
        # leaflet(quakes, width="100%", height="100%") %>% addTiles() %>%
        #   fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))
        leaflet(df) %>% 
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
          # addLayersControl(
          #   baseGroups = c("basemap"),
          #   overlayGroups = names(yrdfs), # c("2010", "2018")
          #   options = layersControlOptions(collapsed = TRUE) ) %>%
          # color = ~ pal(current_data)) %>%
          # addLegend("bottomright",
          #   pal = pal,
          #   values = ~ current_data %>% append(values = c(0, max_val)),
          #   title = legend_label,
          #   opacity = 1,
          #   na.label = 'Tracts with little or no population') %>%
        addLegend_decreasing(position = "bottomright",
                             pal = pal, values = df$SUMMARY_VALUE,
                             na.label = 'Tracts with little or <br> no population' %>% lapply(htmltools::HTML),
                             decreasing = TRUE, title = "Median Household <br> Income ($)" %>% lapply(htmltools::HTML)) %>%
          setView(-71.075, 42.318, zoom = 12)
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
        leafletProxy("map") %>% hideGroup(group = names(yrdfs)) %>% showGroup(year_str())
      })
      
      #create empty vector to hold all click ids
      selected <- reactiveValues(groups = vector())
      
      observeEvent(input$clearSelections, {
        selected$groups <- vector()
        leafletProxy("map") %>% hideGroup(group = yrdfs[['2018']]$GEOID) # replace with something more generic than 2018
      })
      
      observeEvent(input$map_shape_click, {
        #print(strsplit(input$map_shape_click$id, split = " ")[[1]][1])
        
        if(nchar(input$map_shape_click$group) == 4){
          selected$groups <- c(selected$groups, strsplit(input$map_shape_click$id, split = " ")[[1]][1])
          leafletProxy("map") %>% showGroup(group = strsplit(input$map_shape_click$id, split = " ")[[1]][1])
        } else {
          selected$groups <- setdiff(selected$groups, input$map_shape_click$group)
          leafletProxy("map") %>% hideGroup(group = input$map_shape_click$group)
        }
        #plotlyProxy("line_chart") %>% plotlyProxyInvoke()
        # this is where we'd call some function to update the line and bar charts, like they do here:
        # https://stackoverflow.com/questions/65893124/select-multiple-items-using-map-click-in-leaflet-linked-to-selectizeinput-in
      })
      
      output$bar_chart <- renderPlotly({
        plot_ly(filteredBar(),
                x = ~CATEGORY, 
                y = ~VALUE, 
                # xend=~variable, yend=0, # if using add_segments()
                # marker = list(color = my_bar_color),
                name = "Household Income",
                hoverinfo = 'text',
                # type = "bar",
                source = "bar_plot"
        ) %>% #hide_legend %>%
          config(displayModeBar = FALSE, scrollZoom = FALSE) %>%
          # htmlwidgets::onRender("function(el, x) {Plotly.d3.select('.cursor-pointer').style('cursor', 'auto')}") %>%
          add_bars(color=I(my_bar_color), hoverinfo = 'y') %>% # line = list(width = 25) # if using add_segments()
          layout(yaxis = list(title = '', fixedrange = TRUE, range = barRange()), title = sprintf('Households by Income in %s', input$yearSelect), # ticksuffix="%", hoverformat = '.1f', 
                 xaxis = list(title = '', fixedrange = TRUE, categoryorder = 'array', categoryarray = names(inc_bckts)))
        # animation_opts(frame=500, transition=500, redraw=FALSE)
      })
      
      output$line_chart <- renderPlotly({
        
        plot_ly(selectedLine(), # df[df$GEOID %in% c('25025010802','25025010801'),] %>% group_by(year) %>% summarise(median_household_incomeE = mean(median_household_incomeE))
                x = ~YEAR,
                y = ~SUMMARY_VALUE,
                hoverinfo = 'text'
                # name = 'MHI',
                # type = 'scatter',
                # mode = 'lines',
                # line = list(color = my_light_line_color,
                #             width = my_line_skinny)
        ) %>% 
          config(displayModeBar = FALSE) %>%
          add_lines(color=I(my_bar_color), hoverinfo = "y") %>%
          add_markers(x = input$yearSelect, name = 'highlight', hoverinfo = "y",
                      y = subset(selectedLine(), YEAR == input$yearSelect)$SUMMARY_VALUE,
                      marker = list(color=my_bar_color, size=10), showlegend = F) %>%
          # add_text(x = input$yearSelect,
          #          y = subset(selectedLine(), year == input$yearSelect)$median_household_incomeE,
          #          text = subset(selectedLine(), year == input$yearSelect)$median_household_incomeE,
          #          textposition = 'top center', hovertext = ''
          #          ) %>% 
          
          layout(yaxis = list(title = '', fixedrange = TRUE, tickprefix = '$', tickformat="~s", hoverformat = ",.0f", range = c(0, 1.1*max(selectedLine()$SUMMARY_VALUE, na.rm=TRUE))), 
                 xaxis = list(title = 'Year', fixedrange = TRUE, range = c(2009, 2019)), title = 'Median Household Income')
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
  )
}

server <- function(input, output, session) {
  lapply(c("tracts", "neighborhoods"), function(geo_type) {
    tabPanelServer(geo_type)
  })
}
# Imports and Setup ##############
library(sf)
library(dplyr)
library(shiny)
#source('utils.R', local=TRUE) # import helper functions from utils.R within this directory
library(plotly)
library(leaflet)
library(leafem)
library(htmltools)
# library(tidycensus)
library(RColorBrewer)

# Data loading ######################

# Probably need to set up a data structure of dfs keyed on variable codes or names,
# and then set up a reactive expression to update which set of dfs is to be used
# although it's not just a set of dfs, it's also an aggregator and potentially prefixes and such
# my python instincts for that are to set up a Variable class with named attributes sb, cb, ss, cs, aggregator, etc

# structre keyed on variable labels = x[['Labor Force']]

df_types <- c('sb', 'ss', 'cb', 'cs')

dfs_from_varcode <- function(varcode) {
  dfs <- lapply(df_types, function(x) paste0("./data/", varcode, "_", x, ".rds")) %>%
    lapply(readRDS) %>% `names<-`(lapply(df_types, function(x) paste0(x, '_df')))
  dfs #%>% lapply(as.data.frame) %>% lapply(function(df) df %>% mutate(YEAR = as.character(YEAR)))
}

myvars <- all_vars %>% lapply(function(geo_type) geo_type %>% lapply(
  function(var) var$varcode %>% 
    dfs_from_varcode
  )
)

geoms <- list()
geoms$tracts <- read_sf('geoms/boston_tracts_2010.geojson') %>% mutate(GEOID = as.character(GEOID10))
geoms$neighborhoods <- read_sf('geoms/boston_neighborhoods_2020bg.geojson') %>% mutate(GEOID = BlockGr202)

for (geo_type in names(myvars)) {
  for (varname in names(myvars[[geo_type]])) {
    myvars[[geo_type]][[varname]]$ss_df <- myvars[[geo_type]][[varname]]$ss_df %>%
      mutate(GEOID = as.character(GEOID)) %>%
      merge(y=geoms[[geo_type]], by.y = "GEOID", by.x = "GEOID") %>%
      st_as_sf()
    
    myvars[[geo_type]][[varname]]$sb_df <- myvars[[geo_type]][[varname]]$sb_df %>%
      mutate(GEOID = as.character(GEOID))
  }
}

print("hey")

# For each variable, we have (sub-city / citywide) * (binned data / central tendency)
# each variable has a code, so we read in varcode_<sb/cb/ss/cs>.rds

# tracts2010 <- read_sf('geoms/boston_tracts_2010.geojson') %>%
#   mutate(GEOID10 = as.character(GEOID10))
# 
# df <- readRDS(file ="./data/acshhi_ss.rds") %>% 
#   mutate(GEOID = as.character(GEOID)) %>%
#   merge(tracts2010, by.y = "GEOID10", by.x = "GEOID") %>%
#   st_as_sf()
# # df_prev <- readRDS(file ="./data/tract_hh_income_geo.rds")
# 
# bdf <- readRDS(file = "./data/acshhi_sb.rds") %>%
#   mutate(GEOID = as.character(GEOID)) # %>%
#   # merge(tracts2010, by.y = "GEOID10", by.x = "GEOID")
# 
# cs_df <- readRDS(file = './data/acshhi_cs.rds')
# cb_df <- readRDS(file = './data/acshhi_cb.rds')
# t <- subset(bdf, GEOID %in% c('25025010802', '25025010801') & year == 2018) %>%
#   group_by(variable) %>% summarise(estimate = mean(estimate))
# t <- subset(df, GEOID == '25025010802' & year == 2018)$median_household_incomeE
# d10 <- df[df$year == 2010,]
# d18 <- df[df$year == 2018,]
# yrdfs <- split(df, df$YEAR)
# yrdfs_prev <- split(df_prev, df_prev$year)
# pal <- colorNumeric("Purples", domain = df$SUMMARY_VALUE)

# # the below variables are used to reformat the map legend to place the NA value below the color
# # palette - default behavior in the current version of Leaflet is for them to be side by side
# css_fix <- "div.info.legend.leaflet-control br {clear: both;}" # CSS to correct spacing
# html_fix <- htmltools::tags$style(type = "text/css", css_fix)  # Convert CSS to HTML

my_bar_color <- '#60809f'
my_light_line_color <- "#c6c6b9"
my_line_skinny <- .75

# inc_bckts <- c(
#   "Less than $10,000" = "S1901_C01_002"
#   , "$10,000 to $14,999" = "S1901_C01_003"
#   , "$15,000 to $24,999" = "S1901_C01_004"
#   , "$25,000 to $34,999" = "S1901_C01_005"
#   , "$35,000 to $49,999" = "S1901_C01_006"
#   , "$50,000 to $74,999" = "S1901_C01_007"
#   , "$75,000 to $99,999" = "S1901_C01_008"
#   , "$100,000 to $149,999" = "S1901_C01_009"
#   , "$150,000 to $199,999" = "S1901_C01_010"
#   , "More than $200,000" = "S1901_C01_011"
# )

# t <- myvars[["neighborhoods"]][["Labor Force"]]$ss_df %>% split(~YEAR)

# 
# addTimedLayers <- function(map) {
#   map <- map %>% addMapPane("layer1", zIndex=420) %>% addMapPane("layer2",zIndex=410)
#   for (yr in names(yrdfs)) {
#     map <- map %>% 
#       addPolygons(data=yrdfs[[yr]], group=yr, layerId = ~paste(GEOID, yr), fillColor = ~pal(SUMMARY_VALUE),
#                  weight = 1, color = "gray", smoothFactor = 0, fillOpacity = 0.7, # label = ~htmlEscape(NAME),
#                  options = pathOptions(pane = "layer2"), # lower pane
#                  # Highlight polygons upon mouseover
#                  highlight = highlightOptions(
#                    weight = 3,
#                    #stroke = 2,
#                    fillOpacity = 0.7,
#                    color = "black",
#                    #opacity = 1.0,
#                    #bringToFront = TRUE,
#                    #sendToBack = TRUE
#                    ), 
#                  )
#   } 
#   map %>% # hidden layer of identical polygons that will be added in response to clicks
#     addPolygons(data=yrdfs[[yr]], group=~GEOID, weight = 3, color = "red", fillOpacity=0,
#                 options = pathOptions(pane = "layer1") # upper pane
#                 ) %>% hideGroup(group = yrdfs[[yr]]$GEOID) #
# }

# t <- subset(myvars[['neighborhoods']][['Labor Force']]$cs_df, YEAR == 2010)$SUMMARY_VALUE

# Server ##############
tabPanelServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      # output$varText <- reactive ({
      #   as.character(sum(var_data()$ss_df$SUMMARY_VALUE, na.rm = TRUE))
      # }) # for debugging
      
      geo_namespace <- reactive({
        substr(session$ns(''), 1, nchar(session$ns('')) - 1)
      })
      
      var_params <- reactive({
        all_vars[[geo_namespace()]][[input$variable]]
      })
      
      var_data <- reactive({
        myvars[[geo_namespace()]][[input$variable]]
      })

      year_str <- reactive({
        as.character(input$yearSelect)
      }) # is this necessary, or can I just call as.character on input$yearSelect when i need it?
      
      selectedLine <- reactive({
        print(length(var_data()$cs_df))
        if (length(selected$groups) == 0) {var_data()$cs_df}
        else {
          subset(var_data()$ss_df, GEOID %in% selected$groups) %>%
            group_by(YEAR) %>% summarise(SUMMARY_VALUE = mean(SUMMARY_VALUE)) # TODO: parameterize this agg func, or aggregate from sb in some fashion
        }
      })
      
      filteredBar <- reactive({
        aggregator <- sum # we need some kind of reactive that changes the aggregator based on the variable.
        # this could be tricky if we get to pareto interpolation because that requires multiple vectors of input
        
        if (length(selected$groups) == 0) {
          subset(var_data()$cb_df, YEAR == input$yearSelect)
        }
        else {
          subset(var_data()$sb_df, GEOID %in% selected$groups & YEAR == input$yearSelect) %>%
            group_by(CATEGORY) %>% summarise(VALUE = aggregator(VALUE))
        }
        # we'll probably use a different variable for citywide and then move this statement inside the else block
        
      })
      
      barRange <- reactive({
        # we want this to be as high as 
        aggregator <- sum
        if (length(selected$groups) == 0) {
          data <- var_data()$cb_df
        }
        else {
          data <- subset(var_data()$sb_df, GEOID %in% selected$groups) %>%
            group_by(CATEGORY, YEAR) %>% summarise(VALUE = aggregator(VALUE), .groups="drop")
        }
        c(0, 1.1*max(data$VALUE))
      }) # is this necessary, or should i just call the 0 to max when i need the range?
      
      output$selectionText <- reactive({
        # the word "tract" should eventually be a variable reflecting the geography selection (tract, neighborhood, etc)
        msg <- "Currently viewing data for: " 
        if (length(selected$groups) == 0) {paste(msg, "<b>the whole city<b>")}
        else {paste(msg, sprintf("<b>%s selected %s<b>", length(selected$groups), geo_namespace()))}
      })
      
      output$map <- renderLeaflet({
        # Use leaflet() here, and only include aspects of the map that
        # won't need to change dynamically (at least, not unless the
        # entire map is being torn down and recreated).
        # leaflet(quakes, width="100%", height="100%") %>% addTiles() %>%
        #   fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat))

        leaflet() %>% 
          addProviderTiles(provider = "CartoDB.Positron", group='basemap') %>%
          addMapPane("layer1", zIndex=420) %>% addMapPane("layer2",zIndex=410) %>%
          setView(-71.075, 42.318, zoom = 12)
      })
      
      outputOptions(output, "map", suspendWhenHidden = FALSE) # map for one geo_type will stay rendered when user is on another tab
      
      observeEvent(input$variable, { # if polygons need to be redrawn for other reasons, we can make this a general observer
        ss <- var_data()$ss_df
        yrdfs <- split(ss, ss$YEAR)
        pal <- colorNumeric("Purples", domain = ss$SUMMARY_VALUE)
        leafletProxy("map") %>% clearShapes()
        
        for (yr in names(yrdfs)) {
          leafletProxy("map") %>% 
            addPolygons(data=yrdfs[[yr]], group=yr, layerId = ~paste(GEOID, yr), fillColor = ~pal(SUMMARY_VALUE),
                        weight = 1, color = "gray", smoothFactor = 0, fillOpacity = 0.7, label = ~htmlEscape(NAME),
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
        leafletProxy("map") %>% # hidden layer of identical polygons that will be added in response to clicks
          addPolygons(data=yrdfs[[yr]], group=~GEOID, weight = 3, color = "red", fillOpacity=0,
                      options = pathOptions(pane = "layer1") # upper pane
          ) %>% hideGroup(group = yrdfs[[yr]]$GEOID) %>%
          addLegend_decreasing(position = "bottomright",
                               pal = pal, values = ss$SUMMARY_VALUE,
                               na.label = 'Tracts with little or <br> no population' %>% lapply(htmltools::HTML), # PARAM
                               decreasing = TRUE, title = var_params()$lineTitle %>% lapply(htmltools::HTML))
        
      })
      
      var_years <- reactive({
        var_data()$cs_df$YEAR
      })
      
      # Incremental changes to the map (in this case, replacing the
      # circles when a new color is chosen) should be performed in
      # an observer. Each independent set of things that can change
      # should be managed in its own observer.
      observeEvent(input$yearSelect, {
        # pal <- colorpal()
        #
        # leafletProxy("map", data = filteredData()) %>%
        #   clearShapes() %>%
        #   addPolygons(weight = 1, color = "#777777",
        #              fillColor = ~pal(median_household_incomeE), fillOpacity = 0.7 #, popup = ~paste(median_household_incomeE)
        #   )
        leafletProxy("map") %>% hideGroup(group = var_years()) %>% showGroup(year_str())
      })
      
      #create empty vector to hold IDs of all selected polygons
      selected <- reactiveValues(groups = vector())
      
      observeEvent(input$clearSelections, {
        selected$groups <- vector()
        leafletProxy("map") %>% hideGroup(group = var_data()$ss_df$GEOID) # replace with something more generic than 2018
      })
      
      observeEvent(input$map_shape_click, {
        # this if statement is checking whether the clicked polygon is currently selected or not
        # the group name of each unselected polygon is its YEAR, and it's id is NAME YEAR
        if(input$map_shape_click$group %in% var_years()){
          # this gsub expression removes the last space-delimited word from a string (in this case, removing the YEAR to extract the NAME)
          # I got it from https://stackoverflow.com/questions/13093931/remove-last-word-from-string
          this_selection_id <- gsub("\\s*\\w*$", "", input$map_shape_click$id)
          selected$groups <- c(selected$groups, this_selection_id)
          leafletProxy("map") %>% showGroup(group = this_selection_id) # shows the red-bordered overlay polygon
        } else { # the group name of each selected polygon is the NAME of the polygon
          selected$groups <- setdiff(selected$groups, input$map_shape_click$group)
          leafletProxy("map") %>% hideGroup(group = input$map_shape_click$group)
        }
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
          layout(yaxis = list(title = '', fixedrange = TRUE, range = barRange()), title = paste0(var_params()$barTitle, " in ", input$yearSelect), # ticksuffix="%", hoverformat = '.1f', 
                 xaxis = list(title = '', fixedrange = TRUE, categoryorder = 'array', categoryarray = names(var_params()$barCats)))
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
                 xaxis = list(
                   title = 'Year'
                   , fixedrange = TRUE
                   , range = c(
                     as.numeric(var_params()$start) - 0.1*(as.numeric(var_params()$end)-as.numeric(var_params()$start))
                     , as.numeric(var_params()$end) + 0.1*(as.numeric(var_params()$end)-as.numeric(var_params()$start))
                     )
                   ), title = var_params()$lineTitle
                 )
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
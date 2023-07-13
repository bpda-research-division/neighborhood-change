# Data loading ######################

# subcity bins, subcity summary, citywide bins, citywide summary
df_types <- c('sb', 'ss', 'cb', 'cs')

#' Reads in and returns the four dataframes for a given variable defined by
#' varcode. Files expected to use the naming convention <varcode>_<df_type>.rds
dfs_from_varcode <- function(varcode) {
  dfs <- lapply(df_types, function(x) paste0("./data/", varcode, "_", x, ".rds")) %>%
    lapply(readRDS) %>% `names<-`(lapply(df_types, function(x) paste0(x, '_df')))
  dfs #%>% lapply(as.data.frame) %>% lapply(function(df) df %>% mutate(YEAR = as.character(YEAR)))
}

# using all_vars_info, defined in global.R, read in the four dataframes for each
# variable into all_vars_data
all_vars_data <- all_vars_info %>% lapply(function(geo_type) geo_type %>% lapply(
  function(var) var$varcode %>% 
    dfs_from_varcode
  )
)

# # read one shapefile for each geography type that we have. Must have a unique
# # GEOID attribute that corresponds with the GEOID attribute in our tabular data
# geoms <- list()
# geoms$`census tracts` <- read_sf('geoms/boston_tracts_2020.geojson') %>% mutate(GEOID = as.character(geoid20))
# geoms$neighborhoods <- read_sf('geoms/boston_neighborhoods_2020bg.geojson') %>% mutate(GEOID = BlockGr202)
# 
# # Join each ss_df (the df that gets mapped) to its appropriate shapefile
# for (geo_type in names(all_vars_data)) {
#   for (varname in names(all_vars_data[[geo_type]])) {
#     all_vars_data[[geo_type]][[varname]]$ss_df <- all_vars_data[[geo_type]][[varname]]$ss_df %>%
#       mutate(GEOID = as.character(GEOID)) %>%
#       merge(y=geoms[[geo_type]], by.y = "GEOID", by.x = "GEOID") %>%
#       st_as_sf()
#     
#     # all_vars_data[[geo_type]][[varname]]$sb_df <- all_vars_data[[geo_type]][[varname]]$sb_df %>%
#     #   mutate(GEOID = as.character(GEOID))
#   }
# }

# Server Module ##############
#' Build the server for a tabPanel for a given namespaced geography type 
tabPanelServer <- function(geo_type) {
  moduleServer(
    geo_type,
    function(input, output, session) {
      # string representation of the namespaced geography type (e.g. "tracts")
      geo_namespace <- substr(session$ns(''), 1, nchar(session$ns('')) - 1)
      # Label for map polygons with null values - function of geography type
      null_label <- lapply(
        split_max_2_lines(paste(
          tools::toTitleCase(geo_namespace), 'with little or no population')
        )
        , htmltools::HTML)
      
      # Shortcut to the parameters for whichever variable the user has selected
      var_params <- reactive({
        all_vars_info[[geo_namespace]][[input$variable]]
      })
      
      # output$varText <- reactive ({
      #   var_params()$note
      # }) # for debugging
      
      # Shortcut to the data for whichever variable the user has selected
      var_data <- reactive({
        all_vars_data[[geo_namespace]][[input$variable]]
      })
      
      # Static components of the map
      output$map <- renderLeaflet({
        leaflet() %>% 
          addProviderTiles(provider = "CartoDB.Positron", group='basemap') %>%
          # having two separate panes helps us display user map selections
          addMapPane("layer1", zIndex=420) %>% addMapPane("layer2",zIndex=410) %>%
          # could be worth parameterizing the initial map center and zoom at some point
          setView(-71.075, 42.318, zoom = 12)
      })
      
      # This makes it so that maps on inactive tabs stay rendered
      outputOptions(output, "map", suspendWhenHidden = FALSE) 
      
      # Draw all the map polygons as a function of the variable the user selects
      observeEvent(input$variable, { 
        ss <- var_data()$ss_df # ss_df is the portion of the data that we map
        yrdfs <- split(ss, ss$YEAR)
        pal <- colorNumeric(my_map_palette, domain = ss$SUMMARY_VALUE)
        leafletProxy("map") %>% clearShapes() %>% clearControls()
        
        # draw and add one layer of polygons for each year
        for (yr in names(yrdfs)) {
          leafletProxy("map") %>% 
            addPolygons(data=yrdfs[[yr]], group=yr, layerId = ~paste(GEOID, yr), 
                        fillColor = ~pal(SUMMARY_VALUE), fillOpacity = 0.7, 
                        weight = 1, smoothFactor = 0, label = ~htmlEscape(NAME),
                        labelOptions = labelOptions(style=list("font-size" = sprintf("%spx", APP_FONT_SIZE-2))),
                        options = pathOptions(pane = "layer2"), color = "gray", 
                        highlight = highlightOptions(
                          weight = 3,
                          fillOpacity = 0.85,
                          color = "black",
                          # opacity = 1.0,
                          bringToFront = TRUE
                        ), 
            )
        } 
        # add a hidden layer of polygons that will display in response to clicks
        leafletProxy("map") %>%
          addPolygons(data=yrdfs[[yr]], group=~GEOID, weight = 3, color = "red", 
                      fillOpacity=0, label = ~htmlEscape(NAME), 
                      labelOptions = labelOptions(style=list("font-size" = sprintf("%spx", APP_FONT_SIZE-2))),
                      options = pathOptions(pane = "layer1")
          ) %>% hideGroup(group = yrdfs[[yr]]$GEOID) %>%
          
          # add the map legend, formatting labels appropriately for the given variable
          addLegend_decreasing(position = "bottomright", values = ss$SUMMARY_VALUE,
             pal = pal, labFormat = labelFormat(
               prefix = var_params()$tickprefix, 
               # if the tick format includes a %, add a % suffix and multiply values by 100
               suffix = ifelse(grepl("%", var_params()$tickformat, fixed = TRUE), "%", ""),
               transform = ifelse(grepl("%", var_params()$tickformat, fixed = TRUE), 
                                  function(x) round(x*100), function (x) x)
               ),
             na.label = null_label, decreasing = TRUE, title = split_max_2_lines(var_params()$lineTitle)) # paste(c("hi", "<br>", "there"), collapse = ' ')
      })
      
      # Keep track of the full set of years for the variable the user selects
      var_years <- reactive({
        var_data()$cs_df$YEAR
      })
      
      # Updates the map When the user changes the selected year on the time slider
      observeEvent(list(input$yearSelect, input$variable), {
        leafletProxy("map") %>% hideGroup(group = var_years()) %>% showGroup(input$yearSelect)
      })
      
      # This vector holds the IDs of all currently selected polygons. The line
      # and bar charts are configured to update in response to this vector
      selected <- reactiveValues(groups = vector())
      
      # This handles user clicks on map polygons, selecting and deselecting them
      # under the hood, selected polygons are a separate, initially hidden layer
      observeEvent(input$map_shape_click, {
        # To check whether the clicked polygon is currently selected or not, we 
        # check its group name. The group name of each unselected polygon is its YEAR
        if(input$map_shape_click$group %in% var_years()){
          # The ID of each unselected polygon is NAME YEAR. The group name of each
          # selected polygon is the NAME alone. This gsub expression removes the
          # last space-delimited word from the unselected polygon ID to extract
          # the NAME of the selected polygon to display where the user clicked.
          this_selection_id <- gsub("\\s*\\w*$", "", input$map_shape_click$id)
          
          # only add clicked polygon to selected groups if its data is non-NA
          if (!any(is.na(subset(var_data()$ss_df, GEOID == this_selection_id)$SUMMARY_VALUE))) {
            selected$groups <- c(selected$groups, this_selection_id)
            leafletProxy("map") %>% showGroup(group = this_selection_id) 
          }
          
        } else { # To deselect a polygon, we just hide the polygon and update the data
          selected$groups <- setdiff(selected$groups, input$map_shape_click$group)
          leafletProxy("map") %>% hideGroup(group = input$map_shape_click$group)
        }
      })
      
      # Clears the map and updates the data in response to the user clearing all selections or changing variables
      observeEvent(list(input$clearSelections, input$variable), {
        selected$groups <- vector()
        leafletProxy("map") %>% hideGroup(group = var_data()$ss_df$GEOID)
      })
      
      # Updates the slider input each time the user selects a different variable
      observeEvent(input$variable, {
        st <- var_params()$start

        updateSliderInput(session, "yearSelect", value = st, min = st, 
                          max = var_params()$end, step = var_params()$step)
      })
      
      selectionName <- reactive({
        sprintf("%s selected %s", length(selected$groups), geo_namespace)
      })
      
      # The text telling the user what data they are looking at (based on map selections)
      output$selectionText <- reactive({
        msg <- "Currently viewing data for:<br>" 
        if (length(selected$groups) == 0) {paste(msg, "<b>the whole city<b>")}
        else {paste(msg, paste0('<b>', selectionName(), '<b>'))}
      })
      
      # The data that is displayed on the bar chart
      filteredBar <- reactive({ # if the user doesn't have any polygons selected...
        if (length(selected$groups) == 0) { # ...show citywide data
          subset(var_data()$cb_df, YEAR == input$yearSelect)
        }
        else { # Otherwise, aggregate the data for the set of selected polygons
          subset(var_data()$sb_df, GEOID %in% selected$groups & YEAR == input$yearSelect) %>%
            group_by(CATEGORY) %>% 
            summarise(VALUE = var_params()[["agg_func"]](VALUE))
        }
      })
      
      # The y-axis range for the bar chart
      barRange <- reactive({
        if (length(selected$groups) == 0) {
          data <- var_data()$cb_df
        }
        else {
          data <- subset(var_data()$sb_df, GEOID %in% selected$groups) %>%
            group_by(CATEGORY, YEAR) %>% 
            summarise(VALUE = var_params()[["agg_func"]](VALUE), .groups="drop")
        }
        # multiplying by 1.1 adds some padding between the max value and the top of the chart
        c(0, 1.1*max(data$VALUE, na.rm = TRUE))
      })
      
      # Renders the bar chart
      output$bar_chart <- renderPlotly({
        plot_ly(filteredBar(),
                x = ~CATEGORY, 
                y = ~VALUE, 
                # marker = list(pattern = list(shape = "/")),
                hoverinfo = 'text',
                source = "bar_plot"
        ) %>% 
          config(displayModeBar = FALSE) %>% # remove default plotly controls
          add_bars(color=I(my_bar_color), 
                   hoverinfo = 'y', marker = list(line = list(width=2, color=my_bar_color))
                   ) %>% 
          layout(yaxis = list(
                  title = ''
                  , fixedrange = TRUE
                  , range = barRange()
                  , hoverformat = var_params()$barhoverformat 
                  ), 
                 title = paste0(var_params()$barTitle, " in ", input$yearSelect),
                 xaxis = list(
                   title = ''
                   , fixedrange = TRUE
                   , categoryorder = 'array' # these 2 lines set the order of the bar categories
                   , categoryarray = names(var_params()$barCats)
                  ),
                 hoverlabel = list(bordercolor = 'white', font = list(color="white", size=APP_FONT_SIZE-2)),
                 annotations = list(xref = 'paper', x = 0.5, y=barRange()[2], showarrow=FALSE, text=ifelse(is.null(var_params()$note),"",var_params()$note)),
                 margin = list(t=40),
                 font=list(color="black", family = APP_FONT, size = APP_FONT_SIZE-1)
                 )
      })
      
      # The data that is displayed on the line chart
      selectedLine <- reactive({ # if the user doesn't have any polygons selected...
        if (length(selected$groups) == 0) {var_data()$cs_df} # ...show citywide data
        else if (length(selected$groups) == 1) {
          subset(var_data()$ss_df, GEOID %in% selected$groups)
        }
        else { # Otherwise, aggregate the data for the set of selected polygons
          t <- subset(var_data()$sb_df, GEOID %in% selected$groups) %>%
            group_by(CATEGORY, YEAR) %>%
            summarise(VALUE = var_params()[["agg_func"]](VALUE), .groups = "drop")
          t$CATEGORY <- plyr::mapvalues(t$CATEGORY,
                                        to = unname(var_params()$barCats),
                                        from = names(var_params()$barCats))
          t %>% pivot_wider(id_cols = "YEAR",
                                 names_from='CATEGORY',
                                 values_from = 'VALUE') %>% rowwise() %>%
            mutate(SUMMARY_VALUE = !!var_params()$summary_expression) %>%
            select(c("YEAR", "SUMMARY_VALUE"))
        }
      })
      
      # The y-axis range for the line chart
      lineRange <- reactive ({
        top_val <- max(selectedLine()$SUMMARY_VALUE, na.rm=TRUE)
        if (length(selected$groups) > 0) {
          cw_val <- max(var_data()$cs_df$SUMMARY_VALUE, na.rm=TRUE)
          if (cw_val > top_val) {top_val <- cw_val}
        }
        c(0, 1.1*top_val)
      })
      
      # "Bookend" is my term for the amount of padding to put on either end of
      # the line chart x-axis range in order for no data to be cut off
      var_xrange_bookend <- reactive({
        # 0.1 of the difference between the min and max year seems to be good
        0.1*(as.numeric(var_params()$end)-as.numeric(var_params()$start))
      })
      
      # Renders the line chart
      output$line_chart <- renderPlotly({
        
        p <- plot_ly(selectedLine(), 
                x = ~YEAR,
                y = ~SUMMARY_VALUE,
                hoverinfo = 'text'
        ) %>% 
          config(displayModeBar = FALSE) %>% # remove default plotly controls
          add_lines(color=I(my_line_color), hoverinfo = "y", name=selectionName(),
                    line=list(width = my_line_width, shape = 'spline', smoothing = 1),
                    # mode = 'lines+markers', marker = list(color=my_line_color, size=6)
                    )
        if (length(selected$groups) > 0) {
          p <- p %>% 
            add_lines(x = var_data()$cs_df$YEAR, y = var_data()$cs_df$SUMMARY_VALUE,
               hoverinfo="y", name="Citywide", color=I(my_line_color),
               line = list(dash='dash', shape = 'spline', smoothing = 1),
               
               )
        }
        p %>%
          add_markers(x = input$yearSelect, name = 'highlight', hoverinfo = "skip",
                      y = subset(selectedLine(), YEAR == input$yearSelect)$SUMMARY_VALUE,
                      marker = list(color=my_line_color, symbol="diamond", size=10), showlegend = F) %>%
          layout(yaxis = list(
                  title = '' 
                  , fixedrange = TRUE 
                  , tickprefix = var_params()$tickprefix 
                  , tickformat = var_params()$tickformat 
                  , hoverformat = var_params()$linehoverformat
                  , range = lineRange()
                  ), 
                 xaxis = list(
                   title = ''
                   , fixedrange = TRUE
                   , range = c(
                     as.numeric(var_params()$start) - var_xrange_bookend()
                     , as.numeric(var_params()$end) + var_xrange_bookend()
                     )
                   ), 
                 hovermode = "x unified",
                 title = var_params()$lineTitle,
                 margin = list(t=40),
                 legend = list(orientation = 'h', x=0.5, y=1.03),
                 font=list(color="black", family = APP_FONT, size = APP_FONT_SIZE-2)
                 )
      })
    }
  )
}

# Server ##############
#' Each geography type gets its own server so that they can act independently
server <- function(input, output, session) {
  lapply(names(all_vars_info), function(geo_type) {
    tabPanelServer(geo_type)
  })
}
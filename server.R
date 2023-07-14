# Define how the app works

# Server module function ##############
#' For each geography type tabPanel, the data display and interactions are basically 
#' all the same. This method defines those functionalities, namespacing by geography type.
tabPanelServer <- function(geo_type) {
  moduleServer(
    geo_type,
    function(input, output, session) {
      # string representation of the namespaced geography type (e.g. "tracts")
      geo_namespace <- substr(session$ns(''), 1, nchar(session$ns('')) - 1)
      
      # input$variable values are initialized in the UI with the year range
      # concatenated to each variable name. To extract the variable name from an 
      # input$variable string, we use a regex to find the last occurrence of a ( 
      # followed by a digit in the string and strip that part away.
      var_name <- reactive({
        idxs <- gregexpr("\\((\\d)", input$variable)
        idx <- idxs[[1]][length(idxs)]
        substr(input$variable, 1, idx - 2)
      })
      
      # Keep track of the data parameters for whichever variable the user has selected
      var_params <- reactive({
        ALL_VARS_INFO[[geo_namespace]][[var_name()]]
      })
      
      # Keep track of the data frames for whichever variable the user has selected
      var_data <- reactive({
        ALL_VARS_DATA[[geo_namespace]][[var_name()]]
      })
      
      # Static components of the map
      output$map <- renderLeaflet({
        leaflet() %>% 
          addProviderTiles(provider = "CartoDB.Positron", group='basemap') %>%
          
          # we create two panes with different z values to ensure that the polygons 
          # currently selected by the user are always displayed in front of other polygons
          addMapPane("layer1", zIndex=420) %>% addMapPane("layer2",zIndex=410) %>%
          
          # could be worth parameterizing the initial map center and zoom level at some point
          setView(-71.075, 42.318, zoom = 12)
      })
      
      # The label for map polygons with null values is a function of the geography type
      null_label <- lapply(
        split_max_2_lines(paste(
          tools::toTitleCase(geo_namespace), 'with little or no population')
        )
        , htmltools::HTML)
      
      # This makes it so that maps on inactive tabs stay rendered
      outputOptions(output, "map", suspendWhenHidden = FALSE) 
      
      # This defines how we draw all the map polygons when a variable is selected
      observeEvent(input$variable, { 
        ss <- var_data()$ss_df # for each variable, ss_df is the data frame that gets mapped
        yrdfs <- split(ss, ss$YEAR) # separate the dfs to create separate map layers for each year
        # this palette shades the polygons on a continuous scale that's consistent between years
        pal <- colorNumeric(MAP_PALETTE, domain = ss$SUMMARY_VALUE)
        
        leafletProxy("map") %>% clearShapes() %>% clearControls() # clear existing stuff before redrawing
        
        for (yr in names(yrdfs)) { # draw and add one layer of polygons for each year
          leafletProxy("map") %>% 
            addPolygons(data=yrdfs[[yr]],
              group=yr, # each layer is assigned its year as its group ID
              layerId = ~paste(GEOID, yr), # and each polygon is identified by its GEOID and year
              fillColor = ~pal(SUMMARY_VALUE), fillOpacity = 0.7, # polygon shading
              weight = 1, color = "gray", # polygon border formatting
              options = pathOptions(pane = "layer2"), # place these layers on the lower pane
              label = ~htmlEscape(NAME), # geography name displayed in a tooltip on hover 
              labelOptions = labelOptions( 
                style=list("font-size" = sprintf("%spx", APP_FONT_SIZE-2))
                ), # set font size for the tooltips a bit smaller than in the rest of app
              highlight = highlightOptions( 
                weight = 3,
                fillOpacity = 0.85,
                color = "black",
                bringToFront = TRUE
              ), # give these polygons a black border when you hover over them
            )
        } 
        # on top of the layers of polygons for each year, add a hidden layer of 
        # polygons that we'll set up to display when they're clicked on by users
        leafletProxy("map") %>%
          addPolygons(data=yrdfs[[yr]], 
            group=~GEOID, # for these polygons, we'll use the GEOID as the group name
            weight = 3, color = "red", fillOpacity=0, # no fill but red border
            options = pathOptions(pane = "layer1"), # place these polygons on the upper pane
            label = ~htmlEscape(NAME), # geography name displayed in a tooltip on hover 
            labelOptions = labelOptions( 
              style=list("font-size" = sprintf("%spx", APP_FONT_SIZE-2))
            ) # set font size for the tooltips a bit smaller than in the rest of app
          ) %>% hideGroup(group = yrdfs[[yr]]$GEOID) %>% # hide these polygons initially
          
          # add the map legend, formatting labels appropriately for the given variable
          addLegend_decreasing(values = ss$SUMMARY_VALUE, decreasing = TRUE,  
            position = "bottomright", pal = pal, na.label = null_label, 
            title = split_max_2_lines(var_params()$lineTitle),
            labFormat = labelFormat(
               prefix = var_params()$tickprefix, 
               # if the specified tick format is for percentages, add a % suffix
               suffix = ifelse(grepl("%", var_params()$tickformat, fixed = TRUE), "%", ""),
               transform = ifelse( # additionally (for percentages)...
                 grepl("%", var_params()$tickformat, fixed = TRUE), 
                 function(x) round(x*100), # ...multiply the data values by 100 when displaying...
                 function (x) x  # ...otherwise, display the data values as they are
                 )
               )
            )
      })
      
      # Keep track of the unique set of years for the variable the user selects
      var_years <- reactive({
        var_data()$cs_df$YEAR
      })
      
      # Updates the map When the user moves the time slider or picks a new variable
      observeEvent(list(input$yearSelect, input$variable), {
        leafletProxy("map") %>% 
          hideGroup(group = var_years()) %>% # hide all of the yearly layers
          showGroup(input$yearSelect) # then show the layer for the selected year
      })
      
      # This vector holds the IDs of all currently selected polygons. The line
      # and bar charts are configured to update in response to this vector
      selected <- reactiveValues(groups = vector())
      
      # This defines what happens when the user clicks on map polygons
      observeEvent(input$map_shape_click, {
        # To check whether the clicked polygon is currently selected or not, we 
        # check its group name. The group name of each unselected polygon is its YEAR,
        # whereas the group name of each selected polygon is its GEOID
        if(input$map_shape_click$group %in% var_years()){
          # The ID of each unselected polygon is a string concatenation of its GEOID 
          # and its YEAR. This gsub expression removes the last space-delimited word 
          # from the unselected polygon ID to extract the GEOID of the polygon.
          this_selection_id <- gsub("\\s*\\w*$", "", input$map_shape_click$id)
          
          # if the data for the clicked-upon polygon is all non-null values...
          if (!any(is.na(subset(var_data()$ss_df, GEOID == this_selection_id)$SUMMARY_VALUE))) {
            leafletProxy("map") %>% showGroup(group = this_selection_id) 
            # ...display that polygon as selected and record the polygon's ID in selected$groups
            selected$groups <- c(selected$groups, this_selection_id)
          }
          
        } else { # To deselect a polygon that's currently selected...
          leafletProxy("map") %>% hideGroup(group = input$map_shape_click$group)
          # display the polygon as unselected and remove that polygon from selected$groups
          selected$groups <- setdiff(selected$groups, input$map_shape_click$group)
        }
      })
      
      # In response to the user clearing all selections or choosing a new variable...
      observeEvent(list(input$clearSelections, input$variable), {
        selected$groups <- vector() # ...unselect all polygons and hide them on the map.
        leafletProxy("map") %>% hideGroup(group = var_data()$ss_df$GEOID)
      })
      
      # To update the slider input each time the user selects a different variable...
      observeEvent(input$variable, {
        st <- var_params()$start

        updateSliderTextInput(session, "yearSelect",
          # ...reset the choices on the slider based on the new variable's parameters...
          choices = seq(st, var_params()$end, by=var_params()$step),
          selected = st # ...and reset the slider back to the start.
          )
      })
      
      # Describes what the user currently has selected (e.g. "2 selected tracts")
      # This reactive is used for both the controls and the legend on the line chart
      selectionName <- reactive({
        sprintf("%s selected %s", length(selected$groups), geo_namespace)
      })
      
      # Dynamic text on the controls telling the user what data they are currently looking at
      output$selectionText <- reactive({
        msg <- "Currently viewing data for:<br>" 
        if (length(selected$groups) == 0) { # if nothing is currently selected,
          paste(msg, "<b>the whole city<b>") # it's citywide data
        } else { # but if at least one polygon is selected,
          paste(msg, paste0('<b>', selectionName(), '<b>')) # then we show selectionName() in bold
        }
      })
      
      # This is the data that is displayed on the bar chart. It's a function of 
      # the current variable and map selection as well as the year on the slider.
      filteredBar <- reactive({ # if the user doesn't have any polygons selected...
        if (length(selected$groups) == 0) { # ...show citywide data for the given year
          subset(var_data()$cb_df, YEAR == input$yearSelect) 
        } else if (length(selected$groups) == 1) { # if only one polygon is selected...
          # ...use the subcity binned data filtered to that polygon
          subset(var_data()$sb_df, GEOID %in% selected$groups & YEAR == input$yearSelect)
        } else { # If multiple polygons are selected...
          # ...we aggregate the data for the set of selected polygons for each year...
          data <- subset(var_data()$sb_df, GEOID %in% selected$groups & YEAR == input$yearSelect) %>%
            group_by(CATEGORY) %>% # ...by category, using the agg_func that's defined for the given variable
            summarise(VALUE = var_params()[["agg_func"]](VALUE))
          if (nrow(data) == 0) { # if there's missing data for a given year...
            data <- subset(var_data()$cb_df, YEAR == input$yearSelect)
            data$VALUE <- 0 # ... display the bar chart with all 0s for the given categories
          }
          return(data)
        }
      })
      
      # The y-axis range for the bar chart is a function of the map selection, but
      # remains constant for a given selection over a range of years
      barRange <- reactive({
        if (length(selected$groups) == 0) {
          data <- var_data()$cb_df # if nothing is selected, use citywide data
        }
        else { # otherwise, consider all the data values by category and year
          data <- subset(var_data()$sb_df, GEOID %in% selected$groups) %>%
            group_by(CATEGORY, YEAR) %>% 
            summarise(VALUE = var_params()[["agg_func"]](VALUE), .groups="drop")
        }
        # The y axis range is set according to the maximum data value for all years
        # Multiplying by 1.1 adds some padding between the max value and the top of the chart
        return(c(0, 1.1*max(data$VALUE, na.rm = TRUE)))
      })
      
      # Define how we render the bar chart at any given time
      output$bar_chart <- renderPlotly({
        plot_ly(filteredBar(),
                x = ~CATEGORY, 
                y = ~VALUE, 
                hoverinfo = 'text'
                ) %>% 
          config(displayModeBar = FALSE) %>% # remove default plotly controls
          add_bars(color=I(BAR_COLOR), # set the fill color for the bars
                   marker = list(line = list(color=BAR_COLOR)), # set bar outline color
                   hoverinfo = 'y' # display y-values when hovering over bars
                   ) %>% 
          layout(title = paste0(var_params()$barTitle, " in ", input$yearSelect), # dynamic title
                 font=list(color="black", family = APP_FONT, size=APP_FONT_SIZE - 2),
                 xaxis = list(title = '', fixedrange = TRUE,
                              # set the order in which categories appear on the x axis
                              categoryorder = 'array', categoryarray = names(var_params()$barCats)
                              ),
                 yaxis = list(title = '' , fixedrange = TRUE, range = barRange(), 
                              hoverformat = var_params()$barhoverformat
                              ), 
                 hoverlabel = list(bordercolor = 'white', # hover text formatting options
                                   font = list(color="white", size=APP_FONT_SIZE-2)
                                   ),
                 annotations = list(# if a "note" has been specified for a given
                                    # variable, display the note as an annotation
                                    text=ifelse(is.null(var_params()$note),"",var_params()$note),
                                    xref = 'paper', x = 0.5, y=barRange()[2], # annotation placement
                                    showarrow=FALSE, font = list(size = APP_FONT_SIZE - 4) 
                                    ),
                 margin = list(t=55) # top padding to make sure chart title is visible
                 )
      })
      
      # This is the data that is displayed on the line chart. It's a function of 
      # the current variable and map selection.
      selectedLine <- reactive({ # if the user doesn't have any polygons selected...
        if (length(selected$groups) == 0) {var_data()$cs_df} # ...show citywide data
        else if (length(selected$groups) == 1) { # if only 1 polygon is selected...
          # ...use the subcity summary data filtered to that polygon
          subset(var_data()$ss_df, GEOID %in% selected$groups)
        }
        else { # If multiple polygons are selected...
          
          # we start by aggregating the binned subcity data for those polygons
          t <- subset(var_data()$sb_df, GEOID %in% selected$groups) %>%
            group_by(CATEGORY, YEAR) %>% # ...by category and year
            summarise(VALUE = var_params()[["agg_func"]](VALUE), .groups = "drop")
          
          # then, we convert the category values from their labels to their identifiers...
          t$CATEGORY <- plyr::mapvalues(t$CATEGORY,
                                        to = unname(var_params()$barCats),
                                        from = names(var_params()$barCats))
          
          # ...so that once we pivot the categories into their own columns...
          t %>% pivot_wider(id_cols = "YEAR",
                                 names_from='CATEGORY',
                                 values_from = 'VALUE') %>% 
            # ...we can use the variable's summary_expression (which uses category identifiers)...
            rowwise() %>% mutate(SUMMARY_VALUE = !!var_params()$summary_expression) %>%
            # ...to compute the yearly summary values for the selected polygons.
            select(c("YEAR", "SUMMARY_VALUE"))
        }
      })
      
      # The y-axis range for the line chart is fixed for a given variable and map selection,
      # but if we want to display a citywide comparison line, the range shouldn't cut that off
      lineRange <- reactive ({
        # the default upper value of the line chart y axis is the maximum summary value for the selection
        top_val <- max(selectedLine()$SUMMARY_VALUE, na.rm=TRUE)
        
        # but if we want to show a citywide comparison... 
        if (length(selected$groups) > 0) {
          cw_val <- max(var_data()$cs_df$SUMMARY_VALUE, na.rm=TRUE)
          # ...and the maximum summary value citywide is bigger than teh default upper value...
          if (cw_val > top_val) {top_val <- cw_val} # ... we use the citywide max as the top.
        }
        c(0, 1.1*top_val) # multiplying te top value by 1.1 gives a little padding at the top
      })
      
      # "Bookend" is my term for the amount of padding to put on each side of
      # the line chart x-axis range to ensure that no data points are visually cut off
      line_xrange_bookend <- reactive({
        # 0.1 of the difference between the min and max year seems to be a good bookend
        0.1*(as.numeric(var_params()$end)-as.numeric(var_params()$start))
      })
      
      # Define how we render the bar chart at any given time
      output$line_chart <- renderPlotly({
        p <- plot_ly(selectedLine(), 
                x = ~YEAR,
                y = ~SUMMARY_VALUE,
                hoverinfo = 'text'
        ) %>% 
          config(displayModeBar = FALSE) %>% # remove default plotly controls
          add_lines(color=I(LINE_COLOR), # set the line formatting options
                    line=list(width = 2, shape = 'spline', smoothing = 1),
                    hoverinfo = "y", # display y-values when hovering over line chart
                    name=selectionName() # set the series name for the line
                    ) %>%
          layout(title = var_params()$lineTitle,
                 font=list(color="black", family = APP_FONT, size = APP_FONT_SIZE-2),
                 xaxis = list(title = '', fixedrange = TRUE,
                              range = c(
                                as.numeric(var_params()$start) - line_xrange_bookend(),
                                as.numeric(var_params()$end) + line_xrange_bookend()
                                )
                              ),
                 yaxis = list(title = '', fixedrange = TRUE, range = lineRange(),
                              # set the number format for the data that appear on hover...
                              hoverformat = var_params()$linehoverformat,
                              # ... and the number formatting for the y axis labels
                              tickprefix = var_params()$tickprefix, 
                              tickformat = var_params()$tickformat
                              ),
                 legend = list(orientation = 'h', # place legend items side by side
                               x=0.3, y=1.03 # position legend near the top center
                               ), # legend will only appear if there are multiple lines
                 hovermode = "x unified", # show the data values for all lines upon hover
                 margin = list(t=40) # top padding to make sure chart title is visible
          )
        
        if (length(selected$groups) > 0) { # if at least one polygon is selected...
          p <- p %>% # ...add a dashed line showing the citywide comparison for the summary value over time
            add_lines(x = var_data()$cs_df$YEAR, y = var_data()$cs_df$SUMMARY_VALUE,
               hoverinfo="y", name="Citywide", color=I(LINE_COLOR),
               line = list(dash='dash', shape = 'spline', smoothing = 1)
               )
        }
        
        # we also add a marker highlighting the summary value for the year currently on the slider...
        marker_y_data <- subset(selectedLine(), YEAR == input$yearSelect)
        if (nrow(marker_y_data) > 0) { # ...as long as there isn't missing data for that year.
          p <- p %>%
            add_markers(x = input$yearSelect, y = marker_y_data$SUMMARY_VALUE,
                        marker = list(color=LINE_COLOR, symbol="diamond", size=10), showlegend = F,
                        hoverinfo = "skip" # we already have hoverinfo for the lines, so no need for the marker
                        )           
        }
        return(p)
      })
    }
  )
}

# Server ##############
#' Each geography type gets its own module server so that they can act independently
server <- function(input, output, session) {
  
  # show the about page modal dialog when the user clicks the "About" button
  observeEvent(input$about, {
    showModal(ABOUT_PAGE)
  })
  
  # build the module servers for each geography type in ALL_VARS_INFO
  lapply(names(ALL_VARS_INFO), function(geo_type) {
    tabPanelServer(geo_type)
  })
}
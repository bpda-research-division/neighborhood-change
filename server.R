# Define how the app works

# Server module function ##############
#' For each geography type tabPanel, the components and interactions are basically all the
#' same. This method defines those functionalities, namespacing components by geography type.
tabPanelServer <- function(geo_type) {
  moduleServer(
    geo_type,
    function(input, output, session) {
      # string representation of the namespaced geography type (e.g. "census tracts")
      geo_units <- gsub("_"," ", session$ns(''))
      geo_unit <- substr(geo_units, 1, nchar(geo_units) - 1)
      
      # geographic features for the namespaced geography type
      geo_shapes <- APP_CONFIG[[geo_unit]]$geoms
      
      # Keep track of the unique years of data we have for each topic
      variables_years <- APP_DATA[[geo_unit]] %>%
        lapply(function(var) unique(var$areas_categories_df$YEAR) %>% sort())
      
      # Keep track of the full list of topic display names that can appear in the "Choose a topic:" menu
      topic_display_names <- names(APP_CONFIG[[geo_unit]]$topics) %>%
        lapply(function (n) { # display each variable with its start and end year
          paste0(n, " (", variables_years[[n]][1], "-", tail(variables_years[[n]], 1), ")")
        })
      
      # Topic display names are initialized with the year range concatenated to each variable name.
      # To extract the currently selected topic name from input$topicSelect... 
      topic_name <- reactive({ # ...we use a regex to find the last occurrence of a ( followed by a digit...
        idxs <- gregexpr("\\((\\d)", input$topicSelect)
        idx <- idxs[[1]][length(idxs)]
        topicName <- substr(input$topicSelect, 1, idx - 2) # ...and strip that part away.
        topicName
      })
      
      # Keep track of the parameters for whichever topic is selected
      var_params <- reactive({
        APP_CONFIG[[geo_unit]]$topics[[topic_name()]]
      })
      
      # Keep track of the parameters for whichever indicator is selected...
      indicator_params <- reactive({ 
        # ...making sure that the indicator is updated before we start accessing parameters
        req(input$indicatorSelect %in% names(var_params()$summary_indicators))
        var_params()$summary_indicators[[input$indicatorSelect]]
      })
      
      # Keep track of the data frames for whichever topic is selected...
      var_data <- reactive({
        # ...making sure that the indicator is updated before we start accessing data
        req(input$indicatorSelect %in% names(var_params()$summary_indicators))
        APP_DATA[[geo_unit]][[topic_name()]]
      })
      
      # this df has one value for each year and category for each area
      areas_categories_df <- reactive({
        var_data()$areas_categories_df
      })
      
      # this df has one value for each year and category for the entire place
      totalarea_categories_df <- reactive({
        if ("totalarea_categories_df" %in% names(var_data())) {var_data()$totalarea_categories_df}
        else {
          areas_categories_df() %>% 
            group_by(YEAR, CATEGORY) %>% 
            summarise(VALUE = sum(VALUE, na.rm=TRUE), .groups='drop')
        }
      })
      
      # this df has one summary value for each year and geography
      areas_summary_df <- reactive({
        
        pivot_summarise(
          df = areas_categories_df(), 
          cats = var_params()$barCats, 
          summary_expr = indicator_params()$summary_expression, 
          id_columns = c("YEAR", "GEOID", "NAME")
        ) %>% # add geometries to the subcity summary data so we can map them
          mutate(GEOID = as.character(GEOID)) %>%
          merge(y=geo_shapes, by.y = "GEOID", by.x = "GEOID") %>%
          st_as_sf()
      })
      
      # this df has one summary value for each year for the entire place
      totalarea_summary_df <- reactive({
        pivot_summarise(
          df = totalarea_categories_df(), 
          cats = var_params()$barCats, 
          summary_expr = indicator_params()$summary_expression, 
          id_columns = "YEAR"
        )
      })
      
      # Set up static components of the map
      output$map <- renderLeaflet({
        leaflet() %>% 
          addProviderTiles(provider = "CartoDB.Positron", group='basemap') %>%
          
          # we create two panes with different z values to ensure that the polygons 
          # currently selected by the user are always displayed in front of other polygons
          addMapPane("layer1", zIndex=420) %>% addMapPane("layer2",zIndex=410) %>%
          
          # would be nice to parameterize the initial map center and zoom level someday
          setView(-71.075, 42.318, zoom = 12)
      })
      
      # The label for map polygons with null values is a function of the geography type and topic
      null_label <- reactive({
        
        if ("null_description" %in% names(var_params())) {
          null_description <- var_params()$null_description
        } else {
          null_description <- "null values"
        }
        
        lapply(
          split_max_2_lines(paste(
            tools::toTitleCase(geo_unit), 'with', null_description)
          )
          , htmltools::HTML
        )
      })
      
      # Make it so that maps on inactive tabs stay rendered
      outputOptions(output, "map", suspendWhenHidden = FALSE) 
      
      # Redraw all the map polygons when a new variable or indicator is selected
      observeEvent(input$indicatorSelect, { 
        label_format_digits <- as.numeric(gsub(".*?([0-9]+).*", "\\1", indicator_params()$hoverformat))
        
        # for each variable, areas_summary_df is the simple features dataframe that gets mapped
        ss <- areas_summary_df() %>% mutate(
          labelText = paste0( # we add a column to ss with the tooltip label formatted how we want
            "<center>", NAME, # the name of the geographic area always appears on the tooltip
            case_when( # If the summary value is not null, we add a second line to the tooltip...
              !is.na(SUMMARY_VALUE) ~ paste0(
                "<br>", 
                indicator_params()$tickprefix, # ...containing the summary value formatted according to our parameters
                case_when(grepl("%", indicator_params()$tickformat, fixed=TRUE) ~ as.character(round(SUMMARY_VALUE*100, digits = label_format_digits)), 
                          .default = format(round(SUMMARY_VALUE, digits = label_format_digits), big.mark=",", trim=TRUE)
                          ),
                case_when(grepl("%", indicator_params()$tickformat, fixed=TRUE) ~ "%", .default = "")
                ), 
              .default = "" # If the summary value is null, the tooltip just displays the geographic area name
              )
          )
        )
        
        # for any area with a null value in any of the years...
        areas_with_nulls <- c(
          unique(ss[is.na(ss$SUMMARY_VALUE),]$GEOID),
          var_params()$additional_null_geoms # ...or areas which have been specified as additional null geoms...
        )
        if (length(areas_with_nulls) > 0) {
          ss$SUMMARY_VALUE[which(ss$GEOID %in% areas_with_nulls)] <- NA
        } # ...temporarily replace the value with a NA for the purposes of mapping (to gray the polygon out)
        
        yrdfs <- split(ss, ss$YEAR) # split the data on YEAR to create separate map layers for each year
        
        # Shade the polygons on a single continuous scale rather than using new ranges for each year
        pal <- colorNumeric(MAP_PALETTE, domain = ss$SUMMARY_VALUE)
        
        leafletProxy("map") %>%
          clearShapes() %>% clearControls() %>% # clear existing stuff before redrawing...
          addControl( # ...but make sure each map has a button for users to clear their map selections...
            actionButton(session$ns("clearSelections"), "Clear all selections",
                         style=sprintf("font-size:%spx", APP_FONT_SIZE)),
            position="topright", className = "fieldset {border: 0;}" # remove default button border
          ) %>%
          addControl( # ...as well as a data download button.
            actionButton(session$ns("downloadButton"), "Download selected data", icon = icon("download"),
                         style=sprintf("font-size:%spx", APP_FONT_SIZE)),
            position="bottomright", className = "fieldset {border: 0;}" # remove default button border
          )
        
        for (yr in names(yrdfs)) { # Draw and add one layer of polygons for each year
          leafletProxy("map") %>% 
            addPolygons(data=yrdfs[[yr]],
              group=yr, # each layer is assigned to a group ID corresponding to its year...
              layerId = ~paste(GEOID, yr), # ...and each polygon is identified by its GEOID and year
              fillColor = ~pal(SUMMARY_VALUE), fillOpacity = 0.7, # polygon shading
              weight = 1, color = "gray", # polygon border formatting
              options = pathOptions(pane = "layer2"), # place these layers on the lower pane
              label = ~lapply(labelText, htmltools::HTML), # custom label text displayed in a tooltip on hover 
              labelOptions = labelOptions( 
                style=list("font-size" = sprintf("%spx", APP_FONT_SIZE-2))
                ), # set font size for the tooltips a bit smaller than in the rest of app
              highlight = highlightOptions( # when you hover over these polygons...
                weight = 3,
                fillOpacity = 0.7,
                color = "black", # ...given them a black border of weight 3 so they "pop out"
                bringToFront = TRUE
              ),
            ) %>% hideGroup(group = yr) # hide each yearly layer after initializing it
        } 
        
        # prevent topic-switching from inadvertently selecting multiple areas for destination topics where that's disabled
        if ("disable_multiselection" %in% names(indicator_params()) & length(selectedPolygons$groups) > 1) {
          selectedPolygons$groups <- c() 
        }
        
        # prevent topic-switching from inadvertently selecting an area that has null data for the destination topic
        selectedPolygons$groups <- setdiff(selectedPolygons$groups, areas_with_nulls)
        
        # on top of the layers of polygons for each year, add a hidden layer of 
        # polygons that will be displayed when they're clicked on by users
        leafletProxy("map") %>%
          addPolygons(data=yrdfs[[yr]], 
            group=~GEOID, # for these polygons, we'll use the GEOID as the group name
            weight = 3, color = "red", fillOpacity=0, # no fill but red border
            options = pathOptions(pane = "layer1"), # place these polygons on the upper pane
            label = ~lapply(labelText, htmltools::HTML), # custom label text displayed in a tooltip on hover 
            labelOptions = labelOptions( 
              style=list("font-size" = sprintf("%spx", APP_FONT_SIZE-2))
            ) # set font size for the tooltips a bit smaller than in the rest of app
          ) %>% hideGroup(group = yrdfs[[yr]]$GEOID) %>% # hide these polygons initially
          
          # add the map legend, formatting labels as specified in APP_CONFIG for the given variable 
          addLegend_decreasing(values = ss$SUMMARY_VALUE, decreasing = TRUE,  
            position = "bottomright", pal = pal, na.label = null_label(), 
            title = split_max_2_lines(input$indicatorSelect),
            labFormat = labelFormat(
               prefix = indicator_params()$tickprefix, 
               # if the specified tick format is for percentages, add a % suffix
               suffix = ifelse(grepl("%", indicator_params()$tickformat, fixed = TRUE), "%", ""),
               transform = ifelse( # additionally (for percentages)...
                 grepl("%", indicator_params()$tickformat, fixed = TRUE), 
                 function(x) round(x*100), # ...multiply the data values by 100 when displaying...
                 identity  # ...otherwise, display the data values as they are
                 )
               )
            ) %>% 
          showGroup(group = yr) %>% # The initial map display for a new topic is the most recent year of data...
          showGroup(selectedPolygons$groups) # ...with any previously selected geographies still selected.
      })
      
      # Keep track of the unique set of years for the variable the user selects
      var_years <- reactive({
        unique(APP_DATA[[geo_unit]][[topic_name()]]$areas_categories_df$YEAR) %>% sort()
      })
      
      # Update the map When the user moves the time slider or picks a new variable
      observeEvent(input$yearSelect, {
        leafletProxy("map") %>% 
          hideGroup(group = var_years()) %>% # hide whatever year is currently selected
          showGroup(input$yearSelect) # then show the layer for the selected year
      })
      
      # This vector holds the IDs of all currently selected polygons. The line and
      # bar charts are configured to dynamically update in response to this vector
      selectedPolygons <- reactiveValues(groups = vector())
      
      # Define what happens when the user clicks on map polygons
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
          if (!any(is.na(subset(areas_summary_df(), GEOID == this_selection_id)$SUMMARY_VALUE))) {
            # ...depending on the indicator parameters, we can either do multiselection...
            if (!"disable_multiselection" %in% names(indicator_params())) {
              leafletProxy("map") %>% showGroup(group = this_selection_id) 
              # ...displaying that polygon as selected and appending thepolygon's ID to selectedPolygons$groups
              selectedPolygons$groups <- c(selectedPolygons$groups, this_selection_id)
            } else { # ...or we can do single selection...
              leafletProxy("map") %>% hideGroup(group = selectedPolygons$groups) # ...hiding the previous selection...
              leafletProxy("map") %>% showGroup(group = this_selection_id) # ...and showing the current one...
              selectedPolygons$groups <- c(this_selection_id) # ...and updating selectedPolygons$groups accordingly.
            }
          }
          
        } else { # To deselect a polygon that's currently selected...
          leafletProxy("map") %>% hideGroup(group = input$map_shape_click$group)
          # display the polygon as unselected and remove that polygon from selectedPolygons$groups
          selectedPolygons$groups <- setdiff(selectedPolygons$groups, input$map_shape_click$group)
        }
      })
      
      # In response to the user clearing all selections or choosing a new variable...
      observeEvent(session$input$clearSelections, {
        leafletProxy("map") %>% hideGroup(group = selectedPolygons$groups)
        selectedPolygons$groups <- vector() # ...unselect all polygons and hide them on the map.
      })
      # To update the specific topic selection menu each time the user selects a different general topic...
      observeEvent(input$generalTopicSelect, {

        selected_gt <- input$generalTopicSelect
        if (length(selected_gt) > 0) { # ...identify the topics that match the selected set of general topics...
          selected <- APP_CONFIG[[geo_unit]]$topics %>%
            lapply(function(topic) {topic$generalTopic %in% selected_gt}) %>%
            unlist()
          selectedTopics <- topic_display_names[selected] # ...and filter out the other topics' names...
        }
        else { # ...(unless no general topics are selected, in which case, show all possible topic names)...
          selectedTopics <- topic_display_names
        } # ...so that we can update the "Choose a topic:" menu to only show topics matching selected general topics
        updateSelectInput(session, "topicSelect", choices=selectedTopics, selected=selectedTopics[[1]])
      }, ignoreNULL = FALSE)
      
      # Each time the user selects a different topic...
      observeEvent(input$topicSelect, {
        indicators <- names(var_params()$summary_indicators) # ...update the list of available indicators...
        updateSelectInput(session, "indicatorSelect", choices=indicators, selected=indicators[[1]])
        updateSliderTextInput(session, "yearSelect",
          # ...reset the choices on the slider based on the new topic's parameters...
          choices = var_years(),
          selected = tail(var_years(), 1) # ...and reset the slider to start on the most recent year of data
          )
      })
      
      # Describes the geographic scope of the current map selection (e.g. "2 selected tracts" or "Citywide")
      selectionName <- reactive({ # this is used for the legends on the bar and line charts
        geo_type <- geo_unit
        num_selected <- length(selectedPolygons$groups)
        
        if (num_selected == 0) { # if nothing is selected, we're showing citywide data
          return("Citywide")
        } else if (num_selected == 1) { # if only one polygon is selected, lop the "s" off the geo type
          geo_type <- substr(geo_type, 1, nchar(geo_type) - 1)
        }
        sprintf("%s selected %s", num_selected, geo_type) # show the current number of selected areas
      })
      
      # The data that goes into data downloads / onto the bar chart for a given topic & geo selection
      selectionData <- reactive({ # if the user doesn't have any polygons selected...
        if (length(selectedPolygons$groups) == 0) { # ...use citywide data for the given year
          data <- totalarea_categories_df() 
        } else if (length(selectedPolygons$groups) == 1) { # if only one polygon is selected...
          # ...use the subcity binned data filtered to that polygon
          data <- subset(areas_categories_df(), GEOID %in% selectedPolygons$groups)
        } else { # If multiple polygons are selected...
          # ...we aggregate the data for the set of selected polygons for each year...
          data <- subset(areas_categories_df(), GEOID %in% selectedPolygons$groups) %>%
            group_by(CATEGORY, YEAR) %>% # ...by category
            summarise(VALUE = sum(VALUE), .groups='drop')
        }
        
        return( # only return the categories that have been named in the barCats parameter
          data %>% filter(CATEGORY %in% names(var_params()$barCats))
        )
      })
      
      # The y-axis range for the bar chart is a function of the topics and geo selection...
      barRange <- reactive({ # ...but remains constant for a given selection over a range of years
        # Multiplying by 1.25 adds some padding between the max value and the top of the chart
        return(c(0, 1.25*max(selectionData()$VALUE, na.rm = TRUE)))
      })
      
      # The data that is displayed on the bar chart for a given topic, geo selection, and year
      filteredBar <- reactive({
        data <- selectionData() %>% subset(YEAR == input$yearSelect)
        
        if (nrow(data) == 0) { # if there's missing data for a given year...
          data <- totalarea_categories_df() %>% filter(CATEGORY %in% names(var_params()$barCats)) %>% subset(YEAR == input$yearSelect)
          data$VALUE <- 0 # ... display the bar chart with all 0s for the given categories
        }
        
        data
      })
      
      bar_font_size <- reactive({
        if ("smallbarfont" %in% names(var_params())) {
          return(APP_FONT_SIZE - 6)
        } else {
          return(APP_FONT_SIZE - 2)
        }
      })
      
      # Define how we render the bar chart at any given time
      output$bar_chart <- renderPlotly({
        plot_ly(filteredBar(),
                x = ~CATEGORY, 
                y = ~VALUE,
                name = selectionName(), # this gets displayed on the chart legend
                text = ~VALUE, # data label for each bar, formatted according to our topic parameters
                texttemplate=paste0(var_params()$bartickprefix, '%', sprintf('{y:%s}', var_params()$barhoverformat)),
                textposition = 'outside', # for a bar chart, "text" is the labeling directly above each bar
                textfont=list(color="black", family = APP_FONT, size=APP_FONT_SIZE - 2),
                hoverinfo = 'skip' # to enable values to be displayed on hover, use 'y' here and uncomment stuff below
                ) %>% 
          config(displayModeBar = FALSE) %>% # remove default plotly controls
          add_bars(color=I(BAR_COLOR), # set the fill color for the bars
                   marker = list(line = list(color=BAR_COLOR)) #, # set bar outline color
                   # hoverinfo = 'y' # display y-values when hovering over bars
                   ) %>% 
          layout(title = list(
                    text = paste0(var_params()$barTitle, " in ", input$yearSelect), # dynamic title
                    font=list( # for the chart title
                      color="black", family = APP_FONT, 
                      size=APP_FONT_SIZE + 4
                    )
                  ),
                 font=list( # for the labels underneath each bar
                   color="black", family = APP_FONT, 
                   size=bar_font_size()
                   ),
                 xaxis = list(title = '', fixedrange = TRUE,
                              # set the order in which categories appear on the x axis
                              categoryorder = 'array', categoryarray = names(var_params()$barCats)
                              ),
                 yaxis = list(title = '' , fixedrange = TRUE, range = barRange(), 
                              # hoverformat = var_params()$barhoverformat,
                              # tickprefix = var_params()$bartickprefix, 
                              showticklabels = FALSE, showgrid = FALSE
                              ), 
                 # hoverlabel = list(bordercolor = 'white', # hover text formatting options
                 #                   font = list(color="white", size=APP_FONT_SIZE-2)
                 #                   ),
                 showlegend=T, # always show the legend (even with just 1 series) 
                 legend = list(orientation = 'h', # place legend items side by side
                               x=0.01, y=1.05, # position legend near the top left
                               font=list(
                                 color="black", family = APP_FONT, 
                                 size=APP_FONT_SIZE - 2
                               ),
                               itemclick = FALSE, itemdoubleclick = FALSE # disable default plotly clicking options
                               ),
                 margin = list(t=50) # top padding to make sure chart title is visible
                 )
      })
      
      # Displays any note that's been provided for the variable in APP_CONFIG
      output$note <- reactive({ var_params()$note })
      
      # This is the data that is displayed on the line chart. It's a function of topic and geo selection
      selectedLine <- reactive({ # if the user doesn't have any polygons selected...
        if (length(selectedPolygons$groups) == 0) {totalarea_summary_df()} # ...show citywide data.
        else if (length(selectedPolygons$groups) == 1) { # if only 1 polygon is selected...
          # ...use the subcity summary data frame, filtered to that polygon
          subset(areas_summary_df(), GEOID %in% selectedPolygons$groups)
        }
        else { # If multiple polygons are selected...

          # we start by aggregating the subcity binned data for those polygons...
          t <- subset(areas_categories_df(), GEOID %in% selectedPolygons$groups) %>%
            group_by(CATEGORY, YEAR) %>% # ...by category and year
            summarise(VALUE = sum(VALUE), .groups = "drop")

          # then, we convert the category values from their labels to their aliases...
          # t$CATEGORY <- plyr::mapvalues(t$CATEGORY,
          #                               to = unname(var_params()$barCats),
          #                               from = names(var_params()$barCats))
          t$CATEGORY <- t$CATEGORY %>% recode(!!!var_params()$barCats)

          # ...so that once we pivot the categories into their own columns...
          t %>% pivot_wider(id_cols = "YEAR",
                                 names_from='CATEGORY',
                                 values_from = 'VALUE') %>% 
            # ...we can use the variable's summary_expression (which uses category aliases)...
            rowwise() %>% mutate(SUMMARY_VALUE = !!indicator_params()$summary_expression) %>%
            # ...to compute the yearly summary values for the selected polygons.
            select(all_of(c("YEAR", "SUMMARY_VALUE")))
        }
      })
      
      # The y-axis range for the line chart is fixed for a given variable and map selection,
      # but if we want to display a citywide comparison line, the range shouldn't cut that off
      lineRange <- reactive ({
        # the default upper value of the line chart y axis is the maximum summary value for the selection
        top_val <- max(selectedLine()$SUMMARY_VALUE, na.rm=TRUE)
        
        # but if we want to show a citywide comparison...
        if (length(selectedPolygons$groups) > 0 & indicator_params()$citywide_comparison) {
          cw_val <- max(totalarea_summary_df()$SUMMARY_VALUE, na.rm=TRUE)
          # ...and the maximum summary value citywide is bigger than the default upper value...
          if (cw_val > top_val) {top_val <- cw_val} # ... we use the citywide max as the top.
        }
        c(0, 1.1*top_val) # multiplying the top value by 1.1 gives a little padding at the top
      })
      
      # "Bookend" is my term for the amount of padding to put on each side of the
      # line chart x-axis range to ensure that no data points are visually cut off
      line_xrange_bookend <- reactive({
        # 0.1 of the difference between the min and max year seems to work decently well
        0.1*(as.numeric(tail(var_years(), 1))-as.numeric(var_years()[1]))
      })
      
      # Define how we render the line chart at any given time
      output$line_chart <- renderPlotly({
        p <- plot_ly(selectedLine(), 
                x = ~YEAR,
                y = ~SUMMARY_VALUE,
                source = session$ns("line_chart"), # this label helps us manage click events
                hoverinfo = 'text' # enable text to be displayed on hover
        ) %>% 
          config(displayModeBar = FALSE) %>% # remove default plotly controls
          add_trace(hoverinfo='x', mode = 'markers', type = 'scatter', # add a hidden trace for displaying years on hover
                    marker=list(size=0, color='white'), showlegend=FALSE, opacity=0) %>%
          add_lines(color=I(LINE_COLOR), # set the line formatting options
                    line=list(width = 2, shape = 'spline', smoothing = 1),
                    hoverinfo = "y", # display y-values when hovering over line chart
                    name=selectionName() # set the series name for the line (appears in legend)
                    ) %>%
          layout(title = input$indicatorSelect,
                 font=list(color="black", family = APP_FONT, size = APP_FONT_SIZE-2),
                 xaxis = list(title = '', fixedrange = TRUE,
                              range = c( # set x axis range a little wider than the data so as to not cut off text
                                as.numeric(var_years()[1]) - line_xrange_bookend(),
                                as.numeric(tail(var_years(), 1)) + line_xrange_bookend()
                                )
                              ),
                 yaxis = list(title = '', fixedrange = TRUE, range = lineRange(),
                              # set the number format for the data that appear on hover...
                              hoverformat = indicator_params()$hoverformat,
                              # ... and the number formatting for the y axis labels
                              tickprefix = indicator_params()$tickprefix, 
                              tickformat = indicator_params()$tickformat
                              ),
                 showlegend = T, # always show the legend (even with just 1 series)
                 legend = list(orientation = 'h', # place legend items side by side
                               x=0.01, y=1.05, # position legend near the top left
                               itemclick = FALSE, itemdoubleclick = FALSE
                               ), 
                 hovermode = "x unified", # show the data values for all lines when hovering over a given x
                 margin = list(t=40) # top padding to make sure chart title is visible
          )
        
        # if at least one polygon is selected and the given variable is configured to show citywide comparisons...
        if (length(selectedPolygons$groups) > 0 & indicator_params()$citywide_comparison) { 
          p <- p %>% # ...add a dashed line showing the citywide comparison for the summary value over time
            add_lines(x = totalarea_summary_df()$YEAR, y = totalarea_summary_df()$SUMMARY_VALUE,
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
        line_loaded(TRUE) # change line_loaded() from false to true when line chart is ready to be clicked upon
        p
      })
      
      # initializing this as FALSE initially and requiring it before event_data() prevents plotly click warnings
      line_loaded <- reactiveVal(value = FALSE)
      
      # This returns the year (x axis value) for any clicks that users make on the line chart
      clickedYear <- reactive({
        req(line_loaded()) # event_data() triggers a warning if it's called before the relevant plot is loaded
        event_data("plotly_click", source=session$ns("line_chart"))[['x']][1]
      })
      
      # This updates the slider anytime the user clicks on the line chart to select a year
      observeEvent(clickedYear(), {
        updateSliderTextInput(session, "yearSelect", selected = clickedYear())
      })
      
      # Display the data source citation for whatever variable is selected
      output$sourceText <- reactive({
        paste("Source:", var_params()$source)
      })
      
      # Displays a modal (pop up window) in response to user clicking download button
      observeEvent(input$downloadButton, {
        showModal(modalDialog(
          HTML(
            paste(
              "<b>Topic:</b>", topic_name(), "<br>", 
              "<b>Geographic extent:</b>", selectionName(), selectedAreas(), "<br>",
              "<b>Variable:</b>", input$indicatorSelect
              )
          ), # the modal displays download details, and then the user can either cancel or proceed downloading
          title=h2("Confirm download details", align='center'),
          easyClose = TRUE,
          footer = tagList(modalButton("Cancel"),
                           downloadButton(session$ns("downloadData"), "Download .csv file")
                           
          )
        ))
      })
      
      # String representation of the currently selected areas (used in downloads)
      selectedAreas <- reactive({
        ifelse(
          length(selectedPolygons$groups) > 0, 
          paste0("(", toString(selectedPolygons$groups), ")"),
          "")
      })
      
      # Performs the data download in response to user confirming their download
      output$downloadData <- downloadHandler(
        filename = function() {
          "NeighborhoodChangeExplorer_download.csv"
        },
        content = function(out_file) {
          removeModal() # close confirmation window
          output <- selectionData() %>% 
            pivot_wider(id_cols = "YEAR", names_from='CATEGORY', values_from='VALUE') %>%
            subset(select = c("YEAR", names(var_params()$barCats))) %>%
            # downloaded data includes bar chart categories + the selected indicator for each year
            merge(selectedLine() %>% select(YEAR, SUMMARY_VALUE) %>% st_drop_geometry(), by='YEAR') %>%
            # rename the SUMMARY_VALUE column to the name of the selected indicator
            mutate(YEAR = as.character(YEAR), !!input$indicatorSelect := SUMMARY_VALUE, .keep='unused')
          
          list_of_areas <- ifelse(
            length(selectedPolygons$groups) > 0, 
            paste0("(", toString(selectedPolygons$groups), ")"),
            "")
            
          o <- as.data.frame(
            rbind( # paste a description and citation of the data on the first few lines of the output csv file
              c(paste("Topic:", topic_name()),rep(NA,ncol(output)-1)),
              c(paste("Geographic Extent:", selectionName(), list_of_areas),rep(NA,ncol(output)-1)),
              c(paste("Source:", gsub("\\s+", " ", gsub("[\r\n]", "", var_params()$source))),rep(NA,ncol(output)-1)),
              c(rep(NA,ncol(output))), # blank line
              colnames(output), # headers)
              matrix(unlist(output, use.names=FALSE), # data rows
                     nrow = nrow(output),
              )
            )
          )
          write.table( 
            o,
            na = "", # NA values will show up as blanks
            out_file, 
            sep = ",", # use comma separation since the output is a csv
            col.names = FALSE, 
            row.names = FALSE)
        }
      )
    }
  )
}

# Server ##############
#' Each geography type gets its own module server so that they can act independently
server <- function(input, output, session) {
  
  showModal( # Welcome page which shows on page load
    modalDialog(
      title = h2("Welcome!", align='center'),
      htmltools::includeMarkdown("dialog/welcome.md"),
      footer = modalButton("Got it!"),
      easyClose = TRUE, size="l"
    )
  )
  
  observeEvent(input$about, { # show the about page when the user clicks the "About" button
    showModal(modalDialog(
      title = h2("About", align='center'),
      htmltools::includeMarkdown("dialog/about.md"),
      easyClose = TRUE, size="l"
      )
    )
  })
  
  # build the server modules for each geography type in APP_CONFIG
  lapply(names(APP_CONFIG), function(geo_type) {
    tabPanelServer(gsub(" ","_",geo_type))
  })
}
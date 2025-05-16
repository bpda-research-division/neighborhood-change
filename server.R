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
      
      # get current year, used to determine whether selected years are projections or now
      current_year <- as.integer(format(Sys.Date(), "%Y"))
      
      # Keep track of the unique years of data we have for each topic, displayed in the topic dropdown
      dropdown_years <- APP_DATA[[geo_unit]] %>%
        lapply(function(var) unique(var$areas_categories_df$YEAR) %>% sort())
      
      # Keep track of the full list of topic names that can appear in the "TOPIC" menu
      topic_names <- names(APP_CONFIG[[geo_unit]]$topics)
      #topic_display_names <- names(APP_CONFIG[[geo_unit]]$topics) %>%
      #  lapply(function (n) { # display each variable with its start and end year
      #    paste0(n, ", ", dropdown_years[[n]][1], "-", tail(dropdown_years[[n]], 1))
      #  })
      
      # Extract the currently selected topic name from input$topicSelect
      topic_name <- reactive({
        input$topicSelect
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
      
      label_format_digits <- reactive({
        # extract the number of digits to which summary values should be rounded when displayed
        as.numeric(gsub(".*?([0-9]+).*", "\\1", indicator_params()$hoverformat))
      })
      
      # this df has one summary value for each year and geography
      areas_summary_df <- reactive({
        ss <- pivot_summarise(
          df = areas_categories_df(), 
          cats = var_params()$barCats, 
          summary_expr = indicator_params()$summary_expression, 
          id_columns = c("YEAR", "GEOID", "NAME")
        ) %>% # add geometries to the subcity summary data so we can map them
          mutate(GEOID = as.character(GEOID)) %>%
          merge(y=geo_shapes, by.y = "GEOID", by.x = "GEOID") %>%
          st_as_sf() 
        
        # identify any area that should be grayed out: areas with null summary values for any of the years...
        areas_to_gray <- c(
          unique(ss[is.na(ss$SUMMARY_VALUE),]$GEOID),
          var_params()$additional_null_geoms # ...and areas which have been specified as additional null geoms
        )
        
        ss %>% mutate(
            grayOut = GEOID %in% areas_to_gray, # add a column indicating whether each feature will be grayed out
            labelText = paste0( # add a column with the tooltip label formatted how we want
              "<center>", NAME, # the name of the geographic area always appears on the tooltip
              case_when( # If we aren't graying out the polygon, we add a second line to the tooltip...
                !grayOut ~ paste0(
                  "<br>", 
                  indicator_params()$tickprefix, # ...containing the summary value formatted according to our parameters
                  case_when(grepl("%", indicator_params()$tickformat, fixed=TRUE) ~ as.character(round(SUMMARY_VALUE*100, digits = label_format_digits())), 
                            .default = format(round(SUMMARY_VALUE, digits = label_format_digits()), big.mark=",", trim=TRUE)
                  ),
                  case_when(grepl("%", indicator_params()$tickformat, fixed=TRUE) ~ "%", .default = "")
                ), 
                .default = "" # If the polygon is grayed out, its tooltip just displays the geographic area name
              )
            )
          )
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
      
      # initial map centering + zoom level information
      initial_lat <- APP_CONFIG[[geo_unit]]$center_lat
      initial_lon <- APP_CONFIG[[geo_unit]]$center_lon
      initial_zoom_level <- APP_CONFIG[[geo_unit]]$zoom_level
      
      # Set up static components of the map
      output$map <- renderLeaflet({
        leaflet() %>% 
          addProviderTiles(provider = "CartoDB.Positron", group='basemap') %>%
          addMapPane("outline_mbta", zIndex = 420) %>% # pane for outline and mbta layers
          addMapPane("selected_polyg", zIndex = 430) %>% # pane for polygons currently selected
          addMapPane("all_polyg", zIndex = 410) %>% # pane for all polygons
          setView(initial_lon, initial_lat, zoom = initial_zoom_level)
      })
      
      # The legend label for map polygons with null values is a function of the geography type and topic
      null_label <- reactive({
        if ("null_description" %in% names(var_params())) {
          null_description <- var_params()$null_description
        } else {
          null_description <- "null values"
        }
        styled_text <- lapply( # Apply font size and style adjustments to the null description
          split_max_2_lines(paste(tools::toTitleCase(geo_unit), "with", null_description)), # split description to 2 lines
          function(line) htmltools::HTML(paste0("<span style='font-size:", APP_FONT_SIZE - 5,
                                                "px;font-style:italic;line-height:0;font-weight: 400;'>", line, "</span>"))
        )
        styled_text
      })
      
      # Make it so that maps on inactive tabs stay rendered
      outputOptions(output, "map", suspendWhenHidden = FALSE) 
      
      # Redraw all the map polygons when a new variable or indicator is selected
      observeEvent(input$indicatorSelect, { 
        ss <- areas_summary_df()
        
        # temporarily replace any value with a NA for polygons that we want to gray out
        ss$SUMMARY_VALUE[which(ss$grayOut)] <- NA
        
        yrdfs <- split(ss, ss$YEAR) # split the data on YEAR to create separate map layers for each year
        
        # This parameter overrides the default map color scale with a custom set of lower bin cutoffs
        if ("map_legend_bins" %in% names(indicator_params())) {
          pal <- colorBin(
            MAP_PALETTE, 
            domain = ss$SUMMARY_VALUE,
            bins = c(
              indicator_params()$map_legend_bins, # the user-defined bins will be equal intervals...
              max(ss$SUMMARY_VALUE, na.rm = TRUE) + 0.01 # ...but with a single category at the top which captures outliers
              ) %>% round(label_format_digits())
            )
        } else { # By default, polygons are shaded using a continuous (rather than binned) linear scale
          pal <- colorNumeric(MAP_PALETTE, domain = ss$SUMMARY_VALUE)
        }
        
        leafletProxy("map") %>%
          clearShapes() %>% clearControls() %>% # clear existing stuff before redrawing...
          addControl( # ...but make sure each map has a button for users to clear their map selections...
            actionButton(session$ns("clearSelections"), "Clear selections", icon = icon("trash"), title = "Clear selections", # use trash icon, and add tooltip
                         style=sprintf('font-size:%spx; font-family: "%s"', APP_FONT_SIZE-3, FONT_LORA)),
            position="topright", className = "fieldset {border: 0;}" # remove default button border
          ) %>%
          addControl( #...and a button to reset the map view...
            actionButton(session$ns("resetMapButton"), "",icon = icon("home"), title = "Reset view"), # use home icon, and add tooltip
            position="topleft", className = "fieldset {border: 0;}" # remove default button border
          ) %>%
          addControl( # ...as well as a data download button...
            actionButton(session$ns("downloadButton"), "Download selected data", icon = icon("download"), title = "Download selected data", 
                         style=sprintf('font-size:%spx; font-family: "%s"', APP_FONT_SIZE-3, FONT_LORA)),
            position="bottomright", className = "fieldset {border: 0;}" # remove default button border
          ) %>% 
          leaflet.extras2::addSpinner() %>% # add a loading spinner to be displayed while the map renders
          leaflet.extras2::startSpinner(
            options=list(
              "lines"=15, "length"=15, "width"=10, "radius"=45, "scale"=1.1,"corners" = 1, "speed"=1.3,
              "color"="#fb4d42", "animation"="spinner-line-fade-default"
              )
            ) # This starts the spinner off
        
        # Add Boston outline to each map
        if (geo_type %in% c("census_tracts", "neighborhoods")) {
          leafletProxy("map") %>%
            addPolygons(
              data = OUTLINE_TRACTS, 
              color = "#222222", weight = 2, fillOpacity = 0, 
              options = pathOptions(pane = "outline_mbta", clickable = FALSE)
            )
        } else if (geo_type == "zip_code_areas") {
          leafletProxy("map") %>%
            addPolygons(
              data = OUTLINE_ZIPS, 
              color = "#222222", weight = 2, fillOpacity = 0, 
              options = pathOptions(pane = "outline_mbta", clickable = FALSE)
            )
        }
        
        # Create color palettes based on MBTA subway line colors
        line_colors <- colorFactor(
          palette = c("#003da5", "#00843d", "#ed8b00", "#da291c"), 
          domain = MBTA_LINES$LINE
        )
        station_colors <- colorFactor(
          palette = c("#003da5", "#00843d", "#ed8b00", "#da291c"), 
          domain = MBTA_STOPS$LINE
        )
        
        # Add MBTA lines and stations to the outline later of the map
        leafletProxy("map") %>%
          addPolylines( # Add CR lines
            data = CR_LINES, 
            color = "#7B388C", weight = 2, opacity = 0.8, 
            group = "MBTA lines",
            options = pathOptions(pane = "outline_mbta", clickable = TRUE), # Enable clickable for tooltips
            label = "Commuter Rail", # Tooltip content from a column in your data
            labelOptions = labelOptions(
              style = list("color" = APP_FONT_COLOR, "font-size" = "12px", "background-color" = "white"),
              textsize = "15px", direction = "auto")
            ) %>%
          addCircleMarkers( # Add CR stations
            data = CR_STOPS, 
            color = "white", fillColor = "#7B388C", radius = 3, 
            fillOpacity = 0.8, stroke = TRUE, weight = 0.4, opacity = 1,
            group = "MBTA lines", 
            options = pathOptions(pane = "outline_mbta", clickable = TRUE), # Enable clickable for tooltips
            label = ~paste0(LINE, ": ", STATION_ID), # Tooltip with station name
            labelOptions = labelOptions(
              style = list("color" = APP_FONT_COLOR, "font-size" = "12px", "background-color" = "white"),
              textsize = "15px", direction = "auto"
            )
          ) %>%
          addPolylines( # Add MBTA lines
            data = MBTA_LINES, 
            color = ~line_colors(LINE), weight = 2, opacity = 0.8,  
            group = "MBTA lines", 
            options = pathOptions(pane = "outline_mbta", clickable = TRUE), # Enable clickable for tooltips
            label = ~LINE_NAME, # Tooltip showing line name
            labelOptions = labelOptions(
              style = list("color" = APP_FONT_COLOR, "font-size" = "12px", "background-color" = "white"),
              textsize = "15px", direction = "auto")
            ) %>%
          addCircleMarkers( # Add MBTA stations
            data = MBTA_STOPS, 
            color = "white", fillColor = ~station_colors(LINE), radius = 3, 
            fillOpacity = 0.8, stroke = TRUE, weight = 0.4, opacity = 1,
            group = "MBTA lines", 
            options = pathOptions(pane = "outline_mbta", clickable = TRUE), # Enable clickable for tooltips
            label = ~paste0(LINE_NAME, ": ",  STATION), # Tooltip with station name
            labelOptions = labelOptions(
              style = list("color" = APP_FONT_COLOR, "font-size" = "12px", "background-color" = "white"),
              textsize = "15px", direction = "auto"
            )
          ) %>%  
          addLayersControl( # Single layers control for both groups
            overlayGroups = c("MBTA lines"), 
            options = layersControlOptions(collapsed = FALSE),
            position = "topright" # Specify position for the control
          ) %>%
          hideGroup("MBTA lines") # Hide both groups initially
        
        for (yr in names(yrdfs)) { # Draw and add one layer of polygons for each year
          leafletProxy("map") %>% 
            addPolygons(data=yrdfs[[yr]],
              group=yr, # each layer is assigned to a group ID corresponding to its year...
              layerId = ~paste(GEOID, yr), # ...and each polygon is identified by its GEOID and year
              fillColor = ~pal(SUMMARY_VALUE), fillOpacity = 0.7, # polygon shading
              weight = 1, color = "#818185", # polygon border formatting
              options = pathOptions(pane = "all_polyg"), # place these layers on the lower pane
              label = ~lapply(labelText, htmltools::HTML), # custom label text displayed in a tooltip on hover 
              highlight = highlightOptions( # when you hover over these polygons...
                weight = 3,
                fillOpacity = 0.7,
                color = "#091f2f", # ...given them a dark blue border of weight 3 so they "pop out"
                bringToFront = TRUE
              ),
            ) %>% hideGroup(group = yr) # hide each yearly layer after initializing it
        } 
        
        # prevent topic-switching from inadvertently selecting multiple areas for destination topics where that's disabled
        if ("disable_multiselection" %in% names(indicator_params()) & length(selectedPolygons$groups) > 1) {
          selectedPolygons$groups <- c() 
        }
        
        # prevent topic-switching from inadvertently selecting an area that has null data for the destination topic
        selectedPolygons$groups <- setdiff(selectedPolygons$groups, unique(ss[ss$grayOut,]$GEOID))
        
        # on top of the layers of polygons for each year, add a hidden layer of 
        # polygons that will be displayed when they're clicked on by users
        leafletProxy("map") %>%
          addPolygons(data=yrdfs[[yr]], 
            group = ~GEOID, # for these polygons, we'll use the GEOID as the group name
            layerId = ~GEOID,
            weight = 3, color = "#fb4d42", opacity = 1, fillOpacity=0, # no fill but red border
            options = pathOptions(pane = "selected_polyg"), # place these polygons on the upper pane
            label = ~lapply(labelText, htmltools::HTML) # custom label text displayed in a tooltip on hover 
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
          stopSpinner() %>% # now that the layers are all added, stop the loading spinner
          showGroup(group = input$yearSelect) %>% # The initial map display for a new topic is the most recent year of data...
          showGroup(selectedPolygons$groups) # ...with any previously selected geographies still selected.
      })
      
      # Keep track of the unique set of years for the variable the user selects
      var_years <- reactive({
        years <- unique(APP_DATA[[geo_unit]][[topic_name()]]$areas_categories_df$YEAR) %>% sort()
        # Check the selected indicator and remove 1950 and 1960 if necessary (for race/ethnicity categories)
        if (input$indicatorSelect %in% c("Share of population, Hispanic of any race", "Share of population, Asian alone")) {
          years <- years[!years %in% c(1950, 1960)]
        }   
        if (!input$projectionsToggle) {   # If projections are turned off, remove future years
          years <- years[years <= current_year]
        }
        return(years)
      })
      
      # Keep track of the the unique set of *possible* years for each variable the user selects
      possible_years <- reactive({
        years <- unique(APP_DATA[[geo_unit]][[topic_name()]]$areas_categories_df$YEAR) %>% sort()
        return(years)
      })
      
      # Keep track of the subset of the summary indicators dataframe for the currently selected year
      asdf_selectedYear <- reactive({
        areas_summary_df() %>% filter(YEAR == input$yearSelect)
      })
      
      # Update the map When the user moves the time slider or picks a new variable
      observeEvent(input$yearSelect, {
        leafletProxy("map") %>% 
          hideGroup(group = possible_years()) %>% # hide whatever year is currently selected...
          showGroup(input$yearSelect) %>% # ...then show the layer for the selected year...
          # ...while updating the labels of all selected polygons to reflect data for the given year
          setShapeLabel(layerId = asdf_selectedYear()$GEOID, label = asdf_selectedYear()$labelText)
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
          # from the unselected polygon ID in order to extract the GEOID of the polygon.
          this_selection_id <- gsub("\\s*\\w*$", "", input$map_shape_click$id)
          
          # if the data for the clicked-upon polygon is all non-null values...
          if (!any(is.na(subset(areas_summary_df(), GEOID == this_selection_id)$SUMMARY_VALUE))) {
            # ...depending on the indicator parameters, we either do multiselection...
            if (!"disable_multiselection" %in% names(indicator_params())) {
              leafletProxy("map") %>% showGroup(group = this_selection_id) 
              # ...displaying that polygon as selected and appending thepolygon's ID to selectedPolygons$groups...
              selectedPolygons$groups <- c(selectedPolygons$groups, this_selection_id)
            } else { # ...or we can do single selection...
              leafletProxy("map") %>% hideGroup(group = selectedPolygons$groups) # ...hiding the previous selection...
              leafletProxy("map") %>% showGroup(group = this_selection_id) # ...and showing the current one...
              selectedPolygons$groups <- c(this_selection_id) # ...while updating selectedPolygons$groups accordingly.
            }
          }
          
        } else { # To deselect a polygon that's currently selected...
          leafletProxy("map") %>% hideGroup(group = input$map_shape_click$group)
          # display the polygon as unselected and remove that polygon from selectedPolygons$groups
          selectedPolygons$groups <- setdiff(selectedPolygons$groups, input$map_shape_click$group)
        }
      })
      
      # In response to the user clearing all selections...
      observeEvent(session$input$clearSelections, {
        leafletProxy("map") %>% hideGroup(group = selectedPolygons$groups)
        selectedPolygons$groups <- vector() # ...unselect all polygons and hide them on the map.
      })
      
      # In response to the user resetting the map view...
      observeEvent(input$resetMapButton, {
        leafletProxy("map") %>%
          setView(lng = initial_lon, lat = initial_lat, zoom = initial_zoom_level) #...set lat, long and zoom to initial.
      })
      
      # To update the specific topic selection menu each time the user selects a different general topic...
      observeEvent(input$generalTopicSelect, {
        selectedTopics <- list()
        selected_gt <- input$generalTopicSelect

        if (length(selected_gt) > 0) { # ...identify the topics that match the selected set of general topics...
          selected <- APP_CONFIG[[geo_unit]]$topics %>%
            lapply(function(topic) {topic$generalTopic %in% selected_gt}) %>%
            unlist()
          selectedTopics <- topic_names[selected] %>% # ...and filter out the other topics' names...
            lapply(function(n) {  # (function creates a dataframe of topics and corresponding year ranges)
              data.frame(topic = n, years = paste0(dropdown_years[[n]][1], "-", tail(dropdown_years[[n]], 1)))
            }) %>% bind_rows()
        } else { # ...(unless no general topics are selected, in which case, show all possible topic names)...
          selectedTopics <- topic_names %>%
            lapply(function(n) {  # (function creates a dataframe of topics and corresponding year ranges)
              data.frame(topic = n, years = paste0(dropdown_years[[n]][1], "-", tail(dropdown_years[[n]], 1)))
            }) %>% bind_rows()
          } # ...so that we can update the "TOPIC" menu to only show topics matching selected general topics
        updateSelectizeInput(session, "topicSelect", choices = selectedTopics, selected = selectedTopics$topic[1],
                             options = list(
                               valueField = 'topic',   # the html below allows year ranges to be shown next to topic names
                               labelField = 'topic',   # and styled differently to create visual hierarchy
                               render = I("{
                                option: function(item, escape) {
                                  return '<div>'
                                    + '<span>' + escape(item.topic) + '       </span>'
                                    + '<small class=\"years-in-dropdown\">' + escape(item.years) + '</small>'
                                    + '</div>';
                                  }
                                }" 
                               )
                             ),
                             server = TRUE
                           )
      }, ignoreNULL = FALSE)
      
      # Each time the user selects a different topic or toggles the projections...
      observeEvent(c(input$topicSelect, input$projectionsToggle), {
        indicators <- names(var_params()$summary_indicators) # ...update the list of available indicators...
        updateSelectInput(session, "indicatorSelect", choices=indicators, selected=indicators[[1]])
        updateSliderTextInput(session, "yearSelect",
          # ...reset the choices on the slider based on the new topic's parameters...
          choices = var_years(),
          selected = tail(var_years(), 1) # ...and reset the slider to start on the most recent year of data
          )
      })
      
      # Display label for the geographic scope of the current map selection
      selectionName <- reactive({ # this is used for the legends on the bar and line charts
        geo_type <- geo_unit
        num_selected <- length(selectedPolygons$groups)
        
        # define the names of the selected areas to be shown in the chart legends
        if (num_selected == 0) { # if nothing is selected, we're showing citywide data
          return("City of Boston")
        } else if (num_selected > 5) { # if more than 5 geos selected, then show the current number of selected areas
          sprintf("%s selected %s", num_selected, geo_type) 
        } else if (geo_type == "census tracts") { # for tracts, reformat GEOIDs to cleaner tract names
          tract_label <- ifelse(num_selected == 1, "Census tract", "Census tracts") # Use "Tract" for a singular selection, "Tracts" for multiple
          list_of_areas <- paste(tract_label, paste(sapply(selectedPolygons$groups, function(tract) {
            as.character(as.numeric(substr(tract, nchar(tract) - 5, nchar(tract))) / 100)
          }), collapse = ", "))
        } else if (geo_type == "zip code areas") { # For zip code areas, remove "Zip Code" from each entry
          zip_label <- ifelse(num_selected == 1, "Zip code", "Zip codes") # Use "Zip code" for a singular selection, "Zip codes" for multiple
          list_of_areas <- paste(zip_label, paste(sapply(selectedPolygons$groups, function(zca) {
            as.character(sub("^Zip Code(s)? ", "", zca)) # Remove "Zip Code(s)" from the start of each zip code area object
          }), collapse = ", "))
        } else { # otherwise, just show the list of neighborhoods that are selected
          toString(selectedPolygons$groups)
        }
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
        
        return( # only return the columns that have been named in the barCats parameter
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
      
      # The font size of the bar chart labels can be smaller than the default if the smallbarfont parameter is included
      bar_font_size <- reactive({
        if ("smallbarfont" %in% names(var_params())) {
          return(APP_FONT_SIZE - 4)
        } else {
          return(APP_FONT_SIZE - 2)
        }
      })
      
      # title font size depending on the number of chars in the bar chart title, used as an input for the title in both charts
      title_font_size <- reactive({
        if (nchar(var_params()$barTitle) > 30) {
          return(APP_FONT_SIZE + 1)
        } else {
          return(APP_FONT_SIZE + 3)
        }
      })
      
      # Define how we render the bar chart at any given time
      output$bar_chart <- renderPlotly({
        # Check if "smallbarfont" exists in var_params to render a horizontal chart
        if ("smallbarfont" %in% names(var_params())) {
          plot_ly(filteredBar(),
                  y = ~CATEGORY,    # CATEGORY goes on the y-axis
                  x = ~VALUE,       # VALUE goes on the x-axis
                  name = selectionName(),
                  text = ~VALUE,
                  texttemplate = paste0(var_params()$bartickprefix, '%', sprintf('{x:%s}', var_params()$barhoverformat)),
                  textposition = 'outside',
                  textfont = list(color = APP_FONT_COLOR, family = FONT_MONT, size = APP_FONT_SIZE - 2),
                  hoverinfo = 'skip',
                  orientation = 'h'  # Explicitly set orientation to horizontal
          ) %>% 
            config(displayModeBar = FALSE) %>%
            add_bars(color = I(BAR_COLOR),
                     marker = list(pattern = list(shape = ifelse(input$yearSelect > current_year, "/", ""), # Apply diagonal pattern to future bars
                                                  bgcolor = ifelse(input$yearSelect > current_year, "#B3DFFC", ""),
                                                  size = ifelse(input$yearSelect > current_year, "20", ""),
                                                  solidity = ifelse(input$yearSelect > current_year, "0.7", ""))) 
            ) %>% 
            layout(title = list(
              text = toupper(paste0(ifelse(input$yearSelect > current_year, "Projected ", ""), # If selected year is in the future, prepend "Projected" to the title
                                    var_params()$barTitle, " in ", input$yearSelect)),         # Otherwise, display the title as usual
              font = list(color = APP_FONT_COLOR, family = FONT_MONT, size = title_font_size())
            ),
            font = list(color = APP_FONT_COLOR, family = FONT_LORA, size = bar_font_size()),
            xaxis = list(title = '', fixedrange = TRUE, range = barRange(),
                         showticklabels = FALSE, showgrid = FALSE
            ),  # Adjust x-axis for horizontal bars
            yaxis = list(title = '', fixedrange = TRUE,
                         ticklabelposition = "outside", ticklen = 5, tickcolor = "rgba(0,0,0,0)", # add clear tick lines to distance labels from axis
                         categoryorder = 'array', categoryarray = names(var_params()$barCats), autorange = "reversed"
            ),  # Adjust y-axis for categories
            showlegend = T,
            legend = list(orientation = 'h', 
                          x = -0.4, y = 1.10, # location of the legend
                          font = list(
                            color = APP_FONT_COLOR, family = FONT_LORA,
                            size = APP_FONT_SIZE - 2
                          ),
                          itemclick = FALSE, itemdoubleclick = FALSE
            ),
            margin = list(t = 75) # margin above chart
            )
        } else {
          # Default axes (no flip)
          plot_ly(filteredBar(),
                  x = ~CATEGORY,    # CATEGORY goes on the x-axis
                  y = ~VALUE,       # VALUE goes on the y-axis
                  name = selectionName(),
                  text = ~VALUE,
                  texttemplate = paste0(var_params()$bartickprefix, '%', sprintf('{y:%s}', var_params()$barhoverformat)),
                  textposition = 'outside',
                  textfont = list(color = APP_FONT_COLOR, family = FONT_MONT, size = APP_FONT_SIZE - 2),
                  hoverinfo = 'skip'
          ) %>% 
            config(displayModeBar = FALSE) %>%
            add_bars(color = I(BAR_COLOR),
                     marker = list(pattern = list(shape = ifelse(input$yearSelect > current_year, "/", ""), # Apply diagonal pattern to future bars
                                                  bgcolor = ifelse(input$yearSelect > current_year, "#B3DFFC", ""),
                                                  size = ifelse(input$yearSelect > current_year, "20", ""),
                                                  solidity = ifelse(input$yearSelect > current_year, "0.7", ""))) 
            ) %>% 
            layout(title = list(
              text = toupper(paste0(ifelse(input$yearSelect > current_year, "Projected ", ""), # If selected year is in the future, prepend "Projected" to the title
                                    var_params()$barTitle, " in ", input$yearSelect)),         # Otherwise, display the title as usual
              font = list(color = APP_FONT_COLOR, family = FONT_MONT, size = title_font_size())
            ),
            font = list(color = APP_FONT_COLOR, family = FONT_LORA, size = bar_font_size()),
            xaxis = list(title = '', fixedrange = TRUE,
                         categoryorder = 'array', categoryarray = names(var_params()$barCats)
            ),
            yaxis = list(title = '', fixedrange = TRUE, range = barRange(),
                         showticklabels = FALSE, showgrid = FALSE
            ),
            showlegend = T,
            legend = list(orientation = 'h', 
                          x = 0.01, y = 1.05,
                          font = list(
                            color = APP_FONT_COLOR, family = FONT_LORA,
                            size = APP_FONT_SIZE - 2
                          ),
                          itemclick = FALSE, itemdoubleclick = FALSE
            ),
            margin = list(t = 50) # margin above chart
            )
        }
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
      
      selectedLineFilter <- reactive({ # remove old years for Hispanic and Asian population indicators
        selectedLine() %>% # get data that has already been filtered for the selected geographies
          filter(!(input$indicatorSelect %in% c("Share of population, Hispanic of any race", "Share of population, Asian alone") & YEAR %in% c(1950, 1960)))
      })
      
      # The y-axis range for the line chart is fixed for a given variable and map selection,
      # but if we want to display a citywide comparison line, the range shouldn't cut that off
      lineRange <- reactive ({
        # the default upper value of the line chart y axis is the maximum summary value for the selection
        top_val <- max(selectedLineFilter()$SUMMARY_VALUE, na.rm=TRUE)
        
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
        # Separate selected data into past and future based on the current year
        past_data <- subset(selectedLineFilter(), YEAR <= current_year)
        max_past_year <- max(past_data$YEAR, na.rm = TRUE)
        future_data <- subset(selectedLineFilter(), YEAR > current_year | YEAR == max_past_year)
        
        # Separate citywide data into past and future based on the current year
        past_citywide_data <- subset(totalarea_summary_df(), YEAR <= current_year)
        max_past_citywide_year <- max(past_citywide_data$YEAR, na.rm = TRUE)
        future_citywide_data <- subset(totalarea_summary_df(), YEAR > current_year | YEAR == max_past_year)
        
        # Create the base plot for the past data
        p <- plot_ly(past_data, 
                     x = ~YEAR,
                     y = ~SUMMARY_VALUE,
                     source = session$ns("line_chart"), 
                     hoverinfo = 'text'
        ) %>% 
          config(displayModeBar = FALSE) %>% # remove default plotly controls
          add_trace(hoverinfo='x', mode = 'markers', type = 'scatter', # add a hidden trace for displaying years on hover
                    marker=list(size=0, color='white'), showlegend=FALSE, opacity=0) %>%
          add_lines(color=I(LINE_COLOR), # set the line formatting options
                    line=list(width = 3, shape = 'spline', smoothing = 1),
                    hoverinfo = "y", # display y-values when hovering over line chart
                    name=selectionName() # set the series name for the line (appears in legend)
                    ) %>%
          add_markers(x = ~YEAR, 
                      y = ~SUMMARY_VALUE,
                      marker = list(color = LINE_COLOR, size = 6, symbol = 'circle'), # solid circle marker for years w/ data
                      showlegend = FALSE, # don't show this marker in the legend
                      hoverinfo = 'none' # enable hover info for the markers
          ) %>%
          
          layout(title = list(text = toupper(input$indicatorSelect), font=list(color = APP_FONT_COLOR, family = FONT_MONT, size=title_font_size())), 
                 font=list(color=APP_FONT_COLOR, family = FONT_LORA, size = APP_FONT_SIZE-2),
                 xaxis = list(title = '', fixedrange = TRUE,
                              range = c( # set x axis range a little wider than the data so as to not cut off text
                                as.numeric(var_years()[1]) - line_xrange_bookend(),
                                as.numeric(tail(var_years(), 1)) + line_xrange_bookend()
                              ),
                              tickfont = list(family = FONT_MONT, color = APP_FONT_COLOR)
                 ),
                 yaxis = list(title = '', fixedrange = TRUE, range = lineRange(),
                              # set the number format for the data that appear on hover...
                              hoverformat = indicator_params()$hoverformat,
                              # ... and the number formatting for the y axis labels
                              tickprefix = indicator_params()$tickprefix, 
                              tickformat = indicator_params()$tickformat,
                              tickfont = list(family = FONT_MONT, color = APP_FONT_COLOR)
                 ),
                 showlegend = T, # always show the legend (even with just 1 series)
                 legend = list(orientation = 'h', # place legend items side by side
                               x=0.01, y=1.05, # position legend near the top left
                               itemclick = FALSE, itemdoubleclick = FALSE,
                               font = list(family = FONT_LORA)
                 ), 
                 hovermode = "x unified", # show the data values for all lines when hovering over a given x
                 hoverlabel = list(font = list(family = FONT_MONT, color = APP_FONT_COLOR)),
                 margin = list(t=40) # top padding to make sure chart title is visible
          )
        
        # If projections are enabled, add future data
        if (input$projectionsToggle) {
          # Format the future line as dotted (if projections are on)
          p <- p %>%
            add_trace(x = future_data$YEAR, y = future_data$SUMMARY_VALUE,
                      hoverinfo=ifelse(future_data$YEAR > max_past_year, "x", "none"), mode = 'markers', type = 'scatter', # add a hidden trace for displaying years on hover
                      marker=list(size=0, color='white'), showlegend=FALSE, opacity=0
            ) %>%
            add_lines(x = future_data$YEAR, y = future_data$SUMMARY_VALUE,
                      hoverinfo=ifelse(future_data$YEAR > max_past_year, "y", "none"), color=I(LINE_COLOR), # set the line formatting options
                      line=list(width = 3, shape = 'spline', smoothing = 1, dash='dot'), name="Projection", showlegend = FALSE
            ) %>% 
            add_markers(x = future_data$YEAR,
                        y = future_data$SUMMARY_VALUE,
                        marker = list(color = LINE_COLOR, size = 6, symbol = 'circle'), # solid circle marker for years w/ data
                        showlegend = FALSE, # don't show this marker in the legend
                        hoverinfo = 'none' # enable hover info for the markers
            )
        }
        
        # If at least one polygon is selected and citywide comparison is enabled...
        if (length(selectedPolygons$groups) > 0 & indicator_params()$citywide_comparison) {
          p <- p %>%
            add_lines(x = past_citywide_data$YEAR, y = past_citywide_data$SUMMARY_VALUE,
                      hoverinfo="y", name="City of Boston", color = I("rgba(40, 139, 228, 0.4)"),
                      line=list(width = 3, shape = 'spline', smoothing = 1)
            )
          if (input$projectionsToggle) {
            p <- p %>%
              add_lines(x = future_citywide_data$YEAR, y = future_citywide_data$SUMMARY_VALUE,
                        hoverinfo=ifelse(future_citywide_data$YEAR > max_past_citywide_year, "y", "none"), name="City of Boston", color = I("rgba(40, 139, 228, 0.4)"),
                        line=list(width = 3, shape = 'spline', smoothing = 1, dash='dot'), showlegend = FALSE
              )
          }
        }
        
        # Add a marker highlighting the current year on the slider
        marker_y_data <- subset(selectedLineFilter(), YEAR == input$yearSelect)
        if (nrow(marker_y_data) > 0) {
          p <- p %>%
            add_markers(x = input$yearSelect, y = marker_y_data$SUMMARY_VALUE,
                        marker = list(color = "white", symbol = "circle", size = 16,
                                      line = list(color = "#fb4d42", width = 3)
                        ), showlegend = FALSE, hoverinfo = "skip"
            )
        }
        
        # Mark the line chart as ready to be clicked
        line_loaded(TRUE)
        
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
      
      # this records whether the aggregation options panel on the download window should be shown
      output$showAggPanel <- reactive({
        length(selectedPolygons$groups) != 1 # the only case where we don't show the panel is when a single area is selected
      })
      outputOptions(output, "showAggPanel", suspendWhenHidden = FALSE) # this ensures that showAggPanel is continuously calculated
      
      # Displays a modal (pop up window) in response to user clicking download button
      observeEvent(input$downloadButton, {
        showModal(modalDialog(
          HTML(
            paste(
              "<b>Topic:</b>", topic_name(), "<br>", 
              "<b>Geographic extent:</b>", selectedAreas(), "<br>",
              "<b>Variable:</b>", input$indicatorSelect, "<br><br>",
              conditionalPanel(   # prompts user to select a disagreegaged data series, or disaggregated series
                condition = 'output.showAggPanel == true', ns = session$ns,
                radioButtons(
                  session$ns("downloadType"), "Choose a download type:",
                  choiceNames = c("Download a single, aggregated data series for this extent", "Download disaggregated data for each area within this extent"),
                  choiceValues = c("single-area / aggregated", "geographically disaggregated"),
                  width = "100%",
                  selected = "single-area / aggregated"
                )
              ),
              conditionalPanel(
                condition = 'true', ns = session$ns,  # always show this modal dialog
                radioButtons(
                  session$ns("yearDownload"), "Years for download:",
                  choiceNames = c(paste0("All years: ", var_years()[1], "-", var_years()[length(var_years())]),
                                  paste0("Only selected year: ", input$yearSelect)),  # Display selected year
                  choiceValues = c("all", "selected"),
                  width = "100%",
                  selected = "all"  # Default selection
                  )
              ),
              conditionalPanel(
                condition = 'true', ns = session$ns,  # Always show file name editing panel
                textInput(
                  session$ns("customFileName"), "Enter a custom file name:",
                  value = paste0(gsub("[[:punct:][:space:]]+", "_", topic_name()), "_in_Boston")  # Default file name
                ) %>% tagAppendAttributes(class="filename-input") # assign a class to the input object so we can style it differently from other input divs
              ) 
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
        geo_type <- geo_unit
        num_selected <- length(selectedPolygons$groups)
        # define the names of the selected areas to be shown in the chart legends
        if (num_selected == 0) { # if nothing is selected, we're showing citywide data
          return("City of Boston")
        } else if (geo_type == "census tracts") {
          tract_label <- ifelse(num_selected == 1, "Census tract", "Census tracts") # Use "Tract" for a singular selection, "Tracts" for multiple
          list_of_areas <- paste(tract_label, toString(selectedPolygons$groups))
        } else if (geo_type == "zip code areas") { # For zip code areas, remove "Zip Code" from each entry
          zip_label <- ifelse(num_selected == 1, "Zip code", "Zip codes") # Use "Zip code" for a singular selection, "Zip codes" for multiple
          list_of_areas <- paste(zip_label, paste(sapply(selectedPolygons$groups, function(zca) {
            as.character(sub("^Zip Code(s)? ", "", zca)) # Remove "Zip Code(s)" from the start of each zip code area object
          }), collapse = ", "))
        } else { # otherwise, just show the list of neighborhoods that are selected
          toString(selectedPolygons$groups)
        }
      })
      
      # Performs the data download in response to user confirming their download
      output$downloadData <- downloadHandler(
        filename = function() {
          paste0(ifelse(input$customFileName != "",input$customFileName, 
                        (paste0(gsub("[[:punct:]\\s]+", "_", topic_name()), "_in_Boston"))),
                 ".csv")
        },
        content = function(out_file) {
          removeModal() # close confirmation window
          
          # Get selected year(s) from the modal dialog
          if (input$yearDownload == "selected") {
            years_to_use <- input$yearSelect # Use only the selected year if 'Only selected year' is chosen
          } else {
            years_to_use <- var_years() # Use all years if 'All years' is selected
            
          }
          
          if (input$downloadType == "single-area / aggregated") {
            output <- selectionData() %>% 
              pivot_wider(id_cols = "YEAR", names_from='CATEGORY', values_from='VALUE') %>%
              subset(select = c("YEAR", names(var_params()$barCats))) %>%
              filter(YEAR %in% years_to_use) %>% # filter for year selection
              # downloaded data includes bar chart categories + the selected indicator for each year
              merge(selectedLineFilter() %>% select(YEAR, SUMMARY_VALUE) %>% st_drop_geometry(), by='YEAR') %>%
              # rename the SUMMARY_VALUE column to the name of the selected indicator
              mutate(YEAR = as.character(YEAR), !!input$indicatorSelect := SUMMARY_VALUE, .keep='unused')
          } else if (length(selectedPolygons$groups) == 0) { # i.e. if the user wants disaggregated citywide data
            output <- areas_categories_df() %>% 
              pivot_wider(id_cols = c("GEOID", "YEAR"), names_from='CATEGORY', values_from='VALUE') %>%
              subset(select = c("GEOID", "YEAR", names(var_params()$barCats))) %>%
              filter(YEAR %in% years_to_use) %>% # filter for year selection
              # downloaded data includes bar chart categories + the selected indicator for each year
              merge(
                areas_summary_df() %>% 
                  select(GEOID, YEAR, SUMMARY_VALUE) %>% 
                  st_drop_geometry(), 
                by=c('GEOID', 'YEAR')
              ) %>%
              # rename the SUMMARY_VALUE column to the name of the selected indicator
              mutate(YEAR = as.character(YEAR), !!input$indicatorSelect := SUMMARY_VALUE, .keep='unused')
          } else { # i.e. if the user wants disaggregated data for a multiselection
            output <- areas_categories_df() %>% 
              subset(GEOID %in% selectedPolygons$groups) %>% 
              pivot_wider(id_cols = c("GEOID", "YEAR"), names_from='CATEGORY', values_from='VALUE') %>%
              subset(select = c("GEOID", "YEAR", names(var_params()$barCats))) %>%
              filter(YEAR %in% years_to_use) %>% # filter for year selection
              # downloaded data includes bar chart categories + the selected indicator for each year
              merge(
                areas_summary_df() %>% 
                  subset(GEOID %in% selectedPolygons$groups) %>% 
                  select(GEOID, YEAR, SUMMARY_VALUE) %>% 
                  st_drop_geometry(), 
                by=c('GEOID', 'YEAR')
              ) %>%
              # rename the SUMMARY_VALUE column to the name of the selected indicator
              mutate(YEAR = as.character(YEAR), !!input$indicatorSelect := SUMMARY_VALUE, .keep='unused')
          }

          o <- as.data.frame(
            rbind( # paste a description and citation of the data on the first few lines of the output csv file
              c(paste("Topic:", topic_name()),rep(NA,ncol(output)-1)),
              c(paste("Geographic Extent:", selectedAreas()),rep(NA,ncol(output)-1)),
              c(paste("Year(s):", paste(years_to_use, collapse = ", ")), rep(NA, ncol(output) - 1)),  
              c(paste(var_params()$note), rep(NA, ncol(output) - 1)),
              c(paste("Source:", gsub("\\s+", " ", gsub("[\r\n]", "", var_params()$source))),rep(NA,ncol(output)-1)),
              c(paste("Download type:", input$downloadType),rep(NA,ncol(output)-1)),
              c(rep(NA,ncol(output))), # blank line
              colnames(output), # headers
              matrix(
                unlist(
                  output %>%
                    mutate(across(
                      where(is.numeric) & !any_of(c("YEAR", "GEOID", input$indicatorSelect)), 
                      round # round the raw data columns
                    )), 
                  use.names = FALSE
                ), 
                nrow = nrow(output)
              )
            )
          )
          write.table( 
            o,
            na = "", # NA values will show up as blanks
            out_file, 
            sep = ",", # use comma separation since the output is a csv
            col.names = FALSE, 
            row.names = FALSE
          )
        }
      )
#      # Observe the downloadMapButton click
#      observeEvent(input$downloadMapButton, {
#        showModal(modalDialog(
#          HTML(paste("<b>Topic:</b>", topic_name(), "<br>", 
#                  "<b>Variable:</b>", input$indicatorSelect, "<br>",
#                  "<b>Year:</b>", input$yearSelect, "<br>"
#                    )
#              ),
#          title = h2("Confirm download details", align = 'center'),
#          easyClose = TRUE,
#          footer = tagList(
#            modalButton("Cancel"),
#            actionButton("downloadMap", "Download map as .png file", icon = icon("image"))
#          )
#        ))
#      })
#      
#        # Trigger download
#      output$DownloadMap <- downloadHandler(
#        filename = 'mymap.png',  # Can also be .pdf if desired
#        content = function(file) {
#          # Use `mapshot` to capture the current map view with all layers
#          mapshot(mymap(), file = file, cliprect = "viewport")
#        }
#      )
        
      
#      # Download handler for the bar chart
#      output$download_bar <- downloadHandler(
#        filename = function() {
#          paste("bar_chart", Sys.Date(), ".png", sep = "")
#        },
#        content = function(file) {
#          # Save the plotly output as an HTML file first
#          html_file <- tempfile(fileext = ".html")
#          saveWidget(ggplotly(bar_chart()), html_file, selfcontained = TRUE)  # Capture the rendered plot
#          
#          # Use webshot to take a screenshot of the HTML file
#          webshot(html_file, file = file, vwidth = 800, vheight = 600)
#        }
#      )
#      
#      output$download_line <- downloadHandler(
#        filename = function() {
#          paste("line_chart", Sys.Date(), ".png", sep = "")
#        },
#        content = function(file) {
#          # Save the plotly output as an HTML file first
#          html_file <- tempfile(fileext = ".html")
#          saveWidget(output$line_chart(), html_file, selfcontained = TRUE)
#          
#          # Use webshot to take a screenshot of the HTML file
#          webshot(html_file, file = file, vwidth = 800, vheight = 600)
#        }
#      )
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
# Define how the app looks

# UI module function ##########

#' For each geography type, we are creating basically the exact same tabPanel
#' with a given set of UI components (map, bar chart, line chart, etc). 
#' This function creates those components, namespacing them by geography type. 
geoTabPanelUI <- function(geo_type) {
  ns <- NS(gsub(" ","_",geo_type)) # define the namespace based on the geography type
  initial_years <- APP_DATA[[geo_type]][[1]]$areas_categories_df$YEAR %>% 
    unique() # extract year range for the initial slider
  generalTopics <- APP_CONFIG[[geo_type]]$topics %>% 
    lapply(function(topic) {topic$generalTopic}) %>% 
    unique() # extract list of general topics for the topic filter checkboxes
  
  # the below variables are used to reformat the map legend to place the NA value below the color
  # palette - default behavior in the current version of Leaflet is for them to be side by side
  css_legend_fix <- "div.info.legend.leaflet-control br {clear: both;}" # CSS to correct spacing
  html_legend_fix <- htmltools::tags$style(type = "text/css", css_legend_fix)  # Convert CSS to HTML
  
  tabPanel(tools::toTitleCase(geo_type), style='padding:10px;', # within each tab,
    # the left side of the screen will be filled by a sidebarPanel containing the controls
    sidebarPanel(width=6, style="height:850px;", tags$style(".well {background-color:#ebedf2;}"),
        fluidRow( # The first row of controls is dedicated to general topic filtering
          column(width=4, 
                 HTML("<b>Topic filters:</b>")
                 ),
          column(width=8, align='center', style="margin-top:-5px;",
                 checkboxGroupInput(ns("generalTopicSelect"), label = NULL, # no default label since we have our own
                             choices = generalTopics, selected = generalTopics, 
                             inline = TRUE # displays general topics horizontally rather than vertically
                             )
                 )
        ),
        fluidRow( # The second row of controls is for choosing a specific topic
          column(width=4, 
                 HTML("<b>Choose a topic:</b>")
                 ),
          column(width=8, style="z-index:1011; margin-top:-5px;", # ensure this drop-down menu displays in front of other stuff
                 selectInput(ns("topicSelect"), label = NULL, 
                             choices = NULL # topic choices are populated by the server, so no initialization here
                             )
                 )
        ),
        fluidRow( # The third row of controls is for choosing a specific indicator / variable within a topic
          column(width=4, 
                 HTML("<b>Choose a variable:</b>")
                 ),
          column(width=8, style="z-index:1010;",
                 selectInput(ns("indicatorSelect"), label = NULL, 
                             choices = NULL # again, these choices will be populated by the server
                 )
          )
        ),
        fluidRow( # The fourth row of controls is for the time slider
           column(width=4, 
                  HTML(
                    "<b>Drag the slider or click &#9658; to move through time:</b>"
                    ) # the above jumble of characters is the HTML code for a play button symbol
                  ),
           column(width=8,
                  sliderTextInput(inputId = ns("yearSelect"), 
                      choices = initial_years,
                      selected = tail(initial_years, 1), grid=TRUE, label = NULL,
                      animate = animationOptions(interval = 800) # set play button's animation speed here
                      )
                  )
       ),

       div(style="padding-bottom:5px;", HTML(
         sprintf("<b>Select one or more %s on the map:</b>", geo_type)
       )), # the bottom section of the controls is dedicated to the map, with a line of instructional text above it
       leafletOutput(ns("map"), height="530px") %>%
         htmlwidgets::prependContent(html_legend_fix), # apply the legend NA values fix
    ),
    mainPanel(width=6, # the right-hand side of the screen displays the charts, any notes, and source citations
       # bar chart with loading spinner enabled. hide.ui = TRUE ensures that the chart UI isn't redrawn each time a year changes
       shinycssloaders::withSpinner(plotlyOutput(ns("bar_chart")), color="#2186bb", size=1.5, type=5, hide.ui = FALSE),

       # default background color for plotly charts is white, so our note area matches that
       htmlOutput(align="center", style="font-size:9pt; background-color: #ffffff; padding-bottom:5px;", ns("note")),
       
       # line chart with loading spinner enabled. hide.ui = TRUE ensures that the chart UI isn't redrawn each time a year changes
       shinycssloaders::withSpinner(plotlyOutput(ns("line_chart")), color="#2186bb", size=1.5, type=5, hide.ui = FALSE),

       htmlOutput(style=sprintf('padding:10px; font-size:%spx', APP_FONT_SIZE - 4), ns("sourceText")) # citation at bottom
       )
  )
}

# UI ##########
styling_commands = c(
  ".container-fluid {background-color: #f5f7fb;}" # set app background
  , ".irs-grid-pol.small {height: 0px;}" # hide minor ticks on slider
  , sprintf( # standardize font type and size across the app
    '* {font-size: %spx;}; * {font-family: "%s";};', APP_FONT_SIZE, APP_FONT
    ) 
)

ui <- fluidPage(title = "Neighborhood Change Explorer",
  # set the browser icon for the page to be the BPDA logo
  tags$head(tags$link(rel="shortcut icon", href="cob_favicon.ico")),
  tags$head(tags$script(src='setLeafletLabel.js')), # function for updating leaflet labels
  ## CECILIA'S EDITS ##
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "test_theme.css")),
  #####################
  tags$head(tags$style(HTML(paste(styling_commands)))), # apply other style commands
  tags$head(tags$style( # increase the size of the the play button on the slider
    type='text/css', ".slider-animate-button { font-size: 20pt !important; }"
    )),
  chooseSliderSkin("Square"), # set slider style according to a template
  
  fluidRow( # this is the top of the page / title area
    column(10, # title
      h3("BOSTON NEIGHBORHOOD CHANGE EXPLORER", 
         align = "left", 
         style = sprintf('font-size:32px; font-family: "%s"; padding: 0px; font-weight: 800; color: #091F2F', APP_HEADER_FONT)
        ) 
    ),
    column(2, align='right', # about button goes at the top right
      div(style='padding:10px;',
        actionButton("about", "About", style='padding:7px; font-size:120%')
        )
      )
  ),
  # create a tabsetPanel with one tab for each geography type present in APP_CONFIG
  do.call(tabsetPanel, lapply(names(APP_CONFIG), geoTabPanelUI))
)
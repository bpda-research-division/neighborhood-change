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
  
  tabPanel(
    title = tags$div(class = "tab-title",
                     toupper(tools::toTitleCase(geo_type))),
    # the left side of the screen will be filled by a sidebarPanel containing the controls
    sidebarPanel(
      width = 6,
      class = "sidebar-panel",  # Use the sidebar panel class
      fluidRow(  class = "first-control-row",# The first row of controls is dedicated to general topic filtering
        column(
          width = 2, 
          HTML("<b class='control-row-title'>FILTERS</b>")  # Use a class for bold titles
        ),
        column(
          width = 10, class = "selectors", align = 'center',  
          div(class = "custom-checkbox",  # Add custom class here
              checkboxGroupInput(ns("generalTopicSelect"), label = NULL, 
                                 choices = generalTopics, selected = generalTopics, 
                                 inline = TRUE
              )
          )
        )
      ),
      fluidRow(class = "control-row", # The second row of controls is for choosing a specific topic
        column(
          width = 2, 
          HTML("<b class='control-row-title'>TOPIC</b>")  # Use a class for bold titles
        ),
        column(
          width = 10, class = "selectors", style="z-index:1011;",  # Ensure this drop-down menu displays in front of other stuff
          selectInput(ns("topicSelect"), label = NULL, 
                      choices = NULL  # Topic choices are populated by the server, so no initialization here
          )
        )
      ),
      fluidRow(class = "control-row",  # The third row of controls is for choosing a specific indicator / variable within a topic
        column(
          width = 2, 
          HTML("<b class='control-row-title'>VARIABLE</b>")  # Use a class for bold titles
        ),
        column(
          width = 10, class = "selectors", style="z-index:1010;",
          selectInput(ns("indicatorSelect"), label = NULL, 
                      choices = NULL  # Again, these choices will be populated by the server
          )
        )
      ),
      fluidRow(class = "control-row",  # The fourth row of controls is for the time slider
        column(
          width = 1, 
          HTML("<b class='control-row-title'>YEAR</b>")  # Use a class for the slider instruction
        ),
        column(
          width = 11, style = "min-width: 0;",
          sliderTextInput(inputId = ns("yearSelect"), 
                          choices = initial_years,
                          selected = tail(initial_years, 1), grid = TRUE, label = NULL,
                          animate = animationOptions(interval = 800)  # Set play button's animation speed here
          )
        )
      ),
      
      div(class = "instruction-map",  # Use a class for padding in the  map instruction row
          HTML(sprintf("<b>Click one or more %s on the map:</b>", geo_type))
      ),  # The bottom section of the controls is dedicated to the map, with a line of instructional text above it
      leafletOutput(ns("map"), height = "580px") %>%
        htmlwidgets::prependContent(html_legend_fix),
  # Apply the legend NA values fix
    ),
    mainPanel(class = "main-panel",
              width=6,  # the right-hand side of the screen displays the charts, any notes, and source citations
       # bar chart with loading spinner enabled. hide.ui = TRUE ensures that the chart UI isn't redrawn each time a year changes
       div(class = "chart-pane",
           shinycssloaders::withSpinner(plotlyOutput(ns("bar_chart")), color = "#2186bb", size = 1, type = 8, hide.ui = FALSE)
       ),
       # Note area for bar chart
       htmlOutput(class = "footnote", ns("note")),
       
       # Line chart with loading spinner
       div(class = "chart-pane", 
           shinycssloaders::withSpinner(plotlyOutput(ns("line_chart")), color = "#2186bb", size = 1, type = 8, hide.ui = FALSE)
       ),
       htmlOutput(class = "source-citation", ns("sourceText")) # citation at bottom
    )
  )
}

# UI ##########
ui <- fluidPage(
  title = "Neighborhood Change Explorer",
  
  # Set the browser icon for the page to be the city logo
  tags$head(tags$link(rel="shortcut icon", href="cob_favicon.ico")),
  tags$head(tags$script(src='setLeafletLabel.js')), # Function for updating leaflet labels
  
  ## LOAD CSS STYLE ##
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "test_theme.css")),
  #####################
  
#  tags$head(tags$style(HTML(paste(styling_commands)))), # Apply other style commands
  
  chooseSliderSkin("Round"), # Set slider style according to a template
  
  # Top header with Logo, Title, Subtitle, and About Button
  div(class = "header",  # Use the header class for styling
      fluidRow(
        column(1, align = "center", img(src = "planning_logo.png", height = "60px")),  # Logo top left of header
        column(9, 
               h3("BOSTON NEIGHBORHOOD CHANGE EXPLORER", align = "left", 
                  class = "header-title" # Use the header title class
               ), 
               div("Developed by the City of Boston Planning Department Research Division", 
                   class = "header-subtitle" # Use the header subtitle class
               ) # Add subtitle
        ),
        column(2, align = 'right', 
               div(class = 'about-button',  # Use the about button class
                   actionLink("about", "ABOUT")
               )
        )
      )
  ),
  # create a tabsetPanel with one tab for each geography type present in APP_CONFIG
  do.call(tabsetPanel, lapply(names(APP_CONFIG), geoTabPanelUI))
)
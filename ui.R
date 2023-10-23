# Define how the app looks

# UI module function ##########

#' For each geography type, we are creating basically the exact same tabPanel
#' with a given set of UI components (map, bar chart, line chart, etc). 
#' This function creates those components, namespacing them by geography type. 
geoTabPanelUI <- function(geo_type) {
  ns <- NS(gsub(" ","_",geo_type)) # when the initialization is only general topics...
  variables <- APP_CONFIG[[geo_type]]$topics # ...most of these lines of code will be moved to the server # xyz123
  variables_years <- APP_DATA[[geo_type]] %>% 
    lapply(function(var) unique(var$sb_df$YEAR))
  initial_years <- variables_years[[1]]
  
  # the below variables are used to reformat the map legend to place the NA value below the color
  # palette - default behavior in the current version of Leaflet is for them to be side by side
  css_legend_fix <- "div.info.legend.leaflet-control br {clear: both;}" # CSS to correct spacing
  html_legend_fix <- htmltools::tags$style(type = "text/css", css_legend_fix)  # Convert CSS to HTML
  
  tabPanel(tools::toTitleCase(geo_type), style='padding:10px;', # within each tab,
    # the left side of the screen will be filled by a sidebarPanel containing the controls
    sidebarPanel(width=6, style="height:850px;", tags$style(".well {background-color:#ebedf2;}"),
        fluidRow(
          column(width=4, style="margin-top:5px;",
                 HTML("<b>Choose a general topic:</b>")
                 ),
          column(width=8, style="z-index:1012;", # ensure drop-down menu displays in front of other stuff
                 selectInput(ns("generalTopicSelect"),
                             NULL,
                             choices = c("Demographics", "Housing", "Businesses") # names(APP_CONFIG[[geo_unit]]$generalTopics) # xyz123
                             )
                 )
        ),
        fluidRow(
          column(width=4, style="margin-top:5px;", 
                 HTML("<b>Choose a specific topic:</b>")
                 ),
          column(width=8, style="z-index:1011;", # ensure drop-down menu displays in front of other stuff
                 selectInput(ns("topicSelect"),
                             NULL, 
                             choices = names(variables) %>% 
                               lapply(function (n) { # display each variable with its start and end year
                                 paste0(n, " (", variables_years[[n]][1], "-", tail(variables_years[[n]], 1), ")")
                               }) # NULL # xyz123
                             )
                 )
        ),
        fluidRow(
          column(width=4, style="margin-top:5px;", 
                 HTML("<b>Choose a variable:</b>")
          ),
          column(width=8, style="z-index:1010;",
                 selectInput(ns("indicatorSelect"),
                             NULL, choices = NULL
                 )
          )
        ),
        fluidRow(
           column(width=4, 
                  HTML(
                    "<b>Drag the slider or click &#9658; to move through time:</b>"
                    ), # the above jumble of characters is the HTML code for a play button symbol
                  ),
           column(width=8, #style="margin-top:5px;",
                  sliderTextInput(inputId = ns("yearSelect"), 
                      choices = initial_years,
                      selected = tail(initial_years, 1), grid=TRUE, label = NULL,
                      animate = animationOptions(interval = 800) # set animation speed here
                      )
                  )
       ),
       div(style="padding-bottom:5px;", HTML(
         sprintf("<b>Select one or more %s on the map:</b>", geo_type)
       )),
       leafletOutput(ns("map"), height="510px") %>%
         htmlwidgets::prependContent(html_legend_fix), # apply the legend NA values fix
    ),
    mainPanel(width=6, # the right-hand side of the screen displays the charts, any notes, and source citations
       plotlyOutput(ns("bar_chart")), # default background color for plotly charts is white, so our note matches that
       htmlOutput(align="center", style="font-size:9pt; background-color: #ffffff; padding-bottom:5px;", ns("note")),
       plotlyOutput(ns("line_chart")),
       htmlOutput(style=sprintf('padding:10px; font-size:%spx', APP_FONT_SIZE - 4), ns("sourceText"))
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
  tags$head(tags$link(rel="shortcut icon", href="bpda_logo.ico")),
  tags$head(tags$style(HTML(paste(styling_commands)))), # apply other style commands
  tags$head(tags$style( # increase the size of the the play button on the slider
    type='text/css', ".slider-animate-button { font-size: 20pt !important; }"
    )),
  chooseSliderSkin("Square"), # set slider style according to a template
  
  fluidRow( # this is the top of the page / title area
    column(10, # title
      h3("Boston Neighborhood Change Explorer", 
         align = "left", 
         style = sprintf('font-size:32px; font-family: "%s"; padding: 0px;', APP_FONT)
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
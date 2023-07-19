# Define how the app looks

# UI module function ##########

#' For each geography type, we are creating basically the exact same tabPanel
#' with a given set of UI components (map, bar chart, line chart, etc). 
#' This function creates those components, namespacing them by geography type. 
geoTabPanelUI <- function(geo_type) {
  ns <- NS(geo_type)
  variables <- ALL_VARS_INFO[[geo_type]]
  initial_st <- as.numeric(variables[[1]]$start)
  initial_end <- as.numeric(variables[[1]]$end)
  initial_step <- as.numeric(variables[[1]]$step)
  
  # the below variables are used to reformat the map legend to place the NA value below the color
  # palette - default behavior in the current version of Leaflet is for them to be side by side
  css_legend_fix <- "div.info.legend.leaflet-control br {clear: both;}" # CSS to correct spacing
  html_legend_fix <- htmltools::tags$style(type = "text/css", css_legend_fix)  # Convert CSS to HTML
  
  tabPanel(tools::toTitleCase(geo_type), style='padding:10px;', 
    sidebarPanel(width=6, style="height:800px;", tags$style(".well {background-color:#ebedf2;}"),
        fluidRow(
          column(width=4,  
                 HTML("<b>Choose a topic:</b>")
                 ),
          column(width=8, style="z-index:1010;", # ensure drop-down menu displays in front of other stuff
                 selectInput(ns("variable"), 
                             NULL, choices = names(variables) %>% 
                               lapply(function (n) { # display each variable with its start and end year
                                 paste0(n, " (", variables[[n]]$start, "-", variables[[n]]$end, ")")
                               })
                 )
                 )
        ),
         fluidRow(
           column(width=4, style="margin-top:10px;",
                  HTML(
                    "<b>Drag the slider or click &#9658; to see change over time:</b>"
                  ), # the above jumble of characters is the code for a play button symbol
                  ),
           column(width=8, style="margin-top:5px;",
                  sliderTextInput(inputId = ns("yearSelect"), 
                      choices = seq(initial_st, initial_end, by=initial_step),
                      selected = initial_st, grid=TRUE, label = NULL,
                      animate = animationOptions(interval = 800) # set animation speed here
                  )
                  )
       ),
       fluidRow(style="padding-bottom:10px", # top row of controls
         column(width = 7, 
                HTML("<b>Select one or more areas on the map to filter the data shown on the charts.</b>")
         ),
         column(width = 5, align="left", style="padding-left:40px;",
                htmlOutput(ns("selectionText"))
         )
       ),
       leafletOutput(ns("map"), height="550px") %>%
         htmlwidgets::prependContent(html_legend_fix), # apply the legend NA values fix
    ),
    mainPanel(width=6, # the right-hand side of the screen
       plotlyOutput(ns("bar_chart")),
       plotlyOutput(ns("line_chart")),
       htmlOutput(style=sprintf('padding:10px; font-size:%spx', APP_FONT_SIZE - 4), ns("sourceText"))
       )
  )
}

# UI ##########
styling_commands = c(
  ".container-fluid {background-color: #f5f7fb;}" # set app background
  , ".irs-grid-pol.small {height: 0px;}" # hide minor ticks on slider
  , sprintf(
    '* {font-size: %spx;}; * {font-family: "%s";};', APP_FONT_SIZE, APP_FONT
    ) # standardize font type and size across the app
)

ui <- fluidPage(title = "Neighborhood Change Explorer",
  # set the browser icon for the page to be the BPDA logo
  tags$head(tags$link(rel="shortcut icon", href="bpda_logo.ico")),
  tags$head(tags$style(HTML(paste(styling_commands)))), # apply other style commands
  tags$head(tags$style( # increase the size of the the play button on the slider
    type='text/css', ".slider-animate-button { font-size: 20pt !important; }"
    )),
  chooseSliderSkin("Square"), # set slider style according to a template
  
  fluidRow( # top-of page / title area
    column(10, # title
      h3("Boston Neighborhood Change Explorer", 
         align = "left", 
         style = sprintf('font-size:32px; font-family: "%s"; padding: 0px;', APP_FONT)
        ) 
    ),
    column(2, align='right', # about button
      div(style='padding:10px;',
        actionButton("about", "About", style='padding:7px; font-size:120%')
        )
      )
  ),
  # create a tabsetPanel with one tab for each geography type present in ALL_VARS_INFO
  do.call(tabsetPanel, lapply(names(ALL_VARS_INFO), geoTabPanelUI))
)
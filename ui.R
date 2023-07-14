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
    sidebarPanel(width=6, style = "height: 85vh;", tags$style(".well {background-color:#ebedf2;}"),
       fluidRow( # top row of controls
         column(width = 5,
            selectInput(ns("variable"), 
              HTML("1. Select a variable:"), choices = names(variables) %>% 
                lapply(function (n) { # display each variable with its start and end year
                  paste0(n, " (", variables[[n]]$start, "-", variables[[n]]$end, ")")
                })
            )
         ),
         column(width = 7, 
            sliderTextInput(inputId = ns("yearSelect"), 
                label = HTML(
                  "3. Drag the slider or click &#9658; to see change over time:"
                ), # the above jumble of characters is the code for a play button symbol
                choices = seq(initial_st, initial_end, by=initial_step),
                selected = initial_st, grid=TRUE, 
                animate = animationOptions(interval = 800) # set animation speed here
            )
         )
       ),
       fluidRow(style='padding:5px;', # bottom row of controls
         column(width=5,
            HTML(sprintf("<b>2. Select one or more %s on the map to update the charts.</b>", geo_type))
         ),
         column(width=3, align='right',
            actionButton(ns("clearSelections"), "Clear all selections", 
                style=sprintf("font-size:%spx", APP_FONT_SIZE)
            )
         ),
         column(width=4, align='left',
            htmlOutput(ns("selectionText"))
         )
       ),
       leafletOutput(ns("map"), height='57vh') %>% 
         htmlwidgets::prependContent(html_legend_fix), # apply the legend NA values fix
       
       #width=6 # overall width of the sidebarPanel containing these components
    ),
    mainPanel(width=6, # the right-hand side of the screen
      plotlyOutput(ns("bar_chart")),
      plotlyOutput(ns("line_chart")),
      h5(style = "padding: 10px;", "Data source: BPDA Research Division")
    )
  )
}

# UI ##########
styling_commands = c(
  ".container-fluid {background-color: #f5f7fb;}" # set app background
  , ".irs-grid-pol.small {height: 0px;}" # hide minor ticks on slider
  , sprintf('* {font-size: %spx;}; * {font-family: "%s";};', 
            APP_FONT_SIZE, APP_FONT
            ) # standardize fonts across the app
  , ".selectize-dropdown {z-index: 10000}" # ensure drop-down menu displays over everything
  , ".slider-animate-button { font-size: 20pt !important; }" # increase play button size
)

ui <- fluidPage(title = "Neighborhood Change Explorer",
  # set the browser icon for the page to be the BPDA logo
  tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),
  tags$head(tags$style(HTML(paste(styling_commands)))), # apply other style commands
  chooseSliderSkin("Square"), # set slider style according to a template
  
  fluidRow( # top-of page / title area
    column(11, # title
      h1("Boston Neighborhood Change Explorer", 
         align = "left", 
         style = sprintf('font-size:40px; font-family: "%s";', APP_FONT)
        ) 
    ),
    column(1, align='center', # about button
      div(style='padding:15px;',
        actionButton("about", "About", style='padding:10px; font-size:120%')
        )
      )
  ),
  # create a tabsetPanel with one tab for each geography type present in ALL_VARS_INFO
  do.call(tabsetPanel, lapply(names(ALL_VARS_INFO), geoTabPanelUI))
)
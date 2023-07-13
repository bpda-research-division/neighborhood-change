# Define the basic skeleton of the user interface

# the below variables are used to reformat the map legend to place the NA value below the color
# palette - default behavior in the current version of Leaflet is for them to be side by side
css_legend_fix <- "div.info.legend.leaflet-control br {clear: both;}" # CSS to correct spacing
html_legend_fix <- htmltools::tags$style(type = "text/css", css_legend_fix)  # Convert CSS to HTML

# UI Module ##########

#' For each geography type with its given collection of variables, we create a
#' tabPanel with a uniform set of UI components (map, bar chart, line chart, etc)
#' This function creates those components, namespacing them using the geo_type
geoTabPanelUI <- function(geo_type) {
  ns <- NS(geo_type)
  variables <- ALL_VARS_INFO[[geo_type]]
  initial_st <- as.numeric(variables[[1]]$start)
  initial_end <- as.numeric(variables[[1]]$end)
  initial_step <- as.numeric(variables[[1]]$step)
  
  tabPanel(tools::toTitleCase(geo_type), style='padding:10px;',
    sidebarPanel(style = "height: 90vh;",
       fluidRow(
         column(width = 5,
                selectInput(ns("variable"), 
                            "1. Select a variable:", choices = names(variables) %>% lapply(function (n) {paste0(n, " (", variables[[n]]$start, "-", variables[[n]]$end, ")")})
                            )
         ),
         column(width = 7, 
                sliderTextInput(inputId = ns("yearSelect"), 
                            label = HTML("3. Drag the slider or click &#9658; to see change over time:"), # this is the code for a play button symbol
                            # initial_st, initial_end, 
                            # sep = "", ticks=TRUE,
                            # value = initial_st, step = initial_step,
                            animate = animationOptions(interval = 600),
                            choices = seq(initial_st, initial_end, by=initial_step),
                            selected = initial_st, grid=TRUE
                            )
         )
       ),
       fluidRow(style='padding:5px;',
         column(width=5,
                HTML(sprintf("<b>2. Select one or more %s on the map to update the charts.</b>", geo_type))
         ),
         column(width=3, align='right',
                actionButton(ns("clearSelections"), "Clear all selections", style=sprintf("font-size:%spx", APP_FONT_SIZE))
         ),
         column(width=4, align='left',
                htmlOutput(ns("selectionText"))
         )
       ),
       leafletOutput(ns("map"), height='70%') %>% 
         htmlwidgets::prependContent(html_legend_fix), # apply the legend NA values fix
       width=6
    ),
    mainPanel( #style='padding:10px;',
      fluidRow(style='padding:10px;',
               column(width=12, align='center',
                      plotlyOutput(ns("bar_chart")),
                      htmlOutput(ns("varText"), style='padding:10px;'), # for debugging
                      plotlyOutput(ns("line_chart")),
               )
      ),
      h5("Data source: BPDA Research Division"),
      width = 6
    )
  )
}

# UI ##########

ui <- fluidPage(
  # tags$style('.container-fluid {background-color: #007BA7;}'),
  tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"), # this css hides the minor tick marks on the slider
  # for shiny style, .irs-grid-text {font-size: 0px;} .irs--shiny .irs-from,.irs--shiny .irs-to,.irs--shiny .irs-single {font-size: 11px;}
  setSliderColor(c("#201934"), c(1)), # https://divadnojnarg.github.io/post/customsliderinput/
  tags$head(tags$style(HTML(sprintf('* {font-size: %spx;};', APP_FONT_SIZE)))),
  tags$head(tags$style(HTML(sprintf('* {font-family: "%s";};', APP_FONT)))), 
  #tags$style(HTML(".tabbable > .nav > li > a[data-value='About'] {background-color: #edeff2;}")), # change background color of About tab
  tags$head(tags$style(HTML('.well {background-color: #edeff2;}'))), #change background color of sidebarPanel
  # tags$style("body { font-size: 14px; line-height: 14px; }"),
  tags$head(tags$style(type='text/css', ".slider-animate-button { font-size: 20pt !important; }")),
  tags$head(tags$style('.selectize-dropdown {z-index: 10000}')), # place the variable selection in front of other elements
  chooseSliderSkin("Square"),
  fluidRow(
    column(11,
      h1("Boston Neighborhood Change Explorer", align = "left", style = sprintf('font-size:40px; font-family: "%s";', APP_FONT)) 
    ),
    column(1, align='center',
      div(style='padding:15px;',
        actionButton("about", "About", style='padding:10px; font-size:120%') 
      )
    )
    #, windowTitle = "Boston Neighborhood Change Dashboard"
  ),
  do.call(tabsetPanel,
          lapply(names(ALL_VARS_INFO), geoTabPanelUI)
  )
)
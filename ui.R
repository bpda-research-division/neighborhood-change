# the below variables are used to reformat the map legend to place the NA value below the color
# palette - default behavior in the current version of Leaflet is for them to be side by side
css_fix <- "div.info.legend.leaflet-control br {clear: both;}" # CSS to correct spacing
html_fix <- htmltools::tags$style(type = "text/css", css_fix)  # Convert CSS to HTML

# UI Module and Functions ##########

#' For each geography type with its given collection of variables, we create a
#' tabPanel with the same set of components (map, line chart, bar chart, etc)
#' This function creates those components, namespacing them using the geo_type
geoTabPanelUI <- function(geo_type, variables) {
  ns <- NS(geo_type)
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
       fluidRow(style='padding:10px;',
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
       leafletOutput(ns("map"), height='65%') %>% 
         htmlwidgets::prependContent(html_fix), # apply the legend NA values fix
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
      width = 6
    )
  )
}

#' Defines the About page
aboutTabPanelUI <- function(title) {
  tabPanel(title, style='padding:10px;',
           "under construction",
           "hbic labor force is 14+ for 1950 and 1960, 16+ since then. source = decennial except 2010 & 2020 are 5yr acs")
}

#' To create all tabPanels in a single lapply() below, this function generates
#' tabs based on tab names. By default, each name is treated as a geo type. 
tabGenerator <- function(name) {
  if (name == "About") {
    aboutTabPanelUI(name) # can pass in other data if we want a parameterized about page
  }
  else { # name is either About or a geography type (tracts, neighborhoods, etc)
    geoTabPanelUI(name, all_vars_info[[name]])
  }
}

# UI ##########

# Creates as many tabs as there are geography types, plus one for the about page
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
  div(#style = "background-color: #4163ff;", #headerPanel(
    h1("Boston Neighborhood Change Dashboard", align = "left", style = sprintf('font-size:40px; font-family: "%s";', APP_FONT)) 
    #, windowTitle = "Boston Neighborhood Change Dashboard"
  ),
  # modalDialog(
  #   "For instructions, see xyz. Terms and services, etc",
  #   title = h3("Welcome to the Boston Neighborhood Change Explorer!", align="center"),
  #   size = "l",
  #   easyClose = FALSE
  # ),
  # 4("A BPDA Research Division project", align = "left"),
  do.call(tabsetPanel,
          lapply(append(names(all_vars_info), "About"), tabGenerator)
  )
)
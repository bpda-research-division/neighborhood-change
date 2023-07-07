library(dplyr)
library(shinyWidgets)
library(leaflet)
library(plotly)

# the below variables are used to reformat the map legend to place the NA value below the color
# palette - default behavior in the current version of Leaflet is for them to be side by side
css_fix <- "div.info.legend.leaflet-control br {clear: both;}" # CSS to correct spacing
html_fix <- htmltools::tags$style(type = "text/css", css_fix)  # Convert CSS to HTML

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
                            "1. Select data:", choices = names(variables)
                            )
                , HTML(sprintf("<b>2. Select one or more %s on the map to update the charts.</b>", geo_type))
         ),
         column(width = 7, 
                sliderInput(ns("yearSelect"), 
                            "3. Drag the slider to see change over time:",
                            initial_st, initial_end, sep = "", ticks=TRUE,
                            value = initial_st, step = initial_step, 
                            animate = animationOptions(interval = 500 #, playButton = icon('play', "fa-3x"), pauseButton = icon('pause', "fa-3x")
                                                       )
                            )
                , fluidRow(style='padding:10px;',
                  column(width=5, align='center',
                         actionButton(ns("clearSelections"), "Clear all selections")
                  ),
                  column(width=7, align='center',
                         htmlOutput(ns("selectionText"))
                  )
                )
         )
       ),
       # fluidRow(style='padding:10px;',
       #          column(width=4, align = 'center',
       #                 HTML(sprintf("<b>2. Select one or more %s on the map to update the charts.</b>", geo_type))
       #          ), # this may turn into a textOutput and live in the server if we want areas to say tracts/neighborhoods instead
       #          column(width=3, align='right',
       #                 actionButton(ns("clearSelections"), "Clear all selections")
       #          ),
       #          column(width=5, align='center',
       #                 htmlOutput(ns("selectionText"))
       #          )
       #          
       #          
       # ),
       leafletOutput(ns("map"), height='65%') %>% 
         htmlwidgets::prependContent(html_fix), # apply the legend NA values fix
       width=6
    ),
    mainPanel( #style='padding:10px;',
      # selectInput("colors", "Color Scheme",
      #             rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
      # ),
      # checkboxInput("legend", "Show legend", TRUE),
      plotlyOutput(ns("bar_chart")),
      # htmlOutput(ns("varText"), style='padding:10px;'), # for debugging
      plotlyOutput(ns("line_chart")),
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

# Creates as many tabs as there are geography types, plus one for the about page
ui <- fluidPage(tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"
                           ), # this css hides the minor tick marks on the slider
  # setSliderColor(c("DimGrey", "DimGray"), c(1,2)), # https://divadnojnarg.github.io/post/customsliderinput/
  tags$head(tags$style(HTML(sprintf('* {font-family: "%s"};', APP_FONT)))),
  chooseSliderSkin("Shiny", color = "#112446"),
  headerPanel(h1("Boston Neighborhood Change Dashboard", align = "center")),
  do.call(tabsetPanel,
          lapply(append(names(all_vars_info), "About"), tabGenerator)
  )
)
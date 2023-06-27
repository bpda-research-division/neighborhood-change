library(dplyr)
library(shinyWidgets)
library(leaflet)
library(plotly)

# the below variables are used to reformat the map legend to place the NA value below the color
# palette - default behavior in the current version of Leaflet is for them to be side by side
css_fix <- "div.info.legend.leaflet-control br {clear: both;}" # CSS to correct spacing
html_fix <- htmltools::tags$style(type = "text/css", css_fix)  # Convert CSS to HTML

tract_vars <- as.data.frame(
  rbind(
    c("Income", 2010, 2018, 2),
    c("Age", 2010, 2020, 1)
  )
)
colnames(tract_vars) <- c("name", "start", "end", "step")

neigh_vars <- as.data.frame(
  rbind(
    c("Educ", 1950, 2020, 10),
    c("Race", 1950, 2020, 10)
  )
)
colnames(neigh_vars) <- c("name", "start", "end", "step")

all_vars <- list(tract_vars, neigh_vars)
names(all_vars) <- c("tracts", "neighborhoods")

geoTabPanelUI <- function(geo_type, variables) {
  ns <- NS(geo_type)
  initial_st <- as.numeric(variables$start[1])
  initial_end <- as.numeric(variables$end[1])
  initial_step <- as.numeric(variables$step[1])
  
  # each variable has a display name, a start year, an end year, and a timestep, 
  tabPanel(tools::toTitleCase(geo_type), style='padding:10px;',
    sidebarPanel(style = "height: 90vh;",
                 fluidRow(
                   column(width = 6,
                          selectInput(ns("variable"), "1. Select data:", choices = variables$name)
                   ),
                   column(width = 6, 
                          sliderInput(ns("yearSelect"), "3. Drag the slider to see change over time:",
                                      initial_st, initial_end, value = initial_st, step = initial_step, sep = "", ticks=TRUE)
                   )
                 ),
                 fluidRow(style='padding:10px;',
                          column(width=4, align = 'center',
                                 HTML(sprintf("<b>2. Select one or more %s on the map to update the charts.</b>", geo_type))
                          ), # this may turn into a textOutput and live in the server if we want areas to say tracts/neighborhoods instead
                          column(width=3, align='center',
                                 actionButton(ns("clearSelections"), "Clear all selections")
                          ),
                          column(width=5, align='center',
                                 htmlOutput(ns("selectionText"))
                          )
                          
                          
                 ),
                 leafletOutput(ns("map"), height='65%') %>% 
                   htmlwidgets::prependContent(html_fix),
                 width=6 # will probably go for 6 on the slider + map side...
    ),
    mainPanel( #style='padding:10px;',
      # selectInput("colors", "Color Scheme",
      #             rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
      # ),
      # checkboxInput("legend", "Show legend", TRUE),
      plotlyOutput(ns("bar_chart")),
      # htmlOutput(ns("selectionText"), style='padding:10px;'),
      plotlyOutput(ns("line_chart")),
      width = 6 # and 6 on the bar + line side
    )
  )
}

aboutTabPanelUI <- function(title) {
  tabPanel(title, style='padding:10px;',
           "under construction",
           "hbic labor force is 14+ for 1950 and 1960, 16+ since then. source = decennial except 2010 & 2020 are 5yr acs")
}

tabGenerator <- function(name) {
  if (name == "About") {
    aboutTabPanelUI(name) # can pass in other data if we want a parameterized about page
  }
  else {
    geoTabPanelUI(name, all_vars[[name]])
  }
}

ui <- fluidPage(tags$style(type = "text/css", "#buttons {align-items: center; justify-content: center}
                           .irs-grid-pol.small {height: 0px;}
                           "), # this css hides the minor tick marks on the slider
  chooseSliderSkin("Shiny"),
  headerPanel(h1("Boston Neighborhood Change Dashboard", align = "center")),
  do.call(tabsetPanel, 
          lapply(append(names(all_vars), "About"), function(name) {
            tabGenerator(name)
          })
  )
)
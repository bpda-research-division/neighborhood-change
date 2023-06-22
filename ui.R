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
t <- names(all_vars)

geoTabPanelUI <- function(geo_type, variables) {
  ns <- NS(geo_type)
  initial_st <- as.numeric(variables$start[1])
  initial_end <- as.numeric(variables$end[1])
  initial_step <- as.numeric(variables$step[1])
  
  # each variable has a display name, a start year, an end year, and a timestep, 
  tabPanel(tools::toTitleCase(geo_type),
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
                          column(width=7, align = 'center',
                                 HTML(sprintf("<b>2. Select one or more %s on the map to update the charts.</b>", geo_type))
                          ), # this may turn into a textOutput and live in the server if we want areas to say tracts/neighborhoods instead
                          column(width=5, align='left',
                                 actionButton(ns("clearSelections"), "Clear all selections")
                          )
                          
                          
                 ),
                 leafletOutput(ns("map"), height="80%") %>% 
                   htmlwidgets::prependContent(html_fix),
                 width=6 # will probably go for 6 on the slider + map side...
    ),
    mainPanel(style='padding:10px;',
      # selectInput("colors", "Color Scheme",
      #             rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
      # ),
      # checkboxInput("legend", "Show legend", TRUE),
      plotlyOutput(ns("bar_chart")),
      htmlOutput(ns("selectionText"), style='padding:10px;'),
      plotlyOutput(ns("line_chart")),
      width = 6 # and 6 on the bar + line side
    )
  )
}

ui <- fluidPage(tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"), # this css hides the minor tick marks on the slider
  chooseSliderSkin("Shiny"),
  headerPanel(h1("Neighborhood Change Dashboard", align = "center")),
  do.call(tabsetPanel, 
          # append(
            lapply(names(all_vars), function(geo_type) {
              geoTabPanelUI(geo_type, all_vars[[geo_type]])
            }), 
          #   tabPanel("About")
          # )
  )
  # sidebarPanel(style = "height: 90vh;",
  #              fluidRow(
  #                column(width = 6,
  #                       selectInput("variable", "1. Select data:", choices = c("Income", "Age"))
  #                ),
  #                column(width = 6, 
  #                       sliderInput("yearSelect", "3. Drag the slider to see change over time:",
  #                                   2010, 2018, value = 2010, step = 2, sep = "", ticks=TRUE)
  #                )
  #              ),
  #              fluidRow(style='padding:10px;',
  #                column(width=6, align = 'right',
  #                       HTML("<b>2. Select one or more areas on the map to update the charts.</b>")
  #                ), # this may turn into a textOutput and live in the server if we want areas to say tracts/neighborhoods instead
  #                column(width=6, align='center',
  #                       actionButton("clearSelections", "Clear all selections")
  #                )
  # 
  # 
  #              ),
  #              leafletOutput("map", height="80%") %>% 
  #                htmlwidgets::prependContent(html_fix),
  #              width=6 # will probably go for 6 on the slider + map side...
  # ),
  # mainPanel(
  #   # selectInput("colors", "Color Scheme",
  #   #             rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
  #   # ),
  #   # checkboxInput("legend", "Show legend", TRUE),
  #   plotlyOutput("bar_chart"),
  #   htmlOutput("selectionText", style='padding:10px;'),
  #   plotlyOutput("line_chart"),
  #   width = 6 # and 6 on the bar + line side
  # )
)
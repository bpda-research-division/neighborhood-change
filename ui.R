library(dplyr)
library(shinyWidgets)
library(leaflet)
library(plotly)

# the below variables are used to reformat the map legend to place the NA value below the color
# palette - default behavior in the current version of Leaflet is for them to be side by side
css_fix <- "div.info.legend.leaflet-control br {clear: both;}" # CSS to correct spacing
html_fix <- htmltools::tags$style(type = "text/css", css_fix)  # Convert CSS to HTML

ui <- fluidPage(tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"), # this css hides the minor tick marks on the slider
  chooseSliderSkin("Shiny"),
  headerPanel(h1("Neighborhood Change Dashboard", align = "center")),
  sidebarPanel(style = "height: 90vh;",
               fluidRow(
                 column(width = 6,
                        selectInput("variable", "Select Data:", choices = c("Income", "Age"))
                 ),
                 column(width = 6, 
                        sliderInput("yearSelect", "Drag the slider to see change over time:",
                                    2010, 2018, value = 2010, step = 2, sep = "", ticks=TRUE)
                 )
               ),
               fluidRow(
                 column(width=2,
                        actionButton("clearSelections", "Clear all selections")
                 ),
                 column(width=9, offset = 1,
                        htmlOutput("selectionText")
                 )

               ),
               leafletOutput("map", height="75%") %>% 
                 htmlwidgets::prependContent(html_fix),
               width=6 # will probably go for 6 on the slider + map side...
  ),
  mainPanel(
    # selectInput("colors", "Color Scheme",
    #             rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
    # ),
    # checkboxInput("legend", "Show legend", TRUE),
    plotlyOutput("bar_chart"),
    plotlyOutput("line_chart"),
    width = 6 # and 6 on the bar + line side
  )
)
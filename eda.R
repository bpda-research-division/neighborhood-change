library(shiny)
library(plotly)
library(dplyr)

ui <- fluidPage(
  actionButton("go", "Click to recalc"),
  plotlyOutput("plot")
)

gendata <- function(){
  ndata <- 10
  d <- tibble(text=LETTERS[1:ndata], f=1, x=runif(ndata)) %>% mutate(r = rank(x))
  rbind(mutate(d, x=-1), d, mutate(d, x=-1)) %>%
    arrange(text)
}

server <- function(input, output, session){
  
  origdata <- gendata()
  if (FALSE){ # for offline testing
    print(head(origdata))
    my <- list(olddata = origdata, newdata = origdata)
  }
  
  my <- reactiveValues(
    olddata = origdata,
    newdata = origdata
  )
  
  output$plot <- renderPlotly({
    cat("renderPlotly\n")
    plot_ly() %>%
      add_trace(x=origdata$x, y=origdata$r, frame=origdata$f, line=list(width=20, simplify=FALSE), type="scatter", opacity=0.5, mode="lines", name="Rank") %>%
      add_trace(x=origdata$x + 0.02, y=origdata$r, frame=origdata$f, text=origdata$text, type="scatter", mode="text", showlegend=FALSE) %>%
      layout(xaxis=list(range=list(0,1.1))) %>%
      animation_opts(frame=500, transition=500, redraw=FALSE)
  })
  
  observeEvent(input$go, {
    req(my$newdata)
    cat("observeEvent input$go\n")
    my$olddata <- my$newdata # save old data
    my$newdata <- gendata() %>% # generate new data
      mutate(f=my$olddata$f+1)
    print(head(my$newdata))
    # https://plot.ly/javascript/plotlyjs-function-reference/#plotlyanimate
    plotlyProxy("plot", session=session, deferUntilFlush=FALSE) %>%
      plotlyProxyInvoke("animate",
                        # frameOrGroupNameOrFrameList
                        list(
                          data = list(list(
                            x = my$newdata$x,
                            y = my$newdata$r,
                            frame = my$newdata$f
                          ),
                          list(
                            x = my$newdata$x + 0.02,
                            y = my$newdata$r,
                            text = my$newdata$text,
                            frame = my$newdata$f
                          )),
                          traces = list(as.integer(0), as.integer(1)),
                          layout = list()
                        ),
                        # animationAttributes
                        list()
      )# plotlyProxyInvoke
  })
  
}
shinyApp(ui, server)
# 
# library(tidycensus)
# library(dplyr)
# library(ggplot2)
# 
# census_api_key("3910e99aea0a472b50f5cdc422c9a3395b3c87b3")
# 
# #v21 <- load_variables(2021, "acs5/subject", cache = TRUE)
# 
# my_states = c("MA")
# my_vars <- c(
#   total_households = "S1901_C01_001"
#   , median_household_income = "S1901_C01_012"
# )
# 
# get_acs_for_year = function(yr) {
#   get_acs(
#     geography = "tract",
#     variables = my_vars,
#     state = my_states,
#     county = "025",
#     year = yr,
#     survey="acs5",
#     output = "wide",
#     geometry = TRUE
#   ) 
# }
# 
# # to do: filter out winthrop and chelsea, standardize color scale bounds across years
# 
# for (yr in c(2010, 2016, 2021)) {
#   (get_acs_for_year(yr) %>%
#     ggplot(aes(fill = median_household_incomeE)) + 
#     geom_sf(color = NA) + 
#     scale_fill_viridis_c(option = "magma")) %>% print()
# }
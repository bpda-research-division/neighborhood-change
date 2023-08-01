If you plan to work on an existing issue, please comment on it. 

If your idea isn't already an existing issue, feel free to create one of your own.

To set up your R development environment and run the app locally: 

1. Clone or fork this repository
3. Set your R working directory to this repository, or if using RStudio, open neighborhood-change.Rproj
3. Run `install.packages("renv")` on your version of R if it's not installed already
4. Run `renv::restore()` to set up an R environment within your copy of the repo that has all the necessary packages
5. Run `shiny::runApp()`, or if using RStudio, select the "Run App" button
The Neighborhood Change Explorer is an R Shiny application that uses a modular, parameterized code structure to enhance reproducibility and make maintenance easier. 

This document provides an overview of how the app works, plus step-by-step instructions on:
* how to create your own version of the app
* how to update the underlying data for an existing app

## File Descriptions
| name | description |
| -------- | --------- |
| `ui.R` | defines the basic layout of the app + some styling parameters |
| `server.R` | defines how the map & charts are rendered + how the various controls interact with the visualizations & with each other |
| `global.R` | defines how the data are loaded into the app + some app-wide formatting parameters (e.g. colors and fonts) and miscellaneous functions |

## Under construction

The actual data that are loaded into the app are stored in RDS files in the `data/` folder. There is a particular expected format. Script that takes in csv data and writes it to that format: `pull_data.R`

Within `global.R` concepts to cover:
- ALL_VARS_INFO created in `global.R` - modify it to modify what and how data is shown
- geographic units, which "contain" topics
- topics, with their set of named parameters
- four dataframes for each topic, stored in ALL_VARS_DATA

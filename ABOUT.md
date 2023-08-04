This document provides an overview of how the Neighborhood Change Explorer app works, plus step-by-step instructions on:
* how to create your own version of the app
* how to update the underlying data for an existing app

The Neighborhood Change Explorer is built in R Shiny, a framework for building web applications using the R programming language. The app consists of three R source files:

| name | description |
| -------- | --------- |
| `ui.R` | defines the basic layout of the app + some styling parameters |
| `server.R` | defines how the map & charts are rendered + how the various controls interact with the visualizations & with each other |
| `global.R` | defines how the data are loaded into the app + some app-wide formatting parameters (e.g. colors and fonts) and miscellaneous functions |

The actual data are loaded in from files in the `data/` folder, and the contents of the welcome page and the about page are rendered from markdown files stored in the `dialog/` folder. 

When the app runs, everything defined in `global.R` is made available to both the UI and the server, without the need for import statements. See the R Shiny documentation on [two-file Shiny apps](https://shiny.posit.co/r/articles/build/two-file/) and [scoping](https://shiny.posit.co/r/articles/improve/scoping/) to learn more about this setup.

Configuration options for the Neighborhood Change Explorer are centralized within the APP_CONFIG variable defined in `global.R`. APP_CONFIG is set up as a nested list of **geographic units**, which contain **topics**.

```
APP_CONFIG <- list(
  "geo unit 1" = list(
    "topic A" = list(
      # parameters for topic A
    ),
    "topic B" = list(
      # parameters for topic B
    ),
    ...
  ),
  "geo unit 2" = list(
    # topics
  ),
  ...
)
```

Examples of geographic units include "census tracts" and "neighborhoods". Each geographic unit becomes a tab on the app. Tabs are displayed from left to right in the order in which they're declared in APP_CONFIG. The declared names of geographic units are converted into title case when displayed on tabs. 

Examples of topics include "Age" and "Total Housing Units". Each topic declared for a given geographic unit becomes an entry on that tab's drop-down menu. Topic entries are displayed from top to bottom in the order in which they're declared in APP_CONFIG. The names of topics appear on the app exactly as they are declared.

For example, if the above pseudocode were to be used in an app, it would look like this:

![screenshot of ](img/geo_topic_demo.png)

Each combination of geographic unit and topic constitutes a unique **variable**. Each file within the `data/` folder contains all the data for a given variable. 

`ui.R` and `server.R` are basically functions that display data about one variable at a time. The configuration of each variable is defined by a set of **parameters** within APP_CONFIG.

| parameter | required? | description | example |
| ------ | ---- | ------ | ----- |
| data_code | yes | name of the corresponding .RDS file in the `data/` folder | "hbicttp" |

Under the hood, each geographic unit is a separate Shiny module with its own UI and server. 

Params to rename:
- lineTitle -> summaryIndicatorTitle
- linehoverformat -> summaryIndicatorFormat
- 

can do some reordering of params too

Within the Neighborhood Change Explorer, the fundamental unit of analysis is a topic. Some examples of topics might include age, race/ethnicity, or housing units. Fundamentally, . global is where all the data for each topic is loaded into the app and where parameters for each topic can be defined. When users switch between topics using the drop-down menu --

The data for each variable consists of:
* topic: 
* summary indicator: eg young adult share, non white-alone share, total housing units
* category: eg population 10-19, 20-34. CATEGORY field is category names to be displayed. should have more than 1 for the bar chart to be interesting
* category label: column alias for a given category xyz, used in summary expression
* geographic unit: eg tracts, neighborhoods, block groups
* geographic area: uniquely identified with GEOID field, and named/labeled with the NAME field. should be non overlapping
* aggregate function
* summary expression
* variable code

The app holds data in dataframes. A dataframe is a data structure can be thought of as a table with rows of features and columns of fields / attributes. Another type of data structure we use is the simple features object, which is just a dataframe with an additional geometry column that stores the spatial properties of each feature.  

The data for each topic consists of four dataframes:

| name | alias | required fields | fields which uniquely identify each row | type of R object | where it's used |
| -------- | --------- | --------- | ---------- | ---------- | -------- |
| sb_df | subcity bins | GEOID, NAME, CATEGORY, YEAR, VALUE | GEOID, CATEGORY, YEAR | dataframe | bar chart |
| ss_df | subcity summary | GEOID, NAME, YEAR, VALUE | GEOID, YEAR | simple features object | line chart, map |
| cb_df | citywide bins | CATEGORY, YEAR, SUMMARY_VALUE | CATEGORY, YEAR | dataframe | bar chart |
| cs_df | citywide summary | YEAR, SUMMARY_VALUE | YEAR | dataframe | line chart |

ss_df is a simple features object because that is the dataframe that is used for the map. sb_df and cb_df are used on the bar chart, and cs_df and ss_df are used on the line chart.

For each topic, all four dataframes are bundled together into a list and stored in an RDS file named after the variable code.

Script that takes in csv data formatted like sb_df plus an aggregate function and a summary expression and writes it to that format: `pull_data.R`

Within `global.R` concepts to cover:
- APP_CONFIG created in `global.R` - modify it to modify what and how data is shown
- geographic units, which "contain" topics
- topics, with their set of named parameters
- four dataframes for each topic, stored in ALL_VARS_DATA

also, 
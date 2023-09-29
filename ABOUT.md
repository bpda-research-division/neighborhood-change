# For maintainers
This document provides an overview of how the Neighborhood Change Explorer app works, plus step-by-step instructions on:
* how to create your own version of the app
* how to update the underlying data for an existing app

For instructions on how to set up your R development environment in order to run an instance of the Neighborhood Change Explorer locally, see the [contribution guidelines](CONTRIBUTING.md).

## Basic architecture
The Neighborhood Change Explorer is built in R Shiny, a framework for building web applications using the R programming language. The app consists of three R source files:

| name | description |
| -------- | --------- |
| `ui.R` | defines the basic layout of the app + some styling parameters |
| `server.R` | defines how the map & charts are rendered + how the various controls interact with the visualizations & with each other |
| `global.R` | defines how the data are loaded into the app + some app-wide formatting parameters (e.g. colors and fonts) and miscellaneous functions |

The actual data are loaded in from files in the `data/` folder, and the contents of the welcome page and the about page are rendered from markdown files stored in the `dialog/` folder. 

When the app runs, every R object defined in `global.R` is made available to both the UI and the server, without the need for import statements. See the R Shiny documentation on [two-file Shiny apps](https://shiny.posit.co/r/articles/build/two-file/) and [scoping](https://shiny.posit.co/r/articles/improve/scoping/) to learn more about this architecture.

## Configuring instances of the Neighborhood Change Explorer

The data preprocessing folder of this repository includes a script called `pull_data.R` which does not run as part of the app itself, but rather is meant to be modified and run by app maintainers in order to set up the data files that are used by a particular instance of the Neighborhood Change Explorer. 

Specifically, the names, data sources, and configuration parameters for all of the topics displayed by a Neighborhood Change Explorer instance are defined within the `APP_CONFIG` object in `pull_data.R`. `APP_CONFIG` is set up as a nested list of **geographic units**, which contain or more **topics**, which in turn contain one or more **indicators**.

```
APP_CONFIG <- list(
  "geo unit 1" = list(
    geoms = geo_unit_1_geoms, 
    topics = list(
      "topic A" = list(
        ... # parameters for topic A
        summary_indicators = list(
          "first indicator" = list(
            ... # parameters for the first indicator of topic A
          ),
          "second indicator" = list(
            ... # parameters for the second indicator of topic A
          )
          ... # other indicators for topic A
        )
      ),
      "topic B" = list(
        ... # parameters for topic B, including indicators
      ),
      ... # other topics
    )
  ),
  "geo unit 2" = list(
    geoms = geo_unit_1_geoms, 
    topics = list(
    ... # topics for geo unit 2
    )
  ),
  ... # other geographic units
)
```

Examples of geographic units include "census tracts" and "neighborhoods". Each geographic unit becomes a tab on the app. Tabs are displayed from left to right in the order in which they're declared in `APP_CONFIG`. The declared names of geographic units should be entered in all lower case, but they will be converted into title case when displayed on tabs. The geoms parameter should be a simple features object representing the polygons for the geographic unit that make up your study area. see the section below or move it here

Examples of topics include "Age" and "Total Housing Units". Each topic declared for a given geographic unit becomes an entry on that tab's drop-down menu. Topic entries are displayed from top to bottom in the order in which they're declared in `APP_CONFIG`. The names of topics appear on the app exactly as they are declared.

Examples of indicators within an "Age" topic might include "Young adult (20-34) share of population" or "Total population aged 65+". Indicator options are displayed from top to bottom in the order in which they're declared in `APP_CONFIG`. The names of indicators appear on the app exactly as they are declared.

For example, if the above pseudocode were to be used in a new instance of the Neighborhood Change Explorer, it would look like this:

![screenshot of ](img/geo_topic_demo.png) # TODO: update this image with indicators

## Setting up data for the Neighborhood Change Explorer

Each combination of geographic unit, topic, and indicator constitutes a unique **variable**. Variables are the fundamental unit of analysis within the Neighborhood Change Explorer. `ui.R` and `server.R` are basically large functions that are designed to display data about one variable at a time in response to user selections.

the data needed for a topic are the tabular data and the geographic information to associate particular rows with particular features on the map

### Ingesting geographies

geographic units require spatial data formats - section on how to modify pull_data with a custom set of polygon or multipolygon geometries; the requirement for a GEOID; what file formats read_sf supports

### Ingesting tabular data

Part of the utility of pull_data is that you can create a csv file with your data in a specific format, store it in the csv folder, and then associate that file name with a particular topic in APP_CONFIG. Then, you can run pull_data and it will read in the csv file and process the data in such a way that the NCE can display your data.

csv format: required columns, unique keys, how missing values in a row and missing elements in a time series are handled

### how the app processes your data

The Neighborhood Change Explorer app uses R data frames to store and work with the data for each topic. A data frame is a data structure can be thought of as a spreadsheet table with rows of features and columns of fields / attributes.

The app also uses [simple features objects](https://r-spatial.github.io/sf/articles/sf1.html), which are basically just data frames with an additional geometry column that stores the spatial properties of each feature. Simple features objects allow spatial data to be displayed on a map.

pull_data creates four data frames for each topic based on a given csv:

| name | alias | required fields | fields which uniquely identify each row | type of R object | where it's used |
| -------- | --------- | --------- | ---------- | ---------- | -------- |
| sb_df | subcity bins | GEOID, NAME, CATEGORY, YEAR, VALUE | GEOID, CATEGORY, YEAR | data frame | bar chart |
| ss_df | subcity summary | GEOID, NAME, YEAR, VALUE | GEOID, YEAR | simple features object | line chart, map |
| cb_df | citywide bins | CATEGORY, YEAR, SUMMARY_VALUE | CATEGORY, YEAR | data frame | bar chart |
| cs_df | citywide summary | YEAR, SUMMARY_VALUE | YEAR | data frame | line chart |

ss_df is a simple features object because that is the data frame that is used for the map. sb_df and cb_df are used on the bar chart, and cs_df and ss_df are used on the line chart.

For each topic, all four data frames are bundled together into a list and stored in an RDS file named after the variable code. Each file within the `data/` folder contains all the data for a given geographic unit and topic. 

use pull data to both create new data files and modify existing ones. walk through the example of modifying a category name.

also talk about the use case of overrides and walk through an example of that.

## Configuring topics and defining indicators

 The configuration of each variable is defined by a set of **parameters** within APP_CONFIG. this is where you specify the csv name(s) but also control things like how the data should be formatted for display, and define indicators.

### Topic parameters

| parameter | required? | description | example |
| ------ | ---- | ------ | ----- |
| data_code | yes | name of the corresponding .RDS file in the `data/` folder | "hbicttp" |

etc fill out topic params

The data for each topic consists of:
* topic: 
* summary indicator: eg young adult share, non white-alone share, total housing units
* category: eg population 10-19, 20-34. CATEGORY field is category names to be displayed. should have more than 1 for the bar chart to be interesting
* category label: column alias for a given category xyz, used in summary expression
* geographic unit: eg tracts, neighborhoods, block groups
* geographic area: uniquely identified with GEOID field, and named/labeled with the NAME field. should be non overlapping
* aggregate function
* summary expression
* variable code 
* summary indicators

### Indicator parameters

For some topics, it may only make sense to have one indicator, but for other topics, particularly ones with a larger number of categories, a larger number of indicators may be possible and desirable to implement.

| parameter | required? | description | example |
| ------ | ---- | ------ | ----- |
| summary_expression | yes | An R expression object showing what operations to perform on category aliases in order to compute an indicator | `rlang::expr(foreign / (foreign + native))` |

etc fill out indicator params

and i think that's it for this document
# Import all libraries, define data parameters, and read all global data structures 

# Imports #####
library(sf)
library(dplyr)
library(tidyr)
library(shiny)
library(shinyWidgets)
library(plotly)
library(leaflet)
library(htmltools)

# Define global formatting parameters #######
APP_FONT <- "Helvetica"
APP_FONT_SIZE <- 17
MAP_PALETTE <- "YlGnBu" # https://r-graph-gallery.com/38-rcolorbrewers-palettes.html
BAR_COLOR <- '#60809f' # previously, we set the map palette as Purples with bar/line color 7f76b7
LINE_COLOR <- BAR_COLOR # could also change line to a separate hex code if desired

# Define parameters for each geography type and variable ############
ALL_VARS_INFO <- list()

# TODO: documentation explaining what each of these parameters does, and the ordering
ALL_VARS_INFO$tracts <- list(
  "Total Population" = list(varcode = "hbicttp", start = 1950, end = 2020, step = 10,
   lineTitle = "Total population", linehoverformat = ",.0f", 
   tickprefix = NULL, tickformat = NULL, agg_func = sum, citywide_comparison = FALSE,
   barTitle = "Population by sex", barhoverformat = ",.0f",
   barCats = list(
     "Male" = "male"
     , "Female" = "female"
   ), summary_expression = rlang::expr(male + female), 
   source = "U.S. Census Bureau, 1950-2020 Decennial Censuses (with 2020 adjusted to reflect Boston's successful group quarters challenge); IPUMS-NHGIS, University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
  )
  
  , "Age" = list(varcode = "hbicta", start = 1950, end = 2020, step = 10,
   lineTitle = "Young adult (20-34) share of population", linehoverformat = ".0%",
   tickprefix = NULL, tickformat = ".0%", agg_func = sum, citywide_comparison = TRUE,
   barTitle = "Population by age", barhoverformat = ",.0f",
   barCats = list(
     "0-9 years" = "zero_nine",
     "10-19 years" = "ten_nineteen",
     "20-34 years" = "twenty_thirtyfour",
     "35-54 years" = "thirtyfive_fiftyfour",
     "55-64 years" = "fiftyfive_sixtyfour",
     "65 years and over" = "sixtyfive_more"
   ), summary_expression = rlang::expr(
     (twenty_thirtyfour) /
       (zero_nine + ten_nineteen + twenty_thirtyfour + 
          thirtyfive_fiftyfour + fiftyfive_sixtyfour + sixtyfive_more)
   ), source = "U.S. Census Bureau, 1950-2010 Decennial Censuses, 2016-2020 American Community Survey, IPUMS-NHGIS, University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
  )
  
  , "Race and Ethnicity" = list(varcode = "hbictre", start = 1950, end = 2020, step = 10,
    lineTitle = "Non-white share of population", linehoverformat = ".0%",
    tickprefix = NULL, tickformat = ".0%", agg_func = sum, citywide_comparison = TRUE,
    barTitle = "Population by race/ethnicity", barhoverformat = ",.0f",
    barCats = list(
      "White" = "white",
      "Black/African American" = "black",
      "Hispanic/Latino" = "hisp",
      "Asian/Pacific Islander" = "asian",
      "Native American" = "native",
      "Two or More" = "two_plus",
      "Other" = "other"
    ), summary_expression = rlang::expr(
      (black + hisp + asian + native + two_plus + other) /
        (white + black + hisp + asian + native + two_plus + other)
    ), 
    source = "U.S. Census Bureau, 1950-2020 Decennial Censuses, IPUMS-NHGIS, University of Minnesota, www.nhgis.org; BPDA Research Division Analysis",
    note = "Note: In 1950 and 1960, the only race/ethnicity categories on the Census were White, Black, and Other."
  )
  
  , "Nativity" = list(varcode = "hbictnat", start = 1950, end = 2020, step = 10,
    lineTitle = "Foreign-born share of population", linehoverformat = ".0%",
    tickprefix = NULL, tickformat = ".0%", agg_func = sum, citywide_comparison = TRUE,
    barTitle = "Population by nativity", barhoverformat = ",.0f",
    barCats = list("Native-born" = "native", "Foreign-born" = "foreign"), 
    summary_expression = rlang::expr(foreign / (foreign + native)),
    source = "U.S. Census Bureau, 1950-2000 Decennial Censuses, 2006-2010 & 2016-2020 American Community Survey, IPUMS-NHGIS, University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
  )
  
  , "Educational Attainment" = list(varcode = "hbictedu", start = 1950, end = 2020, step = 10,
    lineTitle = "Share of population with a bachelor's degree or higher", linehoverformat = ".0%",
    tickprefix = NULL, tickformat = ".0%", agg_func = sum, citywide_comparison = TRUE,
    barTitle = "Population by educational attainment", barhoverformat = ",.0f",
    barCats = list(
      "Less than high school" = "lhs",
      "High school or some equivalent" = "he",
      "Some college" = "sc",
      "Bachelor's or more" = "bm"
    ), summary_expression = rlang::expr(bm / (lhs + he + sc + bm)),
    source = "U.S. Census Bureau, 1950-2000 Decennial Censuses, 2006-2010 & 2016-2020 American Community Survey, IPUMS-NHGIS, University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
  )
  
  , "Labor Force" = list(varcode = "hbictlf", start = 1950, end = 2020, step = 10,
    lineTitle = "Female labor force participation rate (16+)", linehoverformat = ".0%",
    tickprefix = NULL, tickformat = ".0%", agg_func = sum, citywide_comparison = TRUE,
    barTitle = "Population (16+) by labor force status and sex", barhoverformat = ",.0f",
    barCats = list(
      "Male in labor force" = "ilf_m"
      , "Male not in labor force" = "nilf_m"
      , "Female in labor force" = "ilf_f"
      , "Female not in labor force" = "nilf_f"
    ), summary_expression = rlang::expr(ilf_f / (ilf_f + nilf_f)),
    source = "U.S. Census Bureau, 1950-2000 Decennial Censuses, 2006-2010 & 2016-2020 American Community Survey, IPUMS-NHGIS, University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
  )
  
  # , "Income" = list(varcode = 'acshhi', start = 2010, end = 2018, step = 2,
  #   lineTitle = "Median Household Income", linehoverformat = ",.0f",
  #   tickprefix = "$", tickformat = "~s", agg_func = sum, citywide_comparison = TRUE,
  #   barTitle = "Households by Income", barhoverformat = ",.0f",
  #   barCats = list(
  #     "Less than $10,000" = "S1901_C01_002"
  #     , "$10,000 to $14,999" = "S1901_C01_003"
  #     , "$15,000 to $24,999" = "S1901_C01_004"
  #     , "$25,000 to $34,999" = "S1901_C01_005"
  #     , "$35,000 to $49,999" = "S1901_C01_006"
  #     , "$50,000 to $74,999" = "S1901_C01_007"
  #     , "$75,000 to $99,999" = "S1901_C01_008"
  #     , "$100,000 to $149,999" = "S1901_C01_009"
  #     , "$150,000 to $199,999" = "S1901_C01_010"
  #     , "More than $200,000" = "S1901_C01_011"
  #   ), summary_expression = rlang::expr(pareto_median_income(
  #     hh_by_income = c(S1901_C01_002, S1901_C01_003, S1901_C01_004,
  #                      S1901_C01_005, S1901_C01_006, S1901_C01_007,
  #                      S1901_C01_008, S1901_C01_009, S1901_C01_010, S1901_C01_011)
  #     , cutoffs = c(10000, 15000, 25000, 35000, 50000, 75000, 100000, 150000, 200000)
  #   ))
  # )
  
  , "Total Housing Units" = list(varcode = "hbicthou", start = 1950, end = 2020, step = 10,
    lineTitle = "Total housing units", linehoverformat = ",.0f",
    tickprefix = NULL, tickformat = NULL, agg_func = sum, citywide_comparison = FALSE,
    barTitle = "Housing units by occupancy", barhoverformat = ",.0f",
    barCats = list(
      "Occupied" = "occ",
      "Vacant" = "vac"
    ), summary_expression = rlang::expr(vac + occ),
    source = "U.S. Census Bureau, 1950-2020 Decennial Censuses, IPUMS-NHGIS, University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
  )
  
  , "Housing Occupancy" = list(varcode = "hbicthouvac", start = 1950, end = 2020, step = 10,
    lineTitle = "Housing vacancy rate", linehoverformat = ".0%",
    tickprefix = NULL, tickformat = ".0%", agg_func = sum, citywide_comparison = TRUE,
    barTitle = "Housing units by occupancy", barhoverformat = ",.0f",
    barCats = list(
      "Occupied" = "occ",
      "Vacant" = "vac"
    ), summary_expression = rlang::expr(vac / (vac + occ)),
    source = "U.S. Census Bureau, 1950-2020 Decennial Censuses, IPUMS-NHGIS, University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
  )
  
  , "Housing Tenure" = list(varcode = "hbicthouten", start = 1950, end = 2020, step = 10,
    lineTitle = "Owner occupancy rate", linehoverformat = ".0%",
    tickprefix = NULL, tickformat = ".0%", agg_func = sum, citywide_comparison = TRUE,
    barTitle = "Occupied housing units by tenure", barhoverformat = ",.0f",
    barCats = list(
      "Owner-occupied" = "owner",
      "Renter-occupied" = "renter"
    ), summary_expression = rlang::expr(owner / (owner + renter)),
    source = "U.S. Census Bureau, 1950-2010 Decennial Censuses, 2016-2020 American Community Survey, IPUMS-NHGIS, University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
  )
)

ALL_VARS_INFO$neighborhoods <- list(
  "Total Population" = list(varcode = "hbicntp", start = 1950, end = 2020, step = 10,
   lineTitle = "Total population", linehoverformat = ",.0f",
   tickprefix = NULL, tickformat = NULL, agg_func = sum, citywide_comparison = FALSE,
   barTitle = "Population by sex", barhoverformat = ",.0f",
   barCats = list(
     "Male" = "male"
     , "Female" = "female"
   ), summary_expression = rlang::expr(male + female), 
   source = "U.S. Census Bureau, 1950-2020 Decennial Censuses (with 2020 adjusted to reflect Boston's successful group quarters challenge); IPUMS-NHGIS, University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
  )
  
  , "Age" = list(varcode = "hbicna", start = 1950, end = 2020, step = 10,
   lineTitle = "Young adult (20-34) share of population", linehoverformat = ".0%",
   tickprefix = NULL, tickformat = ".0%", agg_func = sum, citywide_comparison = TRUE,
   barTitle = "Population by age", barhoverformat = ",.0f",
   barCats = list(
     "0-9 years" = "zero_nine",
     "10-19 years" = "ten_nineteen",
     "20-34 years" = "twenty_thirtyfour",
     "35-54 years" = "thirtyfive_fiftyfour",
     "55-64 years" = "fiftyfive_sixtyfour",
     "65 years and over" = "sixtyfive_more"
   ), summary_expression = rlang::expr(
     (twenty_thirtyfour) /
       (zero_nine + ten_nineteen + twenty_thirtyfour + 
          thirtyfive_fiftyfour + fiftyfive_sixtyfour + sixtyfive_more)
   ),
   source = "U.S. Census Bureau, 1950-2010 Decennial Censuses, 2016-2020 American Community Survey, IPUMS-NHGIS, University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
  )
  
  , "Race and Ethnicity" = list(varcode = "hbicnre", start = 1950, end = 2020, step = 10,
    lineTitle = "Non-white share of population", linehoverformat = ".0%",
    tickprefix = NULL, tickformat = ".0%", agg_func = sum, citywide_comparison = TRUE,
    barTitle = "Population by race/ethnicity", barhoverformat = ",.0f",
    barCats = list(
      "White" = "white",
      "Black/African American" = "black",
      "Hispanic/Latino" = "hisp",
      "Asian/Pacific Islander" = "asian",
      "Native American" = "native",
      "Two or More" = "two_plus",
      "Other" = "other"
    ), summary_expression = rlang::expr(
      (black + hisp + asian + native + two_plus + other) /
        (white + black + hisp + asian + native + two_plus + other)
    ), 
    source = "U.S. Census Bureau, 1950-2020 Decennial Censuses, IPUMS-NHGIS, University of Minnesota, www.nhgis.org; BPDA Research Division Analysis",
    note = "Note: In 1950 and 1960, the only race/ethnicity categories on the Census were White, Black, and Other."
  )
  
  , "Nativity" = list(varcode = "hbicnnat", start = 1950, end = 2020, step = 10,
    lineTitle = "Foreign-born share of population", linehoverformat = ".0%",
    tickprefix = NULL, tickformat = ".0%", agg_func = sum, citywide_comparison = TRUE,
    barTitle = "Population by nativity", barhoverformat = ",.0f",
    barCats = list("Native-born" = "native", "Foreign-born" = "foreign"), 
    summary_expression = rlang::expr(foreign / (foreign + native)),
    source = "U.S. Census Bureau, 1950-2000 Decennial Censuses, 2006-2010 & 2016-2020 American Community Survey, IPUMS-NHGIS, University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
  )
  
  , "Educational Attainment" = list(varcode = "hbicnedu", start = 1950, end = 2020, step = 10,
    lineTitle = "Share of population with a bachelor's degree or higher", linehoverformat = ".0%",
    tickprefix = NULL, tickformat = ".0%", agg_func = sum, citywide_comparison = TRUE,
    barTitle = "Population by educational attainment", barhoverformat = ",.0f",
    barCats = list(
      "Less than high school" = "lhs",
      "High school or some equivalent" = "he",
      "Some college" = "sc",
      "Bachelor's or more" = "bm"
    ), summary_expression = rlang::expr(bm / (lhs + he + sc + bm)),
    source = "U.S. Census Bureau, 1950-2000 Decennial Censuses, 2006-2010 & 2016-2020 American Community Survey, IPUMS-NHGIS, University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
  )
  
  , "Labor Force" = list(varcode = "hbicnlf", start = 1950, end = 2020, step = 10,
    lineTitle = "Female labor force participation rate (16+)", linehoverformat = ".0%",
    tickprefix = NULL, tickformat = ".0%", agg_func = sum, citywide_comparison = TRUE,
    barTitle = "Population (16+) by labor force status and sex", barhoverformat = ",.0f",
    barCats = list(
      "Male in labor force" = "ilf_m"
      , "Male not in labor force" = "nilf_m"
      , "Female in labor force" = "ilf_f"
      , "Female not in labor force" = "nilf_f"
    ), summary_expression = rlang::expr(ilf_f / (ilf_f + nilf_f)),
    source = "U.S. Census Bureau, 1950-2000 Decennial Censuses, 2006-2010 & 2016-2020 American Community Survey, IPUMS-NHGIS, University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
  )
  
  , "Total Housing Units" = list(varcode = "hbicnhou", start = 1950, end = 2020, step = 10,
    lineTitle = "Total housing units", linehoverformat = ",.0f",
    tickprefix = NULL, tickformat = NULL, agg_func = sum, citywide_comparison = FALSE,
    barTitle = "Housing units by occupancy", barhoverformat = ",.0f",
    barCats = list(
      "Occupied" = "occ",
      "Vacant" = "vac"
    ), summary_expression = rlang::expr(vac + occ),
    source = "U.S. Census Bureau, 1950-2020 Decennial Censuses, IPUMS-NHGIS, University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
  )
  
  , "Housing Occupancy" = list(varcode = "hbicnhouvac", start = 1950, end = 2020, step = 10,
    lineTitle = "Housing vacancy rate", linehoverformat = ".0%",
    tickprefix = NULL, tickformat = ".0%", agg_func = sum, citywide_comparison = TRUE,
    barTitle = "Housing units by occupancy", barhoverformat = ",.0f",
    barCats = list(
      "Occupied" = "occ",
      "Vacant" = "vac"
    ), summary_expression = rlang::expr(vac / (vac + occ)),
    source = "U.S. Census Bureau, 1950-2020 Decennial Censuses, IPUMS-NHGIS, University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
  )
  
  , "Housing Tenure" = list(varcode = "hbicnhouten", start = 1950, end = 2020, step = 10,
    lineTitle = "Owner occupancy rate", linehoverformat = ".0%",
    tickprefix = NULL, tickformat = ".0%", agg_func = sum, citywide_comparison = TRUE,
    barTitle = "Occupied housing units by tenure", barhoverformat = ",.0f",
    barCats = list(
      "Owner-occupied" = "owner",
      "Renter-occupied" = "renter"
    ), summary_expression = rlang::expr(owner / (owner + renter)),
    source = "U.S. Census Bureau, 1950-2010 Decennial Censuses, 2016-2020 American Community Survey, IPUMS-NHGIS, University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
  )
) 

# Miscellaneous functions ###########

#' Given a string, returns that string with an HTML <br> inserted as close to 
#' halfway through the string as possible without breaking up a word, so that 
#' when the string is displayed, it will wrap onto two lines. The new line is 
#' only inserted if the original string has more than 20 characters.
split_max_2_lines <- function(s) {
  words <- unlist(strsplit(s, " +"))
  num_words <- length(words)
  num_chars <- nchar(s) # sum(words %>% sapply(nchar))

  if (num_chars < 20) {
    return(s)
  } else {
    halfway <- num_chars/2
    dist_to_halfway <- halfway
    closest_to_halfway <- 0
    chars_so_far <- 0
    
    for (i in 1:num_words) {
      chars_so_far <- chars_so_far + nchar(words[i]) + 1
      dist_to_halfway_i <- abs(halfway - chars_so_far)
      if (dist_to_halfway_i < dist_to_halfway) {
        dist_to_halfway <- dist_to_halfway_i
        closest_to_halfway <- i
      } else {
        words <- append(words, "<br>", after=closest_to_halfway)
        return(paste(words, collapse = " "))
      }
    }
  }
}

#' Implements https://en.wikipedia.org/wiki/Pareto_interpolation, returning the
#' estimated median income of a population given that lower_pct of the individuals
#' in a sample have incomes below lower_income and upper_pct of the individuals in
#' that sample have incomes below upper_income. The percentages should be passed
#' in as decimals between 0 and 1.
pareto_median <- function(lower_income, upper_income, lower_pct, upper_pct) {
  if (lower_pct == 0) {
    return(lower_income + ((upper_income - lower_income) / 2))
  } else {
    theta_hat <- (log(1-lower_pct) - log(1-upper_pct)) / (log(upper_income) - log(lower_income))
    k_hat <- ( (upper_pct - lower_pct) / 
                 ((1/(lower_income ^ theta_hat)) - (1/(upper_income ^ theta_hat)))
    ) ^ (1/theta_hat)
    return(k_hat * (2 ^ (1/theta_hat)))
  }
}


#' Returns Pareto median income given hh_by_income, a list with the numbers of households per 
#' income bucket, ordered from lowest to highest income, and cutoffs, a list with the
#' dollar amounts at the top of each income bucket, again ordered from lowest to
#' highest. The first element of hh_by_income should be the number of households making 
#' incomes between 0 and the first cutoff, and the last element of hh_by_income should be 
#' the number of households making incomes above the last cutoff.
pareto_median_income <- function(hh_by_income, cutoffs) {

  if (length(hh_by_income) - length(cutoffs) != 1) {
    stop("Income data must have as many elements as the number of income cutoffs plus one.")
  }
  
  num_bins <- length(hh_by_income)
  total <- sum(hh_by_income)
  sum_so_far <- 0
  
  for (i in 1:(num_bins - 2)) {
    upper_sum <- sum_so_far + hh_by_income[i]
    
    if (upper_sum > total / 2) {
      return(pareto_median(
        lower_pct = sum_so_far / total
        , upper_pct = upper_sum / total
        , lower_income = ifelse(i == 1, 0, cutoffs[i-1])
        , upper_income = cutoffs[i]
      ))
    }
    sum_so_far <- upper_sum
  }
  
  # if >50% of HH are in the highest bin ($$ and above), return the highest $$
  return(cutoffs[length(cutoffs)])
}

#' GitHub user mpriem89 wrote this method as a workaround for an open Leaflet
#' issue regarding map legends: https://github.com/rstudio/leaflet/issues/256
#' The function can be used on a leaflet map in place of addLegend when you want the
#' values of the legend to decrease and have the color palette continue to match.
addLegend_decreasing <- function (map, position = c("topright", "bottomright", "bottomleft","topleft"),
                                  pal, values, na.label = "NA", bins = 7, colors,
                                  opacity = 0.5, labels = NULL, labFormat = labelFormat(),
                                  title = NULL, className = "info legend", layerId = NULL,
                                  group = NULL, data = getMapData(map), decreasing = FALSE) {
  
  position <- match.arg(position)
  type <- "unknown"
  na.color <- NULL
  extra <- NULL
  if (!missing(pal)) {
    if (!missing(colors))
      stop("You must provide either 'pal' or 'colors' (not both)")
    if (missing(title) && inherits(values, "formula"))
      title <- deparse(values[[2]])
    values <- evalFormula(values, data)
    type <- attr(pal, "colorType", exact = TRUE)
    args <- attr(pal, "colorArgs", exact = TRUE)
    na.color <- args$na.color
    if (!is.null(na.color) && col2rgb(na.color, alpha = TRUE)[[4]] ==
        0) {
      na.color <- NULL
    }
    if (type != "numeric" && !missing(bins))
      warning("'bins' is ignored because the palette type is not numeric")
    if (type == "numeric") {
      cuts <- if (length(bins) == 1)
        pretty(values, bins)
      else bins
      if (length(bins) > 2)
        if (!all(abs(diff(bins, differences = 2)) <=
                 sqrt(.Machine$double.eps)))
          stop("The vector of breaks 'bins' must be equally spaced")
      n <- length(cuts)
      r <- range(values, na.rm = TRUE)
      cuts <- cuts[cuts >= r[1] & cuts <= r[2]]
      n <- length(cuts)
      p <- (cuts - r[1])/(r[2] - r[1])
      extra <- list(p_1 = p[1], p_n = p[n])
      p <- c("", paste0(100 * p, "%"), "")
      if (decreasing == TRUE){
        colors <- pal(rev(c(r[1], cuts, r[2])))
        labels <- rev(labFormat(type = "numeric", cuts))
      }else{
        colors <- pal(c(r[1], cuts, r[2]))
        labels <- rev(labFormat(type = "numeric", cuts))
      }
      colors <- paste(colors, p, sep = " ", collapse = ", ")
    }
    else if (type == "bin") {
      cuts <- args$bins
      n <- length(cuts)
      mids <- (cuts[-1] + cuts[-n])/2
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "bin", cuts))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "bin", cuts)
      }
    }
    else if (type == "quantile") {
      p <- args$probs
      n <- length(p)
      cuts <- quantile(values, probs = p, na.rm = TRUE)
      mids <- quantile(values, probs = (p[-1] + p[-n])/2, na.rm = TRUE)
      if (decreasing == TRUE){
        colors <- pal(rev(mids))
        labels <- rev(labFormat(type = "quantile", cuts, p))
      }else{
        colors <- pal(mids)
        labels <- labFormat(type = "quantile", cuts, p)
      }
    }
    else if (type == "factor") {
      v <- sort(unique(na.omit(values)))
      colors <- pal(v)
      labels <- labFormat(type = "factor", v)
      if (decreasing == TRUE){
        colors <- pal(rev(v))
        labels <- rev(labFormat(type = "factor", v))
      }else{
        colors <- pal(v)
        labels <- labFormat(type = "factor", v)
      }
    }
    else stop("Palette function not supported")
    if (!any(is.na(values)))
      na.color <- NULL
  }
  else {
    if (length(colors) != length(labels))
      stop("'colors' and 'labels' must be of the same length")
  }
  legend <- list(colors = I(unname(colors)), labels = I(unname(labels)),
                 na_color = na.color, na_label = na.label, opacity = opacity,
                 position = position, type = type, title = title, extra = extra,
                 layerId = layerId, className = className, group = group)
  invokeMethod(map, data, "addLegend", legend)
}

# Data loading ######################

#' Reads in and returns the four dataframes for a given variable defined by
#' varcode. The data files need to use the naming convention <varcode>_<df_type>.rds
dfs_from_varcode <- function(varcode) {
  
  # subcity bins, subcity summary, citywide bins, citywide summary
  df_types <- c('sb', 'ss', 'cb', 'cs')
  
  dfs <- lapply(df_types, function(x) paste0("./data/", varcode, "_", x, ".rds")) %>%
    lapply(readRDS) %>% `names<-`(lapply(df_types, function(x) paste0(x, '_df')))
  dfs #%>% lapply(as.data.frame) %>% lapply(function(df) df %>% mutate(YEAR = as.character(YEAR)))
}

# using ALL_VARS_INFO, read the four data frames for each variable into ALL_VARS_DATA
# ALL_VARS_DATA has the same structure as ALL_VARS_INFO, but with a list of data
# frames instead of a list of parameters being stored for each variable
ALL_VARS_DATA <- ALL_VARS_INFO %>% 
  lapply(function(geo_type) geo_type %>% 
    lapply(
      function(var) var$varcode %>% 
        dfs_from_varcode
    )
  )
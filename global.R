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

#' Helper function which pivots a dataframe of binned values into a dataframe of summary values 
pivot_summarise <- function(df, cats, summary_expr, id_columns) {
  df$CATEGORY <- df$CATEGORY %>% recode(!!!cats) # rename categories from their display names to their aliases
  df <- df %>% 
    pivot_wider(id_cols = all_of(id_columns),
                names_from = 'CATEGORY', 
                values_from = 'VALUE') %>% 
    mutate(SUMMARY_VALUE = !!summary_expr) %>% 
    select(-all_of(unlist(unname(cats))))
  df
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

# this configuration object is created and modified in prep_data.R, along with the rds data files for each topic
APP_CONFIG <- readRDS("config/APP_CONFIG.rds") 

# using APP_CONFIG, read the data frames for each variable into APP_DATA
# APP_DATA has the same structure as APP_CONFIG, but with a list of data
# frames instead of a list of parameters being stored for each topic
APP_DATA <- APP_CONFIG %>% 
  lapply(function(geo_type) geo_type$topics %>% 
    lapply(
      function(topic) readRDS(sprintf("./data/%s.rds", topic$data_code))
    )
  )
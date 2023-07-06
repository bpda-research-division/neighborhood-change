# Imports and Setup #####
library(dplyr)

# Define details for each variable ############
all_vars_info <- list()

all_vars_info$neighborhoods <- list(
  "Labor Force" = list(varcode = "hbicnlf", start = 1950, end = 2020, step = 10,
    lineTitle = "Female Labor Force Participation Rate", linehoverformat = ".0%",
    tickprefix = NULL, tickformat = ".0%", agg_func = sum,
    barTitle = "Labor Force Status by Sex", barhoverformat = ",.0f",
    barCats = list(
      "Male in labor force" = "ilf_m"
      , "Male not in labor force" = "nilf_m"
      , "Female in labor force" = "ilf_f"
      , "Female not in labor force" = "nilf_f"
    ), summary_expression = rlang::expr(ilf_f / (ilf_f + nilf_f))
  )
  , "Race and Ethnicity" = list(varcode = "hbicnre", start = 1950, end = 2020, step = 10,
    lineTitle = "Non-white Share of Population", linehoverformat = ".0%",
    tickprefix = NULL, tickformat = ".0%", agg_func = sum,
    barTitle = "Population by Race/Ethnicity", barhoverformat = ",.0f",
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
    )
  )
) # %>% as.data.frame() #%>% setNames(var_attrs)

all_vars_info$tracts <- list(
  "Income" = list(varcode = 'acshhi', start = 2010, end = 2018, step = 2,
    lineTitle = "Median Household Income", linehoverformat = ",.0f",
    tickprefix = "$", tickformat = "~s", agg_func = sum,
    barTitle = "Households by Income", barhoverformat = ",.0f",
    barCats = list(
      "Less than $10,000" = "S1901_C01_002"
      , "$10,000 to $14,999" = "S1901_C01_003"
      , "$15,000 to $24,999" = "S1901_C01_004"
      , "$25,000 to $34,999" = "S1901_C01_005"
      , "$35,000 to $49,999" = "S1901_C01_006"
      , "$50,000 to $74,999" = "S1901_C01_007"
      , "$75,000 to $99,999" = "S1901_C01_008"
      , "$100,000 to $149,999" = "S1901_C01_009"
      , "$150,000 to $199,999" = "S1901_C01_010"
      , "More than $200,000" = "S1901_C01_011"
    ), summary_expression = rlang::expr(pareto_median_income(
      hh_by_income = S1901_C01_005 # TODO: vectorize the pareto function?
      , cutoffs = c(35000, 50000, 75000, 100000)
    ))
    # , "Age" = list(...)
  )
) # %>% as.data.frame() #%>% setNames(var_attrs)

# Miscellaneous Functions ###########

#' Implements https://en.wikipedia.org/wiki/Pareto_interpolation
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

#' hh_by_income is a list with the numbers of households per income bucket,
#' ordered from lowest income to highest income
pareto_median_income <- function(hh_by_income, cutoffs) {
  
  if (length(hh_by_income) - length(cutoffs) != 1) {
    print(hh_by_income)
    print(cutoffs)
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

# I grabbed the code for this function from GitHub user mpriem89, who wrote it as a workaround
# for an open Leaflet issue regarding map legends: https://github.com/rstudio/leaflet/issues/256
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
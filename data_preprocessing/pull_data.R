# Imports and Setup ##############
library(dplyr)
library(tidyr)
library(sf)

tract2020_geoms <- read_sf('../geoms/boston_tracts_2020_complex.geojson') %>% mutate(GEOID = as.character(geoid20))
neigh2020_geoms <- read_sf('../geoms/boston_neighborhoods_2020tract.geojson') %>% mutate(GEOID = nbhd)

# Define parameters for each geography type and variable ##########
APP_CONFIG <- list(
  "census tracts" = list(geoms = tract2020_geoms, topics = list(

    "Population" = list(
      data_code = 'hbicttp', agg_func = sum, 
      sb_csv = 'csv/hbic_tract_totpop_sex_bins.csv', cb_csv = 'csv/hbictpop_cb.csv',
      barTitle = "Population by sex", barhoverformat = ",.0f",
      barCats = list("Male" = "male", "Female" = "female"),
      summary_indicators = list(
        "Total Population" = list(
          summary_expression = rlang::expr(male + female),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickprefix = NULL, tickformat = ""
          # cs_csv = 'csv/hbictpop_cs.csv'
        ),
        "Female share of population" = list(
          summary_expression = rlang::expr(female / (male + female)), 
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickprefix = NULL, tickformat = ".0%"
        )
      ),
      source = "U.S. Census Bureau, 1950-2020 Decennial Censuses (with 2020 adjusted
        to reflect Boston's successful group quarters challenge); IPUMS-NHGIS,
        University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
    ),
    
    "Age" = list(
      data_code = "hbicta", agg_func = sum,
      sb_csv = 'csv/hbic_tract_age_year_bins.csv', 
      barTitle = "Population by age", barhoverformat = ",.0f",
      barCats = list(
        "0-9 years" = "zero_nine",
        "10-19 years" = "ten_nineteen",
        "20-34 years" = "twenty_thirtyfour",
        "35-54 years" = "thirtyfive_fiftyfour",
        "55-64 years" = "fiftyfive_sixtyfour",
        "65 years and over" = "sixtyfive_more"
       ),
     summary_indicators = list(
       "Share of population aged 20-34" = list(
         summary_expression = rlang::expr(
           (twenty_thirtyfour) /
             (zero_nine + ten_nineteen + twenty_thirtyfour + 
                thirtyfive_fiftyfour + fiftyfive_sixtyfour + sixtyfive_more)
         ),
         citywide_comparison = TRUE,
         hoverformat = ".0%", tickprefix = NULL, tickformat = ".0%"
       ),
       "Total population aged 20-34" = list(
         summary_expression = rlang::expr(twenty_thirtyfour),
         citywide_comparison = FALSE,
         hoverformat = ",.0f", tickprefix = NULL, tickformat = ""
       ),
       "Share of population aged 0-9" = list(
         summary_expression = rlang::expr(
           (zero_nine) /
             (zero_nine + ten_nineteen + twenty_thirtyfour + 
                thirtyfive_fiftyfour + fiftyfive_sixtyfour + sixtyfive_more)
         ),
         citywide_comparison = TRUE,
         hoverformat = ".0%", tickprefix = NULL, tickformat = ".0%"
       ),
       "Total population aged 0-9" = list(
         summary_expression = rlang::expr(zero_nine),
         citywide_comparison = FALSE,
         hoverformat = ",.0f", tickprefix = NULL, tickformat = ""
       ),
       "Share of population aged 55+" = list(
         summary_expression = rlang::expr(
           (fiftyfive_sixtyfour + sixtyfive_more) /
             (zero_nine + ten_nineteen + twenty_thirtyfour + 
                thirtyfive_fiftyfour + fiftyfive_sixtyfour + sixtyfive_more)
         ),
         citywide_comparison = TRUE,
         hoverformat = ".0%", tickprefix = NULL, tickformat = ".0%"
       ),
       "Total population aged 55+" = list(
         summary_expression = rlang::expr(fiftyfive_sixtyfour + sixtyfive_more),
         citywide_comparison = FALSE,
         hoverformat = ",.0f", tickprefix = NULL, tickformat = ""
       )
     ),
     source = "U.S. Census Bureau, 1950-2020 Decennial Censuses, 
      IPUMS-NHGIS, University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
    ),
    
    "Children by Age" = list(
      data_code = "chilta", agg_func = sum,
      sb_csv = 'csv/children_tract_age_bins.csv',
      barTitle = "Children by age", barhoverformat = ",.0f",
      barCats = list(
        "0-4 years" = "under5",
        "5-17 years" = "fiveto17"
      ),
      summary_indicators = list(
        "Total children (0-17)" = list(
          summary_expression = rlang::expr(under5 + fiveto17),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickprefix = NULL, tickformat = ""
        ),
        "Total children aged 5-17" = list(
          summary_expression = rlang::expr(fiveto17),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickprefix = NULL, tickformat = ""
        ),
        "Total children aged 0-4" = list(
          summary_expression = rlang::expr(under5),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickprefix = NULL, tickformat = ""
        )
      ),
      additional_null_geoms = c("Census Tract 9811"),
      source = "U.S. Census Bureau, 1980-2020 Decennial Censuses,
      IPUMS-NHGIS, University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
    ),

    "Race and Ethnicity" = list(
      data_code = "hbictre", agg_func = sum,
      sb_csv = 'csv/hbic_tract_race_ethn_bins.csv',
      barTitle = "Population by race/ethnicity", barhoverformat = ",.0f",
      barCats = list(
        "White" = "white",
        "Black/African American" = "black",
        "Hispanic/Latino" = "hisp",
        "Asian/Pacific Islander" = "asian",
        "American Indian" = "native",
        "Two or More" = "two_plus",
        "Other" = "other"
      ), 
      summary_indicators = list(
        "Non-white share of population" = list(
          summary_expression = rlang::expr(
            (black + hisp + asian + native + two_plus + other) /
              (white + black + hisp + asian + native + two_plus + other)
          ),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickprefix = NULL, tickformat = ".0%"
          ),
        "Share of population, Black alone" = list(
          summary_expression = rlang::expr(
            (black) /
              (white + black + hisp + asian + native + two_plus + other)
          ),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickprefix = NULL, tickformat = ".0%"
          ),
        "Share of population, Hispanic of any race" = list(
          summary_expression = rlang::expr(
            (hisp) /
              (white + black + hisp + asian + native + two_plus + other)
          ),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickprefix = NULL, tickformat = ".0%"
          ),
        "Share of population, Asian alone" = list(
          summary_expression = rlang::expr(
            (asian) /
              (white + black + hisp + asian + native + two_plus + other)
          ),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickprefix = NULL, tickformat = ".0%"
          )
      ),
      source = "U.S. Census Bureau, 1950-2020 Decennial Censuses, IPUMS-NHGIS,
        University of Minnesota, www.nhgis.org; BPDA Research Division Analysis",
      note = "Note: In 1950 and 1960, the only race/ethnicity categories on the Census were White, Black, and Other."
    ),
    
    "Children by Race and Ethnicity" = list(
      data_code = "chiltre", agg_func = sum,
      sb_csv = 'csv/children_tract_race_bins.csv',
      barTitle = "Children by race/ethnicity", barhoverformat = ",.0f",
      barCats = list(
        "White" = "white",
        "Black/African American" = "black",
        "Hispanic/Latino" = "hisp",
        "Asian/Pacific Islander" = "aapi",
        "American Indian" = "ainh",
        "Two or More" = "twoplus",
        "Other" = "other"
      ), 
      summary_indicators = list(
        "Non-white share of children" = list(
          summary_expression = rlang::expr(
            (black + hisp + aapi + ainh + twoplus + other) /
              (white + black + hisp + aapi + ainh + twoplus + other)
          ),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickprefix = NULL, tickformat = ".0%"
        ),
        "Share of children, Black alone" = list(
          summary_expression = rlang::expr(
            (black) /
              (white + black + hisp + aapi + ainh + twoplus + other)
          ),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickprefix = NULL, tickformat = ".0%"
        ),
        "Share of children, Hispanic of any race" = list(
          summary_expression = rlang::expr(
            (hisp) /
              (white + black + hisp + aapi + ainh + twoplus + other)
          ),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickprefix = NULL, tickformat = ".0%"
        ),
        "Share of children, Asian alone" = list(
          summary_expression = rlang::expr(
            (aapi) /
              (white + black + hisp + aapi + ainh + twoplus + other)
          ),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickprefix = NULL, tickformat = ".0%"
        )
      ),
      additional_null_geoms = c("Census Tract 9811"),
      source = "U.S. Census Bureau, 1980-2020 Decennial Censuses, IPUMS-NHGIS,
        University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
    ),
    
    "Nativity" = list(
      data_code = "hbictnat", agg_func = sum,
      sb_csv = 'csv/hbic_tract_nativity_bins.csv', 
      barTitle = "Population by nativity", barhoverformat = ",.0f",
      barCats = list("Native-born" = "native", "Foreign-born" = "foreign"),
      summary_indicators = list(
        "Foreign-born share of population" = list(
          summary_expression = rlang::expr(foreign / (foreign + native)),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickprefix = NULL, tickformat = ".0%"
        ),
        "Total foreign-born population" = list(
          summary_expression = rlang::expr(foreign),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickprefix = NULL, tickformat = ""
        )
      ),
      source = "U.S. Census Bureau, 1950-2000 Decennial Censuses, 2006-2010 & 2016-2020 American 
      Community Survey, IPUMS-NHGIS, University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
    ),
    
    "Educational Attainment" = list(
      data_code = "hbictedu", agg_func = sum,
      sb_csv = 'csv/hbic_tract_edu_attain_bins.csv',
      barTitle = "Population (25+) by educational attainment", barhoverformat = ",.0f",
      barCats = list(
        "Less than high school" = "lhs",
        "High school or some equivalent" = "he",
        "Some college" = "sc",
        "Bachelor's or more" = "bm"
      ),
      summary_indicators = list(
        "Share of population (25+) with bachelor's or more" = list(
          summary_expression = rlang::expr(bm / (lhs + he + sc + bm)),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickprefix = NULL, tickformat = ".0%"
        ),
        "Share of population (25+) with less than high school" = list(
          summary_expression = rlang::expr(lhs / (lhs + he + sc + bm)),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickprefix = NULL, tickformat = ".0%"
        )
      ),
      source = "U.S. Census Bureau, 1950-2000 Decennial Censuses, 2006-2010 & 2016-2020 American 
      Community Survey, IPUMS-NHGIS, University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
    ),
    
    "Labor Force" = list(
      data_code = "hbictlf",agg_func = sum,
      sb_csv = 'csv/hbic_tract_labor_force_bins.csv', 
      barTitle = "Population (16+) by labor force status and sex", barhoverformat = ",.0f",
      barCats = list(
        "Male in labor force" = "ilf_m"
        , "Male not in labor force" = "nilf_m"
        , "Female in labor force" = "ilf_f"
        , "Female not in labor force" = "nilf_f"
      ), 
      summary_indicators = list(
        "Female labor force participation rate (16+)" = list(
          summary_expression = rlang::expr(ilf_f / (ilf_f + nilf_f)),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickprefix = NULL, tickformat = ".0%"
        ),
        "Male labor force participation rate (16+)" = list(
          summary_expression = rlang::expr(ilf_m / (ilf_m + nilf_m)),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickprefix = NULL, tickformat = ".0%"
        )
      ),
      source = "U.S. Census Bureau, 1950-2000 Decennial Censuses, 2006-2010 & 2016-2020 American 
      Community Survey, IPUMS-NHGIS, University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
    ),
    # 
    # # "Income" = list(data_code = 'acshhi', 
    # #   lineTitle = "Median Household Income", linehoverformat = ",.0f",
    # #   tickprefix = "$", tickformat = "~s", agg_func = sum, citywide_comparison = TRUE,
    # #   barTitle = "Households by Income", barhoverformat = ",.0f",
    # #   barCats = list(
    # #     "Less than $10,000" = "S1901_C01_002"
    # #     , "$10,000 to $14,999" = "S1901_C01_003"
    # #     , "$15,000 to $24,999" = "S1901_C01_004"
    # #     , "$25,000 to $34,999" = "S1901_C01_005"
    # #     , "$35,000 to $49,999" = "S1901_C01_006"
    # #     , "$50,000 to $74,999" = "S1901_C01_007"
    # #     , "$75,000 to $99,999" = "S1901_C01_008"
    # #     , "$100,000 to $149,999" = "S1901_C01_009"
    # #     , "$150,000 to $199,999" = "S1901_C01_010"
    # #     , "More than $200,000" = "S1901_C01_011"
    # #   ), summary_expression = rlang::expr(pareto_median_income(
    # #     hh_by_income = c(S1901_C01_002, S1901_C01_003, S1901_C01_004,
    # #                      S1901_C01_005, S1901_C01_006, S1901_C01_007,
    # #                      S1901_C01_008, S1901_C01_009, S1901_C01_010, S1901_C01_011)
    # #     , cutoffs = c(10000, 15000, 25000, 35000, 50000, 75000, 100000, 150000, 200000)
    # #   ))
    # # )

    "Housing Units" = list(
      data_code = "hbicthou", agg_func = sum,
      sb_csv = 'csv/hbic_tract_housing_bins.csv', 
      cb_csv = 'csv/hbicthou_cb.csv',
      barTitle = "Housing units by occupancy", barhoverformat = ",.0f",
      barCats = list(
        "Occupied" = "occ",
        "Vacant" = "vac"
      ), 
      summary_indicators = list(
        "Total housing units" = list(
          summary_expression = rlang::expr(vac + occ),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickprefix = NULL, tickformat = ""
        )
      ),
      source = "U.S. Census Bureau, 1950-2020 Decennial Censuses, IPUMS-NHGIS, University of 
      Minnesota, www.nhgis.org; Mayor's Office of Housing; BPDA Research Division Analysis"
    ),

    "Housing Occupancy" = list(
      data_code = "hbicthouvac", agg_func = sum,
      sb_csv = 'csv/hbic_tract_housing_vacancy_bins.csv', 
      cb_csv = 'csv/hbicthouvac_cb.csv',
      barTitle = "Housing units by occupancy", barhoverformat = ",.0f",
      barCats = list(
        "Occupied" = "occ",
        "Vacant" = "vac"
      ),
      summary_indicators = list(
        "Housing vacancy rate" = list(
          summary_expression = rlang::expr(vac / (vac + occ)),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickprefix = NULL, tickformat = ".0%"
        ),
        "Total vacant units" = list(
          summary_expression = rlang::expr(vac),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickprefix = NULL, tickformat = ""
        )
      ),
      source = "U.S. Census Bureau, 1950-2020 Decennial Censuses, IPUMS-NHGIS, University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
    ),

    "Housing Tenure" = list(
      data_code = "hbicthouten", agg_func = sum,
      sb_csv = 'csv/hbic_tract_housing_tenure_bins.csv',
      barTitle = "Occupied housing units by tenure", barhoverformat = ",.0f",
      barCats = list(
        "Owner-occupied" = "owner",
        "Renter-occupied" = "renter"
      ), 
      summary_indicators = list(
        "Owner occupancy rate" = list(
          summary_expression = rlang::expr(owner / (owner + renter)),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickprefix = NULL, tickformat = ".0%"
        )
      ),
      source = "U.S. Census Bureau, 1950-2010 Decennial Censuses, 2016-2020 American 
      Community Survey, IPUMS-NHGIS, University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
    )
    )
  ),
  
  "neighborhoods" = list(geoms = neigh2020_geoms, topics = list(
    
    "Population" = list(
      data_code = 'hbicntp', agg_func = sum, 
      sb_csv = 'csv/hbic_neigh_totpop_sex_bins.csv',
      barTitle = "Population by sex", barhoverformat = ",.0f",
      barCats = list("Male" = "male", "Female" = "female"),
      summary_indicators = list(
        "Total Population" = list(
          summary_expression = rlang::expr(male + female),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickprefix = NULL, tickformat = ""
        ),
        "Female share of population" = list(
          summary_expression = rlang::expr(female / (male + female)), 
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickprefix = NULL, tickformat = ".0%"
        )
      ),
      source = "U.S. Census Bureau, 1950-2020 Decennial Censuses (with 2020 adjusted
        to reflect Boston's successful group quarters challenge); IPUMS-NHGIS,
        University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
    ),
    
    "Age" = list(
      data_code = "hbicna", agg_func = sum,
      sb_csv = 'csv/hbic_neigh_age_year_bins.csv', 
      barTitle = "Population by age", barhoverformat = ",.0f",
      barCats = list(
        "0-9 years" = "zero_nine",
        "10-19 years" = "ten_nineteen",
        "20-34 years" = "twenty_thirtyfour",
        "35-54 years" = "thirtyfive_fiftyfour",
        "55-64 years" = "fiftyfive_sixtyfour",
        "65 years and over" = "sixtyfive_more"
      ),
      summary_indicators = list(
        "Share of population aged 20-34" = list(
          summary_expression = rlang::expr(
            (twenty_thirtyfour) /
              (zero_nine + ten_nineteen + twenty_thirtyfour + 
                 thirtyfive_fiftyfour + fiftyfive_sixtyfour + sixtyfive_more)
          ),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickprefix = NULL, tickformat = ".0%"
        ),
        "Total population aged 20-34" = list(
          summary_expression = rlang::expr(twenty_thirtyfour),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickprefix = NULL, tickformat = ""
        ),
        "Share of population aged 0-9" = list(
          summary_expression = rlang::expr(
            (zero_nine) /
              (zero_nine + ten_nineteen + twenty_thirtyfour + 
                 thirtyfive_fiftyfour + fiftyfive_sixtyfour + sixtyfive_more)
          ),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickprefix = NULL, tickformat = ".0%"
        ),
        "Total population aged 0-9" = list(
          summary_expression = rlang::expr(zero_nine),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickprefix = NULL, tickformat = ""
        ),
        "Share of population aged 55+" = list(
          summary_expression = rlang::expr(
            (fiftyfive_sixtyfour + sixtyfive_more) /
              (zero_nine + ten_nineteen + twenty_thirtyfour + 
                 thirtyfive_fiftyfour + fiftyfive_sixtyfour + sixtyfive_more)
          ),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickprefix = NULL, tickformat = ".0%"
        ),
        "Total population aged 55+" = list(
          summary_expression = rlang::expr(fiftyfive_sixtyfour + sixtyfive_more),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickprefix = NULL, tickformat = ""
        )
      ),
      source = "U.S. Census Bureau, 1950-2020 Decennial Censuses,
      IPUMS-NHGIS, University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
    ),
    
    "Children by Age" = list(
      data_code = "chilna", agg_func = sum,
      sb_csv = 'csv/children_neigh_age_bins.csv',
      barTitle = "Children by age", barhoverformat = ",.0f",
      barCats = list(
        "0-4 years" = "under5",
        "5-17 years" = "fiveto17"
      ),
      summary_indicators = list(
        "Total children (0-17)" = list(
          summary_expression = rlang::expr(under5 + fiveto17),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickprefix = NULL, tickformat = ""
        ),
        "Total children aged 5-17" = list(
          summary_expression = rlang::expr(fiveto17),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickprefix = NULL, tickformat = ""
        ),
        "Total children aged 0-4" = list(
          summary_expression = rlang::expr(under5),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickprefix = NULL, tickformat = ""
        )
      ),
      source = "U.S. Census Bureau, 1980-2020 Decennial Censuses,
      IPUMS-NHGIS, University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
    ),
    
    "Race and Ethnicity" = list(
      data_code = "hbicnre", agg_func = sum,
      sb_csv = 'csv/hbic_neigh_race_ethn_bins.csv',
      barTitle = "Population by race/ethnicity", barhoverformat = ",.0f",
      barCats = list(
        "White" = "white",
        "Black/African American" = "black",
        "Hispanic/Latino" = "hisp",
        "Asian/Pacific Islander" = "asian",
        "American Indian" = "native",
        "Two or More" = "two_plus",
        "Other" = "other"
      ), 
      summary_indicators = list(
        "Non-white share of population" = list(
          summary_expression = rlang::expr(
            (black + hisp + asian + native + two_plus + other) /
              (white + black + hisp + asian + native + two_plus + other)
          ),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickprefix = NULL, tickformat = ".0%"
        ),
        "Share of population, Black alone" = list(
          summary_expression = rlang::expr(
            (black) /
              (white + black + hisp + asian + native + two_plus + other)
          ),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickprefix = NULL, tickformat = ".0%"
        ),
        "Share of population, Hispanic of any race" = list(
          summary_expression = rlang::expr(
            (hisp) /
              (white + black + hisp + asian + native + two_plus + other)
          ),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickprefix = NULL, tickformat = ".0%"
        ),
        "Share of population, Asian alone" = list(
          summary_expression = rlang::expr(
            (asian) /
              (white + black + hisp + asian + native + two_plus + other)
          ),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickprefix = NULL, tickformat = ".0%"
        ),
        "Share of population, two or more races" = list(
          summary_expression = rlang::expr(
            (two_plus) /
              (white + black + hisp + asian + native + two_plus + other)
          ),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickprefix = NULL, tickformat = ".0%"
        )
      ),
      source = "U.S. Census Bureau, 1950-2020 Decennial Censuses, IPUMS-NHGIS,
        University of Minnesota, www.nhgis.org; BPDA Research Division Analysis",
      note = "Note: In 1950 and 1960, the only race/ethnicity categories on the Census were White, Black, and Other. Two or more races became an option in 2000."
    ),
    
    "Children by Race and Ethnicity" = list(
      data_code = "chilnre", agg_func = sum,
      sb_csv = 'csv/children_neigh_race_bins.csv',
      barTitle = "Children by race/ethnicity", barhoverformat = ",.0f",
      barCats = list(
        "White" = "white",
        "Black/African American" = "black",
        "Hispanic/Latino" = "hisp",
        "Asian/Pacific Islander" = "aapi",
        "American Indian" = "ainh",
        "Two or More" = "twoplus",
        "Other" = "other"
      ), 
      summary_indicators = list(
        "Non-white share of children" = list(
          summary_expression = rlang::expr(
            (black + hisp + aapi + ainh + twoplus + other) /
              (white + black + hisp + aapi + ainh + twoplus + other)
          ),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickprefix = NULL, tickformat = ".0%"
        ),
        "Share of children, Black alone" = list(
          summary_expression = rlang::expr(
            (black) /
              (white + black + hisp + aapi + ainh + twoplus + other)
          ),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickprefix = NULL, tickformat = ".0%"
        ),
        "Share of children, Hispanic of any race" = list(
          summary_expression = rlang::expr(
            (hisp) /
              (white + black + hisp + aapi + ainh + twoplus + other)
          ),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickprefix = NULL, tickformat = ".0%"
        ),
        "Share of children, Asian alone" = list(
          summary_expression = rlang::expr(
            (aapi) /
              (white + black + hisp + aapi + ainh + twoplus + other)
          ),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickprefix = NULL, tickformat = ".0%"
        )
      ),
      additional_null_geoms = c("Census Tract 9811"),
      source = "U.S. Census Bureau, 1980-2020 Decennial Censuses, IPUMS-NHGIS,
        University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
    ),
    
    "Nativity" = list(
      data_code = "hbicnnat", agg_func = sum,
      sb_csv = 'csv/hbic_neigh_nativity_bins.csv', 
      barTitle = "Population by nativity", barhoverformat = ",.0f",
      barCats = list("Native-born" = "native", "Foreign-born" = "foreign"),
      summary_indicators = list(
        "Foreign-born share of population" = list(
          summary_expression = rlang::expr(foreign / (foreign + native)),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickprefix = NULL, tickformat = ".0%"
        ),
        "Total foreign-born population" = list(
          summary_expression = rlang::expr(foreign),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickprefix = NULL, tickformat = ""
        )
      ),
      source = "U.S. Census Bureau, 1950-2000 Decennial Censuses, 2006-2010 & 2016-2020 American 
      Community Survey, IPUMS-NHGIS, University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
    ),
    
    "Educational Attainment" = list(
      data_code = "hbicnedu", agg_func = sum,
      sb_csv = 'csv/hbic_neigh_edu_attain_bins.csv',
      barTitle = "Population (25+) by educational attainment", barhoverformat = ",.0f",
      barCats = list(
        "Less than high school" = "lhs",
        "High school or some equivalent" = "he",
        "Some college" = "sc",
        "Bachelor's or more" = "bm"
      ),
      summary_indicators = list(
        "Share of population (25+) with bachelor's or more" = list(
          summary_expression = rlang::expr(bm / (lhs + he + sc + bm)),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickprefix = NULL, tickformat = ".0%"
        ),
        "Share of population (25+) with less than high school" = list(
          summary_expression = rlang::expr(lhs / (lhs + he + sc + bm)),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickprefix = NULL, tickformat = ".0%"
        )
      ),
      source = "U.S. Census Bureau, 1950-2000 Decennial Censuses, 2006-2010 & 2016-2020 American 
      Community Survey, IPUMS-NHGIS, University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
    ),
    
    "Labor Force" = list(
      data_code = "hbicnlf",agg_func = sum,
      sb_csv = 'csv/hbic_neigh_labor_force_bins.csv', 
      barTitle = "Population (16+) by labor force status and sex", barhoverformat = ",.0f",
      barCats = list(
        "Male in labor force" = "ilf_m"
        , "Male not in labor force" = "nilf_m"
        , "Female in labor force" = "ilf_f"
        , "Female not in labor force" = "nilf_f"
      ), 
      summary_indicators = list(
        "Female labor force participation rate (16+)" = list(
          summary_expression = rlang::expr(ilf_f / (ilf_f + nilf_f)),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickprefix = NULL, tickformat = ".0%"
        ),
        "Male labor force participation rate (16+)" = list(
          summary_expression = rlang::expr(ilf_m / (ilf_m + nilf_m)),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickprefix = NULL, tickformat = ".0%"
        )
      ),
      source = "U.S. Census Bureau, 1950-2000 Decennial Censuses, 2006-2010 & 2016-2020 American 
      Community Survey, IPUMS-NHGIS, University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
    ),
    
    "Housing Units" = list(
      data_code = "hbicnhou", agg_func = sum,
      sb_csv = 'csv/hbic_neigh_housing_bins.csv',
      barTitle = "Housing units by occupancy", barhoverformat = ",.0f",
      barCats = list(
        "Occupied" = "occ",
        "Vacant" = "vac"
      ), 
      summary_indicators = list(
        "Total housing units" = list(
          summary_expression = rlang::expr(vac + occ),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickprefix = NULL, tickformat = ""
        )
      ),
      source = "U.S. Census Bureau, 1950-2020 Decennial Censuses, IPUMS-NHGIS, University of 
      Minnesota, www.nhgis.org; Mayor's Office of Housing; BPDA Research Division Analysis"
    ),
    
    "Housing Occupancy" = list(
      data_code = "hbicnhouvac", agg_func = sum,
      sb_csv = 'csv/hbic_neigh_housing_vacancy_bins.csv',
      barTitle = "Housing units by occupancy", barhoverformat = ",.0f",
      barCats = list(
        "Occupied" = "occ",
        "Vacant" = "vac"
      ),
      summary_indicators = list(
        "Housing vacancy rate" = list(
          summary_expression = rlang::expr(vac / (vac + occ)),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickprefix = NULL, tickformat = ".0%"
        ),
        "Total vacant units" = list(
          summary_expression = rlang::expr(vac),
          citywide_comparison = FALSE,
          hoverformat = ",.0f", tickprefix = NULL, tickformat = ""
        )
      ),
      source = "U.S. Census Bureau, 1950-2020 Decennial Censuses, IPUMS-NHGIS, University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
    ),
    
    "Housing Tenure" = list(
      data_code = "hbicnhouten", agg_func = sum,
      sb_csv = 'csv/hbic_neigh_housing_tenure_bins.csv',
      barTitle = "Occupied housing units by tenure", barhoverformat = ",.0f",
      barCats = list(
        "Owner-occupied" = "owner",
        "Renter-occupied" = "renter"
      ), 
      summary_indicators = list(
        "Owner occupancy rate" = list(
          summary_expression = rlang::expr(owner / (owner + renter)),
          citywide_comparison = TRUE,
          hoverformat = ".0%", tickprefix = NULL, tickformat = ".0%"
        )
      ),
      source = "U.S. Census Bureau, 1950-2010 Decennial Censuses, 2016-2020 American 
      Community Survey, IPUMS-NHGIS, University of Minnesota, www.nhgis.org; BPDA Research Division Analysis"
      )
    )
  )
)

# Functions ##########

pivot_long_and_rename_categories <- function(df, bin_col_names) {
  out <- df %>% 
    pivot_longer(cols = unlist(unname(bin_col_names)), 
                 names_to='CATEGORY', 
                 values_to = 'VALUE')
  out$CATEGORY <- plyr::mapvalues(out$CATEGORY, 
                                  from = unname(bin_col_names), 
                                  to = names(bin_col_names))
  out
}

prep_data <- function(topic) {
  subcity_bins <- read.csv(topic$sb_csv) %>%
    pivot_long_and_rename_categories(bin_col_names = topic$barCats) %>%
    mutate(GEOID = as.character(GEOID))
  
  dfs = list("sb_df" = subcity_bins)
  if ("cb_csv" %in% names(topic)) {
    dfs$cb_df <- read.csv(topic$cb_csv) %>% pivot_long_and_rename_categories(bin_col_names = topic$barCats)
  }
  indicators = list()
  for (ind_name in names(topic$summary_indicators)) {
    ind_dfs = list()
    for (x in c("ss", "cs")) {
      if (paste0(x, "_csv") %in% names(topic$summary_indicators[[ind_name]])) {
        ind_dfs[[paste0(x, "_df")]] = read.csv(topic$summary_indicators[[ind_name]][[paste0(x, "_csv")]])
      }
    }
    if (length(ind_dfs) > 0) {
      indicators[[ind_name]] = ind_dfs
    }
  }
  if (length(indicators) > 0) {
    dfs$indicators = indicators
  }
  #appdata = dfs
  dfs %>% saveRDS(file=sprintf('../data/%s.rds', topic$data_code))
}

# Prep data #######

# # # You can either prep data for individual topics...
# prep_data(APP_CONFIG[['census tracts']]$topics[['Children by Age']])
prep_data(APP_CONFIG[['census tracts']]$topics[['Children by Race and Ethnicity']])
# prep_data(APP_CONFIG[['neighborhoods']]$topics[['Children by Age']])
prep_data(APP_CONFIG[['neighborhoods']]$topics[['Children by Race and Ethnicity']])

# # ...or prep data for all topics
# for (geo_type in APP_CONFIG) {
#   for (topic in geo_type$topics) {
#     prep_data(topic)
#   }
# }

# always save config after making changes to it
APP_CONFIG %>% saveRDS(file='../config/APP_CONFIG.rds')

# ACS Median Household Income (not currently used) ######################
# library(tidycensus)
# options(tigris_use_cache = TRUE)
# census_api_key(read.csv('../extra/census_api_key.csv')$key)
tract2010_geoms <- read_sf('../geoms/boston_tracts_2010.geojson') %>% mutate(GEOID = as.character(GEOID10))
# 
# v21 <- load_variables(2021, "acs5/subject", cache = TRUE)
# 
# my_states = c("MA")
# mhi <- c(
#   median_household_income = "S1901_C01_012"
# )
# 
inc_bckts <- c(
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
)
# 
# years <- c(2010, 2012, 2014, 2016, 2018)
# 
# get_acs_tract_by_yr <- function(yr, vars, output_type, include_geom) {
#   ct <- get_acs(
#     geography = "tract",
#     variables = vars,
#     state = my_states,
#     county = "025",
#     year = yr,
#     survey="acs5",
#     output = output_type,
#     geometry = include_geom,
#     cache_table = TRUE
#   )
#   ct$YEAR <- yr
#   ct
# }
# 
# get_acs_place_by_year <- function(yr) {
#   data <- get_acs(
#     geography = "place",
#     variables = mhi,
#     state = my_states,
#     year = yr,
#     survey="acs5",
#     output = 'wide',
#     geometry = FALSE,
#     cache_table = TRUE
#   ) %>% subset(GEOID == '2507000')
#   data$YEAR <- yr
#   data
# }
# 
# mhi_bos <- lapply(years, get_acs_place_by_year) %>% bind_rows()
# city_summary <- mhi_bos %>% select(-c("GEOID", "NAME")) %>%
#   rename(MOE = median_household_incomeM, SUMMARY_VALUE = median_household_incomeE)
# city_summary %>% write.csv(file='csv/acshhi_cs.csv', row.names=FALSE)
# 
# med_hh_income <- lapply(years, get_acs_tract_by_yr, vars=mhi, output_type="wide", include_geom=FALSE) %>% bind_rows()
# subcity_summary <- med_hh_income %>%
#   mutate(SUMMARY_VALUE = ifelse(startsWith(NAME, "Census Tract 98"), NaN,median_household_incomeE)) %>%
#   mutate(MOE = ifelse(startsWith(NAME, "Census Tract 98"), NaN,median_household_incomeM)) %>%
#   select(-c("median_household_incomeM", "median_household_incomeE"))
# subcity_summary %>% write.csv(file='csv/acshhi_ss.csv', row.names=FALSE)
# 
# total_hh <- lapply(years, get_acs_tract_by_yr, vars=c("S1901_C01_001"), output_type="wide", include_geom=FALSE) %>% bind_rows()
# df <- lapply(years, get_acs_tract_by_yr, vars=unname(inc_bckts), output_type = "tidy", include_geom=FALSE) %>% bind_rows() 
# subcity_bins <- left_join(df, 
#                total_hh %>% as_tibble() %>% select(GEOID, S1901_C01_001E, YEAR), 
#                by=c("GEOID", "YEAR")) %>%
#   mutate(across(c(estimate, moe), ~ . * S1901_C01_001E / 100)) %>%
#   mutate(across(c(estimate, moe), round, 0)) %>% 
#   select(-c(S1901_C01_001E, moe)) %>%
#   mutate_at(.vars = c("estimate"), # names(df)[!names(df) %in% c("GEOID", "NAME", "YEAR")]
#             list(~ifelse(startsWith(NAME, "Census Tract 98"), NaN, .))) %>%
#   pivot_wider(names_from = variable, values_from = estimate)
# subcity_bins %>% write.csv(file='csv/acshhi_sb.csv', row.names=FALSE)
# 
prepare_data(
  var_code = 'acshhi'
  , sb_csv = 'csv/acshhi_sb.csv'
  , bin_col_names = inc_bckts
  , agg_func = sum
  , geoms = tract2010_geoms
  , ss_csv = 'csv/acshhi_ss.csv'
  , cs_csv = 'csv/acshhi_cs.csv'
)

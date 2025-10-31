# Data Acquisition#

if(!dir.exists(file.path("data", "mp02"))){
  dir.create(file.path("data", "mp02"), showWarnings=FALSE, recursive=TRUE)
}

library <- function(pkg){
  ## Mask base::library() to automatically install packages if needed
  ## Masking is important here so downlit picks up packages and links
  ## to documentation
  pkg <- as.character(substitute(pkg))
  options(repos = c(CRAN = "https://cloud.r-project.org"))
  if(!require(pkg, character.only=TRUE, quietly=TRUE)) install.packages(pkg)
  stopifnot(require(pkg, character.only=TRUE, quietly=TRUE))
}

library(tidyverse)
library(glue)
library(readxl)
library(tidycensus)

get_acs_all_years <- function(variable, geography="cbsa",
                              start_year=2009, end_year=2023){
  fname <- glue("{variable}_{geography}_{start_year}_{end_year}.csv")
  fname <- file.path("data", "mp02", fname)
  
  if(!file.exists(fname)){
    YEARS <- seq(start_year, end_year)
    YEARS <- YEARS[YEARS != 2020] # Drop 2020 - No survey (covid)
    
    ALL_DATA <- map(YEARS, function(yy){
      tidycensus::get_acs(geography, variable, year=yy, survey="acs1") |>
        mutate(year=yy) |>
        select(-moe, -variable) |>
        rename(!!variable := estimate)
    }) |> bind_rows()
    
    write_csv(ALL_DATA, fname)
  }
  
  read_csv(fname, show_col_types=FALSE)
}

# Household income (12 month)
INCOME <- get_acs_all_years("B19013_001") |>
  rename(household_income = B19013_001)

# Monthly rent
RENT <- get_acs_all_years("B25064_001") |>
  rename(monthly_rent = B25064_001)

# Total population
POPULATION <- get_acs_all_years("B01003_001") |>
  rename(population = B01003_001)

# Total number of households
HOUSEHOLDS <- get_acs_all_years("B11001_001") |>
  rename(households = B11001_001)

# Number of new housing units built each year#
get_building_permits <- function(start_year = 2009, end_year = 2023){
  fname <- glue("housing_units_{start_year}_{end_year}.csv")
  fname <- file.path("data", "mp02", fname)
  
  if(!file.exists(fname)){
    HISTORICAL_YEARS <- seq(start_year, 2018)
    
    HISTORICAL_DATA <- map(HISTORICAL_YEARS, function(yy){
      historical_url <- glue("https://www.census.gov/construction/bps/txt/tb3u{yy}.txt")
      
      LINES <- readLines(historical_url)[-c(1:11)]
      
      CBSA_LINES <- str_detect(LINES, "^[[:digit:]]")
      CBSA <- as.integer(str_sub(LINES[CBSA_LINES], 5, 10))
      
      PERMIT_LINES <- str_detect(str_sub(LINES, 48, 53), "[[:digit:]]")
      PERMITS <- as.integer(str_sub(LINES[PERMIT_LINES], 48, 53))
      
      data_frame(CBSA = CBSA,
                 new_housing_units_permitted = PERMITS, 
                 year = yy)
    }) |> bind_rows()
    
    CURRENT_YEARS <- seq(2019, end_year)
    
    CURRENT_DATA <- map(CURRENT_YEARS, function(yy){
      current_url <- glue("https://www.census.gov/construction/bps/xls/msaannual_{yy}99.xls")
      
      temp <- tempfile()
      
      download.file(current_url, destfile = temp, mode="wb")
      
      fallback <- function(.f1, .f2){
        function(...){
          tryCatch(.f1(...), 
                   error=function(e) .f2(...))
        }
      }
      
      reader <- fallback(read_xlsx, read_xls)
      
      reader(temp, skip=5) |>
        na.omit() |>
        select(CBSA, Total) |>
        mutate(year = yy) |>
        rename(new_housing_units_permitted = Total)
    }) |> bind_rows()
    
    ALL_DATA <- rbind(HISTORICAL_DATA, CURRENT_DATA)
    
    write_csv(ALL_DATA, fname)
    
  }
  
  read_csv(fname, show_col_types=FALSE)
}

PERMITS <- get_building_permits()

#Core-Based Statistical Areas Data Acquisition#
library(httr2)
library(rvest)
get_bls_industry_codes <- function(){
  fname <- fname <- file.path("data", "mp02", "bls_industry_codes.csv")
  
  if(!file.exists(fname)){
    
    resp <- request("https://www.bls.gov") |> 
      req_url_path("cew", "classifications", "industry", "industry-titles.htm") |>
      req_headers(`User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:143.0) Gecko/20100101 Firefox/143.0") |> 
      req_error(is_error = \(resp) FALSE) |>
      req_perform()
    
    resp_check_status(resp)
    
    naics_table <- resp_body_html(resp) |>
      html_element("#naics_titles") |> 
      html_table() |>
      mutate(title = str_trim(str_remove(str_remove(`Industry Title`, Code), "NAICS"))) |>
      select(-`Industry Title`) |>
      mutate(depth = if_else(nchar(Code) <= 5, nchar(Code) - 1, NA)) |>
      filter(!is.na(depth))
    
    naics_table <- naics_table |> 
      filter(depth == 4) |> 
      rename(level4_title=title) |> 
      mutate(level1_code = str_sub(Code, end=2), 
             level2_code = str_sub(Code, end=3), 
             level3_code = str_sub(Code, end=4)) |>
      left_join(naics_table, join_by(level1_code == Code)) |>
      rename(level1_title=title) |>
      left_join(naics_table, join_by(level2_code == Code)) |>
      rename(level2_title=title) |>
      left_join(naics_table, join_by(level3_code == Code)) |>
      rename(level3_title=title) |>
      select(-starts_with("depth")) |>
      rename(level4_code = Code) |>
      select(level1_title, level2_title, level3_title, level4_title, 
             level1_code,  level2_code,  level3_code,  level4_code)
    
    write_csv(naics_table, fname)
  }
  
  read_csv(fname, show_col_types=FALSE)
  
}

INDUSTRY_CODES <- get_bls_industry_codes()

#BLS BLS Quarterly Census of Employment and Wages Data Acqusition#
library(httr2)
library(rvest)
get_bls_qcew_annual_averages <- function(start_year=2009, end_year=2023){
  fname <- glue("bls_qcew_{start_year}_{end_year}.csv.gz")
  fname <- file.path("data", "mp02", fname)
  
  YEARS <- seq(start_year, end_year)
  YEARS <- YEARS[YEARS != 2020] # Drop Covid year to match ACS
  
  if(!file.exists(fname)){
    ALL_DATA <- map(YEARS, .progress=TRUE, possibly(function(yy){
      fname_inner <- file.path("data", "mp02", glue("{yy}_qcew_annual_singlefile.zip"))
      
      if(!file.exists(fname_inner)){
        request("https://www.bls.gov") |> 
          req_url_path("cew", "data", "files", yy, "csv",
                       glue("{yy}_annual_singlefile.zip")) |>
          req_headers(`User-Agent` = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:143.0) Gecko/20100101 Firefox/143.0") |> 
          req_retry(max_tries=5) |>
          req_perform(fname_inner)
      }
      
      if(file.info(fname_inner)$size < 755e5){
        warning(sQuote(fname_inner), "appears corrupted. Please delete and retry this step.")
      }
      
      read_csv(fname_inner, 
               show_col_types=FALSE) |> 
        mutate(YEAR = yy) |>
        select(area_fips, 
               industry_code, 
               annual_avg_emplvl, 
               total_annual_wages, 
               YEAR) |>
        filter(nchar(industry_code) <= 5, 
               str_starts(area_fips, "C")) |>
        filter(str_detect(industry_code, "-", negate=TRUE)) |>
        mutate(FIPS = area_fips, 
               INDUSTRY = as.integer(industry_code), 
               EMPLOYMENT = as.integer(annual_avg_emplvl), 
               TOTAL_WAGES = total_annual_wages) |>
        select(-area_fips, 
               -industry_code, 
               -annual_avg_emplvl, 
               -total_annual_wages) |>
        # 10 is a special value: "all industries" , so omit
        filter(INDUSTRY != 10) |> 
        mutate(AVG_WAGE = TOTAL_WAGES / EMPLOYMENT)
    })) |> bind_rows()
    
    write_csv(ALL_DATA, fname)
  }
  
  ALL_DATA <- read_csv(fname, show_col_types=FALSE)
  
  ALL_DATA_YEARS <- unique(ALL_DATA$YEAR)
  
  YEARS_DIFF <- setdiff(YEARS, ALL_DATA_YEARS)
  
  if(length(YEARS_DIFF) > 0){
    stop("Download failed for the following years: ", YEARS_DIFF, 
         ". Please delete intermediate files and try again.")
  }
  
  ALL_DATA
}

WAGES <- get_bls_qcew_annual_averages()

#Task 2#

#Question 1#
library(dplyr)
library(readr)

PERMITS <- PERMITS %>%
  mutate(CBSA = as.character(CBSA))

INCOME <- INCOME %>%
  mutate(GEOID = as.character(GEOID))

INCOME_unique <- INCOME %>%
  group_by(GEOID) %>%
  slice(1) %>%
  ungroup() %>%
  select(GEOID, NAME)

# Identify CBSA with largest number of new housing units 2010–2019
top_cbsa <- PERMITS %>%
  filter(year >= 2010 & year <= 2019) %>%
  group_by(CBSA) %>%
  summarize(total_units = sum(new_housing_units_permitted, na.rm = TRUE)) %>%
  arrange(desc(total_units)) %>%
  slice(1)

# Join with unique names
top_cbsa_named <- top_cbsa %>%
  left_join(INCOME_unique, by = c("CBSA" = "GEOID"))

cat(
  "The CBSA that permitted the largest number of new housing units from 2010 to 2019 is",
  top_cbsa_named$NAME, "with a total of",
  format(top_cbsa_named$total_units, big.mark = ","), "units.\n")


#Question 2#
library(dplyr)

PERMITS <- PERMITS %>% mutate(CBSA = as.character(CBSA))

# Filter for Albuquerque CBSA
albuquerque_permits <- PERMITS %>%
  filter(CBSA == "10740")

# Find year with most new housing units
albuquerque_max <- albuquerque_permits %>%
  arrange(desc(new_housing_units_permitted)) %>%
  slice(1) %>%
  select(year, new_housing_units_permitted)

# Print in sentence format
cat("Albuquerque, NM (CBSA 10740) permitted the most new housing units in",
  albuquerque_max$year, "with a total of",
  format(albuquerque_max$new_housing_units_permitted, big.mark = ","), "units.\n")


#Question 3#
library(dplyr)
library(stringr)
library(tidycensus)

INCOME <- get_acs(
  geography = "cbsa",
  variables = "B19013_001",
  year = 2015,
  survey = "acs1") %>%
  rename(household_income = estimate)

HOUSEHOLDS <- get_acs(
  geography = "cbsa",
  variables = "B11001_001",
  year = 2015,
  survey = "acs1") %>%
  rename(households = estimate)

POPULATION_clean1 <- get_acs(
  geography = "cbsa",
  variables = "B01003_001",
  year = 2015,
  survey = "acs1") %>%
  rename(population = estimate)

CBSA_2015 <- INCOME %>%
  left_join(HOUSEHOLDS, by = "GEOID") %>%
  left_join(POPULATION_clean1, by = "GEOID")

CBSA_2015 <- transform(
  CBSA_2015,
  total_income = household_income * households,
  state = str_replace(str_extract(NAME, ", (.{2})"), ", ", ""))

STATE_INCOME <- CBSA_2015 %>%
  group_by(state) %>%
  summarize(
    total_income_state = sum(total_income, na.rm = TRUE),
    total_population_state = sum(population, na.rm = TRUE),
    avg_individual_income = total_income_state / total_population_state,
    .groups = "drop")

state_df <- data.frame(
  abb  = c(state.abb, "DC", "PR"),
  name = c(state.name, "District of Columbia", "Puerto Rico"))

STATE_INCOME <- STATE_INCOME %>%
  left_join(state_df, by = c("state" = "abb"))

top_state <- STATE_INCOME %>%
  arrange(desc(avg_individual_income)) %>%
  slice(1)

cat("The state with the highest average individual income in 2015 is",
  top_state$name, "with an average income of $",
  format(round(top_state$avg_individual_income, 2), big.mark = ","), ".\n")


#Question 4#
library(dplyr)
library(stringr)
library(DT)

data_scientists <- WAGES %>%
  filter(INDUSTRY == 5182) %>%
  group_by(FIPS, YEAR) %>%
  summarize(total_employment = sum(EMPLOYMENT, na.rm = TRUE), .groups = "drop")

top_cbsa_each_year <- data_scientists %>%
  group_by(YEAR) %>%
  slice_max(order_by = total_employment, n = 1, with_ties = FALSE) %>%
  ungroup()

cbsa_names <- INCOME %>%
  select(GEOID, NAME)
cbsa_names$std_cbsa <- paste0("C", cbsa_names$GEOID)

top_cbsa_each_year$std_cbsa <- paste0(top_cbsa_each_year$FIPS, "0")

top_cbsa_each_year <- left_join(top_cbsa_each_year, cbsa_names, by = "std_cbsa")

datatable(
  top_cbsa_each_year %>%
    arrange(YEAR) %>%
    select(YEAR, NAME, total_employment),
  caption = "Top CBSA for Data Scientists by Employment (5182)",
  options = list(pageLength = 10))



#Question 5#
library(dplyr)
library(stringr)

nyc_cbsa <- WAGES %>%
  filter(str_detect(FIPS, "C1018|C1038|C1042|C1050|C1058")) %>%
  pull(FIPS) %>%
  unique()

nyc_finance <- WAGES %>%
  filter(FIPS %in% nyc_cbsa) %>%
  group_by(YEAR) %>%
  summarise(
    total_wages = sum(TOTAL_WAGES, na.rm = TRUE),
    fin_wages = sum(TOTAL_WAGES[str_starts(as.character(INDUSTRY), "52")], na.rm = TRUE),
    .groups = "drop")

nyc_finance$share_fin <- nyc_finance$fin_wages / nyc_finance$total_wages

peak <- nyc_finance %>%
  filter(share_fin == max(share_fin, na.rm = TRUE))

cat("In", peak$YEAR, ",",
  round(peak$share_fin * 100, 2),"% of NYC total wages were in Finance & Insurance (NAICS 52), the peak in the period.\n")

#Task 3#

#Question 1#
library(dplyr)
library(ggplot2)

INCOME <- read_csv("data/mp02/B19013_001_cbsa_2009_2023.csv", show_col_types = FALSE)

INCOME <- INCOME %>%
  rename(household_income = B19013_001)

INCOME <- INCOME %>% mutate(year = as.numeric(.data$year))
RENT   <- RENT   %>% mutate(year = as.numeric(.data$year))

income_rent <- inner_join(INCOME, RENT, by = c("GEOID", "year"))

data_2009 <- income_rent %>% filter(.data$year == 2009)

ggplot(data_2009, aes(x = household_income, y = monthly_rent)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(
    title = "Monthly Rent vs. Average Household Income per CBSA (2009)",
    x = "Average Household Income (USD)",
    y = "Average Monthly Rent (USD)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12))


#Question 2#

library(dplyr)
library(tidycensus)
library(ggplot2)
library(scales)
library(purrr)

years <- 2013:2019

get_employment_data <- function(y) {
  total_emp <- get_acs(
    geography = "cbsa",
    variables = "B23025_003",
    year = y,
    survey = "acs1"
  ) %>% rename(total_employment = estimate)
  
  healthcare_emp <- get_acs(
    geography = "cbsa",
    variables = "C24010_005",
    year = y,
    survey = "acs1"
  ) %>% rename(healthcare_employment = estimate)
  
  total_emp %>%
    left_join(healthcare_emp, by = "GEOID") %>%
    mutate(NAME = total_emp$NAME, YEAR = y) %>%
    select(GEOID, NAME, YEAR, total_employment, healthcare_employment)
}

employment_data <- map_dfr(years, get_employment_data)

selected_cbsa <- c("35620", "31080", "16980", "14460") # NYC + 3 others
employment_data_4 <- employment_data %>%
  filter(GEOID %in% selected_cbsa)

ggplot(employment_data_4, aes(
  x = total_employment,
  y = healthcare_employment
)) +
  geom_point(aes(color = YEAR), size = 3) +
  geom_line(aes(group = 1), color = "gray", alpha = 0.7) +
  facet_wrap(~ NAME, scales = "free") +
  scale_x_log10(labels = comma) + 
  scale_y_continuous(labels = comma) +
  scale_color_viridis_c(option = "plasma", name = "Year") +
  labs(
    title = "Health Care & Social Services Employment vs Total Employment",
    subtitle = "Selected CBSAs including NYC (2013–2019)",
    x = "Total Employment (log scale)",
    y = "Health Care & Social Services Employment (NAICS 62)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11),
    axis.title = element_text(size = 12))



#Question 3#

library(dplyr)
library(ggplot2)
library(readr)

POPULATION <- read_csv("data/mp02/B01003_001_cbsa_2009_2023.csv", show_col_types = FALSE) %>%
  rename(population = B01003_001)

HOUSEHOLDS <- read_csv("data/mp02/B11001_001_cbsa_2009_2023.csv", show_col_types = FALSE) %>%
  rename(households = B11001_001)

CBSA_HH <- POPULATION %>%
  select(GEOID, NAME, year, population) %>%
  left_join(
    HOUSEHOLDS %>% select(GEOID, year, households),
    by = c("GEOID", "year")
  ) %>%
  mutate(avg_household_size = population / households)

top_cities <- CBSA_HH %>%
  filter(year == max(year)) %>%
  arrange(desc(population)) %>%
  slice_head(n = 10) %>%
  pull(NAME)

CBSA_HH_filtered <- CBSA_HH %>%
  filter(NAME %in% top_cities)

ggplot(CBSA_HH_filtered, aes(x = year, y = avg_household_size, color = NAME)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(min(CBSA_HH_filtered$year), max(CBSA_HH_filtered$year), by = 1)) +
  labs(
    title = "Average Household Size Over Time",
    x = "Year",
    y = "Average Household Size",
    color = "City") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10))

#Task 4#

library(tidyverse)
library(glue)
library(DT)

RENT_BURDEN <- INCOME %>%
  inner_join(RENT, by = c("GEOID", "NAME", "year"))

RENT_BURDEN <- RENT_BURDEN %>%
  mutate(rent_to_income_percentage = ((monthly_rent * 12) / household_income) * 100)

NATIONAL_AVG <- RENT_BURDEN %>%
  group_by(year) %>%
  summarize(national_avg_rti = mean(rent_to_income_percentage, na.rm = TRUE))

baseline_rti <- NATIONAL_AVG %>%
  filter(year == 2009) %>%
  pull(national_avg_rti)

RENT_BURDEN <- RENT_BURDEN %>%
  mutate(rent_burden_index = (rent_to_income_percentage / baseline_rti) * 100)

nyc_name <- RENT_BURDEN %>%
  filter(str_detect(NAME, "New York")) %>%
  distinct(NAME) %>%
  pull(NAME) %>%
  first()

print(glue("Using metro name: {nyc_name}"))

nyc_rent <- RENT_BURDEN %>%
  filter(NAME == nyc_name) %>%
  select(year, household_income, monthly_rent, rent_to_income_percentage, rent_burden_index) %>%
  mutate(rent_to_income_percentage = round(rent_to_income_percentage, 2),
         rent_burden_index = round(rent_burden_index, 2))

datatable(
  nyc_rent,
  caption = glue("Rent Burden Over Time — {nyc_name}"),
  options = list(pageLength = 10))

#TABLE 2: Top 10 and Bottom CBSAs by Rent Burden Index
latest_year <- max(RENT_BURDEN$year, na.rm = TRUE)

top_bottom <- RENT_BURDEN %>%
  filter(year == latest_year) %>%
  group_by(NAME) %>%
  summarize(avg_index = mean(rent_burden_index, na.rm = TRUE)) %>%
  arrange(desc(avg_index)) %>%
  mutate(rank = row_number())

datatable(
  bind_rows(
    head(top_bottom, 10),
    tail(top_bottom, 10)
  ),
  caption = glue("Top & Bottom 10 Metro Areas by Rent Burden Index ({latest_year})"),
  options = list(pageLength = 20))

#Task 5#
library(dplyr)
library(RcppRoll)
library(DT)

POPULATION_clean1 <- POPULATION %>%
  rename(CBSA = GEOID) %>%
  mutate(CBSA = as.character(CBSA))

PERMITS_clean1 <- PERMITS %>%
  rename(CBSA = CBSA,
         permits = new_housing_units_permitted) %>%
  mutate(CBSA = as.character(CBSA)) 

housing_data <- POPULATION_clean1 %>%
  left_join(PERMITS_clean1, by = c("CBSA", "year"))

housing_data <- housing_data %>%
  group_by(CBSA) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    pop_5yrs_ago = lag(population, 5),
    pop_growth_5yr = population - pop_5yrs_ago
  ) %>%
  filter(year >= 2014) %>%  
  ungroup()

housing_data <- housing_data %>%
  mutate(
    housing_growth_instant = (permits / population) * 1000,
    housing_growth_rate = if_else(pop_growth_5yr > 0, permits / pop_growth_5yr, 0)
  )

housing_data <- housing_data %>%
  mutate(
    instant_z = scale(housing_growth_instant)[,1],
    rate_z = scale(housing_growth_rate)[,1],
    composite_score = instant_z + rate_z
  )

housing_data <- housing_data %>%
  group_by(CBSA) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    instant_roll5 = roll_mean(housing_growth_instant, 5, fill = NA, align = "right"),
    rate_roll5 = roll_mean(housing_growth_rate, 5, fill = NA, align = "right")
  ) %>%
  ungroup()

top_bottom_instant <- housing_data %>%
  filter(!is.na(instant_z)) %>%
  arrange(desc(instant_z)) %>%
  slice(c(1:10, (n()-9):n())) %>%
  select(CBSA, NAME, year, population, permits, housing_growth_instant, instant_z)

top_bottom_rate <- housing_data %>%
  filter(!is.na(rate_z)) %>%
  arrange(desc(rate_z)) %>%
  slice(c(1:10, (n()-9):n())) %>%
  select(CBSA, NAME, year, population, permits, housing_growth_rate, rate_z)

top_bottom_composite <- housing_data %>%
  filter(!is.na(composite_score)) %>%
  arrange(desc(composite_score)) %>%
  slice(c(1:10, (n()-9):n())) %>%
  select(CBSA, NAME, year, population, permits, composite_score)

datatable(top_bottom_instant, 
          caption = "Top and Bottom CBSAs — Instant Housing Growth (Z-Score)",
          options = list(pageLength = 10))

datatable(top_bottom_rate, 
          caption = "Top and Bottom CBSAs — Housing Growth Rate (Z-Score)",
          options = list(pageLength = 10))

datatable(top_bottom_composite, 
          caption = "Top and Bottom CBSAs — Composite Growth Score",
          options = list(pageLength = 10))

#Task 6 #
library(dplyr)
library(ggplot2)
library(scales)
library(RcppRoll)

POPULATION_clean1 <- POPULATION %>%
  rename(CBSA = GEOID) %>%
  select(CBSA, NAME, year, population) %>%
  mutate(CBSA = as.character(CBSA))

RENT_clean <- RENT %>%
  rename(CBSA = GEOID) %>%
  select(CBSA, NAME, year, monthly_rent) %>%
  mutate(CBSA = as.character(CBSA))

PERMITS_clean <- PERMITS %>%
  select(CBSA, new_housing_units_permitted, year) %>%
  mutate(CBSA = as.character(CBSA))

housing_data <- POPULATION_clean1 %>%
  left_join(PERMITS_clean, by = c("CBSA", "year")) %>%
  group_by(CBSA) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    pop_5yrs_ago = lag(population, 5),
    pop_growth_5yr = population - pop_5yrs_ago,
    housing_growth_instant = (new_housing_units_permitted / population) * 1000,
    housing_growth_rate = if_else(pop_growth_5yr > 0,
                                  new_housing_units_permitted / pop_growth_5yr, 0)
  ) %>%
  ungroup()

exclude_cols <- c("GEOID", "NAME", "year", "moe", "variable")
income_candidates <- setdiff(names(INCOME), exclude_cols)
num_candidates <- income_candidates[sapply(INCOME[income_candidates], is.numeric)]

income_col <- num_candidates[1]
message("Detected income column: ", income_col)

rent_burden <- RENT_clean %>%
  left_join(
    INCOME %>%
      mutate(GEOID = as.character(GEOID)) %>%
      select(GEOID, year, !!sym(income_col)) %>%
      rename(household_income = !!sym(income_col)),
    by = c("CBSA" = "GEOID", "year")) %>%
  mutate(
    rent_burden_ratio = (monthly_rent * 12) / household_income) %>%
  select(CBSA, NAME, year, rent_burden_ratio)


merged_data <- housing_data %>%
  mutate(CBSA = as.character(CBSA)) %>%
  left_join(
    rent_burden %>% mutate(CBSA = as.character(CBSA)),
    by = c("CBSA", "NAME", "year"))

glimpse(merged_data)
summary(merged_data$rent_burden_ratio)


library(dplyr)

cbsa_summary <- merged_data %>%
  group_by(CBSA, NAME) %>%
  summarize(
    rent_burden_early = mean(rent_burden_ratio[year %in% 2009:2012], na.rm = TRUE),
    rent_burden_recent = mean(rent_burden_ratio[year %in% 2019:2023], na.rm = TRUE),
    rent_burden_change = rent_burden_recent - rent_burden_early,
    pop_early = mean(population[year %in% 2009:2012], na.rm = TRUE),
    pop_recent = mean(population[year %in% 2019:2023], na.rm = TRUE),
    pop_growth_rate = (pop_recent - pop_early) / pop_early,
    housing_growth_avg = mean(housing_growth_instant, na.rm = TRUE)
  ) %>%
  ungroup()

#Graph 1#
library(ggplot2)

ggplot(cbsa_summary, aes(x = housing_growth_avg, y = -rent_burden_change)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_vline(xintercept = mean(cbsa_summary$housing_growth_avg, na.rm = TRUE), linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "YIMBY Scorecard: Housing Growth vs Rent Burden Change (2009–2023)",
    subtitle = "Top-right quadrant: High housing growth + falling rent burden",
    x = "Average Housing Growth (permits per 1,000 residents)",
    y = "Decrease in Rent Burden (negative change = improvement)"
  ) +
  theme_minimal()

#Graph 2#
ggplot(cbsa_summary, aes(x = pop_growth_rate * 100, y = -rent_burden_change)) +
  geom_point(aes(size = housing_growth_avg, color = housing_growth_avg), alpha = 0.6) +
  scale_color_viridis_c(option = "plasma") +
  labs(
    title = "Population Growth vs Rent Burden Change (2009–2023)",
    subtitle = "Bubble size and color represent housing growth intensity",
    x = "Population Growth Rate (%)",
    y = "Decrease in Rent Burden (negative change = improvement)",
    color = "Housing Growth\n(per 1,000 residents)"
  ) +
  theme_minimal()

#Task 7#


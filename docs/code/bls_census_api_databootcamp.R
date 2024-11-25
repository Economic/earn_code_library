# code to download some data from the BLS API
# get some data that looks like: https://www.epi.org/indicators/unemployment/


# series IDS to use: overall employment changes from CPS
# CES0000000001 : payroll emp

# documentation for blsR: https://rdrr.io/cran/blsR/man/blsR.html
# install.packages("blsR")

#documentation for tidycensus: https://walker-data.com/tidycensus/articles/basic-usage.html
# install.packages("tidycensus")

library(tidyverse)
library(tidycensus)
library(blsR)

#set bls API key (optional but encouraged)
bls_set_key("YOUR BLS API KEY HERE")

# get_series_table to pull one data series
get_series_table(series_id = "CES0000000001", start_year = 2020, end_year = 2023)

#for more information about series ids, go to https://download.bls.gov/pub/time.series/overview.txt
get_all_surveys()

#store in object
payroll_emp <- get_series_table(series_id = "CES0000000001", start_year = 2020, end_year = 2023) %>% 
  tidy_periods() %>% 
  mutate(total_emp_mom = c(NA, diff(value)),
         date = as.Date(paste(year, month, "01", sep = "-")))

#plot data
ggplot(data = payroll_emp, aes(x = date, y = total_emp_mom)) +
  geom_bar(stat = "identity", fill = "blue",  alpha = 0.7) +
  labs(title = "Monthly change in payroll employment, January 2020–August 2023",
       y = "Seasonally adjusted, in thousands")


#### Multiple series IDs from BLS
codes <- c("LNS14000003", #White
           "LNS14000006", #Black
           "LNS14000009", #Hispanic or Latino
           "LNS14032183") #AAPI

unemp_race <- get_n_series_table(series_ids = codes, start_year = 1995, end_year = 2023, tidy = TRUE) %>% 
  mutate(date = as.Date(paste(year, month, "01", sep = "-")),
         "White" = LNS14000003, 
         "Black" = LNS14000006, 
         "Hispanic or Latino" = LNS14000009, 
         "Asian" = LNS14032183,
         .keep = "none")


# Define a custom color palette with EPI specific shades of blue
custom_blue_palette <- c("#A8CDEF", "#004466", "#4A81A3", "#709FC1")

# Create a ggplot line graph with the custom blue palette
ggplot(data = unemp_race, aes(x = date)) +
  geom_line(aes(y = White, color = "White"), linewidth = 1) +
  geom_line(aes(y = Black, color = "Black"), linewidth = 1) +
  geom_line(aes(y = `Hispanic or Latino`, color = "Hispanic or Latino"), linewidth = 1) +
  geom_line(aes(y = Asian, color = "Asian"), linewidth = 1) +
  labs(title = "Unemployment Rates by Race",
       x = "Year",
       y = "Unemployment Rate") +
  scale_color_manual(values = custom_blue_palette, 
                     name = "Race/Ethnicity") +  # Customize line colors and legend
  theme_minimal()

#### Census data 

#get API key from https://api.census.gov/data/key_signup.html
#Tidycensus documentation: https://walker-data.com/tidycensus/articles/basic-usage.html

#show all available tables:
acs_2021_variables <- load_variables(2021, "acs5", cache = TRUE)


options(tigris_use_cache = TRUE)
census_api_key(Sys.getenv("CENSUS_API_KEY"))

MI_demographics <- get_acs(table = "B01001",
                           geography = "county",
                           year = 2021,
                           state = "MI",
                           survey = "acs5")


get_acs(table = "B19013",
        geography = "county",
        year = 2021,
        state = "MI", 
        survey = "acs5")

MI_income <- get_acs(
  geography = "county", 
  state = "MI",
  variables = "B19013_001",
  year = 2021,
  geometry = TRUE,
)

plot(MI_income["estimate"])

detroit_income <- get_acs(
  geography = "tract", 
  state = "MI",
  county = "Wayne",
  variables = "B19013_001",
  year = 2021,
  geometry = TRUE,
)

plot(detroit_income["estimate"])

### more complex data requests using tidycensus
# poverty in rhode island: https://data.census.gov/table?q=B17001B
#define function to load multiple years of acs data
load_acs_tables <- function(x){
  get_acs(geography = "state", 
          variables = c(total_count = "B17001_001",  
                        count_income_below_poverty = "B17001_002",  
                        count_income_below_poverty_level_male = "B17001_003",  
                        count_income_below_poverty_level_female = "B17001_017"), 
          state = "RI", 
          year = x, 
          output = "wide") %>% 
    #create year variable
    mutate(year = x)
}

#load 2009:2018 5yr datasets with map_dfr()
RI_Poverty_B <- map_dfr(2009:2021, load_acs_tables)

#define function to load multiple years and multiple demographic groups for table b17001
load_acs_tables2 <- function(x,y){
  get_acs(geography = "state", 
          variables = c(total_count = paste0("B17001",y,"_001"),  
                        count_income_below_poverty = paste0("B17001",y,"_002"),  
                        count_income_below_poverty_level_male = paste0("B17001",y,"_003"),  
                        count_income_below_poverty_level_female = paste0("B17001",y,"_017")), 
          state = "RI", 
          year = x, 
          output = "wide") %>% 
    #create variables to identify years and demographic groups
    mutate(year = x,
           group = y)
}

#create list of arguments to pass to function
crossargs <- expand.grid(x=2009:2021, y=LETTERS[1:9])
#load all data
RI_Poverty <- map2_dfr(crossargs$x, crossargs$y, load_acs_tables2)





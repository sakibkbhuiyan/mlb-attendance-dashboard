## *****************************************************************************
##
## File Name: RScript_PrepData.R ----
##
## Author: Sakib Bhuiyan 
##
## Date Created: January 2023 
##
## *****************************************************************************
##
## Notes: 
##      
##      * This script scrapes MLB clubs' historical attendance data from ESPN. 
##        (e.g., https://www.espn.com/mlb/attendance/_/year/2012)
##
##
##
##
##
## *****************************************************************************


## *****************************************************************************
##
## IMPORT: Libraries & Dependencies ----
##
## *****************************************************************************

library(data.table)
library(dplyr)
library(arrow)
library(stringr)
library(janitor)
library(knitr)
library(broom)
library(rvest)





## *****************************************************************************
##
## Set Global Variables ----
##
## *****************************************************************************





## *****************************************************************************
## 
## Define Functions ----
##
## *****************************************************************************

## Function for "NOT IN" 
`%!in%` <- Negate('%in%')





## *****************************************************************************
## 
## IMPORT: Data ----
##
## *****************************************************************************

## *Scrape MLB Attendance Data from ESPN ----
list_years = c(seq(2012, 2019, 1), seq(2021, 2022, 1))

dat_all <- tibble()

for (yr in list_years) {
  
  end_point <- str_glue("https://www.espn.com/mlb/attendance/_/year/", {yr})
  
  html_data <- read_html(end_point)
  
  # Sys.sleep(3)
  
  dat_table <- html_data %>% html_table(fill = TRUE)
  
  dat_attendance <- dat_table[[1]][-c(1, 2),] %>%
    rename(
      RANK = X1,
      TEAM = X2,
      HOME_GAMES  = X3,
      HOME_TOTAL_ATTENDANCE  = X4,
      HOME_AVG_ATTENDANCE = X5,
      HOME_PCT_ATTENDANCE  = X6,
      AWAY_GAMES = X7,
      AWAY_AVG_ATTENDANCE = X8,
      AWAY_PCT_ATTENDANCE  = X9,
      OVERALL_GAMES = X10,
      OVERALL_AVG_ATTENDANCE  = X11,
      OVERALL_PCT_ATTENDANCE = X12
    ) %>%
    ## Remove commas
    mutate_at(
      c(
        'HOME_TOTAL_ATTENDANCE',
        'HOME_AVG_ATTENDANCE',
        'AWAY_AVG_ATTENDANCE',
        'OVERALL_AVG_ATTENDANCE'
      ),
      ~ stringr::str_replace(., ',', '')
    ) %>%
    mutate_at(c('HOME_TOTAL_ATTENDANCE'),
              ~ stringr::str_replace(., ',', '')) %>%
    ## Convert to numeric
    mutate_at(
      c(
        'RANK',
        'HOME_GAMES',
        'HOME_TOTAL_ATTENDANCE',
        'HOME_AVG_ATTENDANCE',
        'HOME_PCT_ATTENDANCE',
        'AWAY_GAMES',
        'AWAY_AVG_ATTENDANCE',
        'AWAY_PCT_ATTENDANCE',
        'OVERALL_GAMES',
        'OVERALL_AVG_ATTENDANCE',
        'OVERALL_PCT_ATTENDANCE'
      ),
      as.numeric
    ) %>%
    ## Convert to percentages
    mutate(
      HOME_PCT_ATTENDANCE = HOME_PCT_ATTENDANCE / 100,
      AWAY_PCT_ATTENDANCE = AWAY_PCT_ATTENDANCE / 100,
      OVERALL_PCT_ATTENDANCE = OVERALL_PCT_ATTENDANCE / 100, 
      YEAR = yr
    ) %>% 
    relocate(YEAR, .before = everything())
  
    dat_all <- bind_rows(dat_all, dat_attendance) 
  
}


## Convert to factor
dat_all_analysis <- dat_all %>% 
  mutate(YEAR = factor(YEAR))


## Sanity check data structure 
# dat_all_analysis %>% dim()
# dat_all_analysis %>% colnames()
# dat_all_analysis %>% str()
# dat_all_analysis %>% summary()



## *Scrape Favorite Team Data from Vivid Seats----

end_point <- "https://www.vividseats.com/blog/most-popular-mlb-teams-by-us-state"

html_data <- read_html(end_point)

dat_table <- html_data %>% html_table(fill = TRUE)

dat_favorites <- dat_table[[1]][-c(1, 2),] %>% 
  rename('STATE' = 1, 
         'TEAM_FIRST_FAV' = 2, 
         'PCT_TKT_SOLD_fIRST_FAV' = 3, 
         'TEAM_SECOND_FAV' = 4, 
         'PCT_TKT_SOLD_SECOND_FAV'= 5)




## *****************************************************************************
## 
## EXPORT: Data ----
##
## *****************************************************************************

arrow::write_parquet(dat_all_analysis, 'data/df_attendance.parquet')

readr::write_csv(dat_favorites, 'df_favorite_teams.csv')

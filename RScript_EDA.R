## *****************************************************************************
##
## File Name: RScript_EDA.R ----
##
## Author: Sakib Bhuiyan 
##
## Date Created: January 2023 
##
## *****************************************************************************
##
## Notes: 
##      
##      * This script explores conversion values from various email campaigns. 
##
##
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
library(ggplot2)
library(ggimage)
library(ggtext)
library(broom)





## *****************************************************************************
##
## Set Global Variables ----
##
## *****************************************************************************

## Brand colors
game_tier_cols <-
  c(
    "B" = "#8A724B",
    "S" = "#BBBDBE",
    "G" = "#FBDD69",
    "P" = "#8EABD1",
    "M" = "#F59799",
    "D" = "#578039"
  )


strategy_cols <-
  c(
    "blue" = "#000063",
    "gray" = "#aaaab0",
    "orange" = "#e19c1f",
    "red" = "#580000",
    "green" = "#003300"
  )


## Default theme for plots
ggplot2::theme_set(
  theme_bw() +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      text = element_text(family = 'sans'),
      strip.background = element_rect(
        color = "black",
        fill = strategy_cols[['blue']],
        size = 1,
        linetype = "solid"
      ),
      strip.text.x = element_text(size = 20,
                                  color = "white"),
      axis.title.x = ggtext::element_markdown(size = 18),
      axis.title.y = ggtext::element_markdown(size = 18),
      axis.text.x = element_text(size = 18),
      axis.text.y = element_text(size = 18),
      legend.text = element_text(size = 18),
      legend.title = ggtext::element_markdown(size = 18),
      plot.title = ggtext::element_markdown(size = 20),
      plot.subtitle = ggtext::element_markdown(size = 14),
      plot.title.position = "plot",
      plot.caption.position =  "plot", 
      ## Transparent panel background
      panel.background = element_rect(fill = 'transparent'),
      ## Transparent plot background
      plot.background = element_rect(fill = 'transparent', color = NA),
      ## Remove major gridlines
      panel.grid.major = element_blank(),
      ## Remove minor gridlines
      panel.grid.minor = element_blank(),
      # ## Transparent legend background
      # legend.background = element_rect(fill = 'transparent'),
      # ## Transparent legend panel
      # legend.box.background = element_rect(fill = 'transparent')
      legend.background = element_blank(), 
      legend.box.background = element_blank()
    )
)





## *****************************************************************************
## 
## Define Functions ----
##
## *****************************************************************************

## Function for "NOT IN" 
`%!in%` <- Negate('%in%')


## Function to trim trailing spaces
trim.trailing <- function (x) sub("\\s+$", "", x) 



## *****************************************************************************
## 
## IMPORT: Data ----
##
## *****************************************************************************

## *Load historical attendance data (parquet) ----
df_attendance <- arrow::read_parquet('data/df_attendance.parquet') %>% 
  mutate(TEAM = trim.trailing(TEAM))


## *Load logo data ----
df_logo <- read.csv('data/df_logo_source.csv') %>% 
  mutate(TEAM = trim.trailing(TEAM), 
         TEAM_COLOR = as.character(TEAM_COLOR), 
         LOGO_SRC = as.character(LOGO_SRC),
         LOGO_SRC = gsub(x = LOGO_SRC, pattern = "'", replacement = ""), 
         LOGO_SRC_2 = paste0("<img src = ", LOGO_SRC, ", width = '25' />"))


## *Join Attendance & Logo data ----
df_join <- df_attendance %>% 
  full_join(df_logo, by = 'TEAM')


## Sanity check data
# df_join %>% dim()
# df_join %>% colnames()
# df_join %>% str() 
# df_join %>% summary()





## *Favorite Team data ----
df_fav_team <- readr::read_csv('data/df_favorite_teams.csv')

df_fav_team_first <-
  df_fav_team %>% select(STATE, TEAM_FIRST_FAV, PCT_TKT_SOLD_FIRST_FAV)

df_fav_team_second <-
  df_fav_team %>% select(STATE, TEAM_SECOND_FAV, PCT_TKT_SOLD_SECOND_FAV)



## *Map data ----
# states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
# 
# states <- cbind(states, st_coordinates(st_centroid(states)))
# 
# states_analysis <- states %>%
#   mutate(ID = as.character(ID),
#          ID = toTitleCase(ID), 
#          STATE_ABB = purrr::map(.x = ID, ~ state.abb[grep(.x, state.name)])) %>%
#   rename(STATE = ID) 


df_state_grid <- readr::read_csv('data/df_state_grid.csv')




## *Join Favorite Team & Map & Logo data ----
# df_fav_team_first_join <- df_fav_team_first %>%
#   left_join(states_analysis, by = 'STATE') %>%
#   filter(!is.na(X)) %>%
#   left_join(df_logo, by = c('TEAM_FIRST_FAV' = 'TEAM_NAME'))


## First favorite team by state 
df_fav_team_first_join_grid <- df_fav_team_first %>%
  full_join(df_state_grid, by = 'STATE') %>%
  mutate(TEAM_FIRST_FAV = case_when(STATE_ABB == 'DC' ~ 'Washington Nationals',
                                    TRUE ~ TEAM_FIRST_FAV)) %>%
  left_join(df_logo, by = c('TEAM_FIRST_FAV' = 'TEAM_NAME')) 


## Second favorite team by state
df_fav_team_second_join_grid <- df_fav_team_second %>%
  full_join(df_state_grid, by = 'STATE') %>%
  mutate(TEAM_SECOND_FAV = case_when(STATE_ABB == 'DC' ~ 'Washington Nationals',
                                    TRUE ~ TEAM_SECOND_FAV)) %>%
  left_join(df_logo, by = c('TEAM_SECOND_FAV' = 'TEAM_NAME')) 
  


## *Export data as parquet ----
## (These parquet files will be loaded by the Shiny app at startup)
arrow::write_parquet(df_join, 'data/app_data_attendance_by_year.parquet')

arrow::write_parquet(df_fav_team_first_join_grid, 'data/app_data_fav_team_first.parquet')

arrow::write_parquet(df_fav_team_second_join_grid, 'data/app_data_fav_team_second.parquet')





## *****************************************************************************
## 
## ANALYSIS: Home Attendance (Snapshot Year) ----
##
## *****************************************************************************

## *PLOT: (Bar Plot): Top 10 HOME_AVG_ATTENDANCE x YEAR ----
## Filter to top-10 for a specific year 
choice_yr <- 2019
dat <- df_join %>%
  filter(YEAR == choice_yr) %>%
  mutate(
    RANK_IMPUTE = row_number(desc(HOME_AVG_ATTENDANCE)),
    COLOR = case_when(RANK_IMPUTE <= 5 ~ '#5CF64A',
                      TRUE ~ 'gray80'),
    LABEL = case_when(RANK_IMPUTE == 1 ~ paste0('#', RANK_IMPUTE),
                      TRUE ~ paste0(RANK_IMPUTE))
  ) %>%
  filter(RANK_IMPUTE <= 10)


## Create x-axis's image labels 
x_labels <- dat %>% 
  arrange(desc(HOME_AVG_ATTENDANCE)) %>% 
  select(LOGO_SRC_2) %>% 
  pull()


fig <- ggplot(data = dat,
              mapping = aes(x = reorder(LOGO_SRC_2,-HOME_AVG_ATTENDANCE),
                            y = HOME_AVG_ATTENDANCE)) +
  geom_bar(stat = 'identity',
           mapping = aes(fill = RANK),
           width = 0.80) +
  geom_text(
    mapping = aes(label = LABEL),
    vjust = 2.0,
    color = '#FFFFFF',
    size = 8,
    fontface = 'bold'
  ) +
  geom_image(mapping = aes(image = LOGO_SRC, y = HOME_AVG_ATTENDANCE + 2.0e3), 
             size = 0.07) +
  scale_y_continuous(
    name = 'Average Home Attendance',
    limits = c(0, 5.5e4),
    breaks = seq(0, 4.0e4, 1.0e4), 
    labels = scales::number_format(scale = 1.0e-3, suffix = 'K')
  ) +
  scale_fill_gradient(low = '#09203F', high = '#537895', guide = 'none') + 
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank()
  ) 


print(fig)





## *PLOT: (Bar Plot): Top 10 HOME_TOTAL_ATTENDANCE x YEAR ----
## Filter to top-10 for a specific year 
choice_yr <- 2022
dat <- df_join %>%
  filter(YEAR == choice_yr) %>%
  mutate(
    RANK_IMPUTE = row_number(desc(HOME_TOTAL_ATTENDANCE)),
    COLOR = case_when(RANK_IMPUTE <= 5 ~ '#5CF64A',
                      TRUE ~ 'gray80'),
    LABEL = case_when(RANK_IMPUTE == 1 ~ paste0('#', RANK_IMPUTE),
                      TRUE ~ paste0(RANK_IMPUTE))
  ) %>%
  filter(RANK_IMPUTE <= 10)


fig <- ggplot(data = dat,
              mapping = aes(x = reorder(LOGO_SRC_2,-HOME_TOTAL_ATTENDANCE),
                            y = HOME_TOTAL_ATTENDANCE)) +
  geom_bar(stat = 'identity',
           mapping = aes(fill = RANK),
           width = 0.80) +
  geom_text(
    mapping = aes(label = LABEL),
    vjust = 2.0,
    color = '#FFFFFF',
    size = 8,
    fontface = 'bold'
  ) +
  geom_image(mapping = aes(image = LOGO_SRC, y = HOME_TOTAL_ATTENDANCE + 2.0e5), 
             size = 0.07) +
  scale_y_continuous(
    name = 'Total Home Attendance',
    limits = c(0, 4.25e6),
    breaks = seq(0, 4.0e6, 1.0e6), 
    labels = scales::number_format(scale = 1.0e-6, suffix = 'MM')
  ) +
  scale_fill_gradient(low = '#09203F', high = '#537895', guide = 'none') + 
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(), 
    axis.ticks.x = element_blank()
  ) 


print(fig)





## *****************************************************************************
## 
## ANALYSIS: Home Attendance (Over Time) ----
##
## *****************************************************************************

choice_team <- 'Chicago Cubs'

dat <- df_join %>%
  filter(TEAM_NAME == choice_team) %>% 
  add_row(YEAR = '2020', TEAM_NAME = choice_team, HOME_TOTAL_ATTENDANCE = 0, HOME_AVG_ATTENDANCE = 0) %>% 
  mutate(YEAR = as.numeric(YEAR))

line_color <- dat %>%
  filter(!is.na(TEAM_COLOR)) %>% 
  distinct(TEAM_COLOR) %>% 
  pull()

bkground_image <- dat %>%
  filter(!is.na(LOGO_SRC_2)) %>% 
  distinct(LOGO_SRC) %>% 
  pull()

bkground_image_rendered <- png::readPNG(bkground_image, native = TRUE)

## *PLOT: (Line Plot): Home attendance for 1 club over time ----
## (READY TO PUBLISH)
p <- ggplot(data = dat, 
       mapping = aes(x = YEAR, 
                     y = HOME_TOTAL_ATTENDANCE, 
                     group = '1')) + 
  geom_line(color = line_color,
            size = 2) +
  # geom_point(color = line_color, 
  #            size = 6) + 
  scale_x_continuous(limits = c(2012, 2022), 
                     breaks = seq(2012, 2022, 1))+ 
  scale_y_continuous(name = 'Total Home Attendance', 
                     labels = scales::number_format(scale = 1e-6, suffix = 'MM'), 
                     limits = c(0, 4.0e6), 
                     breaks = seq(0, 4.0e6, 1.0e6)) + 
  patchwork::inset_element(p = bkground_image_rendered,
                           left = 0.05,
                           bottom = 0.05,
                           right = 0.25,
                           top = 0.25, 
                           align_to = 'panel') + 
  theme(
    axis.title.x = element_blank()
  )


print(p)





## *****************************************************************************
## 
## ANALYSIS: Home Attendance 2 Team Comparison (Over Time) ----
##
## *****************************************************************************

choice_team_1 <- 'Chicago Cubs'
choice_team_2 <- 'Houston Astros'

choice_teams <- c(choice_team_1, choice_team_2)

line_color_1 <- df_join %>%
  filter(!is.na(TEAM_COLOR), 
         TEAM_NAME == choice_teams[1]) %>% 
  distinct(TEAM_NAME, TEAM_COLOR) %>% 
  pull()


line_color_2 <- df_join %>%
  filter(!is.na(TEAM_COLOR), 
         TEAM_NAME == choice_teams[2]) %>% 
  distinct(TEAM_NAME, TEAM_COLOR) %>% 
  pull()

dat <- df_join %>%
  filter(TEAM_NAME %in% choice_teams) %>%
  add_row(
    YEAR = '2020',
    TEAM_NAME = choice_teams[1],
    HOME_TOTAL_ATTENDANCE = 0,
    HOME_AVG_ATTENDANCE = 0,
    TEAM_COLOR = line_color_1
  ) %>%
  add_row(
    YEAR = '2020',
    TEAM_NAME = choice_teams[2],
    HOME_TOTAL_ATTENDANCE = 0,
    HOME_AVG_ATTENDANCE = 0,
    TEAM_COLOR = line_color_2
  ) %>%
  mutate(YEAR = as.numeric(YEAR))

dat_annotate <- dat %>% 
  filter(YEAR == 2012)


## *PLOT: (Line Plot): 2 team attendance comparison ----
fig <- ggplot(
  data = dat,
  mapping = aes(x = YEAR,
                y = HOME_TOTAL_ATTENDANCE,
                group = TEAM_NAME)
) +
  geom_line(
    mapping = aes(color = TEAM_COLOR),
    size = 1,
    alpha = 0.9
  ) +
  geom_image(data = dat_annotate,
             mapping = aes(image = LOGO_SRC),
             size = 0.07) +
  scale_x_continuous(limits = c(2012, 2022),
                     breaks = seq(2012, 2022, 1)) +
  scale_y_continuous(
    name = 'Total Home Attendance',
    limits = c(0, 4.5e6),
    breaks = seq(0, 4.0e6, 1.0e6),
    labels = scales::number_format(scale = 1.0e-6, suffix = 'MM')
  ) +
  scale_color_identity() +
  theme(axis.title.x = element_blank())

print(fig)





## *****************************************************************************
## 
## ANALYSIS: Favorite Teams ----
##
## *****************************************************************************

## *PLOT (Map): 1st Favorite Team by State ---- 
## (READY TO PUBLISH)
fig <- ggplot(data = df_fav_team_first_join_grid, 
       mapping = aes(x = X, 
                     y = Y)) + 
  geom_text(mapping = aes(label = STATE_ABB, 
                          y = Y-0.3, 
                          color = TEAM_COLOR), 
            size = 8, 
            alpha = 0.8,
            fontface = 'bold') + 
  geom_image(mapping = aes(image = LOGO_SRC), 
             size = 0.07) + 
  scale_color_identity() + 
  theme_void()


print(fig)


ggsave(filename = 'www/map_first_favorite.png', 
       height = 1000, 
       width = 2000, 
       units = 'px')



## *PLOT (Map): 2nd Favorite Team by State ---- 
## (READY TO PUBLISH)
fig <- ggplot(data = df_fav_team_second_join_grid, 
              mapping = aes(x = X, 
                            y = Y)) + 
  geom_text(mapping = aes(label = STATE_ABB, 
                          y = Y-0.3, 
                          color = TEAM_COLOR), 
            size = 8, 
            alpha = 0.8,
            fontface = 'bold') + 
  geom_image(mapping = aes(image = LOGO_SRC), 
             size = 0.07) + 
  scale_color_identity() + 
  theme_void()


print(fig)


ggsave(filename = 'www/map_second_favorite.png', 
       height = 1000, 
       width = 2000, 
       units = 'px')

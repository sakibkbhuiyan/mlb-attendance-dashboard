## *****************************************************************************
## 
## IMPORT: Libraries ----
##
## *****************************************************************************

suppressMessages(require(shiny))
suppressMessages(require(shinythemes))
suppressMessages(require(shinycssloaders))
suppressMessages(require(showtext))
suppressMessages(require(data.table))
suppressMessages(require(dplyr))
suppressMessages(require(arrow))
suppressMessages(require(stringr))
suppressMessages(require(janitor))
suppressMessages(require(knitr))
suppressMessages(require(ggplot2))
suppressMessages(require(ggimage))
suppressMessages(require(ggtext))
suppressMessages(require(broom))





## *****************************************************************************
##
## Set Global Variables ----
##
## *****************************************************************************

## Font specification
sysfonts::font_add_google(name = 'Poppins', family = 'poppins')


## Brand colors
strategy_cols <-
  c(
    "blue" = "#000063",
           "gray" = "#aaaab0",
           "orange" = "#e19c1f",
           "red" = "#580000",
           "green" = "#003300"
  )


## Aesthetic options
options(
  spinner.color = strategy_cols[['blue']],
  spinner.color.background = '#FAF9F6',
  spinner.size = 2
)





## *****************************************************************************
##
## User Interface ----
##
## *****************************************************************************
ui <- fluidPage(
  list(tags$head(HTML('<link rel="icon", href="logo_MLB.png", 
                                   type="image/png" />'))),
  div(style="padding: 1px 0px; width: '100%'",
      titlePanel(
        title="", windowTitle="MLB Attendance"
      )
  ), 
  
  navbarPage(title = div(img(src = 'logo_MLB.png', height = '40px'), "MLB Attendance"),
             tags$head(tags$style(
               HTML(
                 '.navbar-nav > li > a, .navbar-brand {
                            padding-top:10px !important;
                            padding-bottom:10px !important;
                            font-size: 40px !important;
                            height: 60px;
                            color: white
               }
               .navbar {
                   font-size: 40px !important; 
                   background-color: #cbd4e7;
                   overflow: hidden;
                   min-height:60px
               }

              .navbar .navbar-default {color: white;}
              
              .navbar .tab-panel{background-color: #cbd4e7; 
                  color: #cbd4e7
              }
              
              .navbar .navbar-nav li a:hover, .navbar-nav > .active > a {
                color: #FFFFFF !important;
                background-color:#cbd4e7 !important;
                background-image: #FFFFFF !important;
                border-bottom: 3px solid red;
              }
              
              .navbar a.active {
                border-bottom: 3px solid red;
              }'
               )
             )), 
             
             ## *Top 10 Attendance Tab ---- 
             tabPanel(
               title = 'Top 10 Attendance', 
               includeHTML('www/ui_top_ten.html'), 
               textOutput("yr") %>% 
                 tagAppendAttributes(class = 'boxed'), 
               fluidRow(withSpinner(
                 plotOutput("plot_top_ten",
                            width = "100%",
                            height = "1000px"),
                 type = 1
               ),
               align = 'center'), 
               HTML("<br>"), 
               HTML('<p style= "color:#AEB6BF; font-size:20px">Data sourced from ESPN.com</p>')
               
             ), 
             
             ## *Individual Team Tab ---- 
             tabPanel(
               '1 Team', 
               includeHTML('www/ui_indv_team.html'), 
               textOutput("indv_team") %>% 
                 tagAppendAttributes(class = 'boxed'), 
               HTML("<br>"), 
               fluidRow(withSpinner(
                 plotOutput("plot_indv_team",
                            width = "100%",
                            height = "1000px"),
                 type = 1
               ),
               align = 'center'),  
               HTML("<br>"), 
               HTML('<p style= "color:#AEB6BF; font-size:20px">Data sourced from ESPN.com</p>')
             ), 
             
             ## *Two-team Comparison Tab ---- 
             tabPanel(
               '2 Team Head-to-Head',
               includeHTML('www/ui_two_team.html'),
               HTML("<br>"), 
               fluidRow(withSpinner(
                 plotOutput("plot_2_team",
                            width = "100%",
                            height = "1000px"),
                 type = 1
               ),
               align = 'center'),
               HTML("<br>"), 
               HTML('<p style= "color:#AEB6BF; font-size:20px">Data sourced from ESPN.com</p>')
             ),
             
             ## *Favorite Teams Tab ---- 
             tabPanel(
               'State Favorites',
               includeHTML('www/ui_fav_team.html'),
               HTML("<br>"),
               
               textOutput("fav_team_choice") %>%
                 tagAppendAttributes(class = 'boxed-dark'),
               
               fluidRow(withSpinner(
                 plotOutput("plot_fav",
                            width = "100%",
                            height = "1000px"),
                 type = 1
               ),
               align = 'center'),
               HTML("<br>"), 
               HTML('<p style= "color:#AEB6BF; font-size:20px">Data sourced from VividSeats.com</p>')
             ), 
             collapsible = TRUE, 
             tags$style(
               ".navbar-nav li a {
                  font-size: 60px;
                  font-weight: bold;
                }
              "
             )
  )
)



## *****************************************************************************
##
## Define Server Logic ----
##
## *****************************************************************************

server <- function(input, output) {
  
  ## ***************************************************************************
  ##
  ## Set Global Variables ----
  ##
  ## ***************************************************************************
  
  ## Font specification
  sysfonts::font_add_google(name = 'Poppins', family = 'poppins')
  
  
  ## Brand colors 
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
        text = element_text(family = 'poppins'),
        strip.background = element_rect(
          color = "black",
          fill = strategy_cols[['blue']],
          size = 1,
          linetype = "solid"
        ),
        strip.text.x = element_text(size = 32,
                                    color = "white"),
        axis.title.x = ggtext::element_markdown(size = 20),
        axis.title.y = ggtext::element_markdown(size = 20),
        axis.text.x = element_text(size = 16, vjust = -0.5),
        axis.text.y = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.title = ggtext::element_markdown(size = 20),
        plot.title = ggtext::element_markdown(size = 32),
        plot.subtitle = ggtext::element_markdown(size = 18),
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
  
  
  
  
  
  ## ***************************************************************************
  ##
  ## IMPORT: Data ----
  ##
  ## ***************************************************************************
  
  ## Attendance by Year data 
  dat_att_by_yr <-
    arrow::read_parquet('data/app_data_attendance_by_year.parquet')

  
  
  
  
  ## ***************************************************************************
  ##
  ## Top 10 Attendance Page ----
  ##
  ## ***************************************************************************
  
  ## Get user's choice of year 
  choice_yr = reactiveVal(value = '2012')
  
  observeEvent(input$yr_2012, {
    choice_yr('2012')
    
  })
  
  observeEvent(input$yr_2013, {
    choice_yr('2013')
    
  })
  
  observeEvent(input$yr_2014, {
    choice_yr('2014')
    
  })
  
  observeEvent(input$yr_2015, {
    choice_yr('2015')
    
  })
  
  observeEvent(input$yr_2016, {
    choice_yr('2016')
    
  })
  
  observeEvent(input$yr_2017, {
    choice_yr('2017')
    
  })
  
  observeEvent(input$yr_2018, {
    choice_yr('2018')
    
  })
  
  observeEvent(input$yr_2019, {
    choice_yr('2019')
    
  })
  
  observeEvent(input$yr_2021, {
    choice_yr('2021')
    
  })
  
  observeEvent(input$yr_2022, {
    choice_yr('2022')
    
  })
  
  output$yr = renderText(choice_yr())
  
  
  ##* Create plot ----
  output$plot_top_ten = renderPlot({
    
    if (is.null(choice_yr())) {
      return(NULL)
    }
    
    ## Use Google font
    showtext_auto()
    
    ## Filter data frame to chosen year
    dat <- dat_att_by_yr %>%
      filter(YEAR == choice_yr()) %>%
      mutate(
        RANK_IMPUTE = row_number(desc(HOME_AVG_ATTENDANCE)),
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
                  mapping = aes(x = reorder(LOGO_SRC_2, -HOME_AVG_ATTENDANCE),
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
        limits = c(0, 5.5e4),
        breaks = seq(0, 5.0e4, 1.0e4),
        labels = scales::number_format(scale = 1.0e-3, suffix = 'K')
      ) +
      scale_fill_gradient(low = '#09203F',
                          high = '#537895',
                          guide = 'none') +
      theme(axis.title.y = element_blank(), 
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank()
      ) 
    
    
    fig
    
  })
  
  
  
  ## ***************************************************************************
  ##
  ## Individual Team Page ----
  ##
  ## ***************************************************************************
  
  ## Get user's choice of team 
  choice_indv_team = reactiveVal(value = 'Chicago Cubs')
  
  observeEvent(input$btn_indv_team_ARI, {
    choice_indv_team('Arizona Diamondbacks')
    
  })
  
  observeEvent(input$btn_indv_team_ATL, {
    choice_indv_team('Atlanta Braves')
    
  })
  
  observeEvent(input$btn_indv_team_ATL, {
    choice_indv_team('Atlanta Braves')
    
  })
  
  observeEvent(input$btn_indv_team_BAL, {
    choice_indv_team('Baltimore Orioles')
    
  })
  
  observeEvent(input$btn_indv_team_BOS, {
    choice_indv_team('Boston Red Sox')
    
  })
  
  observeEvent(input$btn_indv_team_CHC, {
    choice_indv_team('Chicago Cubs')
    
  })
  
  observeEvent(input$btn_indv_team_CWS, {
    choice_indv_team('Chicago White Sox')
    
  })
  
  observeEvent(input$btn_indv_team_CIN, {
    choice_indv_team('Cincinnati Reds')
    
  })
  
  observeEvent(input$btn_indv_team_CLE, {
    choice_indv_team('Cleveland Guardians')
    
  })
  
  observeEvent(input$btn_indv_team_COL, {
    choice_indv_team('Colorado Rockies')
    
  })
  
  observeEvent(input$btn_indv_team_DET, {
    choice_indv_team('Detroit Tigers')
    
  })
  
  observeEvent(input$btn_indv_team_HOU, {
    choice_indv_team('Houston Astros')
    
  })
  
  observeEvent(input$btn_indv_team_KC, {
    choice_indv_team('Kansas City Royals')
    
  })
  
  observeEvent(input$btn_indv_team_LAA, {
    choice_indv_team('Los Angeles Angels')
    
  })
  
  observeEvent(input$btn_indv_team_LAD, {
    choice_indv_team('Los Angeles Dodgers')
    
  })
  
  observeEvent(input$btn_indv_team_FLA, {
    choice_indv_team('Miami Marlins')
    
  })
  
  observeEvent(input$btn_indv_team_MIL, {
    choice_indv_team('Milwaukee Brewers')
    
  })
  
  observeEvent(input$btn_indv_team_MIN, {
    choice_indv_team('Minnesota Twins')
    
  })
  
  observeEvent(input$btn_indv_team_NYM, {
    choice_indv_team('New York Mets')
    
  })
  
  observeEvent(input$btn_indv_team_NYY, {
    choice_indv_team('New York Yankees')
    
  })
  
  observeEvent(input$btn_indv_team_OAK, {
    choice_indv_team('Oakland Athletics')
    
  })
  
  observeEvent(input$btn_indv_team_PHI, {
    choice_indv_team('Philadelphia Phillies')
    
  })
  
  observeEvent(input$btn_indv_team_PIT, {
    choice_indv_team('Pittsburgh Pirates')
    
  })
  
  observeEvent(input$btn_indv_team_SD, {
    choice_indv_team('San Diego Padres')
    
  })
  
  observeEvent(input$btn_indv_team_SEA, {
    choice_indv_team('Seattle Mariners')
    
  })
  
  observeEvent(input$btn_indv_team_SF, {
    choice_indv_team('San Francisco Giants')
    
  })
  
  observeEvent(input$btn_indv_team_STL, {
    choice_indv_team('St. Louis Cardinals')
    
  })
  
  observeEvent(input$btn_indv_team_TB, {
    choice_indv_team('Tampa Bay Rays')
    
  })
  
  observeEvent(input$btn_indv_team_TEX, {
    choice_indv_team('Texas Rangers')
    
  })
  
  observeEvent(input$btn_indv_team_TOR, {
    choice_indv_team('Toronto Blue Jays')
    
  })
  
  observeEvent(input$btn_indv_team_WSH, {
    choice_indv_team('Washington Nationals')
    
  })
  
  output$indv_team = renderText(choice_indv_team())
  
  
  ## *Create plot ----
  output$plot_indv_team = renderPlot({
    
    if (is.null(choice_indv_team())) {
      return(NULL)
    }
    
    ## Use Google font
    showtext_auto()
    
    ## Filter data frame to chosen team
    dat <- dat_att_by_yr %>%
      filter(TEAM_NAME == choice_indv_team()) %>%
      add_row(
        YEAR = '2020',
        TEAM_NAME = choice_indv_team(),
        HOME_TOTAL_ATTENDANCE = 0,
        HOME_AVG_ATTENDANCE = 0
      ) %>%
      mutate(YEAR = as.numeric(YEAR))
    
    
    line_color <- dat %>%
      filter(!is.na(TEAM_COLOR)) %>%
      distinct(TEAM_COLOR) %>%
      pull()
    
    
    fig <- ggplot(data = dat,
                  mapping = aes(x = YEAR,
                                y = HOME_TOTAL_ATTENDANCE,
                                group = '1')) +
      geom_line(size = 1,
                color = line_color) +
      geom_point(size = 10,
                 color = line_color) +
      scale_x_continuous(limits = c(2012, 2022),
                         breaks = seq(2012, 2022, 1)) +
      scale_y_continuous(
        labels = scales::number_format(scale = 1e-6, suffix = 'MM'),
        limits = c(0, 4.0e6),
        breaks = seq(0, 4.0e6, 1.0e6)
      ) +
      theme(axis.title.y = element_blank(), 
            axis.title.x = element_blank())
    
    
    fig 
  })
  
  
  
  
  ## ***************************************************************************
  ##
  ## Two Team Head-to-head Comparison Page ----
  ##
  ## ***************************************************************************
  
  ## Get user's choice of team 1
  choice_team_1 = reactiveVal(value = "Chicago Cubs")
  
  observeEvent(input$team_1_ARI, {
    choice_team_1('Arizona Diamondbacks')
    
  })
  
  observeEvent(input$team_1_ATL, {
    choice_team_1('Atlanta Braves')
    
  })
  
  observeEvent(input$team_1_ATL, {
    choice_team_1('Atlanta Braves')
    
  })
  
  observeEvent(input$team_1_BAL, {
    choice_team_1('Baltimore Orioles')
    
  })
  
  observeEvent(input$team_1_BOS, {
    choice_team_1('Boston Red Sox')
    
  })
  
  observeEvent(input$team_1_CHC, {
    choice_team_1('Chicago Cubs')
    
  })
  
  observeEvent(input$team_1_CWS, {
    choice_team_1('Chicago White Sox')
    
  })
  
  observeEvent(input$team_1_CIN, {
    choice_team_1('Cincinnati Reds')
    
  })
  
  observeEvent(input$team_1_CLE, {
    choice_team_1('Cleveland Guardians')
    
  })
  
  observeEvent(input$team_1_COL, {
    choice_team_1('Colorado Rockies')
    
  })
  
  observeEvent(input$team_1_DET, {
    choice_team_1('Detroit Tigers')
    
  })
  
  observeEvent(input$team_1_HOU, {
    choice_team_1('Houston Astros')
    
  })
  
  observeEvent(input$team_1_KC, {
    choice_team_1('Kansas City Royals')
    
  })
  
  observeEvent(input$team_1_LAA, {
    choice_team_1('Los Angeles Angels')
    
  })
  
  observeEvent(input$team_1_LAD, {
    choice_team_1('Los Angeles Dodgers')
    
  })
  
  observeEvent(input$team_1_FLA, {
    choice_team_1('Miami Marlins')
    
  })
  
  observeEvent(input$team_1_MIL, {
    choice_team_1('Milwaukee Brewers')
    
  })
  
  observeEvent(input$team_1_MIN, {
    choice_team_1('Minnesota Twins')
    
  })
  
  observeEvent(input$team_1_NYM, {
    choice_team_1('New York Mets')
    
  })
  
  observeEvent(input$team_1_NYY, {
    choice_team_1('New York Yankees')
    
  })
  
  observeEvent(input$team_1_OAK, {
    choice_team_1('Oakland Athletics')
    
  })
  
  observeEvent(input$team_1_PHI, {
    choice_team_1('Philadelphia Phillies')
    
  })
  
  observeEvent(input$team_1_PIT, {
    choice_team_1('Pittsburgh Pirates')
    
  })
  
  observeEvent(input$team_1_SD, {
    choice_team_1('San Diego Padres')
    
  })
  
  observeEvent(input$team_1_SEA, {
    choice_team_1('Seattle Mariners')
    
  })
  
  observeEvent(input$team_1_SF, {
    choice_team_1('San Francisco Giants')
    
  })
  
  observeEvent(input$team_1_STL, {
    choice_team_1('St. Louis Cardinals')
    
  })
  
  observeEvent(input$team_1_TB, {
    choice_team_1('Tampa Bay Rays')
    
  })
  
  observeEvent(input$team_1_TEX, {
    choice_team_1('Texas Rangers')
    
  })
  
  observeEvent(input$team_1_TOR, {
    choice_team_1('Toronto Blue Jays')
    
  })
  
  observeEvent(input$team_1_WSH, {
    choice_team_1('Washington Nationals')
    
  })
  
  output$team_1= renderText(choice_team_1())
  
  
  ## Get user's choice of team 2
  choice_team_2 = reactiveVal(value = "Pittsburgh Pirates")
  
  observeEvent(input$team_2_ARI, {
    choice_team_2('Arizona Diamondbacks')
    
  })
  
  observeEvent(input$team_2_ATL, {
    choice_team_2('Atlanta Braves')
    
  })
  
  observeEvent(input$team_2_ATL, {
    choice_team_2('Atlanta Braves')
    
  })
  
  observeEvent(input$team_2_BAL, {
    choice_team_2('Baltimore Orioles')
    
  })
  
  observeEvent(input$team_2_BOS, {
    choice_team_2('Boston Red Sox')
    
  })
  
  observeEvent(input$team_2_CHC, {
    choice_team_2('Chicago Cubs')
    
  })
  
  observeEvent(input$team_2_CWS, {
    choice_team_2('Chicago White Sox')
    
  })
  
  observeEvent(input$team_2_CIN, {
    choice_team_2('Cincinnati Reds')
    
  })
  
  observeEvent(input$team_2_CLE, {
    choice_team_2('Cleveland Guardians')
    
  })
  
  observeEvent(input$team_2_COL, {
    choice_team_2('Colorado Rockies')
    
  })
  
  observeEvent(input$team_2_DET, {
    choice_team_2('Detroit Tigers')
    
  })
  
  observeEvent(input$team_2_HOU, {
    choice_team_2('Houston Astros')
    
  })
  
  observeEvent(input$team_2_KC, {
    choice_team_2('Kansas City Royals')
    
  })
  
  observeEvent(input$team_2_LAA, {
    choice_team_2('Los Angeles Angels')
    
  })
  
  observeEvent(input$team_2_LAD, {
    choice_team_2('Los Angeles Dodgers')
    
  })
  
  observeEvent(input$team_2_FLA, {
    choice_team_2('Miami Marlins')
    
  })
  
  observeEvent(input$team_2_MIL, {
    choice_team_2('Milwaukee Brewers')
    
  })
  
  observeEvent(input$team_2_MIN, {
    choice_team_2('Minnesota Twins')
    
  })
  
  observeEvent(input$team_2_NYM, {
    choice_team_2('New York Mets')
    
  })
  
  observeEvent(input$team_2_NYY, {
    choice_team_2('New York Yankees')
    
  })
  
  observeEvent(input$team_2_OAK, {
    choice_team_2('Oakland Athletics')
    
  })
  
  observeEvent(input$team_2_PHI, {
    choice_team_2('Philadelphia Phillies')
    
  })
  
  observeEvent(input$team_2_PIT, {
    choice_team_2('Pittsburgh Pirates')
    
  })
  
  observeEvent(input$team_2_SD, {
    choice_team_2('San Diego Padres')
    
  })
  
  observeEvent(input$team_2_SEA, {
    choice_team_2('Seattle Mariners')
    
  })
  
  observeEvent(input$team_2_SF, {
    choice_team_2('San Francisco Giants')
    
  })
  
  observeEvent(input$team_2_STL, {
    choice_team_2('St. Louis Cardinals')
    
  })
  
  observeEvent(input$team_2_TB, {
    choice_team_2('Tampa Bay Rays')
    
  })
  
  observeEvent(input$team_2_TEX, {
    choice_team_2('Texas Rangers')
    
  })
  
  observeEvent(input$team_2_TOR, {
    choice_team_2('Toronto Blue Jays')
    
  })
  
  observeEvent(input$team_2_WSH, {
    choice_team_2('Washington Nationals')
    
  })
  
  output$team_2 = renderText(choice_team_2())
  
  
  ## *Create plot ----
  output$plot_2_team = renderPlot({
    
    ## Use Google font
    showtext_auto()
    
    
    
    line_color_1 <- dat_att_by_yr %>%
      filter(!is.na(TEAM_COLOR), 
             TEAM_NAME == choice_team_1()) %>% 
      distinct(TEAM_NAME, TEAM_COLOR) %>% 
      pull()
    
    
    line_color_2 <- dat_att_by_yr %>%
      filter(!is.na(TEAM_COLOR), 
             TEAM_NAME == choice_team_2()) %>% 
      distinct(TEAM_NAME, TEAM_COLOR) %>% 
      pull()
    
    
    dat <- dat_att_by_yr %>%
      filter(TEAM_NAME == choice_team_1() | TEAM_NAME == choice_team_2()) %>%
      add_row(
        YEAR = '2020',
        TEAM_NAME = choice_team_1(),
        HOME_TOTAL_ATTENDANCE = 0,
        HOME_AVG_ATTENDANCE = 0,
        TEAM_COLOR = line_color_1
      ) %>%
      add_row(
        YEAR = '2020',
        TEAM_NAME = choice_team_2(),
        HOME_TOTAL_ATTENDANCE = 0,
        HOME_AVG_ATTENDANCE = 0,
        TEAM_COLOR = line_color_2
      ) %>%
      mutate(YEAR = as.numeric(YEAR))
    
    
    dat_annotate <- dat %>% 
      filter(YEAR == 2012)
    
    
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
      geom_point(
        mapping = aes(color = TEAM_COLOR),
        size = 10,
        alpha = 0.9
      ) +
      geom_image(data = dat_annotate,
                 mapping = aes(image = LOGO_SRC),
                 size = 0.06) +
      scale_x_continuous(limits = c(2012, 2022),
                         breaks = seq(2012, 2022, 1)) +
      scale_y_continuous(
        limits = c(0, 4.5e6),
        breaks = seq(0, 4.0e6, 1.0e6),
        labels = scales::number_format(scale = 1.0e-6, suffix = 'MM')
      ) +
      scale_color_identity() +
      theme(axis.title.y = element_blank(),
            axis.title.x = element_blank())
    
    fig
  })
  
  
  
  
  
  ## ***************************************************************************
  ##
  ## Favorite Teams Page ----
  ##
  ## ***************************************************************************
  
  ## *First favorite team ----
  ## Wait for user's choice of first favorite vs. second favorite 
  fav_team = reactiveVal(value = NULL)
  
  
  observeEvent(input$fav_teams_first, {
    fav_team("first")
    
    output$fav_team_choice = renderText("1st Favorite Team by State")
    
    output$plot_fav = renderImage({
        if (fav_team() != "first") {
          return(NULL)
        }
      
      list(src = 'www/map_first_favorite.png', 
           contentType = 'image/png', 
           width = 2000, 
           height = 1000, 
           alt = '1st Favorite by State')
      
    }, deleteFile = FALSE)
    
  })
  
  ## *Second favorite team ----
  observeEvent(input$fav_teams_second, {
    fav_team("second")
    
    output$fav_team_choice = renderText("2nd Favorite Team by State")
    
    output$plot_fav = renderImage({
      if (fav_team() != "second") {
        return(NULL)
      }
      
      list(src = 'www/map_second_favorite.png', 
           contentType = 'image/png', 
           width = 2000, 
           height = 1000, 
           alt = '2nd Favorite by State')
      
    }, deleteFile = FALSE)
    
  })
}





## *****************************************************************************
##
## Run the Application ----
##
## *****************************************************************************
shinyApp(ui = ui, server = server)

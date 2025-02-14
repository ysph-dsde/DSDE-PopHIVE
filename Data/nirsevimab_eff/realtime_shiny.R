# 
# library(shinyWidgets)
# library(shiny)
# library(ggplot2)
# library(dplyr)
# library(tidyr)
# library(tidyverse)
# library(readxl)
# library(writexl)
# library(reactable)
# library(ggplot2)
# library(stringr)
# library(flextable)
# library(lubridate)
# library(MASS)
# library(smd)
# library(patchwork)
# library(gtsummary) 
# library(multcomp)
#library(INLA)
#library(gt)

source("../../code/utils_mAb_VE.R")
source("../../code/jags_waning.R")


set.seed(123)


#############################
### read in data from file
#############################

# read data from season 23/24
df.2324 <- read.csv("../../data/clean_data/df.mab.csv") %>% 
  mutate(collection_date = as.Date(collection_date), mab_date = as.Date(mab_date)) %>% 
  process_df()

# read data from season 24/25
df.2425 <- readRDS("../../data/clean_data/ve_realtime/df.mab.realtime.2025-02-05.rds") %>% 
  mutate(rsv_mab = rsv_mab_2425) %>% # use only this season's (24/25) vaccination status as exposure
  process_df_2425()


# select variables and concatenate the two seasons together 
df.season1 <- df.2324 %>%
  mutate(collection_season = "23/24") %>%
  dplyr::select(collection_season, collection_date, rsv_mab, rsv_mab_dose, mab_date, 
                encounter_type, Sex,
                icu_admitted, hosp_los, icu_los_days,
                race_ethnicity, insurance_type, birth_weight, # highflow_oxygen,
                starts_with("days_btw_mab_collection"), weeks_btw_mab_collection_cat,
                starts_with("month_when_tested"), starts_with("age_at_test"),
                starts_with("symp"), starts_with("risk_factor"), starts_with("positive"), starts_with("cf_")
  ) %>% 
  mutate(hosp_los = as.numeric(hosp_los)) %>% 
  mutate(pull_date = as.Date("2024-5-9"))



df.season2 <- df.2425 %>%  
  mutate(collection_season = "24/25") %>%
  dplyr::select(collection_season, collection_date, rsv_mab, rsv_mab_dose, rsv_mab_2324, rsv_mab_2425, rsv_mab_season, mab_date,
                encounter_type, Sex,
                icu_admitted, hosp_los_days, icu_los_days,
                race_ethnicity, insurance_type, birth_weight, # highflow_oxygen,
                starts_with("days_btw_mab_collection"), weeks_btw_mab_collection_cat,
                starts_with("month_when_tested"), starts_with("age_at_test"),
                starts_with("symp"), starts_with("risk_factor"), starts_with("positive"), starts_with("cf_")
  ) %>% 
  mutate(Sex = case_when(Sex == "F" ~ "Female",
                         Sex == "M" ~ "Male", 
                         TRUE ~ Sex)) %>%
  rename(hosp_los = hosp_los_days)


# for 24/25 season, also add column "pull_date": indicate date of data pulling (which will be time point for analysis in the shiny app)
df.season2 <- df.season2 %>%
  mutate(
    first_day = floor_date(collection_date, "month"),                # First day of the month
    fifteenth = first_day + days(14),                               # 15th of the month
    last_day = ceiling_date(collection_date, "month") - days(1),    # Last day of the month
    pull_date = case_when(
      collection_date >= first_day & collection_date <= fifteenth ~ fifteenth,
      collection_date > fifteenth & collection_date <= last_day ~ last_day,
      TRUE ~ as.Date(NA)  # Optional: for dates that don't fit any rule
    )
  ) %>%
  select(-first_day, -fifteenth, -last_day)  # Remove intermediate columns



# combine two seasons (main data records)
df <- bind_rows(df.season1, df.season2)



##----------------------------------------------
# read in processed outputs
##----------------------------------------------

### main VE results
ve.main <- readRDS("../../data/realtime/shiny_inputs/ve.main.rds")

### VE by gestational age 
ve.gest <- readRDS("../../data/realtime/shiny_inputs/ve.gest.rds")

### VE against other viruses
ve.otherviruses <- readRDS("../../data/realtime/shiny_inputs/ve.otherviruses.rds")

### waning VE
ve.wane.monotrend <- readRDS("../../data/realtime/shiny_inputs/df.ve.biwkinterval.monotrend.rds")
ve.wane.RW2 <- readRDS("../../data/realtime/shiny_inputs/df.ve.biwkinterval.RW2.rds")
ve.wane.notrend <- readRDS("../../data/realtime/shiny_inputs/df.ve.biwkinterval.notrend.rds")


### make a list of input data 
ve_datasets <- list("df" = df, 
                    "ve.main" = ve.main, 
                    "ve.gest" = ve.gest,
                    "ve.otherviruses" = ve.otherviruses,
                    "ve.wane.monotrend" = ve.wane.monotrend,
                    "ve.wane.RW2" = ve.wane.RW2,
                    "ve.wane.notrend" = ve.wane.notrend
)


# all the pull dates 
pull_dates_range <- sort(unique(df$pull_date))



# Suppress warnings globally
options(warn = -1)  

# Reset warning level when app exits
on.exit(options(warn = 0))

# Or wrap specific problematic sections with suppressWarnings
# suppressWarnings({
#   # Your data loading and processing code here
# })

####################################################################################
### UI function
####################################################################################
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f5f5f5;
        color: #333333;
      }
      .well {
        background-color: #ffffff;
        border: none;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        border-radius: 8px;
      }
      .title-panel {
        background-color: #8b0000;
        color: white;
        padding: 20px;
        margin-bottom: 20px;
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      .plot-container {
        background: white;
        padding: 20px;
        margin-bottom: 20px;
        border-radius: 8px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      .irs--shiny .irs-bar {
        background: #8b0000;
      }
      .irs--shiny .irs-single {
        background: #8b0000;
      }
      .irs--shiny .irs-handle {
        border-color: #8b0000;
      }
      h2 {
        color: #8b0000;
        margin-bottom: 20px;
      }
    "))
  ),
  
  div(class = "title-panel",
      h1("Effectiveness of nirsevimab against RSV outcomes (23/24-24/25)", style = "text-align: center; margin: 0;")
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3, 
      style = "background-color: white; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
      h2("Main Controls"),
      sliderTextInput("main_date", 
                      "Select data pulling date:",
                      choices = pull_dates_range,
                      selected = pull_dates_range[2],
                      grid = TRUE,
                      width = "100%"
      )
    ),
    
    # mainPanel(
    #   
    #   width = 9,
    #   ### collection date and immunization date
    #   div(class = "plot-container",
    #       h2("Collection date and Nirsevimab immunization date"),
    #       plotOutput("plot_collection_vax", height = "800px", width = "1200px"),
    #       sliderTextInput("date1", "Data pull date:",
    #                       choices = pull_dates_range,
    #                       selected = pull_dates_range[1],
    #                       grid = TRUE,
    #                       width = "100%"
    #       )
    #   ),
    #   
    #   ### Table 1:  Cases vs. controls
    #   div(class = "plot-container",
    #       h2("Table 1. Cases vs. controls (two seasons combined)"),
    #       gt_output("table_cases_controls"),
    #       sliderTextInput("date2", "Data pull date:",
    #                       choices = pull_dates_range,
    #                       selected = pull_dates_range[1],
    #                       grid = TRUE,
    #                       width = "100%"
    #       )
    #   ),
    #   
    #   ### Table 1: Cases vs. controls (2024-25 only)  
    #   div(class = "plot-container",
    #       h2("Table 1. Cases vs. controls (24/25 only)"),
    #       gt_output("table_cases_controls_2425"),
    #       sliderTextInput("date3", "Data pull date:",
    #                       choices = pull_dates_range,
    #                       selected = pull_dates_range[1],
    #                       grid = TRUE,
    #                       width = "100%"
    #       )
    #   ),
    #   
    #   
    #   ### Table 2: Clinical outcomes (two seasons)
    #   div(class = "plot-container",
    #       h2("Table 2. Clinical outcomes (two seasons combined)"),
    #       gt_output("table_clinical_outcomes"),
    #       sliderTextInput("date4", "Data pull date:",
    #                       choices = pull_dates_range,
    #                       selected = pull_dates_range[1],
    #                       grid = TRUE,
    #                       width = "100%"
    #       )
    #   ),
    #   
    #   ### Table 2: Clinical outcomes (24/25 only)
    #   div(class = "plot-container",
    #       h2("Table 2. Clinical outcomes (24/25 only)"),
    #       gt_output("table_clinical_outcomes_2425"),
    #       sliderTextInput("date5", "Data pull date:",
    #                       choices = pull_dates_range,
    #                       selected = pull_dates_range[1],
    #                       grid = TRUE,
    #                       width = "100%"
    #       )
    #   ),
    #   
    #   ### VE main (2 seasons)
    #   div(class = "plot-container",
    #       h2("VE (main outcomes, 2 seasons combined)"),
    #       plotOutput("plot_ve_main", height = "400px", width = "1100px"),
    #       sliderTextInput("date6", "Data pull date:",
    #                       choices = pull_dates_range,
    #                       selected = pull_dates_range[1],
    #                       grid = TRUE,
    #                       width = "100%"
    #       )
    #   ),
    #   
    #   ### VE by gest age (2 seasons)
    #   div(class = "plot-container",
    #       h2("VE (by gestational age, 2 seasons)"),
    #       plotOutput("plot_ve_gest", height = "400px", width = "1100px"),
    #       sliderTextInput("date7", "Data pull date:",
    #                       choices = pull_dates_range,
    #                       selected = pull_dates_range[1],
    #                       grid = TRUE,
    #                       width = "100%"
    #       )
    #   ),
    #   
    #   ### VE against other viruses (2 seasons)
    #   div(class = "plot-container",
    #       h2("VE against other viruses (2 seasons combined)"),
    #       plotOutput("plot_ve_otherviruses", height = "400px", width = "1100px"),
    #       sliderTextInput("date8", "Data pull date:",
    #                       choices = pull_dates_range,
    #                       selected = pull_dates_range[1],
    #                       grid = TRUE,
    #                       width = "100%"
    #       )
    #   ),
    #   
    #   ### VE waning  (2 seasons, no trend)
    #   div(class = "plot-container",
    #       h2("VE waning (imposing monotonic trend, 2 seasons combined)"),
    #       plotOutput("plot_ve_wane_monotrend", height = "800px", width = "1200px"),
    #       sliderTextInput("date9", "Data pull date:",
    #                       choices = pull_dates_range,
    #                       selected = pull_dates_range[1],
    #                       grid = TRUE,
    #                       width = "100%"
    #       )
    #   )
    #   
    #   
    #   
    #   
    #   
    # ) 
    
    mainPanel(
      width = 9,
      tabsetPanel(
        # Tab 1: Collection and Immunization
        tabPanel("Data collection",
                 div(class = "plot-container",
                     h2("RSV test record collection date and Nirsevimab immunization date"),
                     plotOutput("plot_collection_vax", height = "600px", width = "1000px"),
                     sliderTextInput("date1", "Data pull date:",
                                     choices = pull_dates_range,
                                     selected = pull_dates_range[2],
                                     grid = TRUE,
                                     width = "100%"
                     )
                 )
        ),
        
        # Tab 2: Cases vs Controls Tables
        tabPanel("Table 1. Cases vs Controls",
                 div(class = "plot-container",
                     h2("Table 1. Cases vs. controls (two seasons combined)"),
                     gt_output("table_cases_controls"),
                     sliderTextInput("date2", "Data pull date:",
                                     choices = pull_dates_range,
                                     selected = pull_dates_range[2],
                                     grid = TRUE,
                                     width = "100%"
                     )
                 ),
                 div(class = "plot-container",
                     h2("Table 1. Cases vs. controls (24/25 only)"),
                     gt_output("table_cases_controls_2425"),
                     sliderTextInput("date3", "Data pull date:",
                                     choices = pull_dates_range,
                                     selected = pull_dates_range[2],
                                     grid = TRUE,
                                     width = "100%"
                     )
                 )
        ),
        
        # Tab 3: Clinical Outcomes
        tabPanel("Table 2. Clinical Outcomes",
                 div(class = "plot-container",
                     h2("Clinical outcomes (two seasons)"),
                     gt_output("table_clinical_outcomes"),
                     sliderTextInput("date4", "Data pull date:",
                                     choices = pull_dates_range,
                                     selected = pull_dates_range[2],
                                     grid = TRUE,
                                     width = "100%"
                     )
                 ),
                 div(class = "plot-container",
                     h2("Clinical outcomes (24/25 only)"),
                     gt_output("table_clinical_outcomes_2425"),
                     sliderTextInput("date5", "Data pull date:",
                                     choices = pull_dates_range,
                                     selected = pull_dates_range[2],
                                     grid = TRUE,
                                     width = "100%"
                     )
                 )
        ),
        
        # Tab 4: VE Analysis
        tabPanel("VE Analysis",
                 div(class = "plot-container",
                     h2("VE (main outcomes)"),
                     plotOutput("plot_ve_main", height = "400px", width = "1100px"),
                     sliderTextInput("date6", "Data pull date:",
                                     choices = pull_dates_range,
                                     selected = pull_dates_range[2],
                                     grid = TRUE,
                                     width = "100%"
                     )
                 ),
                 div(class = "plot-container",
                     h2("VE (by gestational age)"),
                     plotOutput("plot_ve_gest", height = "400px", width = "1100px"),
                     sliderTextInput("date7", "Data pull date:",
                                     choices = pull_dates_range,
                                     selected = pull_dates_range[2],
                                     grid = TRUE,
                                     width = "100%"
                     )
                 ),
                 div(class = "plot-container",
                     h2("VE against other viruses"),
                     plotOutput("plot_ve_otherviruses", height = "400px", width = "1100px"),
                     sliderTextInput("date8", "Data pull date:",
                                     choices = pull_dates_range,
                                     selected = pull_dates_range[2],
                                     grid = TRUE,
                                     width = "100%"
                     )
                 )
        ),
        
        # Tab 5: VE Waning
        tabPanel("VE Waning",
                 div(class = "plot-container",
                     h2("VE waning analysis (Imposing monotonic trend)"),
                     plotOutput("plot_ve_wane_monotrend", height = "800px", width = "1200px"),
                     sliderTextInput("date9", "Data pull date:",
                                     choices = pull_dates_range,
                                     selected = pull_dates_range[2],
                                     grid = TRUE,
                                     width = "100%"
                     )
                 ), 
                 
                 div(class = "plot-container",
                     h2("VE waning analysis (RW2)"),
                     plotOutput("plot_ve_wane_RW2", height = "800px", width = "1200px"),
                     sliderTextInput("date10", "Data pull date:",
                                     choices = pull_dates_range,
                                     selected = pull_dates_range[2],
                                     grid = TRUE,
                                     width = "100%"
                     )
                 )
                 
        )
      )
    )
    
    
    
  )
)

####################################################################################
### server function
####################################################################################
server <- function(input, output, session) {
  
  # Theme for all plots
  custom_theme <- theme_minimal() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(color = "#8b0000", size = 14, face = "bold"),
      axis.title = element_text(color = "#333333"),
      axis.text = element_text(color = "#666666")
    )
  
  ### synchronize all the pull_date in all plots
  # Observe main date control changes
  observeEvent(input$main_date, {
    updateSliderTextInput(session, "date1", selected = input$main_date)
    updateSliderTextInput(session, "date2", selected = input$main_date)
    updateSliderTextInput(session, "date3", selected = input$main_date)
    updateSliderTextInput(session, "date4", selected = input$main_date)
    updateSliderTextInput(session, "date5", selected = input$main_date)
    updateSliderTextInput(session, "date6", selected = input$main_date)
    updateSliderTextInput(session, "date7", selected = input$main_date)
    updateSliderTextInput(session, "date8", selected = input$main_date)
    updateSliderTextInput(session, "date9", selected = input$main_date)
    updateSliderTextInput(session, "date10", selected = input$main_date)

  })
  # 
  # # Observe individual slider changes
  # observeEvent(input$date1, {
  #   updateSliderTextInput(session, "main_date", selected = input$date1)
  #   updateSliderTextInput(session, "date2", selected = input$date1)
  #   updateSliderTextInput(session, "date3", selected = input$date1)
  #   updateSliderTextInput(session, "date4", selected = input$date1)
  #   updateSliderTextInput(session, "date5", selected = input$date1)
  #   updateSliderTextInput(session, "date6", selected = input$date1)
  #   updateSliderTextInput(session, "date7", selected = input$date1)
  #   updateSliderTextInput(session, "date8", selected = input$date1)
  #   updateSliderTextInput(session, "date9", selected = input$date1)
  #   
  # })
  # 
  # observeEvent(input$date2, {
  #   updateSliderTextInput(session, "main_date", selected = input$date2)
  #   updateSliderTextInput(session, "date1", selected = input$date2)
  #   updateSliderTextInput(session, "date3", selected = input$date2)
  #   updateSliderTextInput(session, "date4", selected = input$date2)
  #   updateSliderTextInput(session, "date5", selected = input$date2)
  #   updateSliderTextInput(session, "date6", selected = input$date2)
  #   updateSliderTextInput(session, "date7", selected = input$date2)
  #   updateSliderTextInput(session, "date8", selected = input$date2)
  #   updateSliderTextInput(session, "date9", selected = input$date2)
  # })
  # 
  # observeEvent(input$date3, {
  #   updateSliderTextInput(session, "main_date", selected = input$date3)
  #   updateSliderTextInput(session, "date1", selected = input$date3)
  #   updateSliderTextInput(session, "date2", selected = input$date3)
  #   updateSliderTextInput(session, "date4", selected = input$date3)
  #   updateSliderTextInput(session, "date5", selected = input$date3)
  #   updateSliderTextInput(session, "date6", selected = input$date3)
  #   updateSliderTextInput(session, "date7", selected = input$date3)
  #   updateSliderTextInput(session, "date8", selected = input$date3)
  #   updateSliderTextInput(session, "date9", selected = input$date3)
  #   
  # })
  # 
  # observeEvent(input$date4, {
  #   updateSliderTextInput(session, "main_date", selected = input$date4)
  #   updateSliderTextInput(session, "date1", selected = input$date4)
  #   updateSliderTextInput(session, "date2", selected = input$date4)
  #   updateSliderTextInput(session, "date3", selected = input$date4)
  #   updateSliderTextInput(session, "date5", selected = input$date4)
  #   updateSliderTextInput(session, "date6", selected = input$date4)
  #   updateSliderTextInput(session, "date7", selected = input$date4)
  #   updateSliderTextInput(session, "date8", selected = input$date4)
  #   updateSliderTextInput(session, "date9", selected = input$date4)
  # })
  # 
  # observeEvent(input$date5, {
  #   updateSliderTextInput(session, "main_date", selected = input$date5)
  #   updateSliderTextInput(session, "date1", selected = input$date5)
  #   updateSliderTextInput(session, "date2", selected = input$date5)
  #   updateSliderTextInput(session, "date3", selected = input$date5)
  #   updateSliderTextInput(session, "date4", selected = input$date5)
  #   updateSliderTextInput(session, "date6", selected = input$date5)
  #   updateSliderTextInput(session, "date7", selected = input$date5)
  #   updateSliderTextInput(session, "date8", selected = input$date5)
  #   updateSliderTextInput(session, "date9", selected = input$date5)
  #   
  # })
  # 
  # observeEvent(input$date6, {
  #   updateSliderTextInput(session, "main_date", selected = input$date6)
  #   updateSliderTextInput(session, "date1", selected = input$date6)
  #   updateSliderTextInput(session, "date2", selected = input$date6)
  #   updateSliderTextInput(session, "date3", selected = input$date6)
  #   updateSliderTextInput(session, "date4", selected = input$date6)
  #   updateSliderTextInput(session, "date5", selected = input$date6)
  #   updateSliderTextInput(session, "date7", selected = input$date6)
  #   updateSliderTextInput(session, "date8", selected = input$date6)
  #   updateSliderTextInput(session, "date9", selected = input$date6)
  #   
  # })
  # 
  # observeEvent(input$date7, {
  #   updateSliderTextInput(session, "main_date", selected = input$date7)
  #   updateSliderTextInput(session, "date1", selected = input$date7)
  #   updateSliderTextInput(session, "date2", selected = input$date7)
  #   updateSliderTextInput(session, "date3", selected = input$date7)
  #   updateSliderTextInput(session, "date4", selected = input$date7)
  #   updateSliderTextInput(session, "date5", selected = input$date7)
  #   updateSliderTextInput(session, "date6", selected = input$date7)
  #   updateSliderTextInput(session, "date8", selected = input$date7)
  #   updateSliderTextInput(session, "date9", selected = input$date7)
  # })
  # 
  # observeEvent(input$date8, {
  #   updateSliderTextInput(session, "main_date", selected = input$date8)
  #   updateSliderTextInput(session, "date1", selected = input$date8)
  #   updateSliderTextInput(session, "date2", selected = input$date8)
  #   updateSliderTextInput(session, "date3", selected = input$date8)
  #   updateSliderTextInput(session, "date4", selected = input$date8)
  #   updateSliderTextInput(session, "date5", selected = input$date8)
  #   updateSliderTextInput(session, "date6", selected = input$date8)
  #   updateSliderTextInput(session, "date7", selected = input$date8)
  #   updateSliderTextInput(session, "date9", selected = input$date8)
  # })
  # 
  # observeEvent(input$date9, {
  #   updateSliderTextInput(session, "main_date", selected = input$date9)
  #   updateSliderTextInput(session, "date1", selected = input$date9)
  #   updateSliderTextInput(session, "date2", selected = input$date9)
  #   updateSliderTextInput(session, "date3", selected = input$date9)
  #   updateSliderTextInput(session, "date4", selected = input$date9)
  #   updateSliderTextInput(session, "date5", selected = input$date9)
  #   updateSliderTextInput(session, "date6", selected = input$date9)
  #   updateSliderTextInput(session, "date7", selected = input$date9)
  #   updateSliderTextInput(session, "date8", selected = input$date9)
  # })
  
  
  
  # ----------------------------------------------------
  # plt: collection date and vax date
  # ----------------------------------------------------
  output$plot_collection_vax <- renderPlot({
    data <- ve_datasets[["df"]]
    filtered_data <- data[data$pull_date <= as.Date(input$date1), ]
    max_collection_date <- max(filtered_data$collection_date, na.rm = T)
    
    plt.collection.date <- 
      filtered_data %>% 
      filter(collection_date >= as.Date("2023-10-2")) %>%
      mutate(positive_rsv = ifelse(positive_rsv == 1, "Case", "Control")) %>%
      mutate(positive_rsv = factor(positive_rsv, levels = c("Control", "Case"))) %>%
      ggplot() +
      geom_histogram(aes(x = collection_date, fill = factor(positive_rsv)), color = "black",
                     binwidth = 7) +
      scale_x_date(date_breaks = "1 month", date_labels = "%y-%m",
                   limits = as.Date(c("2023-9-1", max_collection_date))) +
      theme_classic() +
      xlab("Time of sample collection for RSV test") +
      ylab("Number of RSV test records")  +
      labs(fill = "Case status") +
      scale_fill_brewer(palette = "Set2") +
      geom_vline(xintercept = as.Date("2023-10-1"), linetype = "dashed")  +
      # geom_text(aes(x = as.Date("2023-10-5"), y = 140, vjust = 0.8,
      #               label = "Nirsevimab \nadministration\n started"),
      #           angle = 90, size = 5, fontface = "plain",
      #           check_overlap = TRUE, lineheight = 0.75) +
      # annotate("rect", fill = "grey", alpha = 0.5, xmin = as.Date("2024-5-12"), xmax = as.Date("2024-9-29"), ymin = -Inf, ymax = Inf) +
      #geom_text(aes(x = as.Date("2024-7-22"), y = 100, label = "N"), size = 5, check_overlap = TRUE) +
      theme(legend.position = "top", axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 14),
            axis.title = element_text(size = 15)) 
    
    
    plt.vax.time <- 
      filtered_data %>% 
      mutate(mab_date = as.Date(mab_date)) %>%
      ggplot() +
      geom_histogram(aes(x = mab_date), color = "black", fill = "white", binwidth = 7) +
      scale_x_date(date_breaks = "1 month", date_labels = "%y-%m") + 
      theme_classic() +
      xlab("Time of nirsevimab immunization") +
      ylab("Count")  +
      geom_vline(xintercept = as.Date("2023-10-1"), linetype = "dashed")  +
      # geom_text(aes(x = as.Date("2023-10-5"), y = 40, vjust = 0.8, lineheight = 0.75,
      #               label = "Nirsevimab \nadministration \nstarted"), 
      #           angle = 90, size = 5, fontface = "plain", check_overlap = TRUE) +
      scale_x_date(date_breaks = "1 month", date_labels = "%y-%m",
                   limits = as.Date(c("2023-9-1", max_collection_date))) +
      theme(legend.position = "top",
            axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 14),
            axis.title = element_text(size = 15))
    
    plot_grid(plt.collection.date, plt.vax.time,
              labels = c("A.", "B."), 
              ncol = 1, align = "v", rel_heights = c(4:3))
    
    
  }, height = function() {
    # Dynamic height logic
    return(600)  # returns height in pixels
  }, width = function() {
    # Dynamic width logic
    return(1000)  # returns width in pixels
  })
  
  
  # ------------------------------------------------------
  # Table 1. Cases vs. Controls (two seasons)
  # ------------------------------------------------------
  output$table_cases_controls <- render_gt({
    data <- ve_datasets[["df"]]
    filtered_data <- data[data$pull_date <= as.Date(input$date2), ]
    
    df_table1 <- filtered_data %>%
      # add dose of mab 
      mutate(rsv_mab = rsv_mab_dose) %>%
      mutate(rsv_mab = case_when((rsv_mab_2324 == 1 & !is.na(rsv_mab)) ~ "no mAb", 
                                 TRUE ~ rsv_mab)) %>%
      mutate(risk_factor_gestagelessthan37wks = case_when(
        risk_factor_gestagelessthan37wks == 1 ~ "Less than 37 weeks",
        risk_factor_gestagelessthan37wks == 0 ~ "37 weeks or more"
      )) %>% 
      dplyr::select(collection_season, 
                    positive_rsv, Sex, age_at_test_in_months, age_at_test_in_months_cat_2, race_ethnicity, birth_weight, insurance_type, rsv_mab, month_when_tested_cat, encounter_type, 
                    # risk factors (main ones that are >= 5% prevalence)
                    risk_factor_pulmonary, risk_factor_cardiac, risk_factor_asthma, risk_factor_anemia,
                    risk_factor_gestagelessthan37wks,
                    risk_factor_atleastone,
                    symp_LRT_distress) %>% 
      mutate(positive_rsv = case_when(positive_rsv == 1 ~ "Cases",
                                      positive_rsv == 0 ~ "Controls")) %>%
      # to display the missing, making the NA value explicit level of factor 
      mutate(across(c(positive_rsv,
                      Sex, race_ethnicity, insurance_type, rsv_mab, 
                      risk_factor_atleastone, risk_factor_gestagelessthan37wks), ~ factor(.) %>% forcats::fct_explicit_na())) %>% 
      # re-rank variables 
      dplyr::select(collection_season, 
                    positive_rsv, 
                    Sex, age_at_test_in_months, age_at_test_in_months_cat_2, 
                    month_when_tested_cat,
                    race_ethnicity, 
                    birth_weight,
                    risk_factor_gestagelessthan37wks,
                    risk_factor_pulmonary, risk_factor_cardiac, risk_factor_anemia,
                    risk_factor_atleastone,  insurance_type, rsv_mab, 
                    encounter_type, 
                    symp_LRT_distress)
    
    df_table1_infection <- df_table1 %>% dplyr::select(-encounter_type, -symp_LRT_distress)
    
    # footnotes 
    footnote_otherraces <- "Inclusing Asian, Pacific Islander, Middle Eastern or Northern American, American Indian or Native American by self-reporting."
    footnote_atleastoneriskfactor <- "Have at least one of the following conditions recorded in the infant's medical history or diagnosis records: 1) Anemia; 2) Immunodeficiency (e.g. transplantation history, leukemia, etc.); 3) Cardiac diseases (including congenital heart diseases diagnosed at birth or any reporting of heart conditions); 4) Pulmonary diseases; 5) Down syndrome; 6) Small for gestational age (birth weight < 2,500 grams); 7) Prematurity (gestational age less than 37 weeks)."
    
    
    table1_infection <- df_table1_infection %>% 
      tbl_summary(by = c(positive_rsv),
                  
                  type = list(all_continuous() ~ "continuous2"),       # indicate that you want to print multiple statistics 
                  statistic = list(all_continuous() ~ c("{mean} ({sd})", 
                                                        "{median} ({p25}, {p75})",
                                                        "{N_miss} ({p_miss}%)"),                 
                                   all_categorical() ~ "{n} ({p}%)"),
                  
                  digits = list(all_categorical() ~ c(0, 1),
                                all_continuous() ~ 1),
                  
                  label  = list(  
                    Sex ~ "Sex", 
                    age_at_test_in_months ~ "Age at testing (months)",
                    age_at_test_in_months_cat_2 ~ "Age at testing",
                    month_when_tested_cat ~ "Time tested", 
                    race_ethnicity ~ "Race and ethnicity",
                    risk_factor_pulmonary ~ "Pulmonary diseases", 
                    risk_factor_cardiac ~ "Cardiac diseases", 
                    risk_factor_anemia ~ "Anemia",
                    risk_factor_gestagelessthan37wks ~ "Gestational age",
                    risk_factor_atleastone ~ "Having at least one risk factor",
                    insurance_type ~ "Insurance type",
                    rsv_mab ~ "mAb status",
                    birth_weight ~ "Birth weight"
                  ),
                  
                  missing_text = "Missing"
                  #,
                  #     percent = "row"
                  #      missing= "no"
      ) %>% 
      #add_difference(everything() ~ "smd") %>%
      add_p(pvalue_fun = ~ style_pvalue(., digits = 3)) %>% 
      add_overall(last = F) %>%
      bold_labels() %>%
      modify_table_styling(
        columns = label,
        rows = label == "Other non-Hispanic",
        footnote = footnote_otherraces
      ) %>%
      modify_table_styling(
        columns = label,
        rows = label == "Having at least one risk factor",
        footnote = footnote_atleastoneriskfactor
      ) %>%
      as_gt() %>%
      opt_row_striping() %>%
      tab_style(
        style = cell_text(color = "#8b0000", weight = "bold"),
        locations = cells_title()
      )

  })
  
  
  # ------------------------------------------------------
  # Table 1. Cases vs. Controls (24/25 only)
  # ------------------------------------------------------
  output$table_cases_controls_2425 <- render_gt({
    data <- ve_datasets[["df"]]
    filtered_data <- data[data$pull_date <= as.Date(input$date3), ]
    
    df_table1_2425 <- filtered_data %>%
      filter(collection_season == "24/25") %>%
      # add dose of mab 
      mutate(rsv_mab = rsv_mab_dose) %>%
      mutate(rsv_mab = case_when((rsv_mab_2324 == 1 & !is.na(rsv_mab)) ~ "no mAb", 
                                 TRUE ~ rsv_mab)) %>%
      mutate(risk_factor_gestagelessthan37wks = case_when(
        risk_factor_gestagelessthan37wks == 1 ~ "Less than 37 weeks",
        risk_factor_gestagelessthan37wks == 0 ~ "37 weeks or more"
      )) %>% 
      dplyr::select(positive_rsv, Sex, age_at_test_in_months, age_at_test_in_months_cat_2, race_ethnicity, birth_weight, insurance_type, rsv_mab, month_when_tested_cat, encounter_type, 
                    # risk factors (main ones that are >= 5% prevalence)
                    risk_factor_pulmonary, risk_factor_cardiac, risk_factor_asthma, risk_factor_anemia,
                    risk_factor_gestagelessthan37wks,
                    risk_factor_atleastone,
                    symp_LRT_distress) %>% 
      mutate(positive_rsv = case_when(positive_rsv == 1 ~ "Cases",
                                      positive_rsv == 0 ~ "Controls")) %>%
      # to display the missing, making the NA value explicit level of factor 
      mutate(across(c(positive_rsv,
                      Sex, race_ethnicity, insurance_type, rsv_mab, 
                      risk_factor_atleastone, risk_factor_gestagelessthan37wks), ~ factor(.) %>% forcats::fct_explicit_na())) %>% 
      # re-rank variables 
      dplyr::select(positive_rsv, 
                    Sex, age_at_test_in_months, age_at_test_in_months_cat_2, 
                    month_when_tested_cat,
                    race_ethnicity, 
                    birth_weight,
                    risk_factor_gestagelessthan37wks,
                    risk_factor_pulmonary, risk_factor_cardiac, risk_factor_anemia,
                    risk_factor_atleastone,  insurance_type, rsv_mab, 
                    encounter_type, 
                    symp_LRT_distress)
    
    df_table1_infection_2425 <- df_table1_2425 %>% dplyr::select(-encounter_type, -symp_LRT_distress)
    
    # footnotes 
    footnote_otherraces <- "Inclusing Asian, Pacific Islander, Middle Eastern or Northern American, American Indian or Native American by self-reporting."
    footnote_atleastoneriskfactor <- "Have at least one of the following conditions recorded in the infant's medical history or diagnosis records: 1) Anemia; 2) Immunodeficiency (e.g. transplantation history, leukemia, etc.); 3) Cardiac diseases (including congenital heart diseases diagnosed at birth or any reporting of heart conditions); 4) Pulmonary diseases; 5) Down syndrome; 6) Small for gestational age (birth weight < 2,500 grams); 7) Prematurity (gestational age less than 37 weeks)."
    
    
    table1_infection_2425 <- df_table1_infection_2425 %>% 
      tbl_summary(by = c(positive_rsv),
                  
                  type = list(all_continuous() ~ "continuous2"),       # indicate that you want to print multiple statistics 
                  statistic = list(all_continuous() ~ c("{mean} ({sd})", 
                                                        "{median} ({p25}, {p75})",
                                                        "{N_miss} ({p_miss}%)"),                 
                                   all_categorical() ~ "{n} ({p}%)"),
                  
                  digits = list(all_categorical() ~ c(0, 1),
                                all_continuous() ~ 1),
                  
                  label  = list(  
                    Sex ~ "Sex", 
                    age_at_test_in_months ~ "Age at testing (months)",
                    age_at_test_in_months_cat_2 ~ "Age at testing",
                    month_when_tested_cat ~ "Time tested", 
                    race_ethnicity ~ "Race and ethnicity",
                    risk_factor_pulmonary ~ "Pulmonary diseases", 
                    risk_factor_cardiac ~ "Cardiac diseases", 
                    risk_factor_anemia ~ "Anemia",
                    risk_factor_gestagelessthan37wks ~ "Gestational age",
                    risk_factor_atleastone ~ "Having at least one risk factor",
                    insurance_type ~ "Insurance type",
                    rsv_mab ~ "mAb status",
                    birth_weight ~ "Birth weight"
                  ),
                  
                  missing_text = "Missing"
                  #,
                  #     percent = "row"
                  #      missing= "no"
      ) %>% 
      #add_difference(everything() ~ "smd") %>%
      add_p(pvalue_fun = ~ style_pvalue(., digits = 3)) %>% 
      add_overall(last = F) %>%
      bold_labels() %>%
      modify_table_styling(
        columns = label,
        rows = label == "Other non-Hispanic",
        footnote = footnote_otherraces
      ) %>%
      modify_table_styling(
        columns = label,
        rows = label == "Having at least one risk factor",
        footnote = footnote_atleastoneriskfactor
      ) %>%
      as_gt() %>%
      opt_row_striping() %>%
      tab_style(
        style = cell_text(color = "#8b0000", weight = "bold"),
        locations = cells_title()
      )
    
  })
  
  
  
  # ------------------------------------------------------
  # Table 2. Clinical outcomes
  # ------------------------------------------------------
  output$table_clinical_outcomes <- render_gt({
    data <- ve_datasets[["df"]]
    filtered_data <- data[data$pull_date <= as.Date(input$date4), ]
    
    df_table1_outcome <- filtered_data %>%
      mutate(rsv_mab = ifelse(rsv_mab == 1, "Vaccinated", "Unvaccinated")) %>%
      mutate(encounter_type = case_when(encounter_type %in% c("inpatient_unrelated", "outpatient") ~ "no",
                                        encounter_type == "inpatient" ~ "yes")) %>%
      mutate(icu_admitted = case_when(icu_admitted %in% c("yes_unrelated", "no") ~ "no",
                                      TRUE ~ "yes")) %>%
      mutate(hosp_los = ifelse(encounter_type == "no", NA, hosp_los)) %>%
      mutate(icu_los_days = ifelse(icu_admitted == "no", NA, icu_los_days)) %>%
      # mutate(highflow_oxygen = case_when(highflow_oxygen == 1 ~ "yes",
      #                                    TRUE ~ "no")) %>% 
      mutate(across(starts_with("symp_"), ~ ifelse(is.na(.), "no", "yes"))) %>%
      mutate(positive_rsv = case_when(positive_rsv == 1 ~ "RSV+",
                                      positive_rsv == 0 ~ "RSV-")) %>%
      # to display the missing, making the NA value explicit level of factor 
      mutate(across(c(positive_rsv,
                      rsv_mab), ~ factor(.) %>% forcats::fct_explicit_na())) %>% 
      # re-rank variables 
      dplyr::select(rsv_mab, positive_rsv, 
                    encounter_type, hosp_los, icu_admitted, icu_los_days, #highflow_oxygen,
                    starts_with("symp_")) %>%
      filter(positive_rsv == "RSV+") %>% 
      dplyr::select(-positive_rsv)
    
    # footnotes 
    footnote_otherraces <- "Inclusing Asian, Pacific Islander, Middle Eastern or Northern American, American Indian or Native American by self-reporting."
    footnote_atleastoneriskfactor <- "Have at least one of the following conditions recorded in the infant's medical history or diagnosis records: 1) Anemia; 2) Immunodeficiency (e.g. transplantation history, leukemia, etc.); 3) Cardiac diseases (including congenital heart diseases diagnosed at birth or any reporting of heart conditions); 4) Pulmonary diseases; 5) Down syndrome; 6) Small for gestational age (birth weight < 2,500 grams); 7) Prematurity (gestational age less than 37 weeks)."
    
    
    
    table1_outcome <- df_table1_outcome %>% 
      tbl_summary(by = rsv_mab,
                  
                  type = list(all_continuous() ~ "continuous2"),       # indicate that you want to print multiple statistics 
                  statistic = list(all_continuous() ~ c("{median} ({p25}, {p75})",
                                                        "{N_miss} ({p_miss}%)"),                 
                                   all_categorical() ~ "{n} ({p}%)"),
                  
                  digits = list(all_categorical() ~ c(0, 1),
                                all_continuous() ~ 1),
                  
                  label  = list(  
                    encounter_type = "Hospital admission",
                    hosp_los = "Duration of hospitalization (days)",
                    icu_admitted = "ICU admission",
                    icu_los_days = "Duration of ICU admission (days)",
                    # highflow_oxygen = "Required highflow oxygen support",
                    symp_URT_distress = "Distress in URT",
                    symp_LRT_distress = "Distress in LRT",
                    symp_fever = "Fever (> 38°C/100.4°F)",
                    symp_cough = "Cough",
                    symp_wheezing = "Wheezing",
                    symp_breathing = "Breathing difficulties",
                    symp_bronchiolitis = "Bronchiolitis",
                    symp_sepsis = "Sepsis"
                  ),
                  
                  missing_text = "Missing"
                  #,
                  #     percent = "row"
                  #      missing= "no"
      ) %>% 
      #add_difference(everything() ~ "smd") %>%
      #add_p(pvalue_fun = ~ style_pvalue(., digits = 3)) %>% 
      add_overall(last = F) %>%
      bold_labels() %>%
      modify_table_styling(
        columns = label,
        rows = label == "Other non-Hispanic",
        footnote = footnote_otherraces
      ) %>%
      modify_table_styling(
        columns = label,
        rows = label == "Having at least one risk factor",
        footnote = footnote_atleastoneriskfactor
      ) %>%
      as_gt() %>%
      opt_row_striping() %>%
      tab_style(
        style = cell_text(color = "#8b0000", weight = "bold"),
        locations = cells_title()
      )
    
  })
  
  
  # ------------------------------------------------------
  # Table 2. Clinical outcomes (24/25 only)
  # ------------------------------------------------------
  output$table_clinical_outcomes_2425 <- render_gt({
    data <- ve_datasets[["df"]]
    filtered_data <- data[data$pull_date <= as.Date(input$date5), ]
    
    df_table1_outcome_2425 <- filtered_data %>%
      filter(collection_season == "24/25") %>%
      mutate(rsv_mab = ifelse(rsv_mab == 1, "Vaccinated", "Unvaccinated")) %>%
      mutate(encounter_type = case_when(encounter_type %in% c("inpatient_unrelated", "outpatient") ~ "no",
                                        encounter_type == "inpatient" ~ "yes")) %>%
      mutate(icu_admitted = case_when(icu_admitted %in% c("yes_unrelated", "no") ~ "no",
                                      TRUE ~ "yes")) %>%
      mutate(hosp_los = ifelse(encounter_type == "no", NA, hosp_los)) %>%
      # mutate(icu_los_days = ifelse(icu_admitted == "no", NA, icu_los_days)) %>%
      # mutate(highflow_oxygen = case_when(highflow_oxygen == 1 ~ "yes",
      #                                    TRUE ~ "no")) %>% 
      mutate(across(starts_with("symp_"), ~ ifelse(is.na(.), "no", "yes"))) %>%
      mutate(positive_rsv = case_when(positive_rsv == 1 ~ "RSV+",
                                      positive_rsv == 0 ~ "RSV-")) %>%
      # to display the missing, making the NA value explicit level of factor 
      mutate(across(c(positive_rsv,
                      rsv_mab), ~ factor(.) %>% forcats::fct_explicit_na())) %>% 
      # re-rank variables 
      dplyr::select(rsv_mab, positive_rsv, 
                    encounter_type, hosp_los, icu_admitted, #icu_los_days, #highflow_oxygen,
                    starts_with("symp_")) %>%
      filter(positive_rsv == "RSV+") %>% 
      dplyr::select(-positive_rsv)
    
    # footnotes 
    footnote_otherraces <- "Inclusing Asian, Pacific Islander, Middle Eastern or Northern American, American Indian or Native American by self-reporting."
    footnote_atleastoneriskfactor <- "Have at least one of the following conditions recorded in the infant's medical history or diagnosis records: 1) Anemia; 2) Immunodeficiency (e.g. transplantation history, leukemia, etc.); 3) Cardiac diseases (including congenital heart diseases diagnosed at birth or any reporting of heart conditions); 4) Pulmonary diseases; 5) Down syndrome; 6) Small for gestational age (birth weight < 2,500 grams); 7) Prematurity (gestational age less than 37 weeks)."
    
    
    
    table1_outcome_2425 <- df_table1_outcome_2425 %>% 
      tbl_summary(by = rsv_mab,
                  
                  type = list(all_continuous() ~ "continuous2"),       # indicate that you want to print multiple statistics 
                  statistic = list(all_continuous() ~ c("{median} ({p25}, {p75})",
                                                        "{N_miss} ({p_miss}%)"),                 
                                   all_categorical() ~ "{n} ({p}%)"),
                  
                  digits = list(all_categorical() ~ c(0, 1),
                                all_continuous() ~ 1),
                  
                  label  = list(  
                    encounter_type = "Hospital admission",
                    hosp_los = "Duration of hospitalization (days)",
                    icu_admitted = "ICU admission",
                    # icu_los_days = "Duration of ICU admission (days)",
                    # highflow_oxygen = "Required highflow oxygen support",
                    symp_URT_distress = "Distress in URT",
                    symp_LRT_distress = "Distress in LRT",
                    symp_fever = "Fever (> 38°C/100.4°F)",
                    symp_cough = "Cough",
                    symp_wheezing = "Wheezing",
                    symp_breathing = "Breathing difficulties",
                    symp_bronchiolitis = "Bronchiolitis",
                    symp_sepsis = "Sepsis"
                  ),
                  
                  missing_text = "Missing"
                  #,
                  #     percent = "row"
                  #      missing= "no"
      ) %>% 
      #add_difference(everything() ~ "smd") %>%
      #add_p(pvalue_fun = ~ style_pvalue(., digits = 3)) %>% 
      add_overall(last = F) %>%
      bold_labels() %>%
      modify_table_styling(
        columns = label,
        rows = label == "Other non-Hispanic",
        footnote = footnote_otherraces
      ) %>%
      modify_table_styling(
        columns = label,
        rows = label == "Having at least one risk factor",
        footnote = footnote_atleastoneriskfactor
      ) %>%
      as_gt() %>%
      opt_row_striping() %>%
      tab_style(
        style = cell_text(color = "#8b0000", weight = "bold"),
        locations = cells_title()
      )
    
  })
  
  
  # ------------------------------------------------------
  # Main VE plot (2 seasons)
  # ------------------------------------------------------
  output$plot_ve_main <- renderPlot({
    data <- ve_datasets[["ve.main"]]
    filtered_data <- data[data$pull_date == as.Date(input$date6), ] %>% dplyr::select(-pull_date)
    
    ve.2seasons <- rbind(filtered_data[1,], filtered_data[3,], filtered_data[2,], filtered_data[4,], filtered_data[5,]) %>% 
      mutate(across(starts_with("n_"), ~str_replace(., " \\s*\\([^\\)]+\\)", ""))) 
  
    ve_forest_2rows(df.plot = ve.2seasons)
  
  }, height = function() {
    # Dynamic height logic
    return(400)  # returns height in pixels
  }, width = function() {
    # Dynamic width logic
    return(1100)  # returns width in pixels
  })
  
  # ------------------------------------------------------
  # VE by gestational age (2 seasons)
  # ------------------------------------------------------
  output$plot_ve_gest <- renderPlot({
    
    data <- ve_datasets[["ve.gest"]]
    filtered_data <- data[data$pull_date == as.Date(input$date7), ] %>% dplyr::select(-pull_date)
    
    ve.gest <- filtered_data %>% mutate(across(starts_with("n_"), ~str_replace(., " \\s*\\([^\\)]+\\)", "")))
    
    ve_forest_2rows(ve.gest, ssa = TRUE)
    
    
  }, height = function() {
    # Dynamic height logic
    return(400)  # returns height in pixels
  }, width = function() {
    # Dynamic width logic
    return(1100)  # returns width in pixels
  })
  
  # ------------------------------------------------------
  # VE against other viruses (2 seasons)
  # ------------------------------------------------------
  output$plot_ve_otherviruses <- renderPlot({
    
    data <- ve_datasets[["ve.otherviruses"]]
    filtered_data <- data[data$pull_date == as.Date(input$date8), ] %>% dplyr::select(-pull_date)
    
    ve.otherviruses <- filtered_data %>% mutate(across(starts_with("n_"), ~str_replace(., " \\s*\\([^\\)]+\\)", "")))
  
    ve_forest_2rows(ve.otherviruses, ssa = T)
    
  }, height = function() {
    # Dynamic height logic
    return(400)  # returns height in pixels
  }, width = function() {
    # Dynamic width logic
    return(1100)  # returns width in pixels
  })
  
  # ------------------------------------------------------
  # Waning VE (2 seasons, mono trend)
  # ------------------------------------------------------
  output$plot_ve_wane_monotrend <- renderPlot({
    
    data <- ve_datasets[["ve.wane.monotrend"]]
    filtered_data <- data[data$pull_date == as.Date(input$date9), ] %>% dplyr::select(-pull_date)
    
    df.plt.wane.biwk <- 
      filtered_data %>%
      mutate(time_since_vax = case_when(
        vax_time == "(0, 2)" ~ "2",
        vax_time == "[2, 4)" ~ "4",
        vax_time == "[4, 6)" ~ "6",
        vax_time == "[6, 8)" ~ "8",
        vax_time == "[8, 10)" ~ "10",
        vax_time == "[10, 12)" ~ "12",
        vax_time == "[12, 14)" ~ "14",
        vax_time == "[14, 16)" ~ "16",
        vax_time == "[16, )" ~ "16+"
      )) %>%
      mutate(VE = paste0(round(VE.median), " (", round(VE.lb), ", ", round(VE.ub), ")")) %>% 
      # mutate(time_since_vax = factor(time_since_vax, levels = c("(0, 2)", "[2, 4)", "[4, 6)", "[6, 8)",
      #                                                           "[8, 10)", "[10, 12)", "[12, 14)", "[14, 16)", "[16, )"))) %>% 
      mutate(time_since_vax = factor(time_since_vax, levels = c("2", "4", "6", "8",
                                                                "10", "12", "14", "16", "16+"))) %>% 
      mutate(against = paste0("Against ", against)) %>%
      mutate(against = factor(against, levels = c("Against Medically attended RSV infection",
                                                  "Against RSV outpatient visit",
                                                  "Against RSV hospitalization",
                                                  "Against RSV LRTI",
                                                  "Against RSV LRTI hospitalization",
                                                  "Against RSV severe outcomes")))
    
    df.no.arrow <- 
      df.plt.wane.biwk %>%
      mutate(add_arrow = ifelse(VE.lb <= -25, 1, 0)) %>%
      mutate(VE.lb = ifelse(add_arrow == 1, -25, VE.lb)) %>% 
      mutate(across(starts_with("VE"), ~ ifelse(add_arrow == 1, NA, .)))
    
    df.add.arrow <- 
      df.plt.wane.biwk %>%
      mutate(add_arrow = ifelse(VE.lb <= -25, 1, 0)) %>%
      mutate(VE.lb = ifelse(add_arrow == 1, -25, VE.lb)) %>% 
      mutate(across(starts_with("VE"), ~ ifelse(add_arrow != 1, NA, .)))
    
    
    
    plt.jags.wane.biwk <- 
      ggplot() +
      # if lb > -25
      geom_point(aes(x = time_since_vax, y = VE.median), 
                 position = position_dodge(0.5), data = df.no.arrow) +
      geom_errorbar(aes(x = time_since_vax, ymin = VE.lb, ymax = VE.ub), 
                    width = 0, position = position_dodge(0.5), data = df.no.arrow) +
      geom_text(aes(x = time_since_vax, y = VE.median-10, label = VE), 
                angle = 90, position = position_dodge(0.5), vjust = - 0.7, show_guide = F,
                size = 4.3, data = df.no.arrow) +
      # if lb < -25
      geom_point(aes(x = time_since_vax, y = VE.median), 
                 position = position_dodge(0.5), data = df.add.arrow) +
      geom_segment(aes(x = time_since_vax, xend = time_since_vax, 
                       y = VE.ub, yend = VE.lb),
                   arrow = arrow(length = unit(2, "mm")),
                   position = position_dodge(0.5),
                   data = df.add.arrow, show_guide = F) +
      geom_text(aes(x = time_since_vax, y = VE.median-10, label = VE), 
                angle = 90, position = position_dodge(0.5), vjust = - 0.7, show_guide = F,
                size = 4.3, data = df.add.arrow) +
      theme_classic() +
      scale_color_brewer(palette = "Dark2") +
      geom_hline(yintercept = 0, linetype = "dashed") +
      ylab("Effectiveness of Nirsevimab (%)") +
      xlab("Time since immunization (weeks)") +
      labs(color = "Clinical endpoint") +
      ylim(c(-25,100)) +
      scale_y_continuous(breaks = c(-25, 0, 25, 50, 75, 100),
                         labels = c("-25", "0", "25", "50", "75", "100")) +
      facet_wrap( ~ against) +
      theme(
        #panel.grid.major = element_line(color = "grey", linewidth = 0.1), 
        #panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        strip.text.x = element_text(size = 12), 
        strip.text.y = element_text(size = 11)
      )
    
    plt.jags.wane.biwk
    
  }, height = function() {
    # Dynamic height logic
    return(800)  # returns height in pixels
  }, width = function() {
    # Dynamic width logic
    return(1200)  # returns width in pixels
  })
  
  # ------------------------------------------------------
  # Waning VE (2 seasons, RW2 trend)
  # ------------------------------------------------------
  output$plot_ve_wane_RW2 <- renderPlot({
    
    data <- ve_datasets[["ve.wane.RW2"]]
    filtered_data <- data[data$pull_date == as.Date(input$date10), ] %>% dplyr::select(-pull_date)
    
    df.plt.wane.biwk <- 
      filtered_data %>%
      mutate(time_since_vax = case_when(
        vax_time == "(0, 2)" ~ "2",
        vax_time == "[2, 4)" ~ "4",
        vax_time == "[4, 6)" ~ "6",
        vax_time == "[6, 8)" ~ "8",
        vax_time == "[8, 10)" ~ "10",
        vax_time == "[10, 12)" ~ "12",
        vax_time == "[12, 14)" ~ "14",
        vax_time == "[14, 16)" ~ "16",
        vax_time == "[16, )" ~ "16+"
      )) %>%
      mutate(VE = paste0(round(VE.median), " (", round(VE.lb), ", ", round(VE.ub), ")")) %>% 
      # mutate(time_since_vax = factor(time_since_vax, levels = c("(0, 2)", "[2, 4)", "[4, 6)", "[6, 8)",
      #                                                           "[8, 10)", "[10, 12)", "[12, 14)", "[14, 16)", "[16, )"))) %>% 
      mutate(time_since_vax = factor(time_since_vax, levels = c("2", "4", "6", "8",
                                                                "10", "12", "14", "16", "16+"))) %>% 
      mutate(against = paste0("Against ", against)) %>%
      mutate(against = factor(against, levels = c("Against Medically attended RSV infection",
                                                  "Against RSV outpatient visit",
                                                  "Against RSV hospitalization",
                                                  "Against RSV LRTI",
                                                  "Against RSV LRTI hospitalization",
                                                  "Against RSV severe outcomes")))
    
    df.no.arrow <- 
      df.plt.wane.biwk %>%
      mutate(add_arrow = ifelse(VE.lb <= -25, 1, 0)) %>%
      mutate(VE.lb = ifelse(add_arrow == 1, -25, VE.lb)) %>% 
      mutate(across(starts_with("VE"), ~ ifelse(add_arrow == 1, NA, .)))
    
    df.add.arrow <- 
      df.plt.wane.biwk %>%
      mutate(add_arrow = ifelse(VE.lb <= -25, 1, 0)) %>%
      mutate(VE.lb = ifelse(add_arrow == 1, -25, VE.lb)) %>% 
      mutate(across(starts_with("VE"), ~ ifelse(add_arrow != 1, NA, .)))
    
    
    
    plt.jags.wane.biwk <- 
      ggplot() +
      # if lb > -25
      geom_point(aes(x = time_since_vax, y = VE.median), 
                 position = position_dodge(0.5), data = df.no.arrow) +
      geom_errorbar(aes(x = time_since_vax, ymin = VE.lb, ymax = VE.ub), 
                    width = 0, position = position_dodge(0.5), data = df.no.arrow) +
      geom_text(aes(x = time_since_vax, y = VE.median-10, label = VE), 
                angle = 90, position = position_dodge(0.5), vjust = - 0.7, show_guide = F,
                size = 4.3, data = df.no.arrow) +
      # if lb < -25
      geom_point(aes(x = time_since_vax, y = VE.median), 
                 position = position_dodge(0.5), data = df.add.arrow) +
      geom_segment(aes(x = time_since_vax, xend = time_since_vax, 
                       y = VE.ub, yend = VE.lb),
                   arrow = arrow(length = unit(2, "mm")),
                   position = position_dodge(0.5),
                   data = df.add.arrow, show_guide = F) +
      geom_text(aes(x = time_since_vax, y = VE.median-10, label = VE), 
                angle = 90, position = position_dodge(0.5), vjust = - 0.7, show_guide = F,
                size = 4.3, data = df.add.arrow) +
      theme_classic() +
      scale_color_brewer(palette = "Dark2") +
      geom_hline(yintercept = 0, linetype = "dashed") +
      ylab("Effectiveness of Nirsevimab (%)") +
      xlab("Time since immunization (weeks)") +
      labs(color = "Clinical endpoint") +
      ylim(c(-25,100)) +
      scale_y_continuous(breaks = c(-25, 0, 25, 50, 75, 100),
                         labels = c("-25", "0", "25", "50", "75", "100")) +
      facet_wrap( ~ against) +
      theme(
        #panel.grid.major = element_line(color = "grey", linewidth = 0.1), 
        #panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 12),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        strip.text.x = element_text(size = 12), 
        strip.text.y = element_text(size = 11)
      )
    
    plt.jags.wane.biwk
    
  }, height = function() {
    # Dynamic height logic
    return(800)  # returns height in pixels
  }, width = function() {
    # Dynamic width logic
    return(1200)  # returns width in pixels
  })
  
  
  
  
  
  
}

shinyApp(ui = ui, server = server)
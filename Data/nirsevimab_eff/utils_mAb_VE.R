
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(patchwork)
library(cowplot)

###############################
# some processing of dataset
###############################

process_df <- function(df){
  
  # add/recode some variables 
  
  # month when tested and age when tested 
  df <- df %>% 
    mutate(month_when_tested = as.character(month(collection_date, label = T))) %>% 
    mutate(month_when_tested = ifelse(month_when_tested == "Sep", "Oct", month_when_tested)) %>%
    # age when tested (6m interval)
    mutate(age_at_test_in_months_cat = case_when(
      age_at_test_in_months < 6 ~ "under 6m",
      age_at_test_in_months >= 6 & age_at_test_in_months < 12 ~ "6-12m",
      age_at_test_in_months >= 12 ~ "above 1 yo" 
    )) %>%
    # age when tested (3m interval)
    mutate(age_at_test_in_months_cat_2 = case_when(
      age_at_test_in_months < 3 ~ "under 3m",
      age_at_test_in_months >= 3 & age_at_test_in_months < 6 ~ "3-6m",
      age_at_test_in_months >= 6 & age_at_test_in_months < 9 ~ "6-9m",
      age_at_test_in_months >= 9 & age_at_test_in_months < 12 ~ "9-12m",
      age_at_test_in_months >= 12 ~ "above 1 yo" 
    )) %>%
    mutate(risk_factor_atleastone = case_when(risk_factor_atleastone == 1 ~ "yes",
                                              risk_factor_atleastone == 0 ~ "no")) %>% 
    mutate(across(c(risk_factor_anemia, 
                    risk_factor_pulmonary,
                    risk_factor_cardiac,
                    risk_factor_immunodeficiency,
                    risk_factor_down,
                    #risk_factor_small_for_gestage # this has values of 1 0 NA
    ), ~ ifelse(is.na(.), "no", "yes"))) %>%
    mutate(age_at_test_in_months_cat = factor(age_at_test_in_months_cat,
                                              levels = c("under 6m",
                                                         "6-12m",
                                                         "above 1 yo"))) %>%
    mutate(age_at_test_in_months_cat_2 = factor(age_at_test_in_months_cat_2,
                                                levels = c("under 3m",
                                                           "3-6m",
                                                           "6-9m",
                                                           "9-12m",
                                                           "above 1 yo"))) %>% 
    mutate(race_ethnicity = factor(race_ethnicity,
                                   levels = c("Hispanic",
                                              "White non-Hispanic",
                                              "Black non-Hispanic",
                                              "Other non-Hispanic",
                                              "unknown")))
  
  # birth weight in grams (originally ounce)
  df <- df %>%  mutate(birth_weight = Birth.Wgt * 28.3)
  
  # classify time since mab to testing 
  df <- df %>% 
    # 2-month interval
    mutate(days_btw_mab_collection_cat = 
             case_when(days_btw_mab_collection < 0 & days_btw_mab_collection > -60 ~ "0-2 months", 
                       days_btw_mab_collection <= -60 & days_btw_mab_collection > -120 ~ "2-4 months",
                       days_btw_mab_collection <= -120 ~ "4 months +",
                       is.na(days_btw_mab_collection) | days_btw_mab_collection >= 0 ~ "no mAb")) %>%
    mutate(days_btw_mab_collection_cat = factor(days_btw_mab_collection_cat,
                                                levels = c("no mAb",
                                                           "0-2 months",
                                                           "2-4 months",
                                                           "4 months +"))) %>%
    # 3-months interval
    mutate(days_btw_mab_collection_cat_2 = 
             case_when(days_btw_mab_collection < 0 & days_btw_mab_collection > -90 ~ "0-3 months", 
                       days_btw_mab_collection <= -90 & days_btw_mab_collection > -190 ~ "3-6 months",
                       is.na(days_btw_mab_collection) | days_btw_mab_collection >= 0 ~ "no mAb")) %>%
    mutate(days_btw_mab_collection_cat_2 = factor(days_btw_mab_collection_cat_2,
                                                  levels = c("no mAb",
                                                             "0-3 months",
                                                             "3-6 months"))) %>%
    # 1 months interval
    mutate(days_btw_mab_collection_cat_3 = 
             case_when(days_btw_mab_collection < 0 & days_btw_mab_collection > -30 ~ "0-1 months", 
                       days_btw_mab_collection <= -30 & days_btw_mab_collection > -60 ~ "1-2 months",
                       days_btw_mab_collection <= -60 & days_btw_mab_collection > -90 ~ "2-3 months",
                       days_btw_mab_collection <= -90 & days_btw_mab_collection > -120 ~ "3-4 months",
                       days_btw_mab_collection <= -120 ~ "4 months +",
                       is.na(days_btw_mab_collection) | days_btw_mab_collection >= 0 ~ "no mAb")) %>%
    mutate(days_btw_mab_collection_cat_3 = factor(days_btw_mab_collection_cat_3,
                                                levels = c("no mAb",
                                                           "0-1 months",
                                                           "1-2 months",
                                                           "2-3 months",
                                                           "3-4 months",
                                                           "4 months +"))) %>%
    mutate(weeks_btw_mab_collection_cat = 
             case_when(
               days_btw_mab_collection < 0 & days_btw_mab_collection > -14 ~ "(0, 2)",
               days_btw_mab_collection <= -14 & days_btw_mab_collection > -14*2 ~ "[2, 4)",
               days_btw_mab_collection <= -14*2 & days_btw_mab_collection > -14*3 ~ "[4, 6)",
               days_btw_mab_collection <= -14*3 & days_btw_mab_collection > -14*4 ~ "[6, 8)",
               days_btw_mab_collection <= -14*4 & days_btw_mab_collection > -14*5 ~ "[8, 10)",
               days_btw_mab_collection <= -14*5 & days_btw_mab_collection > -14*6 ~ "[10, 12)",
               days_btw_mab_collection <= -14*6 & days_btw_mab_collection > -14*7 ~ "[12, 14)",
               days_btw_mab_collection <= -14*7 & days_btw_mab_collection > -14*8 ~ "[14, 16)",
               days_btw_mab_collection <= -14*8 ~ "[16, )",
               is.na(days_btw_mab_collection) | days_btw_mab_collection >= 0 ~ "no mAb"
             )) %>% 
    mutate(weeks_btw_mab_collection_cat = factor(weeks_btw_mab_collection_cat,
                                                 levels = c("no mAb",
                                                            "(0, 2)", "[2, 4)", "[4, 6)", "[6, 8)",
                                                            "[8, 10)", "[10, 12)", "[12, 14)", "[14, 16)",
                                                            "[16, )"
                                                            )))
    
    
  
  
  # group month_when_tested into fewer groups
  df <- df %>% 
    mutate(month_when_tested_cat = case_when(
      collection_date >= as.Date("2023-9-30") & collection_date <= as.Date("2023-11-30") ~ "Oct-Nov",
      collection_date >= as.Date("2023-12-1") & collection_date <= as.Date("2024-1-31") ~ "Dec-Jan",
      collection_date >= as.Date("2024-2-1") & collection_date <= as.Date("2024-3-31") ~ "Feb-Mar",
      collection_date >= as.Date("2024-4-1")  ~ "April and after"
    )) %>% 
    mutate(month_when_tested_cat = factor(month_when_tested_cat,
                                          levels = c("Oct-Nov", "Dec-Jan", "Feb-Mar","April and after")))
  
  
  # dosage 
  df <- df %>% 
    mutate(rsv_mab_dose = case_when(rsv_mab_detailed == "100mg before collection" ~ "100mg",
                                    rsv_mab_detailed == "50mg before collection" ~ "50mg",
                                    TRUE ~ "no mAb"))
  
  
  # oxygen support (assume those without oxygen information are those without oxygen support)
  df <- df %>% 
    mutate(highflow_oxygen = ifelse(is.na(highflow_oxygen), 0, highflow_oxygen))
  
  # there seems to be individuals not admitted to hospital but had hosp los
  
  # add confounders dummy variables 
  df <- df %>% 
    # recode confounders
    mutate(cf_birth_weight = birth_weight) %>% # about 25% missing 
    mutate(cf_age = case_when(age_at_test_in_months_cat_2 == "under 3m" ~ 1,
                              age_at_test_in_months_cat_2 == "3-6m" ~ 2,
                              age_at_test_in_months_cat_2 == "6-9m" ~ 3,
                              age_at_test_in_months_cat_2 == "9-12m" ~ 4,
                              age_at_test_in_months_cat_2 == "above 1 yo" ~ 5)) %>%
    mutate(cf_month_tested = case_when(month_when_tested_cat == "Oct-Nov" ~ 1,
                                       month_when_tested_cat == "Dec-Jan" ~ 2,
                                       month_when_tested_cat == "Feb-Mar" ~ 3,
                                       month_when_tested_cat == "April and after" ~ 4
    )) %>%
    # convert confounders to dummy variables 
    mutate(cf_age_1 = ifelse(cf_age == 1, 1, 0),
           cf_age_2 = ifelse(cf_age == 2, 1, 0),
           cf_age_3 = ifelse(cf_age == 3, 1, 0),
           cf_age_4 = ifelse(cf_age == 4, 1, 0), 
           cf_age_5 = ifelse(cf_age == 5, 1, 0)) %>%
    mutate(cf_month_tested_1 = ifelse(cf_month_tested == 1, 1, 0),
           cf_month_tested_2 = ifelse(cf_month_tested == 2, 1, 0),
           cf_month_tested_3 = ifelse(cf_month_tested == 3, 1, 0),
           cf_month_tested_4 = ifelse(cf_month_tested == 4, 1, 0)) %>%
    mutate(cf_insurance_1 = ifelse(insurance_type == "private", 1, 0),
           cf_insurance_2 = ifelse(insurance_type == "public", 1, 0),
           cf_insurance_3 = ifelse(insurance_type == "uninsured", 1, 0)) %>%
    # having at least one risk factor
    mutate(cf_onerf = ifelse(risk_factor_atleastone == "yes", 1, 0))
  
  return(df)
  
}


# additional processing for sensitivity analysis 
process_df_ssa <- function(df){
  
  # reclassify time since vaccination
  # classify time since mab to testing 
  df <- df %>% 
    # 2-month interval
    mutate(days_btw_mab_collection_cat = 
             case_when(days_btw_mab_collection <= -7 & days_btw_mab_collection > -60 ~ "0-2 months", 
                       days_btw_mab_collection <= -60 & days_btw_mab_collection > -120 ~ "2-4 months",
                       days_btw_mab_collection <= -120 ~ "4 months +",
                       is.na(days_btw_mab_collection) | days_btw_mab_collection >= 0 ~ "no mAb")) %>%
    mutate(days_btw_mab_collection_cat = factor(days_btw_mab_collection_cat,
                                                levels = c("no mAb",
                                                           "0-2 months",
                                                           "2-4 months",
                                                           "4 months +"))) %>%
    # 3-months interval
    mutate(days_btw_mab_collection_cat_2 = 
             case_when(days_btw_mab_collection <= -7 & days_btw_mab_collection > -90 ~ "0-3 months", 
                       days_btw_mab_collection <= -90 & days_btw_mab_collection > -190 ~ "3-6 months",
                       is.na(days_btw_mab_collection) | days_btw_mab_collection >= 0 ~ "no mAb")) %>%
    mutate(days_btw_mab_collection_cat_2 = factor(days_btw_mab_collection_cat_2,
                                                  levels = c("no mAb",
                                                             "0-3 months",
                                                             "3-6 months"))) %>%
    # 1 months interval
    mutate(days_btw_mab_collection_cat_3 = 
             case_when(days_btw_mab_collection <= -7 & days_btw_mab_collection > -30 ~ "0-1 months", 
                       days_btw_mab_collection <= -30 & days_btw_mab_collection > -60 ~ "1-2 months",
                       days_btw_mab_collection <= -60 & days_btw_mab_collection > -90 ~ "2-3 months",
                       days_btw_mab_collection <= -90 & days_btw_mab_collection > -120 ~ "3-4 months",
                       days_btw_mab_collection <= -120 ~ "4 months +",
                       is.na(days_btw_mab_collection) | days_btw_mab_collection >= 0 ~ "no mAb")) %>%
    mutate(days_btw_mab_collection_cat_3 = factor(days_btw_mab_collection_cat_3,
                                                  levels = c("no mAb",
                                                             "0-1 months",
                                                             "1-2 months",
                                                             "2-3 months",
                                                             "3-4 months",
                                                             "4 months +")))
  
  return(df)
  
  
}


###############################
# Functions for VE estimation (regression models)
###############################

## unmatched  model (unstratified)
regress_unmatch <- function(df.ve,
                            confounders){
  
  # get n of cases and controls for unadjusted analysis
  n.cases.mab <- df.ve %>% filter(case_control == 1 & rsv_mab == 1) %>% nrow()
  n.cases.nomab <- df.ve %>% filter(case_control == 1 & rsv_mab == 0) %>% nrow()
  n.controls.mab <- df.ve %>% filter(case_control == 0 & rsv_mab == 1) %>% nrow()
  n.controls.nomab <- df.ve %>% filter(case_control == 0 & rsv_mab == 0) %>% nrow()

  # get n of cases and controls for adjusted analysis
  n.cases.mab.adj <-  df.ve[, c("case_control", "rsv_mab", confounders)] %>% 
    drop_na() %>% filter(case_control == 1 & rsv_mab == 1) %>% nrow()
  n.cases.nomab.adj <-  df.ve[, c("case_control",  "rsv_mab", confounders)] %>% 
    drop_na() %>% filter(case_control == 1 & rsv_mab == 0) %>% nrow()
  n.controls.mab.adj <- df.ve[, c("case_control",  "rsv_mab", confounders)] %>% 
    drop_na() %>% filter(case_control == 0 & rsv_mab == 1) %>% nrow()
  n.controls.nomab.adj <- df.ve[, c("case_control",  "rsv_mab", confounders)] %>% 
    drop_na() %>% filter(case_control == 0 & rsv_mab == 0) %>% nrow()
  
  
  # unadjusted
  formula <- as.formula("case_control ~ rsv_mab") 
  model <- glm(formula, data = df.ve, family = "binomial") 
  ve.median.unadj <- 1 - exp(model$coefficients[2])
  ve.ub.unadj <- 1 - exp(confint(model)[2,][1])
  ve.lb.unadj <- 1 - exp(confint(model)[2,][2])

  # adjusted
  formula <- as.formula(paste(c("case_control ~ rsv_mab", confounders), collapse = " + ")) 
  model <- glm(formula, data = df.ve, family = "binomial") 
  ve.median.adj <- 1 - exp(model$coefficients[2])
  ve.ub.adj <- 1 - exp(confint(model)[2,][1])
  ve.lb.adj <- 1 - exp(confint(model)[2,][2])
  
  # make a table
  ve.results <- data.frame(
    n_cases_mab = paste0(n.cases.mab, " (", n.cases.mab.adj, ")"),
    n_cases_nomab = paste0(n.cases.nomab, " (", n.cases.nomab.adj, ")"),
    n_controls_mab = paste0(n.controls.mab, " (", n.controls.mab.adj, ")"),
    n_controls_nomab = paste0(n.controls.nomab, " (", n.controls.nomab.adj, ")"),
    ve_unadj = paste0(round(ve.median.unadj*100,1), " (", round(ve.lb.unadj*100,1), "-", round(ve.ub.unadj*100,1), ")"),
    ve_adj = paste0(round(ve.median.adj*100,1), " (", round(ve.lb.adj*100,1), "-", round(ve.ub.adj*100,1), ")")
  ) 
  
  # row.names(ve.results) <- NULL
  
  return(ve.results)
}


###############################
# (SSA) Functions for VE estimation (regression models) for SSA!
###############################

## unmatched  model (unstratified)
regress_unmatch_hepb <- function(df.ve,
                            confounders){
  
  # get n of cases and controls for unadjusted analysis
  n.cases.mab <- df.ve %>% filter(case_control == 1 & hep_vax == 1) %>% nrow()
  n.cases.nomab <- df.ve %>% filter(case_control == 1 & hep_vax == 0) %>% nrow()
  n.controls.mab <- df.ve %>% filter(case_control == 0 & hep_vax == 1) %>% nrow()
  n.controls.nomab <- df.ve %>% filter(case_control == 0 & hep_vax == 0) %>% nrow()
  
  # get n of cases and controls for adjusted analysis
  n.cases.mab.adj <-  df.ve[, c("case_control", "hep_vax", confounders)] %>% 
    drop_na() %>% filter(case_control == 1 & hep_vax == 1) %>% nrow()
  n.cases.nomab.adj <-  df.ve[, c("case_control",  "hep_vax", confounders)] %>% 
    drop_na() %>% filter(case_control == 1 & hep_vax == 0) %>% nrow()
  n.controls.mab.adj <- df.ve[, c("case_control",  "hep_vax", confounders)] %>% 
    drop_na() %>% filter(case_control == 0 & hep_vax == 1) %>% nrow()
  n.controls.nomab.adj <- df.ve[, c("case_control",  "hep_vax", confounders)] %>% 
    drop_na() %>% filter(case_control == 0 & hep_vax == 0) %>% nrow()
  
  
  # unadjusted
  formula <- as.formula("case_control ~ hep_vax") 
  model <- glm(formula, data = df.ve, family = "binomial") 
  ve.median.unadj <- 1 - exp(model$coefficients[2])
  ve.ub.unadj <- 1 - exp(confint(model)[2,][1])
  ve.lb.unadj <- 1 - exp(confint(model)[2,][2])
  
  # adjusted
  formula <- as.formula(paste(c("case_control ~ hep_vax", confounders), collapse = " + ")) 
  model <- glm(formula, data = df.ve, family = "binomial") 
  ve.median.adj <- 1 - exp(model$coefficients[2])
  ve.ub.adj <- 1 - exp(confint(model)[2,][1])
  ve.lb.adj <- 1 - exp(confint(model)[2,][2])
  
  # make a table
  ve.results <- data.frame(
    n_cases_mab = paste0(n.cases.mab, " (", n.cases.mab.adj, ")"),
    n_cases_nomab = paste0(n.cases.nomab, " (", n.cases.nomab.adj, ")"),
    n_controls_mab = paste0(n.controls.mab, " (", n.controls.mab.adj, ")"),
    n_controls_nomab = paste0(n.controls.nomab, " (", n.controls.nomab.adj, ")"),
    ve_unadj = paste0(round(ve.median.unadj*100,1), " (", round(ve.lb.unadj*100,1), "-", round(ve.ub.unadj*100,1), ")"),
    ve_adj = paste0(round(ve.median.adj*100,1), " (", round(ve.lb.adj*100,1), "-", round(ve.ub.adj*100,1), ")")
  ) 
  
  # row.names(ve.results) <- NULL
  
  return(ve.results)
}


########################################################
### Function to convert VE to  forest plot 
########################################################
ve_forest <- function(df.plot = ve){
  
  # breakdown adjusted ve estimates from character to numeric columns (median, lb, ub)
  a <- matrix(unlist(strsplit(df.plot$ve_adj, "\\(")), ncol = 2, byrow = T) %>% 
    as.data.frame() %>%
    mutate(V2 = str_replace(V2, "\\)", "")) %>% 
    mutate(n_dash = str_count(V2, "-"))
  
  a$ve_adj_lb <- NA
  a$ve_adj_ub <- NA
  
  a[which(a$n_dash == 1),]$ve_adj_lb <- as.numeric(matrix(unlist(strsplit((a[which(a$n_dash == 1),])$V2, "-")), ncol = 2, byrow = T)[,1])
  a[which(a$n_dash == 1),]$ve_adj_ub <- as.numeric(matrix(unlist(strsplit((a[which(a$n_dash == 1),])$V2, "-")), ncol = 2, byrow = T)[,2])
  
  # if there is lb < 0
  #a[which(a$n_dash == 2),]$ve_adj_lb <- - as.numeric(matrix(unlist(strsplit(sub(".", "", (a[which(a$n_dash == 2),])$V2), "-")), ncol = 2, byrow = T)[,1])
  #a[which(a$n_dash == 2),]$ve_adj_ub <- + as.numeric(matrix(unlist(strsplit(sub(".", "", (a[which(a$n_dash == 2),])$V2), "-")), ncol = 2, byrow = T)[,2])
  
  a <- a %>% dplyr::rename(ve_adj_median = V1)
  
  df.plot <- cbind(df.plot, a %>% dplyr::select(starts_with("ve_adj"))) %>% 
    mutate(across(c(ve_adj_median, ve_adj_lb, ve_adj_ub), ~ as.numeric(.)))
  
  # add some vars for plotting
  df.plot <- df.plot %>% 
    # too low lb --> stops at -50% with an arrow 
    mutate(add_arrow = ifelse(ve_adj_lb < -50, 1, 0)) %>% 
    mutate(ve_adj_lb = ifelse(add_arrow == 1, -50, ve_adj_lb)) 
  
  # create labels as the first row
  df.plt <- 
    rbind(
    data.frame(against = "Outcome prevented\n",
               n_cases_mab = "Cases \n(Nirsevimab-recepient)\n",
               n_cases_nomab = "Cases \n(Non-recepient)\n",
               n_controls_mab = "Controls \n(Nirsevimab-recepient)\n",
               n_controls_nomab = "Controls \n(Non-recepient)\n",
               ve_unadj = "Unadjusted VE (95% CI)\n",
               ve_adj = "Adjusted VE (95% CI)\n",
               ve_adj_median = NA,
               ve_adj_lb = NA, 
               ve_adj_ub = NA,
               add_arrow = 0
              ),
    df.plot
  )
  
  df.plt <- df.plt %>% 
    mutate(ve_index = factor(1:nrow(df.plt), levels = as.character(nrow(df.plt):1)))
  
  plt.left <- df.plt %>% 
    ggplot(aes(y = ve_index)) +
    geom_text(aes(x = 0, label = against), hjust = 0) +
    # geom_text(aes(x = 2.4, label = n_cases_mab), hjust = 0) +
    # geom_text(aes(x = 2.8, label = n_controls_mab), hjust = 0) +
    # geom_text(aes(x = 3.5, label = n_cases_nomab), hjust = 0) +
    # geom_text(aes(x = 4.2, label = n_controls_nomab), hjust = 0) +
    geom_text(aes(x = 1.3, label = ve_unadj), hjust = 0) +
    geom_text(aes(x = 2.4, label = ve_adj), hjust = 0) +
    theme_void() +
    coord_cartesian(xlim = c(0, 3))
  
  df.plt.arrow <- df.plt %>% filter(add_arrow == 1)
    
  plt.right <- 
    ggplot(aes(y = ve_index), data = df.plt) +
    geom_point(aes(x= ve_adj_median), shape=15, size=3, data = df.plt) +
    geom_linerange(aes(xmin = ve_adj_lb, xmax = ve_adj_ub), data = df.plt)  +
    geom_segment(aes(x = ve_adj_ub, xend = ve_adj_lb, 
                  y = ve_index, yend = ve_index), data = df.plt.arrow,  
                 arrow = arrow(length = unit(2, "mm"))) +
    geom_vline(xintercept = 0, linetype = "dashed") + 
    xlab("Effectiveness of Nirsevimab (%)") +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.line.y = element_blank(),
          axis.line = element_line(colour = "black"))
    
  layout <- c(
    area(t = 0, l = 0, b = 30, r = 6.3), # left plot, starts at the top of the page (0) and goes 30 units down and 3 units to the right
    area(t = 1, l = 7.5, b = 30, r = 9)  # right plot starts a little lower (t=1) because there's no title. starts 1 unit right of the left plot (l=4, whereas left plot is r=3), goes to the bottom of the page (30 units), and 6 units further over from the left plot (r=9 whereas left plot is r=3)
  )
  
  # final plot arrangement
  plt <- plt.left +  plt.right + plot_layout(design = layout)

  return(plt)

}

ve_forest_withn <- function(df.plot = ve){
  
  # breakdown adjusted ve estimates from character to numeric columns (median, lb, ub)
  a <- matrix(unlist(strsplit(df.plot$ve_adj, "\\(")), ncol = 2, byrow = T) %>% 
    as.data.frame() %>%
    mutate(V2 = str_replace(V2, "\\)", "")) %>% 
    mutate(n_dash = str_count(V2, "-"))
  
  a$ve_adj_lb <- NA
  a$ve_adj_ub <- NA
  
  a[which(a$n_dash == 1),]$ve_adj_lb <- as.numeric(matrix(unlist(strsplit((a[which(a$n_dash == 1),])$V2, "-")), ncol = 2, byrow = T)[,1])
  a[which(a$n_dash == 1),]$ve_adj_ub <- as.numeric(matrix(unlist(strsplit((a[which(a$n_dash == 1),])$V2, "-")), ncol = 2, byrow = T)[,2])
  
  if(2 %in% a$n_dash){
    # when there is lb < 0 
    a[which(a$n_dash == 2),]$ve_adj_lb <- - as.numeric(matrix(unlist(strsplit(sub(".", "", (a[which(a$n_dash == 2),])$V2), "-")), ncol = 2, byrow = T)[,1])
    a[which(a$n_dash == 2),]$ve_adj_ub <- + as.numeric(matrix(unlist(strsplit(sub(".", "", (a[which(a$n_dash == 2),])$V2), "-")), ncol = 2, byrow = T)[,2])
  }
  a <- a %>% dplyr::rename(ve_adj_median = V1)
  
  df.plot <- cbind(df.plot, a %>% dplyr::select(starts_with("ve_adj"))) %>% 
    mutate(across(c(ve_adj_median, ve_adj_lb, ve_adj_ub), ~ as.numeric(.)))
  
  # add some vars for plotting
  df.plot <- df.plot %>% 
    # too low lb --> stops at -50% with an arrow 
    mutate(add_arrow = ifelse(ve_adj_lb < -50, 1, 0)) %>% 
    mutate(ve_adj_lb = ifelse(add_arrow == 1, -50, ve_adj_lb)) 
  
  # create labels as the first row
  df.plt <- 
    rbind(
      data.frame(against = "Outcome prevented",
                 n_cases_mab = "RSV+\n(Vaxed)",
                 n_cases_nomab = "RSV+\n(Unvaxed)",
                 n_controls_mab = "RSV-\n(Vaxed)",
                 n_controls_nomab = "RSV-\n(Unvaxed)",
                 ve_unadj = "Unadjusted VE\n(95% CI)",
                 ve_adj = "Adjusted VE \n(95% CI)",
                 ve_adj_median = NA,
                 ve_adj_lb = NA, 
                 ve_adj_ub = NA,
                 add_arrow = 0
      ),
      df.plot
    )
  
  df.plt <- df.plt %>% 
    mutate(ve_index = factor(1:nrow(df.plt), levels = as.character(nrow(df.plt):1)))
  
  plt.left <- df.plt %>% 
    ggplot(aes(y = ve_index)) +
    geom_text(aes(x = 0, label = against), hjust = 0) +
    geom_text(aes(x = 2, label = n_cases_mab), hjust = 0, lineheight = .75) +
    geom_text(aes(x = 2.5, label = n_controls_mab), hjust = 0, lineheight = .75) +
    geom_text(aes(x = 3.1, label = n_cases_nomab), hjust = 0, lineheight = .75) +
    geom_text(aes(x = 3.6, label = n_controls_nomab), hjust = 0, lineheight = .75) +
    geom_text(aes(x = 4.3, label = ve_unadj), hjust = 0, lineheight = .75) +
    geom_text(aes(x = 5.3, label = ve_adj), hjust = 0, lineheight = .75) +
    theme_void() +
    coord_cartesian(xlim = c(0, 5.8))
  
  df.plt.arrow <- df.plt %>% filter(add_arrow == 1)
  
  plt.right <- 
    ggplot(aes(y = ve_index), data = df.plt) +
    geom_point(aes(x= ve_adj_median), shape=15, size=3, data = df.plt) +
    geom_linerange(aes(xmin = ve_adj_lb, xmax = ve_adj_ub), data = df.plt)  +
    geom_segment(aes(x = ve_adj_ub, xend = ve_adj_lb, 
                     y = ve_index, yend = ve_index), data = df.plt.arrow,  
                 arrow = arrow(length = unit(2, "mm"))) +
    geom_vline(xintercept = 0, linetype = "dashed") + 
    xlab("Effectiveness of Nirsevimab (%)") +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.line.y = element_blank(),
          axis.line = element_line(colour = "black"))
  
  layout <- c(
    area(t = 0, l = 0, b = 30, r = 7), # left plot, starts at the top of the page (0) and goes 30 units down and 3 units to the right
    area(t = 1, l = 8, b = 30, r = 9)  # right plot starts a little lower (t=1) because there's no title. starts 1 unit right of the left plot (l=4, whereas left plot is r=3), goes to the bottom of the page (30 units), and 6 units further over from the left plot (r=9 whereas left plot is r=3)
  )
  
  # final plot arrangement
  plt <- plt.left +  plt.right + plot_layout(design = layout)
  
  return(plt)
  
}


# Function making the plot with 2 rows for each outcome 
ve_forest_2rows <- function(df.plot = ve,
                            ssa = FALSE){
  
  if(ssa){col_name = ""}else{col_name = "Outcome prevented\n"}
  
  # breakdown adjusted ve estimates from character to numeric columns (median, lb, ub)
  a <- matrix(unlist(strsplit(df.plot$ve_adj, "\\(")), ncol = 2, byrow = T) %>% 
    as.data.frame() %>%
    mutate(V2 = str_replace(V2, "\\)", "")) %>% 
    mutate(n_dash = str_count(V2, "-"))
  
  a$ve_adj_lb <- NA
  a$ve_adj_ub <- NA
  
  if(1 %in% a$n_dash){
    a[which(a$n_dash == 1),]$ve_adj_lb <- as.numeric(matrix(unlist(strsplit((a[which(a$n_dash == 1),])$V2, "-")), ncol = 2, byrow = T)[,1])
    a[which(a$n_dash == 1),]$ve_adj_ub <- as.numeric(matrix(unlist(strsplit((a[which(a$n_dash == 1),])$V2, "-")), ncol = 2, byrow = T)[,2])
  }
  # when there is lb < 0 
  # a[which(a$n_dash == 2),]$ve_adj_lb <- - as.numeric(matrix(unlist(strsplit(sub(".", "", (a[which(a$n_dash == 2),])$V2), "-")), ncol = 2, byrow = T)[,1])
  # a[which(a$n_dash == 2),]$ve_adj_ub <- + as.numeric(matrix(unlist(strsplit(sub(".", "", (a[which(a$n_dash == 2),])$V2), "-")), ncol = 2, byrow = T)[,2])
  
  if(2 %in% a$n_dash){
    # when there is lb < 0 
    a[which(a$n_dash == 2),]$ve_adj_lb <- - as.numeric(matrix(unlist(strsplit(sub(".", "", (a[which(a$n_dash == 2),])$V2), "-")), ncol = 2, byrow = T)[,1])
    a[which(a$n_dash == 2),]$ve_adj_ub <- + as.numeric(matrix(unlist(strsplit(sub(".", "", (a[which(a$n_dash == 2),])$V2), "-")), ncol = 2, byrow = T)[,2])
  }
  
  a <- a %>% dplyr::rename(ve_adj_median = V1)
  
  df.plot <- cbind(df.plot, a %>% dplyr::select(starts_with("ve_adj"))) %>% 
    mutate(across(c(ve_adj_median, ve_adj_lb, ve_adj_ub), ~ as.numeric(.)))
  
  # convert to several rows per outcome
  for(i in 1:nrow(df.plot)){
    
    # rearrange
    df.plot.temp <- df.plot[i,]
    df.plot.rearranged.temp <- 
      data.frame(
        outcome = c(df.plot.temp$against, NA, NA),
        vax = c(NA, "Not immunized", "Immunized"),
        case = c(NA, df.plot.temp$n_cases_nomab, df.plot.temp$n_cases_mab),
        control = c(NA, df.plot.temp$n_controls_nomab, df.plot.temp$n_controls_mab),
        ve_unadj = c(NA, "REF", df.plot.temp$ve_unadj), 
        ve_adj = c(NA, "REF", df.plot.temp$ve_adj),
        ve_adj_median = c(NA, NA, df.plot.temp$ve_adj_median),
        ve_adj_lb = c(NA, NA, df.plot.temp$ve_adj_lb),
        ve_adj_ub = c(NA, NA, df.plot.temp$ve_adj_ub)
      )
    
    if(i == 1){df.plot.rearrange <- df.plot.rearranged.temp}
    if(i != 1){df.plot.rearrange <- rbind(df.plot.rearrange, df.plot.rearranged.temp)}
    
  }
  
  # add some vars for plotting
  df.plot.rearrange <- df.plot.rearrange %>% 
    # too low lb --> stops at -50% with an arrow 
    mutate(add_arrow = ifelse(ve_adj_lb < -25, 1, 0)) %>% 
    mutate(ve_adj_lb = ifelse(add_arrow == 1, -25, ve_adj_lb)) 
  
  
  
  # create labels as the first row
  df.plt <- 
    rbind(
      data.frame(outcome = col_name,
                 vax = NA,
                 case = "RSV positive\n",
                 control = "RSV negative\n",
                 ve_unadj = "\nUnadjusted \nEffectiveness (95%CI)\n",
                 ve_adj = "\nAdjusted \nEffectiveness (95%CI)\n",
                 ve_adj_median = NA,
                 ve_adj_lb = NA, 
                 ve_adj_ub = NA,
                 add_arrow = NA
      ),
      df.plot.rearrange
    )
  
  df.plt <- df.plt %>% 
    mutate(ve_index = factor(1:nrow(df.plt), levels = as.character(nrow(df.plt):1)))
  
  
  # plot bold column name separately
  df.plt.colname <- df.plt %>% filter(case == "RSV positive\n")
  # plot other rows
  df.plt.body <- df.plt[-1,]
  
  x_pos <- c(0, 0.05, 0.3, 0.45, 0.6, 0.83)
  
  
  plt.left <- 
    ggplot() +
    # bold colnames
    geom_text(aes(x = x_pos[1],y = ve_index, label = outcome), hjust = 0, fontface= c('bold'), lineheight = 0.8, data = df.plt) +
    geom_text(aes(x = x_pos[2],y = ve_index, label = vax), hjust = 0, lineheight = .75, fontface= c('bold'), data = df.plt.colname) +
    geom_text(aes(x = x_pos[3],y = ve_index, label = case), hjust = 0, lineheight = .75, fontface= c('bold'), data = df.plt.colname) +
    geom_text(aes(x = x_pos[4],y = ve_index, label = control), hjust = 0, lineheight = .75, fontface= c('bold'), data = df.plt.colname) +
    geom_text(aes(x = x_pos[5],y = ve_index, label = ve_unadj), hjust = 0, lineheight = .75, fontface= c('bold'), data = df.plt.colname) +
    geom_text(aes(x = x_pos[6],y = ve_index, label = ve_adj), hjust = 0, lineheight = .75, fontface= c('bold'), data = df.plt.colname) +
    # unbold body
    geom_text(aes(x = x_pos[2],y = ve_index, label = vax), hjust = 0, lineheight = .75, data = df.plt.body) +
    geom_text(aes(x = x_pos[3],y = ve_index, label = case), hjust = 0, lineheight = .75, data = df.plt.body) +
    geom_text(aes(x = x_pos[4],y = ve_index, label = control), hjust = 0, lineheight = .75, data = df.plt.body) +
    geom_text(aes(x = x_pos[5],y = ve_index, label = ve_unadj), hjust = 0, lineheight = .75, data = df.plt.body) +
    geom_text(aes(x = x_pos[6],y = ve_index, label = ve_adj), hjust = 0, lineheight = .75, data = df.plt.body) +
    theme_void() +
    coord_cartesian(xlim = c(0, 1))
  
  df.plt.arrow <- df.plt %>% filter(add_arrow == 1)
  
  plt.right <- 
    ggplot(aes(y = ve_index), data = df.plt) +
    geom_point(aes(x= ve_adj_median), shape=15, size=3, data = df.plt) +
    geom_linerange(aes(xmin = ve_adj_lb, xmax = ve_adj_ub), data = df.plt)  +
    geom_segment(aes(x = ve_adj_ub, xend = -25,
                     y = ve_index, yend = ve_index), data = df.plt.arrow,
                 arrow = arrow(length = unit(2, "mm"))) +
    geom_vline(xintercept = 0, linetype = "dashed") + 
    xlab("Effectiveness of Nirsevimab (%)") +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.line.y = element_blank(),
          axis.line = element_line(colour = "black")) +
    scale_x_continuous(limits = c(-25, 100),
                       breaks = c(-25, 0, 25, 50, 75, 100),
                       labels = c("-25", "0", "25", "50", "75", "100")) 
    
  
  layout <- c(
    patchwork::area(t = 0, l = 0, b = 30, r = 7), # left plot, starts at the top of the page (0) and goes 30 units down and 3 units to the right
    patchwork::area(t = 1, l = 8, b = 30, r = 9)  # right plot starts a little lower (t=1) because there's no title. starts 1 unit right of the left plot (l=4, whereas left plot is r=3), goes to the bottom of the page (30 units), and 6 units further over from the left plot (r=9 whereas left plot is r=3)
  )
  
  # final plot arrangement
  plt <- plt.left +  plt.right + plot_layout(design = layout)
  
  return(plt)
  
}


regress_penalized <- function(df.ve, confounders, against){
  
  # prepare data 
  temp <- df.ve %>% dplyr::select(case_control, rsv_mab, confounders)
  x <- model.matrix(case_control ~ ., temp)[,-1]
  y <- temp$case_control
  
  # Fit model with cross-validation to find optimal lambda
  set.seed(123)
  cv_model <- cv.glmnet(x, y, alpha = 1, family = "binomial") # Lasso
  
  # Fit final model using best lambda
  final_model <- glmnet(x, y, alpha = 1, family = "binomial",
                        lambda = cv_model$lambda.min)
  
  
  # Get VE
  VE <- round((1-exp(coef(final_model))[2]) * 100, 1) # coefficient for rsv_mab
  
  # Get CI of VE through bootstrap
  set.seed(123)
  boot_coef <- matrix(NA, nrow=100, ncol=ncol(x))
  for(i in 1:100) {
    boot_index <- sample(1:nrow(x), replace=TRUE)
    boot_fit <- glmnet(x[boot_index,], y[boot_index], 
                       family="binomial", lambda=cv_model$lambda.min)
    boot_coef[i,] <- coef(boot_fit)[-1]  # Remove intercept
  }
  
  # Calculate CI
  VE_ub <- round((1 - exp(apply(boot_coef, 2, quantile, 0.025))[1]) * 100, 1)
  VE_lb <- round((1 - exp(apply(boot_coef, 2, quantile, 0.975))[1]) * 100, 1)
  
  
  ## fit an unadjust LASSO model (resume)
  
  
  
  # get number of cases and controls 
  n_cases_mab <- df.ve %>% filter(rsv_mab == 1 & case_control == 1) %>% nrow()
  n_cases_nomab <- df.ve %>% filter(rsv_mab == 0 & case_control == 1) %>% nrow()
  n_controls_mab <- df.ve %>% filter(rsv_mab == 1 & case_control == 0) %>% nrow()
  n_controls_nomab <- df.ve %>% filter(rsv_mab == 0 & case_control == 0) %>% nrow()
  
  df.output <- data.frame(
    against = against,
    n_cases_mab = n_cases_mab, 
    n_cases_nomab = n_cases_nomab,
    n_controls_mab = n_controls_mab,
    n_controls_nomab = n_controls_nomab,
    ve_adj = paste0(VE, " (", VE_lb, ", ", VE_ub, ")")
  )
  
  return(df.output)
  
}


regress_subgroup <- function(df.ve, confounders, against){
  
  # unadjusted 
  formula <- as.formula(paste(c("case_control ~ rsv_mab"), collapse = " + ")) 
  model <- glm(formula, data = df.ve, family = binomial)
  ve.unadj.median <- round((1 - exp(model$coefficients[2])) * 100, 1)
  ve.unadj.ub <- round((1 - exp(confint(model)[2,][1])) * 100, 1)
  ve.unadj.lb <- round((1 - exp(confint(model)[2,][2])) * 100, 1)
  
  # adjusted
  formula <- as.formula(paste(c("case_control ~ rsv_mab", confounders), collapse = " + ")) 
  model <- glm(formula, data = df.ve, family = binomial)
  ve.adj.median <- round((1 - exp(model$coefficients[2])) * 100, 1)
  ve.adj.ub <- round((1 - exp(confint(model)[2,][1])) * 100, 1)
  ve.adj.lb <- round((1 - exp(confint(model)[2,][2])) * 100, 1)
  
  # get number of cases and controls 
  n_cases_mab <- df.ve %>% filter(rsv_mab == 1 & case_control == 1) %>% nrow()
  n_cases_nomab <- df.ve %>% filter(rsv_mab == 0 & case_control == 1) %>% nrow()
  n_controls_mab <- df.ve %>% filter(rsv_mab == 1 & case_control == 0) %>% nrow()
  n_controls_nomab <- df.ve %>% filter(rsv_mab == 0 & case_control == 0) %>% nrow()
  
  ve.results <- data.frame(
    against = against,
    n_cases_mab = n_cases_mab,
    n_cases_nomab = n_cases_nomab,
    n_controls_mab = n_controls_mab,
    n_controls_nomab = n_controls_nomab,
    ve_unadj = paste0(ve.unadj.median, " (", ve.unadj.lb, "-", ve.unadj.ub, ")"),
    ve_adj = paste0(ve.adj.median, " (", ve.adj.lb, "-", ve.adj.ub, ")")
  ) 
  
  return(ve.results)
  
}


# run through the four main outcomes
run_subgroup_outcomes <- function(df.subgroup){
  
  # against infection
  df.infection <- df.subgroup %>% mutate(case_control = positive_rsv)
  ve.infection <- regress_subgroup(df.ve = df.infection, confounders = confounders, against = "infection") 
  
  # against ed visit 
  df.ed <- df.subgroup %>% filter(encounter_type == "outpatient") %>% mutate(case_control = positive_rsv)
  ve.ed <- regress_subgroup(df.ve = df.ed, confounders = confounders, against = "ed") 
  
  # against ed visit 
  df.hosp <- df.subgroup %>% filter(encounter_type == "inpatient") %>% mutate(case_control = positive_rsv)
  ve.hosp <- regress_subgroup(df.ve = df.hosp, confounders = confounders, against = "hosp") 
  
  # against severe outcomes 
  df.severe <- df.subgroup %>% filter(icu_admitted == "yes" | highflow_oxygen == 1) %>% mutate(case_control = positive_rsv)
  ve.severe <- regress_subgroup(df.ve = df.severe, confounders = confounders, against = "severe") 
  
  # stack outcomes 
  ve.results <- rbind(ve.infection, ve.ed, ve.hosp, ve.severe)
  
  return(ve.results)
}

run_subgroup_outcomes_penalized <- function(df.subgroup){
  
  # against infection
  df.infection <- df.subgroup %>% mutate(case_control = positive_rsv)
  ve.infection <- regress_penalized(df.ve = df.infection, confounders = confounders, against = "infection") 
  
  # against ed visit 
  df.ed <- df.subgroup %>% filter(encounter_type == "outpatient") %>% mutate(case_control = positive_rsv)
  ve.ed <- regress_penalized(df.ve = df.ed, confounders = confounders, against = "ed") 
  
  # against ed visit 
  df.hosp <- df.subgroup %>% filter(encounter_type == "inpatient") %>% mutate(case_control = positive_rsv)
  ve.hosp <- regress_penalized(df.ve = df.hosp, confounders = confounders, against = "hosp") 
  
  # against severe outcomes 
  df.severe <- df.subgroup %>% filter(icu_admitted == "yes" | highflow_oxygen == 1) %>% mutate(case_control = positive_rsv)
  ve.severe <- regress_penalized(df.ve = df.severe, confounders = confounders, against = "severe") 
  
  # stack outcomes 
  ve.results <- rbind(ve.infection, ve.ed, ve.hosp, ve.severe)
  
  return(ve.results)
}



# convert to plot
ve_forest_subgroup <- function(df.ve){
  
  df.plot <- df.ve %>% 
    mutate(
      ve_adj_median = as.numeric(str_extract(ve_adj, "^[0-9.]+")),
      ve_adj_lower = as.numeric(str_extract(ve_adj, "(?<=\\()[0-9.]+")),
      ve_adj_upper = as.numeric(str_extract(ve_adj, "(?<=, )[0-9.]+(?=\\))"))
    )
  
  # format the subgroup 
  subgroups <- unique(df.plot$subgroup)
  for(sg in subgroups){
    temp <- df.plot %>% filter(subgroup == sg)
    df.plot.temp <- rbind(
      data.frame(ve_adj = NA, strata = NA, subgroup = sg, immunized = NA, unimmunized = NA, ve_adj_median = NA, ve_adj_lower = NA, ve_adj_upper = NA),
      temp %>% mutate(subgroup = NA)
    ) %>% dplyr::select(subgroup, strata, immunized, unimmunized, ve_adj, ve_adj_median, ve_adj_lower, ve_adj_upper)
    
    if(which(sg == subgroups) == 1){df.plt <- df.plot.temp}
    else{df.plt <- rbind(df.plt, df.plot.temp)}
  }
  
  # create labels as the first row
  df.plt <- 
    rbind(
      data.frame(subgroup = NA,
                 strata = NA,
                 immunized = "\nImmunized\n",
                 unimmunized = "\nUnimmunized\n",
                 ve_adj = "\nEffectiveness (95%CI)\n",
                 ve_adj_median = NA,
                 ve_adj_lower = NA, 
                 ve_adj_upper = NA
      ),
      df.plt
    )
  
  df.plt <- df.plt %>% 
    mutate(ve_index = factor(1:nrow(df.plt), levels = as.character(nrow(df.plt):1)))
  
  # plot bold column name separately
  df.plt.colname <- df.plt[1,]
  # plot other rows
  df.plt.body <- df.plt[-1,]
  
  x_pos <- c(0, 0.02, 0.3, 0.45, 0.68)
  
  plt.left <- 
    ggplot() +
    # bold colnames
    geom_text(aes(x = x_pos[1],y = ve_index, label = subgroup), hjust = 0, fontface= c('bold'), lineheight = 0.8, data = df.plt) +
    geom_text(aes(x = x_pos[2],y = ve_index, label = strata), hjust = 0, lineheight = .75, fontface= c('bold'), data = df.plt.colname) +
    geom_text(aes(x = x_pos[3],y = ve_index, label = immunized), hjust = 0, lineheight = .75, fontface= c('bold'), data = df.plt.colname) +
    geom_text(aes(x = x_pos[4],y = ve_index, label = unimmunized), hjust = 0, lineheight = .75, fontface= c('bold'), data = df.plt.colname) +
    geom_text(aes(x = x_pos[5],y = ve_index, label = ve_adj), hjust = 0, lineheight = .75, fontface= c('bold'), data = df.plt.colname) +
    # unbold body
    geom_text(aes(x = x_pos[2],y = ve_index, label = strata), hjust = 0, lineheight = .75, data = df.plt.body) +
    geom_text(aes(x = x_pos[3],y = ve_index, label = immunized), hjust = 0, lineheight = .75, data = df.plt.body) +
    geom_text(aes(x = x_pos[4],y = ve_index, label = unimmunized), hjust = 0, lineheight = .75, data = df.plt.body) +
    geom_text(aes(x = x_pos[5],y = ve_index, label = ve_adj), hjust = 0, lineheight = .75, data = df.plt.body) +
    theme_void() +
    coord_cartesian(xlim = c(0, 1))
  

  plt.right <- 
    ggplot(aes(y = ve_index), data = df.plt) +
    geom_point(aes(x= ve_adj_median), shape=15, size=3, data = df.plt) +
    geom_linerange(aes(xmin = ve_adj_lower, xmax = ve_adj_upper), data = df.plt)  +
    # geom_segment(aes(x = ve_adj_ub, xend = -25,
    #                  y = ve_index, yend = ve_index), data = df.plt.arrow,
    #              arrow = arrow(length = unit(2, "mm"))) +
    geom_vline(xintercept = 0, linetype = "dashed") + 
    xlab("Effectiveness of Nirsevimab (%)") +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.line.y = element_blank(),
          axis.line = element_line(colour = "black")) +
    scale_x_continuous(limits = c(-25, 100),
                       breaks = c(-25, 0, 25, 50, 75, 100),
                       labels = c("-25", "0", "25", "50", "75", "100"))  
    
    
    
    layout <- c(
      patchwork::area(t = 0, l = 0, b = 30, r = 7), # left plot, starts at the top of the page (0) and goes 30 units down and 3 units to the right
      patchwork::area(t = 1, l = 7, b = 30, r = 8)  # right plot starts a little lower (t=1) because there's no title. starts 1 unit right of the left plot (l=4, whereas left plot is r=3), goes to the bottom of the page (30 units), and 6 units further over from the left plot (r=9 whereas left plot is r=3)
    )
  
  # final plot arrangement
  plt <- plt.left +  plt.right + plot_layout(design = layout)
  
  return(plt)
  
  
  
}




# (revision) adjusting for multiple testing
regress_geeglm <- function(df.ve, confounders, against){
  
  # unadjusted 
  formula <- as.formula(paste(c("case_control ~ rsv_mab"), collapse = " + ")) 
  model <- geeglm(formula, family = binomial, id = StudyID, data = df.ve, corstr = "exchangeable")
  ve.unadj.median <- round((1 - exp(model$coefficients[2])) * 100, 1)
  ve.unadj.ub <- round((1 - exp(confint.default(model)[2,][1])) * 100, 1)
  ve.unadj.lb <- round((1 - exp(confint.default(model)[2,][2])) * 100, 1)
  
  # adjusted
  formula <- as.formula(paste(c("case_control ~ rsv_mab", confounders), collapse = " + ")) 
  model <- geeglm(formula, family = binomial, id = StudyID, data = df.ve, corstr = "exchangeable")
  ve.adj.median <- round((1 - exp(model$coefficients[2])) * 100, 1)
  ve.adj.ub <- round((1 - exp(confint.default(model)[2,][1])) * 100, 1)
  ve.adj.lb <- round((1 - exp(confint.default(model)[2,][2])) * 100, 1)
  
  # get number of cases and controls 
  n_cases_mab <- df.ve %>% filter(rsv_mab == 1 & case_control == 1) %>% nrow()
  n_cases_nomab <- df.ve %>% filter(rsv_mab == 0 & case_control == 1) %>% nrow()
  n_controls_mab <- df.ve %>% filter(rsv_mab == 1 & case_control == 0) %>% nrow()
  n_controls_nomab <- df.ve %>% filter(rsv_mab == 0 & case_control == 0) %>% nrow()
  
  ve.results <- data.frame(
    against = against,
    n_cases_mab = n_cases_mab,
    n_cases_nomab = n_cases_nomab,
    n_controls_mab = n_controls_mab,
    n_controls_nomab = n_controls_nomab,
    ve_unadj = paste0(ve.unadj.median, " (", ve.unadj.lb, "-", ve.unadj.ub, ")"),
    ve_adj = paste0(ve.adj.median, " (", ve.adj.lb, "-", ve.adj.ub, ")")
  ) 
  
  return(ve.results)
  
}


ve_wane_forest <- function(df.plot,
                           panel_title){
  
  # create lebels as the first row 
  df.plt <- rbind(
    data.frame(
      vax_time = "\nWeeks since \nimmunization\n",
      n_cases = "\nRSV \npositive\n",
      n_controls = "\nRSV \nnegative\n",
      VE.adjusted = "\nAdjusted \neffectiveness\n",
      VE.median = NA, 
      VE.lb = NA, 
      VE.ub = NA
    ),
    df.plot %>% dplyr::select(-against) %>% dplyr::select(vax_time, n_cases, n_controls, VE.adjusted, VE.median, VE.lb, VE.ub)
  )
  
  df.plt <- df.plt %>% mutate(ve_index = factor(1:nrow(df.plt), levels = as.character(nrow(df.plt):1)))
  
  # filter out those lb < -25 and add an arrow in the end 
  df.plt.arrow <- df.plt %>% filter(VE.lb < -25)
  
  # replace lb < -25 --> -25 
  df.plt <- df.plt %>% mutate(VE.lb = ifelse(VE.lb <= -25, -25, VE.lb))
  
  # plot bold column name separately 
  df.plt.colname <- df.plt %>% filter(n_cases == "\nRSV \npositive\n")
  
  # plot other rows (body)
  df.plt.body <- df.plt[-1,]
  
  # plot first column without colname 
  df.plt.firstcolumn <- df.plt %>% 
    mutate(vax_time = ifelse(vax_time == "\nWeeks since \nimmunization\n", NA, vax_time))
  
  x_pos <- c(0, 0.3, 0.5, 0.7)
  text_size = 5
  
  lineheight_body <- 0.6
  lineheight_colname <- 0.8
  
  # plot the left plot (text)
  plt.left <- 
    ggplot() +
    # bold colnames
    geom_text(aes(x = x_pos[1],y = ve_index, label = vax_time), hjust = 0, lineheight = lineheight_colname, data = df.plt.firstcolumn, size = text_size) +
    geom_text(aes(x = x_pos[1],y = ve_index, label = vax_time), hjust = 0, fontface= c('bold'), lineheight = lineheight_colname, data = df.plt.colname, size = text_size) +
    geom_text(aes(x = x_pos[2],y = ve_index, label = n_cases), hjust = 0, lineheight = lineheight_colname, fontface= c('bold'), data = df.plt.colname, size = text_size) +
    geom_text(aes(x = x_pos[3],y = ve_index, label = n_controls), hjust = 0, lineheight = lineheight_colname, fontface= c('bold'), data = df.plt.colname, size = text_size) +
    geom_text(aes(x = x_pos[4],y = ve_index, label = VE.adjusted), hjust = 0, lineheight = lineheight_colname, fontface= c('bold'), data = df.plt.colname, size = text_size) +
    # unbold body
    geom_text(aes(x = x_pos[2],y = ve_index, label = n_cases), hjust = 0, lineheight = lineheight_body, data = df.plt.body, size = text_size) +
    geom_text(aes(x = x_pos[3],y = ve_index, label = n_controls), hjust = 0, lineheight = lineheight_body, data = df.plt.body, size = text_size) +
    geom_text(aes(x = x_pos[4],y = ve_index, label = VE.adjusted), hjust = 0, lineheight = lineheight_body, data = df.plt.body, size = text_size) +
    theme_void() +
    coord_cartesian(xlim = c(0, 1)) +
    ggtitle(panel_title) +
    theme(title = element_text(size = 15))
  
  plt.right <- 
    ggplot(aes(y = ve_index), data = df.plt) +
    geom_point(aes(x= VE.median), shape=15, size=3, data = df.plt) +
    geom_linerange(aes(xmin = VE.lb, xmax = VE.ub), data = df.plt)  +
    geom_segment(aes(x = VE.ub, xend = -25,
                     y = ve_index, yend = ve_index), data = df.plt.arrow,
                 arrow = arrow(length = unit(2, "mm"))) +
    geom_vline(xintercept = 0, linetype = "dashed") + 
    xlab("Effectiveness (%)") +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          axis.title.x = element_text(size = 15), 
          axis.ticks.y=element_blank(),
          axis.line.y = element_blank(),
          axis.line = element_line(colour = "black")) +
    scale_x_continuous(limits = c(-25, 100),
                       breaks = c(-25, 0, 25, 50, 75, 100),
                       labels = c("-25", "0", "25", "50", "75", "100")) 
  
  layout <- c(
    patchwork::area(t = 0, l = 0, b = 30, r = 6.5), # left plot, starts at the top of the page (0) and goes 30 units down and 3 units to the right
    patchwork::area(t = 1, l = 7, b = 30, r = 9)  # right plot starts a little lower (t=1) because there's no title. starts 1 unit right of the left plot (l=4, whereas left plot is r=3), goes to the bottom of the page (30 units), and 6 units further over from the left plot (r=9 whereas left plot is r=3)
  )
  
  # final plot arrangement
  plt <- plt.left +  plt.right + plot_layout(design = layout)
  
  return(plt)
  
  
}



## function to compare dose-specific VE
ve_forest_dose <- function(df.plot = ve.dose){
  
  # breakdown adjusted ve estimates from character to numeric columns (median, lb, ub)
  
  # first, process 100mg VE
  a.100mg <- matrix(unlist(strsplit(df.plot$ve_adj_100mg, "\\(")), ncol = 2, byrow = T) %>% 
    as.data.frame() %>%
    mutate(V2 = str_replace(V2, "\\)", "")) %>% 
    mutate(n_dash = str_count(V2, "-"))
  
  a.100mg$ve_adj_lb_100mg <- NA
  a.100mg$ve_adj_ub_100mg <- NA
  
  a.100mg[which(a.100mg$n_dash == 1),]$ve_adj_lb_100mg <- as.numeric(matrix(unlist(strsplit((a.100mg[which(a.100mg$n_dash == 1),])$V2, "-")), ncol = 2, byrow = T)[,1])
  a.100mg[which(a.100mg$n_dash == 1),]$ve_adj_ub_100mg <- as.numeric(matrix(unlist(strsplit((a.100mg[which(a.100mg$n_dash == 1),])$V2, "-")), ncol = 2, byrow = T)[,2])
  
  if(2 %in% a.100mg$n_dash){
    # when there is lb < 0 
    a.100mg[which(a.100mg$n_dash == 2),]$ve_adj_lb_100mg <- - as.numeric(matrix(unlist(strsplit(sub(".", "", (a.100mg[which(a.100mg$n_dash == 2),])$V2), "-")), ncol = 2, byrow = T)[,1])
    a.100mg[which(a.100mg$n_dash == 2),]$ve_adj_ub_100mg <- + as.numeric(matrix(unlist(strsplit(sub(".", "", (a.100mg[which(a.100mg$n_dash == 2),])$V2), "-")), ncol = 2, byrow = T)[,2])
  }

  a.100mg <- a.100mg %>% dplyr::rename(ve_adj_median_100mg = V1)
  
  df.plot <- cbind(df.plot, a.100mg %>% dplyr::select(starts_with("ve_adj"))) %>% 
    mutate(across(c(ve_adj_median_100mg, ve_adj_lb_100mg, ve_adj_ub_100mg), ~ as.numeric(.))) 
    
  
  # then, process 50mg VE
  a.50mg <- matrix(unlist(strsplit(df.plot$ve_adj_50mg, "\\(")), ncol = 2, byrow = T) %>% 
    as.data.frame() %>%
    mutate(V2 = str_replace(V2, "\\)", "")) %>% 
    mutate(n_dash = str_count(V2, "-"))
  
  a.50mg$ve_adj_lb_50mg <- NA
  a.50mg$ve_adj_ub_50mg <- NA
  
  a.50mg[which(a.50mg$n_dash == 1),]$ve_adj_lb_50mg <- as.numeric(matrix(unlist(strsplit((a.50mg[which(a.50mg$n_dash == 1),])$V2, "-")), ncol = 2, byrow = T)[,1])
  a.50mg[which(a.50mg$n_dash == 1),]$ve_adj_ub_50mg <- as.numeric(matrix(unlist(strsplit((a.50mg[which(a.50mg$n_dash == 1),])$V2, "-")), ncol = 2, byrow = T)[,2])
  
  if(2 %in% a.50mg$n_dash){
    # when there is lb < 0 
    a.50mg[which(a.50mg$n_dash == 2),]$ve_adj_lb_50mg <- - as.numeric(matrix(unlist(strsplit(sub(".", "", (a.50mg[which(a.50mg$n_dash == 2),])$V2), "-")), ncol = 2, byrow = T)[,1])
    a.50mg[which(a.50mg$n_dash == 2),]$ve_adj_ub_50mg <- + as.numeric(matrix(unlist(strsplit(sub(".", "", (a.50mg[which(a.50mg$n_dash == 2),])$V2), "-")), ncol = 2, byrow = T)[,2])
  }
  
  a.50mg <- a.50mg %>% dplyr::rename(ve_adj_median_50mg = V1)
  
  df.plot <- cbind(df.plot, a.50mg %>% dplyr::select(starts_with("ve_adj"))) %>% 
    mutate(across(c(ve_adj_median_50mg, ve_adj_lb_50mg, ve_adj_ub_50mg), ~ as.numeric(.))) 
  
  
  # rearrange the data frame
  for(i in 1:nrow(df.plot)){
    
    # rearrange
    df.plot.temp <- df.plot[i,]
    df.plot.rearranged.temp <- 
      data.frame(
        outcome = c(df.plot.temp$against, NA, NA, NA),
        vax = c(NA, "Not immunized", "Immunized (50mg)", "Immunized (100mg)"),
        case = c(NA, df.plot.temp$n_cases_nomab, df.plot.temp$n_cases_mab_50mg, df.plot.temp$n_cases_mab_100mg),
        control = c(NA, df.plot.temp$n_controls_nomab, df.plot.temp$n_controls_mab_50mg, df.plot.temp$n_controls_mab_100mg),
        ve_unadj = c(NA, "REF", df.plot.temp$ve_unadj_50mg, df.plot.temp$ve_unadj_100mg), 
        ve_adj = c(NA, "REF", df.plot.temp$ve_adj_50mg, df.plot.temp$ve_adj_100mg),
        ve_adj_median = c(NA, NA, df.plot.temp$ve_adj_median_50mg, df.plot.temp$ve_adj_median_100mg),
        ve_adj_lb = c(NA, NA, df.plot.temp$ve_adj_lb_50mg, df.plot.temp$ve_adj_lb_100mg),
        ve_adj_ub = c(NA, NA, df.plot.temp$ve_adj_ub_50mg, df.plot.temp$ve_adj_ub_100mg)
      )
    
    if(i == 1){df.plot.rearrange <- df.plot.rearranged.temp}
    if(i != 1){df.plot.rearrange <- rbind(df.plot.rearrange, df.plot.rearranged.temp)}
    
  }
  
  
  # add some vars for plotting
  df.plot.rearrange <- df.plot.rearrange %>% 
    # too low lb --> stops at -50% with an arrow 
    mutate(add_arrow = ifelse(ve_adj_lb < -50, 1, 0)) %>% 
    mutate(ve_adj_lb = ifelse(add_arrow == 1, -50, ve_adj_lb)) 
  
  
  # create labels as the first row
  df.plt <- 
    rbind(
      data.frame(outcome = "Outcome prevented\n",
                 vax = NA,
                 case = "RSV positive\n",
                 control = "RSV negative\n",
                 ve_unadj = "Unadjusted \nEffectiveness (95%CI)\n",
                 ve_adj = "Adjusted \nEffectiveness (95%CI)\n",
                 ve_adj_median = NA,
                 ve_adj_lb = NA, 
                 ve_adj_ub = NA,
                 add_arrow = NA
      ),
      df.plot.rearrange
    )
  
  df.plt <- df.plt %>% 
    mutate(ve_index = factor(1:nrow(df.plt), levels = as.character(nrow(df.plt):1)))
  
  plt.left <- df.plt %>% 
    ggplot(aes(y = ve_index)) +
    geom_text(aes(x = 0, label = outcome), hjust = 0, fontface= c('bold'), lineheight = 0.8) +
    geom_text(aes(x = 0.05, label = vax), hjust = 0, lineheight = .75) +
    geom_text(aes(x = 0.3, label = case), hjust = 0, lineheight = .75) +
    geom_text(aes(x = 0.45, label = control), hjust = 0, lineheight = .75) +
    geom_text(aes(x = 0.6, label = ve_unadj), hjust = 0, lineheight = .75) +
    geom_text(aes(x = 0.83, label = ve_adj), hjust = 0, lineheight = .75) +
    theme_void() +
    coord_cartesian(xlim = c(0, 0.97))
  
  df.plt.arrow <- df.plt %>% filter(add_arrow == 1)
  
  plt.right <- 
    ggplot(aes(y = ve_index), data = df.plt) +
    geom_point(aes(x= ve_adj_median), shape=15, size=3, data = df.plt) +
    geom_linerange(aes(xmin = ve_adj_lb, xmax = ve_adj_ub), data = df.plt)  +
    geom_segment(aes(x = ve_adj_ub, xend = ve_adj_lb,
                     y = ve_index, yend = ve_index), data = df.plt.arrow,
                 arrow = arrow(length = unit(2, "mm"))) +
    geom_vline(xintercept = 0, linetype = "dashed") + 
    xlab("Effectiveness of Nirsevimab (%)") +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.line.y = element_blank(),
          axis.line = element_line(colour = "black"))
  
  layout <- c(
    area(t = 0, l = 0, b = 30, r = 7), # left plot, starts at the top of the page (0) and goes 30 units down and 3 units to the right
    area(t = 1, l = 8, b = 30, r = 9)  # right plot starts a little lower (t=1) because there's no title. starts 1 unit right of the left plot (l=4, whereas left plot is r=3), goes to the bottom of the page (30 units), and 6 units further over from the left plot (r=9 whereas left plot is r=3)
  )
  
  # final plot arrangement
  plt <- plt.left +  plt.right + plot_layout(design = layout)
  
  return(plt)
  
   
  
}

# function to compare dose-specific VE, adding any dose (100mg or 50mg) category
ve_forest_dose_addanydose <- function(df.plot){
  
  # breakdown adjusted ve estimates from character to numeric columns (median, lb, ub)
  
  # first, process 100mg VE
  a.100mg <- matrix(unlist(strsplit(df.plot$ve_adj_100mg, "\\(")), ncol = 2, byrow = T) %>% 
    as.data.frame() %>%
    mutate(V2 = str_replace(V2, "\\)", "")) %>% 
    mutate(n_dash = str_count(V2, "-"))
  
  a.100mg$ve_adj_lb_100mg <- NA
  a.100mg$ve_adj_ub_100mg <- NA
  
  a.100mg[which(a.100mg$n_dash == 1),]$ve_adj_lb_100mg <- as.numeric(matrix(unlist(strsplit((a.100mg[which(a.100mg$n_dash == 1),])$V2, "-")), ncol = 2, byrow = T)[,1])
  a.100mg[which(a.100mg$n_dash == 1),]$ve_adj_ub_100mg <- as.numeric(matrix(unlist(strsplit((a.100mg[which(a.100mg$n_dash == 1),])$V2, "-")), ncol = 2, byrow = T)[,2])
  
  if(2 %in% a.100mg$n_dash){
    # when there is lb < 0 
    a.100mg[which(a.100mg$n_dash == 2),]$ve_adj_lb_100mg <- - as.numeric(matrix(unlist(strsplit(sub(".", "", (a.100mg[which(a.100mg$n_dash == 2),])$V2), "-")), ncol = 2, byrow = T)[,1])
    a.100mg[which(a.100mg$n_dash == 2),]$ve_adj_ub_100mg <- + as.numeric(matrix(unlist(strsplit(sub(".", "", (a.100mg[which(a.100mg$n_dash == 2),])$V2), "-")), ncol = 2, byrow = T)[,2])
  }
  
  a.100mg <- a.100mg %>% dplyr::rename(ve_adj_median_100mg = V1)
  
  df.plot <- cbind(df.plot, a.100mg %>% dplyr::select(starts_with("ve_adj"))) %>% 
    mutate(across(c(ve_adj_median_100mg, ve_adj_lb_100mg, ve_adj_ub_100mg), ~ as.numeric(.))) 
  
  
  # then, process 50mg VE
  a.50mg <- matrix(unlist(strsplit(df.plot$ve_adj_50mg, "\\(")), ncol = 2, byrow = T) %>% 
    as.data.frame() %>%
    mutate(V2 = str_replace(V2, "\\)", "")) %>% 
    mutate(n_dash = str_count(V2, "-"))
  
  a.50mg$ve_adj_lb_50mg <- NA
  a.50mg$ve_adj_ub_50mg <- NA
  
  a.50mg[which(a.50mg$n_dash == 1),]$ve_adj_lb_50mg <- as.numeric(matrix(unlist(strsplit((a.50mg[which(a.50mg$n_dash == 1),])$V2, "-")), ncol = 2, byrow = T)[,1])
  a.50mg[which(a.50mg$n_dash == 1),]$ve_adj_ub_50mg <- as.numeric(matrix(unlist(strsplit((a.50mg[which(a.50mg$n_dash == 1),])$V2, "-")), ncol = 2, byrow = T)[,2])
  
  if(2 %in% a.50mg$n_dash){
    # when there is lb < 0 
    a.50mg[which(a.50mg$n_dash == 2),]$ve_adj_lb_50mg <- - as.numeric(matrix(unlist(strsplit(sub(".", "", (a.50mg[which(a.50mg$n_dash == 2),])$V2), "-")), ncol = 2, byrow = T)[,1])
    a.50mg[which(a.50mg$n_dash == 2),]$ve_adj_ub_50mg <- + as.numeric(matrix(unlist(strsplit(sub(".", "", (a.50mg[which(a.50mg$n_dash == 2),])$V2), "-")), ncol = 2, byrow = T)[,2])
  }
  
  a.50mg <- a.50mg %>% dplyr::rename(ve_adj_median_50mg = V1)
  
  df.plot <- cbind(df.plot, a.50mg %>% dplyr::select(starts_with("ve_adj"))) %>% 
    mutate(across(c(ve_adj_median_50mg, ve_adj_lb_50mg, ve_adj_ub_50mg), ~ as.numeric(.))) 
  
  
  # then process anydose cat
  a.anydose <- matrix(unlist(strsplit(df.plot$ve_adj_anydose, "\\(")), ncol = 2, byrow = T) %>% 
    as.data.frame() %>%
    mutate(V2 = str_replace(V2, "\\)", "")) %>% 
    mutate(n_dash = str_count(V2, "-"))
  
  a.anydose$ve_adj_lb_anydose <- NA
  a.anydose$ve_adj_ub_anydose <- NA
  
  a.anydose[which(a.anydose$n_dash == 1),]$ve_adj_lb_anydose <- as.numeric(matrix(unlist(strsplit((a.anydose[which(a.anydose$n_dash == 1),])$V2, "-")), ncol = 2, byrow = T)[,1])
  a.anydose[which(a.anydose$n_dash == 1),]$ve_adj_ub_anydose <- as.numeric(matrix(unlist(strsplit((a.anydose[which(a.anydose$n_dash == 1),])$V2, "-")), ncol = 2, byrow = T)[,2])
  
  if(2 %in% a.anydose$n_dash){
    # when there is lb < 0 
    a.anydose[which(a.anydose$n_dash == 2),]$ve_adj_lb_anydose <- - as.numeric(matrix(unlist(strsplit(sub(".", "", (a.anydose[which(a.anydose$n_dash == 2),])$V2), "-")), ncol = 2, byrow = T)[,1])
    a.anydose[which(a.anydose$n_dash == 2),]$ve_adj_ub_anydose <- + as.numeric(matrix(unlist(strsplit(sub(".", "", (a.anydose[which(a.anydose$n_dash == 2),])$V2), "-")), ncol = 2, byrow = T)[,2])
  }
  
  a.anydose <- a.anydose %>% dplyr::rename(ve_adj_median_anydose = V1)
  
  df.plot <- cbind(df.plot, a.anydose %>% dplyr::select(starts_with("ve_adj"))) %>% 
    mutate(across(c(ve_adj_median_anydose, ve_adj_lb_anydose, ve_adj_ub_anydose), ~ as.numeric(.))) 
  
  
  # rearrange the data frame
  for(i in 1:nrow(df.plot)){
    
    # rearrange
    df.plot.temp <- df.plot[i,]
    df.plot.rearranged.temp <- 
      data.frame(
        outcome = c(df.plot.temp$against, NA, NA, NA, NA),
        vax = c(NA, "Not immunized", "Immunized (50mg)", "Immunized (100mg)", "Immunized (any dosage)"),
        case = c(NA, df.plot.temp$n_cases_nomab, df.plot.temp$n_cases_mab_50mg, df.plot.temp$n_cases_mab_100mg, df.plot.temp$n_cases_mab_anydose),
        control = c(NA, df.plot.temp$n_controls_nomab, df.plot.temp$n_controls_mab_50mg, df.plot.temp$n_controls_mab_100mg, df.plot.temp$n_controls_mab_anydose),
        ve_unadj = c(NA, "REF", df.plot.temp$ve_unadj_50mg, df.plot.temp$ve_unadj_100mg, df.plot.temp$ve_unadj_anydose), 
        ve_adj = c(NA, "REF", df.plot.temp$ve_adj_50mg, df.plot.temp$ve_adj_100mg, df.plot.temp$ve_adj_anydose),
        ve_adj_median = c(NA, NA, df.plot.temp$ve_adj_median_50mg, df.plot.temp$ve_adj_median_100mg, df.plot.temp$ve_adj_median_anydose),
        ve_adj_lb = c(NA, NA, df.plot.temp$ve_adj_lb_50mg, df.plot.temp$ve_adj_lb_100mg, df.plot.temp$ve_adj_lb_anydose),
        ve_adj_ub = c(NA, NA, df.plot.temp$ve_adj_ub_50mg, df.plot.temp$ve_adj_ub_100mg, df.plot.temp$ve_adj_ub_anydose)
      )
    
    if(i == 1){df.plot.rearrange <- df.plot.rearranged.temp}
    if(i != 1){df.plot.rearrange <- rbind(df.plot.rearrange, df.plot.rearranged.temp)}
    
  }
  
  
  # add some vars for plotting
  df.plot.rearrange <- df.plot.rearrange %>% 
    # too low lb --> stops at -50% with an arrow 
    mutate(add_arrow = ifelse(ve_adj_lb < -25, 1, 0)) %>% 
    mutate(ve_adj_lb = ifelse(add_arrow == 1, -25, ve_adj_lb)) 
  
  
  # create labels as the first row
  df.plt <- 
    rbind(
      data.frame(outcome = "Outcome prevented\n",
                 vax = NA,
                 case = "RSV positive\n",
                 control = "RSV negative\n",
                 ve_unadj = "\nUnadjusted \nEffectiveness (95%CI)\n",
                 ve_adj = "\nAdjusted \nEffectiveness (95%CI)\n",
                 ve_adj_median = NA,
                 ve_adj_lb = NA, 
                 ve_adj_ub = NA,
                 add_arrow = NA
      ),
      df.plot.rearrange
    )
  
  df.plt <- df.plt %>% 
    mutate(ve_index = factor(1:nrow(df.plt), levels = as.character(nrow(df.plt):1)))
  
  # plot bold column name separately
  df.plt.colname <- df.plt %>% filter(case == "RSV positive\n")
  # plot other rows
  df.plt.body <- df.plt[-1,]
  
  x_pos <- c(0, 0.05, 0.28, 0.40, 0.52, 0.7)
  
  plt.left <- df.plt %>% 
    ggplot(aes(y = ve_index)) +
    geom_text(aes(x = x_pos[1], y = ve_index, label = outcome), hjust = 0, fontface= c('bold'), lineheight = 0.8, data = df.plt) +
    # bold colnames
    geom_text(aes(x = x_pos[2], y = ve_index, label = vax), hjust = 0,fontface= c('bold'), lineheight = .75, data = df.plt.colname) +
    geom_text(aes(x = x_pos[3], y = ve_index, label = case), hjust = 0, fontface= c('bold'),lineheight = .75, data = df.plt.colname) +
    geom_text(aes(x = x_pos[4], y = ve_index, label = control), hjust = 0,fontface= c('bold'), lineheight = .75, data = df.plt.colname) +
    geom_text(aes(x = x_pos[5], y = ve_index, label = ve_unadj), hjust = 0,fontface= c('bold'), lineheight = .75, data = df.plt.colname) +
    geom_text(aes(x = x_pos[6], y = ve_index, label = ve_adj), hjust = 0,fontface= c('bold'), lineheight = .75, data = df.plt.colname) +
    # unbold body
    geom_text(aes(x = x_pos[2], y = ve_index, label = vax), hjust = 0, lineheight = .75, data = df.plt.body) +
    geom_text(aes(x = x_pos[3], y = ve_index, label = case), hjust = 0, lineheight = .75, data = df.plt.body) +
    geom_text(aes(x = x_pos[4], y = ve_index, label = control), hjust = 0, lineheight = .75, data = df.plt.body) +
    geom_text(aes(x = x_pos[5], y = ve_index, label = ve_unadj), hjust = 0, lineheight = .75, data = df.plt.body) +
    geom_text(aes(x = x_pos[6], y = ve_index, label = ve_adj), hjust = 0, lineheight = .75, data = df.plt.body) +
    theme_void() +
    coord_cartesian(xlim = c(0, 0.82))
  
  df.plt.arrow <- df.plt %>% filter(add_arrow == 1)
  
  plt.right <- 
    ggplot(aes(y = ve_index), data = df.plt) +
    geom_point(aes(x= ve_adj_median), shape=15, size=3, data = df.plt) +
    geom_linerange(aes(xmin = ve_adj_lb, xmax = ve_adj_ub), data = df.plt)  +
    geom_segment(aes(x = ve_adj_ub, xend = ve_adj_lb,
                     y = ve_index, yend = ve_index), data = df.plt.arrow,
                 arrow = arrow(length = unit(2, "mm"))) +
    geom_vline(xintercept = 0, linetype = "dashed") + 
    xlab("Effectiveness of Nirsevimab (%)") +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.line.y = element_blank(),
          axis.line = element_line(colour = "black"))
  
  layout <- c(
    area(t = 0, l = 0, b = 30, r = 7), # left plot, starts at the top of the page (0) and goes 30 units down and 3 units to the right
    area(t = 1, l = 8, b = 30, r = 9)  # right plot starts a little lower (t=1) because there's no title. starts 1 unit right of the left plot (l=4, whereas left plot is r=3), goes to the bottom of the page (30 units), and 6 units further over from the left plot (r=9 whereas left plot is r=3)
  )
  
  # final plot arrangement
  plt <- plt.left +  plt.right + plot_layout(design = layout)
  
  return(plt)
  
  
  
}



# function to compare dose-specific VE, adding any dose (100mg or 50mg) category
# but only stratify by dose for selected outcomes
ve_forest_dose_addanydose_selectoc <- function(df.plot = ve.dose.addanydose){
  
  # breakdown adjusted ve estimates from character to numeric columns (median, lb, ub)
  
  # first, process 100mg VE
  a.100mg <- matrix(unlist(strsplit(df.plot$ve_adj_100mg, "\\(")), ncol = 2, byrow = T) %>% 
    as.data.frame() %>%
    mutate(V2 = str_replace(V2, "\\)", "")) %>% 
    mutate(n_dash = str_count(V2, "-"))
  
  a.100mg$ve_adj_lb_100mg <- NA
  a.100mg$ve_adj_ub_100mg <- NA
  
  a.100mg[which(a.100mg$n_dash == 1),]$ve_adj_lb_100mg <- as.numeric(matrix(unlist(strsplit((a.100mg[which(a.100mg$n_dash == 1),])$V2, "-")), ncol = 2, byrow = T)[,1])
  a.100mg[which(a.100mg$n_dash == 1),]$ve_adj_ub_100mg <- as.numeric(matrix(unlist(strsplit((a.100mg[which(a.100mg$n_dash == 1),])$V2, "-")), ncol = 2, byrow = T)[,2])
  
  if(2 %in% a.100mg$n_dash){
    # when there is lb < 0 
    a.100mg[which(a.100mg$n_dash == 2),]$ve_adj_lb_100mg <- - as.numeric(matrix(unlist(strsplit(sub(".", "", (a.100mg[which(a.100mg$n_dash == 2),])$V2), "-")), ncol = 2, byrow = T)[,1])
    a.100mg[which(a.100mg$n_dash == 2),]$ve_adj_ub_100mg <- + as.numeric(matrix(unlist(strsplit(sub(".", "", (a.100mg[which(a.100mg$n_dash == 2),])$V2), "-")), ncol = 2, byrow = T)[,2])
  }
  
  a.100mg <- a.100mg %>% dplyr::rename(ve_adj_median_100mg = V1)
  
  df.plot <- cbind(df.plot, a.100mg %>% dplyr::select(starts_with("ve_adj"))) %>% 
    mutate(across(c(ve_adj_median_100mg, ve_adj_lb_100mg, ve_adj_ub_100mg), ~ as.numeric(.))) 
  
  
  # then, process 50mg VE
  a.50mg <- matrix(unlist(strsplit(df.plot$ve_adj_50mg, "\\(")), ncol = 2, byrow = T) %>% 
    as.data.frame() %>%
    mutate(V2 = str_replace(V2, "\\)", "")) %>% 
    mutate(n_dash = str_count(V2, "-"))
  
  a.50mg$ve_adj_lb_50mg <- NA
  a.50mg$ve_adj_ub_50mg <- NA
  
  a.50mg[which(a.50mg$n_dash == 1),]$ve_adj_lb_50mg <- as.numeric(matrix(unlist(strsplit((a.50mg[which(a.50mg$n_dash == 1),])$V2, "-")), ncol = 2, byrow = T)[,1])
  a.50mg[which(a.50mg$n_dash == 1),]$ve_adj_ub_50mg <- as.numeric(matrix(unlist(strsplit((a.50mg[which(a.50mg$n_dash == 1),])$V2, "-")), ncol = 2, byrow = T)[,2])
  
  if(2 %in% a.50mg$n_dash){
    # when there is lb < 0 
    a.50mg[which(a.50mg$n_dash == 2),]$ve_adj_lb_50mg <- - as.numeric(matrix(unlist(strsplit(sub(".", "", (a.50mg[which(a.50mg$n_dash == 2),])$V2), "-")), ncol = 2, byrow = T)[,1])
    a.50mg[which(a.50mg$n_dash == 2),]$ve_adj_ub_50mg <- + as.numeric(matrix(unlist(strsplit(sub(".", "", (a.50mg[which(a.50mg$n_dash == 2),])$V2), "-")), ncol = 2, byrow = T)[,2])
  }
  
  a.50mg <- a.50mg %>% dplyr::rename(ve_adj_median_50mg = V1)
  
  df.plot <- cbind(df.plot, a.50mg %>% dplyr::select(starts_with("ve_adj"))) %>% 
    mutate(across(c(ve_adj_median_50mg, ve_adj_lb_50mg, ve_adj_ub_50mg), ~ as.numeric(.))) 
  
  
  # then process anydose cat
  a.anydose <- matrix(unlist(strsplit(df.plot$ve_adj_anydose, "\\(")), ncol = 2, byrow = T) %>% 
    as.data.frame() %>%
    mutate(V2 = str_replace(V2, "\\)", "")) %>% 
    mutate(n_dash = str_count(V2, "-"))
  
  a.anydose$ve_adj_lb_anydose <- NA
  a.anydose$ve_adj_ub_anydose <- NA
  
  a.anydose[which(a.anydose$n_dash == 1),]$ve_adj_lb_anydose <- as.numeric(matrix(unlist(strsplit((a.anydose[which(a.anydose$n_dash == 1),])$V2, "-")), ncol = 2, byrow = T)[,1])
  a.anydose[which(a.anydose$n_dash == 1),]$ve_adj_ub_anydose <- as.numeric(matrix(unlist(strsplit((a.anydose[which(a.anydose$n_dash == 1),])$V2, "-")), ncol = 2, byrow = T)[,2])
  
  if(2 %in% a.anydose$n_dash){
    # when there is lb < 0 
    a.anydose[which(a.anydose$n_dash == 2),]$ve_adj_lb_anydose <- - as.numeric(matrix(unlist(strsplit(sub(".", "", (a.anydose[which(a.anydose$n_dash == 2),])$V2), "-")), ncol = 2, byrow = T)[,1])
    a.anydose[which(a.anydose$n_dash == 2),]$ve_adj_ub_anydose <- + as.numeric(matrix(unlist(strsplit(sub(".", "", (a.anydose[which(a.anydose$n_dash == 2),])$V2), "-")), ncol = 2, byrow = T)[,2])
  }
  
  a.anydose <- a.anydose %>% dplyr::rename(ve_adj_median_anydose = V1)
  
  df.plot <- cbind(df.plot, a.anydose %>% dplyr::select(starts_with("ve_adj"))) %>% 
    mutate(across(c(ve_adj_median_anydose, ve_adj_lb_anydose, ve_adj_ub_anydose), ~ as.numeric(.))) 
  
  
  # rearrange the data frame
  for(i in 1:nrow(df.plot)){
    
    # rearrange
    df.plot.temp <- df.plot[i,]
    df.plot.rearranged.temp <- 
      data.frame(
        outcome = c(df.plot.temp$against, NA, NA, NA, NA),
        vax = c(NA, "Not immunized", "Immunized (50mg)", "Immunized (100mg)", "Immunized (any dosage)"),
        case = c(NA, df.plot.temp$n_cases_nomab, df.plot.temp$n_cases_mab_50mg, df.plot.temp$n_cases_mab_100mg, df.plot.temp$n_cases_mab_anydose),
        control = c(NA, df.plot.temp$n_controls_nomab, df.plot.temp$n_controls_mab_50mg, df.plot.temp$n_controls_mab_100mg, df.plot.temp$n_controls_mab_anydose),
        ve_unadj = c(NA, "REF", df.plot.temp$ve_unadj_50mg, df.plot.temp$ve_unadj_100mg, df.plot.temp$ve_unadj_anydose), 
        ve_adj = c(NA, "REF", df.plot.temp$ve_adj_50mg, df.plot.temp$ve_adj_100mg, df.plot.temp$ve_adj_anydose),
        ve_adj_median = c(NA, NA, df.plot.temp$ve_adj_median_50mg, df.plot.temp$ve_adj_median_100mg, df.plot.temp$ve_adj_median_anydose),
        ve_adj_lb = c(NA, NA, df.plot.temp$ve_adj_lb_50mg, df.plot.temp$ve_adj_lb_100mg, df.plot.temp$ve_adj_lb_anydose),
        ve_adj_ub = c(NA, NA, df.plot.temp$ve_adj_ub_50mg, df.plot.temp$ve_adj_ub_100mg, df.plot.temp$ve_adj_ub_anydose)
      )
    
    if(i == 1){df.plot.rearrange <- df.plot.rearranged.temp}
    if(i != 1){df.plot.rearrange <- rbind(df.plot.rearrange, df.plot.rearranged.temp)}
    
  }
  
  # stratify by dose for only infection 
  df.plot.rearrange <- df.plot.rearrange[-c(8,9,13,14,18,19,23,24,28,29),]
    
  
  # add some vars for plotting
  df.plot.rearrange <- df.plot.rearrange %>% 
    # too low lb --> stops at -50% with an arrow 
    mutate(add_arrow = ifelse(ve_adj_lb < -50, 1, 0)) %>% 
    mutate(ve_adj_lb = ifelse(add_arrow == 1, -50, ve_adj_lb)) 
  
  
  # create labels as the first row
  df.plt <- 
    rbind(
      data.frame(outcome = "Outcome prevented\n",
                 vax = NA,
                 case = "RSV positive\n",
                 control = "RSV negative\n",
                 ve_unadj = "\nUnadjusted \nEffectiveness (95%CI)\n",
                 ve_adj = "\nAdjusted \nEffectiveness (95%CI)\n",
                 ve_adj_median = NA,
                 ve_adj_lb = NA, 
                 ve_adj_ub = NA,
                 add_arrow = NA
      ),
      df.plot.rearrange
    )
  
  df.plt <- df.plt %>% 
    mutate(ve_index = factor(1:nrow(df.plt), levels = as.character(nrow(df.plt):1)))
  
  plt.left <- df.plt %>% 
    ggplot(aes(y = ve_index)) +
    geom_text(aes(x = 0, label = outcome), hjust = 0, fontface= c('bold'), lineheight = 0.8) +
    geom_text(aes(x = 0.05, label = vax), hjust = 0, lineheight = .75) +
    geom_text(aes(x = 0.28, label = case), hjust = 0, lineheight = .75) +
    geom_text(aes(x = 0.40, label = control), hjust = 0, lineheight = .75) +
    geom_text(aes(x = 0.52, label = ve_unadj), hjust = 0, lineheight = .75) +
    geom_text(aes(x = 0.7, label = ve_adj), hjust = 0, lineheight = .75) +
    theme_void() +
    coord_cartesian(xlim = c(0, 0.82))
  
  df.plt.arrow <- df.plt %>% filter(add_arrow == 1)
  
  plt.right <- 
    ggplot(aes(y = ve_index), data = df.plt) +
    geom_point(aes(x= ve_adj_median), shape=15, size=3, data = df.plt) +
    geom_linerange(aes(xmin = ve_adj_lb, xmax = ve_adj_ub), data = df.plt)  +
    geom_segment(aes(x = ve_adj_ub, xend = ve_adj_lb,
                     y = ve_index, yend = ve_index), data = df.plt.arrow,
                 arrow = arrow(length = unit(2, "mm"))) +
    geom_vline(xintercept = 0, linetype = "dashed") + 
    xlab("Effectiveness of Nirsevimab (%)") +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.line.y = element_blank(),
          axis.line = element_line(colour = "black"))
  
  layout <- c(
    area(t = 0, l = 0, b = 30, r = 7), # left plot, starts at the top of the page (0) and goes 30 units down and 3 units to the right
    area(t = 1, l = 8, b = 30, r = 9)  # right plot starts a little lower (t=1) because there's no title. starts 1 unit right of the left plot (l=4, whereas left plot is r=3), goes to the bottom of the page (30 units), and 6 units further over from the left plot (r=9 whereas left plot is r=3)
  )
  
  # final plot arrangement
  plt <- plt.left +  plt.right + plot_layout(design = layout)
  
  return(plt)
  
  
  
}


### function to breakdown VE median CI string into median lb and ub columns
break_ve <-function(df.plot = ve.wane.2m){
  # breakdown adjusted ve estimates from character to numeric columns (median, lb, ub)
  a <- matrix(unlist(strsplit(df.plot$VE.adjusted, "\\(")), ncol = 2, byrow = T) %>% 
    as.data.frame() %>%
    mutate(V2 = str_replace(V2, "\\)", "")) %>% 
    mutate(n_dash = str_count(V2, "-"))
  
  a$ve_adj_lb <- NA
  a$ve_adj_ub <- NA
  
  a[which(a$n_dash == 1),]$ve_adj_lb <- as.numeric(matrix(unlist(strsplit((a[which(a$n_dash == 1),])$V2, "-")), ncol = 2, byrow = T)[,1])
  a[which(a$n_dash == 1),]$ve_adj_ub <- as.numeric(matrix(unlist(strsplit((a[which(a$n_dash == 1),])$V2, "-")), ncol = 2, byrow = T)[,2])
  
  if(2 %in% c(a$n_dash)){
    # when there is lb < 0 
    a[which(a$n_dash == 2),]$ve_adj_lb <- - as.numeric(matrix(unlist(strsplit(sub(".", "", (a[which(a$n_dash == 2),])$V2), "-")), ncol = 2, byrow = T)[,1])
    a[which(a$n_dash == 2),]$ve_adj_ub <- + as.numeric(matrix(unlist(strsplit(sub(".", "", (a[which(a$n_dash == 2),])$V2), "-")), ncol = 2, byrow = T)[,2])
  }
  
  a <- a %>% dplyr::rename(ve_adj_median = V1) %>% dplyr::select(-V2, -n_dash)
  
  df.plot.broken <- cbind(df.plot, a) %>% mutate(across(starts_with("ve_adj_"), ~ as.numeric(.)))
  
  return(df.plot.broken)
}


### Function to plot all VE in literature and compare with current VE
plot_ve <- function(df.outcome){
  
  # first, plot the left panel
  outcome <- df.outcome$outcome_group[1]
  df.plot <- df.outcome %>% arrange(VE_mean) %>% dplyr::select(-outcome) %>%
    mutate(outcome = NA) %>% dplyr::select(outcome, studyID, study_type, country, time_range, ve_adj, VE_mean, VE_lb, VE_ub)
    
  df.plot <- 
    rbind(
      data.frame(
        outcome = c(outcome,NA),
        studyID = c(NA, "Study"),
        study_type = c(NA, "Design"),
        country = c(NA, "Country"),
        time_range = c(NA, "Time"),
        ve_adj = c(NA, "Effectiveness (95%CI)"),
        VE_mean = c(NA,NA), 
        VE_lb = c(NA,NA), 
        VE_ub = c(NA,NA)
      ),
      df.plot
    )
  
  # mark if arrows are needed 
  df.plot <- df.plot %>% 
    mutate(add_arrow = ifelse(VE_lb < -25, 1, 0)) %>%
    mutate(ve_index = factor(1:nrow(df.plot), levels = as.character(nrow(df.plot):1))) %>%
    mutate(VE_lb = ifelse(is.na(VE_lb), VE_mean, VE_lb), # some are missing lb and ub
           VE_ub = ifelse(is.na(VE_ub), VE_mean, VE_ub)) %>%
    mutate(VE_mean = as.numeric(VE_mean),
           VE_lb = as.numeric(VE_lb),
           VE_ub = as.numeric(VE_ub))
  
  # plot bold column name separately
  df.plot.colname <- df.plot %>% filter(studyID == "Study")
  # plot other rows
  df.plot.body <- df.plot %>% filter(studyID != "Study" | is.na(studyID))
  
  # current study 
  df.plot.current <- df.plot %>% 
    mutate(ve_index = factor(1:nrow(df.plot), levels = as.character(nrow(df.plot):1))) %>%
    filter(studyID == "Current study")
  
  # define the position of each column
  x_pos <- c(0.05, 0.25, 0.38, 0.50, 0.62)
  
  
  # making the left plot 
  plt.left <-
    ggplot() +
    # outcome name
    geom_text(aes(x = 0, y = ve_index, label = outcome), hjust = 0, fontface= c('bold'), lineheight = 0.8, data = df.plot) +
    # plot the colname (bold)
    geom_text(aes(x = x_pos[1], y = ve_index, label = studyID), hjust = 0, fontface= c('bold'), lineheight = .75, data = df.plot.colname) +
    geom_text(aes(x = x_pos[2], y = ve_index, label = study_type), hjust = 0, fontface= c('bold'), lineheight = .75, data = df.plot.colname) +
    geom_text(aes(x = x_pos[3], y = ve_index, label = country), hjust = 0, fontface= c('bold'), lineheight = .75, data = df.plot.colname) +
    geom_text(aes(x = x_pos[4], y = ve_index, label = time_range), hjust = 0, fontface= c('bold'), lineheight = .75, data = df.plot.colname) +
    geom_text(aes(x = x_pos[5], y = ve_index, label = ve_adj), hjust = 0, fontface= c('bold'), lineheight = .75, data = df.plot.colname) +
    # plot the body (unbold)
    geom_text(aes(x = x_pos[1], y = ve_index, label = studyID), hjust = 0, lineheight = .75, data = df.plot.body) +
    geom_text(aes(x = x_pos[2], y = ve_index, label = study_type), hjust = 0, lineheight = .75, data = df.plot.body) +
    geom_text(aes(x = x_pos[3], y = ve_index, label = country), hjust = 0, lineheight = .75, data = df.plot.body) +
    geom_text(aes(x = x_pos[4], y = ve_index, label = time_range), hjust = 0, lineheight = .75, data = df.plot.body) +
    geom_text(aes(x = x_pos[5], y = ve_index, label = ve_adj), hjust = 0, lineheight = .75, data = df.plot.body) +
    theme_void() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.line.y = element_blank(),
          axis.line.x = element_blank(),
          axis.line = element_line(colour = "black"),
          plot.margin = unit(c(0, 0, 0, 0), "cm")) +
    coord_cartesian(xlim = c(0, 0.78)) +
    # add shaded areas for current studies
    geom_rect(data = df.plot.current, aes(xmin = -Inf, xmax = Inf, ymin = as.numeric(ve_index)-0.5, ymax = as.numeric(ve_index) + 0.5), 
              fill = "grey", alpha = 0.3)
    
    
  
  # reverse the index to align righ and left plot
  df.plot <- df.plot %>% 
    mutate(ve_index = factor(1:nrow(df.plot), levels = as.character(nrow(df.plot):1)))

  df.plot.arrow <- df.plot %>% filter(add_arrow == 1)
  df.plot.arrow.current <-   df.plot.arrow %>% filter(str_detect(studyID, "Current study"))
  df.plot.current <- df.plot %>% filter(str_detect(studyID, "Current study"))
  
  # filter out clinical trials and current study
  df.trial <- df.plot %>% filter(study_type == "trial")
  df.current <- df.plot %>% filter(str_detect(studyID, "Current study"))
  
  
  plt.right <- 
    ggplot(aes(y = ve_index), data = df.plot) +
    geom_point(aes(x= VE_mean), shape=15, size=3, data = df.plot) +
    geom_linerange(aes(xmin = VE_lb, xmax = VE_ub), data = df.plot)  +
    geom_segment(aes(x = VE_ub, xend = -25,
                     y = ve_index, yend = ve_index), data = df.plot.arrow,
                 arrow = arrow(length = unit(2, "mm"))) +
    # mark trial as gold squares
    geom_point(aes(x= VE_mean), shape=15, size=3, data = df.trial, color = "gold2") +
    geom_linerange(aes(xmin = VE_lb, xmax = VE_ub), data = df.trial, color = "gold2")  +
    # mark current study as blue circles
    geom_point(aes(x= VE_mean), shape=16, size=4.5, data = df.current, color = "#0000CD") +
    geom_linerange(aes(xmin = VE_lb, xmax = VE_ub), data = df.current, color = "#0000CD")  +
    geom_segment(aes(x = VE_ub, xend = -25,
                     y = ve_index, yend = ve_index), data = df.plot.arrow.current, color = "#0000CD",
                 arrow = arrow(length = unit(2, "mm"))) +
    geom_vline(xintercept = 0, linetype = "dashed") + 
    xlab("Effectiveness of Nirsevimab (%)") +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.ticks.length.y = unit(0, "pt"),
          axis.line.y = element_blank(),
          axis.line = element_line(colour = "black"),
          plot.margin = unit(c(0, 0, 0, 0), "cm")) +
    scale_x_continuous(limits = c(-25, 100),
                        breaks = c(-25, 0, 25, 50, 75, 100),
                       labels = c("-25", "0", "25", "50", "75", "100")) +
    # add shaded areas for current studies
    geom_rect(data = df.plot.current, aes(xmin = -Inf, xmax = Inf, ymin = as.numeric(ve_index)-0.5, ymax = as.numeric(ve_index) + 0.5), 
              fill = "grey", alpha = 0.3)
  
  layout <- c(
    area(t = 0, l = 0, b = 30, r = 7), # left plot, starts at the top of the page (0) and goes 30 units down and 3 units to the right
    area(t = 1, l = 8, b = 30, r = 9)  # right plot starts a little lower (t=1) because there's no title. starts 1 unit right of the left plot (l=4, whereas left plot is r=3), goes to the bottom of the page (30 units), and 6 units further over from the left plot (r=9 whereas left plot is r=3)
  )
  
  # final plot arrangement
  plt <- plt.left +   plt.right + plot_layout(design = layout)
  
  return(plt)
  
  
  
}

### functuon to plot VE stratified by time range
plot_ve_stratified <- function(df.outcome){
  
  # first, plot the left panel
  outcome <- df.outcome$outcome_group[1]
  df.plot <- df.outcome %>% arrange(VE_mean) %>% dplyr::select(-outcome) %>%
    mutate(outcome = NA) %>% mutate(time_range_colname = NA) %>%
    dplyr::select(outcome, time_range_colname, studyID, study_type, country, time_range, time_range_cat, ve_adj, VE_mean, VE_lb, VE_ub)
  
  # arrange by full season, peak season, off season
  df.plot.full <- df.plot %>% filter(time_range_cat == "Full season") %>% dplyr::select(-time_range_cat)
  df.plot.peak <- df.plot %>% filter(time_range_cat == "Peak months") %>% dplyr::select(-time_range_cat)
  df.plot.off <- df.plot %>% filter(time_range_cat == "Off-peak months") %>% dplyr::select(-time_range_cat)
  
  # add subgroup title row 
  df.plot.full <- rbind(
    data.frame(
      outcome = NA, time_range_colname = "Full season", studyID = NA, study_type = NA, country = NA, time_range = NA, ve_adj = NA, VE_mean = NA, VE_lb = NA, VE_ub = NA
    ), df.plot.full 
  )
  
  df.plot.peak <- rbind(
    data.frame(
      outcome = NA, time_range_colname = "Peak months", studyID = NA, study_type = NA, country = NA, time_range = NA, ve_adj = NA, VE_mean = NA, VE_lb = NA, VE_ub = NA
    ), df.plot.peak
  )
  
  df.plot.off <- rbind(
    data.frame(
      outcome = NA, time_range_colname = "Off-peak months", studyID = NA, study_type = NA, country = NA, time_range = NA, ve_adj = NA, VE_mean = NA, VE_lb = NA, VE_ub = NA
    ), df.plot.off
  )
  
  df.plot <- rbind(df.plot.full, df.plot.peak, df.plot.off)
  
  
  df.plot <- 
    rbind(
      data.frame(
        outcome = c(outcome,NA),
        time_range_colname = c(NA, NA),
        studyID = c(NA, "Study"),
        study_type = c(NA, "Design"),
        country = c(NA, "Country"),
        time_range = c(NA, "Time"),
        ve_adj = c(NA, "Effectiveness (95%CI)"),
        VE_mean = c(NA,NA), 
        VE_lb = c(NA,NA), 
        VE_ub = c(NA,NA)
      ),
      df.plot
    )
  
  # mark if arrows are needed 
  df.plot <- df.plot %>% 
    mutate(add_arrow = ifelse(VE_lb < -25, 1, 0)) %>%
    mutate(ve_index = factor(1:nrow(df.plot), levels = as.character(nrow(df.plot):1))) %>%
    mutate(VE_lb = ifelse(is.na(VE_lb), VE_mean, VE_lb), # some are missing lb and ub
           VE_ub = ifelse(is.na(VE_ub), VE_mean, VE_ub)) %>%
    mutate(VE_mean = as.numeric(VE_mean),
           VE_lb = as.numeric(VE_lb),
           VE_ub = as.numeric(VE_ub))
  
  # plot bold column name separately
  df.plot.colname <- df.plot %>% filter(studyID == "Study")
  # plot other rows
  df.plot.body <- df.plot %>% filter(studyID != "Study" | is.na(studyID))
  
  # current study 
  df.plot.current <- df.plot %>% 
    mutate(ve_index = factor(1:nrow(df.plot), levels = as.character(nrow(df.plot):1))) %>%
    filter(studyID == "Current study")
  
  # define the position of each column
  x_pos <- c(0.12, 0.27, 0.4, 0.53, 0.65)
  
  
  # making the left plot 
  plt.left <-
    ggplot() +
    # outcome and time range strata name
    geom_text(aes(x = 0, y = ve_index, label = outcome), hjust = 0, fontface= c('bold'), lineheight = 0.8, data = df.plot) +
    geom_text(aes(x = 0.015, y = ve_index, label = time_range_colname), hjust = 0, fontface= c('bold'), lineheight = 0.8, data = df.plot) +
    # plot the colname (bold)
    geom_text(aes(x = x_pos[1], y = ve_index, label = studyID), hjust = 0, fontface= c('bold'), lineheight = .75, data = df.plot.colname) +
    geom_text(aes(x = x_pos[2], y = ve_index, label = study_type), hjust = 0, fontface= c('bold'), lineheight = .75, data = df.plot.colname) +
    geom_text(aes(x = x_pos[3], y = ve_index, label = country), hjust = 0, fontface= c('bold'), lineheight = .75, data = df.plot.colname) +
    geom_text(aes(x = x_pos[4], y = ve_index, label = time_range), hjust = 0, fontface= c('bold'), lineheight = .75, data = df.plot.colname) +
    geom_text(aes(x = x_pos[5], y = ve_index, label = ve_adj), hjust = 0, fontface= c('bold'), lineheight = .75, data = df.plot.colname) +
    # plot the body (unbold)
    geom_text(aes(x = x_pos[1], y = ve_index, label = studyID), hjust = 0, lineheight = .75, data = df.plot.body) +
    geom_text(aes(x = x_pos[2], y = ve_index, label = study_type), hjust = 0, lineheight = .75, data = df.plot.body) +
    geom_text(aes(x = x_pos[3], y = ve_index, label = country), hjust = 0, lineheight = .75, data = df.plot.body) +
    geom_text(aes(x = x_pos[4], y = ve_index, label = time_range), hjust = 0, lineheight = .75, data = df.plot.body) +
    geom_text(aes(x = x_pos[5], y = ve_index, label = ve_adj), hjust = 0, lineheight = .75, data = df.plot.body) +
    theme_void() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.line.y = element_blank(),
          axis.line.x = element_blank(),
          axis.line = element_line(colour = "black"),
          plot.margin = unit(c(0, 0, 0, 0), "cm")) +
    coord_cartesian(xlim = c(0, 0.8)) +
    # add shaded areas for current studies
    geom_rect(data = df.plot.current, aes(xmin = -Inf, xmax = Inf, ymin = as.numeric(ve_index)-0.5, ymax = as.numeric(ve_index) + 0.5), 
              fill = "grey", alpha = 0.3)
  
  
  
  # reverse the index to align righ and left plot
  df.plot <- df.plot %>% 
    mutate(ve_index = factor(1:nrow(df.plot), levels = as.character(nrow(df.plot):1)))
  
  df.plot.arrow <- df.plot %>% filter(add_arrow == 1)
  df.plot.arrow.current <-   df.plot.arrow %>% filter(str_detect(studyID, "Current study"))
  df.plot.current <- df.plot %>% filter(str_detect(studyID, "Current study"))
  
  # filter out clinical trials and current study
  df.trial <- df.plot %>% filter(study_type == "trial")
  df.current <- df.plot %>% filter(str_detect(studyID, "Current study"))
  
  
  plt.right <- 
    ggplot(aes(y = ve_index), data = df.plot) +
    geom_point(aes(x= VE_mean), shape=15, size=3, data = df.plot) +
    geom_linerange(aes(xmin = VE_lb, xmax = VE_ub), data = df.plot)  +
    geom_segment(aes(x = VE_ub, xend = -25,
                     y = ve_index, yend = ve_index), data = df.plot.arrow,
                 arrow = arrow(length = unit(2, "mm"))) +
    # mark trial as gold squares
    geom_point(aes(x= VE_mean), shape=15, size=3, data = df.trial, color = "gold2") +
    geom_linerange(aes(xmin = VE_lb, xmax = VE_ub), data = df.trial, color = "gold2")  +
    # mark current study as blue circles
    geom_point(aes(x= VE_mean), shape=16, size=4.5, data = df.current, color = "#0000CD") +
    geom_linerange(aes(xmin = VE_lb, xmax = VE_ub), data = df.current, color = "#0000CD")  +
    geom_segment(aes(x = VE_ub, xend = -25,
                     y = ve_index, yend = ve_index), data = df.plot.arrow.current, color = "#0000CD",
                 arrow = arrow(length = unit(2, "mm"))) +
    geom_vline(xintercept = 0, linetype = "dashed") + 
    xlab("Effectiveness of Nirsevimab (%)") +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.ticks.length.y = unit(0, "pt"),
          axis.line.y = element_blank(),
          axis.line = element_line(colour = "black"),
          plot.margin = unit(c(0, 0, 0, 0), "cm")) +
    scale_x_continuous(limits = c(-25, 100),
                       breaks = c(-25, 0, 25, 50, 75, 100),
                       labels = c("-25", "0", "25", "50", "75", "100")) +
    # add shaded areas for current studies
    geom_rect(data = df.plot.current, aes(xmin = -Inf, xmax = Inf, ymin = as.numeric(ve_index)-0.5, ymax = as.numeric(ve_index) + 0.5), 
              fill = "grey", alpha = 0.3)
  
  layout <- c(
    area(t = 0, l = 0, b = 30, r = 7), # left plot, starts at the top of the page (0) and goes 30 units down and 3 units to the right
    area(t = 1, l = 8, b = 30, r = 9)  # right plot starts a little lower (t=1) because there's no title. starts 1 unit right of the left plot (l=4, whereas left plot is r=3), goes to the bottom of the page (30 units), and 6 units further over from the left plot (r=9 whereas left plot is r=3)
  )
  
  # final plot arrangement
  plt <- plt.left +   plt.right + plot_layout(design = layout)
  
  return(plt)
  
  
  
}


### functuon to plot VE stratified by time range
# this function extended axis to -60 rather than -25
plot_ve_stratified_extend <- function(df.outcome){
  
  # first, plot the left panel
  outcome <- df.outcome$outcome_group[1]
  df.plot <- df.outcome %>% arrange(VE_mean) %>% dplyr::select(-outcome) %>%
    mutate(outcome = NA) %>% mutate(time_range_colname = NA) %>%
    dplyr::select(outcome, time_range_colname, studyID, study_type, country, time_range, time_range_cat, ve_adj, VE_mean, VE_lb, VE_ub)
  
  # arrange by full season, peak season, off season
  df.plot.full <- df.plot %>% filter(time_range_cat == "Full season") %>% dplyr::select(-time_range_cat)
  df.plot.peak <- df.plot %>% filter(time_range_cat == "Peak months") %>% dplyr::select(-time_range_cat)
  df.plot.off <- df.plot %>% filter(time_range_cat == "Off-peak months") %>% dplyr::select(-time_range_cat)
  
  # add subgroup title row 
  df.plot.full <- rbind(
    data.frame(
      outcome = NA, time_range_colname = "Full season", studyID = NA, study_type = NA, country = NA, time_range = NA, ve_adj = NA, VE_mean = NA, VE_lb = NA, VE_ub = NA
    ), df.plot.full 
  )
  
  df.plot.peak <- rbind(
    data.frame(
      outcome = NA, time_range_colname = "Peak months", studyID = NA, study_type = NA, country = NA, time_range = NA, ve_adj = NA, VE_mean = NA, VE_lb = NA, VE_ub = NA
    ), df.plot.peak
  )
  
  df.plot.off <- rbind(
    data.frame(
      outcome = NA, time_range_colname = "Off-peak months", studyID = NA, study_type = NA, country = NA, time_range = NA, ve_adj = NA, VE_mean = NA, VE_lb = NA, VE_ub = NA
    ), df.plot.off
  )
  
  df.plot <- rbind(df.plot.full, df.plot.peak, df.plot.off)
  
  
  df.plot <- 
    rbind(
      data.frame(
        outcome = c(outcome,NA),
        time_range_colname = c(NA, NA),
        studyID = c(NA, "Study"),
        study_type = c(NA, "Design"),
        country = c(NA, "Country"),
        time_range = c(NA, "Time"),
        ve_adj = c(NA, "Effectiveness (95%CI)"),
        VE_mean = c(NA,NA), 
        VE_lb = c(NA,NA), 
        VE_ub = c(NA,NA)
      ),
      df.plot
    )
  
  # mark if arrows are needed 
  df.plot <- df.plot %>% 
    mutate(add_arrow = ifelse(VE_lb < -75, 1, 0)) %>%
    mutate(ve_index = factor(1:nrow(df.plot), levels = as.character(nrow(df.plot):1))) %>%
    mutate(VE_lb = ifelse(is.na(VE_lb), VE_mean, VE_lb), # some are missing lb and ub
           VE_ub = ifelse(is.na(VE_ub), VE_mean, VE_ub)) %>%
    mutate(VE_mean = as.numeric(VE_mean),
           VE_lb = as.numeric(VE_lb),
           VE_ub = as.numeric(VE_ub))
  
  # plot bold column name separately
  df.plot.colname <- df.plot %>% filter(studyID == "Study")
  # plot other rows
  df.plot.body <- df.plot %>% filter(studyID != "Study" | is.na(studyID))
  
  # current study 
  df.plot.current <- df.plot %>% 
    mutate(ve_index = factor(1:nrow(df.plot), levels = as.character(nrow(df.plot):1))) %>%
    filter(studyID == "Current study")
  
  # define the position of each column
  x_pos <- c(0.12, 0.27, 0.4, 0.53, 0.65)
  
  
  # making the left plot 
  plt.left <-
    ggplot() +
    # outcome and time range strata name
    geom_text(aes(x = 0, y = ve_index, label = outcome), hjust = 0, fontface= c('bold'), lineheight = 0.8, data = df.plot) +
    geom_text(aes(x = 0.015, y = ve_index, label = time_range_colname), hjust = 0, fontface= c('bold'), lineheight = 0.8, data = df.plot) +
    # plot the colname (bold)
    geom_text(aes(x = x_pos[1], y = ve_index, label = studyID), hjust = 0, fontface= c('bold'), lineheight = .75, data = df.plot.colname) +
    geom_text(aes(x = x_pos[2], y = ve_index, label = study_type), hjust = 0, fontface= c('bold'), lineheight = .75, data = df.plot.colname) +
    geom_text(aes(x = x_pos[3], y = ve_index, label = country), hjust = 0, fontface= c('bold'), lineheight = .75, data = df.plot.colname) +
    geom_text(aes(x = x_pos[4], y = ve_index, label = time_range), hjust = 0, fontface= c('bold'), lineheight = .75, data = df.plot.colname) +
    geom_text(aes(x = x_pos[5], y = ve_index, label = ve_adj), hjust = 0, fontface= c('bold'), lineheight = .75, data = df.plot.colname) +
    # plot the body (unbold)
    geom_text(aes(x = x_pos[1], y = ve_index, label = studyID), hjust = 0, lineheight = .75, data = df.plot.body) +
    geom_text(aes(x = x_pos[2], y = ve_index, label = study_type), hjust = 0, lineheight = .75, data = df.plot.body) +
    geom_text(aes(x = x_pos[3], y = ve_index, label = country), hjust = 0, lineheight = .75, data = df.plot.body) +
    geom_text(aes(x = x_pos[4], y = ve_index, label = time_range), hjust = 0, lineheight = .75, data = df.plot.body) +
    geom_text(aes(x = x_pos[5], y = ve_index, label = ve_adj), hjust = 0, lineheight = .75, data = df.plot.body) +
    theme_void() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.line.y = element_blank(),
          axis.line.x = element_blank(),
          axis.line = element_line(colour = "black"),
          plot.margin = unit(c(0, 0, 0, 0), "cm")) +
    coord_cartesian(xlim = c(0, 0.8)) +
    # add shaded areas for current studies
    geom_rect(data = df.plot.current, aes(xmin = -Inf, xmax = Inf, ymin = as.numeric(ve_index)-0.5, ymax = as.numeric(ve_index) + 0.5), 
              fill = "grey", alpha = 0.3)
  
  
  
  # reverse the index to align righ and left plot
  df.plot <- df.plot %>% 
    mutate(ve_index = factor(1:nrow(df.plot), levels = as.character(nrow(df.plot):1)))
  
  df.plot.arrow <- df.plot %>% filter(add_arrow == 1)
  df.plot.arrow.current <-   df.plot.arrow %>% filter(str_detect(studyID, "Current study"))
  df.plot.current <- df.plot %>% filter(str_detect(studyID, "Current study"))
  
  # filter out clinical trials and current study
  df.trial <- df.plot %>% filter(study_type == "trial")
  df.current <- df.plot %>% filter(str_detect(studyID, "Current study"))
  
  
  plt.right <- 
    ggplot(aes(y = ve_index), data = df.plot) +
    geom_point(aes(x= VE_mean), shape=15, size=3, data = df.plot) +
    geom_linerange(aes(xmin = VE_lb, xmax = VE_ub), data = df.plot)  +
    geom_segment(aes(x = VE_ub, xend = -75,
                     y = ve_index, yend = ve_index), data = df.plot.arrow,
                 arrow = arrow(length = unit(2, "mm"))) +
    # mark trial as gold squares
    geom_point(aes(x= VE_mean), shape=15, size=3, data = df.trial, color = "gold2") +
    geom_linerange(aes(xmin = VE_lb, xmax = VE_ub), data = df.trial, color = "gold2")  +
    # mark current study as blue circles
    geom_point(aes(x= VE_mean), shape=16, size=4.5, data = df.current, color = "#0000CD") +
    geom_linerange(aes(xmin = VE_lb, xmax = VE_ub), data = df.current, color = "#0000CD")  +
    geom_segment(aes(x = VE_ub, xend = -75,
                     y = ve_index, yend = ve_index), data = df.plot.arrow.current, color = "#0000CD",
                 arrow = arrow(length = unit(2, "mm"))) +
    geom_vline(xintercept = 0, linetype = "dashed") + 
    xlab("Effectiveness of Nirsevimab (%)") +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.ticks.length.y = unit(0, "pt"),
          axis.line.y = element_blank(),
          axis.line = element_line(colour = "black"),
          plot.margin = unit(c(0, 0, 0, 0), "cm")) +
    scale_x_continuous(limits = c(-75, 100),
                       breaks = c(-75, -50, -25, 0, 25, 50, 75, 100),
                       labels = c("-75", "-50", "-25", "0", "25", "50", "75", "100")) +
    # add shaded areas for current studies
    geom_rect(data = df.plot.current, aes(xmin = -Inf, xmax = Inf, ymin = as.numeric(ve_index)-0.5, ymax = as.numeric(ve_index) + 0.5), 
              fill = "grey", alpha = 0.3)
  
  layout <- c(
    area(t = 0, l = 0, b = 30, r = 7), # left plot, starts at the top of the page (0) and goes 30 units down and 3 units to the right
    area(t = 1, l = 8, b = 30, r = 9)  # right plot starts a little lower (t=1) because there's no title. starts 1 unit right of the left plot (l=4, whereas left plot is r=3), goes to the bottom of the page (30 units), and 6 units further over from the left plot (r=9 whereas left plot is r=3)
  )
  
  # final plot arrangement
  plt <- plt.left +   plt.right + plot_layout(design = layout)
  
  return(plt)
  
  
  
}






#################################################################################
#### Processing the data from 24/25 season
#################################################################################
process_df_2425 <- function(df){
  
  # add/recode some variables 
  
  # month when tested and age when tested 
  df <- df %>% 
    mutate(month_when_tested = as.character(month(collection_date, label = T))) %>% 
    mutate(month_when_tested = ifelse(month_when_tested == "Sep", "Oct", month_when_tested)) %>%
    # age when tested (6m interval)
    mutate(age_at_test_in_months_cat = case_when(
      age_at_test_in_months < 6 ~ "under 6m",
      age_at_test_in_months >= 6 & age_at_test_in_months < 12 ~ "6-12m",
      age_at_test_in_months >= 12 ~ "above 1 yo" 
    )) %>%
    # age when tested (3m interval)
    mutate(age_at_test_in_months_cat_2 = case_when(
      age_at_test_in_months < 3 ~ "under 3m",
      age_at_test_in_months >= 3 & age_at_test_in_months < 6 ~ "3-6m",
      age_at_test_in_months >= 6 & age_at_test_in_months < 9 ~ "6-9m",
      age_at_test_in_months >= 9 & age_at_test_in_months < 12 ~ "9-12m",
      age_at_test_in_months >= 12 ~ "above 1 yo" 
    )) %>%
    mutate(risk_factor_atleastone = case_when(risk_factor_atleastone == 1 ~ "yes",
                                              risk_factor_atleastone == 0 ~ "no")) %>% 
    mutate(across(c(risk_factor_anemia, 
                    risk_factor_pulmonary,
                    risk_factor_cardiac,
                    risk_factor_immunodeficiency,
                    risk_factor_down,
                    #risk_factor_small_for_gestage # this has values of 1 0 NA
    ), ~ ifelse(is.na(.), "no", "yes"))) %>%
    mutate(age_at_test_in_months_cat = factor(age_at_test_in_months_cat,
                                              levels = c("under 6m",
                                                         "6-12m",
                                                         "above 1 yo"))) %>%
    mutate(age_at_test_in_months_cat_2 = factor(age_at_test_in_months_cat_2,
                                                levels = c("under 3m",
                                                           "3-6m",
                                                           "6-9m",
                                                           "9-12m",
                                                           "above 1 yo"))) %>% 
    mutate(race_ethnicity = factor(race_ethnicity,
                                   levels = c("Hispanic",
                                              "White non-Hispanic",
                                              "Black non-Hispanic",
                                              "Other non-Hispanic",
                                              "unknown")))
  

  # classify time since mab to testing 
  df <- df %>% 
    # 2-month interval
    mutate(days_btw_mab_collection_cat = 
             case_when(days_btw_mab_collection < 0 & days_btw_mab_collection > -60 ~ "0-2 months", 
                       days_btw_mab_collection <= -60 & days_btw_mab_collection > -120 ~ "2-4 months",
                       days_btw_mab_collection <= -120 ~ "4 months +",
                       is.na(days_btw_mab_collection) | days_btw_mab_collection >= 0 ~ "no mAb")) %>%
    mutate(days_btw_mab_collection_cat = factor(days_btw_mab_collection_cat,
                                                levels = c("no mAb",
                                                           "0-2 months",
                                                           "2-4 months",
                                                           "4 months +"))) %>%
    # 3-months interval
    mutate(days_btw_mab_collection_cat_2 = 
             case_when(days_btw_mab_collection < 0 & days_btw_mab_collection > -90 ~ "0-3 months", 
                       days_btw_mab_collection <= -90 & days_btw_mab_collection > -180 ~ "3-6 months",
                       days_btw_mab_collection <= -180 ~ "6 months +", 
                       is.na(days_btw_mab_collection) | days_btw_mab_collection >= 0 ~ "no mAb")) %>%
    mutate(days_btw_mab_collection_cat_2 = factor(days_btw_mab_collection_cat_2,
                                                  levels = c("no mAb",
                                                             "0-3 months",
                                                             "3-6 months",
                                                             "6 months +"))) %>%
    # 1 months interval
    mutate(days_btw_mab_collection_cat_3 = 
             case_when(days_btw_mab_collection < 0 & days_btw_mab_collection > -30 ~ "0-1 months", 
                       days_btw_mab_collection <= -30 & days_btw_mab_collection > -60 ~ "1-2 months",
                       days_btw_mab_collection <= -60 & days_btw_mab_collection > -90 ~ "2-3 months",
                       days_btw_mab_collection <= -90 & days_btw_mab_collection > -120 ~ "3-4 months",
                       days_btw_mab_collection <= -120 ~ "4 months +",
                       is.na(days_btw_mab_collection) | days_btw_mab_collection >= 0 ~ "no mAb")) %>%
    mutate(days_btw_mab_collection_cat_3 = factor(days_btw_mab_collection_cat_3,
                                                  levels = c("no mAb",
                                                             "0-1 months",
                                                             "1-2 months",
                                                             "2-3 months",
                                                             "3-4 months",
                                                             "4 months +"))) %>%
    mutate(weeks_btw_mab_collection_cat = 
             case_when(
               days_btw_mab_collection < 0 & days_btw_mab_collection > -14 ~ "(0, 2)",
               days_btw_mab_collection <= -14 & days_btw_mab_collection > -14*2 ~ "[2, 4)",
               days_btw_mab_collection <= -14*2 & days_btw_mab_collection > -14*3 ~ "[4, 6)",
               days_btw_mab_collection <= -14*3 & days_btw_mab_collection > -14*4 ~ "[6, 8)",
               days_btw_mab_collection <= -14*4 & days_btw_mab_collection > -14*5 ~ "[8, 10)",
               days_btw_mab_collection <= -14*5 & days_btw_mab_collection > -14*6 ~ "[10, 12)",
               days_btw_mab_collection <= -14*6 & days_btw_mab_collection > -14*7 ~ "[12, 14)",
               days_btw_mab_collection <= -14*7 & days_btw_mab_collection > -14*8 ~ "[14, 16)",
               days_btw_mab_collection <= -14*8 ~ "[16, )",
               is.na(days_btw_mab_collection) | days_btw_mab_collection >= 0 ~ "no mAb"
             )) %>% 
    mutate(weeks_btw_mab_collection_cat = factor(weeks_btw_mab_collection_cat,
                                                 levels = c("no mAb",
                                                            "(0, 2)", "[2, 4)", "[4, 6)", "[6, 8)",
                                                            "[8, 10)", "[10, 12)", "[12, 14)", "[14, 16)",
                                                            "[16, )"
                                                 ))) 

  
  
  
  # group month_when_tested into fewer groups
  df <- df %>% 
    mutate(month_when_tested_cat = case_when(
      collection_date >= as.Date("2024-9-30") & collection_date <= as.Date("2024-11-30") ~ "Oct-Nov",
      collection_date >= as.Date("2024-12-1") & collection_date <= as.Date("2025-1-31") ~ "Dec-Jan",
      collection_date >= as.Date("2025-2-1") & collection_date <= as.Date("2025-3-31") ~ "Feb-Mar",
      collection_date >= as.Date("2025-4-1")  ~ "April and after"
    )) %>% 
    mutate(month_when_tested_cat = factor(month_when_tested_cat,
                                          levels = c("Oct-Nov", "Dec-Jan", "Feb-Mar","April and after")))
  
  
  # dosage 
  df <- df %>% 
    mutate(rsv_mab_dose = case_when(rsv_mab_dose == 50 ~ "50mg",
                                    rsv_mab_dose == 100 ~ "100mg",
                                    TRUE ~ "no mAb"))
  
  
  # oxygen support (assume those without oxygen information are those without oxygen support)
  # df <- df %>% 
  #   mutate(highflow_oxygen = ifelse(is.na(highflow_oxygen), 0, highflow_oxygen))
  
  # there seems to be individuals not admitted to hospital but had hosp los
  
  # add confounders dummy variables 
  df <- df %>% 
    # recode confounders
    mutate(cf_birth_weight = birth_weight) %>% # about 25% missing 
    mutate(cf_age = case_when(age_at_test_in_months_cat_2 == "under 3m" ~ 1,
                              age_at_test_in_months_cat_2 == "3-6m" ~ 2,
                              age_at_test_in_months_cat_2 == "6-9m" ~ 3,
                              age_at_test_in_months_cat_2 == "9-12m" ~ 4,
                              age_at_test_in_months_cat_2 == "above 1 yo" ~ 5)) %>%
    mutate(cf_month_tested = case_when(month_when_tested_cat == "Oct-Nov" ~ 1,
                                       month_when_tested_cat == "Dec-Jan" ~ 2,
                                       month_when_tested_cat == "Feb-Mar" ~ 3,
                                       month_when_tested_cat == "April and after" ~ 4
    )) %>%
    # convert confounders to dummy variables 
    mutate(cf_age_1 = ifelse(cf_age == 1, 1, 0),
           cf_age_2 = ifelse(cf_age == 2, 1, 0),
           cf_age_3 = ifelse(cf_age == 3, 1, 0),
           cf_age_4 = ifelse(cf_age == 4, 1, 0), 
           cf_age_5 = ifelse(cf_age == 5, 1, 0)) %>%
    mutate(cf_month_tested_1 = ifelse(cf_month_tested == 1, 1, 0),
           cf_month_tested_2 = ifelse(cf_month_tested == 2, 1, 0),
           cf_month_tested_3 = ifelse(cf_month_tested == 3, 1, 0),
           cf_month_tested_4 = ifelse(cf_month_tested == 4, 1, 0)) %>%
    mutate(cf_insurance_1 = ifelse(insurance_type == "private", 1, 0),
           cf_insurance_2 = ifelse(insurance_type == "public", 1, 0),
           cf_insurance_3 = ifelse(insurance_type == "uninsured", 1, 0)) %>%
    # having at least one risk factor
    mutate(cf_onerf = ifelse(risk_factor_atleastone == "yes", 1, 0))
  
  return(df)
  
}


# function to process posterior samples in the waning VE analysis 
process_waning_samples <- function(samples, 
                                   outcome,
                                   iterations){
  
  post.chain1 <- as.data.frame(as.matrix(samples[[1]]))  
  post.chain2 <- as.data.frame(as.matrix(samples[[2]])) 
  post.chain3 <- as.data.frame(as.matrix(samples[[3]])) 
  post.allchains <- as.data.frame(bind_rows(post.chain1, post.chain2, post.chain3)) 
  
  ve.median.1 <- round((1 - exp(as.numeric(median(post.allchains$beta1))))*100,1)
  ve.lb.1 <- round((1 - exp(as.numeric(quantile(post.allchains$beta1, 0.975)))) *100,1)
  ve.ub.1 <- round((1 - exp(as.numeric(quantile(post.allchains$beta1, 0.025))))*100, 1)
  
  ve.median.2 <- round((1 - exp(as.numeric(median(post.allchains$beta2))))*100,1)
  ve.lb.2 <- round((1 - exp(as.numeric(quantile(post.allchains$beta2, 0.975)))) *100,1)
  ve.ub.2 <- round((1 - exp(as.numeric(quantile(post.allchains$beta2, 0.025))))*100, 1)
  
  ve.median.3 <- round((1 - exp(as.numeric(median(post.allchains$beta3))))*100,1)
  ve.lb.3 <- round((1 - exp(as.numeric(quantile(post.allchains$beta3, 0.975)))) *100,1)
  ve.ub.3 <- round((1 - exp(as.numeric(quantile(post.allchains$beta3, 0.025))))*100, 1)
  
  ve.median.4 <- round((1 - exp(as.numeric(median(post.allchains$beta4))))*100,1)
  ve.lb.4 <- round((1 - exp(as.numeric(quantile(post.allchains$beta4, 0.975)))) *100,1)
  ve.ub.4 <- round((1 - exp(as.numeric(quantile(post.allchains$beta4, 0.025))))*100, 1)
  
  ve.median.5 <- round((1 - exp(as.numeric(median(post.allchains$beta5))))*100,1)
  ve.lb.5 <- round((1 - exp(as.numeric(quantile(post.allchains$beta5, 0.975)))) *100,1)
  ve.ub.5 <- round((1 - exp(as.numeric(quantile(post.allchains$beta5, 0.025))))*100, 1)
  
  ve.median.6 <- round((1 - exp(as.numeric(median(post.allchains$beta6))))*100,1)
  ve.lb.6 <- round((1 - exp(as.numeric(quantile(post.allchains$beta6, 0.975)))) *100,1)
  ve.ub.6 <- round((1 - exp(as.numeric(quantile(post.allchains$beta6, 0.025))))*100, 1)
  
  ve.median.7 <- round((1 - exp(as.numeric(median(post.allchains$beta7))))*100,1)
  ve.lb.7 <- round((1 - exp(as.numeric(quantile(post.allchains$beta7, 0.975)))) *100,1)
  ve.ub.7 <- round((1 - exp(as.numeric(quantile(post.allchains$beta7, 0.025))))*100, 1)
  
  ve.median.8 <- round((1 - exp(as.numeric(median(post.allchains$beta8))))*100,1)
  ve.lb.8 <- round((1 - exp(as.numeric(quantile(post.allchains$beta8, 0.975)))) *100,1)
  ve.ub.8 <- round((1 - exp(as.numeric(quantile(post.allchains$beta8, 0.025))))*100, 1)
  
  ve.median.9 <- round((1 - exp(as.numeric(median(post.allchains$beta9))))*100,1)
  ve.lb.9 <- round((1 - exp(as.numeric(quantile(post.allchains$beta9, 0.975)))) *100,1)
  ve.ub.9 <- round((1 - exp(as.numeric(quantile(post.allchains$beta9, 0.025))))*100, 1)
  
  
  # look at the result
  ve.biwkinterval <-
    data.frame(
      against = rep(outcome, 10),
      vax_time = c("not vaccinated",
                   "(0, 2)",
                   "[2, 4)",
                   "[4, 6)",
                   "[6, 8)",
                   "[8, 10)",
                   "[10, 12)",
                   "[12, 14)",
                   "[14, 16)",
                   "[16, )"),
      VE.adjusted = c(
        "REF",
        paste0(ve.median.1, " (", ve.lb.1, "-", ve.ub.1, ")"),
        paste0(ve.median.2, " (", ve.lb.2, "-", ve.ub.2, ")"),
        paste0(ve.median.3, " (", ve.lb.3, "-", ve.ub.3, ")"),
        paste0(ve.median.4, " (", ve.lb.4, "-", ve.ub.4, ")"),
        paste0(ve.median.5, " (", ve.lb.5, "-", ve.ub.5, ")"),
        paste0(ve.median.6, " (", ve.lb.6, "-", ve.ub.6, ")"),
        paste0(ve.median.7, " (", ve.lb.7, "-", ve.ub.7, ")"),
        paste0(ve.median.8, " (", ve.lb.8, "-", ve.ub.8, ")"),
        paste0(ve.median.9, " (", ve.lb.9, "-", ve.ub.9, ")")
      )
    ) %>%
    cbind(
      table(df.jags$weeks_btw_mab_collection_cat, df.jags$case_control) %>%
        matrix(ncol = 2)
    ) %>%
    dplyr::rename(n_cases = `2`, n_controls = `1`)
  
  # for plotting
  df.ve.biwkinterval <-
    data.frame(
      against = rep(outcome, 9),
      vax_time = c("(0, 2)",
                   "[2, 4)",
                   "[4, 6)",
                   "[6, 8)",
                   "[8, 10)",
                   "[10, 12)",
                   "[12, 14)",
                   "[14, 16)",
                   "[16, )"),
      VE.median = c(ve.median.1, ve.median.2, ve.median.3, ve.median.4, ve.median.5, ve.median.6,
                    ve.median.7, ve.median.8, ve.median.9),
      VE.lb = c(ve.lb.1, ve.lb.2, ve.lb.3, ve.lb.4, ve.lb.5,
                ve.lb.6, ve.lb.7, ve.lb.8, ve.lb.9),
      VE.ub = c(ve.ub.1, ve.ub.2, ve.ub.3, ve.ub.4, ve.ub.5,
                ve.ub.6, ve.ub.7, ve.ub.8, ve.ub.9)
    )
  
  # for traceplot
  post_traceplot <- rbind(
    post.chain1 %>% mutate(chain = 1, iter = 1:iterations),
    post.chain2 %>% mutate(chain = 2, iter = 1:iterations),
    post.chain3 %>% mutate(chain = 3, iter = 1:iterations)
  ) %>% mutate(outcome = outcome)
  
  result.list <- list(
    ve.biwkinterval = ve.biwkinterval, # VE summary
    df.ve.biwkinterval = df.ve.biwkinterval, # for plotting VE
    post = post_traceplot # for plotting traceplot
  )
  
  return(result.list)
  
}





################################################################################################################
# Maternal vaccine VE functions 
################################################################################################################


###############################
# Functions for VE estimation (regression models)
###############################

## unmatched  model (unstratified)
regress_maternal <- function(df.ve,
                            confounders){
  
  # get n of cases and controls for unadjusted analysis
  n.cases.mv <- df.ve %>% filter(case_control == 1 & mom_rsv_vaxed == 1) %>% nrow()
  n.cases.nomv <- df.ve %>% filter(case_control == 1 & mom_rsv_vaxed == 0) %>% nrow()
  n.controls.mv <- df.ve %>% filter(case_control == 0 & mom_rsv_vaxed == 1) %>% nrow()
  n.controls.nomv <- df.ve %>% filter(case_control == 0 & mom_rsv_vaxed == 0) %>% nrow()
  
  # get n of cases and controls for adjusted analysis
  n.cases.mv.adj <-  df.ve[, c("case_control", "mom_rsv_vaxed", confounders)] %>% 
    drop_na() %>% filter(case_control == 1 & mom_rsv_vaxed == 1) %>% nrow()
  n.cases.nomv.adj <-  df.ve[, c("case_control",  "mom_rsv_vaxed", confounders)] %>% 
    drop_na() %>% filter(case_control == 1 & mom_rsv_vaxed == 0) %>% nrow()
  n.controls.mv.adj <- df.ve[, c("case_control",  "mom_rsv_vaxed", confounders)] %>% 
    drop_na() %>% filter(case_control == 0 & mom_rsv_vaxed == 1) %>% nrow()
  n.controls.nomv.adj <- df.ve[, c("case_control",  "mom_rsv_vaxed", confounders)] %>% 
    drop_na() %>% filter(case_control == 0 & mom_rsv_vaxed == 0) %>% nrow()
  
  
  # unadjusted
  formula <- as.formula("case_control ~ mom_rsv_vaxed") 
  model <- glm(formula, data = df.ve, family = "binomial") 
  ve.median.unadj <- 1 - exp(model$coefficients[2])
  ve.ub.unadj <- 1 - exp(confint(model)[2,][1])
  ve.lb.unadj <- 1 - exp(confint(model)[2,][2])
  
  # adjusted
  formula <- as.formula(paste(c("case_control ~ mom_rsv_vaxed", confounders), collapse = " + ")) 
  model <- glm(formula, data = df.ve, family = "binomial") 
  ve.median.adj <- 1 - exp(model$coefficients[2])
  ve.ub.adj <- 1 - exp(confint(model)[2,][1])
  ve.lb.adj <- 1 - exp(confint(model)[2,][2])
  
  # make a table
  ve.results <- data.frame(
    n_cases_mv = paste0(n.cases.mv, " (", n.cases.mv.adj, ")"),
    n_cases_nomv = paste0(n.cases.nomv, " (", n.cases.nomv.adj, ")"),
    n_controls_mv = paste0(n.controls.mv, " (", n.controls.mv.adj, ")"),
    n_controls_nomv = paste0(n.controls.nomv, " (", n.controls.nomv.adj, ")"),
    ve_unadj = paste0(round(ve.median.unadj*100,1), " (", round(ve.lb.unadj*100,1), "-", round(ve.ub.unadj*100,1), ")"),
    ve_adj = paste0(round(ve.median.adj*100,1), " (", round(ve.lb.adj*100,1), "-", round(ve.ub.adj*100,1), ")")
  ) 
  
  # row.names(ve.results) <- NULL
  
  return(ve.results)
}





########################################################
# unused functions 
########################################################


# filter out subgroup and calculate VE of each subgroup
regress_unmatch_stratified <- function(df.ve,
                                       stratify_by,
                                       confounders){
  
  df.glm <- df.ve %>% filter(!is.na(get(stratify_by)))
  confounders <- confounders[confounders %in% stratify_by == F]
  
  # remove subgroups with too few records
  if(stratify_by == "risk_factor_gestage"){
    df.glm <- df.glm %>% filter(risk_factor_gestage != "post term")
  }
  
  groups <- unique(df.glm[ , stratify_by])
  # subset each subgroup
  for(i in 1:length(groups)){
    
    # filter out this subgroup
    df.glm <- df.ve %>% filter(get(stratify_by) == groups[i])
    
    
    # run unadjusted glm
    model.unadj <- glm(case_control ~ rsv_mab, data = df.glm, family = "binomial")
    
    # get unadjuested estimates for this group
    ve.median.unadj <- 1 - exp(model.unadj$coefficients[2])
    ve.ub.unadj <- 1 - exp(confint(model.unadj)[2,][1])
    ve.lb.unadj <- 1 - exp(confint(model.unadj)[2,][2])
    
    # run adjusted glm
    if(groups[i] == "preterm"){confounders <- confounders[confounders %in% "risk_factor_atleastone" == F]}
    formula <- as.formula(paste(c("case_control ~ rsv_mab", 
                                  confounders), collapse = " + "))
    model.adj <- glm(formula, data = df.glm, family = "binomial")
    
    # get unadjuested estimates for this group
    ve.median.adj <- 1 - exp(model.adj$coefficients[2])
    ve.ub.adj <- 1 - exp(confint(model.adj)[,1][2])
    ve.lb.adj <- 1 - exp(confint(model.adj)[,2][2])
    
    # get the number of case and controls
    n.cases <- df.glm %>% filter(case_control ==1 ) %>% nrow()
    n.controls <- df.glm %>% filter(case_control == 0) %>% nrow 
    n.cases.eff <- df.glm[, c("case_control", confounders)] %>% filter(case_control == 1) %>%
      filter(across(everything(), ~ !is.na(.))) %>% nrow()
    n.controls.eff <- df.glm[, c("case_control", confounders)] %>% filter(case_control == 0) %>%
      filter(across(everything(), ~ !is.na(.))) %>% nrow()
    
    # combine results 
    df.results.temp <- data.frame(
      n_overall = n.cases + n.controls,
      n_overall_eff = n.cases.eff + n.controls.eff,
      n_cases = n.cases,
      n_cases_eff = n.cases.eff,
      n_controls = n.controls, 
      n_controls_eff = n.controls.eff,
      ve_median_unadj = ve.median.unadj,
      ve_lb_unadj = ve.lb.unadj,
      ve_ub_unadj = ve.ub.unadj,
      ve_median_adj = ve.median.adj,
      ve_lb_adj = ve.lb.adj,
      ve_ub_adj = ve.ub.adj
    ) %>%
      mutate(stratify_by = stratify_by,
             strata = groups[i])
    
    if(i == 1){df.results <- df.results.temp}
    if(i != 1){df.results <- rbind(df.results, df.results.temp)}
    
  }
  
  row.names(df.results) <- NULL
  return(df.results)
  
}



regress_match_stratified <- function(df.ve, 
                                     stratify_by,
                                     confounders){
  
  df.glm <- df.ve %>% filter(!is.na(get(stratify_by)))
  confounders <- confounders[confounders %in% stratify_by == F]
  
  # remove subgroups with too few records
  if(stratify_by == "risk_factor_gestage"){
    df.glm <- df.glm %>% filter(risk_factor_gestage != "post term")
  }
  
  groups <- unique(df.glm[ , stratify_by])
  # subset each subgroup
  for(i in 1:length(groups)){
    
    # filter out this subgroup
    df.glm <- df.ve %>% filter(get(stratify_by) == groups[i])
    
    
    # run unadjusted glm
    model.unadj <- glm(case_control ~ rsv_mab + strata(id_case), data = df.glm, family = "binomial")
    
    # get unadjuested estimates for this group
    ve.median.unadj <- 1 - exp(model.unadj$coefficients[2])
    ve.ub.unadj <- 1 - exp(confint(model.unadj)[2,][1])
    ve.lb.unadj <- 1 - exp(confint(model.unadj)[2,][2])
    
    
    
    # run adjusted glm
    if(groups[i] == "preterm"){confounders <- confounders[confounders %in% "risk_factor_atleastone" == F]}
    formula <- as.formula(paste(c("case_control ~ rsv_mab + strata(id_case)", 
                                  confounders), collapse = " + "))
    model.adj <- glm(formula, data = df.glm, family = "binomial")
    
    # get unadjuested estimates for this group
    ve.median.adj <- 1 - exp(model.adj$coefficients[2])
    ve.ub.adj <- 1 - exp(confint(model.adj)[,1][2])
    ve.lb.adj <- 1 - exp(confint(model.adj)[,2][2])
    
    # get the number of case and controls
    n.cases <- df.glm %>% filter(case_control ==1 ) %>% nrow()
    n.controls <- df.glm %>% filter(case_control == 0) %>% nrow 
    n.cases.eff <- df.glm[, c("case_control", confounders)] %>% filter(case_control == 1) %>%
      filter(across(everything(), ~ !is.na(.))) %>% nrow()
    n.controls.eff <- df.glm[, c("case_control", confounders)] %>% filter(case_control == 0) %>%
      filter(across(everything(), ~ !is.na(.))) %>% nrow()
    
    # combine results 
    df.results.temp <- data.frame(
      n_overall = n.cases + n.controls,
      n_overall_eff = n.cases.eff + n.controls.eff,
      n_cases = n.cases,
      n_cases_eff = n.cases.eff,
      n_controls = n.controls, 
      n_controls_eff = n.controls.eff,
      ve_median_unadj = ve.median.unadj,
      ve_lb_unadj = ve.lb.unadj,
      ve_ub_unadj = ve.ub.unadj,
      ve_median_adj = ve.median.adj,
      ve_lb_adj = ve.lb.adj,
      ve_ub_adj = ve.ub.adj
    ) %>%
      mutate(stratify_by = stratify_by,
             strata = groups[i])
    
    if(i == 1){df.results <- df.results.temp}
    if(i != 1){df.results <- rbind(df.results, df.results.temp)}
    
  }
  
  row.names(df.results) <- NULL
  return(df.results)
  
}














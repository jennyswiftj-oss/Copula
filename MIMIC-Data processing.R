
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 0 - Load libraries   ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)
library(data.table)
library(dplyr)
library(ggplot2)
library(ggpubr)  
library(reshape2) 
library(tidyr)
library(data.table)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 1- Data loading  ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Unzip files
R.utils::gunzip("/chartevents.csv.gz", remove = TRUE)
R.utils::gunzip("/d_items.csv.gz", remove = TRUE)
R.utils::gunzip("/omr.csv.gz", remove = TRUE)
R.utils::gunzip("/patients.csv.gz", remove = TRUE)
R.utils::gunzip("/admissions.csv.gz", remove = TRUE)

# Read data (large data)
cat("start reading data \n")
icu.chartevents <- fread("/chartevents.csv")
reduced_chartevents <- icu.chartevents[, c(1, 5, 7, 8)]
rm(icu.chartevents)

core.patient <- fread("/patients.csv")
omr <- fread("/omr.csv")
icu.d_items <- fread("/d_items.csv")

save(reduced_chartevents,file="MIMIC/reduced_chartevents.Rdata")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 2- Data preparation ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Process weight and height data
omr_wide_weight_height <- omr %>%
  filter(result_name %in% c("Weight (Lbs)", "Height (Inches)")) %>%
  group_by(subject_id, chartdate, result_name) %>%
  slice_max(seq_num, n = 1) %>%                    
  ungroup() %>%
  pivot_wider(id_cols = c(subject_id, chartdate), names_from = result_name, values_from = result_value)

save(omr_wide_weight_height, file = "/omr_wide_weight_height.Rdata")

# Filter and convert weight and height
omr_wide_complete <- omr_wide_weight_height %>% 
  filter(`Height (Inches)` != "NULL") %>%
  mutate(`Weight (Lbs)` = as.numeric(`Weight (Lbs)`), `Height (Inches)` = as.numeric(`Height (Inches)`)) %>%
  rename(weight = `Weight (Lbs)`, height = `Height (Inches)`) %>%
  mutate(weight = weight * 0.453592, height = height * 0.0254) %>%
  group_by(subject_id) %>% 
  filter(chartdate == min(chartdate)) %>% 
  ungroup() %>% 
  select(subject_id, weight, height)

# Add to total_data and filter
total_data <- core.patient %>% 
  left_join(omr_wide_complete) %>%
  filter(!is.na(weight) & !is.na(height))

# Define all itemid
all_ids <- c(
  220224, 223835, 227457, 225690, 220052,  # SOFA components
  220739, 223900, 223901, 220615,          # GCS and Creatinine
  220644, 220587, 225612,                  # Liver function
  220624, 225671, 225693                   # Lipid index
)

# Loop for each itemid
for (item_id in all_ids) {
  one_event_only <- reduced_chartevents[reduced_chartevents$itemid == item_id, ] %>% 
    group_by(subject_id) %>% 
    filter(charttime == min(charttime)) %>% 
    ungroup() %>%  
    select(!!quo_name(icu.d_items$label[icu.d_items$itemid == item_id]) := value, subject_id) %>% 
    distinct(subject_id, .keep_all = TRUE)
  
  total_data <- total_data %>% left_join(one_event_only, by = "subject_id")
}

# Remove variables with excessive missing data
total_data <- total_data %>% select(-c(HDL, `LDL calculated`, Triglyceride)) 
save(total_data, file = "/total_data.Rdata")

# Delete records with all lab values missing
total_data_filtered <- total_data %>% filter(!if_all(9:20, is.na)) 

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 3- Ethnicity and age filtering ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Get race information 
admissions_race <- admissions %>%
  group_by(subject_id) %>%
  arrange(admittime) %>%  
  slice(1) %>%  
  ungroup() %>%
  select(subject_id, race) 

# Join and reclassify races and age
total_data_filtered <- total_data_filtered %>%
  left_join(admissions_race %>% select(subject_id, race), by = "subject_id") %>%
  mutate(race = case_when(
    grepl("WHITE|PORTUGUESE", race) ~ "White",
    race == "BLACK/AFRICAN AMERICAN" ~ "African American",
    grepl("HISPANIC|LATINO|SOUTH AMERICAN", race) ~ "Hispanic",
    grepl("ASIAN", race) ~ "Asian",
    grepl("AMERICAN INDIAN|ALASKA|BLACK/AFRICAN$|BLACK/CAPE VERDEAN|BLACK/CARIBBEAN|HAWAIIAN|PACIFIC ISLANDER|OTHER|MULTIPLE", race) ~ "Other race",
    TRUE ~ "Unknown"
  )) %>%
  filter(race != "Unknown") %>%
  filter(anchor_age >= 18 & anchor_age < 60)

save(total_data_filtered, file = "MIMIC/total_data_filtered.Rdata")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 4- GCS score processing ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Reshape dataset
total_data_filtered_re <- total_data_filtered %>% select(-c(1, 4, 5, 6))

# Define GCS scoring functions
score_eye_opening <- function(response) {
  case_when(
    response == "Spontaneously" ~ 4, response == "To Speech" ~ 3,      
    response == "To Pain" ~ 2, response == "None" ~ 1,           
    TRUE ~ NA_real_)
}

score_verbal_response <- function(response) {
  case_when(
    response == "Oriented" ~ 5, response == "Confused" ~ 4,                  
    response == "Inappropriate words" ~ 3, response == "Incomprehensible sounds" ~ 2,   
    response %in% c("No Response-ETT", "No response") ~ 1,               
    TRUE ~ NA_real_)
}

score_motor_response <- function(response) {
  case_when(
    response == "Obeys Commands" ~ 6, response == "Localizes Pain" ~ 5,        
    response == "Flex-withdraws" ~ 4, response == "Abnormal Flexion" ~ 3,      
    response == "Abnormal extension" ~ 2, response == "No response" ~ 1,           
    TRUE ~ NA_real_)
}

# Apply GCS scoring
total_data_filtered_re <- total_data_filtered_re %>%
  mutate(
    GCS_Eye_Score = score_eye_opening(`GCS - Eye Opening`),
    GCS_Verbal_Score = score_verbal_response(`GCS - Verbal Response`),
    GCS_Motor_Score = score_motor_response(`GCS - Motor Response`),
    GCS_Total_Score = GCS_Eye_Score + GCS_Verbal_Score + GCS_Motor_Score,
    GCS_Category = ifelse(GCS_Total_Score >= 9, "Non-severe", "Severe")
  ) %>% 
  select(-c(`GCS - Eye Opening`, `GCS - Verbal Response`, `GCS - Motor Response`, GCS_Eye_Score, GCS_Verbal_Score, GCS_Motor_Score, GCS_Total_Score))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 5- Variable renaming and recoding ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Rename the data
total_data_filtered_re <- total_data_filtered_re %>%
  rename(
    Gender = gender, Race = race, Age = anchor_age, Weight = weight, Height = height,
    SCR = `Creatinine (serum)`, ALP = `Alkaline Phosphate`, PaO2 = `Arterial O2 pressure`,
    FiO2 = `Inspired O2 Fraction`, PLT = `Platelet Count`, TBIL = `Total Bilirubin`,
    MAP = `Arterial Blood Pressure mean`
  )

# Reorder columns and convert to numeric
total_data_filtered_re <- total_data_filtered_re[, c(1, 14, 15, 2, 3:13)]
ncols <- ncol(total_data_filtered_re)
total_data_filtered_re[, 4:ncols] <- lapply(total_data_filtered_re[, 4:ncols], as.numeric)

# Recode categorical variables
total_data_filtered_re$Race <- factor(total_data_filtered_re$Race,
                                      levels = c("Hispanic", "White", "African American", "Asian", "Other race"),
                                      labels = c("1", "2", "3", "4", "5"))
total_data_filtered_re$Gender <- factor(total_data_filtered_re$Gender, levels = c("M", "F"), labels = c("1", "2")) 
total_data_filtered_re$GCS_Category <- factor(total_data_filtered_re$GCS_Category,
                                              levels = c("Non-severe", "Severe"), labels = c("1", "2"))

# Adjust FiO2 values
total_data_filtered_re <- total_data_filtered_re %>%
  mutate(FiO2 = case_when(is.na(FiO2) | FiO2 == 0 ~ NA_real_, FiO2 > 1 ~ FiO2/100, TRUE ~ FiO2))

save(total_data_filtered_re, file = "MIMIC/total_data_filtered_re.Rdata")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 6- Outlier detection and removal ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define outlier detection function
identify_extreme_outliers <- function(data, variable_name, lower_limit, upper_limit) {
  outliers_indices <- which(data[[variable_name]] < lower_limit | 
                              data[[variable_name]] > upper_limit | 
                              is.infinite(data[[variable_name]]))
  outliers_indices <- outliers_indices[!is.na(data[[variable_name]][outliers_indices])]
  
  list(variable = variable_name, outliers_indices = outliers_indices, lower_limit = lower_limit,
       upper_limit = upper_limit, count = length(outliers_indices),
       percentage = round(length(outliers_indices) / sum(!is.na(data[[variable_name]])) * 100, 2))
}

# Set threshold limits
variable_limits <- list(
  PaO2 = c(0, 1000), PLT = c(0, 2000), Height = c(0.5, 2.5), 
  Weight = c(0.5, 300), MAP = c(0, 300), FiO2 = c(0.21, 1.0)
)

# Identify and replace outliers
extreme_outliers_info <- list()
total_data_filtered_extreme_clean <- total_data_filtered_re

for (var in names(variable_limits)) {
  if (var %in% names(total_data_filtered_re)) {
    limits <- variable_limits[[var]]
    extreme_outliers_info[[var]] <- identify_extreme_outliers(total_data_filtered_re, var, limits[1], limits[2])
    outliers_idx <- extreme_outliers_info[[var]]$outliers_indices
    if (length(outliers_idx) > 0) {
      total_data_filtered_extreme_clean[[var]][outliers_idx] <- NA
    }
  }
}

save(total_data_filtered_extreme_clean, file = "MIMIC/total_data_filtered_extreme_clean.Rdata")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 7- Log transformation and final dataset ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Calculate PF ratio and remove PaO2 and FiO2
total_data_filtered_extreme_clean <- total_data_filtered_extreme_clean %>%
  mutate(PF_ratio = PaO2/FiO2) %>% 
  select(-c(7, 15))

# Log transformation
cols_to_transform <- c("ALT", "AST", "SCR", "ALP", "TBIL", "PF_ratio")

total_data_filtered_extreme_clean[, (cols_to_transform) := lapply(.SD, function(x) ifelse(!is.na(x) & x > 0, log(x), NA)), .SDcols = cols_to_transform]

# Create final dataset with renamed variables
total_data_mimic <- total_data_filtered_extreme_clean %>% 
  rename(logALT = ALT, logAST = AST, logALP = ALP, logSCR = SCR, logTBIL = TBIL, logPF_ratio = PF_ratio) 

save(total_data_mimic, file = "clean_data/total_data_mimic.Rdata")




##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 0 - Load libraries   ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyr)
library(dplyr)
library(combinat)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(ggside)
library(rvinecopulib)
library(pmxcopula)
library(patchwork)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 1 - Source files   ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Note: This code applies to both NHANES and MIMIC datasets
# Function
source("functions/calculate_metric.R") 
source("functions/calculate_frequency.R")           

# Data 
load("clean_data/nhanes_copula_whole.Rdata")  # Change to MIMIC path
load("clean_data/nhanes_data_12d_log.Rdata")  # Change to MIMIC path
load("clean_data/nhanes_sim_whole.Rdata")     # Change to MIMIC path

# Prepare observed data
obs_data <- nhanes_data_log %>% 
  select(-BMI, -Obese_condition)  # Change to select(-GCS_Category) for MIMIC

obs_data <- nhanes_data_log
combi <- permn(1:5) # 120, separate them in 6 files

i = 79
obs_data[, 1] <- ordered(obs_data$Gender, levels = c(1,2))
levels(obs_data$Gender)
obs_data[, 2] <- ordered(obs_data$Race, levels = combi[[i]])
levels(obs_data$Race)
# obs_data$GCS_Category <- ordered(obs_data$GCS_Category, levels = c(1, 2))  # MIMIC only


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 2 - Tree structure and density contours ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Plot the first tree ####
tree_1 <- plot(nhanes_copula_log$copula, 
               tree = 1, 
               var_names = "use", 
               edge_labels = "family_tau")
tree_1$labels$title <- ""

ggsave("tree_structure_1_nhanes.tiff", 
       tree_1,
       width = 12, 
       height = 8,  
       dpi = 300,
       compression = "lzw",
       bg = "white")

### Plot the second tree ####
tree_2 <- plot(nhanes_copula_log$copula,
               tree = 2,
               var_names = "use",
               edge_labels = "family")
tree_2$labels$title <- ""  

ggsave("tree_structure_2_nhanes.tiff", 
       tree_2,
       width = 12, 
       height = 8,  
       dpi = 300,
       compression = "lzw",
       bg = "white")




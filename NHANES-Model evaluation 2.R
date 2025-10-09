
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 0 - Load libraries   ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(tidyr)
library(ks)
library(sf)
library(dplyr)
library(tibble)
library(combinat)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(pmxcopula)
library(parallel)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 1 - Source files   ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Note: 
# 1. The following code structure is applied to both NHANES and MIMIC datasets
# 2. For subpopulation analysis, use the same code structure above with:
# Filter data before analysis: 
# e.g. obs_data<- morbid_obesity <- nhanes_data_log %>% 
#                 mutate(BMI = round(Weight / Height^2,1)) %>% 
#                 filter(BMI >= 40.0) %>% select(-BMI)
#      sim_data<- sim_data_morbid %>% 
#                 mutate(BMI = round(Weight / Height^2,1)) %>% 
#                 filter(BMI >= 40.0) %>% select(-BMI)

## Whole population model ##
# Load data
load("clean_data/nhanes_data_12d_log.Rdata")  # Change to MIMIC path
load("clean_data/nhanes_sim_whole.Rdata")     # Change to MIMIC path

obs_data <- nhanes_data_log %>% select(-c(Gender,race))
sim_data <- nhanes_sim_whole %>% select(-c(Gender,race))


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 2 - Plot   ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Q-Q plot
qqplot_whole<- plot_qq(
  sim_data = sim_data,
  obs_data = obs_data ,
  sim_nr = 100,
  conf_band = 95,
  var = NULL,
  type = "ribbon"
)


for(i in 1:length(qqplot_severe)) {
  qqplot_severe[[i]] <- qqplot_severe[[i]] + theme(
    axis.text = element_text(size = 15, face = "plain"),  
    axis.title.x = element_text(size = 18 , face = "plain"),
    axis.title.y = element_text(size = 18 , face = "plain")
  )
}

qqplot_whole

ggsave("qqplot_whole.tiff", 
       qqplot_severe,
       width = 12, 
       height = 10,
       dpi = 300,
       compression = "lzw") 

#donut VPC (six figures)
donut_whole <- donutVPC(
  sim_data = sim_para_log,
  obs_data = Severe_obesity,
  percentiles = c(5, 50, 95),
  sim_nr = 100,
  conf_band = 99,
  colors_bands = c("#99E0DC", "#E498B4"),
  cores = 8,
  pairs_matrix = matrix(c(
    "logSCR", "logALP",
    "Weight", "Height", 
    "logALT", "logAST",
    "HDL", "logTG",
    "Age", "LDL",
    "Weight", "AGP"
  ), ncol = 2, byrow = TRUE)
)

for(i in 1:length(donut_severe)) {
  donut_severe[[i]] <- donut_severe[[i]] + theme(
    axis.text = element_text(size = 10, face = "plain"),  
    axis.title.x = element_text(size = 15 , face = "plain"),
    axis.title.y = element_text(size = 15 , face = "plain")
  )
}
donut_whole

ggsave("donut_six_whole.tiff", 
       donut,
       width = 8, 
       height = 5,
       dpi = 300
       ) 



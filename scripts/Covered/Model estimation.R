

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
library(kde1d)
library(pmxcopula)
library(stringr)
library(parallel)


##~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 1 - Source files   ----
##~~~~~~~~~~~~~~~~~~~~~~~~

# NHANES data
load("clean_data/nhanes_data_12d_log.Rdata")
nhanes_data_log <- nhanes_data_log %>% select(-BMI, -Obese_condition)

# MIMIC data 
load("clean_data/total_data_mimic.Rdata")


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 2 - Copula model development ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Note: 
# 1. The following code structure is applied to both NHANES and MIMIC datasets
# 2. For subpopulation copula generation, use the same code structure above with:
# Filter data before modeling (e.g., filter (30≤BMI＜40) for normal obesity, filter(BMI ≥ 40) for morbid obesity)

## Whole population model fitting ##
combi <- permn(1:5)
AICform <- data.frame(Num = 1:length(combi), AICvalue = NA, RunningTime = NA)

data <- nhanes_data_log   # Change to total_data_mimic for MIMIC
data[, 1] <- ordered(data$Gender, levels = c(1, 2))   # obs_data$GCS_Category <- ordered(obs_data$GCS_Category, levels = c(1, 2))  # MIMIC only
                          
for (i in 1 : length(combi)) {
  start_time <- Sys.time()
  data$Race <- ordered(data$Race, levels = combi[[i]])
  vineTempo <- vine(dat = data,
                    margins_controls = list(mult = 1, xmin =NaN, xmax = NaN, bw = NA, deg = 2),
                    copula_controls = list(family_set = "parametric", 
                                           par_method = "mle",
                                           selcrit = "aic",
                                           keep_data = TRUE,  
                                           cores = 16,
                                           var_types = c("d", "c", "d", rep("c", 10))),  # NHANES
                    # var_types = c("d", "d", "d", rep("c", 11))),  # MIMIC
                    weights = numeric(),
                    keep_data = TRUE,
                    cores = 16)    
  
  string_vine <- capture.output(vineTempo$copula) 
  getAIC <- word(string_vine[2],start = 18,end = 18,sep = fixed(" ")) %>% as.numeric()
  end_time <- Sys.time()
  AICform[i,2] <- getAIC
  AICform[i,3] <- end_time - start_time
  cat("time of copula fitting: ", end_time - start_time, "\n",i, "run", "\n") # ~6 min for each run
  save(AICform, file = "clean_data/AICform_whole.Rdata")
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 3 - Model estimation ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# find the best order with lowest AIC
min(AICform$AICvalue) 
AICform[AICform$AICvalue == min,] 

# Best order: No.79 for NHANES, No.78 for MIMIC
i = 79  # Change to 78 for MIMIC
data$Race <- ordered(data$Race, levels = combi[[i]])
data$Gender <- ordered(data$Gender, levels = c(1, 2))
# data$GCS_Category <- ordered(data$GCS_Category, levels = c(1, 2))  # MIMIC only

copula_model <- vine(dat = data,
                     margins_controls = list(mult = 1, xmin = NaN, xmax = NaN, bw = NA, deg = 2),
                     copula_controls = list(family_set = "parametric", 
                                            par_method = "mle",
                                            selcrit = "aic",
                                            keep_data = TRUE,  
                                            cores = 8,
                                            var_types = c("d", "c", "d", rep("c", 10))),  # NHANES
                     # var_types = c("d", "d", "d", rep("c", 11))),  # MIMIC
                     weights = numeric(),
                     keep_data = TRUE,
                     cores = 8)

save(copula_model, file = "clean_data/nhanes_copula_whole.Rdata")


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 4 - Model simulation ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

n_sim <- nrow(data)
m <- 100
set.seed(12345)

sim_data <- rvine(n_sim * m, copula_model) %>%
  mutate(simulation_nr = rep(1:m, each = n_sim))

# NHANES only: remove AGP extrapolation
sim_data$AGP[sim_data$Gender == 1 | (sim_data$Age >= 50 & sim_data$Age < 60)] <- NA

save(sim_data, file = "clean_data/nhanes_sim_whole.Rdata")



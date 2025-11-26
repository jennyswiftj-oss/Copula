
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 0 - Load libraries   ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(patchwork)
library(cowplot)
library(reshape2)
library(ggplot2)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 1- Load functions   ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function
source("functions/calculate_metric.R") 
source("functions/calculate_frequency.R") # functions for the batch calculation of frequency of discrete var

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 2- Data preparation   ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data Processing for morbid obesity group only
mimic_severe <- total_data_mimic
n_severe <- nhanes_data_log

mimic_severe <- mimic_severe %>%
  mutate(BMI = round(Weight / (Height^2), 1)) %>% 
  filter(BMI >= 40.0) %>% 
  select(-BMI) %>% 
  select(logSCR, logALT, logAST, logALP)

n_severe <- n_severe %>%
  mutate(BMI = round(Weight / (Height^2), 1)) %>% 
  filter(BMI >= 40.0) %>% 
  select(-BMI) %>% 
  select(logSCR, logALT, logAST, logALP)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 3- Data comparison   ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## 3.1 Morbid obesity (marginal distribution) ----
mimic_severe$source <- "MIMIC"
n_severe$source <- "NHANES"

# Combine dataset
combined_data <- rbind(mimic_severe, n_severe)

long_data_severe <- melt(combined_data, 
                         id.vars = "source", 
                         measure.vars = c("logSCR", "logALT", "logAST", "logALP"))

# Plot marginal distribution
density_comparison_severe <- ggplot(long_data_severe) +
  geom_density(aes(value, fill = source, color = source), alpha = 0.3) +
  scale_fill_manual(name = NULL, values = c("MIMIC" = "#DF837D", "NHANES" = "#7DB0DF")) +  
  scale_colour_manual(name = NULL, values = c("MIMIC" = "#DF837D", "NHANES" = "#7DB0DF")) + 
  facet_wrap(variable ~ ., scales = "free", nrow = 2) +
  labs(x = "Value", y = "Density", tag = "A") +
  theme_bw(base_family = "Arial") +  
  theme(strip.background = element_rect(fill = "white"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 15, family = "Arial"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        strip.text.x = element_text(size = 18, face = "bold", family = "Arial"),  
        axis.text.x = element_text(size = 15, family = "Arial"),                 
        axis.text.y = element_text(size = 15, family = "Arial"),                 
        axis.title = element_text(size = 18, face = "bold", family = "Arial"),
        plot.tag = element_text(size = 20, face = "bold", family = "Arial"))

## 3.2 Morbid obesity (Correlation) ----
nhanes_data_log <- nhanes_data_log %>% 
  select(Age, Weight, Height, logSCR, logALT, logAST, logALP)

total_data_mimic <- total_data_mimic %>% 
  select(Age, Weight, Height, logSCR, logALT, logAST, logALP)

Severe_obesity_nhanes <- nhanes_data_log %>%
  mutate(BMI = round(Weight / (Height^2), 1)) %>% 
  filter(BMI >= 40.0) %>% 
  select(-BMI)

Severe_obesity_mimic <- total_data_mimic %>%
  mutate(BMI = round(Weight / (Height^2), 1)) %>% 
  filter(BMI >= 40.0) %>% 
  select(-BMI)


# Get statistics for two databases
nhanes_stats <- get_statistics(Severe_obesity_nhanes) %>% 
  rename(nhanes_value = value)

mimic_stats <- get_statistics(Severe_obesity_mimic) %>% 
  rename(mimic_value = value)

# Combine statistics for two databases
comparison_stats <- nhanes_stats %>% 
  full_join(mimic_stats, by = c("statistic", "covariate", "Var1", "Var2"))

correlation_comparison <- comparison_stats %>% 
  filter(statistic == "correlation") %>% 
  mutate(covariate = gsub("_", " - ", covariate, fixed = TRUE))

# Plot correlation
comparison_long <- correlation_comparison %>%
  mutate(var_pair = gsub("_", " - ", covariate, fixed = TRUE)) %>%
  pivot_longer(
    cols = c(nhanes_value, mimic_value),
    names_to = "source",
    values_to = "value"
  ) %>%
  mutate(source = ifelse(source == "nhanes_value", "NHANES", "MIMIC"))

correlation_mimic_nhanes_severe <- ggplot(comparison_long, aes(x = var_pair, color = source, fill = source)) +
  geom_hline(yintercept = 0, color = "#515151", linetype = "dashed") +
  geom_point(aes(y = value), position = position_dodge(width = 0.5), 
             size = 3, shape = 22, color = "black", stroke = 1) +
  scale_color_manual(name = NULL, values = c("NHANES" = "#7DB0DF", "MIMIC" = "#DF837D")) +
  scale_fill_manual(name = NULL, values = c("NHANES" = "#7DB0DF", "MIMIC" = "#DF837D")) +
  labs(x = "Covariate combinations", y = "Correlation", tag = "B") +
  scale_x_discrete(expand = expansion(mult = c(0.05, 0.05))) +
  theme_bw(base_family = "Arial") +  
  theme(
    axis.text.x = element_text(size = 16, angle = 90, hjust = 1, family = "Arial"),  
    axis.text.y = element_text(size = 15, family = "Arial"),                         
    axis.title = element_text(size = 18, face = "bold", family = "Arial"),           
    legend.title = element_blank(),         
    legend.text = element_text(size = 16, family = "Arial"),                        
    legend.position = "bottom",
    legend.direction = "horizontal",
    panel.grid.minor = element_blank(),
    plot.tag = element_text(size = 20, face = "bold", family = "Arial")
  )

## 3.3 Combination ----
# Remove legends from individual plots
density_comparison_severe_no_legend <- density_comparison_severe + 
  theme(legend.position = "none")

correlation_mimic_nhanes_severe_no_legend <- correlation_mimic_nhanes_severe + 
  theme(legend.position = "none")

# Extract shared legend
legend <- get_legend(density_comparison_severe + 
                       theme(legend.direction = "horizontal",
                             legend.box = "horizontal"))

# Combine plots
combined_plot_mimic_nhanes_morbid <- density_comparison_severe_no_legend /
  correlation_mimic_nhanes_severe_no_legend /
  wrap_elements(panel = legend) +
  plot_layout(heights = c(1, 0.6, 0.1))

combined_plot_mimic_nhanes_morbid


# Save combined plot
ggsave("combined_plot_mimic_nhanes_morbid_obesity.tiff", 
       combined_plot_mimic_nhanes_morbid,
       width = 11, 
       height = 10,  
       dpi = 300,
       compression = "lzw")

# Save individual plots (optional)
ggsave("density_comparison_MIMIC_NHANES_morbid.tiff", 
       plot = density_comparison_severe, 
       width = 12, height = 8, dpi = 300)

ggsave("correlation_mimic_nhanes_morbid.tiff", 
       correlation_mimic_nhanes_severe,
       width = 12, 
       height = 6,  
       dpi = 300,
       compression = "lzw")

install.packages("usethis")
usethis::use_git()

library(patchwork)
library(cowplot)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 1- Load functions   ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function
source("functions/MIMIC/calculate_metric.R") 
source("functions/MIMIC/calculate_frequency.R") # functions for the batch calculation of frequency of discrete var

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 2- Data preparation   ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data Processing
mimic_normal <- total_data_mimic
mimic_severe <- total_data_mimic

n_normal <- nhanes_data_log
n_severe <- nhanes_data_log

mimic_normal <- mimic_normal %>% 
  mutate(BMI = round(Weight / (Height^2), 1)) %>% 
  filter( BMI >= 30.0 & BMI < 40.0) %>% select(-BMI) %>% 
  select(logSCR, logALT, logAST, logALP)

mimic_severe <- mimic_severe %>%
  mutate(BMI = round(Weight / (Height^2), 1)) %>% 
  filter(BMI >= 40.0)%>% select(-BMI)%>% 
  select(logSCR, logALT, logAST, logALP)

n_normal <- n_normal %>%
  mutate(BMI = round(Weight / (Height^2), 1)) %>% 
  filter( BMI >= 30.0 & BMI < 40.0)%>% select(-BMI)%>% 
  select(logSCR, logALT, logAST, logALP)

n_severe <- n_severe %>%
  mutate(BMI = round(Weight / (Height^2), 1)) %>% 
  filter(BMI >= 40.0)%>% select(-BMI)%>% 
  select(logSCR, logALT, logAST, logALP)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 3- Data comparison   ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 3.1 Normal obesity (marginal distribution) ----
mimic_normal$source <- "MIMIC"
n_normal$source <- "NHANES"

# Combine dataset
combined_data <- rbind(mimic_normal, n_normal)

long_data_normal <- melt(combined_data, 
                  id.vars = "source", 
                  measure.vars = c("logSCR", "logALT", "logAST", "logALP"))

# Plot
density_comparison_normal <- ggplot(long_data_normal) +
  geom_density(aes(value, fill = source, color = source), alpha = 0.3) +
  scale_fill_manual(name = NULL, values = c("MIMIC" = "#DF837D", "NHANES" = "#7DB0DF")) +  
  scale_colour_manual(name = NULL, values = c("MIMIC" = "#DF837D", "NHANES" = "#7DB0DF")) + 
  facet_wrap(variable ~ ., scales = "free", nrow = 2) +
  labs(x = "Value", y = "Density") +
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
        axis.title = element_text(size = 18, face = "bold", family = "Arial"))
density_comparison_normal

ggsave("density_comparison_MIMIC_NHANES_normal.tiff", 
       plot = density_comparison_normal, 
       width = 12, height = 8, dpi = 300)

## 3.2 Severe obesity (marginal distribution) ----
mimic_severe$source <- "MIMIC"
n_severe$source <- "NHANES"

# Combine dataset
combined_data <- rbind(mimic_severe, n_severe)

long_data_severe <- melt(combined_data, 
                  id.vars = "source", 
                  measure.vars = c("logSCR", "logALT", "logAST", "logALP"))

# Plot
density_comparison_severe <- ggplot(long_data_severe) +
  geom_density(aes(value, fill = source, color = source), alpha = 0.3) +
  scale_fill_manual(name = NULL, values = c("MIMIC" = "#DF837D", "NHANES" = "#7DB0DF")) +  
  scale_colour_manual(name = NULL, values = c("MIMIC" = "#DF837D", "NHANES" = "#7DB0DF")) + 
  facet_wrap(variable ~ ., scales = "free", nrow = 2) +
  labs(x = "Value", y = "Density") +
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
        axis.title = element_text(size = 18, face = "bold", family = "Arial"))
density_comparison_severe

ggsave("density_comparison_MIMIC_NHANES_severe.tiff", 
       plot = density_comparison_severe, 
       width = 12, height = 8, dpi = 300)

## 3.3 Normal obesity (Correlation) ----
nhanes_data_log <- nhanes_data_log %>% 
  select(Age, Weight, Height, logSCR, logALT, logAST, logALP)

total_data_mimic <- total_data_mimic %>% 
  select(Age, Weight, Height, logSCR, logALT, logAST, logALP)

Normal_obesity_nhanes <- nhanes_data_log %>%
  mutate(BMI = round(Weight / (Height^2), 1)) %>% 
  filter( BMI >= 30.0 & BMI < 40.0)%>% select(-BMI)

Normal_obesity_mimic <- total_data_mimic %>%
  mutate(BMI = round(Weight / (Height^2), 1)) %>% 
  filter( BMI >= 30.0 & BMI < 40.0)%>% select(-BMI)

# Get statistics for two databases
nhanes_stats <- get_statistics(Normal_obesity_nhanes) %>% 
  rename(nhanes_value = value)

mimic_stats <- get_statistics(Normal_obesity_mimic) %>% 
  rename(mimic_value = value)

# Combine statistics for two databases (full data)
comparison_stats <- nhanes_stats %>% 
  full_join(mimic_stats, by = c("statistic", "covariate", "Var1", "Var2"))

correlation_comparison <- comparison_stats %>% 
  filter(statistic == "correlation") %>% 
  mutate(covariate = gsub("_", " - ", covariate, fixed = TRUE))

# Choose big difference group (big difference)
comparison_stats <- nhanes_stats %>% 
  full_join(mimic_stats, by = c("statistic", "covariate", "Var1", "Var2"))
correlation_comparison <- comparison_stats %>% 
  filter(statistic == "correlation") %>% 
  mutate(covariate = gsub("_", " - ", covariate, fixed = TRUE)) %>%
  mutate(correlation_diff = abs(nhanes_value - mimic_value)) %>%
  filter(correlation_diff > 0.1)

# Plot
comparison_long <- correlation_comparison %>%
  mutate(var_pair = gsub("_", " - ", covariate, fixed = TRUE)) %>%
  pivot_longer(
    cols = c(nhanes_value, mimic_value),
    names_to = "source",
    values_to = "value"
  ) %>%
  mutate(source = ifelse(source == "nhanes_value", "NHANES", "MIMIC"))

common_theme <- theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
    panel.grid.minor = element_blank(),
    legend.position = "top"
  )

correlation_mimic_nhanes <- ggplot(comparison_long, aes(x = var_pair, color = source, fill = source)) +
  geom_hline(yintercept = 0, color = "#515151", linetype = "dashed") +
  geom_point(aes(y = value), position = position_dodge(width = 0.5), 
             size = 3, shape = 22, color = "black", stroke = 1) +
  scale_color_manual(name = NULL, values = c("NHANES" = "#7DB0DF", "MIMIC" = "#DF837D")) +
  scale_fill_manual(name = NULL, values = c("NHANES" = "#7DB0DF", "MIMIC" = "#DF837D")) +
  labs(x = "Covariate combinations", y = "Correlation") +
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
    panel.grid.minor = element_blank()
  )

correlation_mimic_nhanes_normal <- correlation_mimic_nhanes
correlation_mimic_nhanes_normal

correlation_mimic_nhanes_diff_1 <- correlation_mimic_nhanes
correlation_mimic_nhanes_diff_1

ggsave("correlation_mimic_nhanes_normal.tiff", 
       correlation_mimic_nhanes_normal,
       width = 12, 
       height = 6,  
       dpi = 300,
       compression = "lzw")

ggsave("correlation_mimic_nhanes_diff_1.tiff", 
       correlation_mimic_nhanes_diff_1 ,
       width = 4, 
       height = 6,  
       dpi = 300,
       compression = "lzw")

## 3.4 Severe obesity (Correlation) ----
Severe_obesity_nhanes <- nhanes_data_log %>%
  mutate(BMI = round(Weight / (Height^2), 1)) %>% 
  filter( BMI >= 40.0)%>% select(-BMI)

Severe_obesity_mimic <- total_data_mimic %>%
  mutate(BMI = round(Weight / (Height^2), 1)) %>% 
  filter( BMI >= 40.0)%>% select(-BMI)

# Get statistics for two databases
nhanes_stats <- get_statistics(Severe_obesity_nhanes) %>% 
  rename(nhanes_value = value)

mimic_stats <- get_statistics(Severe_obesity_mimic) %>% 
  rename(mimic_value = value)

# Combine statistics for two databases (full data)
comparison_stats <- nhanes_stats %>% 
  full_join(mimic_stats, by = c("statistic", "covariate", "Var1", "Var2"))

correlation_comparison <- comparison_stats %>% 
  filter(statistic == "correlation") %>% 
  mutate(covariate = gsub("_", " - ", covariate, fixed = TRUE))

# Choose big difference group (big difference)
comparison_stats <- nhanes_stats %>% 
  full_join(mimic_stats, by = c("statistic", "covariate", "Var1", "Var2"))
correlation_comparison <- comparison_stats %>% 
  filter(statistic == "correlation") %>% 
  mutate(covariate = gsub("_", " - ", covariate, fixed = TRUE)) %>%
  mutate(correlation_diff = abs(nhanes_value - mimic_value)) %>%
  filter(correlation_diff > 0.1)

# Plot
comparison_long <- correlation_comparison %>%
  mutate(var_pair = gsub("_", " - ", covariate, fixed = TRUE)) %>%
  pivot_longer(
    cols = c(nhanes_value, mimic_value),
    names_to = "source",
    values_to = "value"
  ) %>%
  mutate(source = ifelse(source == "nhanes_value", "NHANES", "MIMIC"))

common_theme <- theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
    panel.grid.minor = element_blank(),
    legend.position = "top"
  )

correlation_mimic_nhanes <- ggplot(comparison_long, aes(x = var_pair, color = source, fill = source)) +
  geom_hline(yintercept = 0, color = "#515151", linetype = "dashed") +
  geom_point(aes(y = value), position = position_dodge(width = 0.5), 
             size = 3, shape = 22, color = "black", stroke = 1) +
  scale_color_manual(name = NULL, values = c("NHANES" = "#7DB0DF", "MIMIC" = "#DF837D")) +
  scale_fill_manual(name = NULL, values = c("NHANES" = "#7DB0DF", "MIMIC" = "#DF837D")) +
  labs(x = "Covariate combinations", y = "Correlation") +
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
    panel.grid.minor = element_blank()
  )

correlation_mimic_nhanes_severe <- correlation_mimic_nhanes
correlation_mimic_nhanes_severe

correlation_mimic_nhanes_diff <- correlation_mimic_nhanes
correlation_mimic_nhanes_diff 

ggsave("correlation_mimic_nhanes_severe.tiff", 
       correlation_mimic_nhanes_severe,
       width = 12, 
       height = 6,  
       dpi = 300,
       compression = "lzw")

ggsave("correlation_mimic_nhanes_diff.tiff", 
       correlation_mimic_nhanes_diff ,
       width = 4, 
       height = 6,  
       dpi = 300,
       compression = "lzw")

## 3.5 Combination ----
density_comparison_normal_no_legend <- density_comparison_normal + 
  theme(legend.position = "none") +
  labs(tag = "A") +
  theme(plot.tag = element_text(size = 20, face = "bold"))

density_comparison_severe_no_legend <- density_comparison_severe + 
  theme(legend.position = "none") +
  labs(tag = "B") +
  theme(plot.tag = element_text(size = 20, face = "bold"))

correlation_mimic_nhanes_normal_no_legend <- correlation_mimic_nhanes_normal + 
  theme(legend.position = "none") +
  labs(tag = "C") +
  theme(plot.tag = element_text(size = 20, face = "bold"))

correlation_mimic_nhanes_severe_no_legend <- correlation_mimic_nhanes_severe + 
  theme(legend.position = "none") +
  labs(tag = "D") +
  theme(plot.tag = element_text(size = 20, face = "bold"))

legend <- get_legend(density_comparison_normal + 
                       theme(legend.direction = "horizontal",
                             legend.box = "horizontal"))

combined_plot_mimic_nhanes <- (density_comparison_normal_no_legend | density_comparison_severe_no_legend) /
  wrap_elements(panel = legend) /
  (correlation_mimic_nhanes_normal_no_legend | correlation_mimic_nhanes_severe_no_legend) +
  plot_layout(heights = c(1, 0.1, 0.6))

combined_plot_mimic_nhanes

ggsave("combined_plot_mimic_nhanes.tiff", 
       combined_plot_mimic_nhanes,
       width = 22, 
       height = 15,  
       dpi = 300,
       compression = "lzw")

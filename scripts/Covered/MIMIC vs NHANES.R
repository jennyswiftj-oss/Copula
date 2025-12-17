
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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 2- Data preparation   ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data Processing for original normal obesity and morbid obesity group
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
## 3.1 Combined density plots (A&B combined) ----
# Prepare data for both groups
mimic_normal$source <- "MIMIC - Normal Obesity"
mimic_severe$source <- "MIMIC - Morbid Obesity" 
n_normal$source <- "NHANES - Normal Obesity"
n_severe$source <- "NHANES - Morbid Obesity"

# Combine all datasets
combined_data_all <- rbind(mimic_normal, mimic_severe, n_normal, n_severe)

long_data_all <- melt(combined_data_all, 
                      id.vars = "source", 
                      measure.vars = c("logSCR", "logALT", "logAST", "logALP"))

# Define color palette with gradients
color_palette <- c(
  "MIMIC - Normal Obesity" = "#F4A6A6",     
  "MIMIC - Morbid Obesity" = "#C85450",     
  "NHANES - Normal Obesity" = "#A6C8F4",   
  "NHANES - Morbid Obesity" = "#5078C8"     
)

# Combined density plot
density_comparison_combined <- ggplot(long_data_all) +
  geom_density(aes(value, fill = source, color = source), alpha = 0.3) +
  scale_fill_manual(name = NULL, values = color_palette) +  
  scale_colour_manual(name = NULL, values = color_palette) + 
  facet_wrap(variable ~ ., scales = "free", nrow = 2) +
  labs(x = "Value", y = "Density") +
  theme_bw(base_family = "Arial") +  
  theme(strip.background = element_rect(fill = "white"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 12, family = "Arial"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        strip.text.x = element_text(size = 18, face = "bold", family = "Arial"),  
        axis.text.x = element_text(size = 15, family = "Arial"),                 
        axis.text.y = element_text(size = 15, family = "Arial"),                 
        axis.title = element_text(size = 18, face = "bold", family = "Arial")) +
  guides(fill = guide_legend(nrow = 1),
         color = guide_legend(nrow = 1))

## 3.2 Combined correlation plots (C&D combined) ----
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

Severe_obesity_nhanes <- nhanes_data_log %>%
  mutate(BMI = round(Weight / (Height^2), 1)) %>% 
  filter( BMI >= 40.0)%>% select(-BMI)

Severe_obesity_mimic <- total_data_mimic %>%
  mutate(BMI = round(Weight / (Height^2), 1)) %>% 
  filter( BMI >= 40.0)%>% select(-BMI)

# Union filtering approach - shows pairs with >0.1 difference in any obesity group
get_correlation_data_union_filter <- function(nhanes_normal, mimic_normal, nhanes_morbid, mimic_morbid) {
  
  get_single_correlation <- function(nhanes_data, mimic_data, obesity_type) {
    nhanes_stats <- get_statistics(nhanes_data) %>% 
      rename(nhanes_value = value)
    
    mimic_stats <- get_statistics(mimic_data) %>% 
      rename(mimic_value = value)
    
    comparison_stats <- nhanes_stats %>% 
      full_join(mimic_stats, by = c("statistic", "covariate", "Var1", "Var2"))
    
    correlation_comparison <- comparison_stats %>% 
      filter(statistic == "correlation") %>% 
      mutate(covariate = gsub("_", " - ", covariate, fixed = TRUE))
    
    comparison_long <- correlation_comparison %>%
      mutate(var_pair = gsub("_", " - ", covariate, fixed = TRUE)) %>%
      pivot_longer(
        cols = c(nhanes_value, mimic_value),
        names_to = "database",
        values_to = "value"
      ) %>%
      mutate(
        database = ifelse(database == "nhanes_value", "NHANES", "MIMIC"),
        obesity_type = obesity_type,
        source = paste(database, "-", obesity_type, "Obesity")
      )
    
    return(comparison_long)
  }
  
  normal_data <- get_single_correlation(nhanes_normal, mimic_normal, "Normal")
  morbid_data <- get_single_correlation(nhanes_morbid, mimic_morbid, "Morbid")
  
  all_data <- rbind(normal_data, morbid_data)
  
  # Filter pairs with >0.1 difference in any obesity group
  diff_summary <- all_data %>%
    group_by(var_pair, obesity_type) %>%
    summarise(
      nhanes_val = value[database == "NHANES"][1],
      mimic_val = value[database == "MIMIC"][1],
      diff = abs(nhanes_val - mimic_val),
      .groups = 'drop'
    ) %>%
    group_by(var_pair) %>%
    summarise(max_diff = max(diff, na.rm = TRUE), .groups = 'drop') %>%
    filter(max_diff > 0.1)
  
  filtered_data <- all_data %>%
    filter(var_pair %in% diff_summary$var_pair) %>%
    mutate(
      database = factor(database, levels = c("NHANES", "MIMIC")),
      obesity_type = factor(obesity_type, levels = c("Normal", "Morbid")),
      source = factor(source, levels = c("NHANES - Normal Obesity", "NHANES - Morbid Obesity",
                                         "MIMIC - Normal Obesity", "MIMIC - Morbid Obesity"))
    )
  
  return(filtered_data)
}

# Get correlation data using union filtering
combined_corr_data <- get_correlation_data_union_filter(
  Normal_obesity_nhanes, Normal_obesity_mimic, 
  Severe_obesity_nhanes, Severe_obesity_mimic
)

# Shape palette: NHANES=square(22), MIMIC=circle(21)
shape_palette <- c(
  "MIMIC - Normal Obesity" = 21,     
  "MIMIC - Morbid Obesity" = 21,     
  "NHANES - Normal Obesity" = 22,    
  "NHANES - Morbid Obesity" = 22     
)

# Combined correlation plot with different shapes
correlation_mimic_nhanes_combined <- ggplot(combined_corr_data, 
                                            aes(x = var_pair, color = source, fill = source, shape = source)) +
  geom_hline(yintercept = 0, color = "#515151", linetype = "dashed") +
  geom_point(aes(y = value), position = position_dodge(width = 0.7), 
             size = 3, color = "black", stroke = 1) +
  scale_color_manual(name = NULL, values = color_palette) +
  scale_fill_manual(name = NULL, values = color_palette) +
  scale_shape_manual(name = NULL, values = shape_palette) +
  labs(x = "Covariate combinations", y = "Correlation") +
  scale_x_discrete(expand = expansion(mult = c(0.05, 0.05))) +
  theme_bw(base_family = "Arial") +  
  theme(
    axis.text.x = element_text(size = 16, angle = 90, hjust = 1, family = "Arial"),  
    axis.text.y = element_text(size = 15, family = "Arial"),                         
    axis.title = element_text(size = 18, face = "bold", family = "Arial"),           
    legend.title = element_blank(),         
    legend.text = element_text(size = 12, family = "Arial"),                        
    legend.position = "bottom",
    legend.direction = "horizontal",
    panel.grid.minor = element_blank()
  ) +
  guides(
    fill = guide_legend(nrow = 1, override.aes = list(shape = c(22, 22, 21, 21))),
    color = guide_legend(nrow = 1, override.aes = list(shape = c(22, 22, 21, 21))),
    shape = guide_legend(nrow = 1)
  )

## 3.3 Final combination ----
# Remove legends from individual plots and add tags
density_comparison_combined_no_legend <- density_comparison_combined + 
  theme(legend.position = "none") +
  labs(tag = "A") +
  theme(plot.tag = element_text(size = 20, face = "bold"))

correlation_mimic_nhanes_combined_no_legend <- correlation_mimic_nhanes_combined + 
  theme(legend.position = "none") +
  labs(tag = "B") +
  theme(plot.tag = element_text(size = 20, face = "bold"))

# Extract legend
legend <- get_legend(density_comparison_combined + 
                       theme(legend.direction = "horizontal",
                             legend.box = "horizontal",
                             legend.text = element_text(size = 12)))

# Final combined plot with legend at bottom
final_combined_plot <- density_comparison_combined_no_legend / 
  correlation_mimic_nhanes_combined_no_legend /
  wrap_elements(panel = legend) +
  plot_layout(heights = c(1, 0.8, 0.15))

final_combined_plot

# Save the final plot
ggsave("final_combined_plot_mimic_nhanes.tiff", 
       final_combined_plot,
       width = 10.5, 
       height = 9,  
       dpi = 300,
       compression = "lzw")



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 0 - Load libraries   ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(dplyr)
library(ggplot2)
library(patchwork)
library(tidyr)
library(ggnewscale)
library(pmxcopula)


##~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 1 - Load data for calculation   ----
##~~~~~~~~~~~~~~~~~~~~~~~~
# 1. The following code structure is applied to both NHANES and MIMIC datasets
# 2. For subpopulation copula generation, use the same code structure above with:
# Filter data before modeling (e.g., filter (30≤BMI＜40) for normal obesity, filter(BMI ≥ 40) for morbid obesity)

#load observed data
load("clean_data/nhanes_data_12d_log.Rdata")  # Change to MIMIC path
obs_data <- nhanes_data_log %>% select(-BMI, -Obese_condition)  # Change to select(-GCS_Category) for MIMIC

#load simulation data
load("clean_data/nhanes_sim_whole.Rdata")     # Full copula - whole population
load("clean_data/nhanes_sim_normal.Rdata")    # Subgroup copula - normal obesity
load("clean_data/nhanes_sim_severe.Rdata")    # Subgroup copula - morbid obesity


##~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 2 - Overlap and correlation data ----
##~~~~~~~~~~~~~~~~~~~~~~~~
# Note: Run this section twice for each subgroup (whole copula and sub copula)

### Full copula - filter to target subgroup ###
sim_full <- nhanes_sim_whole %>%   # Change to MIMIC path
  mutate(BMI = round(Weight / Height^2, 1)) %>% 
  filter(BMI >= 30.0 & BMI < 40.0) %>%  # Change to BMI > 40 for morbid obesity
  select(-BMI)

### Subgroup copula ###
sim_sub <- nhanes_sim_normal  # Change to nhanes_sim_severe for morbid obesity

### Calculate metrics ###
# Full derived copula
overlap_full <- calc_dependency(
  sim_data = sim_full,
  obs_data = obs_data,
  pairs_matrix = NULL,
  percentile = 95,
  sim_nr = 100,
  cores = 8
)

# Sub copula
overlap_sub <- calc_dependency(
  sim_data = sim_sub,
  obs_data = obs_data,
  pairs_matrix = NULL,
  percentile = 95,
  sim_nr = 100,
  cores = 8
)

# Save results with descriptive names
save(overlap_full, file = "clean_data/overlap_nhanes_full_normal.Rdata")  
save(overlap_sub, file = "clean_data/overlap_nhanes_sub_normal.Rdata")

# Repeat for morbid obesity and MIMIC datasets


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 3 - Combine all results ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load all saved overlap results
load("clean_data/overlap_nhanes_full_normal.Rdata")
load("clean_data/overlap_nhanes_sub_normal.Rdata")
load("clean_data/overlap_nhanes_full_severe.Rdata")
load("clean_data/overlap_nhanes_sub_severe.Rdata")

# For MIMIC, load corresponding files

# Combine NHANES data
combined_data_nhanes <- rbind(
  overlap_full_normal %>% mutate(obesity_group = "Obesity", model = "full copula"),
  overlap_sub_normal %>% mutate(obesity_group = "Obesity", model = "subgroup copula"),
  overlap_full_severe %>% mutate(obesity_group = "Morbid Obesity", model = "full copula"),
  overlap_sub_severe %>% mutate(obesity_group = "Morbid Obesity", model = "subgroup copula")
)

# For MIMIC, combine in same way

save(combined_data_nhanes, file = "clean_data/combined_data_nhanes.Rdata")
# save(combined_data_mimic, file = "clean_data/combined_data_mimic.Rdata")


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 4 - Analysis results ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Calculate relative difference 
calculate_relative_diff <- function(data, subgroup_level) {
  data %>%
    filter(obesity_group == subgroup_level) %>%
    select(var_pair, model, statistic, value) %>%
    group_by(var_pair, model, statistic) %>%
    summarise(median_value = median(value), .groups = "drop") %>%
    pivot_wider(names_from = model, values_from = median_value) %>%
    mutate(
      abs_diff = abs(`full copula` - `subgroup copula`),
      relative_diff = case_when(
        statistic == "correlation" & abs(`full copula`) > 0.01 ~ abs_diff / abs(`full copula`),
        statistic == "overlap" & `full copula` > 5 ~ abs_diff / `full copula`,
        TRUE ~ abs_diff
      ),
      diff_category = case_when(
        statistic == "correlation" & relative_diff >= 0.2 ~ "≥20%",
        statistic == "correlation" & relative_diff >= 0.1 ~ "10-20%",
        statistic == "correlation" & relative_diff < 0.1 ~ "<10%",
        statistic == "overlap" & relative_diff >= 0.1 ~ "≥10%",
        statistic == "overlap" & relative_diff >= 0.05 ~ "5-10%",
        statistic == "overlap" & relative_diff < 0.05 ~ "<5%",
        TRUE ~ "other"
      )
    ) %>%
    arrange(statistic, desc(relative_diff))
}

# Check data
check_normal_nhanes <- calculate_relative_error(combined_data_nhanes, "Obesity")
check_severe_nhanes <- calculate_relative_error(combined_data_nhanes, "Morbid Obesity")

diff_normal_nhanes <- calculate_relative_diff(combined_data_nhanes, "Obesity")
diff_severe_nhanes <- calculate_relative_diff(combined_data_nhanes, "Morbid Obesity")

cat("\n=== NHANES | Obesity ===\n")
print(check_normal_nhanes$dist)
print(table(diff_normal_nhanes$statistic, diff_normal_nhanes$diff_category))

cat("\n=== NHANES | Morbid Obesity ===\n")
print(check_severe_nhanes$dist)
print(table(diff_severe_nhanes$statistic, diff_severe_nhanes$diff_category))

# For MIMIC, analyze in same way


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 5 - Plot ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Select top pairs with largest differences
N_PAIRS <- 10

selected_pairs_normal_nhanes <- diff_normal_nhanes %>%
  group_by(statistic) %>% 
  slice_max(relative_diff, n = N_PAIRS/2) %>% 
  ungroup() %>%
  pull(var_pair) %>% 
  unique()

selected_pairs_severe_nhanes <- diff_severe_nhanes %>%
  group_by(statistic) %>% 
  slice_max(relative_diff, n = N_PAIRS/2) %>% 
  ungroup() %>%
  pull(var_pair) %>% 
  unique()

selected_pairs_obesity_mimic <- diff_obesity_mimic %>%
  group_by(statistic) %>% 
  slice_max(relative_diff, n = N_PAIRS/2) %>% 
  ungroup() %>%
  pull(var_pair) %>% 
  unique()

selected_pairs_severe_mimic <- diff_severe_mimic %>%
  group_by(statistic) %>% 
  slice_max(relative_diff, n = N_PAIRS/2) %>% 
  ungroup() %>%
  pull(var_pair) %>% 
  unique()

# Prepare plot data
plot_data_normal_nhanes <- combined_data_nhanes %>%
  filter(obesity_group == "Obesity", var_pair %in% selected_pairs_normal_nhanes) %>%
  left_join(diff_normal_nhanes %>% select(var_pair, statistic, relative_diff, diff_category),
            by = c("var_pair", "statistic"))

plot_data_severe_nhanes <- combined_data_nhanes %>%
  filter(obesity_group == "Morbid Obesity", var_pair %in% selected_pairs_severe_nhanes) %>%
  left_join(diff_severe_nhanes %>% select(var_pair, statistic, relative_diff, diff_category),
            by = c("var_pair", "statistic"))

plot_data_obesity_mimic <- combined_data_mimic %>%
  filter(obesity_group == "Obesity", var_pair %in% selected_pairs_obesity_mimic) %>%
  left_join(diff_obesity_mimic %>% select(var_pair, statistic, relative_diff, diff_category),
            by = c("var_pair", "statistic"))

plot_data_severe_mimic <- combined_data_mimic %>%
  filter(obesity_group == "Morbid Obesity", var_pair %in% selected_pairs_severe_mimic) %>%
  left_join(diff_severe_mimic %>% select(var_pair, statistic, relative_diff, diff_category),
            by = c("var_pair", "statistic"))

# Coloring
category_colors <- c(
  "<10%" = "#f9fafb",      
  "10-20%" = "#e5e7eb",    
  "≥20%" = "#d1d5db",      
  "<5%" = "#f9fafb",       
  "5-10%" = "#e5e7eb",     
  "≥10%" = "#d1d5db"       
)

# Create pics
# NHANES 
p1 <- create_comparison_plot(plot_data_obesity_nhanes, "correlation", "Correlation", c(-1, 1), show_legend = FALSE)
p2 <- create_comparison_plot(plot_data_obesity_nhanes, "overlap", "Overlap (%)", c(0, 100), show_legend = FALSE)
p3 <- create_comparison_plot(plot_data_severe_nhanes, "correlation", "Correlation", c(-1, 1), show_legend = FALSE)
p4 <- create_comparison_plot(plot_data_severe_nhanes, "overlap", "Overlap (%)", c(0, 100), show_legend = FALSE)

# MIMIC
p5 <- create_comparison_plot(plot_data_obesity_mimic, "correlation", "Correlation",  c(-1, 1), show_legend = TRUE)
p6 <- create_comparison_plot(plot_data_obesity_mimic, "overlap", "Overlap (%)", c(0, 100) , show_legend = TRUE)
p7 <- create_comparison_plot(plot_data_severe_mimic,  "correlation", "Correlation",  c(-1, 1), show_legend = FALSE)
p8 <- create_comparison_plot(plot_data_severe_mimic,  "overlap", "Overlap (%)", c(0, 100) , show_legend = FALSE)

# Adjust pics
remove_y <- theme(axis.title.y = element_blank(),
                  axis.text.y  = element_blank(),
                  axis.ticks.y = element_blank(),
                  plot.margin  = margin(4, 2, 4, 2))

p3 <- p3 + remove_y; p4 <- p4 + remove_y
p5 <- p5 + remove_y; p6 <- p6 + remove_y
p7 <- p7 + remove_y; p8 <- p8 + remove_y

p1 <- p1 + theme(plot.margin = margin(4, 2, 4, 2))
p2 <- p2 + theme(plot.margin = margin(4, 2, 4, 2))

col_title <- function(txt) {
  ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = txt, fontface = "bold", size = 5) +
    theme_void() +
    theme(plot.margin = margin(0, 0, 6, 0))
}

t1 <- col_title("Obesity (NHANES)")
t2 <- col_title("Morbid Obesity (NHANES)")
t3 <- col_title("Obesity (MIMIC)")
t4 <- col_title("Morbid Obesity (MIMIC)")

# Final plot
final_plot <-(t1 | t2 | t3 | t4) /(p1 | p3 | p5 | p7) /(p2 | p4 | p6 | p8) +
  plot_layout(widths = c(1, 1, 1, 1),
              heights = c(0.10, 1, 1),
              guides = "collect") &
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 9),
    plot.margin = margin(0, 0, 0, 0)
  )

final_plot

ggsave("plot_nhanes_mimic.tiff", 
       final_plot, width = 18, height = 10, dpi = 300, compression = "lzw")

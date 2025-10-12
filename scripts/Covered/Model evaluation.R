
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


### Plot the density contours ####
# Visualize 6 examples
cat("Visualize set of 6 densities\n")
sim_data <- sim_para_log %>% filter(simulation_nr == 1) %>% select(c(-Gender,-Race,-simulation_nr))
obs_data <- Severe_obesity %>% select(c(-Gender,-Race)) 

all_NHANES <- sim_data %>% mutate(type = "simulated") %>% 
  bind_rows(obs_data %>% mutate(type = "observed")) 

# Selected covariate pairs
sets_of_interest <- matrix(c(
  c("logSCR", "logALP"),
  c("Weight", "Height"),
  c("logALT", "logAST"),
  c("HDL", "logTG"),
  c("Age", "LDL"),
  c("Weight","AGP")),
  ncol = 2, byrow = T)

# Plots
p <- list()
for(i in 1:6){
  plot_dat_ind <- !is.na(all_NHANES[, sets_of_interest[i, 1]]) & !is.na(all_NHANES[, sets_of_interest[i, 2]])
  data <- all_NHANES[plot_dat_ind, ]
  p[[i]] <- ggplot(data, aes(
    x = .data[[sets_of_interest[i, 1]]], 
    y = .data[[sets_of_interest[i, 2]]], 
    color = type, 
    linetype = type)) +
    geom_density_2d(bins = 10, linewidth = 1, show.legend = F) + 
    scale_linetype_manual(values = c(5, 1)) +
    scale_color_manual(values = c("#3ABAC1", "#969696"), limits = force) +
    scale_x_continuous(expand = expansion(mult = c(0, 0)),
                       limits = quantile(data[,sets_of_interest[i, 1]], probs = c(0.01, 0.95), na.rm = TRUE)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0)),
                       limits = quantile(data[,sets_of_interest[i, 2]], probs = c(0.01, 0.95), na.rm = TRUE)) +
    geom_xsidedensity(show.legend = F, linewidth = 1) +
    geom_ysidedensity(show.legend = F, linewidth = 1) +
    scale_ysidex_continuous(minor_breaks = NULL, limits = c(0,NA), expand = expansion(mult = c(0.001, 0.001))) +
    scale_xsidey_continuous(minor_breaks = NULL, limits = c(0,NA), expand = expansion(mult = c(0.001, 0.001))) +
    theme_bw() +
    theme(aspect.ratio = 1, 
          ggside.panel.grid = element_blank(), 
          ggside.axis.line = element_line(color = "white"),
          ggside.axis.text = element_blank(), 
          ggside.axis.ticks = element_blank(), 
          ggside.panel.border = element_rect(colour = "white"), 
          ggside.panel.scale = .2,
          axis.title.x = element_text(color='black',size=15,hjust = 0.4),
          axis.title.y = element_text(color='black',size=15,hjust = 0.4),
          axis.text.x = element_text(color='black', size=12), 
          axis.text.y = element_text(color='black', size=12),
          panel.background = element_rect(fill = "white"),
          plot.background = element_rect(fill = "white", color = NA),
          plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm"))
          }

# Combine plots with no spacing between them
p_densityContour <- (p[[1]] + p[[2]] + p[[3]]) / 
  (p[[4]] + p[[5]] + p[[6]]) + 
  plot_layout(guides = "collect") +
  plot_annotation(theme = theme(plot.margin = unit(c(0, 0, 0, 0), "cm")))
p_densityContour

save(p_densityContour, file = "figure/p_densityContour_whole.RData")
save(p_densityContour, file = "figure/p_densityContour_normal.RData")
save(p_densityContour, file = "figure/p_densityContour_severe.RData")

ggsave("p_densityContour.tiff", 
       p_densityContour,
       width = 8, 
       height = 5,
       dpi = 300,
       bg = "white") 


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 3 - Marginal comparison - discrete var   ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Investigate the metrics for discrete variables ####
#### Race-ethnicity ####
m = 100

# Calculate the frequency of each race-ethnicity subgroup
freq_race <- get_race_multiple(sim_para_log, m = m) %>% 
  mutate(Race = factor(Race, labels = c("Hispanic", "White","African American","Asian","Other race"))) 
target <- c("Hispanic", "White","African American","Asian","Other race")
freq_race$Race <- factor(freq_race$Race,
                         levels = target,
                         labels = target)
freq_race <- freq_race %>% 
  group_by(Race) %>% 
  summarise(mean= mean(value), sd = sd(value)) %>% 
  mutate(type ="Virtual population")

freq_race_obs <- get_race(nhanes_data_log) %>%
  rename(Race = variable) %>% 
  dplyr::rename(mean = value) %>% 
  mutate(sd = 0) %>% 
  mutate(type ="Observed population")

freq_race_combo <- rbind(freq_race_obs,freq_race)

#### Gender ####
target <- c("Male", "Female")
# Calculate the frequency of each sex subgroup
freq_gender <- get_gender_multiple(sim_para_log, m = m) %>% 
  mutate(Gender = factor(Gender, labels = c("Male", "Female"))) 
freq_gender$Gender <- factor(freq_gender$Gender,levels = target,labels = target)
freq_gender <- freq_gender %>% 
  group_by(Gender) %>% 
  summarise(mean= mean(value), sd = sd(value)) %>% 
  mutate(type ="Virtual population")

freq_gender_obs <- get_gender(nhanes_data_log) %>%
  rename(Gender = variable) %>% 
  dplyr::rename(mean = value) %>% 
  mutate(sd = 0) %>% 
  mutate(type ="Observed population")

freq_gender_combo <- rbind(freq_gender_obs,freq_gender)

# Race-ethnicity plot
improved_theme <- theme_bw() +
  theme(
    strip.background = element_rect(fill = "white"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.title.x  = element_text(size = 18),
    axis.title.y  = element_text(size = 18),
    legend.position = "right",
    plot.margin = margin(10, 10, 10, 10)
  )

race_frequency <- ggplot() +
                  geom_errorbar(data = freq_race_combo, aes(x = Race, y = mean, ymin = mean - sd, ymax = mean + sd, color = type), 
                                width = 0.3, position = position_dodge(width = 0.9)) + 
                  geom_bar(data = freq_race_combo, aes(x = Race, y = mean, fill = type), color = "black", alpha = 0.4, position = "dodge", stat = "identity") +
                  scale_fill_manual(name = NULL, values = c("#3ABAC1", "#969696"),labels = c("Observed population", "Virtual population")) +
                  scale_color_manual(name = NULL, values = c("white", "black"),labels = c("Observed population", "Virtual population")) +
                  labs(x = "Race", y = "Frequency") +
                  guides(fill = "none", color = "none") +
                  scale_x_discrete(guide = guide_axis(angle = 45), expand = expansion(mult = c(0.15, 0.15))) +
                  coord_cartesian(ylim = c(0, 2000)) +
                  improved_theme

# Gender plot
gender_frequency <- ggplot() +
                    geom_errorbar(data = freq_gender_combo, aes(x = Gender, y = mean, ymin = mean - sd, ymax = mean + sd, color = type), 
                                  width = 0.3,  position = position_dodge(width = 0.9)) +
                    geom_bar(data = freq_gender_combo, aes(x = Gender, y = mean, fill = factor(type)), 
                             color = "black", stat = "identity", position = position_dodge(width = 0.9),  alpha = 0.4) +
                    scale_fill_manual(name = NULL, values = c("#3ABAC1", "#969696"), breaks = c("Observed population", "Virtual population")) +
                    scale_color_manual(name = NULL, values = c("white", "black")) +labs(x = "Gender", y = "Frequency") +
                    guides(fill = "none", color = "none") +
                    scale_x_discrete(guide = guide_axis(angle = 45), expand = expansion(mult = c(0.6, 0.6))) +  
                    coord_cartesian(ylim = c(0, 2000)) +
                    improved_theme

# Combine the plots
Figure_2_A<- ggarrange(
  race_frequency, gender_frequency, 
  ncol = 2, nrow = 1, 
  widths = c(1.75, 1.5),
  common.legend = TRUE,
  legend = "right",
  align = "h"
)

Figure_2_A

save(Figure_2_A_whole, file = "figure/p_densityContour_whole.RData")
save(Figure_2_A_normal, file = "figure/p_densityContour_normal.RData")
save(Figure_2_A_severe, file = "figure/p_densityContour_severe.RData")

ggsave("Figure_2_A.tiff", 
       Figure_2_A,
       width = 8, 
       height = 4,
       dpi = 300,
) 

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 4 - Marginal comparison - continuous var   ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Density curves ####
long_sim <- melt(sim_para_log[,c(-1,-2)], id.vars = "simulation_nr") %>% 
  mutate(data = "Virtual population")

long_obs <- melt(Severe_obesity[,c(-1,-2)]) %>% 
  mutate(simulation_nr = 101,
         data = "Observed population")

all_line <- rbind(long_sim, long_obs)

density_curve_1D_severe <- ggplot(all_line) +
  geom_density(aes(value, color = factor(data, levels = c("Virtual population", "Observed population")),
                   fill = factor(simulation_nr),linewidth = factor(data)),alpha = 1/10) +
  scale_fill_manual(name = NULL, values = c(rep("grey80", 100), "white"),limits = force) +
  scale_colour_manual(name = "Data type",values = c("#3ABAC1", "#969696"),breaks = c("Observed population", "Virtual population"),limits = force) + 
  scale_linewidth_manual(name = NULL,values = c(0.3, 0.5)) +
  facet_wrap(variable ~ .,scales = "free",nrow = 4) +labs(x = "",y = "Density") +
  guides(fill = "none", linewidth = "none", color = "none") +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "white"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "none",  
    strip.text.x = element_text(size = 15),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 15),
    axis.title = element_text(size = 18),
    axis.title.y = element_text(size = 18))
density_curve_1D_severe

save(density_curve_1D_whole, file = "figure/density_curve_1D__whole.RData")
save(density_curve_1D_normal, file = "figure/density_curve_1D__normal.RData")
save(density_curve_1D_severe, file = "figure/density_curve_1D__severe.RData")

ggsave("density_curve_1D_severe.tiff", 
       density_curve_1D_severe,
       width = 10, 
       height = 6,
       dpi = 300,
) 

#### Marginal metrics ####
nhanes_data_log <- nhanes_data_log %>% select (-BMI, -Obese_condition)
NHANES_sim_con <- sim_para_log[,c(-1,-2)] 

m <- 100

statistics <- get_statistics_multiple(NHANES_sim_con, m = m) %>% 
  mutate(value = ifelse(abs(value) == Inf, NA, value)) %>% 
  left_join(get_statistics(nhanes_data_log_1[,-c(1,3)]) %>% rename(observed = value)) %>% 
  mutate(rel_value = (value - observed)/observed) %>%  # calculate the relative error
  mutate(cov1 = gsub("\\_.*", "", covariate),
         cov2 = gsub("\\w+.\\_", "", covariate))
statistics[, c("covA", "covB")] <- t(apply(as.data.frame(statistics[, c("cov1", "cov2")]), 1, sort))

statistics_5d <- statistics %>% 
  filter(statistic %in% c("Q5.5%","median","Q95.95%","mean","sd")) %>% 
  mutate(statistic = gsub("sd", "standard deviation", statistic, fixed = TRUE)) %>% 
  mutate(covariate = gsub("_", " - ", covariate, fixed = TRUE))

statistics_5d$statistic <- factor(statistics_5d$statistic,
                                  levels = c("Q5.5%","median","Q95.95%","mean","standard deviation"),
                                  labels = c("5th P","median","95th P","mean","SD"))
save(statistics_5d, file = "clean_data/overall_marginal_severe.Rdata")

### Numerical analysis ----
data_report_statistics_5d <- statistics_5d %>% 
  group_by(statistic, covariate) %>% 
  summarise(avg = mean(value),
            sdvalue = sd(rel_value),
            cv = sd(value)/mean(value))

data_report_statistics_5d_1 <- data_report_statistics_5d %>% 
  filter(statistic %in% c("5th P", "median", "9th P", "mean"))
print(max(data_report_statistics_5d_1$cv))

data_report_statistics_5d_2 <- data_report_statistics_5d %>% 
  filter(!statistic %in% c("5th P", "median", "9th P", "mean"))
print(max(data_report_statistics_5d_2$cv))

### Plot ----
# Visualize the relative error of marginal metrics in observed and simulated populations
plot_statistics_5d <- statistics_5d %>% 
  ggplot(aes(y = rel_value, x = covariate)) +
  geom_vline(xintercept = seq(0.5, 10, by = 1), color = "grey95") +
  geom_boxplot(fill = "white", color = "black",outlier.shape = NA) +
  geom_hline(yintercept = c(-0.2, 0.2), linetype = 2, color = "grey65") +
  geom_hline(yintercept = 0, linetype = 1, color = "black") +
  labs(x = "Covariates", y = "Relative error", color = "Method") +
  scale_x_discrete(guide = guide_axis(angle = 90), expand = expansion(mult = c(0.1, 0.1))) +
  coord_cartesian(ylim = c(-0.22,0.22))+ 
  facet_grid(statistic~.)+
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"), panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text=element_text(size=8),
        axis.title=element_text(size=12),
        strip.text.x = element_text(size=10))
plot_statistics_5d
ggsave(plot_statistics_5d, file = "figure/figure_2_severe.tiff",width = 8, height = 8, units = "in", dpi = 300) 



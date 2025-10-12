

##~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 0 - Load libraries   ----
##~~~~~~~~~~~~~~~~~~~~~~~~

library(dplyr)
library(survey)
library(ggplot2)
library(GGally)   
library(ggpubr)  
library(reshape2) 
library(tidyr)
library(foreign)


##~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 1 - Data list   ----
##~~~~~~~~~~~~~~~~~~~~~~~~
## 2009-2020 

# 1.SEQN     : id of participants
# 2.RIAGENDR : gender 
# 3.RIDAGEYR : age  (year)
# 4.RIDRETH3; RIDRETH1 : race(with/without Asian) 
# 5.BMXWT    : weight (kg)
# 6.BMXHT    : height (cm)
# 7.1 LBXAGP : alpha-1-acid glycoprotein (g/L)            # 2021-2023 label name 
# 7.2 SSAGP  : alpha-1-acid glycoprotein (g/L)            # lack of 2009-2010, 2011-2012, 2013-2014 2015-2016, 2017-2020 label name
# 8. LBDTRSI : Triglyceride (mmol/L)                      # lack of 2021-2023, and use 2015-2020 data to keep same method
# 9. LBDLDLSI: LDL-cholesterol (mmol/L)                   # lack of 2021-2023, and use 2015-2020 data to keep same method
# 10. LBDHDDSI: Direct HDL-Cholesterol (mmol/L)           # lack of 2021-2023
# 11.LBXSATSI : ALT (U/L)                                 # lack of 2021-2023
# 12.LBXSASSI : AST (U/L)                                 # lack of 2021-2023
# 13.LBXSAPSI : ALP (IU/L)                                # lack of 2021-2023
# 14. LBXSCR  : Creatinine (mg/dL)                        # lack of 2021-2023

##~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 2 - Data downloading   ----
##~~~~~~~~~~~~~~~~~~~~~~~~
# 2.1 Demographic (DEMO)
# 2017-2020
download.file("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/P_DEMO.XPT", tf <- tempfile(), mode="wb")
DEMO_2017_2020 <- foreign::read.xport(tf)[,c("SEQN","RIAGENDR","RIDAGEYR","RIDRETH1","RIDRETH3")]
# 2015-2016
download.file("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/DEMO_I.XPT", tf <- tempfile(), mode="wb")
DEMO_2015_2016 <- foreign::read.xport(tf)[,c("SEQN","RIAGENDR","RIDAGEYR","RIDRETH1","RIDRETH3")]
# 2013-2014
download.file("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/DEMO_H.XPT", tf <- tempfile(), mode="wb")
DEMO_2013_2014 <- foreign::read.xport(tf)[,c("SEQN","RIAGENDR","RIDAGEYR","RIDRETH1","RIDRETH3")]
# 2011-2012
download.file("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2011/DataFiles/DEMO_G.XPT", tf <- tempfile(), mode="wb")
DEMO_2011_2012 <- foreign::read.xport(tf)[,c("SEQN","RIAGENDR","RIDAGEYR","RIDRETH1","RIDRETH3")]
# 2009-2010
download.file("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2009/DataFiles/DEMO_F.XPT", tf <- tempfile(), mode="wb")
DEMO_2009_2010 <- foreign::read.xport(tf)[,c("SEQN","RIAGENDR","RIDAGEYR","RIDRETH1")] # without "RIDRETH3" 

# label the year of release
DEMO_2017_2020$release <- 1
DEMO_2015_2016$release <- 2
DEMO_2013_2014$release <- 3
DEMO_2011_2012$release <- 4
DEMO_2009_2010$release <- 5

# 2.2 Body measures (BMX) 
# 2017-2020
download.file("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/P_BMX.XPT", tf <- tempfile(), mode="wb")
BMX_2017_2020 <- foreign::read.xport(tf)[,c("SEQN","BMXWT","BMXHT")]
# 2015-2016
download.file("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/BMX_I.XPT", tf <- tempfile(), mode="wb")
BMX_2015_2016 <- foreign::read.xport(tf)[,c("SEQN","BMXWT","BMXHT")]
# 2013-2014
download.file("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/BMX_H.XPT", tf <- tempfile(), mode="wb")
BMX_2013_2014 <- foreign::read.xport(tf)[,c("SEQN","BMXWT","BMXHT")]
# 2011-2012
download.file("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2011/DataFiles/BMX_G.XPT", tf <- tempfile(), mode="wb")
BMX_2011_2012 <- foreign::read.xport(tf)[,c("SEQN","BMXWT","BMXHT")]
# 2009-2010
download.file("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2009/DataFiles/BMX_F.XPT", tf <- tempfile(), mode="wb")
BMX_2009_2010 <- foreign::read.xport(tf)[,c("SEQN","BMXWT","BMXHT")]

# 2.3 Biochemistry Profile (BIO) 
# 2017-2020
download.file("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/P_BIOPRO.XPT", tf <- tempfile(), mode="wb")
BIO_2017_2020 <- foreign::read.xport(tf)[,c("SEQN","LBXSATSI","LBXSASSI","LBXSAPSI","LBXSCR")]
download.file("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/P_TRIGLY.XPT", tf <- tempfile(), mode="wb")
TRI_2017_2020 <- foreign::read.xport(tf)[,c("SEQN","LBDTRSI", "LBDLDLSI")]
download.file("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/P_HDL.XPT", tf <- tempfile(), mode="wb")
HDL_2017_2020 <- foreign::read.xport(tf)[,c("SEQN", "LBDHDDSI")] 
download.file("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/P_SSAGP.XPT", tf <- tempfile(), mode="wb")
AGP_2017_2020 <- foreign::read.xport(tf)[,c("SEQN", "SSAGP")] 
# 2015-2016
download.file("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/BIOPRO_I.XPT", tf <- tempfile(), mode="wb")
BIO_2015_2016 <- foreign::read.xport(tf)[,c("SEQN","LBXSATSI","LBXSASSI","LBXSAPSI","LBXSCR")]
download.file("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/TRIGLY_I.XPT", tf <- tempfile(), mode="wb")
TRI_2015_2016 <- foreign::read.xport(tf)[,c("SEQN","LBDTRSI", "LBDLDLSI")]
download.file("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/HDL_I.XPT", tf <- tempfile(), mode="wb")
HDL_2015_2016 <- foreign::read.xport(tf)[,c("SEQN", "LBDHDDSI")] 
download.file("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/SSAGP_I.XPT", tf <- tempfile(), mode="wb")
AGP_2015_2016 <- foreign::read.xport(tf)[,c("SEQN", "SSAGP")] 
# 2013-2014
download.file("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/BIOPRO_H.XPT", tf <- tempfile(), mode="wb")
BIO_2013_2014 <- foreign::read.xport(tf)[,c("SEQN","LBXSATSI","LBXSASSI","LBXSAPSI","LBXSCR")]
download.file("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/TRIGLY_H.XPT", tf <- tempfile(), mode="wb")
TRI_2013_2014 <- foreign::read.xport(tf)[,c("SEQN","LBDTRSI", "LBDLDLSI")]
download.file("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/HDL_H.XPT", tf <- tempfile(), mode="wb")
HDL_2013_2014 <- foreign::read.xport(tf)[,c("SEQN", "LBDHDDSI")] 
# 2011-2012
download.file("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2011/DataFiles/BIOPRO_G.XPT", tf <- tempfile(), mode="wb")
BIO_2011_2012 <- foreign::read.xport(tf)[,c("SEQN","LBXSATSI","LBXSASSI","LBXSAPSI","LBXSCR")]
download.file("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2011/DataFiles/TRIGLY_G.XPT", tf <- tempfile(), mode="wb")
TRI_2011_2012 <- foreign::read.xport(tf)[,c("SEQN","LBDTRSI", "LBDLDLSI")]
download.file("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2011/DataFiles/HDL_G.XPT", tf <- tempfile(), mode="wb")
HDL_2011_2012 <- foreign::read.xport(tf)[,c("SEQN", "LBDHDDSI")] 
# 2009-2010
download.file("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2009/DataFiles/BIOPRO_F.XPT", tf <- tempfile(), mode="wb")
BIO_2009_2010 <- foreign::read.xport(tf)[,c("SEQN","LBXSATSI","LBXSASSI","LBXSAPSI","LBXSCR")]
download.file("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2009/DataFiles/TRIGLY_F.XPT", tf <- tempfile(), mode="wb")
TRI_2009_2010 <- foreign::read.xport(tf)[,c("SEQN","LBDTRSI", "LBDLDLSI")]
download.file("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2009/DataFiles/HDL_F.XPT", tf <- tempfile(), mode="wb")
HDL_2009_2010 <- foreign::read.xport(tf)[,c("SEQN", "LBDHDDSI")] 

##~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 3 - Data configuration and adjustment   ----
##~~~~~~~~~~~~~~~~~~~~~~~~
# Append Files
DEMO <- bind_rows(DEMO_2009_2010, DEMO_2011_2012, DEMO_2013_2014,DEMO_2015_2016,DEMO_2017_2020)
BMX <- bind_rows(BMX_2009_2010, BMX_2011_2012, BMX_2013_2014,BMX_2015_2016,BMX_2017_2020)
TRI <- bind_rows(TRI_2009_2010,TRI_2011_2012, TRI_2013_2014,TRI_2015_2016,TRI_2017_2020) 
HDL <- bind_rows(HDL_2009_2010,HDL_2011_2012, HDL_2013_2014,HDL_2015_2016,HDL_2017_2020) 
AGP <- bind_rows(AGP_2015_2016,AGP_2017_2020) #lack of 2009-2010,2011-2012, 2013-2014

# Adjustment of biochemistry biofiles 
BIO_2017_2020_1 <- BIO_2017_2020 %>%
  mutate(LBXSCR=1.051*LBXSCR-0.06945,                       # serum creatinine
         LBXSATSI=1.013*LBXSATSI+2.688,                     # ALT
         LBXSASSI=1.018*LBXSASSI+ 3.762,                    # AST
         LBXSAPSI=10^(1.001*log10(LBXSAPSI)-0.04294)) %>%    # ALP
  mutate(LBXSCR=round(LBXSCR,2),                            # serum creatinine
         LBXSATSI=round(LBXSATSI,0),                        # ALT
         LBXSASSI=round(LBXSASSI,0),                        # AST
         LBXSAPSI=round(LBXSAPSI,0))                         # ALP

BIO <- bind_rows(BIO_2009_2010, BIO_2011_2012, BIO_2013_2014,BIO_2015_2016,BIO_2017_2020_1) 

# 14 covariates in total (not include ID)
nhanes_data <- left_join(DEMO, BMX, by="SEQN") 
nhanes_data <- left_join(nhanes_data, TRI, by="SEQN")  
nhanes_data <- left_join(nhanes_data, BIO, by="SEQN")  
nhanes_data <- left_join(nhanes_data, AGP, by="SEQN")
nhanes_data <- left_join(nhanes_data, HDL, by="SEQN")

# Set the age group and change the class of variables (Adults data which should be under 60)
ageThresholdL <- 18
ageThresholdH <- 60
colnames(nhanes_data)
nhanes_data <- nhanes_data %>%
  filter(RIDAGEYR >= ageThresholdL & RIDAGEYR < ageThresholdH) 

# Handle the different records of race: RIDRETH1,-RIDRETH3 --- 6 races in total
nhanes_data <- nhanes_data %>%
  mutate(RIDRETH = case_when(release < 5  ~ RIDRETH3, 
                             release == 5 & RIDRETH1 <= 4 ~ RIDRETH1,
                             release == 5 & RIDRETH1 > 4 ~ 8,)) %>% # adjust of race records (add non-hispanic Asian population)
  filter(RIDRETH!=8) %>% # remove the "Other Race - Including Multi-Racial" category recorded by RIDRETH1
  select(-RIDRETH1, -RIDRETH3) %>% # only keep RIDRETH to indicate race
  mutate_at(.vars = vars("SEQN", "RIAGENDR","RIDRETH"), factor) %>% # change the class of variables
  mutate(RIAGENDR = factor(RIAGENDR, labels  = c("male", "female"))) %>% 
  mutate(RIDRETH = factor(RIDRETH, labels = c("Mexican American", "Other Hispanic","Non-Hispanic White","Non-Hispanic Black","Non-Hispanic Asian","Other Race - Including Multi-Racial"))) 
str(nhanes_data) #To check the type of data,column name and example data

# Select obese people
nhanes_data_1 <- nhanes_data %>% 
  mutate(BMXHT = BMXHT/100) %>%  #change cm to m to calculate BMI
  mutate(BMI = round(BMXWT / (BMXHT^2), 1))

# Adults obesity condition
nhanes_data_1 <- nhanes_data_1 %>% mutate (Obese_condition = case_when( 
  BMI >= 40.0 ~ "Severe obesity",
  BMI >= 30.0 & BMI < 40.0 ~ "Obesity",
  BMI >= 25.0 & BMI < 30.0 ~ "Overweight",
  BMI < 25.0 ~ "Normal weight/underweight",
  TRUE ~ NA_character_                                          
 )
)

# Check obesity people
table(nhanes_data_1$Obese_condition)

# Save the data
cat("saving nhanes_data_1.Rdata")
save(nhanes_data_1, file = "clean_data/nhanes_data_1.Rdata")


##~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 4 - Reshaping the data  ----
##~~~~~~~~~~~~~~~~~~~~~~~~
# Rename data column
data <- nhanes_data_1 %>%
  filter(is.na(BMXHT) + is.na(BMXWT)  + is.na(BMI)
         + is.na(SSAGP) + is.na(LBDTRSI) + is.na(LBDLDLSI)
         + is.na(LBXSCR) + is.na(LBXSATSI) + is.na(LBXSASSI)
         + is.na(LBXSAPSI) + is.na(LBDHDDSI) + is.na(Obese_condition) !=12) %>%   # generate lines with at least one data
  select(-SEQN,-release) %>%
  rename("Age"="RIDAGEYR",
         "Weight"="BMXWT",
         "Height"="BMXHT",
         "AGP"="SSAGP",
         "TG"="LBDTRSI",
         "LDL"="LBDLDLSI",
         "HDL"="LBDHDDSI",
         "SCR"="LBXSCR",
         "ALT"="LBXSATSI",
         "AST"="LBXSASSI",
         "ALP"="LBXSAPSI",
         "Race"= "RIDRETH",
         "Gender" = "RIAGENDR") 

# Reduce the races from 6 into 5 categories 
data$Race <- factor(data$Race,
                    levels = c("Mexican American", "Other Hispanic", "Non-Hispanic White","Non-Hispanic Black","Non-Hispanic Asian","Other Race - Including Multi-Racial"),
                    labels = c("Hispanic", "Hispanic", "White","African American","Asian","Other race"))
data$Race <- factor(data$Race,
                    levels = c("Hispanic", "White","African American","Asian","Other race"),
                    labels = c("1", "2", "3","4","5"))
data$Gender <- factor(data$Gender,
                      levels = c("male", "female"),
                      labels = c("1", "2")) 
nhanes_data_5r <- data
nhanes_data_5r <- nhanes_data_5r[,c(1,2,13,3,4,5,6,7,8,9,10,11,12,14,15)]

# 21867 obs, 15 var
save(nhanes_data_5r, file = "clean_data/nhanes_data_12d_5r.Rdata") 

##~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 5 - Pre-check the data  ----
##~~~~~~~~~~~~~~~~~~~~~~~~
# Density plot for continuous variable
load("clean_data/nhanes_data_12d_5r.Rdata")
long_obs <- melt(nhanes_data_5r[,c(-1,-3,-14,-15)])

density_curve_1D <- ggplot(long_obs) +
  geom_density(aes(value, fill = "Observed population"), color = "#DF837D", alpha = 0.3) +
  scale_fill_manual(name = "Data type", values = "grey") +  
  scale_colour_manual(name="Data type", values = "#DF837D") + 
  scale_linewidth_manual(name = "Data type", values = 20) +
  facet_wrap(variable~.,scales="free",nrow = 4) +
  labs(x = "x-axis label", y = "Density") +
  guides(color = guide_legend(byrow = TRUE)) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"), 
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        legend.direction = "vertical",
        legend.spacing.y = unit(0.5, 'cm'),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        strip.text.x = element_text(size = 14, face = "bold"), 
        axis.text.x = element_text(size = 12),                
        axis.text.y = element_text(size = 12),                 
        axis.title = element_text(size = 14, face = "bold"))  
density_curve_1D
ggsave("density_curve_1D_log.tiff", plot = density_curve_1D , width =12, height = 8, dpi = 300)

# Log-transformation for skewed data
nhanes_data_log <- nhanes_data_5r %>% 
  mutate(logSCR = log(SCR)) %>%
  mutate(logALT = log(ALT)) %>%
  mutate(logAST = log(AST)) %>%
  mutate(logALP = log(ALP)) %>%
  mutate(logTG = log (TG)) %>% 
  select(-c(SCR,ALT,AST,ALP,TG))
# 21867 obs, 15 var
save(nhanes_data_log, file = "clean_data/nhanes_data_12d_log.Rdata")

##~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ 6 - Clean the environment  ----
##~~~~~~~~~~~~~~~~~~~~~~~~
rm(DEMO_2009_2010, DEMO_2011_2012,DEMO_2013_2014,DEMO_2015_2016,DEMO_2017_2020,
   BMX_2009_2010, BMX_2011_2012,BMX_2013_2014,BMX_2015_2016,BMX_2017_2020, 
   BIO_2009_2010, BIO_2011_2012,BIO_2013_2014,BIO_2015_2016,BIO_2017_2020,BIO_2017_2020_2,
   TRI_2009_2010, TRI_2011_2012, TRI_2013_2014, TRI_2015_2016, TRI_2017_2020,
   AGP_2015_2016, AGP_2017_2020, 
   HDL_2009_2010,HDL_2011_2012,HDL_2013_2014,HDL_2015_2016,HDL_2017_2020,
   DEMO, BMX, BIO, TRI, AGP, 
   nhanes_data, nhanes_data_1, data, nhanes_data_5r)











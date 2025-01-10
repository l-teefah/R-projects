library(openxlsx)
library(tidyverse)
library(plm)
library(stargazer)
library(sandwich)
library(clubSandwich)
library(ggcorrplot)
library(kableExtra)
library(lmtest)
library(car)
library(dplyr)
library(tidyr)


researchdata <- read.xlsx("Project_data.xlsx")

######## METHOD 1 #############

##missing indicator approach##
data <- researchdata %>% select(gdp_pc, nuts_code, year, popdens, rd_emp, ht_total, unemp_share, total_pat)

##GDP per capita
data["gdp_pc_impute"] <- ifelse(!is.na(data$gdp_pc), data$ht_total, 0)

data["gdp_pc_miss"] <- ifelse(is.na(data$gdp_pc), 1, 0) 

##population density
data["popdens_impute"] <- ifelse(!is.na(data$popdens), data$popdens, 0)

data["popdens_miss"] <- ifelse(is.na(data$popdens), 1, 0) 

##rd employment
data["rd_emp_impute"] <- ifelse(!is.na(data$rd_emp), data$rd_emp, 0)

data["rd_emp_miss"] <- ifelse(is.na(data$rd_emp), 1, 0) 

##tech employment
data["ht_total_impute"] <- ifelse(!is.na(data$ht_total), data$ht_total, 0)

data["ht_total_miss"] <- ifelse(is.na(data$ht_total), 1, 0) 

##unemp share###
data["unemp_share_impute"] <- ifelse(!is.na(data$unemp_share), data$unemp_share, 0)

data["unemp_share_miss"] <- ifelse(is.na(data$unemp_share), 1, 0) 

##total patents
data["total_pat_impute"] <- ifelse(!is.na(data$total_pat), data$total_pat, 0)

data["total_pat_miss"] <- ifelse(is.na(data$total_pat), 1, 0) 


######## METHOD 2 #############
##missing indicator - mean approach##
data2 <- researchdata %>% select(gdp_pc, nuts_code, year, popdens, rd_emp, ht_total, unemp_share, total_pat)

data2 <- researchdata %>% 
  select(gdp_pc, nuts_code, year, popdens, rd_emp, ht_total, unemp_share, total_pat) %>%
  mutate(across(everything(), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))


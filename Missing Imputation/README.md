
# Missing Indicator Approach

When dealing with missing data in datasets (particularly control variables), various imputation techniques are used to ensure completeness and maintain statistical validity. Three common methods include: the two missing indicator approach, mean imputation, and imputing zero for missing values.

1. **Two Missing Indicator Approach**:
   - This technique involves creating a binary indicator variable for each feature with missing values.It creates two additional column for each variable in the dataset.
   - One column retains the original variable with missing data, while a second column is added to indicate whether a value is missing (1 for missing, 0 for present).
   - This method preserves information about the presence of missingness, which can be informative for predictive modeling.

2. **Mean Imputation**:
   - Mean imputation replaces missing values with the mean of the observed values in the same variable.
   - It is straightforward and maintains the sample mean of the dataset.
   - However, it can reduce variability and distort relationships between variables, as it does not account for the uncertainty introduced by missing data.

3. **Imputing Zero for Missing Values**:
   - This method involves replacing all missing values with zero.
   - It is simple and may be suitable when zero has a meaningful interpretation, such as indicating absence.
   - However, it can introduce bias and misrepresent the data distribution, especially when zero is not a valid representation of the missing data.

Each approach has its own advantages and drawbacks, and the choice of method depends on the data context and the assumptions made about the missingness mechanism.


## Prerequisites

Load the necessary libraries and data file.

```{r, warning = FALSE, message = FALSE}
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

# **File can be downloaded here [Project_data.xlsx](https://github.com/user-attachments/files/18375614/Project_data.xlsx)**

researchdata <- read.xlsx("Project_data.xlsx")
```

## Two Missing Indicator Approach

```{r}
## two missing indicator approach ##
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
```

## Mean Imputation

```{r}
data2 <- researchdata %>% select(gdp_pc, nuts_code, year, popdens, rd_emp, ht_total, unemp_share, total_pat)

data2 <- researchdata %>% 
  select(gdp_pc, nuts_code, year, popdens, rd_emp, ht_total, unemp_share, total_pat) %>%
  mutate(across(everything(), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))
```

## Imputing Zero for Missing Values

```{r}
data3 <- researchdata %>% 
  select(gdp_pc, nuts_code, year, popdens, rd_emp, ht_total, unemp_share, total_pat) %>%
  mutate(across(everything(), ~ifelse(is.na(.), 0, .)))
```

# Purpose: Introduce students to regression discontinuity design 
# Date started: Mon, Jan 30, 2023

# Install packages
install.packages('TeachingDemos')
library('TeachingDemos')


# Set directory
setwd("/Users/hammadshaikh/Documents/University of Stavanger/Teaching/Research Methods/Tutorial 3 - RDD")

# Load data on alcohol drinking
data = read.csv("AlcoholUsage.csv")

# Generate drinking elegibility indicator (can drink if over 21)
data["legally_drink"] = ifelse(data$agecell > 21, 1, 0)

# Make plot of elegibility to drink and age groups
plot(data$agecell, data$legally_drink, main="Legal Alcohol Drinking Elegibility and Age", xlab="Age (years)", ylab="Drinking Elegibility Status")

# Add dotted vertical line at discontinuity
abline(v=21, col = "red", lty=2)

# Plot death rate per 100,000 (from all causes) by age group
plot(data$agecell, data$deathrate, main="Drinking Elegibility and Death Rate", xlab="Age (years)", ylab="Death Rate", col = ifelse(data$agecell >= 21, "blue", "black"))
abline(v=21, col = "red", lty='dashed')

# Plot line of best fit before and after cutoff
line_fit_below = lm(data$deathrate ~ data$agecell, subset = data$agecell < 21)
line_fit_above = lm(data$deathrate ~ data$agecell, subset = data$agecell >= 21)
clipplot(abline(line_fit_below), xlim = c(19, 21))
clipplot(abline(line_fit_above, col = "blue"), xlim = c(21, 23))

# Estimate the effect of being able to drink alcohol legally on death rates using a RDD
rdd_model = lm(data$deathrate ~ data$legally_drink + data$agecell)
summary(rdd_model)

# Compare the effect size relatively to the average death rate
effect_size = rdd_model$coefficients[2]
avg_death = mean(data$deathrate, na.rm = TRUE)
effect_size/avg_death

# Re-estimate the RDD model but only using age values from 20 - 22 (close to threshold)
rdd_model2 = lm(data$deathrate ~ data$legally_drink + data$agecell, subset = data$agecell >= 20 & data$agecell <= 22)
summary(rdd_model2)








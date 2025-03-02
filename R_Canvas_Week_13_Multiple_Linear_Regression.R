
# OVERVIEW OF LAST WEEK 

  # Simple Linear Regression 
    # We predicted one variable from another 
 
# THIS LECTURE 
  
  # Multiple Linear Regression 
    # We predict one variable from multiple variables 


# EXAMPLE: Read data into R - USE ONE OF THEM! 

  # Approach_1: Read data from working directory 
album_sales_2 <- read.delim("Album_Sales_2.dat")

  # Approach_2: Read data into R from an online source 
album_sales_2 <- read.delim("http://discoveringstatistics.com/docs/Album%20Sales%202.dat", header = TRUE)




# INFORMATION ON DATA (Field, A.P. (2017). Discovering Statistics using R: and sex and drugs and rock 'n' roll (5th ed.). SAGE.)
# We are building on our analysis from last week. There are new variables:
  # airplay: the number of times an album is played on Radio 1 (the UK’s biggest national radio station) 
  #          during the week prior to release.
  # attract: attractiveness of the band.




# EXAMPLE: Rename a variable 
names(album_sales_2)[1] = "promotion" 




# EXAMPLE: Display the pattern of your data with plotly

# install.packages("plotly")
# install.packages("tidyverse")
library(plotly)
library(tidyverse)



fig <- plot_ly(album_sales_2, x = ~airplay, y = ~sales, z = ~promotion)
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'No. of Plays on Radio 1 per Week'),
                                   yaxis = list(title = 'Album Sales (Thousands)'),
                                   zaxis = list(title = 'Promotion Budget (Thousands of Pounds')
                                   ))


fig


# QUESTION_1: Explore the structure of your data
str(album_sales_2)




# QUESTION_2: Create the simple linear model between sales and promotion 
#             Hint: we created this last week

model <- lm(sales ~ promotion, album_sales_2)


# EXAMPLE: Now, we want to predict sales from the variables of 
#            promotion, airplay, attract 


album_sales_multi_model <- lm(sales ~ promotion + airplay + attract, data = album_sales_2)
  


# EXAMPLE: stargazer package

  # install.packages("stargazer")
  library(stargazer)

  stargazer(album_sales_multi_model, type = "text", out = "album_sales_multi_model.txt")



# ADDITIONAL INFORMATION: update()
#                         we build on the simple model from last week
#                         we can use update function and add the new variables 

album_sales_multi_model_alternative <- update(album_sales_last_week_model, .~. + airplay + attract)

# .~. means keep the outcome and predictors the same as the baseline model




# QUESTION_3: Print the output of album_sales_last_week_model 
#                             and album_sales_multi_model

# hint: use summary function on each model
summary(model)

summary(album_sales_multi_model)


# RECALL: The value of the R^2 statistics at the bottom of each output describes the overall model
#         whether the model is successful at predicting album sales 

# In the album_sales_last_week_model R^2 is 33.5%. Promotion explains the 33.5% of the album sales
# In the album_sales_multi_model     R^2 is 66.5%. There are two other predictors (attractiveness and radio play)
# So, we can say that attractiveness and airplay explains the additional 33%




# QUESTION_ 4: What is the value of adjusted R^2?



# INFORMATION - ADJUSTED R^2: 
#               The adjusted R^2 gives us some idea of how well our model generalizes.
#               It is better to have the values of R^2 and adjusted R^2 close to each other or the same 




# QUESTION_5: Calculate the difference between R^2 and adjusted R^2 




# EXAMPLE: What does this difference indicate?
  # the difference is about 0.5% 
  # this shrinkage means that if the model were derived from population rather than a sample 
    # it would account for approximately 0.5% less variance of the outcome. 




# INFORMATION: Notation 
# 1.341e+02 move the decimal point two places to the right = 134.1





# QUESTION_6: Write the multi-linear model by using your output
#             Hint: Estimate column on the output gives us the b-values
#                   sales_i = b_0 + b_1*promotion_i + b_2*airplay_i + b_3*attractiveness_i
                    
sales_i = -26.61 + 0.08promotion_i + 3.36airplay_i + 11.08attractiveness_i




# QUESTION_7: The b-values tell us about the relationship between the predictor and the outcome.
#             Is there a negative or positive relationship between the predictor variables and sales?





# INFORMATION: The b-values also tells us to what degree each predictor effects the outcome if the 
#              effects of all other predictors are held constant. 


# EXAMPLE: What is the estimate for promotion? How does promotion effect the outcome? 

  # Promotion budget (b = 0.085):
  #                  This value indicates that as advertising budget increases by one unit, 
  #                  album sales increase by 0.085 units.
  #                  For every £1000 more spent on advertising, an extra 0.085 thousand albums (85 albums) are sold.  
  #                  This interpretation is true only if the effects of attractiveness of the band and airplay are held constant.





# QUESTION_8: Can you interpret the effects of airplay and attractiveness on the outcome? 






# QUESTION_9: Which variable has the less impact on album sales? 
# Hint: you need to look at the values of t on your model output. 
#       smaller t means less impact on the album sales. 



  
   
# EXAMPLE: Calculating confidence intervals - confint function 
  
  confint(album_sales_multi_model)


  
  
# EXAMPLE: Comparing Models 
  
  anova(model, album_sales_multi_model)

  # IMPORTANT: we can only compare hierarchical models
  #            the second model must contain everything that was in the first model plus something new, 
  #            and the third model must contain everything in the second model plus something new, etc...
  
 
  
  

# EXAMPLE: Assessing the Assumption of No Multicollinearity 
  
  # variance inflation factor (vif) and tolerance statistics (1/vif)
  
    # The VIF indicates whether a predictor has a strong linear relationship with the other predictor(s).
    # Related to the VIF is the tolerance statistic, which is its reciprocal (1/VIF). 
  
  # install.packages("car")
    library(car)
  
  vif(album_sales_multi_model)  
  1/vif(album_sales_multi_model)
  mean(vif(album_sales_multi_model))
  
 
   

# EXAMPLE: Checking Assumptions about the Residuals 
  
  album_sales_2$fitted <- album_sales_multi_model$fitted.values
  album_sales_2$studentized_residuals <- rstudent(album_sales_multi_model)
  
  # Studentized residuals (have the same properties as the standardized residuals, follows a Student’s t-distribution).
  
  
  # install.packages("tidyverse")
  library(tidyverse)
  
  
  # histogram of studentized residuals
    # A useful way to check whether the residuals deviate from a normal distribution
    # is to inspect the histogram of the residuals (or the standardized or studentized residuals).
  
  
  histogram <- ggplot(album_sales_2, aes(studentized_residuals)) + 
    theme(legend.position = "none") +
    geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
    labs(x = "Studentized Residual", y = "Density")
 
   histogram + 
     stat_function(fun = dnorm, 
                   args = list(mean = mean(album_sales_2$studentized_residuals, na.rm = TRUE), 
                               sd = sd(album_sales_2$studentized_residuals, na.rm = TRUE)), 
                   colour = "red", size = 1)
   
   
   
   # Q-Q plot of studentized residuals 
   
    # The straight line in this plot represents 
    # a normal distribution, and the points represent the observed residuals. 
    # Therefore, in a perfectly normally distributed data set, all points will lie on the line.
  
    qqplot_resid <- qplot(sample = album_sales_2$studentized_residuals) + 
     labs(x = "Theoretical Values", y = "Observed Values")
  
     qqplot_resid
   
     
   
   # studentized residuals against predicted values
   
      # Homoscedasticity (having the same scatter):
      # at each point along any predictor variable, the spread of residuals should be fairly constant.
   
   scatter <- ggplot(album_sales_2, aes(fitted, studentized_residuals))
   scatter + geom_point() + 
   geom_smooth(method = "lm", colour = "Blue", se = FALSE) + 
   labs(x = "Fitted Values", y = "Studentized Residual")
   
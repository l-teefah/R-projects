Data available [at](./Album_Sales_2.dat)
# Read data from working directory 
album_sales <- read.delim("Album_Sales_2.dat")

# INFORMATION ON DATA (Field, A.P. (2017). Discovering Statistics using R: and sex and drugs and rock 'n' roll (5th ed.). SAGE.)
# variables explanation
# sales: Sales of each album (in thousands) in the week after release
# advert (later renamed to promotion): The amount (in thousands of pounds) spent promoting the album before release
# airplay: the number of times an album is played on Radio 1 (the UK’s biggest national radio station) during the week prior to release.
# attract: attractiveness of the band.

# Rename a variable 
names(album_sales)[1] = "promotion" 

# Display the pattern of your data with plotly
#install packages if you don't have them installed on your pc
# install.packages("plotly")
# install.packages("tidyverse")
library(plotly)
library(tidyverse)


# plot 3D interactive scatter plot 
# it shows how radio airplay and promotion budgets might influence album sales.
fig <- plot_ly(album_sales, x = ~airplay, y = ~sales, z = ~promotion)
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'No. of Plays on Radio 1 per Week'),
                                   yaxis = list(title = 'Album Sales (Thousands)'),
                                   zaxis = list(title = 'Promotion Budget (Thousands of Pounds')
                                   ))


fig


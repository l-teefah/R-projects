# 3D Interactive Album Sales Visualization

## Overview
This R script creates an interactive 3D scatter plot to visualize the relationship between album sales, promotion budget, and radio airplay. The visualization helps analyze how radio exposure and promotional spending influence album sales.

## Data Source
The script uses [data]("Album_Sales_2.dat) from "Album_Sales_2.dat", which is referenced in Field, A.P. (2017), "Discovering Statistics using R: and sex and drugs and rock 'n' roll (5th ed.)", SAGE Publications.

## Variables
The dataset contains the following variables:
- **sales**: Album sales (in thousands) in the week after release
- **promotion** (renamed from "advert"): Amount spent on promotion (in thousands of pounds) before album release
- **airplay**: Number of times the album was played on Radio 1 (UK's biggest national radio station) during the week prior to release
- **attract**: Attractiveness of the band (not used in this visualization)

## Requirements
The script requires the following R packages:
- plotly
- tidyverse

## Usage
1. Ensure the "Album_Sales_2.dat" file is in your working directory
2. Install the required packages if not already installed:
3. 
   ```R
   install.packages("plotly")
   install.packages("tidyverse")
   ```
   
4. Run the [script](3D_interactive_plot.R) to generate the interactive 3D scatter plot
5. Interact with the plot by rotating, zooming, and hovering over data points

## Output
The script produces an interactive 3D scatter plot with:
- X-axis: Number of plays on Radio 1 per week
- Y-axis: Album sales (in thousands)
- Z-axis: Promotion budget (in thousands of pounds)

This visualization allows users to explore the relationships between these three variables and identify patterns in album performance based on promotional strategies. It shows that there's a dense cluster of points in the middle of the plot, suggesting that most albums in this dataset have moderate values across all three variables, with some outliers having particularly high promotion budgets or sales figures.

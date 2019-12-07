# ----------------------------------
# Quiz: Obtain feature/column names of dataframe: ASD_State
# ----------------------------------
# Write your code here:
#

if(!require()){install.packages("")}
library(binom)


# 2019 12 06
# ----------------------------------
# EDA - Visualization on map
# ----------------------------------
# https://cran.r-project.org/web/packages/usmap/vignettes/mapping.html
if(!require(usmap)){install.packages("usmap")}
library(usmap) # usmap: Mapping the US
if(!require(ggplot2)){install.packages("ggplot2")}
library(ggplot2)

# ----------------------------------
# US map 
# ----------------------------------
plot_usmap(regions = "counties") + 
  labs(title = "US Counties",
       subtitle = "This is a blank map of the counties of the United States.") + 
  theme(panel.background = element_rect(color = "white", fill = "white"))

# Add some data to the map
plot_usmap(data = statepop, values = "pop_2015", color = "grey") + 
  scale_fill_continuous(name = "Population (2015)", label = scales::comma) + 
  theme(legend.position = "right")

# Backup on Dec 06

# ----------------------------------
# EDA - Visualization on map
# ----------------------------------
# https://cran.r-project.org/web/packages/usmap/vignettes/mapping.html
if(!require(usmap)){install.packages("usmap")}
library(usmap) # usmap: Mapping the US

# ----------------------------------
# < Prevalence Estimates by Geographic Area >
# ----------------------------------

# Available data in followng years by data sources:
# ASD_State_ADDM # Year: 2000, 2002, 2004, 2006, 2008, 2010, 2012, 2014
# ASD_State_MEDI # Year: 2000 ~ 2012
# ASD_State_NSCH # Year: 2004, 2008, 2012, 2016
# ASD_State_SPED # Year: 2000 ~ 2016

# Prepare data
Map_Data_Source = 'addm' # Available values lowercase: 'addm', 'medi', 'nsch', 'sped'.
Map_Data_Year = 2012 # must be integer
Map_Data_Value = 'Prevalence' # variable must be numeric, variable name in 'quotation'. Or else Error: Discrete value supplied to continuous scale

ASD_State_Subset = subset(ASD_State, Source == Map_Data_Source & Year == Map_Data_Year)
ASD_State_Subset$state = ASD_State_Subset$State

# Show data on map
plot_usmap(data = ASD_State_Subset, values = Map_Data_Value, color = "darkgrey") + 
  scale_fill_continuous(
    low="lightblue", high = "darkblue", name = "Prevalence per 1,000 Children:", label = scales::comma,
    limits=c(0, 40) #same colour levels/limits for plots
  ) +
  labs(title = "Prevalence Estimates by Geographic Area",
       subtitle = paste("Prevalence by US. State    Source [", Map_Data_Source, "] Year [", Map_Data_Year, "]")
  ) + 
  theme(panel.background = element_rect(color = "white", fill = "white"),
        legend.position = "right")


# Prepare data (sped 2016)
Map_Data_Source = 'sped' # Available values lowercase: 'addm', 'medi', 'nsch', 'sped'.
Map_Data_Year = 2016 # must be integer
Map_Data_Value = 'Prevalence' # variable must be numeric, variable name in 'quotation'. Or else Error: Discrete value supplied to continuous scale
ASD_State_Subset = subset(ASD_State, Source == Map_Data_Source & Year == Map_Data_Year)
ASD_State_Subset$state = ASD_State_Subset$State
plot_usmap(data = ASD_State_Subset, values = Map_Data_Value, color = "darkgrey") + scale_fill_continuous(low="lightblue", high = "darkblue", name = "Prevalence per 1,000 Children:", label = scales::comma, limits=c(0, 40)) + labs(title = "Prevalence Estimates by Geographic Area", subtitle = paste("Prevalence by US. State    Source [", Map_Data_Source, "] Year [", Map_Data_Year, "]")) + theme(panel.background = element_rect(color = "white", fill = "white"), legend.position = "right")
# Prepare data (sped 2015)
Map_Data_Source = 'sped' # Available values lowercase: 'addm', 'medi', 'nsch', 'sped'.
Map_Data_Year = 2015 # must be integer
Map_Data_Value = 'Prevalence' # variable must be numeric, variable name in 'quotation'. Or else Error: Discrete value supplied to continuous scale
ASD_State_Subset = subset(ASD_State, Source == Map_Data_Source & Year == Map_Data_Year)
ASD_State_Subset$state = ASD_State_Subset$State
plot_usmap(data = ASD_State_Subset, values = Map_Data_Value, color = "darkgrey") + scale_fill_continuous(low="lightblue", high = "darkblue", name = "Prevalence per 1,000 Children:", label = scales::comma, limits=c(0, 40)) + labs(title = "Prevalence Estimates by Geographic Area", subtitle = paste("Prevalence by US. State    Source [", Map_Data_Source, "] Year [", Map_Data_Year, "]")) + theme(panel.background = element_rect(color = "white", fill = "white"), legend.position = "right")
# Prepare data (sped 2014)
Map_Data_Source = 'sped' # Available values lowercase: 'addm', 'medi', 'nsch', 'sped'.
Map_Data_Year = 2014 # must be integer
Map_Data_Value = 'Prevalence' # variable must be numeric, variable name in 'quotation'. Or else Error: Discrete value supplied to continuous scale
ASD_State_Subset = subset(ASD_State, Source == Map_Data_Source & Year == Map_Data_Year)
ASD_State_Subset$state = ASD_State_Subset$State
plot_usmap(data = ASD_State_Subset, values = Map_Data_Value, color = "darkgrey") + scale_fill_continuous(low="lightblue", high = "darkblue", name = "Prevalence per 1,000 Children:", label = scales::comma, limits=c(0, 40)) + labs(title = "Prevalence Estimates by Geographic Area", subtitle = paste("Prevalence by US. State    Source [", Map_Data_Source, "] Year [", Map_Data_Year, "]")) + theme(panel.background = element_rect(color = "white", fill = "white"), legend.position = "right")

# ----------------------------------
# Quiz: Obtain feature/column names of dataframe: ASD_State
# ----------------------------------
# Write your code here:
#

if(!require()){install.packages("")}
library(binom)



# ----------------------------------
# EDA - Visualization on map
# ----------------------------------
# https://cran.r-project.org/web/packages/usmap/vignettes/mapping.html
if(!require(usmap)){install.packages("usmap")}
library(usmap) # usmap: Mapping the US

# 2019 12 07
# ----------------------------------
# https://cran.r-project.org/web/packages/usmap/readme/README.html
# ----------------------------------

library(usmap)
library(ggplot2)

# Blank state map ####
blank_state_map <- plot_usmap()
blank_state_map

# Blank county map ####
blank_county_map <- plot_usmap("counties")
blank_county_map

# Population by state ####
state_pop_map <-
  plot_usmap(data = statepop, values = "pop_2015") +
  scale_fill_continuous(low = "white", high = "red", guide = FALSE) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))
state_pop_map

# Population by state with labels ####
state_pop_map_labeled <-
  plot_usmap(data = statepop, values = "pop_2015", labels = TRUE) +
  scale_fill_continuous(low = "white", high = "red", guide = FALSE) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))
state_pop_map_labeled

# Population by county ####
county_pop_map <-
  plot_usmap(data = countypop, values = "pop_2015") +
  scale_fill_continuous(low = "blue", high = "yellow", guide = FALSE) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))
county_pop_map

# Poverty percentage by county ####
county_pov_map <-
  plot_usmap(data = countypov, values = "pct_pov_2014") +
  scale_fill_continuous(low = "blue", high = "yellow", guide = FALSE) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))
county_pov_map

if(!require(cowplot)){install.packages("cowplot")}
library('cowplot')

# Combine plots ####
cowplot::plot_grid(
  blank_state_map,
  state_pop_map,
  state_pop_map_labeled,
  blank_county_map,
  county_pop_map,
  county_pov_map,
  nrow = 2
)

# Save plots ####
ggsave("usmap_example.png", width = 40, height = 20, units = 'cm')






p_map <- plot_usmap(data = ASD_State_Subset, values = Map_Data_Value, 
                    color = "white", # map line colour
                    labels = TRUE,  # State name shown
                    label_color = 'white' # State name colour
) + 
  scale_fill_continuous(
    na.value = "lightgrey", # Set colour with no State data
    low="lightblue1", high = "darkblue", name = "Prevalence\nper 1,000\nChildren", label = scales::comma,
    limits=c(0, 40) #same colour levels/limits for plots
  ) +
  labs(title = "Prevalence Estimates by Geographic Area",
       subtitle = paste("Prevalence by US. State    Source [", Map_Data_Source, "] Year [", Map_Data_Year, "]")
  ) + 
  theme(panel.background = element_rect(color = "white", fill = "white"),
        legend.position = "right")

  
p_map



# ----------------------------------
# ASD_State_NSCH # Year: 2004, 2008, 2012, 2016
# ----------------------------------
# Prepare data - nsch 2004
Map_Data_Year = 2004 # must be integer
ASD_State_Subset = subset(ASD_State, Source == Map_Data_Source & Year == Map_Data_Year)
ASD_State_Subset$state = ASD_State_Subset$State
p_map_nsch_2004 <- plot_usmap(data = ASD_State_Subset, values = Map_Data_Value, color = "white", labels = TRUE, label_color = 'white' ) + scale_fill_continuous(na.value = "lightgrey", low="lightblue1", high = "darkblue", name = "Prevalence\nper 1,000\nChildren", limits=c(0, 40) ) + labs(title = "Prevalence Estimates by Geographic Area", subtitle = paste("Prevalence by US. State    Source [", Map_Data_Source, "] Year [", Map_Data_Year, "]") ) + theme(panel.background = element_rect(color = "white", fill = "white"), legend.position = "right")
p_map_nsch_2004
# Prepare data - nsch 2008
Map_Data_Year = 2008 # must be integer
ASD_State_Subset = subset(ASD_State, Source == Map_Data_Source & Year == Map_Data_Year)
ASD_State_Subset$state = ASD_State_Subset$State
p_map_nsch_2008 <- plot_usmap(data = ASD_State_Subset, values = Map_Data_Value, color = "white", labels = TRUE, label_color = 'white' ) + scale_fill_continuous(na.value = "lightgrey", low="lightblue1", high = "darkblue", name = "Prevalence\nper 1,000\nChildren", limits=c(0, 40) ) + labs(title = "Prevalence Estimates by Geographic Area", subtitle = paste("Prevalence by US. State    Source [", Map_Data_Source, "] Year [", Map_Data_Year, "]") ) + theme(panel.background = element_rect(color = "white", fill = "white"), legend.position = "right")
p_map_nsch_2008
# Prepare data - nsch 2012
Map_Data_Year = 2012 # must be integer
ASD_State_Subset = subset(ASD_State, Source == Map_Data_Source & Year == Map_Data_Year)
ASD_State_Subset$state = ASD_State_Subset$State
p_map_nsch_2012 <- plot_usmap(data = ASD_State_Subset, values = Map_Data_Value, color = "white", labels = TRUE, label_color = 'white' ) + scale_fill_continuous(na.value = "lightgrey", low="lightblue1", high = "darkblue", name = "Prevalence\nper 1,000\nChildren", limits=c(0, 40) ) + labs(title = "Prevalence Estimates by Geographic Area", subtitle = paste("Prevalence by US. State    Source [", Map_Data_Source, "] Year [", Map_Data_Year, "]") ) + theme(panel.background = element_rect(color = "white", fill = "white"), legend.position = "right")
p_map_nsch_2012
# Prepare data - nsch 2016
Map_Data_Source = 'nsch' # Available values lowercase: 'addm', 'medi', 'nsch', 'sped'.
Map_Data_Year = 2016 # must be integer
Map_Data_Value = 'Prevalence' # variable must be numeric, variable name in 'quotation'. Or else Error: Discrete value supplied to continuous scale
ASD_State_Subset = subset(ASD_State, Source == Map_Data_Source & Year == Map_Data_Year)
ASD_State_Subset$state = ASD_State_Subset$State
p_map_nsch_2016 <- plot_usmap(data = ASD_State_Subset, values = Map_Data_Value, color = "white", labels = TRUE, label_color = 'white' ) + scale_fill_continuous(na.value = "lightgrey", low="lightblue1", high = "darkblue", name = "Prevalence\nper 1,000\nChildren", limits=c(0, 40) ) + labs(title = "Prevalence Estimates by Geographic Area", subtitle = paste("Prevalence by US. State    Source [", Map_Data_Source, "] Year [", Map_Data_Year, "]") ) + theme(panel.background = element_rect(color = "white", fill = "white"), legend.position = "right")
p_map_nsch_2016

# Combine mutiple plots 
if(!require(cowplot)){install.packages("cowplot")}
library('cowplot')
cowplot::plot_grid(
  p_map_nsch_2004,
  p_map_nsch_2008,
  p_map_nsch_2012,
  p_map_nsch_2016,
  nrow = 2)

# Save as image file
ggsave("Prevalence Estimates by Geographic Area [NSCH] [2004-2016].png", width = 40, height = 20, units = 'cm')


# ----------------------------------
# 'The End'... is called 'The New Start'.
# ----------------------------------
# GU Zhan (Sam)
# zhan.gu@nus.edu.sg
# ----------------------------------

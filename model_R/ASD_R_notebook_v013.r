getwd()

# setwd("/media/sf_vm_shared_folder/git/DDC/DDC-ASD/model_R")
# setwd('~/Desktop/admin-desktop/vm_shared_folder/git/DDC-ASD/model_R')
getwd()

# Dataset: US. National Level Children ASD Prevalence
ASD_National <- read.csv("../dataset/ADV_ASD_National.csv", stringsAsFactors = FALSE)

# Dataset: US. State Level Children ASD Prevalence
ASD_State    <- read.csv("../dataset/ADV_ASD_State.csv", stringsAsFactors = FALSE)

dim(ASD_National)

dim(ASD_State)

str(ASD_National)

str(ASD_State)

head(ASD_National)

head(ASD_State)

names(ASD_National)

names(ASD_State)

cbind(names(ASD_National), c(1:length(names(ASD_National))))

str(ASD_National[, c(1:8, 24, 25, 26)])

# Write your code below and press Shift+Enter to execute 


# use column index:
ASD_National[1]

typeof(ASD_National[1])

ASD_National[1]$Source

# use column name:
ASD_National["Source"]

typeof(ASD_National['Source'])

ASD_National['Source']$Source

ASD_National[, 1]

typeof(ASD_National[, 1])

ASD_National[, 1]

# or
ASD_National[, "Source"]

# or
ASD_National$Source

length(ASD_National) # number of features/columns

length(ASD_National[1, ]) # number of features/columns

length(ASD_National[, 1]) # number of elements(rows) in column 1

length(ASD_National[, "Source"]) # same as above

length(ASD_National$Source) # number of elements in chr list

# using column index
ASD_National[1][1, ]

ASD_National[1][11, ]

ASD_National[1][11:20, ]

# using column name
ASD_National["Source"][1, ]

ASD_National["Source"][11, ]

ASD_National["Source"][11:20, ]

# using column index
ASD_National[, 1][1]

ASD_National[, 1][11]

ASD_National[, 1][11:20]

# using column name
ASD_National[, "Source"][1]

# using column name
ASD_National[, "Source"][11]

# using column name
ASD_National[, "Source"][11:20]

# using $ operator
ASD_National$Source[1]

ASD_National$Source[11]

ASD_National$Source[11:20]

cbind(names(ASD_National), c(1:length(names(ASD_National))))

ASD_National[1, 1] # row 1, column 1: "Source" 

ASD_National[10, 1] # row 10, column 1: "Source"

ASD_National[1, 3] # row 1, column 3: "Prevalence"

ASD_National[10, 3] # row 10, column 3: "Prevalence"

ASD_National[1:10, 1:3] # row 1 to 10 from column 1 to 3

# or using columns names
ASD_National[1:10, c('Source', 'Year', 'Prevalence')]

ASD_National[c(1:10, 20, 30:35), c(1:3, 9, 12)] # row 1 to 10, 20, and 20 to 25 from column 1 to 3, 9, and 12

sum(is.na(ASD_National)) # No missing data recognised by R (NA)

sum(is.na(ASD_State)) # Some missing data recognised by R (NA)

# Define several offending strings
na_strings <- c("", "No data", "NA", "N A", "N / A", "N/A", "N/ A", "Not Available", "NOt available")

# Load required function from packages:
if(!require(naniar)){install.packages("naniar")}
library(naniar)
if(!require(dplyr)){install.packages("dplyr")}
library(dplyr)

# Uncomment below to show help
# ?replace_with_na_all # Documentation

# "~.x" is a reserved keyword of this function:
ASD_National = replace_with_na_all(ASD_National, condition = ~.x %in% na_strings) 

# Count missing values (R's internal NA) in dataframe:
sum(is.na(ASD_National))

ASD_National$Source_Full1[ASD_National$Source_Full1 == "National Survey of Children\x92s Health"] <- 
"National Survey of Children's Health"

ASD_National$Source_Full2[ASD_National$Source_Full2 == "nsch-National Survey of Children\x92s Health"] <- 
"nsch-National Survey of Children's Health"

drop <- c("Prevalence_dup", "Dummy Variable Name")

ASD_National = ASD_National[, !(names(ASD_National) %in% drop)] # Recall Dataframe[rows,columns]

ASD_National$Source_UC <- paste(toupper(ASD_National$Source))

ASD_National$Source_Full3 <- paste(toupper(ASD_National$Source), ASD_National$Source_Full1)

str(ASD_National)
cbind(names(ASD_National), c(1:length(names(ASD_National))))

ix <- 8:25 # define an index
# apply()
ASD_National[ix] <- apply(ASD_National[ix], 2, as.numeric) # "2" meand column-wise; "1" means row-wise.

# Uncomment below to show help
# ?apply # Documentation

# or lapply()
ASD_National[ix] <- lapply(ASD_National[ix], as.numeric) # column-wise

# Uncomment below to show help
# ?lapply # Documentation

ix <- c(1, 6, 7, 26, 27) # define an index
ASD_National[ix] <- lapply(ASD_National[ix], as.factor)

ASD_National$Year_Factor <- factor(ASD_National$Year, ordered = TRUE)

# Observe the difference of 'Levels' in below two factors
ASD_National$Year_Factor # Ordinal categorical variable
str(ASD_National$Year_Factor)

ASD_National$Source # Nominal categorical variable
str(ASD_National$Source)

# Optionally, below is manual conversion examples:
# ASD_National$Male.Prevalence = as.numeric(ASD_National$Male.Prevalence)
# ASD_National$Source = as.factor(ASD_National$Source)

write.csv(ASD_National, file = "../dataset/ADV_ASD_National_R.csv", row.names = FALSE)

# Read back in above saved file:
# ASD_National <- read.csv("../dataset/ADV_ASD_National_R.csv")
# ASD_National$Year_Factor <- factor(ASD_National$Year_Factor, ordered = TRUE) # Convert Year_Factor to ordered.factor

summary(ASD_National)

# Filter only numeric variables/columns
select_if(ASD_National, is.numeric) # library(dplyr)

# Data summarization
summary(select_if(ASD_National, is.numeric))

# Calculate average Prevalence, no error
mean(ASD_National$Prevalence)
mean(ASD_National$Prevalence[ASD_National$Source == 'addm'])
mean(ASD_National$Prevalence[ASD_National$Source == 'medi'])
mean(ASD_National$Prevalence[ASD_National$Source == 'nsch'])
mean(ASD_National$Prevalence[ASD_National$Source == 'sped'])

# Calculate average Male.Prevalence, there is error!
mean(ASD_National$Male.Prevalence)

# Because of NA, mean() cannot process, thus we use na.rm to ignore NAs
mean(ASD_National$Male.Prevalence, na.rm = TRUE)

mean(ASD_National$Female.Prevalence, na.rm = TRUE)

# Count occurrences of uniques values in a variable/column: number of rows (of data entry) per year
table(ASD_National$Year) # ?table

# List of categorical variables
names(select_if(ASD_National, is.factor)) # All categorical variables are factor data type
names(select_if(ASD_National, is.character)) # No categorical variable is character data type

# Look at summary
summary(select_if(ASD_National, is.factor))

summary(select_if(ASD_National, is.character))

# Count occurrences of uniques values in a variable/column
table(ASD_National$Source)

table(ASD_National$Source_Full3)

table(ASD_National$Year_Factor)

table(ASD_National$Prevalence) # numeric is also possible

# Display unique values (levels) of a factor categorical 
lapply(select_if(ASD_National, is.factor), levels)

# or using variable names
lapply(ASD_National[c('Source_UC', 'Year_Factor')], levels)

# Pivot of counting occurrences
table(ASD_National$Source_Full3, ASD_National$Year) # table(ASD_National$Year, ASD_National$Source_Full3)

# library(repr)
# Adjust in-line plot size to M x N
options(repr.plot.width=8, repr.plot.height=4)

hist(ASD_National$Prevalence)

par(mfrow=c(1, 2)) # multiple plots on one page: row split to: 1,column split to: 2
hist(ASD_National$Male.Prevalence)
hist(ASD_National$Female.Prevalence)
par(mfrow=c(1, 1)) # Reset to one plot on one page

# Histogram with annotations
hist(ASD_National$Prevalence,
     main = "Frequency of National ASD Prevalence", # Chart title
     xlab = "Prevalence per 1,000 Children", # x axis label
     ylab = "Frequency or Occurrences",# y axis label
     sub  = "Year 2000 - 2016", # Chart subtitle at bottom
     col.main="blue", col.lab="black", col.sub="darkgrey") # Colours

par(mfrow=c(1, 2)) # multiple plots on one page: row split to: 1,column split to: 2

plot(density(ASD_National$Prevalence))

# Density plot with annotations
plot(density(ASD_National$Prevalence),
     main = "Density of National ASD Prevalence",
     xlab = "Prevalence per 1,000 Children",
     ylab = "Frequency or Occurrences",
     sub  = "Year 2000 - 2016",
     col.main="blue", col.lab="black", col.sub="darkgrey")

par(mfrow=c(1, 1))

par(mfrow=c(1, 2)) # multiple plots on one page: row split to: 1,column split to: 2

# All children prevalence with and without 95% confidence side by side:
boxplot(ASD_National$Prevalence, notch = TRUE) # 95% confidence interval - a notch is drawn in each side of the boxes. If the notches of two plots do not overlap this is ‘strong evidence’ that the two medians differ
boxplot(ASD_National$Prevalence) # All children

par(mfrow=c(1, 1))

par(mfrow=c(1, 2)) # multiple plots on one page: row split to: 1,column split to: 2

# Male prevalence and Female prevalence side by side:
boxplot(ASD_National$Male.Prevalence, ylim = c(0, 35), notch = TRUE) # Male children
boxplot(ASD_National$Female.Prevalence, ylim = c(0, 35), notch = TRUE) # Female children

par(mfrow=c(1, 1))

# Display value ranges
# numeric:
range(ASD_National$Prevalence)

range(ASD_National$Year)

# categorical:
min(ASD_National$Year_Factor)

max(ASD_National$Year_Factor)

# Create 'Prevalence' box plots break by 'Source'
boxplot(ASD_National$Prevalence ~ ASD_National$Source,
        main = "National ASD Prevalence by Data Source",
        xlab = "Data Source",
        ylab = "Prevalence per 1,000 Children",
        sub  = "Year 2000 - 2016",
        col.main="blue", col.lab="black", col.sub="darkgrey")

# Write your code below and press Shift+Enter to execute 


# Adjust in-line plot size to M x N
options(repr.plot.width=8, repr.plot.height=6)

# ----------------------------------
# [National] < Prevalence has changed over Time >
# ----------------------------------
# Prevalence over Year
# Use Year        as x-axis: y value Prevalence is NOT aggregated for different data sources
plot(ASD_National$Year, ASD_National$Prevalence) 

# Use Year_factor as x-axis: y value Prevalence is     aggregated for different data sources
plot(ASD_National$Year_Factor, ASD_National$Prevalence) 

# table(ASD_National$Source_Full3)

par(mfrow=c(2, 2))

# Prevalence over Year, from data source: 
# addm-Autism & Developmental Disabilities Monitoring Network
plot(ASD_National$Year[ASD_National$Source == 'addm'], 
     ASD_National$Prevalence[ASD_National$Source == 'addm'])

# Prevalence over Year, from data source: 
# medi-Medicaid
plot(ASD_National$Year[ASD_National$Source == 'medi'], 
     ASD_National$Prevalence[ASD_National$Source == 'medi'])

# Prevalence over Year, from data source: 
# nsch-National Survey of Children Health
plot(ASD_National$Year[ASD_National$Source == 'nsch'], 
     ASD_National$Prevalence[ASD_National$Source == 'nsch'])

# Prevalence over Year, from data source: 
# sped-Special Education Child Count
plot(ASD_National$Year[ASD_National$Source == 'sped'], 
     ASD_National$Prevalence[ASD_National$Source == 'sped'])

par(mfrow=c(1, 1)) # Reset to one plot on one page

# ----------------------------------
# Add more annotations to above plots
# ----------------------------------
# Color list
# addm : darkblue
# medi : orange
# nsch : darkred
# sped : skyblue

par(mfrow=c(2, 2))

# Prevalence over Year, from data source: 
# addm-Autism & Developmental Disabilities Monitoring Network
plot(ASD_National$Year[ASD_National$Source == 'addm'], 
     ASD_National$Prevalence[ASD_National$Source == 'addm'],
     type="l", # dot/point type
     lty=1, # line type
     lwd=3, # line width
     col="darkblue", # line color
     xlab="Year", 
     ylab="Prevalence per 1,000 Children", 
     ylim = c(0, 30), # Set value range of y axis
     main="[addm] Prevalence Estimates Over Time",
     sub  = "zhan.gu@nus.edu.sg",
     col.main="blue", col.lab="black", col.sub="darkgrey")

# Prevalence over Year, from data source: 
# medi-Medicaid
plot(ASD_National$Year[ASD_National$Source == 'medi'], 
     ASD_National$Prevalence[ASD_National$Source == 'medi'],
     type="b", lty=1, lwd=3,  col="orange",
     xlab="Year", 
     ylab="Prevalence per 1,000 Children", 
     ylim = c(0, 30), # Set value range of y axis
     main="[medi] Prevalence Estimates Over Time",
     sub  = "zhan.gu@nus.edu.sg",
     col.main="blue", col.lab="black", col.sub="darkgrey")

# Prevalence over Year, from data source: 
# nsch-National Survey of Children Health
plot(ASD_National$Year[ASD_National$Source == 'nsch'], 
     ASD_National$Prevalence[ASD_National$Source == 'nsch'],
     type="l", lty=2, lwd=3,  col="darkred",
     xlab="Year", 
     ylab="Prevalence per 1,000 Children", 
     ylim = c(0, 30), # Set value range of y axis
     main="[nsch] Prevalence Estimates Over Time",
     sub  = "zhan.gu@nus.edu.sg",
     col.main="blue", col.lab="black", col.sub="darkgrey")

# Prevalence over Year, from data source: 
# sped-Special Education Child Count
plot(ASD_National$Year[ASD_National$Source == 'sped'], 
     ASD_National$Prevalence[ASD_National$Source == 'sped'],
     type="l", lty=3, lwd=3,  col="skyblue",
     xlab="Year", 
     ylab="Prevalence per 1,000 Children", 
     ylim = c(0, 30), # Set value range of y axis
     main="[sped] Prevalence Estimates Over Time",
     sub  = "zhan.gu@nus.edu.sg",
     col.main="blue", col.lab="black", col.sub="darkgrey")

par(mfrow=c(1, 1)) # Reset to one plot on one page

# ----------------------------------
# [National] < Prevalence Varies over Time/Year by Data Source >
# ----------------------------------
# Create a first line
plot(ASD_National$Year[ASD_National$Source == 'addm'], 
     ASD_National$Prevalence[ASD_National$Source == 'addm'], 
     col = "darkblue", lty = 1, lwd = 2,
     type = "b", # use dot/point
     pch = 0, # dot/point type: http://www.endmemo.com/program/R/pchsymbols.php
     xlab="Year", 
     xlim=c(2000, 2016), # Set x axis value range
     ylab="Prevalence per 1,000 Children", 
     ylim=c(0, 30), # Set y axis value range
     main="Prevalence Estimates Over Time by Data Source",
     col.main="black", col.lab="black", col.sub="grey",
     frame = FALSE, # Remove frame
     axes=FALSE # Remove x and y axis
)
axis(1, at=seq(2000, 2016, 1)) # Customize x axis
axis(2, at=seq(0, 30, 5)) # Customize y axis

# Add another line
lines(ASD_National$Year[ASD_National$Source == 'medi'], 
      ASD_National$Prevalence[ASD_National$Source == 'medi'], 
      pch = 1, col = "orange", type = "b", lty = 1, lwd = 2
)
# Add another line
lines(ASD_National$Year[ASD_National$Source == 'nsch'], 
      ASD_National$Prevalence[ASD_National$Source == 'nsch'], 
      pch = 2, col = "darkred", type = "b", lty = 1, lwd = 2
)
# Add another line
lines(ASD_National$Year[ASD_National$Source == 'sped'], 
      ASD_National$Prevalence[ASD_National$Source == 'sped'], 
      pch = 5, col = "skyblue", type = "b", lty = 1, lwd = 2
)
# Add a legend to the plot
legend("topleft", legend=levels(ASD_National$Source),
       col=c("darkblue", "orange", "darkred", "skyblue"), 
       pch = 20, # dot in a line
       lty = 1, # line type
       lwd = 2, # line width
       cex=0.8, # size of text
       bty = 'n' # Without frame
)


# ----------------------------------
# [addm] < Prevalence Varies by Sex >
# ----------------------------------
# Create a first line
plot(ASD_National$Year[ASD_National$Source == 'addm'], 
     ASD_National$Prevalence[ASD_National$Source == 'addm'], 
     col = "grey", lty = 1, lwd = 2,
     type = "l", # use dot/point
     pch = 0, # dot/point type: http://www.endmemo.com/program/R/pchsymbols.php
     xlab="Year", 
     xlim=c(2000, 2016), # Set x axis value range
     ylab="Prevalence per 1,000 Children", 
     ylim=c(0, 30), # Set y axis value range
     main="Prevalence Estimates by Sex [ADDM]",
     col.main="black", col.lab="black", col.sub="grey",
     frame = FALSE, # Remove frame
     axes=FALSE # Remove x and y axis
)
axis(1, at=seq(2000, 2016, 1)) # Customize x axis
axis(2, at=seq(0, 30, 5)) # Customize y axis

# Add Female prevalence
lines(ASD_National$Year[ASD_National$Source == 'addm'], 
      ASD_National$Female.Prevalence[ASD_National$Source == 'addm'], 
      pch = 1, col = "orange", type = "l", lty = 1, lwd = 2)
# Add Female prevalence lower CI
lines(ASD_National$Year[ASD_National$Source == 'addm'], 
      ASD_National$Female.Lower.CI[ASD_National$Source == 'addm'], 
      pch = 1, col = "orange", type = "l", lty = 3, lwd = 1)
# Add Female prevalence upper CI
lines(ASD_National$Year[ASD_National$Source == 'addm'], 
      ASD_National$Female.Upper.CI[ASD_National$Source == 'addm'], 
      pch = 1, col = "orange", type = "l", lty = 3, lwd = 1)

# Add Male prevalence
lines(ASD_National$Year[ASD_National$Source == 'addm'], 
      ASD_National$Male.Prevalence[ASD_National$Source == 'addm'], 
      pch = 1, col = "blue", type = "l", lty = 1, lwd = 2)
# Add Male prevalence lower CI
lines(ASD_National$Year[ASD_National$Source == 'addm'], 
      ASD_National$Male.Lower.CI[ASD_National$Source == 'addm'], 
      pch = 1, col = "blue", type = "l", lty = 3, lwd = 1)
# Add Male prevalence upper CI
lines(ASD_National$Year[ASD_National$Source == 'addm'], 
      ASD_National$Male.Upper.CI[ASD_National$Source == 'addm'], 
      pch = 1, col = "blue", type = "l", lty = 3, lwd = 1)
# Add a legend to the plot
legend("topleft", legend=c('ADDM Average', 'Female with 95% CI', 'Male with 95% CI'),
       col=c("grey", "orange", "blue"), 
       #       pch = 20, # dot in a line
       lty = 1, # line type
       lwd = 2, # line width
       cex=0.8, # size of text
       bty = 'n' # Without frame
)


# ----------------------------------
# [addm] < Prevalence Varies by Race and Ethnicity >
# ----------------------------------
# Create a first line
plot(ASD_National$Year[ASD_National$Source == 'addm'], 
     ASD_National$Prevalence[ASD_National$Source == 'addm'], 
     col = "grey", lty = 1, lwd = 2,
     type = "l", # use dot/point
     pch = 0, # dot/point type: http://www.endmemo.com/program/R/pchsymbols.php
     xlab="Year", 
     xlim=c(2000, 2016), # Set x axis value range
     ylab="Prevalence per 1,000 Children", 
     ylim=c(0, 30), # Set y axis value range
     main="Prevalence Estimates by Race/Ethnicity [ADDM]",
     col.main="black", col.lab="black", col.sub="grey",
     frame = FALSE, # Remove frame
     axes=FALSE # Remove x and y axis
)
axis(1, at=seq(2000, 2016, 1)) # Customize x axis
axis(2, at=seq(0, 30, 5)) # Customize y axis

# R plot colour list: https://www.r-graph-gallery.com/42-colors-names.html

# Add Asian.or.Pacific.Islander.Prevalence
lines(ASD_National$Year[ASD_National$Source == 'addm'], 
      ASD_National$Asian.or.Pacific.Islander.Prevalence[ASD_National$Source == 'addm'], 
      pch = 20, col = "darkred", type = "b", lty = 1, lwd = 2)
# Add Hispanic.Prevalence
lines(ASD_National$Year[ASD_National$Source == 'addm'], 
      ASD_National$Hispanic.Prevalence[ASD_National$Source == 'addm'], 
      pch = 20, col = "darkorchid3", type = "b", lty = 1, lwd = 2)
# Add Non.hispanic.black.Prevalence
lines(ASD_National$Year[ASD_National$Source == 'addm'], 
      ASD_National$Non.hispanic.black.Prevalence[ASD_National$Source == 'addm'], 
      pch = 20, col = "deepskyblue3", type = "b", lty = 1, lwd = 2)
# Add Non.hispanic.white.Prevalence
lines(ASD_National$Year[ASD_National$Source == 'addm'], 
      ASD_National$Non.hispanic.white.Prevalence[ASD_National$Source == 'addm'], 
      pch = 20, col = "chartreuse3", type = "b", lty = 1, lwd = 2)

# Add a legend to the plot
legend("topleft", legend=c('ADDM Average', 
                           'Non-Hispanic White',
                           'Non-Hispanic Black',
                           'Hispanic', 
                           'Asian/Pacific Islander'),
       col=c("grey", "chartreuse3", "deepskyblue3", "darkorchid3", "darkred"), 
       pch = 20, # dot in a line
       lty = 1, # line type
       lwd = 2, # line width
       cex=0.8, # size of text
       bty = 'n' # Without frame
)


# Adjust in-line plot size to M x N
options(repr.plot.width=8, repr.plot.height=4)

# Write your code below and press Shift+Enter to execute 


# Write your code below and press Shift+Enter to execute 




# Write your code below and press Shift+Enter to execute 


# Write your code below and press Shift+Enter to execute 


# Write your code below and press Shift+Enter to execute 


if(!require(ggplot2)){install.packages("ggplot2")}
library(ggplot2)

# Adjust in-line plot size to M x N
options(repr.plot.width=8, repr.plot.height=4)

# ----------------------------------
# [National] < Years Data Available >
# ----------------------------------
p = ggplot(ASD_National, aes(x = 1, fill = Source)) + 
  geom_bar() + theme(axis.text.x=element_blank(),  # Hide axis
                     axis.ticks.x=element_blank(), # Hide axis
                     axis.text.y=element_blank(),  # Hide axis
                     axis.ticks.y=element_blank(), # Hide axis
                     panel.background = element_blank(), # Remove panel background
                     legend.position="top"
  ) + 
  scale_fill_manual("Data Source:", values = c("addm" = "darkblue", 
                                               "medi" = "orange", 
                                               "nsch" = "darkred",
                                               "sped" = "skyblue")) +
  labs(x="", y="", title="Years Data Available") + # layers of graphics
  facet_grid(facets = Source~Year)
# Show plot
p

# Create bar chart using R graphics
barplot(table(ASD_National$Source))

# Create bar chart using ggplot2
ggplot(ASD_National, aes(x = Source)) + geom_bar(fill = "blue", alpha=0.5)

# Use color to differentiate sub-group data (Year)
ggplot(ASD_National, aes(x = Source, fill = factor(Year))) + geom_bar() + 
  theme(legend.position="top") + labs(fill = "Legend: Year")

# Split chart to mutiple columns by using: facets = . ~ Year
ggplot(ASD_National, aes(x = Source, fill = Source)) + geom_bar() + 
  theme(legend.position="top") + 
  scale_fill_manual("Data Source:", values = c("addm" = "darkblue", 
                                               "medi" = "orange", 
                                               "nsch" = "darkred",
                                               "sped" = "skyblue")) +
  facet_grid(facets = . ~ Year)

# Split chart to mutiple rows and columns by using: facets = Source ~ Year
ggplot(ASD_National, aes(x = Source, fill = Source)) + geom_bar() + 
  theme(legend.position="top") + 
  scale_fill_manual("Data Source:", values = c("addm" = "darkblue", 
                                               "medi" = "orange", 
                                               "nsch" = "darkred",
                                               "sped" = "skyblue")) +
  facet_grid(facets = Source~Year)

# Filter only data of ADDM
ASD_National_ADDM <- subset(ASD_National, Source == 'addm')
#
ASD_National_ADDM

# Construct a new re-shaped dataframe of [ Source: ADDM ] [Year: 2014]
#
Process_Source = 'addm'
Process_Year = 2014

# Create the vectors:
Sex.Group  = c('Overall', 
               'Boys', 
               'Girls')
Sex.Group

Prevalence = c(ASD_National_ADDM$Prevalence[ASD_National_ADDM$Year == Process_Year],
               ASD_National_ADDM$Male.Prevalence[ASD_National_ADDM$Year == Process_Year],
               ASD_National_ADDM$Female.Prevalence[ASD_National_ADDM$Year == Process_Year])
Prevalence

# Combine all the vectors into a data frame:
ASD_National_ADDM_Reshaped = data.frame(Sex.Group, Prevalence, stringsAsFactors=T)

# Add new columns:
ASD_National_ADDM_Reshaped$Source = Process_Source
ASD_National_ADDM_Reshaped$Year = Process_Year
#
ASD_National_ADDM_Reshaped

# Adjust in-line plot size to M x N
options(repr.plot.width=8, repr.plot.height=3)

ggplot(ASD_National_ADDM_Reshaped, aes(Sex.Group, Prevalence)) +
  geom_col(aes(fill = Sex.Group, colours = )) + # Use column chart
  geom_text(aes(label = Prevalence), vjust = +0.5, hjust = -0.2, size = 3) +
  scale_y_continuous(name = "Prevalence per 1,000 Children",
                     breaks = seq(0, 30, 5),
                     limits=c(0, 30)) +
  scale_x_discrete(name = "") +
  scale_fill_manual("Sex Group:", values = c("Overall" = "purple", 
                                             "Boys" = "blue",
                                             "Girls" = "orange")) + 
  ggtitle("Prevalence Estimates by Sex [ Source: ADDM ] [ Year: 2014 ]") +
  theme(title = element_text(face = 'bold.italic', color = "darkslategrey"), 
        axis.title = element_text(face = 'plain', color = "darkslategrey"),
        legend.position = 'none') + 
  coord_flip()  # Rotate chart
#  facet_grid(facets = Year ~ .)

# Create a new datafarme to hold re-shaped data for all years.
ASD_National_ADDM_Reshaped_All = ASD_National_ADDM_Reshaped

Process_Source = 'addm'
unique(ASD_National_ADDM$Year[!is.na(ASD_National_ADDM$Male.Prevalance)])

# 2000 2002 2004 2006 2008 2010 2012 2014
Process_Year = 2012

# Create the vectors:
Sex.Group  = c('Overall', 
               'Boys', 
               'Girls')
#Sex.Group

Prevalence = c(ASD_National_ADDM$Prevalence[ASD_National_ADDM$Year == Process_Year],
               ASD_National_ADDM$Male.Prevalence[ASD_National_ADDM$Year == Process_Year],
               ASD_National_ADDM$Female.Prevalence[ASD_National_ADDM$Year == Process_Year])
#Prevalence

# Combine all the vectors into a data frame:
ASD_National_ADDM_Reshaped = data.frame(Sex.Group, Prevalence, stringsAsFactors=T)

# Add new columns:
ASD_National_ADDM_Reshaped$Source = Process_Source
ASD_National_ADDM_Reshaped$Year = Process_Year
#
ASD_National_ADDM_Reshaped

# Append rows to existing dataframe
ASD_National_ADDM_Reshaped_All = rbind(ASD_National_ADDM_Reshaped_All, ASD_National_ADDM_Reshaped)
#ASD_National_ADDM_Reshaped_All

# 2000 2002 2004 2006 2008 2010 2012 2014
Process_Year = 2010

# Create the vectors:
Sex.Group  = c('Overall', 
               'Boys', 
               'Girls')
#Sex.Group

Prevalence = c(ASD_National_ADDM$Prevalence[ASD_National_ADDM$Year == Process_Year],
               ASD_National_ADDM$Male.Prevalence[ASD_National_ADDM$Year == Process_Year],
               ASD_National_ADDM$Female.Prevalence[ASD_National_ADDM$Year == Process_Year])
#Prevalence

# Combine all the vectors into a data frame:
ASD_National_ADDM_Reshaped = data.frame(Sex.Group, Prevalence, stringsAsFactors=T)

# Add new columns:
ASD_National_ADDM_Reshaped$Source = Process_Source
ASD_National_ADDM_Reshaped$Year = Process_Year
#
ASD_National_ADDM_Reshaped

# Append rows to existing dataframe
ASD_National_ADDM_Reshaped_All = rbind(ASD_National_ADDM_Reshaped_All, ASD_National_ADDM_Reshaped)
#ASD_National_ADDM_Reshaped_All

# 2000 2002 2004 2006 2008 2010 2012 2014
Process_Year = 2008

# Create the vectors:
Sex.Group  = c('Overall', 
               'Boys', 
               'Girls')
#Sex.Group

Prevalence = c(ASD_National_ADDM$Prevalence[ASD_National_ADDM$Year == Process_Year],
               ASD_National_ADDM$Male.Prevalence[ASD_National_ADDM$Year == Process_Year],
               ASD_National_ADDM$Female.Prevalence[ASD_National_ADDM$Year == Process_Year])
#Prevalence

# Combine all the vectors into a data frame:
ASD_National_ADDM_Reshaped = data.frame(Sex.Group, Prevalence, stringsAsFactors=T)

# Add new columns:
ASD_National_ADDM_Reshaped$Source = Process_Source
ASD_National_ADDM_Reshaped$Year = Process_Year
#
ASD_National_ADDM_Reshaped

# Append rows to existing dataframe
ASD_National_ADDM_Reshaped_All = rbind(ASD_National_ADDM_Reshaped_All, ASD_National_ADDM_Reshaped)
#ASD_National_ADDM_Reshaped_All

# 2000 2002 2004 2006 2008 2010 2012 2014
Process_Year = 2006

# Create the vectors:
Sex.Group  = c('Overall', 
               'Boys', 
               'Girls')
#Sex.Group

Prevalence = c(ASD_National_ADDM$Prevalence[ASD_National_ADDM$Year == Process_Year],
               ASD_National_ADDM$Male.Prevalence[ASD_National_ADDM$Year == Process_Year],
               ASD_National_ADDM$Female.Prevalence[ASD_National_ADDM$Year == Process_Year])
#Prevalence

# Combine all the vectors into a data frame:
ASD_National_ADDM_Reshaped = data.frame(Sex.Group, Prevalence, stringsAsFactors=T)

# Add new columns:
ASD_National_ADDM_Reshaped$Source = Process_Source
ASD_National_ADDM_Reshaped$Year = Process_Year
#
ASD_National_ADDM_Reshaped

# Append rows to existing dataframe
ASD_National_ADDM_Reshaped_All = rbind(ASD_National_ADDM_Reshaped_All, ASD_National_ADDM_Reshaped)
#ASD_National_ADDM_Reshaped_All

# 2000 2002 2004 2006 2008 2010 2012 2014
Process_Year = 2004

# Create the vectors:
Sex.Group  = c('Overall', 
               'Boys', 
               'Girls')
#Sex.Group

Prevalence = c(ASD_National_ADDM$Prevalence[ASD_National_ADDM$Year == Process_Year],
               ASD_National_ADDM$Male.Prevalence[ASD_National_ADDM$Year == Process_Year],
               ASD_National_ADDM$Female.Prevalence[ASD_National_ADDM$Year == Process_Year])
#Prevalence

# Combine all the vectors into a data frame:
ASD_National_ADDM_Reshaped = data.frame(Sex.Group, Prevalence, stringsAsFactors=T)

# Add new columns:
ASD_National_ADDM_Reshaped$Source = Process_Source
ASD_National_ADDM_Reshaped$Year = Process_Year
#
ASD_National_ADDM_Reshaped

# Append rows to existing dataframe
ASD_National_ADDM_Reshaped_All = rbind(ASD_National_ADDM_Reshaped_All, ASD_National_ADDM_Reshaped)
#ASD_National_ADDM_Reshaped_All

# 2000 2002 2004 2006 2008 2010 2012 2014
Process_Year = 2002

# Create the vectors:
Sex.Group  = c('Overall', 
               'Boys', 
               'Girls')
#Sex.Group

Prevalence = c(ASD_National_ADDM$Prevalence[ASD_National_ADDM$Year == Process_Year],
               ASD_National_ADDM$Male.Prevalence[ASD_National_ADDM$Year == Process_Year],
               ASD_National_ADDM$Female.Prevalence[ASD_National_ADDM$Year == Process_Year])
#Prevalence

# Combine all the vectors into a data frame:
ASD_National_ADDM_Reshaped = data.frame(Sex.Group, Prevalence, stringsAsFactors=T)

# Add new columns:
ASD_National_ADDM_Reshaped$Source = Process_Source
ASD_National_ADDM_Reshaped$Year = Process_Year
#
ASD_National_ADDM_Reshaped

# Append rows to existing dataframe
ASD_National_ADDM_Reshaped_All = rbind(ASD_National_ADDM_Reshaped_All, ASD_National_ADDM_Reshaped)
#ASD_National_ADDM_Reshaped_All

# 2000 2002 2004 2006 2008 2010 2012 2014
Process_Year = 2000

# Create the vectors:
Sex.Group  = c('Overall', 
               'Boys', 
               'Girls')
#Sex.Group

Prevalence = c(ASD_National_ADDM$Prevalence[ASD_National_ADDM$Year == Process_Year],
               ASD_National_ADDM$Male.Prevalence[ASD_National_ADDM$Year == Process_Year],
               ASD_National_ADDM$Female.Prevalence[ASD_National_ADDM$Year == Process_Year])
#Prevalence

# Combine all the vectors into a data frame:
ASD_National_ADDM_Reshaped = data.frame(Sex.Group, Prevalence, stringsAsFactors=T)

# Add new columns:
ASD_National_ADDM_Reshaped$Source = Process_Source
ASD_National_ADDM_Reshaped$Year = Process_Year
#
ASD_National_ADDM_Reshaped

# Append rows to existing dataframe
ASD_National_ADDM_Reshaped_All = rbind(ASD_National_ADDM_Reshaped_All, ASD_National_ADDM_Reshaped)
#ASD_National_ADDM_Reshaped_All

ASD_National_ADDM_Reshaped_All

# Adjust in-line plot size to M x N
options(repr.plot.width=8, repr.plot.height=6)

ggplot(ASD_National_ADDM_Reshaped_All, aes(Sex.Group, Prevalence)) +
  geom_col(aes(fill = Sex.Group, colours = )) + # Use column chart
  geom_text(aes(label = Prevalence), vjust = +0.5, hjust = -0.2, size = 2.5) +
  scale_y_continuous(name = "Prevalence per 1,000 Children",
                     breaks = seq(0, 30, 5),
                     limits=c(0, 30)) +
  scale_x_discrete(name = "") +
  scale_fill_manual("Sex Group:", values = c("Overall" = "purple", 
                                             "Boys" = "blue",
                                             "Girls" = "orange")) + 
  ggtitle("Prevalence Estimates by Sex [ Source: ADDM ] [ Year: ALL ]") +
  theme(title = element_text(face = 'bold.italic', color = "darkslategrey"), 
        axis.title = element_text(face = 'plain', color = "darkslategrey"),
        legend.position = 'none') + 
  coord_flip() + # Rotate chart
  facet_grid(facets = Year ~ .)


# Adjust in-line plot size to M x N
options(repr.plot.width=8, repr.plot.height=4)

# Create histogram using R graphics
hist(ASD_National$Prevalence, breaks = 10)

# Create histogram using ggplot2
ggplot(ASD_National, aes(x=Prevalence)) + 
  geom_histogram(binwidth = 5, fill = "blue", color = "lightgrey", alpha=0.5)

# Use color to differentiate sub-group data (Data Source)
ggplot(ASD_National, aes(x=Prevalence, fill = Source)) +
  geom_histogram(binwidth = 5) +
  theme_bw() + theme(legend.position="right") +
  scale_fill_manual("Data Source:", values = c("addm" = "darkblue", 
                                               "medi" = "orange", 
                                               "nsch" = "darkred",
                                               "sped" = "skyblue"))

# Plot sub-group data side by side, using position="dodge"
ggplot(ASD_National, aes(x=Prevalence, fill = Source)) +
  geom_histogram(binwidth = 5, position="dodge") +
  theme_bw() + theme(legend.position="right") +
  scale_fill_manual("Data Source:", values = c("addm" = "darkblue", 
                                               "medi" = "orange", 
                                               "nsch" = "darkred",
                                               "sped" = "skyblue"))

# Split plots using facet_grid()
ggplot(ASD_National, aes(x=Prevalence, fill = Source)) +
  geom_histogram(binwidth = 5) +
  theme(legend.position="right") + 
  scale_fill_manual("Data Source:", values = c("addm" = "darkblue", 
                                               "medi" = "orange", 
                                               "nsch" = "darkred",
                                               "sped" = "skyblue")) +
  facet_grid(facets = Source ~ .)

# Add title and caption using ggplot2
ggplot(ASD_National, aes(x=Prevalence, fill = Source)) +
  geom_histogram(binwidth = 5) +
  theme(legend.position="top") + 
  scale_fill_manual("Data Source:", values = c("addm" = "darkblue", 
                                               "medi" = "orange", 
                                               "nsch" = "darkred",
                                               "sped" = "skyblue")) + 
  labs(x="Prevalence per 1,000 Children",
       y="Frequency",
       title="Distribution of Prevalence by Data Source") +
  facet_grid(facets = Source ~ .)

# Adjust in-line plot size to M x N
# options(repr.plot.width=8, repr.plot.height=4)

# Create plot using R graphics
plot(density(ASD_National$Prevalence))
# Optionally, overlay histogram
hist(ASD_National$Prevalence, probability = TRUE, add = TRUE)

# Create plot using ggplot2
p <- ggplot(ASD_National) +
  geom_density(aes(x=Prevalence), fill = "grey", color = "white", alpha=0.75) 
p # Show

# Optionally, overlay histogram
p <- p + geom_histogram(aes(x = Prevalence, y = ..density..), binwidth = 1, fill = "blue", colour = "lightgrey", alpha=0.4) 
p # Show

# Optionally, overlay Prevalence mean
p <- p + geom_vline(aes(xintercept = mean(ASD_National$Prevalence)), colour="darkorange")
p # Show

# Lastly, add other captions
p <- p + coord_cartesian(xlim=c(0, 35), ylim=c(0, 0.2)) +
  labs(x="Prevalence per 1,000 Children", y="Density", 
       title=paste("Density of Prevalence ( mean =", mean(ASD_National$Prevalence), ")")) +
  theme(title = element_text(face = 'bold.italic', color = "darkslategrey"), 
        axis.title = element_text(face = 'plain', color = "darkslategrey"))
p # Show

# Prevelance distribution by Data Source
ggplot(ASD_National) + geom_density(aes(x = Prevalence, fill = Source), alpha = 0.5) + 
  scale_fill_manual("Data Source:", values = c("addm" = "darkblue", 
                                               "medi" = "orange", 
                                               "nsch" = "darkred",
                                               "sped" = "skyblue")) +
  labs(x="Prevalence per 1,000 Children",
       y="Density",
       title="Density of Prevalence by Data Source") +
  theme(title = element_text(face = 'bold.italic', color = "darkslategrey"), 
        axis.title = element_text(face = 'plain', color = "darkslategrey"))

# Prevelance distribution by Data Source with split
ggplot(ASD_National) + geom_density(aes(x = Prevalence, fill = Source), colour = 'lightgrey', alpha = 0.75) + 
  scale_fill_manual("Data Source:", values = c("addm" = "darkblue", 
                                               "medi" = "orange", 
                                               "nsch" = "darkred",
                                               "sped" = "skyblue")) + 
  labs(x="Prevalence per 1,000 Children",
       y="Density",
       title="Density of Prevalence by Data Source") +
  theme(title = element_text(face = 'bold.italic', color = "darkslategrey"), 
        axis.title = element_text(face = 'plain', color = "darkslategrey")) + 
  facet_wrap(~Source)

# Adjust in-line plot size to M x N
# options(repr.plot.width=8, repr.plot.height=4)

# Create plot using R graphics
# Create 'Prevalence' box plots break by 'Source'
boxplot(ASD_National$Prevalence ~ ASD_National$Source,
        main = "National ASD Prevalence by Data Source",
        xlab = "Data Source",
        ylab = "Prevalence per 1,000 Children",
        sub  = "Year 2000 - 2016",
        col.main="blue", col.lab="black", col.sub="darkgrey")

# Create box plot using ggplot2
ggplot(ASD_National, aes(x = Source, y = Prevalence, fill = Source)) + 
  geom_boxplot(alpha = 0.5) + 
  scale_y_continuous(name = "Prevalence per 1,000 Children",
                     breaks = seq(0, 30, 5),
                     limits=c(0, 30)) +
  scale_x_discrete(name = "Data Source (Year 2000 - 2016)") +
  ggtitle("National ASD Prevalence by Data Source") +
  theme(title = element_text(face = 'bold.italic', color = "darkslategrey"), 
        axis.title = element_text(face = 'plain', color = "darkslategrey"))


# Adjust in-line plot size to M x N
# options(repr.plot.width=8, repr.plot.height=4)

# ----------------------------------
# Build chart/plot layer by layer
# ----------------------------------

# Define a ggplot graphic object; provide data and x y for use
p <- ggplot(ASD_National, aes(x = Year, y = Prevalence))
# Show plot
p


# Select (add) line chart type:
p <- p + geom_line(aes(color = Source),
                   linetype = "solid",  # http://sape.inf.usi.ch/quick-reference/ggplot2/linetype
                   size=1,
                   alpha=0.5) 
# Show plot
p

# Select (add) points to chart:
p <- p + geom_point(aes(color = Source),
                    size=2, 
                    shape=20,
                    alpha=0.5) 
# Show plot
p

# Customize line color and legend name:
p <- p + scale_color_manual("Data Source:", 
                            labels = c('ADDM', 'MEDI', 'NSCH', 'SPED'),
                            values = c("addm" = "darkblue", 
                                       "medi" = "orange", 
                                       "nsch" = "darkred",
                                       "sped" = "skyblue"))
# Show plot
p

# Adjust x and y axis, scale, limit and labels:
p <- p + scale_y_continuous(name = "Prevalence per 1,000 Children",
                            breaks = seq(0, 30, 5),
                            limits=c(0, 30)) +
  scale_x_continuous(name = "Year", 
                     breaks = seq(2000, 2016, 1), 
                     limits = c(2000, 2016)) 
# Show plot
p

# Customise chart title:
p <- p + ggtitle("Prevalence Estimates Over Time [ Source: ALL ]") 
# Show plot
p

# Customise chart title and axis labels:
p <- p + theme(title = element_text(face = 'bold.italic', color = "darkslategrey"), 
               axis.title = element_text(face = 'plain', color = "darkslategrey")) 
# Show plot
p

# ----------------------------------
# Consolidate above code into one chunk
# ----------------------------------
p <- ggplot(ASD_National, aes(x = Year, y = Prevalence)) +
  geom_line(aes(color = Source),
            linetype = "solid",  # http://sape.inf.usi.ch/quick-reference/ggplot2/linetype
            size=1,
            alpha=0.5) +
  geom_point(aes(color = Source),
             size=2, 
             shape=20,
             alpha=0.5) + 
  scale_color_manual("Data Source:", 
                     labels = c('ADDM', 'MEDI', 'NSCH', 'SPED'),
                     values = c("addm" = "darkblue", 
                                "medi" = "orange", 
                                "nsch" = "darkred",
                                "sped" = "skyblue")) +
  scale_y_continuous(name = "Prevalence per 1,000 Children",
                     breaks = seq(0, 30, 5),
                     limits=c(0, 30)) +
  scale_x_continuous(name = "Year", 
                     breaks = seq(2000, 2016, 1), 
                     limits = c(2000, 2016)) +
  ggtitle("Prevalence Estimates Over Time [ Source: ALL ]") +
  theme(title = element_text(face = 'bold.italic', color = "darkslategrey"), 
        axis.title = element_text(face = 'plain', color = "darkslategrey"))
# Show plot
p

# Optionally, displaydata values/labels
p + geom_text(aes(label = round(Prevalence, 1)), # Values are rounded for display
              vjust = "outward", 
              #          nudge_y = 0.2, # optionally life the text
              hjust = "outward", 
              check_overlap = TRUE,
              size = 3, # size of textual data label
              col = 'darkslategrey')

if(!require(plotly)){install.packages("plotly")}
library(plotly)

p_dynamic <- p
p_dynamic <- ggplotly(p_dynamic)
p_dynamic

p_dynamic

if(!require(ggthemes)){install.packages("ggthemes")}
library(ggthemes)

# Theme of the economist magazine:
p + theme_economist() + scale_colour_economist()

# Theme of the Wall Street Journal:
p + theme_wsj() + scale_colour_wsj("colors6")

# Dynamic chart with theme of the economist magazine:
p_dynamic <- p + theme_economist() + scale_colour_economist()
p_dynamic <- ggplotly(p_dynamic)
p_dynamic

# Adjust in-line plot size to M x N
# options(repr.plot.width=8, repr.plot.height=4)

# Filter only data of ADDM
ASD_National_ADDM <- subset(ASD_National, Source == 'addm')

# ----------------------------------
# [addm] ADDM Network estimates for overall ASD prevalence in US over time
# ----------------------------------

#  Color:
# 'ADDM_Average' "purple"

p <- ggplot(ASD_National_ADDM, aes(x = Year, y = Prevalence)) +
  geom_point(aes(y = Prevalence, color = 'ADDM_Average'), # Name for manual colour mapping
             size=2, 
             shape=20,
             alpha=0.95) +
  # Add point for Upper.CI
  geom_point(aes(y = Upper.CI, color = 'ADDM_U_CI'), # Name for manual colour mapping
             size=0.1, 
             shape=20,
             alpha=0.95) +
  # Add point for Upper.CI
  geom_point(aes(y = Lower.CI, color = 'ADDM_L_CI'), # Name for manual colour mapping
             size=0.1, 
             shape=20,
             alpha=0.95) +
  scale_colour_manual(name="",
                      labels = c("US (ADDM)", "Upper CI", "Lower CI"), # Names shown in legend 
                      values = c(ADDM_Average="purple", ADDM_U_CI="red", ADDM_L_CI="red")) # Manual colour mapping
# Add title, axis label, and axis scale
p <- p + scale_y_continuous(name = "Prevalence per 1,000 Children",
                            breaks = seq(0, 18, 2),
                            limits=c(0, 18)) +
  scale_x_continuous(name = "Year", 
                     breaks = seq(2000, 2014, 2), 
                     limits = c(2000, 2014)) +
  ggtitle("ADDM Network estimates for overall ASD prevalence in US over time\nwith confidence interval") +
  theme(title = element_text(face = 'bold.italic', color = "darkslategrey"), 
        axis.title = element_text(face = 'plain', color = "darkslategrey"),
        panel.background = element_blank(), # Remove chart backgroun colour
        legend.position = 'top',
        panel.grid.major = element_line(size = 0.2, linetype = 'solid', colour = "lightgrey") # grid colour et al
       )
# Show plot
p

# Add smooth curve to go through date points, using interpolation with splines:
# https://stackoverflow.com/questions/35205795/plotting-smooth-line-through-all-data-points
spline_ADDM_Prevalence <- as.data.frame(spline(ASD_National_ADDM$Year, ASD_National_ADDM$Prevalence))
spline_ADDM_Prevalence_U_CI <- as.data.frame(spline(ASD_National_ADDM$Year, ASD_National_ADDM$Upper.CI))
spline_ADDM_Prevalence_L_CI <- as.data.frame(spline(ASD_National_ADDM$Year, ASD_National_ADDM$Lower.CI))
# Show plot
p + geom_line(data = spline_ADDM_Prevalence, aes(x = x, y = y, color = 'ADDM_Average'), linetype = "solid", size=0.6) + 
  geom_line(data = spline_ADDM_Prevalence_U_CI, aes(x = x, y = y, color = 'ADDM_U_CI'), linetype = 2, size=0.3) +
  geom_line(data = spline_ADDM_Prevalence_L_CI, aes(x = x, y = y, color = 'ADDM_L_CI'), linetype = 2, size=0.3)

# Adjust in-line plot size to M x N
# options(repr.plot.width=8, repr.plot.height=4)

# ----------------------------------
# [addm] < Prevalence Varies by Sex >
# ----------------------------------

#  Color:
# 'ADDM_Average' "darkslategrey"
# 'Female_Prevalence' "orange"
# 'Male_Prevalence' "blue"

p <- ggplot(ASD_National_ADDM, aes(x = Year, y = Prevalence)) +
  geom_line(aes(y = Prevalence, colour = 'ADDM_Average'),
            linetype = "solid",  # http://sape.inf.usi.ch/quick-reference/ggplot2/linetype
            size=1,
            alpha=0.5) +
  geom_point(aes(y = Prevalence, color = 'ADDM_Average'),
             size=2, 
             shape=20,
             alpha=0.5) +
  # Add line for Female
  geom_line(aes(y = Female.Prevalence, colour = 'Female_Prevalence'),
            linetype = "solid",  # http://sape.inf.usi.ch/quick-reference/ggplot2/linetype
            size=1,
            alpha=0.5) +
  geom_point(aes(y = Female.Prevalence, color = 'Female_Prevalence'),
             size=2, 
             shape=20,
             alpha=0.5) +
  # Add line for Male
  geom_line(aes(y = Male.Prevalence, colour = 'Male_Prevalence'),
            linetype = "solid",  # http://sape.inf.usi.ch/quick-reference/ggplot2/linetype
            size=1,
            alpha=0.5) +
  geom_point(aes(y = Male.Prevalence, color = 'Male_Prevalence'),
             size=2, 
             shape=20,
             alpha=0.5) +
  scale_colour_manual(name="",
                      labels = c("ADDM Average", "Female Prevalence", "Male Prevalence"),
                      values = c(ADDM_Average="darkslategrey", Female_Prevalence="orange", Male_Prevalence="blue"))
# Add title, axis label, and axis scale
p <- p + scale_y_continuous(name = "Prevalence per 1,000 Children",
                            breaks = seq(0, 30, 5),
                            limits=c(0, 30)) +
  scale_x_continuous(name = "Year", 
                     breaks = seq(2000, 2016, 1), 
                     limits = c(2000, 2016)) +
  ggtitle("Prevalence Estimates by Sex [ Source: ADDM ]") +
  theme(title = element_text(face = 'bold.italic', color = "darkslategrey"), 
        axis.title = element_text(face = 'plain', color = "darkslategrey")) 
# Show plot
p

# Apply theme
p + theme_economist() + scale_colour_economist() # p + theme_wsj() + scale_colour_wsj("colors6")

# Dynamic chart:
p_dynamic <- p + theme_economist() + scale_colour_economist()
p_dynamic <- ggplotly(p_dynamic)
p_dynamic

# Write your code below and press Shift+Enter to execute 


# Adjust in-line plot size to M x N
# options(repr.plot.width=8, repr.plot.height=4)

# ----------------------------------
# [addm] < Prevalence Varies by Race and Ethnicity >
# ----------------------------------

#  Color:
# 'ADDM_Average' "darkslategrey"
# 'Asian_Pacific_Islander' "darkred"
# 'Hispanic' "darkorchid3"
# 'Non_Hispanic_Black' "deepskyblue3"
# 'Non_Hispanic_White' "chartreuse3"

p <- ggplot(ASD_National_ADDM, aes(x = Year, y = Prevalence)) +
  geom_line(aes(y = Prevalence, colour = 'ADDM_Average'),
            linetype = "dotted",  # http://sape.inf.usi.ch/quick-reference/ggplot2/linetype
            size=1,
            alpha=0.5) +
  geom_point(aes(y = Prevalence, color = 'ADDM_Average'),
             size=2, 
             shape=20,
             alpha=0) +
  # Add line for Asian.or.Pacific.Islander.Prevalence
  geom_line(aes(y = Asian.or.Pacific.Islander.Prevalence, colour = 'Asian_Pacific_Islander'),
            linetype = "solid",  # http://sape.inf.usi.ch/quick-reference/ggplot2/linetype
            size=1,
            alpha=0.5) +
  geom_point(aes(y = Asian.or.Pacific.Islander.Prevalence, colour = 'Asian_Pacific_Islander'),
             size=2, 
             shape=20,
             alpha=0.5) +
  # Add line for Hispanic.Prevalence
  geom_line(aes(y = Hispanic.Prevalence, colour = 'Hispanic'),
            linetype = "solid",  # http://sape.inf.usi.ch/quick-reference/ggplot2/linetype
            size=1,
            alpha=0.5) +
  geom_point(aes(y = Hispanic.Prevalence, colour = 'Hispanic'),
             size=2, 
             shape=20,
             alpha=0.5) +
  # Add line for Non.hispanic.black.Prevalence
  geom_line(aes(y = Non.hispanic.black.Prevalence, colour = 'Non_Hispanic_Black'),
            linetype = "solid",  # http://sape.inf.usi.ch/quick-reference/ggplot2/linetype
            size=1,
            alpha=0.5) +
  geom_point(aes(y = Non.hispanic.black.Prevalence, colour = 'Non_Hispanic_Black'),
             size=2, 
             shape=20,
             alpha=0.5) +
  # Add line for Non.hispanic.white.Prevalence
  geom_line(aes(y = Non.hispanic.white.Prevalence, colour = 'Non_Hispanic_White'),
            linetype = "solid",  # http://sape.inf.usi.ch/quick-reference/ggplot2/linetype
            size=1,
            alpha=0.5) +
  geom_point(aes(y = Non.hispanic.white.Prevalence, colour = 'Non_Hispanic_White'),
             size=2, 
             shape=20,
             alpha=0.5) +
  scale_colour_manual(name="",
                      labels = c("ADDM Average", 
                                 "Asian/Pacific Islander", 
                                 "Hispanic", 
                                 "Non-Hispanic Black", 
                                 "Non-Hispanic White"),
                      values = c(ADDM_Average="darkslategrey", 
                                 Asian_Pacific_Islander ="darkred", 
                                 Hispanic ="darkorchid3", 
                                 Non_Hispanic_Black ="deepskyblue3", 
                                 Non_Hispanic_White ="chartreuse3"))
# Add title, axis label, and axis scale
p <- p + scale_y_continuous(name = "Prevalence per 1,000 Children",
                            breaks = seq(5, 20, 5),
                            limits=c(5, 20)) +
  scale_x_continuous(name = "Year", 
                     breaks = seq(2000, 2016, 1), 
                     limits = c(2000, 2016)) +
  ggtitle("Prevalence Estimates by Race/Ethnicity [ Source: ADDM ]") +
  theme(title = element_text(face = 'bold.italic', color = "darkslategrey"), 
        axis.title = element_text(face = 'plain', color = "darkslategrey")) 
# Show plot
p

# Apply theme
# p + theme_economist() + scale_colour_economist() # p + theme_wsj() + scale_colour_wsj("colors6")

# Dynamic chart:
p_dynamic <- p + theme_economist() + scale_colour_economist()
p_dynamic <- ggplotly(p_dynamic)
p_dynamic

# Write your code below and press Shift+Enter to execute 


# ----------------------------------
# Dataset: US. State Level Children ASD Prevalence
# ----------------------------------

ASD_State    <- read.csv("../dataset/ADV_ASD_State.csv", stringsAsFactors = FALSE)

# Obtain number of rows and number of columns/features/variables
dim(ASD_State)
# Obtain overview (data structure/types)
str(ASD_State)

# Count missing values in dataframe:
sum(is.na(ASD_State)) # No missing data recognised by R (NA)
# Define several offending strings
na_strings <- c("", "No data", "NA", "N A", "N / A", "N/A", "N/ A", "Not Available", "NOt available")
# Replace these defined missing values to R's internal NA
ASD_State = replace_with_na_all(ASD_State, condition = ~.x %in% na_strings)
# Count missing values in dataframe:
sum(is.na(ASD_State))

# Remove invalid unicode char/string: \x92
ASD_State$Source_Full1[ASD_State$Source_Full1 == "National Survey of Children\x92s Health"] <- "National Survey of Children's Health"

cbind(names(ASD_State), c(1:length(names(ASD_State))))

# Delete/Drop variable by index: column from 14 to 26, 29, and 30
# names(ASD_State)
ASD_State <- ASD_State[ -c(14:26, 29, 30) ]

# Create one new variable: Source_UC as uppercase of Source
ASD_State$Source_UC <- paste(toupper(ASD_State$Source))
# Create one new variable: Source_Full3 by combining Source_UC and Source_Full1
ASD_State$Source_Full3 <- paste(ASD_State$Source_UC, ASD_State$Source_Full1)

str(ASD_State)

# cbind(names(ASD_State), c(1:length(names(ASD_State))))

# Convert Prevalence and CIs from categorical/chr to numeric
ix <- 13:33 # define an index
ASD_State[ix] <- lapply(ASD_State[ix], as.numeric)

# Convert Source from categorical/chr to categorical/factor
ix <- c(1, 7, 8, 9, 10, 34, 35) # define an index
ASD_State[ix] <- lapply(ASD_State[ix], as.factor)

# Create new ordered factor Year_Factor from Year
ASD_State$Year_Factor <- factor(ASD_State$Year, ordered = TRUE)

# Display unique values (levels) of a factor categrotical 
lapply(select_if(ASD_State, is.factor), levels)

write.csv(ASD_State, file = "../dataset/ADV_ASD_State_R.csv", row.names = FALSE)

# Read back in above saved file:
# ASD_State <- read.csv("../dataset/ADV_ASD_State_R.csv")
# ASD_State$Year_Factor <- factor(ASD_State$Year_Factor, ordered = TRUE) # Convert Year_Factor to ordered.factor

# Adjust in-line plot size to M x N
options(repr.plot.width=8, repr.plot.height=12)

# ----------------------------------
# [State] < Years Data Available by State >
# ----------------------------------
p <- ggplot(ASD_State, aes(x = Source, fill = Source)) + 
  geom_bar() + theme(axis.text.x=element_blank(),  # Hide axis
                     axis.ticks.x=element_blank(), # Hide axis
                     axis.text.y=element_blank(),  # Hide axis
                     axis.ticks.y=element_blank(), # Hide axis
                     panel.background = element_blank(), # Remove panel background
                     legend.position="top",
                     strip.text.y = element_text(angle=0) # Rotate text to horizontal
  ) + 
  scale_fill_manual("Data Source:", values = c("addm" = "darkblue", 
                                               "medi" = "orange", 
                                               "nsch" = "darkred",
                                               "sped" = "skyblue")) +
  facet_grid(facets = State_Full2 ~ Year) +
  labs(x="", y="", title="Years Data Available by State") # layers of graphics

# Below plot may run for a while
# Show plot
p

# Filter and create dataframe of different data sources, for easy data access
ASD_State_ADDM <- subset(ASD_State, Source == 'addm')
ASD_State_MEDI <- subset(ASD_State, Source == 'medi')
ASD_State_NSCH <- subset(ASD_State, Source == 'nsch')
ASD_State_SPED <- subset(ASD_State, Source == 'sped')

# Adjust in-line plot size to M x N
options(repr.plot.width=8, repr.plot.height=6)

# Years Data Available by State [ Source: ADDM ]
p <- ggplot(ASD_State_ADDM, aes(x = 1, fill = State_Full2)) + 
  geom_bar() + theme(axis.text.x=element_blank(),  # Hide axis
                     axis.ticks.x=element_blank(), # Hide axis
                     axis.text.y=element_blank(),  # Hide axis
                     axis.ticks.y=element_blank(), # Hide axis
                     panel.background = element_blank(), # Remove panel background
                     legend.position="none",
                     strip.text.y = element_text(angle=0) # Rotate text to horizontal
  ) +
  facet_grid(facets = State_Full2 ~ Year_Factor) +
  labs(x="", y="", title="Years Data Available by State [ Source: ADDM ]") # layers of graphics

# Show plot
p

# Write your code below and press Shift+Enter to execute 


# Adjust in-line plot size to M x N
options(repr.plot.width=8, repr.plot.height=4)

# Prevalence Estimates by State [ Source: ADDM ] , aggregated for different years
p <- ggplot(ASD_State_ADDM, aes(x = reorder(State_Full2, Prevalence, FUN = median), # Order States by median of Prevalence  
                                y = Prevalence)) + 
  geom_boxplot(aes(fill = reorder(State_Full2, Prevalence, FUN = median))) + # fill color by State
  scale_fill_discrete(guide = guide_legend(title = "US. States")) + # Legend Name
  #  geom_boxplot(fill = 'darkslategrey', alpha = 0.2) + 
  scale_y_continuous(name = "Prevalence per 1,000 Children",
                     breaks = seq(0, 30, 5),
                     limits=c(0, 30)) +
  scale_x_discrete(name = "") +
  ggtitle("Prevalence Estimates by State [ Source: ADDM ]") +
  theme(title = element_text(face = 'bold.italic', color = "darkslategrey"), 
        axis.title = element_text(face = 'plain', color = "darkslategrey"),
        legend.position = 'none') + 
  coord_flip() + # Rotate chart
  geom_jitter(position=position_jitter(0.1)) # Add actual data points

# Show plot
p

# Theme of the economist magazine:
# p + theme_economist() + scale_colour_economist() + theme(legend.position = 'none')

# Dynamic chart
p_dynamic <- p + theme_economist() + scale_colour_economist() + theme(legend.position = 'none')
p_dynamic <- ggplotly(p_dynamic)
p_dynamic

# Write your code below and press Shift+Enter to execute 


# Adjust in-line plot size to M x N
options(repr.plot.width=8, repr.plot.height=4)

# All State Prevalence data with: Source == 'addm' & Year == 2014
# filter using dataframe: ASD_State_ADDM
ASD_State_Subset <- subset(ASD_State_ADDM, Year == 2014)
# or filer using dataframe: ASD_State
ASD_State_Subset <- subset(ASD_State, Source == 'addm' & Year == 2014)

# Bar plot/chart for < No. Children surveyed by State [ADDM] [Year 2014] >
p <- ggplot(ASD_State_Subset, aes(x = reorder(State_Full1, Denominator, FUN = median), # Order States by median of Denominator  
                                  y = Denominator)) + 
  geom_bar(stat="identity", aes(fill = reorder(State_Full1, Denominator, FUN = median))) + # fill color by State
  scale_fill_discrete(guide = guide_legend(title = "US. States")) + # Legend Name
  scale_x_discrete(name = "US. States") +
  scale_y_continuous(name = "No. Children (Denominator)") +
  ggtitle("No. Children Surveyed by State [ Source: ADDM ] [Year 2014]") +
  #  geom_text(aes(label=Denominator), vjust=1.6, color="darkslategrey", size=3.5) + # Show data label inside bars
  theme(title = element_text(face = 'bold.italic', color = "darkslategrey"), 
        axis.title = element_text(face = 'plain', color = "darkslategrey"),
        legend.position="none") 

# Show plot
p

# Theme of the economist magazine:
# p + theme_economist() + scale_colour_economist() + theme(legend.position = 'none')

# Dynamic chart
p_dynamic <- p + theme_economist() + scale_colour_economist() + theme(legend.position = 'none')
p_dynamic <- ggplotly(p_dynamic)
p_dynamic

# Write your code below and press Shift+Enter to execute 


# Write your code below and press Shift+Enter to execute 


# Adjust in-line plot size to M x N
# options(repr.plot.width=8, repr.plot.height=4)

# ASD_State_Subset <- subset(ASD_State_ADDM, Year == 2014)
# or
# ASD_State_Subset <- subset(ASD_State, Source == 'addm' & Year == 2014)

# Point plot/chart 
p = ggplot(ASD_State_Subset, aes(x = reorder(State_Full1, Prevalence, median), # Order States by median of Prevalence  
                                 y = Prevalence)) + 
  geom_point(stat="identity", aes(colour = reorder(State_Full1, Prevalence, median)), size = 10, alpha = 0.1, pch = 15) + # fill color by State
  scale_colour_discrete(guide = guide_legend(title = "US. States")) + # Legend Name
  scale_y_continuous(name = "Prevalence per 1,000 Children",
                     breaks = seq(10, 35, 5),
                     limits=c(10, 35)) +
  scale_x_discrete(name = "US. States") +
  ggtitle("Prevalence Estimates with 95% CI by State [ Source: ADDM ] [ Year 2014 ]") +
  theme(title = element_text(face = 'bold.italic', color = "darkslategrey"), 
        axis.title = element_text(face = 'plain', color = "darkslategrey"),
        legend.position = 'none') +
  geom_text(aes(label=Prevalence), hjust=0.5, color="black", size=3.5)  # Show data label inside bars

# Show plot
p

# Add Lower.CI
p = p + geom_point(data = ASD_State_Subset, aes(x = reorder(State_Full1, Prevalence, median), y = Lower.CI,
                                                shape=Source # point shape
), 
size = 2 # point size
) +
  #  geom_text(aes(label=Lower.CI), hjust=-0.1, vjust=3, color="darkslategrey", size=2.5) + # Show data label inside bars 
  scale_shape_manual(values=3)  # manual define point shape
# Show plot
p

# Add Upper.CI
p = p + geom_point(data = ASD_State_Subset, aes(x = reorder(State_Full1, Prevalence, median), y = Upper.CI, 
                                                shape=Source # point shape
), 
size = 2 # point size
) 
#  geom_text(aes(label=Upper.CI), hjust=-0.1, vjust=-3, color="darkslategrey", size=2.5) # Show data label inside bars 
# Show plot
p

# theme of the economist magazine:
# p + theme_economist() + scale_colour_economist() + scale_colour_discrete(guide = guide_legend(title = "US. States")) + theme(legend.position = 'none')

# Dynamic chart
p_dynamic <- p + theme_economist() + scale_colour_economist() + scale_colour_discrete(guide = guide_legend(title = "US. States")) + theme(legend.position = 'none')
p_dynamic <- ggplotly(p_dynamic)
p_dynamic

# Write your code below and press Shift+Enter to execute 


# Adjust in-line plot size to M x N
# options(repr.plot.width=8, repr.plot.height=4)

# All year/time Prevalence data with: Source_UC == 'ADDM' & State_Full2 == 'AZ-Arizona'
ASD_State_Subset <- subset(ASD_State, Source_UC == 'ADDM' & State_Full2 == 'AZ-Arizona')

# Line plot/chart for < State ASD Prevalence [ADDM] [AZ-Arizona] >
p <- ggplot(ASD_State_Subset, aes(x = Year, y = Prevalence))
# Select (add) line chart type:
p <- p + geom_line(aes(color = State_Full2),
                   linetype = "solid",  # http://sape.inf.usi.ch/quick-reference/ggplot2/linetype
                   size=1,
                   alpha=0.5) 
# Select (add) points to chart:
p <- p + geom_point(aes(color = State_Full2),
                    size=3, 
                    shape=20,
                    alpha=0.5) 
# Customize legend name:
p <- p + labs(color = "US. State")
# Adjust x and y axis, scale, limit and labels:
p <- p + scale_y_continuous(name = "Prevalence per 1,000 Children",
                            breaks = seq(0, 30, 5),
                            limits=c(0, 30)) +
  scale_x_continuous(name = "Year", 
                     breaks = seq(2000, 2016, 1), 
                     limits = c(2000, 2016)) 
# Customize chart title:
p <- p + ggtitle("Prevalence Estimates over Year [ Source: ADDM ] [ State: AZ-Arizona ]") 
# Customize chart title and axis labels:
p <- p + theme(title = element_text(face = 'bold.italic', color = "darkslategrey"), 
               axis.title = element_text(face = 'plain', color = "darkslategrey")) 

# Show plot
p

# Theme of the economist magazine:
p + theme_economist() + scale_colour_economist()

# Adjust in-line plot size to M x N
# options(repr.plot.width=8, repr.plot.height=4)

p <- ggplot(ASD_State_ADDM, aes(x = Year, y = Prevalence))
# Select (add) line chart type:
p <- p + geom_line(aes(color = State_Full2),
                   linetype = "solid",  # http://sape.inf.usi.ch/quick-reference/ggplot2/linetype
                   size=1,
                   alpha=0.5) 
# Select (add) points to chart:
p <- p + geom_point(aes(color = State_Full2),
                    size=3, 
                    shape=20,
                    alpha=0.5) 
# Show plot
# p
# Customize line color and legend name:
p <- p + labs(color = "US. State")
# Adjust x and y axis, scale, limit and labels:
p <- p + scale_y_continuous(name = "Prevalence per 1,000 Children",
                            breaks = seq(0, 30, 5),
                            limits=c(0, 30)) +
  scale_x_continuous(name = "Year (2000 - 2016)", 
                     breaks = seq(2000, 2016, 1), 
                     limits = c(2000, 2016)) 
# Customize chart title:
p <- p + ggtitle("Prevalence Estimates over Year [ Source: ADDM ] [ State: ALL ]") 
# Customize chart title and axis labels:
p <- p + theme(title = element_text(face = 'bold.italic', color = "darkslategrey"), 
               axis.title = element_text(face = 'plain', color = "darkslategrey"),
               legend.position="right")


# Show plot
p

# Dynamic chart
p_dynamic <- p + theme_economist() + scale_colour_economist() + scale_colour_discrete(guide = guide_legend(title = "US. States"))
p_dynamic <- ggplotly(p_dynamic)
p_dynamic

# Show plot in facet_grid
p + facet_grid(facets = . ~ State) + 
  theme(legend.position = "none", # Hide legend
        axis.text.x=element_blank(),  # Hide axis
        axis.ticks.x=element_blank(), # Hide axis
        panel.background = element_blank(), # Remove panel background
        panel.grid.major = element_line(size = 0.1, linetype = 1, colour = "lightgrey")
  ) 

# ----------------------------------
# EDA - Visualisation on map
# ----------------------------------
if(!require(usmap)){install.packages("usmap")}
library(usmap) # usmap: Mapping the US

# Adjust in-line plot size to M x N
# options(repr.plot.width=8, repr.plot.height=4)

# Prepare data - addm 2014
Map_Data_Source = 'addm' # Available values lowercase: 'addm', 'medi', 'nsch', 'sped'.
Map_Data_Value = 'Prevalence' # variable must be numeric, variable name in 'quotation'. Or else Error: Discrete value supplied to continuous scale

# Uncomment below to use Prevalence of different groups:
# Map_Data_Value = 'Male.Prevalence' # variable must be numeric, variable name in 'quotation'. Or else Error: Discrete value supplied to continuous scale
# Map_Data_Value = 'Female.Prevalence' # variable must be numeric, variable name in 'quotation'. Or else Error: Discrete value supplied to continuous scale
# Map_Data_Value = 'Asian.or.Pacific.Islander.Prevalence' # variable must be numeric, variable name in 'quotation'. Or else Error: Discrete value supplied to continuous scale

Map_Data_Year = 2014 # must be integer
ASD_State_Subset = subset(ASD_State, Source == Map_Data_Source & Year == Map_Data_Year)

# The usmap package/function requires input data to have a column of 'state', or 'fips'. (case sensitive)
ASD_State_Subset$state = ASD_State_Subset$State 
# Glance
head(ASD_State_Subset)

# Show data on map
p_map_addm_2014 <- plot_usmap(data = ASD_State_Subset, values = Map_Data_Value, 
                              color = "white", # map line colour
                              labels = TRUE,  # State name shown
                              label_color = 'white' # State name colour
) + 
  scale_fill_continuous(
    na.value = "lightgrey", # Set colour with no State data
    low="lightblue1", high = "darkblue", 
    name = "Prevalence\nper 1,000\nChildren", 
    limits=c(0, 40) #same colour levels/limits for plots
  ) +
  labs(title = paste("Prevalence Estimates by Geographic Area", '\n[ Measure :', Map_Data_Value, "] [ Source :", Map_Data_Source, "] [ Year :", Map_Data_Year, "]"),
       subtitle = 'https://www.cdc.gov/ncbddd/autism'
  ) + 
  theme(panel.background = element_rect(color = "white", fill = "white"),
        legend.position = "right")
# Show map
p_map_addm_2014

# Dynamic map
p_dynamic <- p_map_addm_2014
p_dynamic <- ggplotly(p_dynamic)
p_dynamic

Map_Data_Source = 'nsch' # Available values lowercase: 'addm', 'medi', 'nsch', 'sped'.
Map_Data_Value = 'Prevalence' # variable must be numeric, variable name in 'quotation'. Or else Error: Discrete value supplied to continuous scale

# Prepare data - nsch 2004
Map_Data_Year = 2004 # must be integer
ASD_State_Subset = subset(ASD_State, Source == Map_Data_Source & Year == Map_Data_Year)
ASD_State_Subset$state = ASD_State_Subset$State
# Plot on map
p_map_nsch_2004 <- plot_usmap(data = ASD_State_Subset, values = Map_Data_Value, color = "white", labels = F, label_color = 'white' ) + scale_fill_continuous(na.value = "lightgrey", low="lightblue1", high = "darkblue", name = "Prevalence\nper 1,000\nChildren", limits=c(0, 40) ) + labs(title = paste("Prevalence Estimates by Geographic Area", '\n[ Measure :', Map_Data_Value, "] [ Source :", Map_Data_Source, "] [ Year :", Map_Data_Year, "]"), subtitle = 'https://www.cdc.gov/ncbddd/autism' ) + theme(panel.background = element_rect(color = "white", fill = "white"), legend.position = "right")
p_map_nsch_2004

# Prepare data - nsch 2008
Map_Data_Year = 2008 # must be integer
ASD_State_Subset = subset(ASD_State, Source == Map_Data_Source & Year == Map_Data_Year)
ASD_State_Subset$state = ASD_State_Subset$State
p_map_nsch_2008 <- plot_usmap(data = ASD_State_Subset, values = Map_Data_Value, color = "white", labels = F, label_color = 'white' ) + scale_fill_continuous(na.value = "lightgrey", low="lightblue1", high = "darkblue", name = "Prevalence\nper 1,000\nChildren", limits=c(0, 40) ) + labs(title = paste("Prevalence Estimates by Geographic Area", '\n[ Measure :', Map_Data_Value, "] [ Source :", Map_Data_Source, "] [ Year :", Map_Data_Year, "]"), subtitle = 'https://www.cdc.gov/ncbddd/autism' ) + theme(panel.background = element_rect(color = "white", fill = "white"), legend.position = "right")
p_map_nsch_2008

# Prepare data - nsch 2012
Map_Data_Year = 2012 # must be integer
ASD_State_Subset = subset(ASD_State, Source == Map_Data_Source & Year == Map_Data_Year)
ASD_State_Subset$state = ASD_State_Subset$State
p_map_nsch_2012 <- plot_usmap(data = ASD_State_Subset, values = Map_Data_Value, color = "white", labels = F, label_color = 'white' ) + scale_fill_continuous(na.value = "lightgrey", low="lightblue1", high = "darkblue", name = "Prevalence\nper 1,000\nChildren", limits=c(0, 40) ) + labs(title = paste("Prevalence Estimates by Geographic Area", '\n[ Measure :', Map_Data_Value, "] [ Source :", Map_Data_Source, "] [ Year :", Map_Data_Year, "]"), subtitle = 'https://www.cdc.gov/ncbddd/autism' ) + theme(panel.background = element_rect(color = "white", fill = "white"), legend.position = "right")
p_map_nsch_2012

# Prepare data - nsch 2016
Map_Data_Year = 2016 # must be integer
ASD_State_Subset = subset(ASD_State, Source == Map_Data_Source & Year == Map_Data_Year)
ASD_State_Subset$state = ASD_State_Subset$State
p_map_nsch_2016 <- plot_usmap(data = ASD_State_Subset, values = Map_Data_Value, color = "white", labels = F, label_color = 'white' ) + scale_fill_continuous(na.value = "lightgrey", low="lightblue1", high = "darkblue", name = "Prevalence\nper 1,000\nChildren", limits=c(0, 40) ) + labs(title = paste("Prevalence Estimates by Geographic Area", '\n[ Measure :', Map_Data_Value, "] [ Source :", Map_Data_Source, "] [ Year :", Map_Data_Year, "]"), subtitle = 'https://www.cdc.gov/ncbddd/autism' ) + theme(panel.background = element_rect(color = "white", fill = "white"), legend.position = "right")
p_map_nsch_2016

# Dynamic map
p_dynamic <- p_map_nsch_2016 # [ Source: NSCH ] [ Year: 2016 ]
p_dynamic <- ggplotly(p_dynamic)
p_dynamic

# Adjust in-line plot size to M x N
options(repr.plot.width=8, repr.plot.height=6)

# ----------------------------------
# Combine multiple plots 
# ----------------------------------
if(!require(cowplot)){install.packages("cowplot")}
library('cowplot')
cowplot::plot_grid(
  p_map_nsch_2004,
  p_map_nsch_2008,
  p_map_nsch_2012,
  p_map_nsch_2016,
  nrow = 2)

# ----------------------------------
# Export current plot as image file
# ----------------------------------
ggsave("plot Map Prevalence Estimates by Geographic Area [NSCH] [2004-2016].png", 
       width = 60, height = 30, units = 'cm')

# Adjust in-line plot size to M x N
# options(repr.plot.width=8, repr.plot.height=4)

# ----------------------------------
# Create a *Population* of US. State level ASD Prevalence from Source SPED in Year 2016 
# ----------------------------------
ASD_State_SPED_2016 <- subset(ASD_State, Source == 'sped' & Year == 2016, select=c('State', 'Prevalence'))
#
head(ASD_State_SPED_2016)

dim(ASD_State_SPED_2016)
# *Population* mean Prevalence
mean(ASD_State_SPED_2016$Prevalence)

# Use sd()   to calculate *sample*     std-dev (S)
# Use sd.p() to calculate *population* std-dev (Omega)

# Define a function sd.p() to calculate *population* std-dev (Omega)
# https://www.dummies.com/education/math/statistics/standard-deviation-r/

sd.p = function(x) {sd(x) * sqrt((length(x)-1)/length(x))}

# Treat as sample:
cat('sd()   of ASD_State_SPED_2016$Prevalence : ', sd(ASD_State_SPED_2016$Prevalence) )

# Treat as population:
cat('\nsd.p() of ASD_State_SPED_2016$Prevalence : ', sd.p(ASD_State_SPED_2016$Prevalence) )

# Create a *Sample* from ASD_State_SPED_2016$Prevalence,
# with sample size n =
clt_n = 20
# clt_n = 40

set.seed(88)
clt_sample_1 = sample(x = ASD_State_SPED_2016$Prevalence, size = clt_n, replace = TRUE)
clt_sample_1

# Adjust in-line plot size to M x N
options(repr.plot.width=8, repr.plot.height=4)

plot(density(clt_sample_1), col="darkgrey", lwd=2) 
hist(clt_sample_1, probability = T, add = T)

# Repeatedly sample for k times, create a matrix/array to store these samples
clt_k = 10000 # or called 'N', but this can be confusing due to N can also be population size. Thus we use 'k' here.

set.seed(88) # Repeatable sampling using pseudo random methold
clt_sample_k <- (replicate(clt_k, sample(x = ASD_State_SPED_2016$Prevalence, size = clt_n, replace = TRUE)))

# first few samples
clt_sample_k[, 1:6]

# last sample
clt_sample_k[, clt_k]

# mean values of first few samples
mean(clt_sample_k[, 1])
mean(clt_sample_k[, 2])
mean(clt_sample_k[, 3])
mean(clt_sample_k[, 4])
mean(clt_sample_k[, 5])
mean(clt_sample_k[, 6])

# or use apply() function to loop
apply(clt_sample_k[, 1:6], 2, mean)

# std-dev values of first few samples
sd(clt_sample_k[, 1])
sd(clt_sample_k[, 2])
sd(clt_sample_k[, 3])
sd(clt_sample_k[, 4])
sd(clt_sample_k[, 5])
sd(clt_sample_k[, 6])

# or use apply() function to loop
apply(clt_sample_k[, 1:6], 2, sd)

# ----------------------------------
# k sample's distributions (k many)
# ----------------------------------
# Show the first few sample's histogram
par(mfrow=c(2, 3))
apply(clt_sample_k[, 1:6], 2, FUN=hist)
# Reset
par(mfrow=c(1, 1))

# Show the first few sample's density, together with Population
# Population (Prevalence) histogram in probablity
hist(ASD_State_SPED_2016$Prevalence, probability = T, 
     col=rgb(0.75,0.75,0.75,0.5), breaks = 50,
     xlab = 'Prevalvence', xlim = (c(0, 25)),
     ylab = 'Probability Density', 
     ylim = (c(0, 0.6)),
     main = 'Visualize Population & Samples')

# Overlay curve:
# Population (Prevalence) density
lines(density(ASD_State_SPED_2016$Prevalence), col="grey4", lwd=2) 

# Overlay line:
# mean = mean of Population (Prevalence)
abline(v=mean(ASD_State_SPED_2016$Prevalence), col="grey4", lwd=2) 

# Show the first few sample's density, together with Population
# Population (Prevalence) histogram in probablity
hist(ASD_State_SPED_2016$Prevalence, probability = T, 
     col=rgb(0.75,0.75,0.75,0.5), breaks = 50,
     xlab = 'Prevalvence', xlim = (c(0, 25)),
     ylab = 'Probability Density', 
     ylim = (c(0, 0.6)),
     main = 'Visualize Population & Samples')

# Overlay curve:
# Population (Prevalence) density
lines(density(ASD_State_SPED_2016$Prevalence), col="grey4", lwd=2) 

# Overlay line:
# mean = mean of Population (Prevalence)
abline(v=mean(ASD_State_SPED_2016$Prevalence), col="grey4", lwd=2) 

# Overlay:
# First few sample's density & mean
lines(density(clt_sample_k[, 1]), col="blue", lwd=1) 
abline(v=mean(clt_sample_k[, 1]), col="blue", lwd=1) 

lines(density(clt_sample_k[, 2]), col="blue", lwd=1) 
abline(v=mean(clt_sample_k[, 2]), col="blue", lwd=1) 

lines(density(clt_sample_k[, 3]), col="blue", lwd=1) 
abline(v=mean(clt_sample_k[, 3]), col="blue", lwd=1) 

lines(density(clt_sample_k[, 4]), col="blue", lwd=1) 
abline(v=mean(clt_sample_k[, 4]), col="blue", lwd=1) 

lines(density(clt_sample_k[, 5]), col="blue", lwd=1) 
abline(v=mean(clt_sample_k[, 5]), col="blue", lwd=1) 

lines(density(clt_sample_k[, 6]), col="blue", lwd=1) 
abline(v=mean(clt_sample_k[, 6]), col="blue", lwd=1) 

lines(density(clt_sample_k[, clt_k]), col="blue", lwd=1) 
abline(v=mean(clt_sample_k[, clt_k]), col="blue", lwd=1) 

# We can see that sample's distributions are all different.

# ----------------------------------
# Sampling distribution (only one)
# ----------------------------------
# Calculate sample mean value for k samples
clt_sample_k_mean <- apply(clt_sample_k, 2, mean)
# Show first few sample means
clt_sample_k_mean[1:6]

# Calculate sample std-dev value for each individual sample (totally k std-dev)
clt_sample_k_sd <- apply(clt_sample_k, 2, sd)
# Show first few samples' std-dev
clt_sample_k_sd[1:6]

# Calculate std-dev value for Sampling DIsdtribution (only one std-dev)
sd(clt_sample_k_mean)

# histogram of sample means (Sampling distribution of the mean)
hist(clt_sample_k_mean, probability = T, breaks = 100)
lines(density(clt_sample_k_mean), col="cyan2", lwd=2) 

# histogram of sample std-dev
hist(clt_sample_k_sd, probability = T, breaks = 100)
lines(density(clt_sample_k_sd), col="orange2", lwd=2) 

# k *Sample* (sample size = n) mean Prevalence
mean(clt_sample_k_mean)

# *Population* mean Prevalence
mean(ASD_State_SPED_2016$Prevalence)

# ----------------------------------
# Sampling distribution vs. Population distribution vs. Z-Norm
# ----------------------------------
# Create:
# Population (Prevalence) histogram in probablity
hist(ASD_State_SPED_2016$Prevalence, probability = T, 
     col=rgb(0.75,0.75,0.75,0.5), breaks = 50,
     xlab = 'Prevalvence', xlim = (c(0, 25)),
     ylab = 'Probability Density', ylim = (c(0, 0.6)),
     main = 'Visualize Central Limit Theorem (CLT)')

# Overlay curve:
# Population (Prevalence) density
lines(density(ASD_State_SPED_2016$Prevalence), col="grey4", lwd=2) 

# Overlay line:
# mean = mean of Population (Prevalence)
abline(v=mean(ASD_State_SPED_2016$Prevalence), col="black", lwd=2) 

# ----------------------------------
# Sampling distribution vs. Population distribution vs. Z-Norm
# ----------------------------------
# Create:
# Population (Prevalence) histogram in probablity
hist(ASD_State_SPED_2016$Prevalence, probability = T, 
     col=rgb(0.75,0.75,0.75,0.5), breaks = 50,
     xlab = 'Prevalvence', xlim = (c(0, 25)),
     ylab = 'Probability Density', ylim = (c(0, 0.6)),
     main = 'Visualize Central Limit Theorem (CLT)')

# Overlay curve:
# Population (Prevalence) density
lines(density(ASD_State_SPED_2016$Prevalence), col="grey4", lwd=2) 

# Overlay line:
# mean = mean of Population (Prevalence)
abline(v=mean(ASD_State_SPED_2016$Prevalence), col="black", lwd=2) 

# Overlay line:
# Sample means histogram in probability (Sampling disribution)
hist(clt_sample_k_mean, probability = T, 
     col=rgb(0,1,1,0.3), # https://www.dataanalytics.org.uk/make-transparent-colors-in-r/
     add=T)

# Overlay curve:
# Sample (Prevalence) density (Sampling disribution)
lines(density(clt_sample_k_mean), col="cyan2", lwd=2) 
# Overlay line:
# mean of Sampling distribution (of Prevelance, sample size n) 
abline(v=mean(clt_sample_k_mean), col="cyan2", lwd=2, lty=3) 

col2rgb(c("cyan", "grey", "purple", "orange")) / 255

# Recall:
# k *Sample* (sample size = n) mean Prevalence
mean(clt_sample_k_mean)
# *Population* mean Prevalence
mean(ASD_State_SPED_2016$Prevalence)
# We see that the above two means are close. Good estimation!

# ----------------------------------
# Standard Error (SE) (of mean prevalence), can be estimated as:
# std-dev of the Sampling distribution (of mean prevalence)
# ----------------------------------
# https://en.wikipedia.org/wiki/Sampling_distribution

# [1] Actual SE: When Population std-dev is known, SE using Population standard deviation:
sd.p(ASD_State_SPED_2016$Prevalence) / sqrt(clt_n)

# [2] Estimated SE with k samples: When Population std-dev is NOT known, but many sample means are known, SE using standard deviation of Sampling distribution
sd(clt_sample_k_mean)

# [3] Estimated SE with only one sample: When Population std-dev is NOT known, and only one sample obtained, SE using Sample's standard deviation: (std-dev of 1 Sample)
clt_sample_k_sd[1] / sqrt(clt_n)

# ----------------------------------
# Sampling distribution vs. Population distribution vs. Z-Norm
# ----------------------------------
# Create:
# Population (Prevalence) histogram in probablity
hist(ASD_State_SPED_2016$Prevalence, probability = T, 
     col=rgb(0.75,0.75,0.75,0.5), breaks = 50,
     xlab = 'Prevalvence', xlim = (c(0, 25)),
     ylab = 'Probability Density', ylim = (c(0, 0.6)),
     main = 'Visualize Central Limit Theorem (CLT)')

# Overlay curve:
# Population (Prevalence) density
lines(density(ASD_State_SPED_2016$Prevalence), col="grey4", lwd=2) 

# Overlay line:
# mean = mean of Population (Prevalence)
abline(v=mean(ASD_State_SPED_2016$Prevalence), col="black", lwd=2) 

# Overlay line:
# Sample means histogram in probability (Sampling disribution)
hist(clt_sample_k_mean, probability = T, 
     col=rgb(0,1,1,0.3), # https://www.dataanalytics.org.uk/make-transparent-colors-in-r/
     add=T)

# Overlay curve:
# Sample (Prevalence) density (Sampling disribution)
lines(density(clt_sample_k_mean), col="cyan2", lwd=2) 
# Overlay line:
# mean of Sampling distribution (of Prevelance, sample size n) 
abline(v=mean(clt_sample_k_mean), col="cyan2", lwd=2, lty=3) 

# Overlay curve:
# *Theoretic Sampling Distribution* with population mean & std-dev = Actual SE
# mean = mean of Population (Prevalence) & std-dev = std-dev of Population (Prevalence) / square root of sample size n
curve(dnorm(x, 
            mean(ASD_State_SPED_2016$Prevalence), # Actual Population mean
            sd.p(ASD_State_SPED_2016$Prevalence) / sqrt(clt_n)), # Actual SE (for mean prevalence) = Population standard deviation / square root of sample size
      add=TRUE, col="red", lwd=2, lty=3)

# ----------------------------------
# Evaluate normality
# ----------------------------------
# Construct a Quantile-Quantile Plot (QQ plot)
# https://youtu.be/okjYjClSjOg

par(mfrow=c(1, 2))
# Sample means
qqnorm(clt_sample_k_mean, col="darkgrey", 
       xlab="z Value", ylab="Prevalence")
qqline(clt_sample_k_mean, col="red", lwd=2, lty=3)
# Population
qqnorm(ASD_State_SPED_2016$Prevalence, col="darkgrey", 
       xlab="z Value", ylab="Prevalence")
qqline(ASD_State_SPED_2016$Prevalence, col="red", lwd=2, lty=3)
# Reset
par(mfrow=c(1, 1))


# Alternatively, use shapiro.test() to test Normality
set.seed(88)

# Test data of k smaple's means (Sampling Distribution data):
shapiro.test(sample(x = clt_sample_k_mean, size = 1000))

# Test data of population's Prevalence values (Population Distribution data):
shapiro.test(ASD_State_SPED_2016$Prevalence)

# ----------------------------------
# Use a sample of a few US. State's ASD prevalence (mean) to estimate:
# Average prevalence of ALL US. States (the *Population*) [Source SPED, Year 2016]
# ----------------------------------
dim(ASD_State_SPED_2016)
#
ASD_State_SPED_2016 # This is considered as a population now.

# Create a *Sample* from ASD_State_SPED_2016$Prevalence,
# with sample size n =
clt_n = 10
# Try 20 or 40, larger sample size, narrower the CI (more confident at xx% level)
# clt_n = 20 

set.seed(88)
clt_sample_1 = sample(x = ASD_State_SPED_2016$Prevalence, size = clt_n, replace = FALSE)
clt_sample_1

plot(density(clt_sample_1), col="darkgrey", lwd=2) 
hist(clt_sample_1, probability = T, add = T)

# Sample mean Prevalence
mean(clt_sample_1)

# *Population* mean Prevalence
mean(ASD_State_SPED_2016$Prevalence)

# ----------------------------------
# CI using Z (Standard Normal) distribution
# ----------------------------------
# sample mean
sample_mean = mean(clt_sample_1)
sample_mean

# sample size n
sample_size_n = length(clt_sample_1)
sample_size_n

# sample standard deviation
sample_sd = sd(clt_sample_1)
sample_sd

# sample standard error
sample_se = sample_sd / sqrt(sample_size_n)
sample_se

# 95% quantile (z score)
z_score = qnorm(p = 0.975)
z_score

# ?qnorm

# CI using Z distribution
sample_ci = z_score * sample_se
sample_ci

# Lower CI: mean + CI
sample_mean - sample_ci

# Upper CI: mean + CI
sample_mean + sample_ci

# Display
cat('\t< Confidence Interval (Prevalence) >\n',  '\tLower CI : ',  sample_mean - sample_ci, '\tMean : ', sample_mean, '\tUpper CI : ',  sample_mean + sample_ci)

# ----------------------------------
# CI using T distribution
# ----------------------------------
# sample mean
sample_mean = mean(clt_sample_1)
sample_mean
# sample size n
sample_size_n = length(clt_sample_1)
sample_size_n
# sample standard deviation
sample_sd = sd(clt_sample_1)
sample_sd
# sample standard error
sample_se = sample_sd / sqrt(sample_size_n)
sample_se

# 95% quantile (t score)
t_score = qt(p = 0.975, df = sample_size_n - 1)
t_score

# ?qt

# CI using T distribution
sample_ci = t_score * sample_se
sample_ci

# Lower CI: mean + CI
sample_mean - sample_ci

# Upper CI: mean + CI
sample_mean + sample_ci

# Display
cat('\t< Confidence Interval (Prevalence) >\n',  '\tLower CI : ',  sample_mean - sample_ci, '\tMean : ', sample_mean, '\tUpper CI : ',  sample_mean + sample_ci)

# Alternatively, calculate CI using t.test() function
t.test(clt_sample_1, conf.level = 0.95)

# Two group hypothesis test : sample mean vs. population mean
t.test(clt_sample_1, conf.level = 0.95, mu = mean(ASD_State_SPED_2016$Prevalence))

# Write your code below and press Shift+Enter to execute 


# ----------------------------------
# Use a sample of one US. State's ASD prevalence (proportion) to estimate:
# Prevalence of THAT US. State's ALL Children (the *Population*) [Source SPED, Year 2016] 
# ----------------------------------

# No. Children with ASD
ASD  <- ASD_State_SPED$Numerator_ASD[ASD_State_SPED$Year == 2016]
#
str(ASD)

# No. Children with ASD of first US. State (AL-Alabama)
ASD[1]

# No. Children surveyed
Children  <- ASD_State_SPED$Denominator[ASD_State_SPED$Year == 2016]
#
str(Children)

# No. Children surveyed of first US. State (AL-Alabama)
Children[1]

# ----------------------------------
# CI using Z score interval  (standard normal distribution)
# https://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval
# ----------------------------------

# sample proportion of first US. State (AL-Alabama) in year 2016 of source SPED
sample_proportion = ASD[1] / Children[1]
sample_proportion

# sample size n
sample_size_n = Children[1]
sample_size_n

# 95% quantile (z score)
z_score = qnorm(p = 0.975)
z_score

sample_ci = z_score * sqrt(sample_proportion * (1 - sample_proportion) / sample_size_n)
sample_ci

# Lower CI: mean + CI
sample_proportion - sample_ci

# Upper CI: mean + CI
sample_proportion + sample_ci

# Display
cat('\t< Confidence Interval >\n',  '\tLower CI : ',  sample_proportion - sample_ci, '\tMean : ', sample_proportion, '\tUpper CI : ',  sample_proportion + sample_ci)


# Display * 1000 -> Prevalence
cat('\t< Confidence Interval (Prevalence) >\n',  '\tLower CI : ',  1000*(sample_proportion - sample_ci), '\tMean : ', 1000*sample_proportion, '\tUpper CI : ',  1000*(sample_proportion + sample_ci))


# ----------------------------------
# CI using Wilson score interval
# https://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval
# ----------------------------------

sample_proportion # = ASD[1] / Children[1]

# Yates' chi-squared test = Wilson score interval with continuity correction - prop.test
prop.test(ASD[1], Children[1], conf.level = 0.95) 

# Pearson's chi-squared test = Wilson score interval - wilson
prop.test(ASD[1], Children[1], conf.level = 0.95, correct = FALSE) 

# Write your code below and press Shift+Enter to execute 


# Write your code below and press Shift+Enter to execute 


# Write your code below and press Shift+Enter to execute 


# ----------------------------------
# Interactive workshops: < Learning R inside R > using swirl() (in R/RStudio)
# ----------------------------------
if(!require(swirl)){install.packages("swirl")}
library("swirl")
install_course("R Programming")
install_course("Exploratory Data Analysis")
install_course("Regression Models")
install_course("Statistical Inference")
swirl()

# ----------------------------------
# Neural Network 101 using nnet()
# ----------------------------------
if(!require(nnet)){install.packages("nnet")}
library("nnet")
# ?nnet
 
# < Case: predict three different iris flower types >

# https://en.wikipedia.org/wiki/Iris_flower_data_set
# https://archive.ics.uci.edu/ml/datasets/iris

# Data preparation: split iris data in two halves, for training & testing respectively.
ir <- rbind(iris3[,,1],iris3[,,2],iris3[,,3])
targets <- class.ind( c(rep("s", 50), rep("c", 50), rep("v", 50)) )
samp <- c(sample(1:50,25), sample(51:100,25), sample(101:150,25))
# Model training (machine learning / data fitting)
ir1 <- nnet(ir[samp,], targets[samp,], size = 2, rang = 0.1,
            decay = 5e-4, maxit = 200)
# Model prediciton
test.cl <- function(true, pred) {
  true <- max.col(true)
  cres <- max.col(pred)
  table(true, cres)
}

# Model evaluation
test.cl(targets[-samp,], predict(ir1, ir[-samp,]))


# ----------------------------------
# Hypothesis Test - Mean - Z Test & t.test()
# ----------------------------------

# Create sample 1
set.seed(88)
sample_1 = rnorm(n = 10)
# sample_1 = rnorm(n = 1000)

# Create sample 2
set.seed(88)
sample_2 = rchisq(n = 10, df = 30) - 29
# sample_2 = rchisq(n = 1000, df = 30) - 29

mean(sample_1)
mean(sample_2)

par(mfrow=c(1, 2))
hist(sample_1)
hist(sample_2)
par(mfrow=c(1, 1))

t.test(sample_1, sample_2)

# ----------------------------------
# Hypothesis Test - Proportion - prop.test()
# ----------------------------------
if(!require(binom)){install.packages("binom")}
library('binom')

# Different flavours of proportion test:
binom.confint (x=ASD[1], n=Children[1], conf.level =0.95, method="all")

# Multiple group hypothesis test : proportions (Prevalvence) among all US. States
prop.test(ASD, Children)

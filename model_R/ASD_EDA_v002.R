###########################################################
# Study of Autism Spectrum Disorder (ASD)
# https://www.cdc.gov/ncbddd/autism/data/index.html#explore
# https://www.cdc.gov/ncbddd/autism/index.html
###########################################################

# 2019 12 03
# ----------------------------------
# Set working directory
# ----------------------------------
setwd("/media/sf_vm_shared_folder/git/DDC/DDC-ASD/model_R")
getwd()
options(warn=-1) # Turning off unnecessary warning messages. To reset: options(warn=0)

# ----------------------------------
# Read in data
# ----------------------------------
#
# ----------------------------------
# Dataset: US. National Level Children ASD Prevalence
# ----------------------------------
ASD_National <- read.csv("../dataset/ADV_ASD_National.csv", stringsAsFactors = FALSE)
# ----------------------------------
# Dataset: US. State Level Children ASD Prevalence
# ----------------------------------
ASD_State    <- read.csv("../dataset/ADV_ASD_State.csv", stringsAsFactors = FALSE)

# Obtain number of rows and number of columns/features/variables
dim(ASD_National)
dim(ASD_State)

# Obtain name of columns
names(ASD_National)
names(ASD_State)

# Obtain overview (data structure/types)
str(ASD_National)
str(ASD_State)

# Look at first few rows of data 
head(ASD_National)
head(ASD_State)

# Look at data stucture/schema (Selected columns)
str(ASD_National[, c(1:8, 24, 25, 26)])


# ----------------------------------
# Quiz: Obtain feature/column names of dataframe: ASD_State
# ----------------------------------
# Write your code here:
#



# ----------------------------------
# Work with dataframe
# ----------------------------------

# Access column 1 as a dataframe:
ASD_National[1] # str(ASD_National[1])
ASD_National["Source"]

# Access column 1 as a list of string/chr:
ASD_National[, 1] # str(ASD_National[, 1])
ASD_National[, "Source"]
ASD_National$Source

# Count number of elements in a object:
length(ASD_National) # number of features/columns
length(ASD_National[1, ]) # number of features/columns
length(ASD_National[, 1]) # number of elements(rows) in column 1
length(ASD_National[, "Source"]) # same as above
length(ASD_National$Source) # number of elements in list

# Access elements from column 1, which is a dataframe:
ASD_National[1][1, ]
ASD_National[1][11, ]
ASD_National[1][11:20, ]
# or
ASD_National["Source"][1, ]
ASD_National["Source"][11, ]
ASD_National["Source"][11:20, ]

# Access elements from column 1, which is a list of string/chr:
ASD_National[, 1][1]
ASD_National[, 1][11]
ASD_National[, 1][11:20]
# or uisng column name
ASD_National[, "Source"][1]
ASD_National[, "Source"][11]
ASD_National[, "Source"][11:20]
# or using $ operator
ASD_National$Source[1]
ASD_National$Source[11]
ASD_National$Source[11:20]


# Access elements of different column:
names(ASD_National)
ASD_National[1, 1] # row 1 column 1: "Source" 
ASD_National[10, 1] # row 10 column 1: "Source"
ASD_National[1, 3] # row 1 column 3: "Prevalence"
ASD_National[10, 3] # row 10 column 3: "Prevalence"

ASD_National[1:10, 1:3] # row 1 to 10 from column 1 to 3
# or using columns names
ASD_National[1:10, c('Source', 'Year', 'Prevalence')]

ASD_National[c(1:10, 20, 30:35), c(1:3, 9, 12)] # row 1 to 10, 20, and 20 to 25 from column 1 to 3, 9, and 12
# Tips: we noticed missing data from above.


# ----------------------------------
# Process missing data
# ----------------------------------

# Count missing values in dataframe:
sum(is.na(ASD_National)) # No missing data recognized by R (NA)
sum(is.na(ASD_State)) # Some missing data recognized by R (NA)

# Define several offending strings
na_strings <- c("", "No data", "NA", "N A", "N / A", "N/A", "N/ A", "Not Available", "NOt available")

# Load required function from packages:
if(!require(naniar)){install.packages("naniar")}
library(naniar)
if(!require(dplyr)){install.packages("dplyr")}
library(dplyr)
# Replace these defined missing values to R's internal NA
ASD_National = replace_with_na_all(ASD_National, condition = ~.x %in% na_strings)

# Count missing values in dataframe:
sum(is.na(ASD_National))

# ----------------------------------
# Process invalid characters
# ----------------------------------
# Remove invalid unicode char/string: \x92
ASD_National$Source_Full1[ASD_National$Source_Full1 == "National Survey of Children\x92s Health"] <- "National Survey of Children's Health"
ASD_National$Source_Full2[ASD_National$Source_Full2 == "nsch-National Survey of Children\x92s Health"] <- "nsch-National Survey of Children's Health"

# ----------------------------------
# Delete/Drop dataframe variable
# ----------------------------------
# Delete/Drop duplicate variable: Prevalence_dup
drop <- c("Prevalence_dup", "Dummy Variable Name")
ASD_National = ASD_National[,!(names(ASD_National) %in% drop)]

# ----------------------------------
# Create/Add dataframe variable
# ----------------------------------
# Create one new variable: Source_Full3 by combining Source, space, and Source_Full1
ASD_National$Source_UC <- paste(toupper(ASD_National$Source))
# Create one new variable: Source_Full3 by combining Source and Source_Full1
ASD_National$Source_Full3 <- paste(toupper(ASD_National$Source), ASD_National$Source_Full1)

# ----------------------------------
# Convert to correct data types
# ----------------------------------
# Review data structure and variable names:
str(ASD_National)
names(ASD_National)

# Convert Prevalence and CIs from categorical/chr to numeric, column 8 to 25
ix <- 8:25 # define an index
ASD_National[ix] <- lapply(ASD_National[ix], as.numeric)

# Convert Source from categorical/chr to categorical/factor
ix <- c(1, 6, 7, 26, 27) # define an index
ASD_National[ix] <- lapply(ASD_National[ix], as.factor)

# Create new ordered factor Year_Factor from Year
ASD_National$Year_Factor <- factor(ASD_National$Year, ordered = TRUE)

# Observe the difference of 'Levels' in below two factors
ASD_National$Year_Factor # Ordinal categorical variable
ASD_National$Source # Nominal categorical variable

# Optionally, below is manual conversion examples:
# ASD_National$Male.Prevalence = as.numeric(ASD_National$Male.Prevalence)
# ASD_National$Source = as.factor(ASD_National$Source)


# ----------------------------------
# Optionally, export the processed dataframe data to CSV file.
# ----------------------------------
write.table(ASD_National, file = "../dataset/ASD_National_R.csv", sep = ',', row.names = FALSE)
ASD_National <- read.csv("../dataset/ASD_National_R.csv")
ASD_National$Year_Factor <- factor(ASD_National$Year_Factor, ordered = TRUE) # Convert Year_Factor to ordered.factor


# ----------------------------------
# EDA - Descriptive Analysis
# ----------------------------------
# Look at high level data summary:
summary(ASD_National)

# ----------------------------------
# Look at summary of numeric variables
# ----------------------------------
# Filter only numeric variables/columns
select_if(ASD_National, is.numeric) # library(dplyr)
# Data summarization
summary(select_if(ASD_National, is.numeric))

# Calculate agerage Prevalence, no error
mean(ASD_National$Prevalence)
mean(ASD_National$Prevalence[ASD_National$Source == 'addm'])
mean(ASD_National$Prevalence[ASD_National$Source == 'medi'])
mean(ASD_National$Prevalence[ASD_National$Source == 'nsch'])
mean(ASD_National$Prevalence[ASD_National$Source == 'sped'])
# Calculate agerage Male.Prevalence, there is error!
mean(ASD_National$Male.Prevalence)
# Because of NA, mean() cannot process, thus we use na.rm to ignore NAs
mean(ASD_National$Male.Prevalence, na.rm = TRUE)
mean(ASD_National$Female.Prevalence, na.rm = TRUE)

# Count occurrences of uniques values in a variable/column: number of rows (of data entry) per year
table(ASD_National$Year)

# ----------------------------------
# Look at summary of categorical variables
# ----------------------------------
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

# Display unique values (levels) of a factor categrotical 
lapply(select_if(ASD_National, is.factor), levels)
# or using variable names
lapply(ASD_National[c('Source_UC', 'Year_Factor')], levels)

# Pivot of counting occurrences
table(ASD_National$Source_Full3, ASD_National$Year) # table(ASD_National$Year, ASD_National$Source_Full3)


# ----------------------------------
# EDA - Visualization
# ----------------------------------

# Histogram
hist(ASD_National$Prevalence)
hist(ASD_National$Male.Prevalence)
hist(ASD_National$Female.Prevalence)
# Histogram with annotations
hist(ASD_National$Prevalence,
        main = "Frequency of National ASD Prevalence",
        xlab = "Prevalence per 1,000 Children",
        ylab = "Frequency or Occurrences",
        sub  = "Year 2000 - 2016",
        col.main="blue", col.lab="black", col.sub="darkgrey")


# Density plot
plot(density(ASD_National$Prevalence))
# Density plot with annotations
plot(density(ASD_National$Prevalence),
     main = "Density of National ASD Prevalence",
     xlab = "Prevalence per 1,000 Children",
     ylab = "Frequency or Occurrences",
     sub  = "Year 2000 - 2016",
     col.main="blue", col.lab="black", col.sub="darkgrey")


# Boxplot
boxplot(ASD_National$Male.Prevalence) # Male children
boxplot(ASD_National$Female.Prevalence) # Female children

par(mfrow=c(1, 2)) # multiple plots on one page: row split to: 1,column split to: 2
# Male prevalence and Female prevalence side by side:
boxplot(ASD_National$Male.Prevalence) # Male children
boxplot(ASD_National$Female.Prevalence) # Female children
# All children prevalence with and without 95% confidence side by side:
boxplot(ASD_National$Prevalence, notch = TRUE) # 95% confidence interval
boxplot(ASD_National$Prevalence) # All children
par(mfrow=c(1, 1)) # Reset to one plot on one page

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
        col.main="blue", col.lab="black", col.sub="darkgrey"
        )


# ----------------------------------
# [National] < Prevalence has changed over Time >
# ----------------------------------
# Prevalence over Year
# Use Year        as x-axis: y value Prevalence is NOT aggregated for differnt data scources
plot(ASD_National$Year, ASD_National$Prevalence) 
# Use Year_factor as x-axis: y value Prevalence is     aggregated for differnt data scources
plot(ASD_National$Year_Factor, ASD_National$Prevalence) 

table(ASD_National$Source_Full3)

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

# ----------------------------------
# Add more annotations to above plots
# ----------------------------------
# Color list
# addm : darkblue
# medi : orange
# nsch : darkred
# sped : skyblue

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
# Create multiple lines with a single chart
# ----------------------------------

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


# ----------------------------------
# Quiz: Add 95% Confidence Interval to above plot
# ----------------------------------
# Write your code here:
#



# ----------------------------------
# Quiz: Count occurrences of catergorical variables
# ----------------------------------
# Write your code here:
#
table(ASD_National$Source)
barplot(table(ASD_National$Source))


# ----------------------------------
# Quiz: Which data sources are available in which years
# ----------------------------------
# Write your code here:
#
table(ASD_National$Year, ASD_National$Source)
plot(table(ASD_National$Year, ASD_National$Source))

# ----------------------------------
# Quiz: Which data source has breakdown data by sex/gender
# ----------------------------------
# Write your code here:
#
table(ASD_National$Source_Full2, ASD_National$Male.Prevalence)
plot(table(ASD_National$Source_Full2, ASD_National$Male.Prevalence))


# ----------------------------------
# Quiz: Which data source has breakdown data by race and ethnicity
# ----------------------------------
# Write your code here:
#
table(ASD_National$Source, ASD_National$Asian.or.Pacific.Islander.Prevalence)
plot(table(ASD_National$Source, ASD_National$Asian.or.Pacific.Islander.Prevalence))


# ----------------------------------
# EDA - Nicer Visualization with ggplot2
# ----------------------------------
if(!require(ggplot2)){install.packages("ggplot2")}
library(ggplot2)
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

# ----------------------------------
# Barplot
# ----------------------------------
# Create bar chart using R graphics
barplot(table(ASD_National$Source))

# Create bar chart using ggplot2
ggplot(ASD_National, aes(x = Source)) + geom_bar()
# 
ggplot(ASD_National, aes(x = Source, fill = factor(Year))) + geom_bar() + 
  theme(legend.position="top") + labs(fill = "Legend: Year")
# facets = . ~ Year
ggplot(ASD_National, aes(x = Source, fill = Source)) + geom_bar() + 
  theme(legend.position="top") + 
  scale_fill_manual("Data Source:", values = c("addm" = "darkblue", 
                                               "medi" = "orange", 
                                               "nsch" = "darkred",
                                               "sped" = "skyblue")) +
  facet_grid(facets = . ~ Year)
# facets = Source ~ Year
ggplot(ASD_National, aes(x = Source, fill = Source)) + geom_bar() + 
  theme(legend.position="top") + 
  scale_fill_manual("Data Source:", values = c("addm" = "darkblue", 
                                               "medi" = "orange", 
                                               "nsch" = "darkred",
                                               "sped" = "skyblue")) +
  facet_grid(facets = Source~Year)


# ----------------------------------
# Histogram (binned distribution)
# ----------------------------------

# Create histogram using R graphics
hist(ASD_National$Prevalence, breaks = 10)

# Create histogram using ggplot2
ggplot(ASD_National, aes(x=Prevalence)) + geom_histogram(binwidth = 5)

# Use color to differentiate sub-group data
ggplot(ASD_National, aes(x=Prevalence, fill = Source)) +
  geom_histogram(binwidth = 5) +
  theme_bw() + theme(legend.position="right") +
  scale_fill_manual("Data Source:", values = c("addm" = "darkblue", 
                                               "medi" = "orange", 
                                               "nsch" = "darkred",
                                               "sped" = "skyblue"))

# Plot sub-group data side by side
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


# ----------------------------------
# Density plot (distribution for continuous variable)
# ----------------------------------
# Create plot using R graphics
plot(density(ASD_National$Prevalence))

# Create box plot using ggplot2
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
ggplot(ASD_National) + geom_density(aes(x = Prevalence, fill = Source), alpha = 0.5) + 
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


# ----------------------------------
# Boxplot
# ----------------------------------
# Create 'Prevalence' box plots break by 'Source'
# Create box plot using R graphics
boxplot(ASD_National$Prevalence ~ ASD_National$Source,
        main = "National ASD Prevalence by Data Source",
        xlab = "Data Source",
        ylab = "Prevalence per 1,000 Children",
        sub  = "Year 2000 - 2016",
        col.main="blue", col.lab="black", col.sub="darkgrey"
)

# Create box plot using ggplot2
ggplot(ASD_National, aes(x = Source, y = Prevalence)) + 
  geom_boxplot(fill = 'darkslategrey', alpha = 0.2) + 
  scale_y_continuous(name = "Prevalence per 1,000 Children",
                     breaks = seq(0, 30, 5),
                     limits=c(0, 30)) +
  scale_x_discrete(name = "Data Source (Year 2000 - 2016)") +
  ggtitle("National ASD Prevalence by Data Source") +
  theme(title = element_text(face = 'bold.italic', color = "darkslategrey"), 
        axis.title = element_text(face = 'plain', color = "darkslategrey"))


# ----------------------------------
# Line chart
# ----------------------------------

# ----------------------------------
# [National] < Prevalence Varies over Time/Year by Data Source >
# ----------------------------------

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
# Customize chart title:
p <- p + ggtitle("National ASD Prevalence by Data Source") 
# Show plot
p
# Customize chart title and axis labels:
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
  ggtitle("National ASD Prevalence by Data Source") +
  theme(title = element_text(face = 'bold.italic', color = "darkslategrey"), 
        axis.title = element_text(face = 'plain', color = "darkslategrey"))
# Show plot
p
# Optionally, add data values/labels
p + geom_text(aes(label = round(Prevalence, 1)), # Values are rounded for display
          vjust = "outward", 
#          nudge_y = 0.2, # optionally life the text
          hjust = "outward", 
          check_overlap = TRUE,
          size = 3, # size of textual data label
          col = 'darkslategrey'
          )


# ----------------------------------
# Dynamic Visualization with plotly
# ----------------------------------
if(!require(plotly)){install.packages("plotly")}
library(plotly)
p_dynamic <- p
p_dynamic <- ggplotly(p_dynamic)
p_dynamic


# ----------------------------------
# Use themes as aesthetic template
# ----------------------------------
library(ggthemes)
# theme of the economist magazine:
p + theme_economist() + scale_colour_economist()
# theme of the Wall Street Journal:
p + theme_wsj() + scale_colour_wsj("colors6")

# Dynamic chart with theme of the economist magazine:
p_dynamic <- p + theme_economist() + scale_colour_economist()
p_dynamic <- ggplotly(p_dynamic)
p_dynamic


# ----------------------------------
# [addm] < Prevalence Varies by Sex >
# ----------------------------------

# Filter only data of ADDM
ASD_National_ADDM <- subset(ASD_National, Source == 'addm')

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
  ggtitle("Prevalence Estimates by Sex [ADDM]") +
  theme(title = element_text(face = 'bold.italic', color = "darkslategrey"), 
        axis.title = element_text(face = 'plain', color = "darkslategrey")) 
# Show plot
p
# apply theme
p + theme_economist() + scale_colour_economist() # p + theme_wsj() + scale_colour_wsj("colors6")

# Dynamic chart:
p_dynamic <- p + theme_economist() + scale_colour_economist()
p_dynamic <- ggplotly(p_dynamic)
p_dynamic


# ----------------------------------
# Quiz: Use ggplot to create other ASD prevalence visualizaiotns
# ----------------------------------

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
  ggtitle("Prevalence Estimates by Race/Ethnicity [ADDM]") +
  theme(title = element_text(face = 'bold.italic', color = "darkslategrey"), 
        axis.title = element_text(face = 'plain', color = "darkslategrey")) 
# Show plot
p
# apply theme
p + theme_economist() + scale_colour_economist() # p + theme_wsj() + scale_colour_wsj("colors6")

# Dynamic chart:
p_dynamic <- p + theme_economist() + scale_colour_economist()
p_dynamic <- ggplotly(p_dynamic)
p_dynamic


# ----------------------------------
# Dataset: US. State Level Children ASD Prevalence
# ----------------------------------

ASD_State    <- read.csv("../dataset/ADV_ASD_State.csv", stringsAsFactors = FALSE)

# Obtain number of rows and number of columns/features/variables
dim(ASD_State)
# Obtain overview (data structure/types)
str(ASD_State)

# ----------------------------------
# Pre-Process data
# ----------------------------------
# Count missing values in dataframe:
sum(is.na(ASD_State)) # No missing data recognized by R (NA)
# Define several offending strings
na_strings <- c("", "No data", "NA", "N A", "N / A", "N/A", "N/ A", "Not Available", "NOt available")
# Replace these defined missing values to R's internal NA
ASD_State = replace_with_na_all(ASD_State, condition = ~.x %in% na_strings)
# Count missing values in dataframe:
sum(is.na(ASD_State))
# Remove invalid unicode char/string: \x92
ASD_State$Source_Full1[ASD_State$Source_Full1 == "National Survey of Children\x92s Health"] <- "National Survey of Children's Health"
# Delete/Drop variable by index: column from 14 to 26, 29, and 30
names(ASD_State)
ASD_State <- ASD_State[ -c(14:26, 29, 30) ]
# Create one new variable: Source_UC as uppercase of Source
ASD_State$Source_UC <- paste(toupper(ASD_State$Source))
# Create one new variable: Source_Full3 by combining Source_UC and Source_Full1
ASD_State$Source_Full3 <- paste(ASD_State$Source_UC, ASD_State$Source_Full1)
# Convert to correct data types
str(ASD_State)
names(ASD_State)
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


# ----------------------------------
# Optionally, export the processed dataframe data to CSV file.
# ----------------------------------
write.table(ASD_State, file = "../dataset/ASD_State_R.csv", sep = ',', row.names = FALSE)
ASD_State <- read.csv("../dataset/ASD_State_R.csv")
ASD_State$Year_Factor <- factor(ASD_State$Year_Factor, ordered = TRUE) # Convert Year_Factor to ordered.factor


# ----------------------------------
# EDA - Visualization of US. State level data
# ----------------------------------

# Below plot may run for a while
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
# Show plot
p

# ASD_State_ADDM <- subset(ASD_State, Year == 2014 & Source == 'addm')
ASD_State_ADDM <- subset(ASD_State, Source == 'addm')
ASD_State_MEDI <- subset(ASD_State, Source == 'medi')
ASD_State_NSCH <- subset(ASD_State, Source == 'nsch')
ASD_State_SPED <- subset(ASD_State, Source == 'sped')

# < Years Data Available [State] [ADDM] >
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
  labs(x="", y="", title="Years Data Available by State [ADDM]") # layers of graphics
# Show plot
p


# ----------------------------------
# Quiz: create < Years Data Available by State [Source] > for other three data sources:
# ----------------------------------
# Write your code here:
#


# ----------------------------------
# < ASD Prevalence by State [ADDM] > aggregated for different years
# ----------------------------------

p <- ggplot(ASD_State_ADDM, aes(x = reorder(State_Full2, Prevalence, FUN = median), # Order States by median of Prevalence  
                           y = Prevalence)) + 
  geom_boxplot(aes(fill = reorder(State_Full2, Prevalence, FUN = median))) + # fill color by State
  scale_fill_discrete(guide = guide_legend(title = "US. States")) + # Legend Name
#  geom_boxplot(fill = 'darkslategrey', alpha = 0.2) + 
  scale_y_continuous(name = "Prevalence per 1,000 Children",
                     breaks = seq(0, 30, 5),
                     limits=c(0, 30)) +
  scale_x_discrete(name = "") +
  ggtitle("ASD Prevalence by State [ADDM]") +
  theme(title = element_text(face = 'bold.italic', color = "darkslategrey"), 
        axis.title = element_text(face = 'plain', color = "darkslategrey"),
        legend.position = 'none') + 
  coord_flip() + # Rotate chart
  geom_jitter(position=position_jitter(0.1)) # Add actual data points
# Show plot
p
# theme of the economist magazine:
p + theme_economist() + scale_colour_economist() + theme(legend.position = 'none')

# Dynamic chart
p_dynamic <- p + theme_economist() + scale_colour_economist() + theme(legend.position = 'none')
p_dynamic <- ggplotly(p_dynamic)
p_dynamic


# ----------------------------------
# Quiz: create < ASD Prevalence by State [Source] > for other three data sources:
# ----------------------------------
# Write your code here:
#



# ----------------------------------
# < No. Children surveyed by State [ADDM] [Year 2014] >
# ----------------------------------

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
  ggtitle("No. Children surveyed by State [ADDM] [Year 2014]") +
#  geom_text(aes(label=Denominator), vjust=1.6, color="darkslategrey", size=3.5) + # Show data label inside bars
  theme(title = element_text(face = 'bold.italic', color = "darkslategrey"), 
        axis.title = element_text(face = 'plain', color = "darkslategrey"),
        legend.position="none") 
# Show plot
p
# theme of the economist magazine:
p + theme_economist() + scale_colour_economist() + theme(legend.position = 'none')

# Dynamic chart
p_dynamic <- p + theme_economist() + scale_colour_economist() + theme(legend.position = 'none')
p_dynamic <- ggplotly(p_dynamic)
p_dynamic

# ----------------------------------
# Quiz: create< No. Children surveyed by State [ADDM] [Year XXXX] > for other years:
# ----------------------------------
# Write your code here:
#


# ----------------------------------
# Quiz: create< No. ASD Children by State [ADDM] [Year 2014] > :
# ----------------------------------
# hint: use variable: ASD_State_ADDM$Numerator_ASD
# Write your code here:
#


# ----------------------------------
# < ASD Prevalence with 95% CI by State [ADDM] [Year 2014] >
# ----------------------------------
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
  ggtitle("ASD Prevalence with 95% CI by State [ADDM] [Year 2014]") +
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
p + theme_economist() + scale_colour_economist() + scale_colour_discrete(guide = guide_legend(title = "US. States")) + theme(legend.position = 'none')

# Dynamic chart
p_dynamic <- p + theme_economist() + scale_colour_economist() + scale_colour_discrete(guide = guide_legend(title = "US. States")) + theme(legend.position = 'none')
p_dynamic <- ggplotly(p_dynamic)
p_dynamic

# ----------------------------------
# Quiz: create < State ASD Prevalence with 95% CI by State [ADDM] [Year XXXX] > for other years:
# ----------------------------------
# Write your code here:
#


# ----------------------------------
# < ASD Prevalence by Year [ADDM] [AZ-Arizona] >
# ----------------------------------

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
p <- p + ggtitle("ASD Prevalence by Year [ADDM] [AZ-Arizona]") 
# Customize chart title and axis labels:
p <- p + theme(title = element_text(face = 'bold.italic', color = "darkslategrey"), 
               axis.title = element_text(face = 'plain', color = "darkslategrey")) 
# Show plot
p
# theme of the economist magazine:
p + theme_economist() + scale_colour_economist()


# ----------------------------------
# Line plot/chart for < ASD Prevalence by Year [ADDM] [All States] >
# ----------------------------------

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
p
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
p <- p + ggtitle("ASD Prevalence by Year [ADDM] [All States]") 
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
p + facet_grid(facets = . ~ State_Full1) + 
  theme(legend.position = "none", # Hide legend
      axis.text.x=element_blank(),  # Hide axis
      axis.ticks.x=element_blank(), # Hide axis
      panel.background = element_blank() # Remove panel background
      ) 


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

# Prepare data - addm 2014
#
Map_Data_Source = 'addm' # Available values lowercase: 'addm', 'medi', 'nsch', 'sped'.
Map_Data_Value = 'Prevalence' # variable must be numeric, variable name in 'quotation'. Or else Error: Discrete value supplied to continuous scale
# or Prevalence of different groups
Map_Data_Value = 'Male.Prevalence' # variable must be numeric, variable name in 'quotation'. Or else Error: Discrete value supplied to continuous scale
Map_Data_Value = 'Female.Prevalence' # variable must be numeric, variable name in 'quotation'. Or else Error: Discrete value supplied to continuous scale
Map_Data_Value = 'Asian.or.Pacific.Islander.Prevalence' # variable must be numeric, variable name in 'quotation'. Or else Error: Discrete value supplied to continuous scale
#
Map_Data_Year = 2014 # must be integer
#
ASD_State_Subset = subset(ASD_State, Source == Map_Data_Source & Year == Map_Data_Year)
ASD_State_Subset$state = ASD_State_Subset$State

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


# ----------------------------------
# ASD_State_NSCH # Year: 2004, 2008, 2012, 2016
# ----------------------------------
#
Map_Data_Source = 'nsch' # Available values lowercase: 'addm', 'medi', 'nsch', 'sped'.
Map_Data_Value = 'Prevalence' # variable must be numeric, variable name in 'quotation'. Or else Error: Discrete value supplied to continuous scale
#
# Prepare data - nsch 2004
Map_Data_Year = 2004 # must be integer
ASD_State_Subset = subset(ASD_State, Source == Map_Data_Source & Year == Map_Data_Year)
ASD_State_Subset$state = ASD_State_Subset$State
p_map_nsch_2004 <- plot_usmap(data = ASD_State_Subset, values = Map_Data_Value, color = "white", labels = TRUE, label_color = 'white' ) + scale_fill_continuous(na.value = "lightgrey", low="lightblue1", high = "darkblue", name = "Prevalence\nper 1,000\nChildren", limits=c(0, 40) ) + labs(title = paste("Prevalence Estimates by Geographic Area", '\n[ Measure :', Map_Data_Value, "] [ Source :", Map_Data_Source, "] [ Year :", Map_Data_Year, "]"), subtitle = 'https://www.cdc.gov/ncbddd/autism' ) + theme(panel.background = element_rect(color = "white", fill = "white"), legend.position = "right")
p_map_nsch_2004
# Prepare data - nsch 2008
Map_Data_Year = 2008 # must be integer
ASD_State_Subset = subset(ASD_State, Source == Map_Data_Source & Year == Map_Data_Year)
ASD_State_Subset$state = ASD_State_Subset$State
p_map_nsch_2008 <- plot_usmap(data = ASD_State_Subset, values = Map_Data_Value, color = "white", labels = TRUE, label_color = 'white' ) + scale_fill_continuous(na.value = "lightgrey", low="lightblue1", high = "darkblue", name = "Prevalence\nper 1,000\nChildren", limits=c(0, 40) ) + labs(title = paste("Prevalence Estimates by Geographic Area", '\n[ Measure :', Map_Data_Value, "] [ Source :", Map_Data_Source, "] [ Year :", Map_Data_Year, "]"), subtitle = 'https://www.cdc.gov/ncbddd/autism' ) + theme(panel.background = element_rect(color = "white", fill = "white"), legend.position = "right")
p_map_nsch_2008
# Prepare data - nsch 2012
Map_Data_Year = 2012 # must be integer
ASD_State_Subset = subset(ASD_State, Source == Map_Data_Source & Year == Map_Data_Year)
ASD_State_Subset$state = ASD_State_Subset$State
p_map_nsch_2012 <- plot_usmap(data = ASD_State_Subset, values = Map_Data_Value, color = "white", labels = TRUE, label_color = 'white' ) + scale_fill_continuous(na.value = "lightgrey", low="lightblue1", high = "darkblue", name = "Prevalence\nper 1,000\nChildren", limits=c(0, 40) ) + labs(title = paste("Prevalence Estimates by Geographic Area", '\n[ Measure :', Map_Data_Value, "] [ Source :", Map_Data_Source, "] [ Year :", Map_Data_Year, "]"), subtitle = 'https://www.cdc.gov/ncbddd/autism' ) + theme(panel.background = element_rect(color = "white", fill = "white"), legend.position = "right")
p_map_nsch_2012
# Prepare data - nsch 2016
Map_Data_Year = 2016 # must be integer
ASD_State_Subset = subset(ASD_State, Source == Map_Data_Source & Year == Map_Data_Year)
ASD_State_Subset$state = ASD_State_Subset$State
p_map_nsch_2016 <- plot_usmap(data = ASD_State_Subset, values = Map_Data_Value, color = "white", labels = TRUE, label_color = 'white' ) + scale_fill_continuous(na.value = "lightgrey", low="lightblue1", high = "darkblue", name = "Prevalence\nper 1,000\nChildren", limits=c(0, 40) ) + labs(title = paste("Prevalence Estimates by Geographic Area", '\n[ Measure :', Map_Data_Value, "] [ Source :", Map_Data_Source, "] [ Year :", Map_Data_Year, "]"), subtitle = 'https://www.cdc.gov/ncbddd/autism' ) + theme(panel.background = element_rect(color = "white", fill = "white"), legend.position = "right")
p_map_nsch_2016

# Dynamic map
p_dynamic <- p_map_nsch_2016
p_dynamic <- ggplotly(p_dynamic)
p_dynamic


# ----------------------------------
# Combine mutiple plots 
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
ggsave("Prevalence Estimates by Geographic Area [NSCH] [2004-2016].png", width = 60, height = 30, units = 'cm')


# ----------------------------------
# Mean, Median, Central Tendency
# ----------------------------------

# Sample median [0]
median(ASD_State$Prevalence[ASD_State$Source == 'addm' & ASD_State$Year == 2014])
boxplot(ASD_State$Prevalence[ASD_State$Source == 'addm' & ASD_State$Year == 2014])
#
# Analyze/Estimate Population mean of Prevalence ( State: All , Source: ADDM, Year: 2014 )
#
# Sample mean [1]
mean(ASD_State$Prevalence[ASD_State$Source == 'addm' & ASD_State$Year == 2014])
hist(ASD_State$Prevalence[ASD_State$Source == 'addm' & ASD_State$Year == 2014])
# Population mean [2]
sum(ASD_State$Numerator_ASD[ASD_State$Source == 'addm' & ASD_State$Year == 2014])
sum(ASD_State$Denominator[ASD_State$Source == 'addm' & ASD_State$Year == 2014])
1000*sum(ASD_State$Numerator_ASD[ASD_State$Source == 'addm' & ASD_State$Year == 2014])/sum(ASD_State$Denominator[ASD_State$Source == 'addm' & ASD_State$Year == 2014])
#
# (A/B + C/D) Not = (A+C)/(B+D)
#
# Population mean [3]
mean(ASD_National$Prevalence[ASD_National$Source == 'addm' & ASD_National$Year == 2014])


# ----------------------------------
# Confidence Interval
# ----------------------------------

# ----------------------------------
# CI using Z (Standard Normal) distribution
# ----------------------------------
# sample mean
sample_mean = mean(ASD_State$Prevalence[ASD_State$Source == 'addm' & ASD_State$Year == 2014])
sample_mean
# sample size n
sample_size_n = length(ASD_State$Prevalence[ASD_State$Source == 'addm' & ASD_State$Year == 2014])
sample_size_n
# sample standard deviation
sample_sd = sd(ASD_State$Prevalence[ASD_State$Source == 'addm' & ASD_State$Year == 2014])
sample_sd
# sample standard error
sample_se = sample_sd / sqrt(sample_size_n)
sample_se
# 95% quantile (z score)
z_score = qnorm(p = 0.975)
z_score
# CI using Z distribution
sample_ci = z_score * sample_se
sample_ci
# Lower CI: mean + CI
sample_mean - sample_ci
# Upper CI: mean + CI
sample_mean - sample_ci
# Display
cat('\t< Confidence Interval >\n',  '\tLower CI : ',  sample_mean - sample_ci, '\tMean : ', sample_mean, '\tUpper CI : ',  sample_mean + sample_ci)

# ----------------------------------
# CI using T distribution
# ----------------------------------
# sample mean
sample_mean = mean(ASD_State$Prevalence[ASD_State$Source == 'addm' & ASD_State$Year == 2014])
sample_mean
# sample size n
sample_size_n = length(ASD_State$Prevalence[ASD_State$Source == 'addm' & ASD_State$Year == 2014])
sample_size_n
# sample standard deviation
sample_sd = sd(ASD_State$Prevalence[ASD_State$Source == 'addm' & ASD_State$Year == 2014])
sample_sd
# sample standard error
sample_se = sample_sd / sqrt(sample_size_n)
sample_se
# 95% quantile (t score)
t_score = qt(p = 0.975, df = sample_size_n - 1)
t_score
# CI using T distribution
sample_ci = t_score * sample_se
sample_ci
# Lower CI: mean + CI
sample_mean - sample_ci
# Upper CI: mean + CI
sample_mean - sample_ci
# Display
cat('\t< Confidence Interval >\n',  '\tLower CI : ',  sample_mean - sample_ci, '\tMean : ', sample_mean, '\tUpper CI : ',  sample_mean + sample_ci)

# Alternatively, calculate CI using t.test() function
t.test(ASD_State$Prevalence[ASD_State$Source == 'addm' & ASD_State$Year == 2014])


# ----------------------------------
# Congratulations on the completion!
# ----------------------------------
#
# ----------------------------------
# 'The End'... is called 'The New Start'.
# ----------------------------------
# GU Zhan (Sam)
# zhan.gu@nus.edu.sg
# ----------------------------------


# ----------------------------------
# Appendix - Start
# ----------------------------------


# ----------------------------------
# Hypothesis Test - Mean - Z Test & t.test()
# ----------------------------------
set.seed(0)
sample_1 = rnorm(n = 1000)
sample_2 = rchisq(n = 1000, df = 30)

par(mfrow=c(1, 2))
hist(sample_1)
hist(sample_2)
par(mfrow=c(1, 1))

t.test(sample_1, sample_2)

# ----------------------------------
# Hypothesis Test - Proportion - prop.test()
# ----------------------------------

# State	Denominator	Prevalence	Lower CI	Upper CI	Year	Source	State_Full	Numerator_Prevalence
# AZ	45,322	6.5	5.8	7.3	2000	addm	Arizona	295
# GA	43,593	6.5	5.8	7.3	2000	addm	Georgia	283
# MD	21,532	5.5	4.6	6.6	2000	addm	Maryland	118
# NJ	29,714	9.9	8.9	11.1	2000	addm	New Jersey	294


# Confidence Interval
ASD  <- c( 295 ) 
Children <- c( 45322 )
prop.test(ASD, Children) # Yates' chi-squared test = Wilson score interval with continuity correction - prop.test
prop.test(ASD, Children, correct = FALSE) # Pearson's chi-squared test = Wilson score interval - wilson

ASD  <- c( 283 )
Children <- c( 43593 )
prop.test(ASD, Children)
prop.test(ASD, Children, correct = FALSE)

ASD  <- c( 118 )
Children <- c( 21532 )
prop.test(ASD, Children)
prop.test(ASD, Children, correct = FALSE)

ASD  <- c( 294 )
Children <- c( 29714 )
prop.test(ASD, Children)
prop.test(ASD, Children, correct = FALSE)

?prop.test

ASD  <- c( 295, 283, 118, 294 ) 
Children <- c( 45322, 43593, 21532, 29714 )
prop.test(ASD, Children)
prop.test(ASD, Children, correct = FALSE)

# Different flavours of proportion test:
if(!require(binom)){install.packages("binom")}
library(binom)
binom.confint (x=295, n=45322, conf.level =0.95, method="all")


# ----------------------------------
# Appendix - End
# ----------------------------------

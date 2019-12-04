###########################################################
# Study of Autism Spectrum Disorder (ASD)
# https://www.cdc.gov/ncbddd/autism/data/index.html#explore
# https://www.cdc.gov/ncbddd/autism/index.html
###########################################################

# 2019 12 03
setwd("/media/sf_vm_shared_folder/git/DDC/DDC-ASD/model_R")
getwd()

# ----------------------------------
# Read in data
# ----------------------------------
ASD_National <- read.csv("../dataset/ADV_ASD_National.csv", stringsAsFactors = FALSE)
ASD_State    <- read.csv("../dataset/ADV_ASD_State.csv", stringsAsFactors = FALSE)

# Look at data stucture/schema
head(ASD_National)
head(ASD_State)

# Obtain number of rows and number of columns/features/variables
dim(ASD_National)
dim(ASD_State)

# Obtain name of columns
names(ASD_National)
names(ASD_State)

# Obtain overview (data structure/types)
str(ASD_National)
str(ASD_State)

# Look at data stucture/schema (Selected columns)
str(ASD_National[, c(1:8, 24, 25, 26)])


# Quiz: 
# Obtain feature/column names of dataframe: ASD_State
# Write your code here:



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
library(dplyr)
library(naniar)
# Replace these defined missing values to R's internal NA
ASD_National = replace_with_na_all(ASD_National, condition = ~.x %in% na_strings)

# Count missing values in dataframe:
sum(is.na(ASD_National))


# ----------------------------------
# Process invalid characters
# ----------------------------------
# Remove invalid unicode char/string: \x92
ASD_National$Source_Full1[ASD_National$Source_Full1 == "National Survey of Children\x92s Health"] <- "National Survey of Children Health"
ASD_National$Source_Full2[ASD_National$Source_Full2 == "nsch-National Survey of Children\x92s Health"] <- "nsch-National Survey of Children Health"


# ----------------------------------
# Convert to correct data types
# ----------------------------------

str(ASD_National)

# Convert Prevalence and CIs from categorical/chr to numeric, column 9 to 26
names(ASD_National)
ix <- 9:26 # define an index
ASD_National[ix] <- lapply(ASD_National[ix], as.numeric)

# Convert Source from categorical/chr to categorical/factor, column 1, 7, 8
names(ASD_National)
ix <- c(1, 7, 8) # define an index
ASD_National[ix] <- lapply(ASD_National[ix], as.factor)

# Optionally, below is manual conversion examples:
# ASD_National$Male.Prevalence = as.numeric(ASD_National$Male.Prevalence)
# ASD_National$Source = as.factor(ASD_National$Source)


# ----------------------------------
# EDA - Descriptive Analysis
# ----------------------------------

summary(ASD_National)

# ----------------------------------
# Look at summary of numeric variables
# ----------------------------------
library(dplyr)
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
# categorical variables: Source, Source_Full1, Source_Full2

# library(dplyr)
summary(select_if(ASD_National, is.character)) # No categorical variable is character data type
summary(select_if(ASD_National, is.factor)) # All categorical variables are factor data type

# Count occurrences of uniques values in a variable/column
table(ASD_National$Source)
table(ASD_National$Source_Full1)
table(ASD_National$Source_Full2)
table(ASD_National$Year) # numeric is also possible
table(ASD_National$Prevalence) # numeric is also possible

# Display unique values (levels) of a factor categrotical 
lapply(ASD_National[c(1, 7, 8)], levels)
# or using variable names
lapply(ASD_National[c('Source', 'Source_Full1', 'Source_Full2')], levels)

# Pivot of counting occurrences
table(ASD_National$Source_Full2, ASD_National$Year) # table(ASD_National$Year, ASD_National$Source_Full2)


# ----------------------------------
# EDA - Visualization
# ----------------------------------

# Histogram
hist(ASD_National$Prevalence)
hist(ASD_National$Male.Prevalence)
hist(ASD_National$Female.Prevalence)
# Histogram with annotations
hist(ASD_National$Prevalence,
        main = "Distribution of National ASD Prevalence",
        xlab = "Prevalence per 1000 Children",
        ylab = "Frequency or Occurrences",
        sub  = "Year 2000 - 2016",
        col.main="blue", col.lab="black", col.sub="grey"
)

# Boxplot
boxplot(ASD_National$Male.Prevalence) # Male children
boxplot(ASD_National$Female.Prevalence) # Female children

par(mfrow=c(1, 2)) # multiple plots on one page: row split to: 1,column split to: 2
boxplot(ASD_National$Prevalence) # All children
# with 95% confidence interval
boxplot(ASD_National$Prevalence, notch = TRUE)
par(mfrow=c(1, 1)) # Reset to one plot on one page

# Display value ranges
range(ASD_National$Year)
range(ASD_National$Prevalence)

# Create 'Prevalence' box plots break by 'Source'
boxplot(ASD_National$Prevalence ~ ASD_National$Source,
        main = "National ASD Prevalence by Data Source",
        xlab = "Data Source",
        ylab = "Prevalence per 1000 Children",
        sub  = "Year 2000 - 2016",
        col.main="blue", col.lab="black", col.sub="grey"
        )


# ----------------------------------
# [National] Prevalence has changed over Time
# ----------------------------------
# Prevalence over Year
plot(ASD_National$Year, ASD_National$Prevalence)

table(ASD_National$Source_Full2)

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

# Prevalence over Year, from data source: 
# addm-Autism & Developmental Disabilities Monitoring Network
plot(ASD_National$Year[ASD_National$Source == 'addm'], 
     ASD_National$Prevalence[ASD_National$Source == 'addm'],
     type="l", # dot/point type
     lty=1, # line type
     lwd=3, # line width
     col="blue", # line color
     xlab="Year", 
     ylab="Prevalence per 1000 Children", 
     main="[addm] Prevalence Estimates Over Time",
     col.main="blue", col.lab="black", col.sub="grey"
     )

# Prevalence over Year, from data source: 
# medi-Medicaid
plot(ASD_National$Year[ASD_National$Source == 'medi'], 
     ASD_National$Prevalence[ASD_National$Source == 'medi'],
     type="b", lty=1, lwd=3,  col="orange",
     xlab="Year", 
     ylab="Prevalence per 1000 Children", 
     main="[medi] Prevalence Estimates Over Time",
     col.main="blue", col.lab="black", col.sub="grey"
     )

# Prevalence over Year, from data source: 
# nsch-National Survey of Children Health
plot(ASD_National$Year[ASD_National$Source == 'nsch'], 
     ASD_National$Prevalence[ASD_National$Source == 'nsch'],
     type="l", lty=2, lwd=3,  col="purple",
     xlab="Year", 
     ylab="Prevalence per 1000 Children", 
     main="[nsch] Prevalence Estimates Over Time",
     col.main="blue", col.lab="black", col.sub="grey"
     )

# Prevalence over Year, from data source: 
# sped-Special Education Child Count
plot(ASD_National$Year[ASD_National$Source == 'sped'], 
     ASD_National$Prevalence[ASD_National$Source == 'sped'],
     type="l", lty=3, lwd=3,  col="green",
     xlab="Year", 
     ylab="Prevalence per 1000 Children", 
     main="[sped] Prevalence Estimates Over Time",
     col.main="blue", col.lab="black", col.sub="grey"
     )

par(mfrow=c(1, 1)) # Reset to one plot on one page


# ----------------------------------
# Create a plot/chart with multiple lines
# ----------------------------------

# ----------------------------------
# [National] Prevalence Varies over Time/Year by Data Source
# ----------------------------------
# Create a first line
plot(ASD_National$Year[ASD_National$Source == 'addm'], 
     ASD_National$Prevalence[ASD_National$Source == 'addm'], 
     col = "blue", lty = 1, lwd = 1,
     type = "b", # use dot/point
     pch = 0, # dot/point type: http://www.endmemo.com/program/R/pchsymbols.php
     xlab="Year", 
     xlim=c(2000, 2016), # Set x axis value range
     ylab="Prevalence per 1000 Children", 
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
     pch = 1, col = "orange", type = "b", lty = 1, lwd = 1
     )
# Add another line
lines(ASD_National$Year[ASD_National$Source == 'nsch'], 
      ASD_National$Prevalence[ASD_National$Source == 'nsch'], 
      pch = 2, col = "purple", type = "b", lty = 1, lwd = 1
      )
# Add another line
lines(ASD_National$Year[ASD_National$Source == 'sped'], 
      ASD_National$Prevalence[ASD_National$Source == 'sped'], 
      pch = 5, col = "green", type = "b", lty = 1, lwd = 1
      )
# Add a legend to the plot
legend("topleft", legend=levels(ASD_National$Source),
       col=c("blue", "orange", "purple", "green"), 
       lty = 1, # line type
       lwd = 2, # line width
       cex=0.8, # size of text
       bty = 'n' # Without frame
       )

# ----------------------------------
# [addm] Prevalence Varies by Sex
# ----------------------------------
# Create a first line
plot(ASD_National$Year[ASD_National$Source == 'addm'], 
     ASD_National$Prevalence[ASD_National$Source == 'addm'], 
     col = "grey", lty = 1, lwd = 2,
     type = "l", # use dot/point
     pch = 0, # dot/point type: http://www.endmemo.com/program/R/pchsymbols.php
     xlab="Year", 
     xlim=c(2000, 2016), # Set x axis value range
     ylab="Prevalence per 1000 Children", 
     ylim=c(0, 30), # Set y axis value range
     main="Prevalence Estimates by Sex [ADDM]",
     col.main="black", col.lab="black", col.sub="grey",
     frame = FALSE, # Remove frame
     axes=FALSE # Remove x and y axis
)
axis(1, at=seq(2000, 2016, 1)) # Customize x axis
axis(2, at=seq(0, 30, 5)) # Customize y axis

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

# Add a legend to the plot
legend("topleft", legend=c('ADDM Average', 'Male with 95% CI', 'Female with 95% CI'),
       col=c("grey", "blue", "orange"), 
       lty = 1, # line type
       lwd = 2, # line width
       cex=0.8, # size of text
       bty = 'n' # Without frame
       )

# ----------------------------------
# [addm] Prevalence Varies by Race and Ethnicity
# ----------------------------------
# Create a first line
plot(ASD_National$Year[ASD_National$Source == 'addm'], 
     ASD_National$Prevalence[ASD_National$Source == 'addm'], 
     col = "grey", lty = 1, lwd = 2,
     type = "l", # use dot/point
     pch = 0, # dot/point type: http://www.endmemo.com/program/R/pchsymbols.php
     xlab="Year", 
     xlim=c(2000, 2016), # Set x axis value range
     ylab="Prevalence per 1000 Children", 
     ylim=c(0, 20), # Set y axis value range
     main="Prevalence Estimates by Race/Ethnicity [ADDM]",
     col.main="black", col.lab="black", col.sub="grey",
     frame = FALSE, # Remove frame
     axes=FALSE # Remove x and y axis
)
axis(1, at=seq(2000, 2016, 1)) # Customize x axis
axis(2, at=seq(0, 20, 5)) # Customize y axis

# R plot colour list: https://www.r-graph-gallery.com/42-colors-names.html

# Add Non.hispanic.white.Prevalence
lines(ASD_National$Year[ASD_National$Source == 'addm'], 
      ASD_National$Non.hispanic.white.Prevalence[ASD_National$Source == 'addm'], 
      pch = 20, col = "chartreuse3", type = "b", lty = 1, lwd = 2)
# Add Non.hispanic.black.Prevalence
lines(ASD_National$Year[ASD_National$Source == 'addm'], 
      ASD_National$Non.hispanic.black.Prevalence[ASD_National$Source == 'addm'], 
      pch = 20, col = "deepskyblue3", type = "b", lty = 1, lwd = 2)
# Add Hispanic.Prevalence
lines(ASD_National$Year[ASD_National$Source == 'addm'], 
      ASD_National$Hispanic.Prevalence[ASD_National$Source == 'addm'], 
      pch = 20, col = "darkorchid3", type = "b", lty = 1, lwd = 2)
# Add Asian.or.Pacific.Islander.Prevalence
lines(ASD_National$Year[ASD_National$Source == 'addm'], 
      ASD_National$Asian.or.Pacific.Islander.Prevalence[ASD_National$Source == 'addm'], 
      pch = 20, col = "darkred", type = "b", lty = 1, lwd = 2)

# Add a legend to the plot
legend("topleft", legend=c('ADDM Average', 
                           'Non-Hispanic White',
                           'Non-Hispanic Black',
                           'Hispanic', 
                           'Asian/Pacific Islander'),
       col=c("grey", "chartreuse3", "deepskyblue3", "darkorchid3", "darkred"), 
       lty = 1, # line type
       lwd = 2, # line width
       cex=0.8, # size of text
       bty = 'n' # Without frame
       )


# Quiz: Add 95% Confidence Interval to above plot


# count occurrences of catergorical variables
table(ASD_National$Source)
barplot(table(ASD_National$Source))

# Which data sources are available in which years
table(ASD_National$Year, ASD_National$Source)
plot(table(ASD_National$Year, ASD_National$Source))

# Which data source has breakdown data by sex/gender
table(ASD_National$Source_Full2, ASD_National$Male.Prevalence)
plot(table(ASD_National$Source_Full2, ASD_National$Male.Prevalence))

# Which data source has breakdown data by race and ethnicity
table(ASD_National$Source_Full2, ASD_National$Asian.or.Pacific.Islander.Prevalence)
plot(table(ASD_National$Source_Full2, ASD_National$Asian.or.Pacific.Islander.Prevalence))


# 2019 12 04
# ----------------------------------
# EDA - Nicer Visualization with ggplot2
# ----------------------------------
library(ggplot2)


#


# 2019 12 02

# ----------------------------------
# Hypothesis Test - Proportion Test
# ----------------------------------

?prop.test

# State	Denominator	Prevalence	Lower CI	Upper CI	Year	Source	State_Full	Numerator_Prevalence
# AZ	45,322	6.5	5.8	7.3	2000	addm	Arizona	295
# GA	43,593	6.5	5.8	7.3	2000	addm	Georgia	283
# MD	21,532	5.5	4.6	6.6	2000	addm	Maryland	118
# NJ	29,714	9.9	8.9	11.1	2000	addm	New Jersey	294


# Confidence Interval
ASD  <- c( 283 )
Children <- c( 43593 )
prop.test(ASD, Children)
prop.test(ASD, Children, correct = F)

ASD  <- c( 118 )
Children <- c( 21532 )
prop.test(ASD, Children)
prop.test(ASD, Children, correct = F)

ASD  <- c( 294 )
Children <- c( 29714 )
prop.test(ASD, Children)
prop.test(ASD, Children, correct = F)


ASD  <- c( 295, 283 ) 
Children <- c( 45322, 43593 )
prop.test(ASD, Children)
prop.test(ASD, Children, correct = F)

ASD  <- c( 295, 283, 118, 294 ) 
Children <- c( 45322, 43593, 21532, 29714 )
prop.test(ASD, Children)
prop.test(ASD, Children, correct = F)


ASD  <- c( 295 )
Children <- c( 45322 )
prop.test(ASD, Children)
prop.test(ASD, Children, correct = FALSE)

# Different flavours of proportion test:
if(!require(binom)){install.packages("binom")}
library(binom)
binom.confint (x=295, n=45322, conf.level =0.95, method="all")

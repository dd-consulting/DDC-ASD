###########################################################
# Study of Autism Spectrum Disorder (ASD)
# https://www.cdc.gov/ncbddd/autism/data/index.html#explore
# https://www.cdc.gov/ncbddd/autism/index.html
###########################################################

# 2019 12 03
getwd()

# ----------------------------------
# Read in data
# ----------------------------------
ASD_National <- read.csv("../dataset/ADV_ASD_National.csv", stringsAsFactors = F)
ASD_State <- read.csv("../dataset/ADV_ASD_State.csv", stringsAsFactors = F)

# Look at data stucture/schema
dim(ASD_National)
dim(ASD_State)
str(ASD_National)
str(ASD_State)

# Look at data stucture/schema (Selected columns)
str(ASD_National[, c(1:8, 24, 25, 26)])

# Obtain feature/column names of dataframe
names(ASD_National)

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
# or
ASD_National[, "Source"][1]
ASD_National[, "Source"][11]
ASD_National[, "Source"][11:20]
# or
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
ASD_National[c(1:10, 20, 30:35), c(1:3, 9, 12)] # row 1 to 10, 20, and 20 to 25 from column 1 to 3, 9, and 12
# Tips: we noticed missing data from above.


# ----------------------------------
# Process missing data
# ----------------------------------

# Count missing values in dataframe:
sum(is.na(ASD_National))
sum(is.na(ASD_State))


# Define several offending strings
na_strings <- c("", "No data", "NA", "N A", "N / A", "N/A", "N/ A", "Not Available", "NOt available")
# Load required function from packages:
library(dplyr)
library(naniar)
# Replace these defined missing values to R's internal NA
ASD_National = ASD_National %>%
  replace_with_na_all(condition = ~.x %in% na_strings)

# Count missing values in dataframe:
sum(is.na(ASD_National))

# Convert Prevalence and CIs from categorical to numeric, column 9 to 26
names(ASD_National)
ix <- 9:26 # define an index
ASD_National[ix] <- lapply(ASD_National[ix], as.double)

# Optionally, below is manual conversion:
# ASD_National$Male.Prevalence = as.double(ASD_National$Male.Prevalence)
# ASD_National$Male.Lower.CI = as.double(ASD_National$Male.Lower.CI)
# ASD_National$Male.Upper.CI = as.double(ASD_National$Male.Upper.CI)

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

# Addtionally, we need to remove invalid unicode char/string: \x92
ASD_National$Source_Full1[ASD_National$Source_Full1 == "National Survey of Children\x92s Health"] <- "National Survey of Children Health"
ASD_National$Source_Full2[ASD_National$Source_Full2 == "nsch-National Survey of Children\x92s Health"] <- "nsch-National Survey of Children Health"
table(ASD_National$Source_Full2) # table(ASD_National$Source_Full1)

# ----------------------------------
# EDA - Descriptive Analysis
# ----------------------------------

# Look at summary of numeric variables
summary(ASD_National)

# Look at summary of categorical variables
str(ASD_National)
# Source
# Source_Full1
# Source_Full2
# Year
table(ASD_National$Source)
table(ASD_National$Source_Full1)
table(ASD_National$Source_Full2)
table(ASD_National$Year) # numeric is also possible
table(ASD_National$Prevalence) # numeric is also possible

# Pivot of counting occurrences
table(ASD_National$Source_Full2, ASD_National$Year) # table(ASD_National$Year, ASD_National$Source_Full2)


# ----------------------------------
# EDA - Visualization
# ----------------------------------

# Histogram
hist(ASD_National$Prevalence)
hist(ASD_National$Male.Prevalence)
hist(ASD_National$Female.Prevalence)

# Boxplot
boxplot(ASD_National$Prevalence)
boxplot(ASD_National$Male.Prevalence)
boxplot(ASD_National$Female.Prevalence)

# with 95% confidence interval
boxplot(ASD_National$Prevalence, notch = TRUE)

# Prevalence over Year
plot(ASD_National$Year, ASD_National$Prevalence)

table(ASD_National$Source_Full2)
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

# count occurrences of catergorical variables
table(ASD_National$Source)
table(ASD_National$Year)
table(ASD_National$Male.Prevalence)

barplot(table(ASD_National$Source))

# Which data sources are available in which years
table(ASD_National$Year, ASD_National$Source)
plot(table(ASD_National$Year, ASD_National$Source))

# Which data source has breakdown data by gender, race and ethnicity
table(ASD_National$Source_Full2, ASD_National$Male.Prevalence)
plot(table(ASD_National$Source_Full2, ASD_National$Male.Prevalence))

table(ASD_National$Source_Full2, ASD_National$Asian.or.Pacific.Islander.Prevalence)
plot(table(ASD_National$Source_Full2, ASD_National$Asian.or.Pacific.Islander.Prevalence))

# Plot by year
plot(ASD_National$Year, ASD_National$Male.Prevalence)
plot(ASD_National$Year, ASD_National$Female.Prevalence)
plot(ASD_National$Year, ASD_National$Asian.or.Pacific.Islander.Prevalence)



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

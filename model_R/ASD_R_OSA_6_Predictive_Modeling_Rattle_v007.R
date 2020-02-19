# -*- coding: utf-8 -*-
# ---
# jupyter:
#   jupytext:
#     formats: ipynb,Rmd
#     text_representation:
#       extension: .R
#       format_name: light
#       format_version: '1.5'
#       jupytext_version: 1.3.0
#   kernelspec:
#     display_name: R
#     language: R
#     name: ir
# ---

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <a href="https://github.com/dd-consulting">
#          <img src="../reference/GZ_logo.png" width="60" align="right">
#     </a>
#     <h1>
#         One-Stop Analytics: Predictive Modeling (Rattle)
#     </h1>
# </div>
#

# # Case Study of Autism Spectrum Disorder (ASD) with R
#
# ---
#
# <img src="../reference/CDC_ASD/CDC_ASD_01.jpg" align="left">
#
# <img src="../reference/CDC_ASD/CDC_ASD_02.png" width="700" align="center">
#

# ## <span style="color:blue">[ United States ]</span> 
#
# ## Centers for Disease Control and Prevention (CDC) - Autism Spectrum Disorder (ASD)
#
# Autism spectrum disorder (ASD) is a developmental disability that can cause significant social, communication and behavioral challenges. CDC is committed to continuing to provide essential data on ASD, search for factors that put children at risk for ASD and possible causes, and develop resources that help identify children with ASD as early as possible.
#
# https://www.cdc.gov/ncbddd/autism/data/index.html
#

# ## <span style="color:blue">[ Singapore ]</span> 
#
# ## TODAY Online - More preschoolers diagnosed with developmental issues
#
# Doctors cited better awareness among parents and preschool teachers, leading to early referrals for diagnosis.
#
# https://www.gov.sg/news/content/today-online-more-preschoolers-diagnosed-with-developmental-issues
#
# <img src="../reference/SG_ASD/SG_ASD_01.png" width="650" align="left">
#
#

# <img src="../reference/SG_ASD/SG_ASD_04.png" align="left"> 
#
# https://www.pathlight.org.sg/

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <a href="">
#          <img src="" width="60" align="right">
#     </a>
# </div>
#

# # Workshop Objective: 
#
# ## Use R to predict Autism Spectrum Disorder (ASD) prevalence. 
#
# https://www.cdc.gov/ncbddd/autism/data/index.html
#
# * ## Rattle
#
# * ## Rattle: Import Data
#
# * ## Rattle: EDA Explore & Test
#
# * ## Rattle: Process & Transform Data
#
#     <span style="color:blue">Predict Prevelance Risk Levels (Classification)</span>
#
# * ## Rattle: Train Model (Classification)
#
#     * ### Multi-Class Model: Decision Tree (DT)
#
#     * ### Multi-Class Model: Random Forest (RF)
#
#     * ### Multi-Class Model: Support Vector Machines (SVM)
#
#     * ### Multi-Class Model: Multinomial Logistic Regression (MLR)
#     
#     * ### Binary-Class Model: Boost (AdaBoost & XgBoost)
#
#     * ### Binary-Class Model: Neural Net (NN)
#     
# * ## Rattle: Evaluate Model (Classification)
#
#     <span style="color:blue">Predict Prevelance (Regression)</span>
#
# * ## Rattle: Train Model (Regression)
#
#     * ### Regression Model: Decision Tree (DT)
#
#     * ### Regression Model: Random Forest (RF)
#
#     * ### Regression Model: Linear Regression (LR)
#
#     * ### Regressions Model: Neural Net (NN)
#
# * ## Rattle: Evaluate Model (Regression)
#
# * ## Rattle: Improve Model
#
# * ## Rattle: Save Model & Log
#
# * ## Workshop Submission
#
# * ## Appendices

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <a href="">
#          <img src="" width="750" align="center">
#     </a>
# </div>
#
#

library("repr") # Show graphs in-line notebook

# **Obtain current R <span style="color:blue">working directory</span>**

getwd()

# **Set new R working directory**

# setwd("/media/sf_vm_shared_folder/git/DDC/DDC-ASD/model_R")
# setwd('~/Desktop/admin-desktop/vm_shared_folder/git/DDC-ASD/model_R')
getwd()

# Adjust in-line plot size to M x N
options(repr.plot.width=8, repr.plot.height=5)

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <a href="">
#          <img src="" width="750" align="center">
#     </a>
# </div>
#
#

# ## <span style="color:blue">Rattle</span>
#

# **Rattle** — the **R** **A**nalytical **T**ool **T**o **L**earn **E**asily — is a popular open-source GUI for data mining using R. 
#
# https://rattle.togaware.com/
#
# https://journal.r-project.org/archive/2009-2/RJournal_2009-2_Williams.pdf

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Rattle
#     </h3>
# </div>
#

if(!require(rattle)){install.packages("rattle")}
library('rattle')

# <img src="../reference/R rattle/000 Rattle/a001.png" align="left"> 
#

# +
#=======================================================================

# Rattle is Copyright (c) 2006-2018 Togaware Pty Ltd.
# It is free (as in libre) open source software.
# It is licensed under the GNU General Public License,
# Version 2. Rattle comes with ABSOLUTELY NO WARRANTY.
# Rattle was written by Graham Williams with contributions
# from others as acknowledged in 'library(help=rattle)'.
# Visit https://rattle.togaware.com/ for details.

#=======================================================================
# Rattle timestamp: 2019-12-23 09:42:23 x86_64-pc-linux-gnu 

# Rattle version 5.3.0 user 'iss-user'

# This log captures interactions with Rattle as an R script. 

# For repeatability, export this activity log to a 
# file, like 'model.R' using the Export button or 
# through the Tools menu. Th script can then serve as a 
# starting point for developing your own scripts. 
# After xporting to a file called 'model.R', for exmample, 
# you can type into a new R Console the command 
# "source('model.R')" and so repeat all actions. Generally, 
# you will want to edit the file to suit your own needs. 
# You can also edit this log in place to record additional 
# information before exporting the script. 
 
# Note that saving/loading projects retains this log.

# We begin most scripts by loading the required packages.
# Here are some initial packages to load and others will be
# identified as we proceed through the script. When writing
# our own scripts we often collect together the library
# commands at the beginning of the script here.

library(rattle)   # Access the weather dataset and utilities.
library(magrittr) # Utilise %>% and %<>% pipeline operators.

# This log generally records the process of building a model. 
# However, with very little effort the log can also be used 
# to score a new dataset. The logical variable 'building' 
# is used to toggle between generating transformations, 
# when building a model and using the transformations, 
# when scoring a dataset.

building <- TRUE
scoring  <- ! building

# A pre-defined value is used to reset the random seed 
# so that results are repeatable.

crv$seed <- 88
set.seed(88)
# -

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <a href="">
#          <img src="" width="750" align="center">
#     </a>
# </div>
#
#

# ## <span style="color:blue">Rattle: Import Data</span>
#

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Rattle: Import Data
#     </h3>
# </div>
#

# **Use Case Data: <span style="color:blue">"../dataset/ADV_ASD_State_R.csv"</span>**

# <img src="../reference/R rattle/010 Import Data/a001.png" align="left"> 
#

# +


#=======================================================================
# Rattle timestamp: 2019-12-23 10:01:39 x86_64-pc-linux-gnu 

# Load a dataset from file.

fname <- "../dataset/ADV_ASD_State_R.csv" 
crs$dataset <- read.csv(fname, na.strings=c(".", "NA", "", "?"), strip.white=TRUE, encoding="UTF-8")

#=======================================================================
# Rattle timestamp: 2019-12-23 10:01:40 x86_64-pc-linux-gnu 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=1692 train=1184 validate=254 test=254

set.seed(crv$seed)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
crs$test

# The following variable selections have been noted.

crs$input     <- c("State", "Denominator", "Prevalence",
                   "Lower.CI", "Upper.CI", "Year", "Source_Full1",
                   "State_Full1", "State_Full2", "Numerator_ASD",
                   "Numerator_NonASD", "Proportion",
                   "Chi_Wilson_Corrected_Lower.CI",
                   "Chi_Wilson_Corrected_Upper.CI",
                   "Male.Prevalence", "Male.Lower.CI",
                   "Male.Upper.CI", "Female.Prevalence",
                   "Female.Lower.CI", "Female.Upper.CI",
                   "Non.hispanic.white.Prevalence",
                   "Non.hispanic.white.Lower.CI",
                   "Non.hispanic.white.Upper.CI",
                   "Non.hispanic.black.Prevalence",
                   "Non.hispanic.black.Lower.CI",
                   "Non.hispanic.black.Upper.CI",
                   "Hispanic.Prevalence", "Hispanic.Lower.CI",
                   "Hispanic.Upper.CI",
                   "Asian.or.Pacific.Islander.Prevalence",
                   "Asian.or.Pacific.Islander.Lower.CI",
                   "Asian.or.Pacific.Islander.Upper.CI", "Source_UC",
                   "Source_Full3", "Prevalence_Risk2",
                   "Prevalence_Risk4", "Year_Factor")

crs$numeric   <- c("Denominator", "Prevalence", "Lower.CI",
                   "Upper.CI", "Year", "Numerator_ASD",
                   "Numerator_NonASD", "Proportion",
                   "Chi_Wilson_Corrected_Lower.CI",
                   "Chi_Wilson_Corrected_Upper.CI",
                   "Male.Prevalence", "Male.Lower.CI",
                   "Male.Upper.CI", "Female.Prevalence",
                   "Female.Lower.CI", "Female.Upper.CI",
                   "Non.hispanic.white.Prevalence",
                   "Non.hispanic.white.Lower.CI",
                   "Non.hispanic.white.Upper.CI",
                   "Non.hispanic.black.Prevalence",
                   "Non.hispanic.black.Lower.CI",
                   "Non.hispanic.black.Upper.CI",
                   "Hispanic.Prevalence", "Hispanic.Lower.CI",
                   "Hispanic.Upper.CI",
                   "Asian.or.Pacific.Islander.Prevalence",
                   "Asian.or.Pacific.Islander.Lower.CI",
                   "Asian.or.Pacific.Islander.Upper.CI",
                   "Year_Factor")

crs$categoric <- c("State", "Source_Full1", "State_Full1",
                   "State_Full2", "Source_UC", "Source_Full3",
                   "Prevalence_Risk2", "Prevalence_Risk4")

crs$target    <- "Source"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL
# -

# <img src="../reference/R rattle/010 Import Data/a002.png" align="left"> 
#

# +


#=======================================================================
# Rattle timestamp: 2019-12-23 10:05:28 x86_64-pc-linux-gnu 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=1692 train=1184 validate=254 test=254

set.seed(88)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
crs$test

# The following variable selections have been noted.

crs$input     <- c("State", "Denominator", "Prevalence",
                   "Lower.CI", "Upper.CI", "Year", "Source",
                   "Source_Full1", "State_Full1", "State_Full2",
                   "Numerator_ASD", "Numerator_NonASD", "Proportion",
                   "Chi_Wilson_Corrected_Lower.CI",
                   "Chi_Wilson_Corrected_Upper.CI",
                   "Male.Prevalence", "Male.Lower.CI",
                   "Male.Upper.CI", "Female.Prevalence",
                   "Female.Lower.CI", "Female.Upper.CI",
                   "Non.hispanic.white.Prevalence",
                   "Non.hispanic.white.Lower.CI",
                   "Non.hispanic.white.Upper.CI",
                   "Non.hispanic.black.Prevalence",
                   "Non.hispanic.black.Lower.CI",
                   "Non.hispanic.black.Upper.CI",
                   "Hispanic.Prevalence", "Hispanic.Lower.CI",
                   "Hispanic.Upper.CI",
                   "Asian.or.Pacific.Islander.Prevalence",
                   "Asian.or.Pacific.Islander.Lower.CI",
                   "Asian.or.Pacific.Islander.Upper.CI", "Source_UC",
                   "Source_Full3", "Prevalence_Risk2", "Year_Factor")

crs$numeric   <- c("Denominator", "Prevalence", "Lower.CI",
                   "Upper.CI", "Year", "Numerator_ASD",
                   "Numerator_NonASD", "Proportion",
                   "Chi_Wilson_Corrected_Lower.CI",
                   "Chi_Wilson_Corrected_Upper.CI",
                   "Male.Prevalence", "Male.Lower.CI",
                   "Male.Upper.CI", "Female.Prevalence",
                   "Female.Lower.CI", "Female.Upper.CI",
                   "Non.hispanic.white.Prevalence",
                   "Non.hispanic.white.Lower.CI",
                   "Non.hispanic.white.Upper.CI",
                   "Non.hispanic.black.Prevalence",
                   "Non.hispanic.black.Lower.CI",
                   "Non.hispanic.black.Upper.CI",
                   "Hispanic.Prevalence", "Hispanic.Lower.CI",
                   "Hispanic.Upper.CI",
                   "Asian.or.Pacific.Islander.Prevalence",
                   "Asian.or.Pacific.Islander.Lower.CI",
                   "Asian.or.Pacific.Islander.Upper.CI",
                   "Year_Factor")

crs$categoric <- c("State", "Source", "Source_Full1",
                   "State_Full1", "State_Full2", "Source_UC",
                   "Source_Full3", "Prevalence_Risk2")

crs$target    <- "Prevalence_Risk4"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL
# -

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <a href="">
#          <img src="" width="750" align="center">
#     </a>
# </div>
#
#

# ## <span style="color:blue">Rattle: EDA Explore & Test</span>
#

if(!require(Hmisc)){install.packages("Hmisc")}
library('Hmisc')

if(!require(fBasics)){install.packages("fBasics")}
library('fBasics')

if(!require(mice)){install.packages("mice")}
library('mice')

if(!require(descr)){install.packages("descr")}
library('descr')

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Rattle: EDA Explore & Test: Summary
#     </h3>
# </div>
#

# <img src="../reference/R rattle/020 EDA Explore n Test/a003.png" align="left"> 
#

# +


#=======================================================================
# Rattle timestamp: 2019-12-23 10:12:32 x86_64-pc-linux-gnu 

# The 'Hmisc' package provides the 'contents' function.

library(Hmisc, quietly=TRUE)

# Obtain a summary of the dataset.

contents(crs$dataset[crs$train, c(crs$input, crs$risk, crs$target)])
summary(crs$dataset[crs$train, c(crs$input, crs$risk, crs$target)])

# The 'Hmisc' package provides the 'describe' function.

library(Hmisc, quietly=TRUE)

# Generate a description of the dataset.

describe(crs$dataset[crs$train, c(crs$input, crs$risk, crs$target)])

# The 'basicStats' package provides the 'fBasics' function.

library(fBasics, quietly=TRUE)

# Generate a description of the numeric data.

lapply(crs$dataset[crs$train, c(crs$input, crs$risk, crs$target)][,c(2:6, 11:33, 37)], basicStats)

# The 'kurtosis' package provides the 'fBasics' function.

library(fBasics, quietly=TRUE)

# Summarise the kurtosis of the numeric data.

kurtosis(crs$dataset[crs$train, c(crs$input, crs$risk, crs$target)][,c(2:6, 11:33, 37)], na.rm=TRUE)

# The 'skewness' package provides the 'fBasics' function.

library(fBasics, quietly=TRUE)

# Summarise the skewness of the numeric data.

skewness(crs$dataset[crs$train, c(crs$input, crs$risk, crs$target)][,c(2:6, 11:33, 37)], na.rm=TRUE)

# The 'mice' package provides the 'md.pattern' function.

library(mice, quietly=TRUE)

# Generate a summary of the missing values in the dataset.

md.pattern(crs$dataset[,c(crs$input, crs$target)])

# The 'CrossTable' package provides the 'descr' function.

library(descr, quietly=TRUE)

# Generate cross tabulations for categoric data.

for (i in c(1, 7:10, 34:36)) 
{ 
  cat(sprintf('CrossTab of %s by target variable %s\n\n', names(crs$dataset)[i], crs$target)) 
  print(CrossTable(crs$dataset[[i]], crs$dataset[[crs$target]], expected=TRUE, format='SAS')) 
  cat(paste(rep('=', 70), collapse=''), '

') 
}
# -

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Rattle: EDA Explore & Test: Distribution
#     </h3>
# </div>
#

# <img src="../reference/R rattle/020 EDA Explore n Test/a007.png" align="left"> 
#

# +


#=======================================================================
# Rattle timestamp: 2019-12-23 10:45:21 x86_64-pc-linux-gnu 

# Display box plots for the selected variables. 

# Use ggplot2 to generate box plot for Denominator

# Generate a box plot.

p01 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(Prevalence_Risk4=as.factor(Prevalence_Risk4)) %>%
  ggplot2::ggplot(ggplot2::aes(y=Denominator)) +
  ggplot2::geom_boxplot(ggplot2::aes(x="All"), notch=TRUE, fill="grey") +
  ggplot2::stat_summary(ggplot2::aes(x="All"), fun.y=mean, geom="point", shape=8) +
  ggplot2::geom_boxplot(ggplot2::aes(x=Prevalence_Risk4, fill=Prevalence_Risk4), notch=TRUE) +
  ggplot2::stat_summary(ggplot2::aes(x=Prevalence_Risk4), fun.y=mean, geom="point", shape=8) +
  ggplot2::xlab("Prevalence_Risk4\n\nRattle 2019-Dec-23 10:45:21 iss-user") +
  ggplot2::ggtitle("Distribution of Denominator (sample)\nby Prevalence_Risk4") +
  ggplot2::theme(legend.position="none")

# Use ggplot2 to generate box plot for Prevalence

# Generate a box plot.

p02 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(Prevalence_Risk4=as.factor(Prevalence_Risk4)) %>%
  ggplot2::ggplot(ggplot2::aes(y=Prevalence)) +
  ggplot2::geom_boxplot(ggplot2::aes(x="All"), notch=TRUE, fill="grey") +
  ggplot2::stat_summary(ggplot2::aes(x="All"), fun.y=mean, geom="point", shape=8) +
  ggplot2::geom_boxplot(ggplot2::aes(x=Prevalence_Risk4, fill=Prevalence_Risk4), notch=TRUE) +
  ggplot2::stat_summary(ggplot2::aes(x=Prevalence_Risk4), fun.y=mean, geom="point", shape=8) +
  ggplot2::xlab("Prevalence_Risk4\n\nRattle 2019-Dec-23 10:45:21 iss-user") +
  ggplot2::ggtitle("Distribution of Prevalence (sample)\nby Prevalence_Risk4") +
  ggplot2::theme(legend.position="none")

# Use ggplot2 to generate box plot for Lower.CI

# Generate a box plot.

p03 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(Prevalence_Risk4=as.factor(Prevalence_Risk4)) %>%
  ggplot2::ggplot(ggplot2::aes(y=Lower.CI)) +
  ggplot2::geom_boxplot(ggplot2::aes(x="All"), notch=TRUE, fill="grey") +
  ggplot2::stat_summary(ggplot2::aes(x="All"), fun.y=mean, geom="point", shape=8) +
  ggplot2::geom_boxplot(ggplot2::aes(x=Prevalence_Risk4, fill=Prevalence_Risk4), notch=TRUE) +
  ggplot2::stat_summary(ggplot2::aes(x=Prevalence_Risk4), fun.y=mean, geom="point", shape=8) +
  ggplot2::xlab("Prevalence_Risk4\n\nRattle 2019-Dec-23 10:45:21 iss-user") +
  ggplot2::ggtitle("Distribution of Lower.CI (sample)\nby Prevalence_Risk4") +
  ggplot2::theme(legend.position="none")

# Use ggplot2 to generate box plot for Upper.CI

# Generate a box plot.

p04 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(Prevalence_Risk4=as.factor(Prevalence_Risk4)) %>%
  ggplot2::ggplot(ggplot2::aes(y=Upper.CI)) +
  ggplot2::geom_boxplot(ggplot2::aes(x="All"), notch=TRUE, fill="grey") +
  ggplot2::stat_summary(ggplot2::aes(x="All"), fun.y=mean, geom="point", shape=8) +
  ggplot2::geom_boxplot(ggplot2::aes(x=Prevalence_Risk4, fill=Prevalence_Risk4), notch=TRUE) +
  ggplot2::stat_summary(ggplot2::aes(x=Prevalence_Risk4), fun.y=mean, geom="point", shape=8) +
  ggplot2::xlab("Prevalence_Risk4\n\nRattle 2019-Dec-23 10:45:22 iss-user") +
  ggplot2::ggtitle("Distribution of Upper.CI (sample)\nby Prevalence_Risk4") +
  ggplot2::theme(legend.position="none")

# Display the plots.

gridExtra::grid.arrange(p01, p02, p03, p04)
# -

# ---

# <img src="../reference/R rattle/020 EDA Explore n Test/a008.png" align="left"> 
#

# +


#=======================================================================
# Rattle timestamp: 2019-12-23 10:43:09 x86_64-pc-linux-gnu 

# Display histogram plots for the selected variables. 

# Use ggplot2 to generate histogram plot for Denominator

# Generate the plot.

p01 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(Prevalence_Risk4=as.factor(Prevalence_Risk4)) %>%
  dplyr::select(Denominator, Prevalence_Risk4) %>%
  ggplot2::ggplot(ggplot2::aes(x=Denominator)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(fill=Prevalence_Risk4, colour=Prevalence_Risk4), alpha=0.55) +
  ggplot2::xlab("Denominator\n\nRattle 2019-Dec-23 10:43:09 iss-user") +
  ggplot2::ggtitle("Distribution of Denominator (sample)\nby Prevalence_Risk4") +
  ggplot2::labs(fill="Prevalence_Risk4", y="Density")

# Use ggplot2 to generate histogram plot for Prevalence

# Generate the plot.

p02 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(Prevalence_Risk4=as.factor(Prevalence_Risk4)) %>%
  dplyr::select(Prevalence, Prevalence_Risk4) %>%
  ggplot2::ggplot(ggplot2::aes(x=Prevalence)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(fill=Prevalence_Risk4, colour=Prevalence_Risk4), alpha=0.55) +
  ggplot2::xlab("Prevalence\n\nRattle 2019-Dec-23 10:43:09 iss-user") +
  ggplot2::ggtitle("Distribution of Prevalence (sample)\nby Prevalence_Risk4") +
  ggplot2::labs(fill="Prevalence_Risk4", y="Density")

# Use ggplot2 to generate histogram plot for Lower.CI

# Generate the plot.

p03 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(Prevalence_Risk4=as.factor(Prevalence_Risk4)) %>%
  dplyr::select(Lower.CI, Prevalence_Risk4) %>%
  ggplot2::ggplot(ggplot2::aes(x=Lower.CI)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(fill=Prevalence_Risk4, colour=Prevalence_Risk4), alpha=0.55) +
  ggplot2::xlab("Lower.CI\n\nRattle 2019-Dec-23 10:43:09 iss-user") +
  ggplot2::ggtitle("Distribution of Lower.CI (sample)\nby Prevalence_Risk4") +
  ggplot2::labs(fill="Prevalence_Risk4", y="Density")

# Use ggplot2 to generate histogram plot for Upper.CI

# Generate the plot.

p04 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(Prevalence_Risk4=as.factor(Prevalence_Risk4)) %>%
  dplyr::select(Upper.CI, Prevalence_Risk4) %>%
  ggplot2::ggplot(ggplot2::aes(x=Upper.CI)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(fill=Prevalence_Risk4, colour=Prevalence_Risk4), alpha=0.55) +
  ggplot2::xlab("Upper.CI\n\nRattle 2019-Dec-23 10:43:09 iss-user") +
  ggplot2::ggtitle("Distribution of Upper.CI (sample)\nby Prevalence_Risk4") +
  ggplot2::labs(fill="Prevalence_Risk4", y="Density")

# Display the plots.

gridExtra::grid.arrange(p01, p02, p03, p04)
# -

# ---

# <img src="../reference/R rattle/020 EDA Explore n Test/a009.png" align="left"> 
#

# +


#=======================================================================
# Rattle timestamp: 2019-12-23 10:48:49 x86_64-pc-linux-gnu 

# Display histogram plots for the selected variables. 

# Use ggplot2 to generate histogram plot for Year

# Generate the plot.

p01 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(Prevalence_Risk4=as.factor(Prevalence_Risk4)) %>%
  dplyr::select(Year, Prevalence_Risk4) %>%
  ggplot2::ggplot(ggplot2::aes(x=Year)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(fill=Prevalence_Risk4, colour=Prevalence_Risk4), alpha=0.55) +
  ggplot2::xlab("Year\n\nRattle 2019-Dec-23 10:48:49 iss-user") +
  ggplot2::ggtitle("Distribution of Year (sample)\nby Prevalence_Risk4") +
  ggplot2::labs(fill="Prevalence_Risk4", y="Density")

# Display the plots.

gridExtra::grid.arrange(p01)
# -

# ---

if(!require(GGally)){install.packages("GGally")}
library('GGally')

# <img src="../reference/R rattle/020 EDA Explore n Test/a010.png" align="left"> 
#

# +


#=======================================================================
# Rattle timestamp: 2019-12-23 10:54:13 x86_64-pc-linux-gnu 

# Display a pairs plot for the selected variables. 

# Use GGally's ggpairs() to do the hard work.

crs$dataset[crs$train,] %>%
  dplyr::mutate(Prevalence_Risk4=as.factor(Prevalence_Risk4)) %>%
  GGally::ggpairs(columns=c(2,3,4,5),
        mapping=ggplot2::aes(colour=Prevalence_Risk4, alpha=0.5),
                diag=list(continuous="density",
                          discrete="bar"),
                upper=list(continuous="cor",
                           combo="box",
                           discrete="ratio"),
                lower=list(continuous="points",
                           combo="denstrip",
                           discrete="facetbar")) +
  ggplot2::theme(panel.grid.major=ggplot2::element_blank())
# -

# ---

# <img src="../reference/R rattle/020 EDA Explore n Test/a011.png" align="left"> 
#

# +


#=======================================================================
# Rattle timestamp: 2019-12-23 11:06:10 x86_64-pc-linux-gnu 

# Display a pairs plot for the selected variables. 

# Use GGally's ggpairs() to do the hard work.

crs$dataset[crs$train,] %>%
  dplyr::mutate(Prevalence_Risk4=as.factor(Prevalence_Risk4)) %>%
  GGally::ggpairs(columns=c(3,11,13),
        mapping=ggplot2::aes(colour=Prevalence_Risk4, alpha=0.5),
                diag=list(continuous="density",
                          discrete="bar"),
                upper=list(continuous="cor",
                           combo="box",
                           discrete="ratio"),
                lower=list(continuous="points",
                           combo="denstrip",
                           discrete="facetbar")) +
  ggplot2::theme(panel.grid.major=ggplot2::element_blank())
# -

# ---

# <img src="../reference/R rattle/020 EDA Explore n Test/a012.png" align="left"> 
#

# +


#=======================================================================
# Rattle timestamp: 2019-12-23 11:07:28 x86_64-pc-linux-gnu 

# Display a pairs plot for the selected variables. 

# Use GGally's ggpairs() to do the hard work.

crs$dataset[crs$train,] %>%
  dplyr::mutate(Prevalence_Risk4=as.factor(Prevalence_Risk4)) %>%
  GGally::ggpairs(columns=c(3,16,19),
        mapping=ggplot2::aes(colour=Prevalence_Risk4, alpha=0.5),
                diag=list(continuous="density",
                          discrete="bar"),
                upper=list(continuous="cor",
                           combo="box",
                           discrete="ratio"),
                lower=list(continuous="points",
                           combo="denstrip",
                           discrete="facetbar")) +
  ggplot2::theme(panel.grid.major=ggplot2::element_blank())
# -

# ---

# <img src="../reference/R rattle/020 EDA Explore n Test/a013.png" align="left"> 

# +


#=======================================================================
# Rattle timestamp: 2019-12-23 11:11:18 x86_64-pc-linux-gnu 

# Display a pairs plot for the selected variables. 

# Use GGally's ggpairs() to do the hard work.

crs$dataset[crs$train,] %>%
  dplyr::mutate(Prevalence_Risk4=as.factor(Prevalence_Risk4)) %>%
  GGally::ggpairs(columns=c(3,22,25,28,31),
        mapping=ggplot2::aes(colour=Prevalence_Risk4, alpha=0.5),
                diag=list(continuous="density",
                          discrete="bar"),
                upper=list(continuous="cor",
                           combo="box",
                           discrete="ratio"),
                lower=list(continuous="points",
                           combo="denstrip",
                           discrete="facetbar")) +
  ggplot2::theme(panel.grid.major=ggplot2::element_blank())
# -

# ---

# <img src="../reference/R rattle/020 EDA Explore n Test/a014.png" align="left"> 

# +


#=======================================================================
# Rattle timestamp: 2019-12-23 11:15:09 x86_64-pc-linux-gnu 

# Mosaic Plot 

# Generate the table data for plotting.

ds <- table(crs$dataset[crs$train,]$State, crs$dataset[crs$train,]$Prevalence_Risk4)

# Sort the entries.

ord <- order(apply(ds, 1, sum), decreasing=TRUE)

# Plot the data.

mosaicplot(ds[ord,], main="Mosaic of State (sample)
by Prevalence_Risk4", sub="Rattle 2019-Dec-23 11:15:09 iss-user", color=colorspace::rainbow_hcl(5)[-1], cex=0.7, xlab="State", ylab="Prevalence_Risk4")
# -

# ---

# <img src="../reference/R rattle/020 EDA Explore n Test/a014b.png" align="left"> 

# +


#=======================================================================
# Rattle timestamp: 2019-12-23 14:50:59 x86_64-pc-linux-gnu 

# Mosaic Plot 

# Generate the table data for plotting.

ds <- table(crs$dataset[crs$train,]$State_Region, crs$dataset[crs$train,]$Prevalence_Risk4)

# Sort the entries.

ord <- order(apply(ds, 1, sum), decreasing=TRUE)

# Plot the data.

mosaicplot(ds[ord,], main="Mosaic of State_Region (sample)
by Prevalence_Risk4", sub="Rattle 2019-Dec-23 14:50:59 iss-user", color=colorspace::rainbow_hcl(5)[-1], cex=0.7, xlab="State_Region", ylab="Prevalence_Risk4")
# -

# ---

if(!require(gplots)){install.packages("gplots")}
library('gplots')

# <img src="../reference/R rattle/020 EDA Explore n Test/a015.png" align="left"> 

# +


#=======================================================================
# Rattle timestamp: 2019-12-23 11:16:35 x86_64-pc-linux-gnu 

# The 'gplots' package provides the 'barplot2' function.

library(gplots, quietly=TRUE)

#=======================================================================
# Rattle timestamp: 2019-12-23 11:16:36 x86_64-pc-linux-gnu 

# Bar Plot 

# Generate the summary data for plotting.

ds <- rbind(summary(na.omit(crs$dataset[crs$train,]$Source)),
    summary(na.omit(crs$dataset[crs$train,][crs$dataset[crs$train,]$Prevalence_Risk4=="High",]$Source)),
    summary(na.omit(crs$dataset[crs$train,][crs$dataset[crs$train,]$Prevalence_Risk4=="Low",]$Source)),
    summary(na.omit(crs$dataset[crs$train,][crs$dataset[crs$train,]$Prevalence_Risk4=="Medium",]$Source)),
    summary(na.omit(crs$dataset[crs$train,][crs$dataset[crs$train,]$Prevalence_Risk4=="Very High",]$Source)))

# Sort the entries.

ord <- order(ds[1,], decreasing=TRUE)

# Plot the data.

bp <-  barplot2(ds[,ord], beside=TRUE, ylab="Frequency", xlab="Source", ylim=c(0, 715), col=colorspace::rainbow_hcl(5))

# Add the actual frequencies.

text(bp, ds[,ord]+24, ds[,ord])

# Add a legend to the plot.

legend("topright", bty="n", c("All","High","Low","Medium","Very High"),  fill=colorspace::rainbow_hcl(5))

# Add a title to the plot.

title(main="Distribution of Source (sample)\nby Prevalence_Risk4",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
# -

# ---

# <img src="../reference/R rattle/020 EDA Explore n Test/a016.png" align="left"> 

# +


#=======================================================================
# Rattle timestamp: 2019-12-23 11:21:08 x86_64-pc-linux-gnu 

# Display a pairs plot for the selected variables. 

# Use GGally's ggpairs() to do the hard work.

crs$dataset[crs$train,] %>%
  dplyr::mutate(Prevalence_Risk4=as.factor(Prevalence_Risk4)) %>%
  GGally::ggpairs(columns=c(7,37),
        mapping=ggplot2::aes(colour=Prevalence_Risk4, alpha=0.5),
                diag=list(continuous="density",
                          discrete="bar"),
                upper=list(continuous="cor",
                           combo="box",
                           discrete="ratio"),
                lower=list(continuous="points",
                           combo="denstrip",
                           discrete="facetbar")) +
  ggplot2::theme(panel.grid.major=ggplot2::element_blank())
# -

# ---

# <img src="../reference/R rattle/020 EDA Explore n Test/a017.png" align="left"> 

# +


#=======================================================================
# Rattle timestamp: 2019-12-23 11:25:27 x86_64-pc-linux-gnu 

# Display a pairs plot for the selected variables. 

# Use GGally's ggpairs() to do the hard work.

crs$dataset[crs$train,] %>%
  dplyr::mutate(Prevalence_Risk4=as.factor(Prevalence_Risk4)) %>%
  GGally::ggpairs(columns=c(3,7,37),
        mapping=ggplot2::aes(colour=Prevalence_Risk4, alpha=0.5),
                diag=list(continuous="density",
                          discrete="bar"),
                upper=list(continuous="cor",
                           combo="box",
                           discrete="ratio"),
                lower=list(continuous="points",
                           combo="denstrip",
                           discrete="facetbar")) +
  ggplot2::theme(panel.grid.major=ggplot2::element_blank())
# -

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Rattle: EDA Explore & Test: Correlation
#     </h3>
# </div>
#

# <img src="../reference/R rattle/020 EDA Explore n Test/a018.png" align="left"> 

# +


#=======================================================================
# Rattle timestamp: 2019-12-23 11:31:10 x86_64-pc-linux-gnu 

# Generate a correlation plot for the variables. 

# The 'corrplot' package provides the 'corrplot' function.

library(corrplot, quietly=TRUE)

# Correlations work for numeric variables only.

crs$cor <- cor(crs$dataset[crs$train, crs$numeric], use="pairwise", method="pearson")

# Order the correlations by their strength.

crs$ord <- order(crs$cor[1,])
crs$cor <- crs$cor[crs$ord, crs$ord]

# Display the actual correlations.

print(crs$cor)

# Graphically display the correlations.

corrplot(crs$cor, mar=c(0,0,1,0))
title(main="Correlation ADV_ASD_State_R.csv using Pearson",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
# -

# ---

# <img src="../reference/R rattle/020 EDA Explore n Test/a019.png" align="left"> 

# +


#=======================================================================
# Rattle timestamp: 2019-12-23 11:32:46 x86_64-pc-linux-gnu 

# Hierarchical Variable Correlation 

# Generate the correlations (numerics only).

cc <- cor(crs$dataset[crs$train, crs$numeric], use="pairwise", method="pearson")

# Generate hierarchical cluster of variables.

hc <- hclust(dist(cc), method="average")

# Generate the dendrogram.

dn <- as.dendrogram(hc)

# Now draw the dendrogram.

op <- par(mar = c(3, 4, 3, 10.86))
plot(dn, horiz = TRUE, nodePar = list(col = 3:2, cex = c(2.0, 0.75), pch = 21:22, bg=  c("light blue", "pink"), lab.cex = 0.75, lab.col = "tomato"), edgePar = list(col = "gray", lwd = 2), xlab="Height")
title(main="Variable Correlation Clusters
 ADV_ASD_State_R.csv using Pearson",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
par(op)

# -

# ---

# <img src="../reference/R rattle/020 EDA Explore n Test/a020.png" align="left"> 

# +


#=======================================================================
# Rattle timestamp: 2019-12-23 11:33:55 x86_64-pc-linux-gnu 

# Hierarchical Variable Correlation 

# Generate the correlations (numerics only).

cc <- cor(crs$dataset[crs$train, crs$numeric], use="pairwise", method="spearman")

# Generate hierarchical cluster of variables.

hc <- hclust(dist(cc), method="average")

# Generate the dendrogram.

dn <- as.dendrogram(hc)

# Now draw the dendrogram.

op <- par(mar = c(3, 4, 3, 10.86))
plot(dn, horiz = TRUE, nodePar = list(col = 3:2, cex = c(2.0, 0.75), pch = 21:22, bg=  c("light blue", "pink"), lab.cex = 0.75, lab.col = "tomato"), edgePar = list(col = "gray", lwd = 2), xlab="Height")
title(main="Variable Correlation Clusters
 ADV_ASD_State_R.csv using Spearman",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
par(op)

# -

# ---

# <img src="../reference/R rattle/020 EDA Explore n Test/a021.png" align="left"> 

# +


#=======================================================================
# Rattle timestamp: 2019-12-23 11:36:12 x86_64-pc-linux-gnu 

# Principal Components Analysis (on numerics only).

pc <- prcomp(na.omit(crs$dataset[crs$train, crs$numeric]), scale=TRUE, center=TRUE, tol=0)

# Show the output of the analysis.

pc

# Summarise the importance of the components found.

summary(pc)

# Display a plot showing the relative importance of the components.

plot(pc, main="")
title(main="Principal Components Importance ADV_ASD_State_R.csv",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
axis(1, at=seq(0.7, ncol(pc$rotation)*1.2, 1.2), labels=colnames(pc$rotation), lty=0)

# Display a plot showing the two most principal components.

biplot(pc, main="")
title(main="Principal Components ADV_ASD_State_R.csv",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
# -

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Rattle: EDA Explore & Test: Test
#     </h3>
# </div>
#

# <img src="../reference/R rattle/020 EDA Explore n Test/a024.png" align="left"> 

# +


#=======================================================================
# Rattle timestamp: 2019-12-23 11:42:02 x86_64-pc-linux-gnu 

# Perform Test 

# Use the fBasics package for statistical tests.

library(fBasics, quietly=TRUE)

# Perform the test.

locationTest(na.omit(crs$dataset[, "Male.Prevalence"]), na.omit(crs$dataset[, "Female.Prevalence"]))
# -

# ---

# <img src="../reference/R rattle/020 EDA Explore n Test/a025.png" align="left"> 

# +


#=======================================================================
# Rattle timestamp: 2019-12-23 11:46:39 x86_64-pc-linux-gnu 

# Perform Test 

# Use the fBasics package for statistical tests.

library(fBasics, quietly=TRUE)

# Perform the test.

locationTest(na.omit(crs$dataset[, "Prevalence"]), na.omit(crs$dataset[, "Hispanic.Prevalence"]))
# -

# ---

# <img src="../reference/R rattle/020 EDA Explore n Test/a026.png" align="left"> 

# +


#=======================================================================
# Rattle timestamp: 2019-12-23 11:47:53 x86_64-pc-linux-gnu 

# Perform Test 

# Use the fBasics package for statistical tests.

library(fBasics, quietly=TRUE)

# Perform the test.

locationTest(na.omit(crs$dataset[crs$dataset[["Prevalence_Risk4"]] == "High", "Prevalence"]), na.omit(crs$dataset[crs$dataset[["Prevalence_Risk4"]] == "Low", "Prevalence"]))
# -

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <a href="">
#          <img src="" width="750" align="center">
#     </a>
# </div>
#
#

# ## <span style="color:blue">Rattle: Process & Transform Data</span>
#

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Rattle: Process & Transform Data: Select Variables
#     </h3>
# </div>
#

names(crs$dataset)

# **Select variables to build classification model**
#
# Target: 
#
# * Prevalence_Risk4
#
# Inputs:
#
# * Denominator
#
# * Year
#
# * Source
#
# * State_Region
#
#
# Note: It's not recommended ot use **State**, which has more than 32 levels.
#
# https://stats.stackexchange.com/questions/49243/rs-randomforest-can-not-handle-more-than-32-levels-what-is-workaround
#

# <img src="../reference/R rattle/030 Process n Transform Data/a001.png" align="left">

# +


#=======================================================================
# Rattle timestamp: 2019-12-23 15:06:51 x86_64-pc-linux-gnu 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=1692 train=1184 validate=254 test=254

set.seed(88)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
crs$test

# The following variable selections have been noted.

crs$input     <- c("Denominator", "Year", "Source",
                   "State_Region")

crs$numeric   <- c("Denominator", "Year")

crs$categoric <- c("Source", "State_Region")

crs$target    <- "Prevalence_Risk4"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- c("State", "Prevalence", "Lower.CI", "Upper.CI", "Source_Full1", "State_Full1", "State_Full2", "Numerator_ASD", "Numerator_NonASD", "Proportion", "Chi_Wilson_Corrected_Lower.CI", "Chi_Wilson_Corrected_Upper.CI", "Male.Prevalence", "Male.Lower.CI", "Male.Upper.CI", "Female.Prevalence", "Female.Lower.CI", "Female.Upper.CI", "Non.hispanic.white.Prevalence", "Non.hispanic.white.Lower.CI", "Non.hispanic.white.Upper.CI", "Non.hispanic.black.Prevalence", "Non.hispanic.black.Lower.CI", "Non.hispanic.black.Upper.CI", "Hispanic.Prevalence", "Hispanic.Lower.CI", "Hispanic.Upper.CI", "Asian.or.Pacific.Islander.Prevalence", "Asian.or.Pacific.Islander.Lower.CI", "Asian.or.Pacific.Islander.Upper.CI", "Source_UC", "Source_Full3", "Prevalence_Risk2", "Year_Factor")
crs$weights   <- NULL
# -

# ---

# Adjust in-line plot size to M x N
options(repr.plot.width=8, repr.plot.height=5)

# +


#=======================================================================
# Rattle timestamp: 2019-12-23 14:59:20 x86_64-pc-linux-gnu 

# The 'gplots' package provides the 'barplot2' function.

library(gplots, quietly=TRUE)

#=======================================================================
# Rattle timestamp: 2019-12-23 14:59:20 x86_64-pc-linux-gnu 

# Bar Plot 

# Generate the summary data for plotting.

ds <- rbind(summary(na.omit(crs$dataset[crs$train,]$Source)),
    summary(na.omit(crs$dataset[crs$train,][crs$dataset[crs$train,]$Prevalence_Risk4=="High",]$Source)),
    summary(na.omit(crs$dataset[crs$train,][crs$dataset[crs$train,]$Prevalence_Risk4=="Low",]$Source)),
    summary(na.omit(crs$dataset[crs$train,][crs$dataset[crs$train,]$Prevalence_Risk4=="Medium",]$Source)),
    summary(na.omit(crs$dataset[crs$train,][crs$dataset[crs$train,]$Prevalence_Risk4=="Very High",]$Source)))

# Sort the entries.

ord <- order(ds[1,], decreasing=TRUE)

# Plot the data.

bp <-  barplot2(ds[,ord], beside=TRUE, ylab="Frequency", xlab="Source", ylim=c(0, 715), col=colorspace::rainbow_hcl(5))

# Add the actual frequencies.

text(bp, ds[,ord]+24, ds[,ord])

# Add a legend to the plot.

legend("topright", bty="n", c("All","High","Low","Medium","Very High"),  fill=colorspace::rainbow_hcl(5))

# Add a title to the plot.

title(main="Distribution of Source (sample)\nby Prevalence_Risk4",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#=======================================================================
# Rattle timestamp: 2019-12-23 14:59:21 x86_64-pc-linux-gnu 

# Bar Plot 

# Generate the summary data for plotting.

ds <- rbind(summary(na.omit(crs$dataset[crs$train,]$State_Region)),
    summary(na.omit(crs$dataset[crs$train,][crs$dataset[crs$train,]$Prevalence_Risk4=="High",]$State_Region)),
    summary(na.omit(crs$dataset[crs$train,][crs$dataset[crs$train,]$Prevalence_Risk4=="Low",]$State_Region)),
    summary(na.omit(crs$dataset[crs$train,][crs$dataset[crs$train,]$Prevalence_Risk4=="Medium",]$State_Region)),
    summary(na.omit(crs$dataset[crs$train,][crs$dataset[crs$train,]$Prevalence_Risk4=="Very High",]$State_Region)))

# Sort the entries.

ord <- order(ds[1,], decreasing=TRUE)

# Plot the data.

bp <-  barplot2(ds[,ord], beside=TRUE, ylab="Frequency", xlab="State_Region", ylim=c(0, 265), col=colorspace::rainbow_hcl(5))

# Add a legend to the plot.

legend("topright", bty="n", c("All","High","Low","Medium","Very High"),  fill=colorspace::rainbow_hcl(5))

# Add a title to the plot.

title(main="Distribution of State_Region (sample)\nby Prevalence_Risk4",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

# +


#=======================================================================
# Rattle timestamp: 2019-12-23 12:14:28 x86_64-pc-linux-gnu 

# Display histogram plots for the selected variables. 

# Use ggplot2 to generate histogram plot for Denominator

# Generate the plot.

p01 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(Prevalence_Risk4=as.factor(Prevalence_Risk4)) %>%
  dplyr::select(Denominator, Prevalence_Risk4) %>%
  ggplot2::ggplot(ggplot2::aes(x=Denominator)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(fill=Prevalence_Risk4, colour=Prevalence_Risk4), alpha=0.55) +
  ggplot2::xlab("Denominator\n\nRattle 2019-Dec-23 12:14:28 iss-user") +
  ggplot2::ggtitle("Distribution of Denominator (sample)\nby Prevalence_Risk4") +
  ggplot2::labs(fill="Prevalence_Risk4", y="Density")

# Use ggplot2 to generate histogram plot for Year

# Generate the plot.

p02 <- crs %>%
  with(dataset[train,]) %>%
  dplyr::mutate(Prevalence_Risk4=as.factor(Prevalence_Risk4)) %>%
  dplyr::select(Year, Prevalence_Risk4) %>%
  ggplot2::ggplot(ggplot2::aes(x=Year)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(fill=Prevalence_Risk4, colour=Prevalence_Risk4), alpha=0.55) +
  ggplot2::xlab("Year\n\nRattle 2019-Dec-23 12:14:28 iss-user") +
  ggplot2::ggtitle("Distribution of Year (sample)\nby Prevalence_Risk4") +
  ggplot2::labs(fill="Prevalence_Risk4", y="Density")

# Display the plots.

gridExtra::grid.arrange(p01, p02)
# -

# **Above shows that Denominator is skrewed, which need rescale transformation**

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Rattle: Process & Transform Data: Rescale (log transformation)
#     </h3>
# </div>
#

# <img src="../reference/R rattle/030 Process n Transform Data/a004.png" align="left">

# <img src="../reference/R rattle/030 Process n Transform Data/a005.png" align="left">

# +


#=======================================================================
# Rattle timestamp: 2019-12-23 15:10:29 x86_64-pc-linux-gnu 

# Transform variables by rescaling. 

# Rescale Denominator.

crs$dataset[["R10_Denominator"]] <- crs$dataset[["Denominator"]]

# Take a log10 transform of the variable - treat -Inf as NA.

if (building)
{
  crs$dataset[["R10_Denominator"]] <-  log10(crs$dataset[["Denominator"]]) 
  crs$dataset[crs$dataset[["R10_Denominator"]] == -Inf & ! is.na(crs$dataset[["R10_Denominator"]]), "R10_Denominator"] <- NA
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["R10_Denominator"]] <-  log10(crs$dataset[["Denominator"]]) 
  crs$dataset[crs$dataset[["R10_Denominator"]] == -Inf & ! is.na(crs$dataset[["R10_Denominator"]]), "R10_Denominator"] <- NA
}

#=======================================================================
# Rattle timestamp: 2019-12-23 15:10:29 x86_64-pc-linux-gnu 

# Action the user selections from the Data tab. 

# The following variable selections have been noted.

crs$input     <- c("Year", "Source", "State_Region",
                   "R10_Denominator")

crs$numeric   <- c("Year", "R10_Denominator")

crs$categoric <- c("Source", "State_Region")

crs$target    <- "Prevalence_Risk4"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- c("State", "Denominator", "Prevalence", "Lower.CI", "Upper.CI", "Source_Full1", "State_Full1", "State_Full2", "Numerator_ASD", "Numerator_NonASD", "Proportion", "Chi_Wilson_Corrected_Lower.CI", "Chi_Wilson_Corrected_Upper.CI", "Male.Prevalence", "Male.Lower.CI", "Male.Upper.CI", "Female.Prevalence", "Female.Lower.CI", "Female.Upper.CI", "Non.hispanic.white.Prevalence", "Non.hispanic.white.Lower.CI", "Non.hispanic.white.Upper.CI", "Non.hispanic.black.Prevalence", "Non.hispanic.black.Lower.CI", "Non.hispanic.black.Upper.CI", "Hispanic.Prevalence", "Hispanic.Lower.CI", "Hispanic.Upper.CI", "Asian.or.Pacific.Islander.Prevalence", "Asian.or.Pacific.Islander.Lower.CI", "Asian.or.Pacific.Islander.Upper.CI", "Source_UC", "Source_Full3", "Prevalence_Risk2", "Year_Factor")
crs$weights   <- NULL
# -

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Rattle: Process & Transform Data: Rescale (normalization)
#     </h3>
# </div>
#

# <img src="../reference/R rattle/030 Process n Transform Data/a006.png" align="left">

# <img src="../reference/R rattle/030 Process n Transform Data/a007.png" align="left">

# +


#=======================================================================
# Rattle timestamp: 2019-12-23 15:12:22 x86_64-pc-linux-gnu 

# Transform variables by rescaling. 

# The 'reshape' package provides the 'rescaler' function.

library(reshape, quietly=TRUE)

# Rescale Year.

crs$dataset[["RMD_Year"]] <- crs$dataset[["Year"]]

# Rescale by subtracting median and dividing by median abs deviation.

if (building)
{
  crs$dataset[["RMD_Year"]] <-  rescaler(crs$dataset[["Year"]], "robust")
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["RMD_Year"]] <- (crs$dataset[["Year"]] - 2007.000000)/5.930400
}

# Rescale R10_Denominator.

crs$dataset[["RMD_R10_Denominator"]] <- crs$dataset[["R10_Denominator"]]

# Rescale by subtracting median and dividing by median abs deviation.

if (building)
{
  crs$dataset[["RMD_R10_Denominator"]] <-  rescaler(crs$dataset[["R10_Denominator"]], "robust")
}

# When scoring transform using the training data parameters.

if (scoring)
{
  crs$dataset[["RMD_R10_Denominator"]] <- (crs$dataset[["R10_Denominator"]] - 5.548179)/0.598315
}

#=======================================================================
# Rattle timestamp: 2019-12-23 15:12:23 x86_64-pc-linux-gnu 

# Action the user selections from the Data tab. 

# The following variable selections have been noted.

crs$input     <- c("Source", "State_Region", "RMD_Year",
                   "RMD_R10_Denominator")

crs$numeric   <- c("RMD_Year", "RMD_R10_Denominator")

crs$categoric <- c("Source", "State_Region")

crs$target    <- "Prevalence_Risk4"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- c("State", "Denominator", "Prevalence", "Lower.CI", "Upper.CI", "Year", "Source_Full1", "State_Full1", "State_Full2", "Numerator_ASD", "Numerator_NonASD", "Proportion", "Chi_Wilson_Corrected_Lower.CI", "Chi_Wilson_Corrected_Upper.CI", "Male.Prevalence", "Male.Lower.CI", "Male.Upper.CI", "Female.Prevalence", "Female.Lower.CI", "Female.Upper.CI", "Non.hispanic.white.Prevalence", "Non.hispanic.white.Lower.CI", "Non.hispanic.white.Upper.CI", "Non.hispanic.black.Prevalence", "Non.hispanic.black.Lower.CI", "Non.hispanic.black.Upper.CI", "Hispanic.Prevalence", "Hispanic.Lower.CI", "Hispanic.Upper.CI", "Asian.or.Pacific.Islander.Prevalence", "Asian.or.Pacific.Islander.Lower.CI", "Asian.or.Pacific.Islander.Upper.CI", "Source_UC", "Source_Full3", "Prevalence_Risk2", "Year_Factor", "R10_Denominator")
crs$weights   <- NULL
# -

# **Optionally, Cleanup variables by Delete Ignored**

# <img src="../reference/R rattle/030 Process n Transform Data/a008.png" align="left">

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <a href="">
#          <img src="" width="750" align="center">
#     </a>
# </div>
#
#

# ## <span style="color:blue">Rattle: Train Model</span> (Classification)
#

# ## <span style="color:blue">Multi-Class Classification: Prevalence_Risk4</span>
#

# <img src="../reference/R rattle/040 Train Model Classification/Prevalence_Risk4.png" align="left">

crs$target

crs$input

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Multi-Class Model: Decision Tree (DT)
#     </h3>
# </div>
#

# <img src="../reference/R rattle/040 Train Model Classification/DT001.png" align="left"> 

# <img src="../reference/R rattle/040 Train Model Classification/DT002.png" align="left"> 

if(!require(rpart)){install.packages("rpart")}
library('rpart')

# +


#=======================================================================
# Rattle timestamp: 2019-12-23 15:16:48 x86_64-pc-linux-gnu 

# Decision Tree 

# The 'rpart' package provides the 'rpart' function.

library(rpart, quietly=TRUE)

# Reset the random number seed to obtain the same results each time.

set.seed(crv$seed)

# Build the Decision Tree model.

crs$rpart <- rpart(Prevalence_Risk4 ~ .,
    data=crs$dataset[crs$train, c(crs$input, crs$target)],
    method="class",
    parms=list(split="information"),
    control=rpart.control(usesurrogate=0, 
        maxsurrogate=0),
    model=TRUE)

# Generate a textual view of the Decision Tree model.

print(crs$rpart)
printcp(crs$rpart)
cat("\n")

# Time taken: 0.05 secs

# List the rules from the tree using a Rattle support function.

asRules(crs$rpart)
# -

# Adjust in-line plot size to M x N
options(repr.plot.width=8, repr.plot.height=6)

rpart.plot::prp(crs$rpart,
    type = 2, extra = "auto", nn = TRUE,
    under = FALSE, fallen.leaves = TRUE,
    digits = 2, varlen = 0, faclen = 0, 
#    roundint = TRUE,
    cex = NULL, tweak = 1,
#    clip.facs = FALSE, 
    clip.right.labs = TRUE,
    snip = FALSE,
    box.palette = "auto", shadow.col = 0)

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Multi-Class Model: Random Forest (RF)
#     </h3>
# </div>
#

# <img src="../reference/R rattle/040 Train Model Classification/RF001.png" align="left"> 

# <img src="../reference/R rattle/040 Train Model Classification/RF002.png" align="left"> 

if(!require(randomForest)){install.packages("randomForest")}
library('randomForest')

# +

#=======================================================================
# Rattle timestamp: 2019-12-23 15:40:42 x86_64-pc-linux-gnu 

# Build a Random Forest model using the traditional approach.

set.seed(crv$seed)

crs$rf <- randomForest::randomForest(Prevalence_Risk4 ~ .,
  data=crs$dataset[crs$train, c(crs$input, crs$target)], 
  ntree=500,
  mtry=2,
  importance=TRUE,
  na.action=randomForest::na.roughfix,
  replace=FALSE)

# Generate textual output of the 'Random Forest' model.

crs$rf

# List the importance of the variables.

rn <- round(randomForest::importance(crs$rf), 2)
rn[order(rn[,3], decreasing=TRUE),]

# Time taken: 2.00 secs

#=======================================================================
# Rattle timestamp: 2019-12-23 15:40:45 x86_64-pc-linux-gnu 

# Plot the relative importance of the variables.

p <- ggVarImp(crs$rf,
              title="Variable Importance Random Forest ADV_ASD_State_R.csv")
p

# +

# Plot the error rate against the number of trees.

plot(crs$rf, main="")
legend("topright", c("OOB", "High", "Low", "Medium", "Very High"), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest ADV_ASD_State_R.csv",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
# -

if(!require(verification)){install.packages("verification")}
library('verification')

head(crs$rf$votes)

# +

# Display tree number 1 to 3.

printRandomForests(crs$rf, 1:3)
# -

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Multi-Class Model: Support Vector Machines (SVM)
#     </h3>
# </div>
#

# <img src="../reference/R rattle/040 Train Model Classification/SVM001.png" align="left"> 

# <img src="../reference/R rattle/040 Train Model Classification/SVM002.png" align="left"> 

if(!require(kernlab)){install.packages("kernlab")}
library('kernlab')

# +


#=======================================================================
# Rattle timestamp: 2019-12-23 16:17:34 x86_64-pc-linux-gnu 

# Support vector machine. 

# The 'kernlab' package provides the 'ksvm' function.

library(kernlab, quietly=TRUE)

# Build a Support Vector Machine model.

set.seed(crv$seed)
crs$ksvm <- ksvm(as.factor(Prevalence_Risk4) ~ .,
      data=crs$dataset[crs$train,c(crs$input, crs$target)],
      kernel="rbfdot",
      prob.model=TRUE)

# Generate a textual view of the SVM model.

crs$ksvm

# Time taken: 0.40 secs
# -

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Multi-Class Model: Multinomial Logistic Regression
#     </h3>
# </div>
#

# <img src="../reference/R rattle/040 Train Model Classification/MLR001.png" align="left"> 

# <img src="../reference/R rattle/040 Train Model Classification/MLR002.png" align="left"> 

# +


#=======================================================================
# Rattle timestamp: 2019-12-23 16:13:19 x86_64-pc-linux-gnu 

# Regression model 

# Build a multinomial model using the nnet package.

library(nnet, quietly=TRUE)

# Summarise multinomial model using Anova from the car package.

library(car, quietly=TRUE)

# Build a Regression model.

crs$glm <- multinom(Prevalence_Risk4 ~ ., data=crs$dataset[crs$train,c(crs$input, crs$target)], trace=FALSE, maxit=1000)

# Generate a textual view of the Linear model.

rattle.print.summary.multinom(summary(crs$glm,
                              Wald.ratios=TRUE))
cat(sprintf("Log likelihood: %.3f (%d df)
", logLik(crs$glm)[1], attr(logLik(crs$glm), "df")))
if (is.null(crs$glm$na.action)) omitted <- TRUE else omitted <- -crs$glm$na.action
cat(sprintf("Pseudo R-Square: %.8f

",cor(apply(crs$glm$fitted.values, 1, function(x) which(x == max(x))),
as.integer(crs$dataset[crs$train,][omitted,]$Prevalence_Risk4))))

cat('==== ANOVA ====
')
print(Anova(crs$glm))
print("
")

# Time taken: 0.37 secs
# -

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <a href="">
#          <img src="" width="750" align="center">
#     </a>
# </div>
#
#

# ## <span style="color:blue">Rattle: Evaluate Model</span> (Classification: Multi-Class)
#

# ## <span style="color:blue">Multi-Class Classification: Prevalence_Risk4</span>
#

# + active=""
# crs$rpart
# crs$rf
# crs$ksvm
# crs$glm
# -

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Rattle: Evaluate Model (Classification): Error/Confusion matrix
#     </h3>
# </div>
#

# <img src="../reference/R rattle/050 Evaluate Model Classification/EM001.png" align="left"> 

# <img src="../reference/R rattle/050 Evaluate Model Classification/EM002.png" align="left"> 

crs$target

# +


#=======================================================================
# Rattle timestamp: 2019-12-23 16:32:12 x86_64-pc-linux-gnu 

# Evaluate model performance on the testing dataset. 

# Generate an Error Matrix for the Decision Tree model.

# Obtain the response from the Decision Tree model.

crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$test, c(crs$input, crs$target)],
    type="class")

# Generate the confusion matrix showing counts.

rattle::errorMatrix(crs$dataset[crs$test, c(crs$input, crs$target)]$Prevalence_Risk4, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(crs$dataset[crs$test, c(crs$input, crs$target)]$Prevalence_Risk4, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

# Generate an Error Matrix for the Random Forest model.

# Obtain the response from the Random Forest model.

crs$pr <- predict(crs$rf, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]))

# Generate the confusion matrix showing counts.

rattle::errorMatrix(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$Prevalence_Risk4, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$Prevalence_Risk4, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

# Generate an Error Matrix for the SVM model.

# Obtain the response from the SVM model.

crs$pr <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]))

# Generate the confusion matrix showing counts.

rattle::errorMatrix(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$Prevalence_Risk4, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)])$Prevalence_Risk4, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

# Generate an Error Matrix for the Linear model.

# Obtain the response from the Linear model.

crs$pr <- predict(crs$glm, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])

# Generate the confusion matrix showing counts.

rattle::errorMatrix(crs$dataset[crs$test, c(crs$input, crs$target)]$Prevalence_Risk4, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(crs$dataset[crs$test, c(crs$input, crs$target)]$Prevalence_Risk4, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))
# -

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Rattle: Evaluate Model (Classification): Score/Write predicted results to CSV file.
#     </h3>
# </div>
#

# <img src="../reference/R rattle/050 Evaluate Model Classification/EM003.png" align="left"> 

# +

#=======================================================================
# Rattle timestamp: 2019-12-23 16:50:20 x86_64-pc-linux-gnu 

# Score the testing dataset. 

# Obtain probability scores for the Decision Tree model on ADV_ASD_State_R.csv [test].

crs$pr_rpart <- predict(crs$rpart, newdata=crs$dataset[crs$test, c(crs$input)],
    type="class")

# Obtain probability scores for the Random Forest model on ADV_ASD_State_R.csv [test].

crs$pr_rf <- predict(crs$rf, newdata=na.omit(crs$dataset[crs$test, c(crs$input)]))

# Obtain probability scores for the SVM model on ADV_ASD_State_R.csv [test].

crs$pr_ksvm <- kernlab::predict(crs$ksvm, newdata=na.omit(crs$dataset[crs$test, c(crs$input)]))

# Obtain probability scores for the Linear model on ADV_ASD_State_R.csv [test].

crs$pr_glm <- predict(crs$glm, newdata=crs$dataset[crs$test, c(crs$input)])

# Extract the relevant variables from the dataset.

sdata <- crs$dataset[crs$test,]

# Output the combined data.

write.csv(cbind(sdata, crs$pr_rpart, crs$pr_rf, crs$pr_ksvm, crs$pr_glm), row.names=FALSE,
          file="../reference/R rattle/ADV_ASD_State_R_test_score_all_Prevalence_Risk4.csv")
# -

# <div class="alert alert-block alert-info" style="margin-top: 20px">
# </div>
#

# ## <span style="color:blue">Rattle: Train Model</span> (Classification)

# ## <span style="color:blue">Binary-Class Classification: Prevalence_Risk2</span>
#

# <img src="../reference/R rattle/040 Train Model Classification/Prevalence_Risk2.png" align="left">

# +

#=======================================================================
# Rattle timestamp: 2019-12-24 10:52:36 x86_64-pc-linux-gnu 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=1692 train=1184 validate=254 test=254

set.seed(88)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
crs$test

# The following variable selections have been noted.

crs$input     <- c("Source", "State_Region", "RMD_Year",
                   "RMD_R10_Denominator")

crs$numeric   <- c("RMD_Year", "RMD_R10_Denominator")

crs$categoric <- c("Source", "State_Region")

crs$target    <- "Prevalence_Risk2"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- c("State", "Denominator", "Prevalence", "Lower.CI", "Upper.CI", "Year", "Source_Full1", "State_Full1", "State_Full2", "Numerator_ASD", "Numerator_NonASD", "Proportion", "Chi_Wilson_Corrected_Lower.CI", "Chi_Wilson_Corrected_Upper.CI", "Male.Prevalence", "Male.Lower.CI", "Male.Upper.CI", "Female.Prevalence", "Female.Lower.CI", "Female.Upper.CI", "Non.hispanic.white.Prevalence", "Non.hispanic.white.Lower.CI", "Non.hispanic.white.Upper.CI", "Non.hispanic.black.Prevalence", "Non.hispanic.black.Lower.CI", "Non.hispanic.black.Upper.CI", "Hispanic.Prevalence", "Hispanic.Lower.CI", "Hispanic.Upper.CI", "Asian.or.Pacific.Islander.Prevalence", "Asian.or.Pacific.Islander.Lower.CI", "Asian.or.Pacific.Islander.Upper.CI", "Source_UC", "Source_Full3", "Prevalence_Risk4", "Year_Factor", "R10_Denominator")
crs$weights   <- NULL
# -

crs$target

crs$input

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Binary-Class Model: Boost
#     </h3>
# </div>
#

# **Ada Boost: Adaptive Boosting**

# <img src="../reference/R rattle/040 Train Model Classification/AB001.png" align="left"> 

# <img src="../reference/R rattle/040 Train Model Classification/AB002.png" align="left"> 

if(!require(ada)){install.packages("ada")}
library('ada')

# +


#=======================================================================
# Rattle timestamp: 2019-12-24 11:00:37 x86_64-pc-linux-gnu 

# Ada Boost 

# The `ada' package implements the boost algorithm.

# Build the Ada Boost model.

set.seed(crv$seed)
crs$ada <- ada::ada(Prevalence_Risk2 ~ .,
                    data=crs$dataset[crs$train,c(crs$input, crs$target)],
                    control=rpart::rpart.control(maxdepth=6,
                                                 cp=0.010000,
                                                 minsplit=20,
                                                 xval=10),
                    iter=50)

# Print the results of the modelling.

print(crs$ada)
round(crs$ada$model$errs[crs$ada$iter,], 2)
cat('Variables actually used in tree construction:\n')
print(sort(names(listAdaVarsUsed(crs$ada))))
cat('\nFrequency of variables actually used:\n')
print(listAdaVarsUsed(crs$ada))

# Time taken: 1.14 secs

# Plot the error rate as we increase the number of trees.

plot(crs$ada)

# Plot the relative importance of the variables.

ada::varplot(crs$ada)

# +

# Display tree number 1.

listTreesAda(crs$ada, 1)

# -

# ---

# **Xg Boost: Extreme Gradient Boost**

# <img src="../reference/R rattle/040 Train Model Classification/XB001.png" align="left"> 

# <img src="../reference/R rattle/040 Train Model Classification/XB002.png" align="left"> 

if(!require(xgboost)){install.packages("xgboost")}
library('xgboost')

# + active=""
#
# #=======================================================================
# # Rattle timestamp: 2019-12-24 11:21:50 x86_64-pc-linux-gnu 
#
# # Extreme Boost 
#
# # The `xgboost' package implements the extreme gradient boost algorithm.
#
# # Build the Extreme Boost model.
#
# set.seed(crv$seed)
#
# crs$xgb <- xgboost(Prevalence_Risk2 ~ .,
#   data              = crs$dataset[crs$train,c(crs$input, crs$target)],
#   max_depth         = 6,
#   eta               = 0.3, 
#   num_parallel_tree = 1, 
#   nthread           = 2, 
#   nround            = 50,
#   metrics           = 'error',
#   objective         = 'binary:logistic')
#
# # Print the results of the modelling.
#
# print(crs$xgb)
#
# cat('\nFinal iteration error rate:\n')
# print(round(crs$xgb$evaluation_log[crs$xgb$niter, ], 2))
#
# cat('\nImportance/Frequency of variables actually used:\n')
# print(crs$imp <- importance(crs$xgb, crs$dataset[crs$train,c(crs$input, crs$target)]))
#
# # Time taken: 0.10 secs
# -

# **Note**: Above R code generated from Rattle log cannot be executed directly due to:
# "Error in xgb.get.DMatrix(data, label, missing, weight): xgboost doesn't support data.frame as input. Convert it to matrix first."
#
# <span style="color:blue">In the workshop submission section, you are required to build **xgboost** model by fixing/handling this issue. References:</span>
#
# https://github.com/dd-consulting/DDC-Data-Science-R/blob/master/HousePriceAnalysisPrediction/codeR/House%20prices_%20Lasso%2C%20XGBoost%2C%20and%20a%20detailed%20EDA.pdf
#
# https://github.com/dd-consulting/DDC-Data-Science-R/blob/master/Google%20Analytics%20Customer%20Revenue%20Prediction/code/Google%20Analytics%20Customer%20Revenue%20Prediction%20EDA.pdf

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Binary-Class Model: Neural Net (NN)
#     </h3>
# </div>
#

# <img src="../reference/R rattle/040 Train Model Classification/NN001.png" align="left"> 

# <img src="../reference/R rattle/040 Train Model Classification/NN002.png" align="left"> 

if(!require(nnet)){install.packages("nnet")}
library('nnet')

# +


#=======================================================================
# Rattle timestamp: 2019-12-24 11:46:00 x86_64-pc-linux-gnu 

# Neural Network 

# Build a neural network model using the nnet package.

library(nnet, quietly=TRUE)

# Build the NNet model.

set.seed(199)
crs$nnet <- nnet(as.factor(Prevalence_Risk2) ~ .,
    data=crs$dataset[crs$train,c(crs$input, crs$target)],
    size=10, skip=TRUE, MaxNWts=10000, trace=FALSE, maxit=100)

# Print the results of the modelling.

cat(sprintf("A %s network with %d weights.\n",
    paste(crs$nnet$n, collapse="-"),
    length(crs$nnet$wts)))
cat(sprintf("Inputs: %s.\n",
    paste(crs$nnet$coefnames, collapse=", ")))
cat(sprintf("Output: %s.\n",
    names(attr(crs$nnet$terms, "dataClasses"))[1]))
cat(sprintf("Sum of Squares Residuals: %.4f.\n",
    sum(residuals(crs$nnet) ^ 2)))
cat("\n")
print(summary(crs$nnet))
cat('\n')

# Time taken: 0.29 secs
# -

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <a href="">
#          <img src="" width="750" align="center">
#     </a>
# </div>
#
#

# ## <span style="color:blue">Rattle: Evaluate Model</span> (Classification: Binary-Class)
#

# ## <span style="color:blue">Binary-Class Classification: Prevalence_Risk2</span>
#

# + active=""
# crs$ada
# crs$nnet
# -

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Rattle: Evaluate Model (Classification): Error/Confusion matrix
#     </h3>
# </div>
#

# <img src="../reference/R rattle/050 Evaluate Model Classification/EM011.png" align="left"> 

# +


#=======================================================================
# Rattle timestamp: 2019-12-24 12:07:18 x86_64-pc-linux-gnu 

# Evaluate model performance on the testing dataset. 

# Generate an Error Matrix for the Extreme Boost model.

# Obtain the response from the Extreme Boost model.

crs$pr <- predict(crs$ada, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])

# Generate the confusion matrix showing counts.

rattle::errorMatrix(crs$dataset[crs$test, c(crs$input, crs$target)]$Prevalence_Risk2, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(crs$dataset[crs$test, c(crs$input, crs$target)]$Prevalence_Risk2, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))

# Generate an Error Matrix for the Neural Net model.

# Obtain the response from the Neural Net model.

crs$pr <- predict(crs$nnet, newdata=crs$dataset[crs$test, c(crs$input, crs$target)], type="class")

# Generate the confusion matrix showing counts.

rattle::errorMatrix(crs$dataset[crs$test, c(crs$input, crs$target)]$Prevalence_Risk2, crs$pr, count=TRUE)

# Generate the confusion matrix showing proportions.

(per <- rattle::errorMatrix(crs$dataset[crs$test, c(crs$input, crs$target)]$Prevalence_Risk2, crs$pr))

# Calculate the overall error percentage.

cat(100-sum(diag(per), na.rm=TRUE))

# Calculate the averaged class error percentage.

cat(mean(per[,"Error"], na.rm=TRUE))
# -

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Rattle: Evaluate Model (Classification): ROC
#     </h3>
# </div>
#

# ROC Curve: **TPR (True Positive Rate) / Hit Rate / Recall / Sensitivity** vs. **FPR / 1- Specificity**

# <img src="../reference/R rattle/050 Evaluate Model Classification/EM012.png" align="left"> 

if(!require(ROCR)){install.packages("ROCR")}
library('ROCR')

# +


#=======================================================================
# Rattle timestamp: 2019-12-24 12:09:03 x86_64-pc-linux-gnu 

# Evaluate model performance on the testing dataset. 

# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the ada model on ADV_ASD_State_R.csv [test].

crs$pr <- predict(crs$ada, newdata=crs$dataset[crs$test, c(crs$input, crs$target)], type="prob")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$Prevalence_Risk2)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve Extreme Boost ADV_ASD_State_R.csv [test] Prevalence_Risk2")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                   label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$Prevalence_Risk2)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
performance(pred, "auc")

# ROC Curve: requires the ROCR package.

library(ROCR)

# ROC Curve: requires the ggplot2 package.

library(ggplot2, quietly=TRUE)

# Generate an ROC Curve for the nnet model on ADV_ASD_State_R.csv [test].

crs$pr <- predict(crs$nnet, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$Prevalence_Risk2)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}

pe <- performance(pred, "tpr", "fpr")
au <- performance(pred, "auc")@y.values[[1]]
pd <- data.frame(fpr=unlist(pe@x.values), tpr=unlist(pe@y.values))
p <- ggplot(pd, aes(x=fpr, y=tpr))
p <- p + geom_line(colour="red")
p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
p <- p + ggtitle("ROC Curve Neural Net ADV_ASD_State_R.csv [test] Prevalence_Risk2")
p <- p + theme(plot.title=element_text(size=10))
p <- p + geom_line(data=data.frame(), aes(x=c(0,1), y=c(0,1)), colour="grey")
p <- p + annotate("text", x=0.50, y=0.00, hjust=0, vjust=0, size=5,
                   label=paste("AUC =", round(au, 2)))
print(p)

# Calculate the area under the curve for the plot.


# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$Prevalence_Risk2)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
performance(pred, "auc")
# -

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Rattle: Evaluate Model (Classification): Sensitivity
#     </h3>
# </div>
#

# <img src="../reference/R rattle/050 Evaluate Model Classification/EM013.png" align="left"> 

# +


#=======================================================================
# Rattle timestamp: 2019-12-24 12:17:21 x86_64-pc-linux-gnu 

# Evaluate model performance on the testing dataset. 

# Sensitivity/Specificity Plot: requires the ROCR package

library(ROCR)

# Generate Sensitivity/Specificity Plot for ada model on ADV_ASD_State_R.csv [test].

crs$pr <- predict(crs$ada, newdata=crs$dataset[crs$test, c(crs$input, crs$target)], type="prob")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$Prevalence_Risk2)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
ROCR::plot(performance(pred, "sens", "spec"), col="#CC0000FF", lty=1, add=FALSE)


# Sensitivity/Specificity Plot: requires the ROCR package

library(ROCR)

# Generate Sensitivity/Specificity Plot for nnet model on ADV_ASD_State_R.csv [test].

crs$pr <- predict(crs$nnet, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$Prevalence_Risk2)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
ROCR::plot(performance(pred, "sens", "spec"), col="#00CCCCFF", lty=2, add=TRUE)


# Add a legend to the plot.

legend("bottomleft", c("ada","nnet"), col=rainbow(2, 1, .8), lty=1:2, title="Models", inset=c(0.05, 0.05))

# Add decorations to the plot.

title(main="Sensitivity/Specificity (tpr/tnr)  ADV_ASD_State_R.csv [test]",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
grid()
# -

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Rattle: Evaluate Model (Classification): Precision
#     </h3>
# </div>
#

# <img src="../reference/R rattle/050 Evaluate Model Classification/EM014.png" align="left"> 

# +


#=======================================================================
# Rattle timestamp: 2019-12-24 12:20:14 x86_64-pc-linux-gnu 

# Evaluate model performance on the testing dataset. 

# Precision/Recall Plot: requires the ROCR package

library(ROCR)

# Generate a Precision/Recall Plot for the ada model on ADV_ASD_State_R.csv [test].

crs$pr <- predict(crs$ada, newdata=crs$dataset[crs$test, c(crs$input, crs$target)], type="prob")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$Prevalence_Risk2)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
ROCR::plot(performance(pred, "prec", "rec"), col="#CC0000FF", lty=1, add=FALSE)


# Precision/Recall Plot: requires the ROCR package

library(ROCR)

# Generate a Precision/Recall Plot for the nnet model on ADV_ASD_State_R.csv [test].

crs$pr <- predict(crs$nnet, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$Prevalence_Risk2)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}
ROCR::plot(performance(pred, "prec", "rec"), col="#00CCCCFF", lty=2, add=TRUE)


# Add a legend to the plot.

legend("bottomleft", c("ada","nnet"), col=rainbow(2, 1, .8), lty=1:2, title="Models", inset=c(0.05, 0.05))

# Add decorations to the plot.

title(main="Precision/Recall Plot  ADV_ASD_State_R.csv [test]",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
grid()
# -

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Rattle: Evaluate Model (Classification): Lift
#     </h3>
# </div>
#

# <img src="../reference/R rattle/050 Evaluate Model Classification/EM015.png" align="left"> 

# +


#=======================================================================
# Rattle timestamp: 2019-12-24 12:11:55 x86_64-pc-linux-gnu 

# Evaluate model performance on the testing dataset. 

# Lift Chart: requires the ROCR package.

library(ROCR)

# Obtain predictions for the ada model on ADV_ASD_State_R.csv [test].

crs$pr <- predict(crs$ada, newdata=crs$dataset[crs$test, c(crs$input, crs$target)], type="prob")[,2]

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$Prevalence_Risk2)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}

# Convert rate of positive predictions to percentage.

per <- performance(pred, "lift", "rpp")
per@x.values[[1]] <- per@x.values[[1]]*100

# Plot the lift chart.
ROCR::plot(per, col="#CC0000FF", lty=1, xlab="Caseload (%)", add=FALSE)

# Lift Chart: requires the ROCR package.

library(ROCR)

# Obtain predictions for the nnet model on ADV_ASD_State_R.csv [test].

crs$pr <- predict(crs$nnet, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])

# Remove observations with missing target.

no.miss   <- na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]$Prevalence_Risk2)
miss.list <- attr(no.miss, "na.action")
attributes(no.miss) <- NULL

if (length(miss.list))
{
  pred <- prediction(crs$pr[-miss.list], no.miss)
} else
{
  pred <- prediction(crs$pr, no.miss)
}

# Convert rate of positive predictions to percentage.

per <- performance(pred, "lift", "rpp")
per@x.values[[1]] <- per@x.values[[1]]*100

# Plot the lift chart.
ROCR::plot(per, col="#00CCCCFF", lty=2, xlab="Caseload (%)", add=TRUE)

# Add a legend to the plot.

legend("topright", c("ada","nnet"), col=rainbow(2, 1, .8), lty=1:2, title="Models", inset=c(0.05, 0.05))

# Add decorations to the plot.

title(main="Lift Chart  ADV_ASD_State_R.csv [test]",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
grid()
# -

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Rattle: Evaluate Model (Classification): Score/Write predicted results to CSV file.
#     </h3>
# </div>
#

# +

#=======================================================================
# Rattle timestamp: 2019-12-24 12:22:22 x86_64-pc-linux-gnu 

# Score the testing dataset. 

# Obtain probability scores for the Extreme Boost model on ADV_ASD_State_R.csv [test].

crs$pr_ada <- predict(crs$ada, newdata=crs$dataset[crs$test, c(crs$input)])

# Obtain probability scores for the Neural Net model on ADV_ASD_State_R.csv [test].

crs$pr_nnet <- predict(crs$nnet, newdata=crs$dataset[crs$test, c(crs$input)], type="class")

# Extract the relevant variables from the dataset.

sdata <- crs$dataset[crs$test,]

# Output the combined data.

write.csv(cbind(sdata, crs$pr_ada, crs$pr_nnet), file="../reference/R rattle/ADV_ASD_State_R_test_score_all_Prevalence_Risk2.csv", row.names=FALSE)
# -

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <a href="">
#          <img src="" width="750" align="center">
#     </a>
# </div>
#
#

# ## <span style="color:blue">Rattle: Train Model</span> (Regression)
#

# <img src="../reference/R rattle/040 Train Model Regression/a001.png" align="left"> 
#

# +


#=======================================================================
# Rattle timestamp: 2019-12-30 11:36:06 x86_64-pc-linux-gnu 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=1692 train=1184 validate=254 test=254

set.seed(crv$seed)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
crs$test

# The following variable selections have been noted.

crs$input     <- c("Source", "State_Region", "RMD_Year",
                   "RMD_R10_Denominator")

crs$numeric   <- c("RMD_Year", "RMD_R10_Denominator")

crs$categoric <- c("Source", "State_Region")

crs$target    <- "Prevalence"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- c("State", "Denominator", "Lower.CI", "Upper.CI", "Year", "Source_Full1", "State_Full1", "State_Full2", "Numerator_ASD", "Numerator_NonASD", "Proportion", "Chi_Wilson_Corrected_Lower.CI", "Chi_Wilson_Corrected_Upper.CI", "Male.Prevalence", "Male.Lower.CI", "Male.Upper.CI", "Female.Prevalence", "Female.Lower.CI", "Female.Upper.CI", "Non.hispanic.white.Prevalence", "Non.hispanic.white.Lower.CI", "Non.hispanic.white.Upper.CI", "Non.hispanic.black.Prevalence", "Non.hispanic.black.Lower.CI", "Non.hispanic.black.Upper.CI", "Hispanic.Prevalence", "Hispanic.Lower.CI", "Hispanic.Upper.CI", "Asian.or.Pacific.Islander.Prevalence", "Asian.or.Pacific.Islander.Lower.CI", "Asian.or.Pacific.Islander.Upper.CI", "Source_UC", "Source_Full3", "Prevalence_Risk2", "Prevalence_Risk4", "Year_Factor", "R10_Denominator")
crs$weights   <- NULL
# -

crs$target

crs$input

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Regression Model: Decision Tree (DT)
#     </h3>
# </div>
#

# <img src="../reference/R rattle/040 Train Model Regression/DT001.png" align="left"> 
#

# +


#=======================================================================
# Rattle timestamp: 2019-12-30 11:36:06 x86_64-pc-linux-gnu 

# Action the user selections from the Data tab. 

# Build the train/validate/test datasets.

# nobs=1692 train=1184 validate=254 test=254

set.seed(crv$seed)

crs$nobs <- nrow(crs$dataset)

crs$train <- sample(crs$nobs, 0.7*crs$nobs)

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  sample(0.15*crs$nobs) ->
crs$validate

crs$nobs %>%
  seq_len() %>%
  setdiff(crs$train) %>%
  setdiff(crs$validate) ->
crs$test

# The following variable selections have been noted.

crs$input     <- c("Source", "State_Region", "RMD_Year",
                   "RMD_R10_Denominator")

crs$numeric   <- c("RMD_Year", "RMD_R10_Denominator")

crs$categoric <- c("Source", "State_Region")

crs$target    <- "Prevalence"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- c("State", "Denominator", "Lower.CI", "Upper.CI", "Year", "Source_Full1", "State_Full1", "State_Full2", "Numerator_ASD", "Numerator_NonASD", "Proportion", "Chi_Wilson_Corrected_Lower.CI", "Chi_Wilson_Corrected_Upper.CI", "Male.Prevalence", "Male.Lower.CI", "Male.Upper.CI", "Female.Prevalence", "Female.Lower.CI", "Female.Upper.CI", "Non.hispanic.white.Prevalence", "Non.hispanic.white.Lower.CI", "Non.hispanic.white.Upper.CI", "Non.hispanic.black.Prevalence", "Non.hispanic.black.Lower.CI", "Non.hispanic.black.Upper.CI", "Hispanic.Prevalence", "Hispanic.Lower.CI", "Hispanic.Upper.CI", "Asian.or.Pacific.Islander.Prevalence", "Asian.or.Pacific.Islander.Lower.CI", "Asian.or.Pacific.Islander.Upper.CI", "Source_UC", "Source_Full3", "Prevalence_Risk2", "Prevalence_Risk4", "Year_Factor", "R10_Denominator")
crs$weights   <- NULL

#=======================================================================
# Rattle timestamp: 2019-12-30 11:41:57 x86_64-pc-linux-gnu 

# Decision Tree 

# The 'rpart' package provides the 'rpart' function.

library(rpart, quietly=TRUE)

# Reset the random number seed to obtain the same results each time.

set.seed(crv$seed)

# Build the Decision Tree model.

crs$rpart <- rpart(Prevalence ~ .,
    data=crs$dataset[crs$train, c(crs$input, crs$target)],
    method="anova",
    parms=list(split="information"),
    control=rpart.control(usesurrogate=0, 
        maxsurrogate=0),
    model=TRUE)

# Generate a textual view of the Decision Tree model.

print(crs$rpart)
printcp(crs$rpart)
cat("\n")

# Time taken: 0.01 secs

# List the rules from the tree using a Rattle support function.

asRules(crs$rpart)

# -

# Adjust in-line plot size to M x N
options(repr.plot.width=8, repr.plot.height=6)

rpart.plot::prp(crs$rpart,
    type = 2, extra = "auto", nn = TRUE,
    under = FALSE, fallen.leaves = TRUE,
    digits = 2, varlen = 0, faclen = 0, 
#    roundint = TRUE,
    cex = NULL, tweak = 1,
#    clip.facs = FALSE, 
    clip.right.labs = TRUE,
    snip = FALSE,
    box.palette = "auto", shadow.col = 0)

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Regression Model: Random Forest (RF)
#     </h3>
# </div>
#

# <img src="../reference/R rattle/040 Train Model Regression/RF001.png" align="left"> 

# +


#=======================================================================
# Rattle timestamp: 2019-12-30 11:45:04 x86_64-pc-linux-gnu 

# Build a Random Forest model using the traditional approach.

set.seed(crv$seed)

crs$rf <- randomForest::randomForest(Prevalence ~ .,
  data=crs$dataset[crs$train, c(crs$input, crs$target)], 
  ntree=500,
  mtry=2,
  importance=TRUE,
  na.action=randomForest::na.roughfix,
  replace=FALSE)

# Generate textual output of the 'Random Forest' model.

crs$rf

# List the importance of the variables.

rn <- crs$rf %>%
    randomForest::importance() %>%
    round(2)
    rn[order(rn[,1], decreasing=TRUE),]

# Time taken: 1.17 secs

#=======================================================================
# Rattle timestamp: 2019-12-30 11:45:10 x86_64-pc-linux-gnu 

# Plot the relative importance of the variables.

p <- ggVarImp(crs$rf,
              title="Variable Importance Random Forest ADV_ASD_State_R.csv")
p

# +


# Plot the error rate against the number of trees.

plot(crs$rf, main="")
legend("topright", c(""), text.col=1:6, lty=1:3, col=1:3)
title(main="Error Rates Random Forest ADV_ASD_State_R.csv",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

# +


# Display tree number 1.

printRandomForests(crs$rf, 1)
# -

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Regression Model: Linear Regression (LR)
#     </h3>
# </div>
#

# <img src="../reference/R rattle/040 Train Model Regression/LR001.png" align="left"> 

# +


#=======================================================================
# Rattle timestamp: 2019-12-30 11:48:44 x86_64-pc-linux-gnu 

# Regression model 

# Build a Regression model.

crs$glm <- lm(Prevalence ~ ., data=crs$dataset[crs$train,c(crs$input, crs$target)])

# Generate a textual view of the Linear model.

print(summary(crs$glm))
cat('==== ANOVA ====

')
print(anova(crs$glm))
print("
")

# Time taken: 0.28 secs

# Plot the model evaluation.

ttl <- genPlotTitleCmd("Linear Model",crs$dataname,vector=TRUE)
plot(crs$glm, main=ttl[1])
# -

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Regression Model: Neural Network (NN)
#     </h3>
# </div>
#

# <img src="../reference/R rattle/040 Train Model Regression/NN001.png" align="left"> 

# +


#=======================================================================
# Rattle timestamp: 2019-12-30 11:51:47 x86_64-pc-linux-gnu 

# Neural Network 

# Build a neural network model using the nnet package.

library(nnet, quietly=TRUE)

# Build the NNet model.

set.seed(199)
crs$nnet <- nnet(Prevalence ~ .,
    data=crs$dataset[crs$train,c(crs$input, crs$target)],
    size=10, linout=TRUE, skip=TRUE, MaxNWts=10000, trace=FALSE, maxit=100)

# Print the results of the modelling.

cat(sprintf("A %s network with %d weights.\n",
    paste(crs$nnet$n, collapse="-"),
    length(crs$nnet$wts)))
cat(sprintf("Inputs: %s.\n",
    paste(crs$nnet$coefnames, collapse=", ")))
cat(sprintf("Output: %s.\n",
    names(attr(crs$nnet$terms, "dataClasses"))[1]))
cat(sprintf("Sum of Squares Residuals: %.4f.\n",
    sum(residuals(crs$nnet) ^ 2)))
cat("\n")
print(summary(crs$nnet))
cat('\n')

# Time taken: 0.30 secs
# -

# ## <span style="color:blue">Rattle: Evaluate Model</span> (Regression)
#

# ## <span style="color:blue">Regression: Prevalence</span>
#

# + active=""
# crs$rpart
# crs$rf
# crs$glm
# crs$nnet
# -

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Rattle: Evaluate Model (Regression): Predicted Versus Observed
#     </h3>
# </div>
#

# <img src="../reference/R rattle/050 Evaluate Model Regression/EM001.png" align="left"> 

# +


#=======================================================================
# Rattle timestamp: 2019-12-30 11:54:46 x86_64-pc-linux-gnu 

# Evaluate model performance on the testing dataset. 

# RPART: Generate a Predicted v Observed plot for rpart model on ADV_ASD_State_R.csv [test].

crs$pr <- predict(crs$rpart, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])

# Obtain the observed output for the dataset.

obs <- subset(crs$dataset[crs$test, c(crs$input, crs$target)], select=crs$target)

# Handle in case categoric target treated as numeric.

obs.rownames <- rownames(obs)
obs <- as.numeric(obs[[1]])
obs <- data.frame(Prevalence=obs)
rownames(obs) <- obs.rownames

# Combine the observed values with the predicted.

fitpoints <- na.omit(cbind(obs, Predicted=crs$pr))

# Obtain the pseudo R2 - a correlation.

fitcorr <- format(cor(fitpoints[,1], fitpoints[,2])^2, digits=4)

# Plot settings for the true points and best fit.

op <- par(c(lty="solid", col="blue"))

# Display the observed (X) versus predicted (Y) points.

plot(fitpoints[[1]], fitpoints[[2]], asp=1, xlab="Prevalence", ylab="Predicted")

# Generate a simple linear fit between predicted and observed.

prline <- lm(fitpoints[,2] ~ fitpoints[,1])

# Add the linear fit to the plot.

abline(prline)

# Add a diagonal representing perfect correlation.

par(c(lty="dashed", col="black"))
abline(0, 1)

# Include a pseudo R-square on the plot

legend("bottomright",  sprintf(" Pseudo R-square=%s ", fitcorr),  bty="n")

# Add a title and grid to the plot.

title(main="Predicted vs. Observed
 Decision Tree Model
 ADV_ASD_State_R.csv [test]",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
grid()

# RF: Generate a Predicted v Observed plot for rf model on ADV_ASD_State_R.csv [test].

crs$pr <- predict(crs$rf, newdata=na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]))

# Obtain the observed output for the dataset.

obs <- subset(na.omit(crs$dataset[crs$test, c(crs$input, crs$target)]), select=crs$target)

# Handle in case categoric target treated as numeric.

obs.rownames <- rownames(obs)
obs <- as.numeric(obs[[1]])
obs <- data.frame(Prevalence=obs)
rownames(obs) <- obs.rownames

# Combine the observed values with the predicted.

fitpoints <- na.omit(cbind(obs, Predicted=crs$pr))

# Obtain the pseudo R2 - a correlation.

fitcorr <- format(cor(fitpoints[,1], fitpoints[,2])^2, digits=4)

# Plot settings for the true points and best fit.

op <- par(c(lty="solid", col="blue"))

# Display the observed (X) versus predicted (Y) points.

plot(fitpoints[[1]], fitpoints[[2]], asp=1, xlab="Prevalence", ylab="Predicted")

# Generate a simple linear fit between predicted and observed.

prline <- lm(fitpoints[,2] ~ fitpoints[,1])

# Add the linear fit to the plot.

abline(prline)

# Add a diagonal representing perfect correlation.

par(c(lty="dashed", col="black"))
abline(0, 1)

# Include a pseudo R-square on the plot

legend("bottomright",  sprintf(" Pseudo R-square=%s ", fitcorr),  bty="n")

# Add a title and grid to the plot.

title(main="Predicted vs. Observed
 Random Forest Model
 ADV_ASD_State_R.csv [test]",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
grid()

# GLM: Generate a Predicted v Observed plot for glm model on ADV_ASD_State_R.csv [test].

crs$pr <- predict(crs$glm, 
   type    = "response",
   newdata = crs$dataset[crs$test, c(crs$input, crs$target)])

# Obtain the observed output for the dataset.

obs <- subset(crs$dataset[crs$test, c(crs$input, crs$target)], select=crs$target)

# Handle in case categoric target treated as numeric.

obs.rownames <- rownames(obs)
obs <- as.numeric(obs[[1]])
obs <- data.frame(Prevalence=obs)
rownames(obs) <- obs.rownames

# Combine the observed values with the predicted.

fitpoints <- na.omit(cbind(obs, Predicted=crs$pr))

# Obtain the pseudo R2 - a correlation.

fitcorr <- format(cor(fitpoints[,1], fitpoints[,2])^2, digits=4)

# Plot settings for the true points and best fit.

op <- par(c(lty="solid", col="blue"))

# Display the observed (X) versus predicted (Y) points.

plot(fitpoints[[1]], fitpoints[[2]], asp=1, xlab="Prevalence", ylab="Predicted")

# Generate a simple linear fit between predicted and observed.

prline <- lm(fitpoints[,2] ~ fitpoints[,1])

# Add the linear fit to the plot.

abline(prline)

# Add a diagonal representing perfect correlation.

par(c(lty="dashed", col="black"))
abline(0, 1)

# Include a pseudo R-square on the plot

legend("bottomright",  sprintf(" Pseudo R-square=%s ", fitcorr),  bty="n")

# Add a title and grid to the plot.

title(main="Predicted vs. Observed
 Linear Model
 ADV_ASD_State_R.csv [test]",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
grid()

# NNET: Generate a Predicted v Observed plot for nnet model on ADV_ASD_State_R.csv [test].

crs$pr <- predict(crs$nnet, newdata=crs$dataset[crs$test, c(crs$input, crs$target)])

# Obtain the observed output for the dataset.

obs <- subset(crs$dataset[crs$test, c(crs$input, crs$target)], select=crs$target)

# Handle in case categoric target treated as numeric.

obs.rownames <- rownames(obs)
obs <- as.numeric(obs[[1]])
obs <- data.frame(Prevalence=obs)
rownames(obs) <- obs.rownames

# Combine the observed values with the predicted.

fitpoints <- na.omit(cbind(obs, Predicted=crs$pr))

# Obtain the pseudo R2 - a correlation.

fitcorr <- format(cor(fitpoints[,1], fitpoints[,2])^2, digits=4)

# Plot settings for the true points and best fit.

op <- par(c(lty="solid", col="blue"))

# Display the observed (X) versus predicted (Y) points.

plot(fitpoints[[1]], fitpoints[[2]], asp=1, xlab="Prevalence", ylab="Predicted")

# Generate a simple linear fit between predicted and observed.

prline <- lm(fitpoints[,2] ~ fitpoints[,1])

# Add the linear fit to the plot.

abline(prline)

# Add a diagonal representing perfect correlation.

par(c(lty="dashed", col="black"))
abline(0, 1)

# Include a pseudo R-square on the plot

legend("bottomright",  sprintf(" Pseudo R-square=%s ", fitcorr),  bty="n")

# Add a title and grid to the plot.

title(main="Predicted vs. Observed
 Neural Net Model
 ADV_ASD_State_R.csv [test]",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
grid()
# -

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Rattle: Evaluate Model (Regression): Score/Write predicted results to CSV file.
#     </h3>
# </div>
#

# <img src="../reference/R rattle/050 Evaluate Model Regression/EM002.png" align="left"> 

# +


#=======================================================================
# Rattle timestamp: 2019-12-30 11:59:50 x86_64-pc-linux-gnu 

# Score the testing dataset. 

# Obtain predictions for the Decision Tree model on ADV_ASD_State_R.csv [test].

crs$pr_rpart <- predict(crs$rpart, newdata=crs$dataset[crs$test, c(crs$input)])

# Obtain predictions for the Random Forest model on ADV_ASD_State_R.csv [test].

crs$pr_rf <- predict(crs$rf, newdata=na.omit(crs$dataset[crs$test, c(crs$input)]))

# Obtain predictions for the Linear model on ADV_ASD_State_R.csv [test].

crs$pr_glm <- predict(crs$glm, 
   type    = "response",
   newdata = crs$dataset[crs$test, c(crs$input)])

# Obtain predictions for the Neural Net model on ADV_ASD_State_R.csv [test].

crs$pr_nnet <- predict(crs$nnet, newdata=crs$dataset[crs$test, c(crs$input)])

# Extract the relevant variables from the dataset.

sdata <- crs$dataset[crs$test,]

# Output the combined data.

write.csv(cbind(sdata, crs$pr_rpart, crs$pr_rf, crs$pr_glm, crs$pr_nnet), file="../reference/R rattle/ADV_ASD_State_R_test_score_all_Prevalence.csv", row.names=FALSE)

# -

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <a href="">
#          <img src="" width="750" align="center">
#     </a>
# </div>
#
#

# ## <span style="color:blue">Rattle: Improve Model</span>
#

# **Tune hyperparameters**

# <img src="../reference/R rattle/060 Improve Model/IM001.png" align="left"> 

# <img src="../reference/R rattle/060 Improve Model/IM002.png" align="left"> 

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <a href="">
#          <img src="" width="750" align="center">
#     </a>
# </div>
#
#

# ## <span style="color:blue">Rattle: Save Model & Log</span>
#

# <img src="../reference/R rattle/070 Save Model n Log/SM003.png" align="left"> 

# <img src="../reference/R rattle/070 Save Model n Log/SM004.png" align="left"> 

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <a href="">
#          <img src="" width="750" align="center">
#     </a>
# </div>
#
#

# ## <span style="color:blue">Workshop Submission</span>
#

# <div class="alert alert-danger alertdanger" style="margin-top: 20px">
#     <h3>
#         What to submit?
#     </h3>
#     <p>
#         Create predictive model for Multi Class Classification of ASD Prevalence Risk Level (Low, Medium, High, Very High) using xgboost package's Extreme Gradient Boosting algorithm.
#     </p>
# </div>

# References:
#
#
# XGBoost support added to Rattle: A demo using Kaggle Competition Credit Card Fraud Detection: https://blog.revolutionanalytics.com/2017/07/xgboost-support-added-to-rattle.html
#
# https://github.com/dd-consulting/DDC-Data-Science-R/blob/master/HousePriceAnalysisPrediction/codeR/House%20prices_%20Lasso%2C%20XGBoost%2C%20and%20a%20detailed%20EDA.pdf
#
# https://github.com/dd-consulting/DDC-Data-Science-R/blob/master/Google%20Analytics%20Customer%20Revenue%20Prediction/code/Google%20Analytics%20Customer%20Revenue%20Prediction%20EDA.pdf
#

# Write your code below and press Shift+Enter to execute 


# Double-click <b>here</b> for the solution.
#
# <!-- The answer is below:
#
# # Write your code below and press Shift+Enter to execute 
# #
# # TBD
#
# -->

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <a href="">
#          <img src="" width="750" align="center">
#     </a>
# </div>
#
#

# ### Excellent! You have completed the workshop notebook!

# **Connect with the author:**
#
# This notebook was written by [GU Zhan (Sam)](https://sg.linkedin.com/in/zhan-gu-27a82823 "GU Zhan (Sam)").
#
# [Sam](https://www.iss.nus.edu.sg/about-us/staff/detail/201/GU%20Zhan "GU Zhan (Sam)") is currently a lecturer in [Institute of Systems Science](https://www.iss.nus.edu.sg/ "NUS-ISS") in [National University of Singapore](http://www.nus.edu.sg/ "NUS"). He devotes himself into pedagogy & andragogy, and is very passionate in inspiring next generation of artificial intelligence lovers and leaders.
#

# Copyright &copy; 2020 GU Zhan
#
# This notebook and its source code are released under the terms of the [MIT License](https://en.wikipedia.org/wiki/MIT_License "Copyright (c) 2020 GU ZHAN").
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
#

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <a href="">
#          <img src="" width="750" align="center">
#     </a>
# </div>
#
#

# ## <span style="color:blue">Appendices</span>
#

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Interactive workshops: < Learning R inside R > using swirl() (in R/RStudio)
#     </h3>
# </div>
#

# https://github.com/telescopeuser/S-SB-Workshop

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <a href="https://github.com/dd-consulting">
#          <img src="../reference/GZ_logo.png" width="60" align="right">
#         https://github.com/dd-consulting
#     </a>
# </div>
#

# ---

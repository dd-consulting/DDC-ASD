###########################################################
# Study of Autism Spectrum Disorder (ASD)
# https://www.cdc.gov/ncbddd/autism/data/index.html#explore
# https://www.cdc.gov/ncbddd/autism/index.html
###########################################################

# ----------------------------------
# Set working directory
# ----------------------------------
setwd("~/Desktop/admin-desktop/vm_shared_folder/git/DDC-ASD/model_R")
setwd("/media/sf_vm_shared_folder/git/DDC/DDC-ASD/model_R")
getwd()
options(warn=-1) # Turning off unnecessary warning messages. To reset: options(warn=0)

# ----------------------------------
# Optionally, export the processed dataframe data to CSV file.
# ----------------------------------
write.csv(ASD_National, file = "../dataset/ADV_ASD_National_R.csv", sep = ',', row.names = FALSE)
# Read back in above saved file:
ASD_National <- read.csv("../dataset/ADV_ASD_National_R.csv")
ASD_National$Year_Factor <- factor(ASD_National$Year_Factor, ordered = TRUE) # Convert Year_Factor to ordered.factor

# ----------------------------------
# Optionally, export the processed dataframe data to CSV file.
# ----------------------------------
write.csv(ASD_State, file = "../dataset/ADV_ASD_State_R.csv", sep = ',', row.names = FALSE)
# Read back in above saved file:
ASD_State <- read.csv("../dataset/ADV_ASD_State_R.csv")
ASD_State$Year_Factor <- factor(ASD_State$Year_Factor, ordered = TRUE) # Convert Year_Factor to ordered.factor

# Filter and create dataframe of different data sources
ASD_State_ADDM <- subset(ASD_State, Source == 'addm')
ASD_State_MEDI <- subset(ASD_State, Source == 'medi')
ASD_State_NSCH <- subset(ASD_State, Source == 'nsch')
ASD_State_SPED <- subset(ASD_State, Source == 'sped')

# ----------------------------------
# Create a *Population* of US. State level ASD Prevalence from Source SPED in Year 2016 
# ----------------------------------
ASD_State_SPED_2016 <- subset(ASD_State, Source == 'sped' & Year == 2016, select=c('State', 'Prevalence'))
dim(ASD_State_SPED_2016)
# *Population* mean Prevalence
mean(ASD_State_SPED_2016$Prevalence)

# ----------------------------------
# R n D
# ----------------------------------

library(ggplot2)
p <- ggplot(ASD_State_SPED_2016) +
  geom_histogram(aes(x = Prevalence, y = ..density..),
                 binwidth = 1, fill = "grey", color = "white", alpha=0.5)
p

p + geom_density(aes(x=Prevalence), fill = "blue", color = "white", alpha=0.1)
p


# ----------------------------------
# Sampling distribution vs. Population distribution vs. Z-Norm
# ----------------------------------
# Create:
# Population (Prevalence) histogram in probablity
p <- ggplot(ASD_State_SPED_2016) +
  geom_histogram(aes(x = Prevalence, y = ..density..),
                 binwidth = 1, fill = "grey", alpha=0.5) +
  coord_cartesian(xlim=c(0, 25), ylim=c(0, 0.6)) +
  labs(x="Prevalvence", 
       y="Probability Density", 
       title="Visualize Central Limit Theorem (CLT)")
p # Show

# Overlay curve:
# Population (Prevalence) density
p = p + geom_density(aes(x=Prevalence), fill = "grey4", color = "lightgrey", alpha=0.1)
p # Show
# Overlay line:
# mean = mean of Population (Prevalence)
p = p + geom_vline(aes(xintercept = mean(ASD_State_SPED_2016$Prevalence)), colour="darkgrey")
p # Show

# Overlay:
# Sample means histogram in probability (Sampling disribution)
p + geom_density(aes(x=clt_sample_k_mean), fill = "grey4", color = "lightgrey", alpha=0.1)



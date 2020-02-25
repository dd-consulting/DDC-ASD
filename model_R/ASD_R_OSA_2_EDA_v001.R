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
#         One-Stop Analytics: Exploratory Data Analysis (EDA)
#     </h1>
# </div>
#

# # Case Study of Autism Spectrum Disorder (ASD) with R
#
# ---
#
# ![](../reference/CDC_ASD/CDC_ASD_01.jpg)
#
# ![](../reference/CDC_ASD/CDC_ASD_02.png)
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
# https://www.todayonline.com/singapore/more-preschoolers-diagnosed-developmental-issues
#
# ![](../reference/SG_ASD/SG_ASD_01.png)
#
#

# ![](../reference/SG_ASD/SG_ASD_04.png) 
#
# https://www.pathlight.org.sg/

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <a href="">
#     </a>
# </div>
#

# # Workshop Objective: 
#
# ## Use R to analyze Autism Spectrum Disorder (ASD) data from CDC USA. 
#
# https://www.cdc.gov/ncbddd/autism/data/index.html
#
# * ## EDA - Summarization
#
# * ## Data Visualisation (Enhanced)
#
# * ## Workshop Submission
#
# * ## Appendices
#

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <a href="">
#     </a>
# </div>

# **Obtain current R <span style="color:blue">working directory</span>**

getwd()

# **Set new R working directory**

# setwd("/media/sf_vm_shared_folder/git/DDC/DDC-ASD/model_R")
# setwd('~/Desktop/admin-desktop/vm_shared_folder/git/DDC-ASD/model_R')
getwd()

# **Read in CSV data, storing as R <span style="color:blue">dataframe</span>**

# Read back in above saved file:
ASD_National <- read.csv("../dataset/ADV_ASD_National_R.csv")
# Convert Year_Factor to ordered.factor
ASD_National$Year_Factor <- factor(ASD_National$Year_Factor, ordered = TRUE) 

# <div class="alert alert-block alert-info" style="margin-top: 20px">
# </div>
#
#

# ## <span style="color:blue">EDA - Summarization </span>
#

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     EDA - Summarization - High Level Data Summary
#     </h3>
# </div>
#

summary(ASD_National)

# <div class="alert alert-block alert-info" style="margin-top: 20px">
# </div>
#
#

# ## <span style="color:blue">Data Visualisation (Enhanced)</span>
#

if(!require(ggplot2)){install.packages("ggplot2")}
library(ggplot2)

# Adjust in-line plot size to M x N
options(repr.plot.width=8, repr.plot.height=4)

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Data Visualisation (Enhanced) - <span style="color:blue">[ CDC ] Explore the Data</span>
#     </h3>
# </div>
#
#

# ![](../reference/CDC_ASD/ADV_Years_Data_Available.png)
#

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Data Visualisation (Enhanced) - <span style="color:blue">[ R ] Explore the Data</span>
#     </h3>
# </div>
#

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

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Data Visualisation (Enhanced) - Barplot
#     </h3>
# </div>
#

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

# **Above chart is now very similar to earlier [National] < Years Data Available >.**

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Data Visualisation (Enhanced) - <span style="color:blue">[ R ] Prevalence by Data Sources and Risk Levels</span>
#     </h3>
# </div>
#

# Use color to differentiate sub-group data (Year)
ggplot(ASD_National, aes(x = Source, fill = Prevalence_Risk4)) + 
  geom_bar(alpha=0.95, position = position_stack(reverse = TRUE)) + # Reverse default colour/fill order
  scale_fill_manual("Data Source:", values = c("Low" = "lightyellow", 
                                               "Medium" = "orange", 
                                               "High" = "red",
                                               "Very High" = "darkred")) +
  labs(x="Data Sources", y="Occurrences", title="Prevalence by Data Sources and Risk Levels") + # layers of graphics
  theme(legend.position="top") + labs(fill = "Legend: Risk")

# **Barplot / Column plot**

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Data Visualisation (Enhanced) - <span style="color:blue">[ CDC ] REPORTED PREVALENCE VARIES BY SEX</span>
#     </h3>
# </div>
#
#

# ![](../reference/CDC_ASD/ADV_addm_2014_Prevalence_Estimates_by_Sex.png)
#

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Data Visualisation (Enhanced) - <span style="color:blue">[ R ] REPORTED PREVALENCE VARIES BY SEX</span> [ Source: ADDM ] [ Year: 2014 ]
#     </h3>
# </div>
#

# Filter only data of ADDM
ASD_National_ADDM <- subset(ASD_National, Source == 'addm')
#
ASD_National_ADDM

# Construct a new re-shaped dataframe of [ Source: ADDM ] [Year: 2014]
#
Process_Source = 'addm'
Process_Year = 2014

# **Define a function to create a re-shaped dataframe:**

Function_Reshape_ASD_National_ADDM <- function(Process_Source, Process_Year) {
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
    ASD_National_ADDM_Reshaped_DF = data.frame(Sex.Group, Prevalence, stringsAsFactors=T)

    # Add new columns:
    ASD_National_ADDM_Reshaped_DF$Source = Process_Source
    ASD_National_ADDM_Reshaped_DF$Year = Process_Year
    return(ASD_National_ADDM_Reshaped_DF) # Return a dataframe
}

# **Use defined function <span style="color:blue">Function_Reshape_ASD_National_ADDM( )</span> for a specific year:**

ASD_National_ADDM_Reshaped_DF <- Function_Reshape_ASD_National_ADDM(Process_Source = 'addm', Process_Year = 2014)
ASD_National_ADDM_Reshaped_DF

# Visualise: **Prevalence Estimates by Sex [ Source: ADDM ] [ Year: 2014 ]**

# Adjust in-line plot size to M x N
options(repr.plot.width=8, repr.plot.height=3)

ggplot(ASD_National_ADDM_Reshaped_DF, aes(Sex.Group, Prevalence)) +
  geom_col(aes(fill = Sex.Group), alpha=0.5) + # Use column chart
  geom_text(aes(label = Prevalence), vjust = +0.75, hjust = -0.2, size = 3) +
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

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Data Visualisation (Enhanced) - <span style="color:blue">[ R ] REPORTED PREVALENCE VARIES BY SEX</span> [ Source: ADDM ] [ Year: ALL ]
#     </h3>
# </div>
#

# Create a new datafarme to hold re-shaped data for all years.
ASD_National_ADDM_Reshaped_DF_All = ASD_National_ADDM_Reshaped_DF # Loaded with initial [ Year: 2014 ] data

Process_Source = 'addm'
unique(ASD_National_ADDM$Year)

# **Use defined function <span style="color:blue">Function_Reshape_ASD_National_ADDM( )</span> for <span style="color:blue">ALL remaining</span> years:**

ASD_National_ADDM_Reshaped_DF <- Function_Reshape_ASD_National_ADDM(Process_Source = 'addm', Process_Year = 2012)
ASD_National_ADDM_Reshaped_DF
# Append rows to existing dataframe, using Row Bind function: rbind()
ASD_National_ADDM_Reshaped_DF_All = rbind(ASD_National_ADDM_Reshaped_DF_All, ASD_National_ADDM_Reshaped_DF)

ASD_National_ADDM_Reshaped_DF <- Function_Reshape_ASD_National_ADDM(Process_Source = 'addm', Process_Year = 2010)
ASD_National_ADDM_Reshaped_DF
# Append rows to existing dataframe, using Row Bind function: rbind()
ASD_National_ADDM_Reshaped_DF_All = rbind(ASD_National_ADDM_Reshaped_DF_All, ASD_National_ADDM_Reshaped_DF)

ASD_National_ADDM_Reshaped_DF <- Function_Reshape_ASD_National_ADDM(Process_Source = 'addm', Process_Year = 2008)
ASD_National_ADDM_Reshaped_DF
# Append rows to existing dataframe, using Row Bind function: rbind()
ASD_National_ADDM_Reshaped_DF_All = rbind(ASD_National_ADDM_Reshaped_DF_All, ASD_National_ADDM_Reshaped_DF)

ASD_National_ADDM_Reshaped_DF <- Function_Reshape_ASD_National_ADDM(Process_Source = 'addm', Process_Year = 2006)
ASD_National_ADDM_Reshaped_DF
# Append rows to existing dataframe, using Row Bind function: rbind()
ASD_National_ADDM_Reshaped_DF_All = rbind(ASD_National_ADDM_Reshaped_DF_All, ASD_National_ADDM_Reshaped_DF)

ASD_National_ADDM_Reshaped_DF <- Function_Reshape_ASD_National_ADDM(Process_Source = 'addm', Process_Year = 2004)
ASD_National_ADDM_Reshaped_DF
# Append rows to existing dataframe, using Row Bind function: rbind()
ASD_National_ADDM_Reshaped_DF_All = rbind(ASD_National_ADDM_Reshaped_DF_All, ASD_National_ADDM_Reshaped_DF)

ASD_National_ADDM_Reshaped_DF <- Function_Reshape_ASD_National_ADDM(Process_Source = 'addm', Process_Year = 2002)
ASD_National_ADDM_Reshaped_DF
# Append rows to existing dataframe, using Row Bind function: rbind()
ASD_National_ADDM_Reshaped_DF_All = rbind(ASD_National_ADDM_Reshaped_DF_All, ASD_National_ADDM_Reshaped_DF)

ASD_National_ADDM_Reshaped_DF <- Function_Reshape_ASD_National_ADDM(Process_Source = 'addm', Process_Year = 2000)
ASD_National_ADDM_Reshaped_DF
# Append rows to existing dataframe, using Row Bind function: rbind()
ASD_National_ADDM_Reshaped_DF_All = rbind(ASD_National_ADDM_Reshaped_DF_All, ASD_National_ADDM_Reshaped_DF)

# Re-shaped ADDM data for ALL years:
ASD_National_ADDM_Reshaped_DF_All

# Visualise: **Prevalence Estimates by Sex [ Source: ADDM ] [ Year: ALL ]**

# Adjust in-line plot size to M x N
options(repr.plot.width=8, repr.plot.height=6)

ggplot(ASD_National_ADDM_Reshaped_DF_All, aes(Sex.Group, Prevalence)) +
  geom_col(aes(fill = Sex.Group), alpha=0.75) + # Use column chart
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


# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Data Visualisation (Enhanced) - Histogram (distribution of binned continuous variable)
#     </h3>
# </div>
#

# Adjust in-line plot size to M x N
options(repr.plot.width=8, repr.plot.height=4)

# Create histogram using R graphics
hist(ASD_National$Prevalence)

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

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Data Visualisation (Enhanced) - Density plot (distribution for continuous variable normalized to 100% area under curve)
#     </h3>
# </div>
#

# +
# Adjust in-line plot size to M x N
# options(repr.plot.width=8, repr.plot.height=4)
# -

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

# **< Prevelance distribution by Data Source >**

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

# **< Prevelance distribution by Data Source with split >**

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

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Data Visualisation (Enhanced) - Box plot
#     </h3>
# </div>
#

# Adjust in-line plot size to M x N
options(repr.plot.width=8, repr.plot.height=4)

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


# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Data Visualisation (Enhanced) - Violin plot
#     </h3>
# </div>
#

# +
# Adjust in-line plot size to M x N
# options(repr.plot.width=8, repr.plot.height=4)
# -

# Create plot using ggplot2
ggplot(ASD_National, aes(x = Source, y = Prevalence, fill = Source)) + 
  geom_violin(alpha = 0.5) + 
  scale_y_continuous(name = "Prevalence per 1,000 Children",
                     breaks = seq(0, 30, 5),
                     limits=c(0, 30)) +
  scale_x_discrete(name = "Data Source (Year 2000 - 2016)") +
  ggtitle("National ASD Prevalence by Data Source") +
  theme(title = element_text(face = 'bold.italic', color = "darkslategrey"), 
        axis.title = element_text(face = 'plain', color = "darkslategrey"))


# Create plot using ggplot2
ggplot(ASD_National, aes(x = Source, y = Prevalence, fill = Source)) + 
  geom_violin(alpha = 0.5) + 
  geom_jitter(alpha = 0.5, position = position_jitter(width = 0.1)) + # Overlay datapoints
#  coord_flip() + # Uncomment to flip x-y axis
  scale_y_continuous(name = "Prevalence per 1,000 Children",
                     breaks = seq(0, 30, 5),
                     limits=c(0, 30)) +
  scale_x_discrete(name = "Data Source (Year 2000 - 2016)") +
  ggtitle("National ASD Prevalence by Data Source") +
  theme(title = element_text(face = 'bold.italic', color = "darkslategrey"), 
        axis.title = element_text(face = 'plain', color = "darkslategrey"))


# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Data Visualisation (Enhanced) - Line chart
#     </h3>
# </div>
#

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Data Visualisation (Enhanced) - <span style="color:blue">[ CDC ] REPORTED PREVALENCE HAS CHANGED OVER TIME</span>
#     </h3>
# </div>
#
#

# ![](../reference/CDC_ASD/ADV_addm_Prevalence_Estimates_Over_Time.png)
#

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Data Visualisation (Enhanced) - <span style="color:blue">[ R ] REPORTED PREVALENCE HAS CHANGED OVER TIME</span> [Source: ALL]
#     </h3>
# </div>
#

# Adjust in-line plot size to M x N
options(repr.plot.width=8, repr.plot.height=4)

# +
# ----------------------------------
# Build chart/plot layer by layer
# ----------------------------------

# Define a ggplot graphic object; provide data and x y for use
p <- ggplot(ASD_National, aes(x = Year, y = Prevalence))
# Show plot
p

# -

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

# **Consolidate above code into one chunk:**

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

# **Optionally, display data values/labels:**

# Optionally, displaydata values/labels
p + geom_text(aes(label = round(Prevalence, 1)), # Values are rounded for display
              vjust = "outward", 
              #          nudge_y = 0.2, # optionally life the text
              hjust = "outward", 
              check_overlap = TRUE,
              size = 3, # size of textual data label
              col = 'darkslategrey')

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Data Visualisation (Enhanced) - Dynamic Visualisation with plotly
#     </h3>
# </div>
#

if(!require(knitr)){install.packages("knitr")}
library("knitr")
if(!require(plotly)){install.packages("plotly")}
library("plotly")

# **Create ployly graph object from ggplot graph object:**

p_dynamic <- p
p_dynamic <- ggplotly(p_dynamic)
p_dynamic

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Data Visualisation (Enhanced) - Use themes as aesthetic template
#     </h3>
# </div>
#

if(!require(ggthemes)){install.packages("ggthemes")}
library('ggthemes')

# **Theme of the Economist magazine:**

# Theme of the economist magazine:
p + theme_economist() + scale_colour_economist()

# **Theme of the Wall Street Journal:**

# Theme of the Wall Street Journal:
p + theme_wsj() + scale_colour_wsj("colors6")

# **Dynamic chart with theme of the economist magazine:**

# Dynamic chart with theme of the economist magazine:
p_dynamic <- p + theme_economist() + scale_colour_economist()
p_dynamic <- ggplotly(p_dynamic)
p_dynamic

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Data Visualisation (Enhanced) - <span style="color:blue">[ CDC ] ADDM Network estimates for overall ASD prevalence in US over time</span> [ Source: ADDM ] over [ Year ]
#     </h3>
# </div>
#
#

# ![](../reference/CDC_ASD/ADV_addm_2014_ADDM_Network_estimates_for_overall_ASD_prevalence_in_US_over_time_with_CI.png)
#

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Data Visualisation (Enhanced) - <span style="color:blue">[ R ] ADDM Network estimates for overall ASD prevalence in US over time</span> [ Source: ADDM ] over [ Year ]
#     </h3>
# </div>
#

# Adjust in-line plot size to M x N
options(repr.plot.width=8, repr.plot.height=4)

# Filter only data of ADDM
ASD_National_ADDM <- subset(ASD_National, Source == 'addm')

# +
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
# -

# Add smooth curve to go through date points, using interpolation with splines:
# https://stackoverflow.com/questions/35205795/plotting-smooth-line-through-all-data-points
spline_ADDM_Prevalence <- as.data.frame(spline(ASD_National_ADDM$Year, ASD_National_ADDM$Prevalence))
spline_ADDM_Prevalence_U_CI <- as.data.frame(spline(ASD_National_ADDM$Year, ASD_National_ADDM$Upper.CI))
spline_ADDM_Prevalence_L_CI <- as.data.frame(spline(ASD_National_ADDM$Year, ASD_National_ADDM$Lower.CI))
# Show plot
p + geom_line(data = spline_ADDM_Prevalence, aes(x = x, y = y, color = 'ADDM_Average'), linetype = "solid", size=0.6) + 
  geom_line(data = spline_ADDM_Prevalence_U_CI, aes(x = x, y = y, color = 'ADDM_U_CI'), linetype = 2, size=0.3) +
  geom_line(data = spline_ADDM_Prevalence_L_CI, aes(x = x, y = y, color = 'ADDM_L_CI'), linetype = 2, size=0.3)

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Data Visualisation (Enhanced) - <span style="color:blue">[ R ] REPORTED PREVALENCE VARIES BY SEX</span> [ Source: ADDM ] over [ Year ]
#     </h3>
# </div>
#

# +
# Adjust in-line plot size to M x N
# options(repr.plot.width=8, repr.plot.height=4)

# +
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
# -

# Apply theme
p + theme_economist() + scale_colour_economist() # p + theme_wsj() + scale_colour_wsj("colors6")

# Dynamic chart:
p_dynamic <- p + theme_economist() + scale_colour_economist()
p_dynamic <- ggplotly(p_dynamic)
p_dynamic

# <div class="alert alert-danger alertdanger" style="margin-top: 20px">
#     <h3>
#         Quiz:
#     </h3>
#     <p>
#         Add 95% Confidence Interval to above plot (Use ggplot)
#     </p>
# </div>

# Write your code below and press Shift+Enter to execute 


# Double-click <b>here</b> for the solution.
#
# <!-- The answer is below:
#
# # Write your code below and press Shift+Enter to execute 
# # TBD
#
# -->

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Data Visualisation (Enhanced) - <span style="color:blue">[ CDC ] REPORTED PREVALENCE VARIES BY RACE AND ETHNICITY</span>
#     </h3>
# </div>
#
#

# ![](../reference/CDC_ASD/ADV_addm_Prevalence_Estimates_by_Race_Ethnicity.png)
#

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Data Visualisation (Enhanced) - <span style="color:blue">[ R ] REPORTED PREVALENCE VARIES BY RACE AND ETHNICITY</span> [ Source: ADDM ] With Average
#     </h3>
# </div>
#

# +
# Adjust in-line plot size to M x N
# options(repr.plot.width=8, repr.plot.height=4)

# +
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

# +
# Apply theme
# p + theme_economist() + scale_colour_economist() # p + theme_wsj() + scale_colour_wsj("colors6")
# -

# Dynamic chart:
p_dynamic <- p + theme_economist() + scale_colour_economist()
p_dynamic <- ggplotly(p_dynamic)
p_dynamic

# <div class="alert alert-danger alertdanger" style="margin-top: 20px">
#     <h3>
#         Quiz:
#     </h3>
#     <p>
#         Change above zig-zag lines to spline/smooth lines.
#     </p>
#     <p>
#         Hints: Refer to <span style="color:blue">ADDM Network estimates for overall ASD prevalence in US over time</span>.
#     </p>
# </div>

# Write your code below and press Shift+Enter to execute 


# Double-click <b>here</b> for the solution.
#
# <!-- The answer is below:
#
# # Write your code below and press Shift+Enter to execute 
# # TBD
#
# -->

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <a href="">
#     </a>
# </div>
#

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Data Visualisation (Enhanced) - <span style="color:blue">US. State Level Data Processing</span>
#     </h3>
# </div>
#

# +
# ----------------------------------
# Dataset: US. State Level Children ASD Prevalence
# ----------------------------------

ASD_State    <- read.csv("../dataset/ADV_ASD_State.csv", stringsAsFactors = FALSE)

# Obtain number of rows and number of columns/features/variables
dim(ASD_State)
# Obtain overview (data structure/types)
str(ASD_State)
# -

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Data Visualisation (Enhanced) - <span style="color:blue">US. State Level Data</span> Pre-Process data
#     </h3>
# </div>
#

# **Pre-Process data: Missing data**

# Load required function from packages:
if(!require(naniar)){install.packages("naniar")}
library(naniar)
if(!require(dplyr)){install.packages("dplyr")}
library(dplyr)


# Count missing values in dataframe:
sum(is.na(ASD_State)) # missing data recognised by R (NA)
# Define several offending strings
na_strings <- c("", "No data", "NA", "N A", "N / A", "N/A", "N/ A", "Not Available", "NOt available")
# Replace these defined missing values to R's internal NA
ASD_State = replace_with_na_all(ASD_State, condition = ~.x %in% na_strings)
# Count missing values in dataframe:
sum(is.na(ASD_State))

# **Remove invalid unicode char/string: \x92**

# Remove invalid unicode char/string: \x92
ASD_State$Source_Full1[ASD_State$Source_Full1 == "National Survey of Children\x92s Health"] <- "National Survey of Children's Health"

# **Delete/Drop variable by index: column from 14 to 26, 29, and 30**

cbind(names(ASD_State), c(1:length(names(ASD_State))))

# Delete/Drop variable by index: column from 14 to 26, 29, and 30
# names(ASD_State)
ASD_State <- ASD_State[ -c(14:26, 29, 30) ]

# **Create new variables**

# Create one new variable: Source_UC as uppercase of Source
ASD_State$Source_UC <- paste(toupper(ASD_State$Source))
# Create one new variable: Source_Full3 by combining Source_UC and Source_Full1
ASD_State$Source_Full3 <- paste(ASD_State$Source_UC, ASD_State$Source_Full1)

# **Create one new ordinal categorical variable: Prevalence_Rank2 ("Low", "High") by binning Prevalence**

# +
# Recode Risk into category from Prevalence

# Low [0, 5)
# High [5, +oo) 

ASD_State$Prevalence_Risk2[ASD_State$Prevalence < 5] = "Low"
ASD_State$Prevalence_Risk2[ASD_State$Prevalence >= 5 ] = "High"
#
# head(ASD_State)
# -

# **Create one new ordinal categorical variable: Prevalence_Rank4 ("Low", "Medium", "High", "Very High") by binning Prevalence**

# +
# Recode Risk into category from Prevalence

# Low [0, 5)
# Medium [5, 10)
# High [10, 20)
# Very High [20, +oo) 

ASD_State$Prevalence_Risk4 = "Very High"
ASD_State$Prevalence_Risk4[ASD_State$Prevalence < 20 ] = "High"
ASD_State$Prevalence_Risk4[ASD_State$Prevalence < 10 ] = "Medium"
ASD_State$Prevalence_Risk4[ASD_State$Prevalence < 5] = "Low"
#
# head(ASD_State)
# -

# **Convert to correct data types**

str(ASD_State)

# +
# cbind(names(ASD_State), c(1:length(names(ASD_State))))
# -

# **Convert variables to numeric**

# Convert Prevalence and CIs from categorical/chr to numeric
ix <- 13:33 # define an index
ASD_State[ix] <- lapply(ASD_State[ix], as.numeric)

# **Convert variables to categorical/factor**

# +
# Convert Source from categorical/chr to categorical/factor
ix <- c(1, 7, 8, 9, 10, 34, 35, 36) # define an index
ASD_State[ix] <- lapply(ASD_State[ix], as.factor)

# Create new ordered factor Year_Factor from Year
ASD_State$Year_Factor <- factor(ASD_State$Year, ordered = TRUE)
# -

# **Convert Prevalence_Rank2 & Prevalence_Rank4 to ordered factor**

# Convert to factor
ASD_State$Prevalence_Risk2 = factor(ASD_State$Prevalence_Risk2, ordered=TRUE,
                                           levels=c("Low", "High"))
# Convert to factor
ASD_State$Prevalence_Risk4 = factor(ASD_State$Prevalence_Risk4, ordered=TRUE,
                                           levels=c("Low", "Medium", "High", "Very High"))

# Display unique values (levels) of a factor categrotical 
lapply(select_if(ASD_State, is.factor), levels)

# **Optionally, export the processed dataframe data to CSV file.**

write.csv(ASD_State, file = "../dataset/ADV_ASD_State_R.csv", row.names = FALSE)

# Read back in above saved file:
# ASD_State <- read.csv("../dataset/ADV_ASD_State_R.csv")
# ASD_State$Year_Factor <- factor(ASD_State$Year_Factor, ordered = TRUE) # Convert Year_Factor to ordered.factor
# ASD_State$Prevalence_Risk2 = factor(ASD_State$Prevalence_Risk2, ordered=TRUE, levels=c("Low", "High"))
# ASD_State$Prevalence_Risk4 = factor(ASD_State$Prevalence_Risk4, ordered=TRUE, levels=c("Low", "Medium", "High", "Very High"))


# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Data Visualisation (Enhanced) - <span style="color:blue">US. State Level Data Visualisation</span>
#     </h3>
# </div>
#
#

# ![](../reference/CDC_ASD/ADV_Years_Data_Available.png)
#

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     <span style="color:blue">Above chat shows at data source level, we'd also like to know State level data availbility. How?</span>
#     </h3>
#     <h3>
#     Data Visualisation (Enhanced) - <span style="color:blue">[ R ] Explore the Data</span> [ Years Data Available by State ]
#     </h3>
# </div>
#

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

# **Filter and create dataframe of different data sources, for easy data access**

# Filter and create dataframe of different data sources, for easy data access
ASD_State_ADDM <- subset(ASD_State, Source == 'addm')
ASD_State_MEDI <- subset(ASD_State, Source == 'medi')
ASD_State_NSCH <- subset(ASD_State, Source == 'nsch')
ASD_State_SPED <- subset(ASD_State, Source == 'sped')

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Data Visualisation (Enhanced) - <span style="color:blue">[ R ] Explore the Data</span> Years Data Available by State [ Source: ADDM ]
#     </h3>
# </div>
#

# Adjust in-line plot size to M x N
options(repr.plot.width=8, repr.plot.height=6)

# **Years Data Available by State [ Source: ADDM ]**

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

# <div class="alert alert-danger alertdanger" style="margin-top: 20px">
#     <h3>
#         Quiz:
#     </h3>
#     <p>
#         Create <span style="color:blue">Years Data Available by State [ Source: XXXX ]</span> for other three data sources:
#     </p>
# </div>

# Write your code below and press Shift+Enter to execute 


# Double-click <b>here</b> for the solution.
#
# <!-- The answer is below:
#
# # Write your code below and press Shift+Enter to execute 
# # TBD
#
# -->

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Data Visualisation (Enhanced) - <span style="color:blue">[ R ] REPORTED PREVALENCE VARIES BY GEOGRAPHIC LOCATION (States)</span> Prevalence Estimates by State [ Source: ADDM ]
#     </h3>
# </div>
#

# Adjust in-line plot size to M x N
options(repr.plot.width=8, repr.plot.height=4)

# Visualise: **Prevalence Estimates by State [ Source: ADDM ]**

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
  geom_jitter(alpha = 0.5, position = position_jitter(width = 0.1)) # Add actual data points

# Show plot
p

# +
# Theme of the economist magazine:
# p + theme_economist() + scale_colour_economist() + theme(legend.position = 'none')
# -

# Dynamic chart
p_dynamic <- p + theme_economist() + scale_colour_economist() + theme(legend.position = 'none')
p_dynamic <- ggplotly(p_dynamic)
p_dynamic

# <div class="alert alert-danger alertdanger" style="margin-top: 20px">
#     <h3>
#         Quiz:
#     </h3>
#     <p>
#         Create <span style="color:blue">Prevalence Estimates by State [ Source: XXXX ]</span> for other three data sources:
#     </p>
# </div>

# Write your code below and press Shift+Enter to execute 


# Double-click <b>here</b> for the solution.
#
# <!-- The answer is below:
#
# # Write your code below and press Shift+Enter to execute 
# # TBD
#
# -->

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Data Visualisation (Enhanced) - <span style="color:blue">[ R ] US. State Level</span> No. Children Surveyed by State [ Source: ADDM ] [Year 2014]
#     </h3>
# </div>
#

# Adjust in-line plot size to M x N
options(repr.plot.width=8, repr.plot.height=4)

# Visualise: **No. Children Surveyed by State [ Source: ADDM ] [Year 2014]**

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

# +
# Theme of the economist magazine:
# p + theme_economist() + scale_colour_economist() + theme(legend.position = 'none')
# -

# Dynamic chart
p_dynamic <- p + theme_economist() + scale_colour_economist() + theme(legend.position = 'none')
p_dynamic <- ggplotly(p_dynamic)
p_dynamic

# <div class="alert alert-danger alertdanger" style="margin-top: 20px">
#     <h3>
#         Quiz:
#     </h3>
#     <p>
#         Create <span style="color:blue">No. Children Surveyed by State [ Source: XXXX ] [Year CCYY]</span> for other data sources & years:
#     </p>
# </div>

# Write your code below and press Shift+Enter to execute 


# Double-click <b>here</b> for the solution.
#
# <!-- The answer is below:
#
# # Write your code below and press Shift+Enter to execute 
# # TBD
#
# -->

# <div class="alert alert-danger alertdanger" style="margin-top: 20px">
#     <h3>
#         Quiz:
#     </h3>
#     <p>
#         Create <span style="color:blue">No. ASD Children by State [ Source: XXXX ] [Year CCYY]</span> for other data sources & years:
#     </p>
#     <p>
#         Hint: Use variable: ASD_State_ADDM$Numerator_ASD
#     </p>
# </div>

# Write your code below and press Shift+Enter to execute 


# Double-click <b>here</b> for the solution.
#
# <!-- The answer is below:
#
# # Write your code below and press Shift+Enter to execute 
# # TBD
#
# -->

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Data Visualisation (Enhanced) - <span style="color:blue">[ R ] US. State Level</span> Prevalence Estimates with 95% CI by State [ Source: ADDM ] [ Year 2014 ]
#     </h3>
# </div>
#

# +
# Adjust in-line plot size to M x N
# options(repr.plot.width=8, repr.plot.height=4)
# -

# Visualise: **Prevalence Estimates with 95% CI by State [ Source: ADDM ] [ Year 2014 ]**

# +
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
# -

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

# +
# theme of the economist magazine:
# p + theme_economist() + scale_colour_economist() + scale_colour_discrete(guide = guide_legend(title = "US. States")) + theme(legend.position = 'none')
# -

# Dynamic chart
p_dynamic <- p + theme_economist() + scale_colour_economist() + scale_colour_discrete(guide = guide_legend(title = "US. States")) + theme(legend.position = 'none')
p_dynamic <- ggplotly(p_dynamic)
p_dynamic

# <div class="alert alert-danger alertdanger" style="margin-top: 20px">
#     <h3>
#         Quiz:
#     </h3>
#     <p>
#         Create <span style="color:blue">Prevalence Estimates with 95% CI by State [ Source: ADDM ] [Year CCYY]</span> for other data sources & years:
#     </p>
# </div>

# Write your code below and press Shift+Enter to execute 


# Double-click <b>here</b> for the solution.
#
# <!-- The answer is below:
#
# # Write your code below and press Shift+Enter to execute 
# # TBD
#
# -->

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Data Visualisation (Enhanced) - <span style="color:blue">[ R ] US. State Level</span> Prevalence Estimates over Year [ Source: ADDM ] [ State: AZ-Arizona ]
#     </h3>
# </div>
#

# +
# Adjust in-line plot size to M x N
# options(repr.plot.width=8, repr.plot.height=4)
# -

# Visualise: **Prevalence Estimates over Year [ Source: ADDM ] [ State: AZ-Arizona ]**

# +
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
# -

# Show plot
p

# Theme of the economist magazine:
p + theme_economist() + scale_colour_economist()

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Data Visualisation (Enhanced) - <span style="color:blue">[ R ] US. State Level</span> Prevalence Estimates over Year [ Source: ADDM ] [ State: ALL ]
#     </h3>
# </div>
#

# +
# Adjust in-line plot size to M x N
# options(repr.plot.width=8, repr.plot.height=4)
# -

# Visualise: **Prevalence Estimates over Year [ Source: ADDM ] [ State: ALL ]**

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

# **Split chart by state**

# Show plot in facet_grid
p + facet_grid(facets = . ~ State) + 
  theme(legend.position = "none", # Hide legend
        axis.text.x=element_blank(),  # Hide axis
        axis.ticks.x=element_blank(), # Hide axis
        panel.background = element_blank(), # Remove panel background
        panel.grid.major = element_line(size = 0.1, linetype = 1, colour = "lightgrey")
  ) 

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Data Visualisation (Enhanced) - Plotting on Map
#     </h3>
# </div>
#

# ----------------------------------
# EDA - Visualisation on map
# ----------------------------------
if(!require(usmap)){install.packages("usmap")}
library(usmap) # usmap: Mapping the US

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Data Visualisation (Enhanced) - Plotting on Map <span style="color:blue">[ CDC ] REPORTED PREVALENCE VARIES BY GEOGRAPHIC LOCATION</span>
#     </h3>
# </div>
#
#

# ![](../reference/CDC_ASD/ADV_addm_2014_Prevalence_Estimates_by_Geographic_Area.png)
#

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Data Visualisation (Enhanced) - Plotting on Map <span style="color:blue">[ R ] REPORTED PREVALENCE VARIES BY GEOGRAPHIC LOCATION</span>
#     </h3>
# </div>
#

# **Let's review data availability by data Sources & Years:**
#
# * ASD_State_ADDM in Years: 2000, 2002, 2004, 2006, 2008, 2010, 2012, 2014
#
# * ASD_State_MEDI in Years: 2000 ~ 2012
#
# * ASD_State_NSCH in Years: 2004, 2008, 2012, 2016
#
# * ASD_State_SPED in Years: 2000 ~ 2016
#

# ![](../reference/R/plot_Barplot_Years_Data_Available.png)
#

# ![](../reference/R/plot_Barplot_Years_Data_Available_by_State.png)
#

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Data Visualisation (Enhanced) - Plotting on Map <span style="color:blue">[ R ] REPORTED PREVALENCE VARIES BY GEOGRAPHIC LOCATION</span> [ Source: ADDM ] [ Year: 2014 ]
#     </h3>
# </div>
#

# +
# Adjust in-line plot size to M x N
# options(repr.plot.width=8, repr.plot.height=4)
# -

# **Prepare US State level data:** [ Source: ADDM ] [ Year: 2014 ]

# +
# Prepare data - addm 2014
Map_Data_Source = 'addm' # Available values lowercase: 'addm', 'medi', 'nsch', 'sped'.
Map_Data_Value = 'Prevalence' # variable must be numeric, variable name in 'quotation'. Or else Error: Discrete value supplied to continuous scale

# Uncomment below to use Prevalence of different groups:
# Map_Data_Value = 'Male.Prevalence' # variable must be numeric, variable name in 'quotation'. Or else Error: Discrete value supplied to continuous scale
# Map_Data_Value = 'Female.Prevalence' # variable must be numeric, variable name in 'quotation'. Or else Error: Discrete value supplied to continuous scale
# Map_Data_Value = 'Asian.or.Pacific.Islander.Prevalence' # variable must be numeric, variable name in 'quotation'. Or else Error: Discrete value supplied to continuous scale

Map_Data_Year = 2014 # must be integer
ASD_State_Subset = subset(ASD_State, Source == Map_Data_Source & Year == Map_Data_Year)
# -

# **The usmap package/function requires input data to have a column of  <span style="color:blue">state</span>, or  <span style="color:blue">fips</span>.** (case sensitive)
#
# * state: Name of US state
#
# * fips: FIPS code for either a US state
#
# https://cran.r-project.org/web/packages/usmap/vignettes/mapping.html
#
# https://cran.r-project.org/web/packages/usmap/usmap.pdf

# The usmap package/function requires input data to have a column of 'state', or 'fips'. (case sensitive)
ASD_State_Subset$state = ASD_State_Subset$State 
# Glance
head(ASD_State_Subset)

# Visualise: **Prevalence Estimates by Geographic Area** [ Source: ADDM ] [ Year: 2014 ]

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

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Data Visualisation (Enhanced) - Plotting on Map <span style="color:blue">[ R ] REPORTED PREVALENCE VARIES BY GEOGRAPHIC LOCATION</span> [ Source: NSCH] [ Year: 2004, 2008, 2012, 2016 ]
#     </h3>
# </div>
#

# **Prepare US State level data:** [ Source: NSCH ] [ Year: ALL ]

Map_Data_Source = 'nsch' # Available values lowercase: 'addm', 'medi', 'nsch', 'sped'.
Map_Data_Value = 'Prevalence' # variable must be numeric, variable name in 'quotation'. Or else Error: Discrete value supplied to continuous scale

# Visualise: **Prevalence Estimates by Geographic Area** [ Source: NSCH ] [ Year: 2004 ]

# Prepare data - nsch 2004
Map_Data_Year = 2004 # must be integer
ASD_State_Subset = subset(ASD_State, Source == Map_Data_Source & Year == Map_Data_Year)
ASD_State_Subset$state = ASD_State_Subset$State
# Plot on map
p_map_nsch_2004 <- plot_usmap(data = ASD_State_Subset, values = Map_Data_Value, color = "white", labels = F, label_color = 'white' ) + scale_fill_continuous(na.value = "lightgrey", low="lightblue1", high = "darkblue", name = "Prevalence\nper 1,000\nChildren", limits=c(0, 40) ) + labs(title = paste("Prevalence Estimates by Geographic Area", '\n[ Measure :', Map_Data_Value, "] [ Source :", Map_Data_Source, "] [ Year :", Map_Data_Year, "]"), subtitle = 'https://www.cdc.gov/ncbddd/autism' ) + theme(panel.background = element_rect(color = "white", fill = "white"), legend.position = "right")
p_map_nsch_2004

# Visualise: **Prevalence Estimates by Geographic Area** [ Source: NSCH ] [ Year: 2008 ]

# Prepare data - nsch 2008
Map_Data_Year = 2008 # must be integer
ASD_State_Subset = subset(ASD_State, Source == Map_Data_Source & Year == Map_Data_Year)
ASD_State_Subset$state = ASD_State_Subset$State
p_map_nsch_2008 <- plot_usmap(data = ASD_State_Subset, values = Map_Data_Value, color = "white", labels = F, label_color = 'white' ) + scale_fill_continuous(na.value = "lightgrey", low="lightblue1", high = "darkblue", name = "Prevalence\nper 1,000\nChildren", limits=c(0, 40) ) + labs(title = paste("Prevalence Estimates by Geographic Area", '\n[ Measure :', Map_Data_Value, "] [ Source :", Map_Data_Source, "] [ Year :", Map_Data_Year, "]"), subtitle = 'https://www.cdc.gov/ncbddd/autism' ) + theme(panel.background = element_rect(color = "white", fill = "white"), legend.position = "right")
p_map_nsch_2008

# Visualise: **Prevalence Estimates by Geographic Area** [ Source: NSCH ] [ Year: 2012 ]

# Prepare data - nsch 2012
Map_Data_Year = 2012 # must be integer
ASD_State_Subset = subset(ASD_State, Source == Map_Data_Source & Year == Map_Data_Year)
ASD_State_Subset$state = ASD_State_Subset$State
p_map_nsch_2012 <- plot_usmap(data = ASD_State_Subset, values = Map_Data_Value, color = "white", labels = F, label_color = 'white' ) + scale_fill_continuous(na.value = "lightgrey", low="lightblue1", high = "darkblue", name = "Prevalence\nper 1,000\nChildren", limits=c(0, 40) ) + labs(title = paste("Prevalence Estimates by Geographic Area", '\n[ Measure :', Map_Data_Value, "] [ Source :", Map_Data_Source, "] [ Year :", Map_Data_Year, "]"), subtitle = 'https://www.cdc.gov/ncbddd/autism' ) + theme(panel.background = element_rect(color = "white", fill = "white"), legend.position = "right")
p_map_nsch_2012

# Visualise: **Prevalence Estimates by Geographic Area** [ Source: NSCH ] [ Year: 2016 ]

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

# **Combine multiple plots to show in one page/screen:**

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

# **Export current plot as image file:**

# ----------------------------------
# Export current plot as image file
# ----------------------------------
ggsave("plot Map Prevalence Estimates by Geographic Area [NSCH] [2004-2016].png", 
       width = 60, height = 30, units = 'cm')

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <a href="">
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
#         Choose one of below visualisations/charts, use R to construct the chart nicely.
#     </p>
#     <p>
#         Optionally, enhance it with additional data dimensions to be better than original chart.
#     </p>
# </div>

# https://www.cdc.gov/ncbddd/autism/data/index.html
#
#

# ![](../reference/CDC_ASD/ADV_ASD_Data_Collection_Locations_US_States.png)

# ![](../reference/CDC_ASD/ADV_addm_2014_ADDM_NETWORK_DATA.png)
#

# ![](../reference/CDC_ASD/ADV_addm_Prevalence_Estimates_by_Race_Ethnicity.png)
#

# ![](../reference/CDC_ASD/ADV_addm_2012_Confidence_Intervals_by_Data_Set_Location.png)
#

# Write your code below and press Shift+Enter to execute 


# <div class="alert alert-block alert-info" style="margin-top: 20px">
# </div>
#

# ### Excellent! You have completed the workshop notebook!

# **Connect with the author:**
#
# This notebook was written by [GU Zhan (Sam)](https://sg.linkedin.com/in/zhan-gu-27a82823 "GU Zhan (Sam)").
#
# [Sam](https://www.iss.nus.edu.sg/about-us/staff/detail/201/GU_Zhan "GU Zhan (Sam)") is currently a lecturer in [Institute of Systems Science](https://www.iss.nus.edu.sg/ "NUS-ISS") in [National University of Singapore](http://www.nus.edu.sg/ "NUS"). He devotes himself into pedagogy & andragogy, and is very passionate in inspiring next generation of artificial intelligence lovers and leaders.
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
#     </a>
# </div>
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
#

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <h3>
#     Correlation of Numeric Variables
#     </h3>
# </div>
#

# +
# ----------------------------------
# Correlation of Numeric Variables
# ----------------------------------
cor_df = select_if(ASD_State, is.numeric) # Select only numeric variables
cor_df = cor_df[, colSums(is.na(cor_df)) == 0] #  Select vaariables without NA

# Compute correlation matrix for No-NA numeric variables:
cor_table = cor(cor_df)
cor_table

# +
# ----------------------------------
# Visualise Correlation Matrix
# ----------------------------------

if(!require(corrplot)){install.packages("corrplot")}
library('corrplot')
# -

# Sort on decreasing correlations with Prevalence
cor_table_sorted <- as.matrix(sort(cor_table[,'Prevalence'], decreasing = TRUE))
#
cor_table_sorted

# Select corelations variables based on threshold:
#cor_var_high <- names(which(apply(cor_table_sorted, 1, function(x) abs(x)>0.25)))
cor_var_high <- names(which(apply(cor_table_sorted, 1, function(x) abs(x)>0.05)))
#
cor_var_high

# Visualise:
cor_table_plot <- cor_table[cor_var_high, cor_var_high]
# cor_table_plot
#
corrplot(cor_table_plot, tl.col="black", tl.pos = "lt")

# <div class="alert alert-block alert-info" style="margin-top: 20px">
#     <a href="https://github.com/dd-consulting">
#          <img src="../reference/GZ_logo.png" width="60" align="right">
#     </a>
# </div>
#

# ---


# 2019 12 06
# ----------------------------------
# Hypothesis Test - Mean - Z Test & t.test()
# ----------------------------------

# Analyze/Estimate Population mean of Prevalence ( State: All , Source: ADDM, Year: 2014 )

# Sample mean [1]
mean(ASD_State$Prevalence[ASD_State$Source == 'addm' & ASD_State$Year == 2014])
hist(ASD_State$Prevalence[ASD_State$Source == 'addm' & ASD_State$Year == 2014])
boxplot(ASD_State$Prevalence[ASD_State$Source == 'addm' & ASD_State$Year == 2014])
# Population mean [2]
sum(ASD_State$Numerator_ASD[ASD_State$Source == 'addm' & ASD_State$Year == 2014])
sum(ASD_State$Denominator[ASD_State$Source == 'addm' & ASD_State$Year == 2014])
1000*sum(ASD_State$Numerator_ASD[ASD_State$Source == 'addm' & ASD_State$Year == 2014])/sum(ASD_State$Denominator[ASD_State$Source == 'addm' & ASD_State$Year == 2014])
# (A/B + C/D) Not = (A+C)/(B+D)
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
t_score = qt(p = 0.975, df = length(ASD_State$Prevalence[ASD_State$Source == 'addm' & ASD_State$Year == 2014]) - 1)
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
# Display using t.test() function
t.test(ASD_State$Prevalence[ASD_State$Source == 'addm' & ASD_State$Year == 2014])



# ----------------------------------
# Hypothesis Test - Mean - Z Test & t.test()
# ----------------------------------


# 2019 12 02

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
prop.test(ASD, Children)
prop.test(ASD, Children, correct = FALSE)

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



# 2019 12 05
# ----------------------------------
# EDA - Dynamic Visualization with plotly
# ----------------------------------
library(plotly)
# install latest dev version of ggplot2 to work with plotly:
# install.packages('devtools')
# devtools::install_github('hadley/ggplot2')
library(ggplot2)

p <- ggplot(ASD_National, aes(x = Source, y = Prevalence)) + 
  geom_boxplot(fill = 'darkslategrey', alpha = 0.2)

p <- ggplotly(p)


library(plotly)

set.seed(1234)
dat <- data.frame(cond = factor(rep(c("A","B"), each=200)), rating = c(rnorm(200),rnorm(200, mean=.8)))

p <- ggplot(dat, aes(x=cond, y=rating, fill=cond)) + geom_boxplot()

library(plotly)
p = p + captions
p <- ggplotly(p)
p




# binning
# factor to numeric
b <- c(-Inf, 10, 20, 30, Inf) 
names <- c(5, 15, 25, 35)

ASD_State_Subset$Prevalence_bin = cut(ASD_State_Subset$Prevalence, breaks = b, labels = names)
ASD_State_Subset$Prevalence = as.numeric(as.character(cut(ASD_State_Subset$Prevalence, breaks = b, labels = names)))





setwd("/media/sf_vm_shared_folder/git/DDC-ASD/model_R")


library(plotly)
p = ggplot(ASD_National, aes(x = Source, y = Prevalence)) + 
  geom_boxplot(fill = 'darkslategrey', alpha = 0.2) + 
  scale_y_continuous(name = "Prevalence per 1000 Children",
                     breaks = seq(0, 30, 5),
                     limits=c(0, 30)) +
  scale_x_discrete(name = "Data Source (Year 2000 - 2016)") +
  ggtitle("National ASD Prevalence by Data Source") +
  theme(title = element_text(face = 'bold.italic', color = "darkslategrey"), 
        axis.title = element_text(face = 'plain', color = "darkslategrey"))


p = p + theme_economist() + scale_colour_economist()

p <- ggplotly(p)
p



# ----------------------------------
# Dynamic Visualization with plotly
# ----------------------------------
if(!require(plotly)){install.packages("plotly")}
library(plotly)
p_dynamic <- ggplotly(p)
p_dynamic


# Dynamic chart
p_dynamic <- ggplotly(p)
p_dynamic

library('ggthemes')
library('ggplot2')
library('usmap')
library('plotly')


devtools::install_github('hadley/ggplot2')
# Dynamic chart
p_dynamic <- p_map
p_dynamic <- ggplotly(p_dynamic)
p_dynamic




# binning
# factor to numeric
b <- c(-Inf, 10, 20, 30, Inf) 
names <- c(5, 15, 25, 35)

# Create US. State level color groups for map
color_bands <- c(-Inf, 10, 20, 30, Inf) 
color_group <- c('< 10', '10 - 20', '20 - 30', '30 +')
ASD_State_Subset$Prevalence_Map_Color_Group = cut(ASD_State_Subset$Prevalence, breaks = color_bands, labels = color_group)

str(ASD_State_Subset)




# Prepare data
Map_Data_Source = 'addm' # Available values lowercase: 'addm', 'medi', 'nsch', 'sped'.
Map_Data_Year = 2012 # must be integer
Map_Data_Value = 'Prevalence' # variable must be numeric, variable name in 'quotation'. Or else Error: Discrete value supplied to continuous scale

ASD_State_Subset = subset(ASD_State, Source == Map_Data_Source & Year == Map_Data_Year)
ASD_State_Subset$state = ASD_State_Subset$State
# Create US. State level color groups for map
color_bands <- c(-Inf, 10, 20, 30, Inf) 
color_group <- c('< 10', '10 - 20', '20 - 30', '30 +')
ASD_State_Subset$Prevalence_Map_Color_Group = cut(ASD_State_Subset$Prevalence, breaks = color_bands, labels = color_group)

# Show data on map
p_map <- plot_usmap(data = ASD_State_Subset, values = 'Prevalence_bin', color = "white", labels = TRUE, label_color = 'white') + 
  scale_fill_manual(
    aes(group = Prevalence_bin, fill = Prevalence_bin),
    na.value = "lightgrey"
#    colours = c("lightblue1", "lightblue3", "blue1", "blue4"), 
#    low="lightblue", high = "darkblue"
#    values = scales::rescale(c(-Inf, 10, 20, 30, Inf)), 
#    breaks = c(0, 10, 20, 30),
#    guide = guide_colourbar(nbin = 40) ,
    #    low="lightblue1", high = "blue4", name = "Prevalence\nper 1,000\nChildren", label = scales::comma,
#    limits=c(0, 40) #same colour levels/limits for plots
  ) +
  
  labs(title = "Prevalence Estimates by Geographic Area",
       subtitle = paste("Prevalence by US. State    Source [", Map_Data_Source, "] Year [", Map_Data_Year, "]")
  ) + 
  theme(panel.background = element_rect(color = "white", fill = "white"),
        legend.position = "right")
# Show plot
p_map


# Dynamic chart
p_dynamic <- p_map
p_dynamic <- ggplotly(p_dynamic)
p_dynamic


#  scale_fill_gradient(
#    colours = c("lightblue1", "lightblue3", "blue1", "blue4"), 
#    low="lightblue", high = "darkblue",
#    values = scales::rescale(c(-Inf, 10, 20, 30, Inf)), 
#                       breaks = c(0, 10, 20, 30),
#                       guide = guide_colourbar(nbin = 4)
#    )  +
#  scale_fill_gradient(
scale_fill_continuous(
  #    aes(fill = Prevalence_bin),
  na.value = "lightgrey",
  colours = c("lightblue1", "lightblue3", "blue1", "blue4"), 
  #    low="lightblue", high = "darkblue",
  values = scales::rescale(c(-Inf, 10, 20, 30, Inf)), 
  breaks = c(0, 10, 20, 30),
  guide = guide_colourbar(nbin = 4) ,
  #    low="lightblue1", high = "blue4", name = "Prevalence\nper 1,000\nChildren", label = scales::comma,
  limits=c(0, 40) #same colour levels/limits for plots
) +
  
  

##

ASD_State_Subset$Prevalence_bin = as.numeric(as.character(cut(ASD_State_Subset$Prevalence, breaks = b, labels = names)))


# export df to csv
ASD_National

# ----------------------------------
# Optionally, export the processed dataframe data to CSV file.
# ----------------------------------
write.table(ASD_National, file = "../dataset/ASD_National_R.csv", sep = ',', row.names = FALSE)
ASD_National <- read.csv("../dataset/ASD_National_R.csv")
ASD_National$Year_Factor <- factor(ASD_National$Year_Factor, ordered = TRUE) # Convert Year_Factor to ordered.factor


# ----------------------------------
# Optionally, export the processed dataframe data to CSV file.
# ----------------------------------
write.table(ASD_State, file = "../dataset/ASD_State_R.csv", sep = ',', row.names = FALSE)
ASD_State <- read.csv("../dataset/ASD_State_R.csv")
ASD_State$Year_Factor <- factor(ASD_State$Year_Factor, ordered = TRUE) # Convert Year_Factor to ordered.factor

str(ASD_State)




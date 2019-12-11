# Uncomment below to show help

# Change plot size to M x N
options(repr.plot.width=8, repr.plot.height=6)


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
# EDA - Visualisation on map
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
#
# Uncomment below to use Prevalence of different groups:
#
# Map_Data_Value = 'Male.Prevalence' # variable must be numeric, variable name in 'quotation'. Or else Error: Discrete value supplied to continuous scale
# Map_Data_Value = 'Female.Prevalence' # variable must be numeric, variable name in 'quotation'. Or else Error: Discrete value supplied to continuous scale
# Map_Data_Value = 'Asian.or.Pacific.Islander.Prevalence' # variable must be numeric, variable name in 'quotation'. Or else Error: Discrete value supplied to continuous scale
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
ggsave("plot Map Prevalence Estimates by Geographic Area [NSCH] [2004-2016].png", width = 60, height = 30, units = 'cm')


# ----------------------------------
# Sampling & Normality
# ----------------------------------

# ----------------------------------
# Use sd()   to calculate *sample* std-dev (S)
# Use sd.p() to calculate *population* std-dev (Omega)
# ----------------------------------
# Define a function sd.p() to calculate *population* std-dev (Omega)
# https://www.dummies.com/education/math/statistics/standard-deviation-r/
sd.p = function(x) {sd(x) * sqrt((length(x)-1)/length(x))}
# Treat as population:
sd.p(ASD_State$Denominator)
# Treat as sample:
sd(ASD_State$Denominator)

# ----------------------------------
# Create a *Population* of US. State level ASD Prevalence from Source SPED in Year 2016 
# ----------------------------------
ASD_State_SPED_2016 <- subset(ASD_State, Source == 'sped' & Year == 2016, select=c('State', 'Prevalence'))
dim(ASD_State_SPED_2016)
# *Population* mean Prevalence
mean(ASD_State_SPED_2016$Prevalence)

# ----------------------------------
# Central Limit Theorem (CLT)
# ----------------------------------
# Create a *Sample* from ASD_State_SPED_2016$Prevalence,
# with sample size n =
clt_n = 20
# clt_n = 40

set.seed(88)
clt_sample_1 = sample(x = ASD_State_SPED_2016$Prevalence, size = clt_n, replace = TRUE)
clt_sample_1
plot(density(clt_sample_1), col="darkgrey", lwd=2) 
hist(clt_sample_1, probability = T, add = T)

# Repeatedly sample for k times, create a matrix/array to store these samples
clt_k = 10000
set.seed(88)
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

# Calculate sample std-dev value for k samples
clt_sample_k_sd <- apply(clt_sample_k, 2, sd)

# Show first few samples' std-dev
clt_sample_k_sd[1:6]

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
#
# Note that the above two means are close.
#

# ----------------------------------
# Visualisation: Central Limit Theorem (CLT)
# ----------------------------------

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

# < How to make transparent colors in R >
# https://www.dataanalytics.org.uk/make-transparent-colors-in-r/
# col2rgb(c("cyan", "grey", "red")) / 255

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
shapiro.test(sample(x = clt_sample_k_mean, size = 1000))
shapiro.test(ASD_State_SPED_2016$Prevalence)


# ----------------------------------
# Estimation : Mean & Confidence Interval (CI)
# ----------------------------------

# ----------------------------------
# Use a sample of a few US. State's ASD prevalence (mean) to estimate:
# Average prevalence of ALL US. States (the *Population*) [Source SPED, Year 2016]
# ----------------------------------
ASD_State_SPED_2016
dim(ASD_State_SPED_2016)

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
# Calculate Confidence Interval for mean
# ----------------------------------

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


# ----------------------------------
# Quiz: Obtain CI using smaller/larger sample size (clt_n) at 99% confidence. Compare CI width.
# Observe: larger sample size, narrower the CI (more confident at xx% level)
# ----------------------------------
# Write your code here:
#


# ----------------------------------
# Estimation : Proportion & Confidence Interval (CI)
# ----------------------------------

# ----------------------------------
# Use a sample of one US. State's ASD prevalence (proportion) to estimate:
# Prevalence of THAT US. State's ALL Children (the *Population*) [Source SPED, Year 2016] 
# ----------------------------------

# ----------------------------------
# CI using Z score interval  (standard normal distribution)
# https://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval
# ----------------------------------
# No. Children with ASD
ASD  <- ASD_State_SPED$Numerator_ASD[ASD_State_SPED$Year == 2016]
# No. Children with ASD of first US. State (AL-Alabama)
ASD[1]
# No. Children surveyed
Children  <- ASD_State_SPED$Denominator[ASD_State_SPED$Year == 2016]
# No. Children surveyed of first US. State (AL-Alabama)
Children[1]
# sample proportion of first US. State (AL-Alabama) in year 2016 of source SPED
sample_proportion = mean(ASD[1]/Children[1])
sample_proportion
# sample size n
sample_size_n = Children[1]
sample_size_n
# 95% quantile (z score)
z_score = qnorm(p = 0.975)
z_score
# CI using Z distribution
# https://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval
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

# Based on above calculation, we have 95% confidence that:
# The actual AL-Alabama state level prevalence (if ALL childrens
# were surveyed) would be in the above calculated CI range 95% times.


# ----------------------------------
# CI using Wilson score interval
# https://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval
# ----------------------------------

ASD[1]
Children[1]
# Yates' chi-squared test = Wilson score interval with continuity correction - prop.test
prop.test(ASD[1], Children[1]) 
prop.test(ASD[1], Children[1], conf.level = 0.95) 
# Pearson's chi-squared test = Wilson score interval - wilson
prop.test(ASD[1], Children[1], conf.level = 0.95, correct = FALSE) 


# ----------------------------------
# Quiz: Obtain CI of Male.Prevalence at 99% confidence.
# ----------------------------------
# Write your code here:
#


# ----------------------------------
# Quiz: Obtain CI of Female.Prevalence at 99% confidence.
# Then Compare CI range with Male children's CI range.
# Which gender has statistically higer ASD prevalence/proportion?
# ----------------------------------
# Write your code here:
#



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
# Interactive workshops: < Learning R inside R > using swirl()
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
?nnet

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
prop.test(ASD, Children) # Yates' chi-squared test = Wilson score interval with continuity correction - prop.test
prop.test(ASD, Children, correct = FALSE) # Pearson's chi-squared test = Wilson score interval - wilson


# ----------------------------------
# Appendix - End
# ----------------------------------

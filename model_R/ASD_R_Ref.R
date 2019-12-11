# Uncomment below to show help


# Change plot size to M x N
options(repr.plot.width=8, repr.plot.height=6)


 <span style="color:blue">




# ----------------------------------
# Sampling & Normality
# ----------------------------------





# WIP 1 from here:



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





# WIP 2 from here:



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

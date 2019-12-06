library(ggplot2)
x <- seq(1992, 2002, by=2)

d1 <- data.frame(x=x, y=rnorm(length(x)))
xy <- expand.grid(x=x, y=x)
d2 <- data.frame(x=xy$x, y=xy$y, z= jitter(xy$x + xy$y))

d1$panel <- "a"
d2$panel <- "b"
d1$z <- d1$x

d <- rbind(d1, d2)

p <- ggplot(data = d, mapping = aes(x = x, y = y)) + 
  facet_grid(panel~., scale="free") + 
  geom_line(data = d1, stat = "identity") + 
  geom_tile(data=d2, mapping=aes(colour=z, fill=z), stat = "identity")
p



# 2019 12 06
# ----------------------------------
# Hypothesis Test - Mean - Z Test & t.test()
# ----------------------------------
# mean of Prevalence
# State ADDM 2014
# [1]
mean(ASD_State$Prevalence[ASD_State$Source == 'addm' & ASD_State$Year == 2014])
hist(ASD_State$Prevalence[ASD_State$Source == 'addm' & ASD_State$Year == 2014])
boxplot(ASD_State$Prevalence[ASD_State$Source == 'addm' & ASD_State$Year == 2014])
# [2]
sum(ASD_State$Numerator_ASD[ASD_State$Source == 'addm' & ASD_State$Year == 2014])
sum(ASD_State$Denominator[ASD_State$Source == 'addm' & ASD_State$Year == 2014])
1000*sum(ASD_State$Numerator_ASD[ASD_State$Source == 'addm' & ASD_State$Year == 2014])/sum(ASD_State$Denominator[ASD_State$Source == 'addm' & ASD_State$Year == 2014])
# (A/B + C/D) Not = (A+C)/(B+D)
# [3]
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
p
p <- ggplotly(p)



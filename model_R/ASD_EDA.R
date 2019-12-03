

?prop.test


ASD  <- c( 295, 283, 118, 294 ) # 
Children <- c( 45322, 43593, 21532, 29714 )
prop.test(ASD, Children)

# State	Denominator	Prevalence	Lower CI	Upper CI	Year	Source	State_Full	Numerator_Prevalence
# AZ	45,322	6.5	5.8	7.3	2000	addm	Arizona	295
# GA	43,593	6.5	5.8	7.3	2000	addm	Georgia	283
# MD	21,532	5.5	4.6	6.6	2000	addm	Maryland	118
# NJ	29,714	9.9	8.9	11.1	2000	addm	New Jersey	294

ASD  <- c( 295, 283, 118, 294 )
Children <- c( 45322, 43593, 21532, 29714 )
prop.test(ASD, Children)

qnorm(p=0.975)

ASD  <- c( 295 )
Children <- c( 45322 )
prop.test(ASD, Children, correct = FALSE)
prop.test(ASD, Children)


if(!require(binom)){install.packages("binom")}
library(binom)
binom.confint (x=295, n=45322, conf.level =0.95, method="all")


ASD  <- c( 283 )
Children <- c( 43593 )
prop.test(ASD, Children)

ASD  <- c( 118 )
Children <- c( 21532 )
prop.test(ASD, Children)

ASD  <- c( 294 )
Children <- c( 29714 )
prop.test(ASD, Children)
prop.test(ASD, Children, correct = F)


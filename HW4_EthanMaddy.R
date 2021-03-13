# ECON 5043
# Ethan Maddy
# HW4 - Computational Questions 
# 3/12/2021

rm(list=ls())

# Question no. 2
# a.

# load df
df <- read.csv("CEX.csv", header=T)

# median, mean, & standard deviation
summary(df$fruitveg)
sd(df$fruitveg)

# median = 6.17
# mean = 12.17
# sd = 17.12968

summary(df$fruitveg[which(df$week == 1)])
# median(week 1) = 12.622

summary(df$fruitveg[which(df$week == 2)])
# median(week 2) = 5.834

length(df$fruitveg[which(df$fruitveg == 0)])
# 3414 spend $0 on fresh fruit and vegetables 

# b.
hist(df$fruitveg, breaks = 25)

# c. 
df$ihsfruitveg <- asinh(df$fruitveg)
hist(df$ihsfruitveg, breaks = 25)

# d. 

library(geoR)

ybar = mean(df$ihsfruitveg)
s_sq = var(df$ihsfruitveg)

# Bayesian analysis 
samp = length(df$ihsfruitveg)
df2 = samp-1
total <- 1000

# Placeholder vectors 
post_norm_var<-rep(0,total)
post_norm_mu<-rep(0,total)

# Loop
for (i in 1:total){
  post_norm_var[i]=rinvchisq(n=1, df2, scale=s_sq)
  post_norm_mu[i]=rnorm(n=1,mean=ybar,sd=sqrt(post_norm_var[i]/samp))
}

median(post_norm_mu)
# 2.141523
ybar
# 2.141911

# 95% credible intervals
quantile(post_norm_mu,c(0.025,0.975))
#    2.5%    97.5% 
# 2.108281 2.171842 

quantile(post_norm_var,c(0.025,0.975))
#   2.5%    97.5% 
# 2.720209 2.862325 

# e. 
drawsback <- sinh(post_norm_mu)
hist(drawsback, breaks = 25)


# Question no. 3
# a. 
install.packages("extraDistr")
library(extraDistr)

# 500,000 draws (generalized Student's t)
genstudentt <- rlst(n=500000,df=100,mu=0,sigma=1)

# 500,000 draws (standard Student's t)
studentt<-rt(n=500000,df=100)

# Comparative histograms 
hist(genstudentt, breaks = 25)
hist(studentt, breaks = 25)








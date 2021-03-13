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
studentt <- rt(n=500000,df=100)

# Comparative histograms 
hist(genstudentt, breaks = 25)
hist(studentt, breaks = 25)

# b. 
samp = length(df$ihsfruitveg)
df2 = samp - 1
ybar = mean(var(df$ihsfruitveg))
s_sq = var(df$ihsfruitveg)

# location & scale parameters
location_para = ybar
scale_para = s_sq*sqrt(1 + (1/samp))

# 1000 draws from generalized Student's t
fruit_genstudentt <- rlst(n = 1000, df = df2, mu = location_para, sigma = scale_para)
median(fruit_genstudentt) # 2.723061

# Question no. 4

# load the MCMC package
library(MCMCpack)

# Data: 7 choices for transportation
y1 = 51
y2 = 84
y3 = 101
y4 = 203
y5 = 32
y6 = 20
y7 = 9

n=(y1+y2+y3+y4+y5+y6+y7)

# Hyperparameters
alpha1 = .01
alpha2 = .01
alpha3 = .01
alpha4 = .01
alpha5 = .01
alpha6 = .01
alpha7 = .01

sumalpha=(alpha1+alpha2+alpha3+alpha4+alpha5+alpha6+alpha7)

# 10,000 draws from the posterior
total<-10000

post1<-alpha1+y1
post2<-alpha2+y2
post3<-alpha3+y3
post4<-alpha4+y4
post5<-alpha5+y5
post6<-alpha6+y6
post7<-alpha7+y7

postmat<-rdirichlet(total,c(post1,post2,post3,post4,post5,post6,post7))

# Means of the posterior draws
colMeans(postmat)
# 0.10233115 0.16803706 0.20187300 0.40597315 0.06396545 0.03988502 0.01793518

# Relative frequency 
c(y1/n,y2/n,y3/n,y4/n,y5/n,y6/n,y7/n)
# 0.102 0.168 0.202 0.406 0.064 0.040 0.018

# Side-by-side comparison 

# mean of draws:      0.10233115 0.16803706 0.20187300 0.40597315 0.06396545 0.03988502 0.01793518
# relative frequency: 0.102      0.168      0.202      0.406      0.064      0.040      0.018

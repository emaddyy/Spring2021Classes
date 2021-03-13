# ECON 5043
# Ethan Maddy
# HW3 - Computational Questions 
# 3/4/2021

rm(list=ls())

# read in data 
df <- read.table(file = "Duration.csv", 
                      sep = ",", header=TRUE)

# Question no. 4
# a 

# mean, median, standard deviation
mean(df$Weeks)   # 16.123
median(df$Weeks) # 14.300
sd(df$Weeks)     # 7.159

# mean & standard deviation for different decades

# 1950s
mean(df$Weeks[which(df$Year %in% 1950:1959)]) # 11.3025
sd(df$Weeks[which(df$Year %in% 1950:1959)])   # 2.341172

# 1960s
mean(df$Weeks[which(df$Year %in% 1960:1969)]) # 11.75833
sd(df$Weeks[which(df$Year %in% 1960:1969)])   # 2.715644

# 1970s
mean(df$Weeks[which(df$Year %in% 1970:1979)]) # 11.87583
sd(df$Weeks[which(df$Year %in% 1970:1979)])   # 2.299324

# 1980s
mean(df$Weeks[which(df$Year %in% 1980:1989)]) # 14.98833
sd(df$Weeks[which(df$Year %in% 1980:1989)])   # 2.533048

# 1990s
mean(df$Weeks[which(df$Year %in% 1990:1999)]) # 15.72083
sd(df$Weeks[which(df$Year %in% 1990:1999)])   # 2.218293

# 2000s
mean(df$Weeks[which(df$Year %in% 2000:2009)]) # 17.5475
sd(df$Weeks[which(df$Year %in% 2000:2009)]) # 3.404445

# 2010s
mean(df$Weeks[which(df$Year %in% 2010:2019)]) # 30.8225
sd(df$Weeks[which(df$Year %in% 2010:2019)])   # 6.395777

# 2020
mean(df$Weeks[which(df$Year %in% 2020:2020)]) # 18.15
sd(df$Weeks[which(df$Year %in% 2020:2020)])   # 5.323447

tail(df, n=24)
# Outliers in 2020 could be April and May, both are siginifcantly lower
# than the months prior. (6.1 and 10.4)

# b: see handwritten work
# alpha = .001, beta = .001

# c

# set hyperparameters
alpha = .001
beta = .001
samp = 876
ybar = mean(df$Weeks) # 16.12317

# 10,000 draws from gamma posterior
post_gamma = rgamma(10000, shape = alpha + samp, scale = 1/(samp*ybar + beta))

# Mean & median
mean(post_gamma) # 0.06204949
median(post_gamma) # 0.06203381

# 95% interval
qgamma(c(0.025,0.975),shape = alpha + samp, scale = 1/(samp*ybar + beta))
# [0.05798286 0.06619644]


# d

# less 2020 data
df2 <- df[which(df$Year<2020),]

# set hyperparameters
samp = 864
ybar = mean(df2$Weeks) # 16.09502

# 10,000 draws from gamma posterior
post_gamma = rgamma(10000, shape = alpha + samp, scale = 1/(samp*ybar + beta))

# Mean and median
mean(post_gamma) # 0.06212324
median(post_gamma) # 0.0621114

# 95% interval
qgamma(c(0.025,0.975),shape = alpha + samp, scale = 1/(samp*ybar + beta))
# [0.05805674 0.06634161]



# Question no. 5 

# a 
Y = c(18, 17, 21, 23, 20, 19, 23, 16, 20, 20)
ybar <- mean(Y)
samp <- length(Y)
theta <- 20
v <- (1/samp)*sum((Y-theta)^2) # v = 4.9

# b 

# see handwritten notes 
# df2 = nu0 + 10
# scale2 = (nu0*sig02 + 49)/(nu0 + 10)

# c

library(geoR)

# set up
nu0 = 1
sig02 = 1

# 1,000 draws from posterior
df2 = nu0 + samp
scale2 = (nu0*sig02 + samp*v)/(nu0 + samp)

post_var <- rinvchisq(1000, df=df2, scale=scale2)

# analysis
hist(post_var, breaks = 50)
mean(post_var) # 5.505652
median(post_var) # 4.751879
# mean is closer to the sample variance than the median


# d 

nu0 = 10
sig02 = 1

# 1,000 draws from posterior
df3 = nu0 + samp
scale3 = (nu0*sig02 + samp*v)/(nu0 + samp)

post_var2 <- rinvchisq(1000, df=df3, scale=scale3)

# analysis
hist(post_var2, breaks = 50)
mean(post_var2) # 3.292231
median(post_var2) # 3.044901
# The mean and median drop significantly and are much closer to each other. 
# Additionally, this new median is further away from the sample variance. 
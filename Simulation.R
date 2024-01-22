################################################################################
# Description: This file contains the code to simulate the data handed out to 
# the students. As we wanted to avoid collaboration of the students during the 
# assessment, we created four different groups with slightly different underlying 
# true effects.
################################################################################


# libraries
library(rlist)
library(ggplot2)
library(mvtnorm)
library(fastDummies)

set.seed(1)

################################################################################
# Simulation of the data for group A
################################################################################

# sample function to create datasets for the students
sample.data <- function(seed, n, main.effect, effects = c(1,1,1,1,1,1,1)){
  set.seed(seed)
  
  # simulate all variables from different distributions
  mean <- c(2,3,5)
  sigma11 <- runif(1,0.5,0.6)
  sigma21 <- runif(1,0.3,0.4)
  sigma31 <- runif(1,0.2,0.4)
  sigma12 <- sigma21
  sigma22 <- runif(1,0.5,0.6)
  sigma32 <- runif(1,0.1,0.2)
  sigma13 <- sigma31
  sigma23 <- sigma32
  sigma33 <- runif(1,0.3,0.6)
  sigma <- matrix(c(sigma11,sigma21,sigma31,sigma12,sigma22,sigma32, sigma13,sigma23,sigma33),3,3)
  corr.data <- as.data.frame(rmvnorm(n, mean, sigma, method = "chol"))
  x1 <- corr.data[,1]
  x2 <- corr.data[,2]
  x3 <- corr.data[,3]
  x4 <- rcauchy(n,0,0.2)
  x5 <- rnorm(n, mean = 1, sd = 2) 
  x6 <- runif(n, 0, 2)          
  x7 <- rt(n,10,1)           
  x8 <- rlnorm(n, meanlog = 0, sdlog = 0.5)           
  x9 <- rbeta(n, 10, 3) 
  x10 <- t(rmultinom(n, size = 1, prob = c(0.05,0.5,0.45)))
  x11 <- rbinom(n, 1, 0.5)
  
  # create response with only 4 effects, other variables not included 
  y <- main.effect*x3 +
    effects[1] + 
    effects[2]*x1 + 
    effects[3]*x6 + 
    effects[4]*x10[,2] + 
    effects[5]*x10[,3] + 
    effects[6]*x11 + 
    effects[7]*x1*x11 +
    rnorm(n,0,1.5)                             
  
  # transform categorical variable
  t <- which(x10==1, arr.ind = T)
  tmp <- toupper(names(as.data.frame(x10))[t[order(t[,1]),2]])
  x10 <- tmp
  df <- data.frame(y=y,x1=x1,x2=x2,x3=x3,x4=x4,x5=x5,x6=x6,x7=x7,x8=x8,x9=x9,
                  x10=x10,x11=x11)
  return(df)
}


# sample data
df1 <- sample.data(1,350, 0.4, c(0.5,-0.2,-0.1,0.15,0.2,-1,0.4))
df2 <- sample.data(2,350, 0.4, c(-0.5,0.2,0.1,-0.15,-0.2,1,-0.4))
df3 <- sample.data(3,350, 0.4, c(0.5,-0.2,-0.1,0.15,0.2,-1,0.4))
df4 <- sample.data(40,350, 0.4, c(-0.5,0.2,0.1,-0.15,-0.2,1,-0.4))
df5 <- sample.data(5,350, 0.4, c(0.5,-0.2,-0.1,0.15,0.2,-1,0.4))
df6 <- sample.data(6,350, 0.4, c(-0.5,0.2,0.1,-0.15,-0.2,1,-0.4))
df7 <- sample.data(7,350, 0.4, c(0.5,-0.2,-0.1,0.15,0.2,-1,0.4))

# check models 
summary(lm(y~x3+x1+x6+factor(x10)+factor(x11)+x1:factor(x11), data=df1))
summary(lm(y~x3+x1+x6+factor(x10)+factor(x11)+x1:factor(x11), data=df2))
summary(lm(y~x3+x1+x6+factor(x10)+factor(x11)+x1:factor(x11), data=df3))
summary(lm(y~x3+x1+x6+factor(x10)+factor(x11)+x1:factor(x11), data=df4))
summary(lm(y~x3+x1+x6+factor(x10)+factor(x11)+x1:factor(x11), data=df5))
summary(lm(y~x3+x1+x6+factor(x10)+factor(x11)+x1:factor(x11), data=df6))
summary(lm(y~x3+x1+x6+factor(x10)+factor(x11)+x1:factor(x11), data=df7))


# function to create missings at random (MAR)
mar <- function(df, variable1, variable2, quant) {
  ind <- which(df[,variable1]<quantile(df[,variable2],quant))
  df[ind,variable1] <- NA
  return(df)
}

# include missings in the data
df1 <- mar(df1,"x3", "x2", 0.95)
df1 <- mar(df1,"x1", "x6", 0.45)
summary(df1)

df2 <- mar(df2,"x3", "x1", 0.999)
df2 <- mar(df2,"x2", "x1", 0.5)
summary(df2)

df3 <- mar(df3,"x3", "x5", 0.95)
df3 <- mar(df3,"x2", "x1", 0.5)
summary(df3)

df4 <- mar(df4,"x3", "x2", 0.95)
df4 <- mar(df4,"x1", "x5", 0.5)
summary(df4)

df5 <- mar(df5,"x3", "x2", 0.95)
df5 <- mar(df5,"x1", "x2", 0.01)
summary(df5)

df6 <- mar(df6,"x3", "x1", 0.99)
df6 <- mar(df6,"x2", "x1", 0.75)
summary(df6)

df7 <- mar(df7,"x3", "x5", 0.95)
df7 <- mar(df7,"x1", "x5", 0.5)
summary(df7)

# check models
summary(lm(y~x3+x1+x6+factor(x10)+factor(x11)+x1:factor(x11), data=df1))
summary(lm(y~x3+x1+x6+factor(x10)+factor(x11)+x1:factor(x11), data=df2))
summary(lm(y~x3+x1+x6+factor(x10)+factor(x11)+x1:factor(x11), data=df3))
summary(lm(y~x3+x1+x6+factor(x10)+factor(x11)+x1:factor(x11), data=df4))
summary(lm(y~x3+x1+x6+factor(x10)+factor(x11)+x1:factor(x11), data=df5))
summary(lm(y~x3+x1+x6+factor(x10)+factor(x11)+x1:factor(x11), data=df6))
summary(lm(y~x3+x1+x6+factor(x10)+factor(x11)+x1:factor(x11), data=df7))

# create final dataset files
write.table(df1, "df1.txt", append = FALSE, sep = " ", dec = ".",
            row.names = TRUE, col.names = TRUE)
write.table(df2, "df2.txt", append = FALSE, sep = " ", dec = ".",
            row.names = TRUE, col.names = TRUE)
write.table(df3, "df3.txt", append = FALSE, sep = " ", dec = ".",
            row.names = TRUE, col.names = TRUE)
write.table(df4, "df4.txt", append = FALSE, sep = " ", dec = ".",
            row.names = TRUE, col.names = TRUE)
write.table(df5, "df5.txt", append = FALSE, sep = " ", dec = ".",
            row.names = TRUE, col.names = TRUE)
write.table(df6, "df6.txt", append = FALSE, sep = " ", dec = ".",
            row.names = TRUE, col.names = TRUE)
write.table(df7, "df7.txt", append = FALSE, sep = " ", dec = ".",
            row.names = TRUE, col.names = TRUE)


################################################################################
# Simulation of the data for group B
################################################################################

# sample function to create datasets for the students
sample.data <- function(seed, n, main.effect, effects = c(1,1,1,1,1,1,1)){
  set.seed(seed)
  
  # simulate all variables from different distributions
  mean <- c(0,0,0)
  sigma11 <- runif(1,0.5,0.6)
  sigma21 <- runif(1,0.3,0.4)
  sigma31 <- runif(1,0.2,0.4)
  sigma12 <- sigma21
  sigma22 <- runif(1,0.5,0.6)
  sigma32 <- runif(1,0.1,0.2)
  sigma13 <- sigma31
  sigma23 <- sigma32
  sigma33 <- runif(1,0.3,0.6)
  sigma <- matrix(c(sigma11,sigma21,sigma31,sigma12,sigma22,sigma32, sigma13,sigma23,sigma33),3,3)
  corr.data <- as.data.frame(rmvnorm(n, mean, sigma, method = "chol"))
  x1 <- corr.data[,1]
  x2 <- corr.data[,2]
  x3 <- corr.data[,3]
  x4 <- rcauchy(n,0,0.2)
  x5 <- rnorm(n, mean = 1, sd = 2) 
  x6 <- runif(n, 0, 2)          
  x7 <- rt(n,10,1)           
  x8 <- rlnorm(n, meanlog = 0, sdlog = 0.5)           
  x9 <- rbeta(n, 10, 3) 
  x10 <- t(rmultinom(n, size = 1, prob = c(0.33,0.33,0.34)))
  x11 <- rbinom(n, 1, 0.35)
  
  # create response with only 4 effects, other variables not included 
  y <- main.effect*x3 +
    effects[1] + 
    effects[2]*x1 + 
    effects[3]*x6 + 
    effects[4]*x10[,2] + 
    effects[5]*x10[,3] + 
    effects[6]*x11 + 
    effects[7]*x1*x11 +
    rnorm(n,0,1.3)                             
  
  # transform categorical variable
  t <- which(x10==1, arr.ind = T)
  tmp <- toupper(names(as.data.frame(x10))[t[order(t[,1]),2]])
  x10 <- tmp
  df <- data.frame(y=y,x1=x1,x2=x2,x3=x3,x4=x4,x5=x5,x6=x6,x7=x7,x8=x8,x9=x9,
                  x10=x10,x11=x11)
  return(df)
}

# sample data
df8 <- sample.data(8,350, 0.5,      c(-0.2,0.3,-0.5,-0.3,-0.19,0.5,0.9))
df9 <- sample.data(93,350, 0.5,     c(-0.2,0.3,0.5,-0.3,-0.19,0.5,0.6))
df10 <- sample.data(10,350, 0.5,    c(-0.2,0.3,-0.5,-0.3,-0.19,0.5,0.6))
df11 <- sample.data(18601,350, 0.5, c(-0.2,0.3,0.5,-0.3,-0.19,0.5,0.6))
df12 <- sample.data(121,350, 0.5,   c(-0.2,0.3,-0.5,-0.3,-0.19,0.5,0.6))
df13 <- sample.data(130,350, 0.5,   c(-0.2,0.3,0.5,-0.3,-0.19,0.5,0.6))
df14 <- sample.data(140,350, 0.5,   c(-0.2,0.3,-0.5,-0.3,-0.19,0.5,0.6))

# check models
summary(lm(y~x3+x1+x6+factor(x10)+factor(x11)+x1:factor(x11), data=df8))
summary(lm(y~x3+x1+x6+factor(x10)+factor(x11)+x1:factor(x11), data=df9))
summary(lm(y~x3+x1+x6+factor(x10)+factor(x11)+x1:factor(x11), data=df10))
summary(lm(y~x3+x1+x6+factor(x10)+factor(x11)+x1:factor(x11), data=df11))
summary(lm(y~x3+x1+x6+factor(x10)+factor(x11)+x1:factor(x11), data=df12))
summary(lm(y~x3+x1+x6+factor(x10)+factor(x11)+x1:factor(x11), data=df13))
summary(lm(y~x3+x1+x6+factor(x10)+factor(x11)+x1:factor(x11), data=df14))


# function to create outliers
outliers <- function(df,variable, ratio, range) {
  ind <- sample(1:nrow(df), ratio*nrow(df))
  df[ind,variable] <- df[ind,variable] + runif(length(ind),range[1],range[2])
  return(df)
}

# include outliers in the data
df8 <- outliers(df8,"x1",0.1,c(3,4))
df8 <- outliers(df8,"x6",0.1,c(5,7))

df9 <- outliers(df9,"x1",0.15,c(-4,-3))
df9 <- outliers(df9,"x6",0.15,c(1,3))

df10 <- outliers(df10,"x1",0.1,c(3,4))
df10 <- outliers(df10,"x6",0.1,c(2,4))

df11 <- outliers(df11,"x1",0.15,c(-4,-3))
df11 <- outliers(df11,"x6",0.15,c(2,3))

df12 <- outliers(df12,"x1",0.1,c(-5,3))
df12 <- outliers(df12,"x6",0.1,c(2,5))

df13 <- outliers(df13,"x6",0.15,c(4,5))
df13 <- outliers(df13,"x1",0.15,c(2,3))

df14 <- outliers(df14,"x1",0.15,c(4,5))
df14 <- outliers(df14,"x6",0.15,c(2,3))

# check models including outliers
summary(lm(y~x3+x1+x6+factor(x10)+factor(x11)+x1:factor(x11), data=df8))
summary(lm(y~x3+x1+x6+factor(x10)+factor(x11)+x1:factor(x11), data=df9))
summary(lm(y~x3+x1+x6+factor(x10)+factor(x11)+x1:factor(x11), data=df10))
summary(lm(y~x3+x1+x6+factor(x10)+factor(x11)+x1:factor(x11), data=df11))
summary(lm(y~x3+x1+x6+factor(x10)+factor(x11)+x1:factor(x11), data=df12))
summary(lm(y~x3+x1+x6+factor(x10)+factor(x11)+x1:factor(x11), data=df13))
summary(lm(y~x3+x1+x6+factor(x10)+factor(x11)+x1:factor(x11), data=df14))

# summary statistics of the data
summary(df8)
summary(df9)
summary(df10)
summary(df11)
summary(df12)
summary(df13)
summary(df14)

# create final dataset files
write.table(df8, "df8.txt", append = FALSE, sep = " ", dec = ".",
            row.names = TRUE, col.names = TRUE)
write.table(df9, "df9.txt", append = FALSE, sep = " ", dec = ".",
            row.names = TRUE, col.names = TRUE)
write.table(df10, "df10.txt", append = FALSE, sep = " ", dec = ".",
            row.names = TRUE, col.names = TRUE)
write.table(df11, "df11.txt", append = FALSE, sep = " ", dec = ".",
            row.names = TRUE, col.names = TRUE)
write.table(df12, "df12.txt", append = FALSE, sep = " ", dec = ".",
            row.names = TRUE, col.names = TRUE)
write.table(df13, "df13.txt", append = FALSE, sep = " ", dec = ".",
            row.names = TRUE, col.names = TRUE)
write.table(df14, "df14.txt", append = FALSE, sep = " ", dec = ".",
            row.names = TRUE, col.names = TRUE)

################################################################################
# Simulation of the data for group C
################################################################################

# sample function to create datasets for the students
sample.data <- function(seed, n, main.effect, effects = c(1,1,1,1,1,1,1)){
  set.seed(seed)
  
  # simulate all variables from different distributions
  mean <- c(1,1,1)
  sigma11 <- runif(1,0.5,0.6)
  sigma21 <- runif(1,0.3,0.4)
  sigma31 <- runif(1,0.2,0.4)
  sigma12 <- sigma21
  sigma22 <- runif(1,0.5,0.6)
  sigma32 <- runif(1,0.1,0.2)
  sigma13 <- sigma31
  sigma23 <- sigma32
  sigma33 <- runif(1,0.3,0.6)
  sigma <- matrix(c(sigma11,sigma21,sigma31,sigma12,sigma22,sigma32, sigma13,sigma23,sigma33),3,3)
  corr.data <- as.data.frame(rmvnorm(n, mean, sigma, method = "chol"))
  x1 <- corr.data[,1]
  x2 <- corr.data[,2]
  x3 <- corr.data[,3]
  x4 <- rcauchy(n,0,0.2)
  x5 <- rnorm(n, mean = 1, sd = 2) 
  x6 <- runif(n, 0, 2)          
  x7 <- rt(n,10,1)           
  x8 <- rlnorm(n, meanlog = 0, sdlog = 0.5)           
  x9 <- rbeta(n, 10, 3) 
  x10 <- t(rmultinom(n, size = 1, prob = c(0.33,0.33,0.34)))
  x11 <- rbinom(n, 1, 0.35)
  
  # create response with only 4 effects, other variables not included 
  y <- main.effect*x3 +
    effects[1] + 
    effects[2]*x1 + 
    effects[3]*x6 + 
    effects[4]*x10[,2] + 
    effects[5]*x10[,3] + 
    effects[6]*x11 + 
    effects[7]*x1*x11 +
    rnorm(n,0,1.5)                             
  
  # transform categorical variable
  t <- which(x10==1, arr.ind = T)
  tmp <- toupper(names(as.data.frame(x10))[t[order(t[,1]),2]])
  x10 <- tmp
  df <- data.frame(y=y,x1=x1,x2=x2,x3=x3,x4=x4,x5=x5,x6=x6,x7=x7,x8=x8,x9=x9,
                  x10=x10,x11=x11)
  return(df)
}

# sample data
df15 <- sample.data(1811,350, 0.6,    c(0.5,0.25,-0.3, 1,0.1,-0.3,0))
df16 <- sample.data(19111,350, 0.6,   c(0.5,0.25, 0.3,-1,-0.1,-0.3,0))
df17 <- sample.data(201,350, 0.6,     c(0.5,0.25,-0.3, 1,0.1,-0.3,0))
df18 <- sample.data(212,350, 0.6,     c(0.5,0.25, 0.3,-1,-0.1,-0.3,0))
df19 <- sample.data(2121,350, 0.6,    c(0.5,0.25,-0.3, 1,0.1,-0.3,0))
df20 <- sample.data(231,350, 0.6,     c(0.5,0.25, 0.3,-1,-0.1,-0.3,0))
df21 <- sample.data(24,350, 0.6,      c(0.5,0.25,-0.3, 1,0.1,-0.3,0))

# check models 
summary(lm(y~x3+x1+x6+factor(x10)+factor(x11), data=df15))
summary(lm(y~x3+x1+x6+factor(x10)+factor(x11), data=df16))
summary(lm(y~x3+x1+x6+factor(x10)+factor(x11), data=df17))
summary(lm(y~x3+x1+x6+factor(x10)+factor(x11), data=df18))
summary(lm(y~x3+x1+x6+factor(x10)+factor(x11), data=df19))
summary(lm(y~x3+x1+x6+factor(x10)+factor(x11), data=df20))
summary(lm(y~x3+x1+x6+factor(x10)+factor(x11), data=df21))


# function to create outliers
outliers <- function(df,variable, ratio, range) {
  ind <- sample(1:nrow(df), ratio*nrow(df))
  df[ind,variable] <- df[ind,variable] + runif(length(ind),range[1],range[2])
  return(df)
}

# include outliers in the data
df15 <- outliers(df15,"x1",0.1,c(3,4))
df16 <- outliers(df16,"x1",0.1,c(5,7))
df17 <- outliers(df17,"x1",0.15,c(-4,-3))
df18 <- outliers(df18,"x1",0.15,c(1,3))
df19 <- outliers(df19,"x1",0.1,c(3,4))
df20 <- outliers(df20,"x1",0.1,c(2,4))
df21 <- outliers(df21,"x1",0.15,c(-4,-3))

# check models
summary(lm(y~x3+x1+x6+factor(x10)+factor(x11), data=df15))
summary(lm(y~x3+x1+x6+factor(x10)+factor(x11), data=df16))
summary(lm(y~x3+x1+x6+factor(x10)+factor(x11), data=df17))
summary(lm(y~x3+x1+x6+factor(x10)+factor(x11), data=df18))
summary(lm(y~x3+x1+x6+factor(x10)+factor(x11), data=df19))
summary(lm(y~x3+x1+x6+factor(x10)+factor(x11), data=df20))
summary(lm(y~x3+x1+x6+factor(x10)+factor(x11), data=df21))

# Include missings
df15 <- mar(df15,"x3", "x2", 0.25)
df16 <- mar(df16,"x3", "x2", 0.25)
df17 <- mar(df17,"x3", "x2", 0.25)
df18 <- mar(df18,"x3", "x2", 0.25)
df19 <- mar(df19,"x3", "x2", 0.25)
df20 <- mar(df20,"x3", "x2", 0.25)
df21 <- mar(df21,"x3", "x2", 0.25)

# check summary statistics of the data
summary(df15)
summary(df16)
summary(df17)
summary(df18)
summary(df19)
summary(df20)
summary(df21)

# check models 
summary(lm(y~x3+x1+x6+factor(x10)+factor(x11), data=df15))
summary(lm(y~x3+x1+x6+factor(x10)+factor(x11), data=df16))
summary(lm(y~x3+x1+x6+factor(x10)+factor(x11), data=df17))
summary(lm(y~x3+x1+x6+factor(x10)+factor(x11), data=df18))
summary(lm(y~x3+x1+x6+factor(x10)+factor(x11), data=df19))
summary(lm(y~x3+x1+x6+factor(x10)+factor(x11), data=df20))
summary(lm(y~x3+x1+x6+factor(x10)+factor(x11), data=df21))

# create final dataset files
write.table(df15, "df15.txt", append = FALSE, sep = " ", dec = ".",
            row.names = TRUE, col.names = TRUE)

write.table(df16, "df16.txt", append = FALSE, sep = " ", dec = ".",
            row.names = TRUE, col.names = TRUE)

write.table(df17, "df17.txt", append = FALSE, sep = " ", dec = ".",
            row.names = TRUE, col.names = TRUE)

write.table(df18, "df18.txt", append = FALSE, sep = " ", dec = ".",
            row.names = TRUE, col.names = TRUE)

write.table(df19, "df19.txt", append = FALSE, sep = " ", dec = ".",
            row.names = TRUE, col.names = TRUE)

write.table(df20, "df20.txt", append = FALSE, sep = " ", dec = ".",
            row.names = TRUE, col.names = TRUE)

write.table(df21, "df21.txt", append = FALSE, sep = " ", dec = ".",
            row.names = TRUE, col.names = TRUE)


################################################################################
# Simulation of the data for group D
################################################################################

# sample function to create datasets for the students
sample.data <- function(seed, n, main.effect, quadratic, effects = c(1,1,1,1,1,1,1)){
  set.seed(seed)
  
  # simulate all variables from different distributions
  mean <- c(0,0,0)
  sigma11 <- runif(1,0.5,0.6)
  sigma21 <- runif(1,0.3,0.4)
  sigma31 <- runif(1,0.2,0.4)
  sigma12 <- sigma21
  sigma22 <- runif(1,0.5,0.6)
  sigma32 <- runif(1,0.1,0.2)
  sigma13 <- sigma31
  sigma23 <- sigma32
  sigma33 <- runif(1,0.3,0.6)
  sigma <- matrix(c(sigma11,sigma21,sigma31,sigma12,sigma22,sigma32, sigma13,sigma23,sigma33),3,3)
  corr.data <- as.data.frame(rmvnorm(n, mean, sigma, method = "chol"))
  x1 <- corr.data[,1]
  x2 <- corr.data[,2]
  x3 <- corr.data[,3]
  x4 <- rcauchy(n,0,0.2)
  x5 <- rnorm(n, mean = 1, sd = 2) 
  x6 <- runif(n, 0, 2)          
  x7 <- rt(n,10,1)           
  x8 <- rlnorm(n, meanlog = 0, sdlog = 0.5)           
  x9 <- rbeta(n, 10, 3) 
  x10 <- t(rmultinom(n, size = 1, prob = c(0.33,0.33,0.34)))
  x11 <- rbinom(n, 1, 0.35)
  
  # create response with only 4 effects, other variables not included 
  y <- main.effect*x3 +
    effects[1] + 
    effects[2]*x1 + 
    effects[3]*x6 + 
    quadratic*(x6^2) +
    effects[4]*x10[,2] + 
    effects[5]*x10[,3] + 
    effects[6]*x11 + 
    effects[7]*x1*x11 +
    rnorm(n,0,1.2)                             
  
  # transform categorical variable
  t <- which(x10==1, arr.ind = T)
  tmp <- toupper(names(as.data.frame(x10))[t[order(t[,1]),2]])
  x10 <- tmp
  df <- data.frame(y=y,x1=x1,x2=x2,x3=x3,x4=x4,x5=x5,x6=x6,x7=x7,x8=x8,x9=x9,
                  x10=x10,x11=x11)
  return(df)
}

# sample data
df22 <- sample.data(251,350, 0.7,  0.7,   c( 0.05,0.3,0.6, 0.2, 0.2,0.25,0.6))
df23 <- sample.data(269,350, 0.7,  0.5,   c(-0.05,0.4,0.6,-0.2,-0.2,0.25,0.7))
df24 <- sample.data(27,350,  0.7,  0.7,   c( 0.05,0.3,0.6, 0.2, 0.2,0.25,0.6))
df25 <- sample.data(286,350,  0.7,  0.5,   c(-0.05,0.4,0.6,-0.2,-0.2,0.25,0.7))
df26 <- sample.data(29,350,  0.7,  0.7,   c( 0.05,0.3,0.6, 0.2, 0.2,0.25,0.6))
df27 <- sample.data(310,350, 0.7,  0.5,   c(-0.05,0.4,0.6,-0.2,-0.2,0.25,0.7))
df28 <- sample.data(314,350, 0.7,  0.7 ,  c( 0.05,0.3,0.6, 0.2, 0.2,0.25,0.6))

# check models
summary(lm(y~x3+x1+x6+I(x6^2)+factor(x10)+factor(x11)+x1:factor(x11), data=df22))
summary(lm(y~x3+x1+x6+I(x6^2)+factor(x10)+factor(x11)+x1:factor(x11), data=df23))
summary(lm(y~x3+x1+x6+I(x6^2)+factor(x10)+factor(x11)+x1:factor(x11), data=df24))
summary(lm(y~x3+x1+x6+I(x6^2)+factor(x10)+factor(x11)+x1:factor(x11), data=df25))
summary(lm(y~x3+x1+x6+I(x6^2)+factor(x10)+factor(x11)+x1:factor(x11), data=df26))
summary(lm(y~x3+x1+x6+I(x6^2)+factor(x10)+factor(x11)+x1:factor(x11), data=df27))
summary(lm(y~x3+x1+x6+I(x6^2)+factor(x10)+factor(x11)+x1:factor(x11), data=df28))


# include missings
df22 <- mar(df22,"x6", "x3", 0.8) 
df23 <- mar(df23,"x6", "x3", 0.8) 
df24 <- mar(df24,"x1", "x3", 0.25) 
df25 <- mar(df25,"x1", "x3", 0.25) 
df26 <- mar(df26,"x6", "x3", 0.8) 
df27 <- mar(df27,"x6", "x3", 0.8)  
df28 <- mar(df28,"x1", "x3", 0.25) 

# check summary statistics of the data
summary(df22$x6)
summary(df23$x6)
summary(df24$x1)
summary(df25$x1)
summary(df26$x6)
summary(df27$x6)
summary(df28$x1)

# check models
summary(lm(y~x3+x1+x6+I(x6^2)+factor(x10)+factor(x11)+x1:factor(x11), data=df22))
summary(lm(y~x3+x1+x6+I(x6^2)+factor(x10)+factor(x11)+x1:factor(x11), data=df23))
summary(lm(y~x3+x1+x6+I(x6^2)+factor(x10)+factor(x11)+x1:factor(x11), data=df24))
summary(lm(y~x3+x1+x6+I(x6^2)+factor(x10)+factor(x11)+x1:factor(x11), data=df25))
summary(lm(y~x3+x1+x6+I(x6^2)+factor(x10)+factor(x11)+x1:factor(x11), data=df26))
summary(lm(y~x3+x1+x6+I(x6^2)+factor(x10)+factor(x11)+x1:factor(x11), data=df27))
summary(lm(y~x3+x1+x6+I(x6^2)+factor(x10)+factor(x11)+x1:factor(x11), data=df28))

# create final dataset files
write.table(df22, "df22.txt", append = FALSE, sep = " ", dec = ".",
            row.names = TRUE, col.names = TRUE)

write.table(df23, "df23.txt", append = FALSE, sep = " ", dec = ".",
            row.names = TRUE, col.names = TRUE)

write.table(df24, "df24.txt", append = FALSE, sep = " ", dec = ".",
            row.names = TRUE, col.names = TRUE)

write.table(df25, "df25.txt", append = FALSE, sep = " ", dec = ".",
            row.names = TRUE, col.names = TRUE)

write.table(df26, "df26.txt", append = FALSE, sep = " ", dec = ".",
            row.names = TRUE, col.names = TRUE)

write.table(df27, "df27.txt", append = FALSE, sep = " ", dec = ".",
            row.names = TRUE, col.names = TRUE)

write.table(df28, "df28.txt", append = FALSE, sep = " ", dec = ".",
            row.names = TRUE, col.names = TRUE)



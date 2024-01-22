################################################################################
# Description: Code to reproduce the VoE plots from the supplementary materials. 
################################################################################

# load libraries
library(rlist)
library(ggplot2)
library(mvtnorm)
library(fastDummies)
library(broom)
library(dplyr)
library(ggpointdensity)
library(reshape2)
library(viridis)

set.seed(1)

# create a sample dataset for the VoE plot
sample.data <- function(seed, n, main.effect, effects = c(1,1,1,1,1,1,1)){
  set.seed(seed)
  
  # simulate all variables
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

# simulate data
df15 <- sample.data(1811,350, 0.6, c(0.5,0.25,-0.3, 1,0.1,-0.3,0))
summary(lm(y~x3+x1+x6+factor(x10)+factor(x11), data=df15))



### include missings and outliers 

# outlier function
outliers <- function(df,variable, ratio, range) {
  ind <- sample(1:nrow(df), ratio*nrow(df))
  df[ind,variable] <- df[ind,variable] + runif(length(ind),range[1],range[2])
  return(df)
}

# missing function
mar <- function(df, variable1, variable2, quant) {
  ind <- which(df[,variable1]<quantile(df[,variable2],quant))
  df[ind,variable1] <- NA
  return(df)
}

# include missings and outliers in the data
df16 <- outliers(df15,"x1",0.1,c(3,4))
df16 <- mar(df16,"x3", "x2", 0.25)

summary(df16)
summary(lm(y~x3+x1+x6+factor(x10)+factor(x11), data=df16))


# Recode variables as factor
# Note that df15 is the full dataset without any missings etc. and df16 is the 
# dataset including missings etc.
df15
df15$x10 <- as.factor(df15$x10)
df15$x11 <- as.factor(df15$x11)

df16
df16$x10 <- as.factor(df16$x10)
df16$x11 <- as.factor(df16$x11)



################################################################################
### Supplement Figure 3 (full data without missings etc.)
################################################################################

# Create matrix for all possible model specifications
reg <- paste0("x", 1:11) # Create a vector with our variables
interactions <- paste0("x11*", reg[1:9])
regressors <- c(reg,interactions)

reg.matrix <- expand.grid(c(TRUE,FALSE), c(TRUE,FALSE),c(TRUE),c(TRUE,FALSE),
                         c(TRUE,FALSE),c(TRUE,FALSE),c(TRUE,FALSE),c(TRUE,FALSE),
                         c(TRUE,FALSE),c(TRUE,FALSE),c(TRUE,FALSE),
                         c(TRUE,FALSE),c(TRUE,FALSE),c(TRUE,FALSE),c(TRUE,FALSE),
                         c(TRUE,FALSE),c(TRUE,FALSE),c(TRUE,FALSE),c(TRUE,FALSE),
                         c(TRUE,FALSE))

names(reg.matrix) <- regressors                 

set.seed(123)

# randomly sample 10000 model specifications due to computational time
reg.matrix <- reg.matrix[sample(1:nrow(reg.matrix), 10000), ] 

all.models <- apply(reg.matrix, 1, function(x) as.formula(
  paste(c("y ~ 1 ", regressors[x]),
        collapse=" + ")) )

all.results <- lapply(all.models, function(x) lm(x, data=df15))

# save coefficients and p-values
coeff <- c()
p <- c()
for (i in 1:nrow(reg.matrix)){
  coeff <- c(coeff, summary(all.results[[i]])$coefficients["x3",][1] )
  p <- c(p, summary(all.results[[i]])$coefficients["x3",][4])
}

# transform p-values and create ggplot data format
res <- data.frame(p = -log(p,base = 10), coeff = coeff)
melted <- melt(res, measure.vars = c("p","coeff"))

# create plot
d <- ggplot(res, aes(coeff, p))
d + 
  # General
  geom_pointdensity() +
  scale_color_viridis() +
  xlab("Effect size") +
  ylab("-log10(p)") +
  theme_bw() +
  labs(color='Density')+
  
  #P-Values
  geom_hline(yintercept=-log(0.05, base = 10), linetype="dashed", color = "black") +
  geom_text(aes(1.1,-log(0.05,10),label = "0.05", vjust = -1), family = "mono") +
  geom_hline(yintercept=-log(0.001, base = 10), linetype="dashed", color = "black") +
  geom_text(aes(1.1,-log(0.001,10),label = "0.001", vjust = -1),family = "mono") +

  # true effect
  geom_vline(xintercept = 0.7, linetype = "dashed", color = "red") + 

  # quantiles effect
  geom_vline(xintercept = quantile(res$coeff,0.025), linetype = "dashed", color = "violet") +
  geom_text(aes(0.6,0.8,label = "2.5%", vjust = -1), family = "mono", col = "violet") +
  
  geom_vline(xintercept = quantile(res$coeff,0.5), linetype = "dashed", color = "violet") +
  geom_text(aes(0.85,0.8,label = "50%", vjust = -1), family = "mono", col = "violet") +
  
  geom_vline(xintercept = quantile(res$coeff,0.975), linetype = "dashed", color = "violet") + 
  geom_text(aes(1.1,0.8,label = "97.5%", vjust = -1), family = "mono", col = "violet") +
  
  # quantiles p
  geom_hline(yintercept = quantile(res$p,0.025), linetype = "dashed", color = "violet") +
  geom_text(aes(0.3,5.9,label = "2.5%", vjust = -1), family = "mono", col = "violet") +
  
  geom_hline(yintercept = quantile(res$p,0.5), linetype = "dashed", color = "violet") +
  geom_text(aes(0.3,4.2,label = "50%", vjust = -1), family = "mono", col = "violet") +
  
  geom_hline(yintercept = quantile(res$p,0.975), linetype = "dashed", color = "violet")+ 
  geom_text(aes(0.3,1.8,label = "97.5%", vjust = -1), family = "mono", col = "violet") +
  
  # shade area
  annotate("rect", xmin = 0.8, xmax = 1.1, ymin = 1, ymax = 9.2,
           alpha = .1,fill = "blue")
  
ggsave("full_data.png", dpi = 300)



################################################################################
### Supplement Figure 4 (data including missings etc.)
################################################################################

# Create matrix for all possible model specifications
reg <- paste0("x", 1:11) # Create a vector with our variables
interactions <- paste0("x11*", reg[1:9])
regressors <- c(reg,interactions)

reg.matrix <- expand.grid(c(TRUE,FALSE), c(TRUE,FALSE),c(TRUE),c(TRUE,FALSE),
                     c(TRUE,FALSE),c(TRUE,FALSE),c(TRUE,FALSE),c(TRUE,FALSE),
                     c(TRUE,FALSE),c(TRUE,FALSE),c(TRUE,FALSE),
                     c(TRUE,FALSE),c(TRUE,FALSE),c(TRUE,FALSE),c(TRUE,FALSE),
                     c(TRUE,FALSE),c(TRUE,FALSE),c(TRUE,FALSE),c(TRUE,FALSE),
                     c(TRUE,FALSE))

names(reg.matrix) <- regressors                 

set.seed(123)

# randomly sample 10000 model specifications due to computational time
reg.matrix <- reg.matrix[sample(1:nrow(reg.matrix), 10000), ] 

all.models <- apply(reg.matrix, 1, function(x) as.formula(
  paste(c("y ~ 1 ", regressors[x]),
        collapse=" + ")) )

all.results <- lapply(all.models, function(x) lm(x, data=df16))

# save coefficients and p-values
coeff <- c()
p <- c()
for (i in 1:nrow(reg.matrix)){
  coeff <- c(coeff, summary(all.results[[i]])$coefficients["x3",][1] )
  p <- c(p, summary(all.results[[i]])$coefficients["x3",][4])
}

# transform p-values and create ggplot data format
res <- data.frame(p = -log(p,base = 10), coeff = coeff)
melted <- melt(res)

# create plot
d <- ggplot(res, aes(coeff, p))
d + 
  # General
  geom_pointdensity() +
  scale_color_viridis() +
  xlab("Effect size") +
  ylab("-log10(p)") +
  theme_bw() +
  labs(color='Density')+
  
  #P-Values
  geom_hline(yintercept=-log(0.05, base = 10), linetype="dashed", color = "black") +
  geom_text(aes(1.1,-log(0.05,10),label = "0.05", vjust = -1), family = "mono") +
  geom_hline(yintercept=-log(0.001, base = 10), linetype="dashed", color = "black") +
  geom_text(aes(1.1,-log(0.001,10),label = "0.001", vjust = -1),family = "mono") +
  
  # true effect
  geom_vline(xintercept = 0.7, linetype = "dashed", color = "red") + 
  
  # quantiles effect
  geom_vline(xintercept = quantile(res$coeff,0.025), linetype = "dashed", color = "violet") +
  geom_text(aes(0.6,0.8,label = "2.5%", vjust = -1), family = "mono", col = "violet") +
  
  geom_vline(xintercept = quantile(res$coeff,0.5), linetype = "dashed", color = "violet") +
  geom_text(aes(0.85,0.8,label = "50%", vjust = -1), family = "mono", col = "violet") +
  
  geom_vline(xintercept = quantile(res$coeff,0.975), linetype = "dashed", color = "violet") + 
  geom_text(aes(1.1,0.8,label = "97.5%", vjust = -1), family = "mono", col = "violet") +
  
  # quantiles p
  geom_hline(yintercept = quantile(res$p,0.025), linetype = "dashed", color = "violet") +
  geom_text(aes(0.3,5.9,label = "2.5%", vjust = -1), family = "mono", col = "violet") +
  
  geom_hline(yintercept = quantile(res$p,0.5), linetype = "dashed", color = "violet") +
  geom_text(aes(0.3,4.2,label = "50%", vjust = -1), family = "mono", col = "violet") +
  
  geom_hline(yintercept = quantile(res$p,0.975), linetype = "dashed", color = "violet")+ 
  geom_text(aes(0.3,1.8,label = "97.5%", vjust = -1), family = "mono", col = "violet") +
  
  # shade area
  annotate("rect", xmin = 0.8, xmax = 1.1, ymin = 1, ymax = 8,
           alpha = .1,fill = "blue")

ggsave("prob_data.png", dpi = 300)



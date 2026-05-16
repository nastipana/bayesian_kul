#load libraries
library(rjags)
library(readr)
library(coda)
library(runjags)
library(MCMCvis)
library(ggmcmc)
library(basicMCMCplots)

#load data
library(MASS)
data(Insurance)
head(Insurance)

#check levels
levels(Insurance$District)
levels(Insurance$Age)
levels(Insurance$Group)

#make everything into numbers
District <- as.numeric(Insurance$District)
Age <- as.numeric(Insurance$Age)
Group <- as.numeric(Insurance$Group)
Claims <- Insurance$Claims
Holders <- Insurance$Holders
N <- nrow(Insurance)

#centering holders to have less autocorrelation
log_Holders_mean <- mean(log(Holders))

#put data together for JAGS
jags_data <- list(
  Claims = Claims,
  Holders = Holders,
  District = District,
  Age = Age,
  Group = Group,
  N = N,
  log_Holders_mean = log_Holders_mean
)

#____________________ 











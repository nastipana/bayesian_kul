install.packages(c("readr","coda","runjags","MCMCvis","ggmcmc","basicMCMCplots"))

library(readr)
library(coda)
library(runjags)
library(MCMCvis)
library(ggmcmc)
library(basicMCMCplots)

install.packages("rjags")
library(rjags)

library(MASS)
data(Insurance)
head(Insurance)
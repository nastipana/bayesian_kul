# #install packages
# install.packages("readr")
# install.packages("coda")
# install.packages("runjags")
# install.packages("MCMCvis")
# install.packages("ggmcmc")
# install.packages("basicMCMCplots")
# install.packages("rjags")

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

#write the BUGS model to a text file
model_string <- "model {
  for (i in 1:N) {
    Claims[i] ~ dnegbin(p[i], r)
    p[i] <- r / (r + mu[i])
    log(mu[i]) <- (log(Holders[i]) - log_Holders_mean) + beta0
                  + beta1 * equals(District[i], 2)
                  + beta2 * equals(District[i], 3)
                  + beta3 * equals(District[i], 4)
                  + beta4 * equals(Age[i], 2)
                  + beta5 * equals(Age[i], 3)
                  + beta6 * equals(Age[i], 4)
                  + beta7 * equals(Group[i], 2)
                  + beta8 * equals(Group[i], 3)
                  + beta9 * equals(Group[i], 4)
  }
  beta0 ~ dnorm(0, 0.0001)
  beta1 ~ dnorm(0, 0.0001)
  beta2 ~ dnorm(0, 0.0001)
  beta3 ~ dnorm(0, 0.0001)
  beta4 ~ dnorm(0, 0.0001)
  beta5 ~ dnorm(0, 0.0001)
  beta6 ~ dnorm(0, 0.0001)
  beta7 ~ dnorm(0, 0.0001)
  beta8 ~ dnorm(0, 0.0001)
  beta9 ~ dnorm(0, 0.0001)
  r ~ dgamma(0.01, 0.01)
}"
writeLines(model_string, "bay_kul_model.txt")

#second question
#we get some random initial values
my.inits <- function() {
  list(
    beta0 = rnorm(1),
    beta1 = rnorm(1),
    beta2 = rnorm(1),
    beta3 = rnorm(1),
    beta4 = rnorm(1),
    beta5 = rnorm(1),
    beta6 = rnorm(1),
    beta7 = rnorm(1),
    beta8 = rnorm(1),
    beta9 = rnorm(1),
    r = runif(1, 0.5, 5)
  )
}

#parameters to monitor
parameters <- c("beta0", "beta1", "beta2", "beta3", "beta4",
                "beta5", "beta6", "beta7", "beta8", "beta9", "r")

#compile model
jags_model <- jags.model(
  file = "bay_kul_model.txt",
  data = jags_data,
  inits = my.inits,
  n.chains = 3
)



#burn-in
update(jags_model, 1000)

#collect samples
results1 <- coda.samples(
  model = jags_model,
  variable.names = parameters,
  n.iter = 10000,
  thin = 1
)

#diagnostics
traceplot(results1)
gelman.diag(results1)
gelman.plot(results1, ask = FALSE)
effectiveSize(results1)
acfplot(results1)
summary(results1)



#burn-in
update(jags_model, 1000)

#collect samples
results2 <- coda.samples(
  model = jags_model,
  variable.names = parameters,
  n.iter = 10000,
  thin = 3
)

#diagnostics
traceplot(results2)
gelman.diag(results2)
gelman.plot(results2, ask = FALSE)
effectiveSize(results2)
acfplot(results2)
summary(results2)



#burn-in
update(jags_model, 5000)

#collect samples
results3 <- coda.samples(
  model = jags_model,
  variable.names = parameters,
  n.iter = 50000,
  thin = 1
)

# save the results so that the mcmc does not need to be computed repeatedly
save.image(file = "mcmc_results.Rdata")

#diagnostics
traceplot(results3)
gelman.diag(results3)
gelman.plot(results3, ask = FALSE)
effectiveSize(results3)
densplot(results3[, "r"])
acfplot(results3)
summary(results3)


# task 5 ########
load("mcmc_results.Rdata")
coef <- summary(results3)[[1]][,1] # the coefficient estimates as vector

# 1: predicting the number of insurance claims for each age group in District 1,
# Car Group 1, and 100 policyholders

# district 1 and car group 1: baseline --> given by intercept beta0, meaning 
# that all x_ji=0, for j=1,2,3,7,8,9.
# holders = 100 = E_i
# what we need to fluctuate: x_4i/x_5i/x_6i
# E[Yi] = exp(log(Ei) + beta0 + beta1*x1i + beta2*x2i + ...)

# prediction of number of claims for ...
# ... age group <25: exp(log(100)-log_Holders_mean + beta_0)
exp(log(100)-log_Holders_mean + coef[1])
# ... age group 25-29: exp(log(100)-log_Holders_mean + beta_0 + beta_4*1)
exp(log(100)-log_Holders_mean + coef[1] + coef[5])
# ... age group 30-35: exp(log(100)-log_Holders_mean + beta_0 + beta_5*1)
exp(log(100)-log_Holders_mean + coef[1] + coef[6])
# ... age group >35: exp(log(100)-log_Holders_mean + beta_0 + beta_6*1)
exp(log(100)-log_Holders_mean + coef[1] + coef[7])

# 2: Give summary measures and plots of the posterior predictive distributions.



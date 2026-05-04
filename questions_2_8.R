load("mcmc_results.Rdata")

library(rjags)
library(readr)
library(coda)
library(runjags)
library(MCMCvis)
library(ggmcmc)
library(basicMCMCplots)

# task 2 ----

# diagnostics model 1
traceplot(results1)
gelman.diag(results1)
gelman.plot(results1, ask = FALSE)
effectiveSize(results1)
acfplot(results1)
summary(results1)


# diagnostics model 2
traceplot(results2)
gelman.diag(results2)
gelman.plot(results2, ask = FALSE)
effectiveSize(results2)
acfplot(results2)
summary(results2)


# diagnostics model 3
traceplot(results3)
gelman.diag(results3)
gelman.plot(results3, ask = FALSE)
effectiveSize(results3)
densplot(results3[, "r"])
acfplot(results3)
summary(results3)



# task 5 ----
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



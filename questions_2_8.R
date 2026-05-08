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


# task 3 ----
summary(results3)


# task 4 ----

# Combine all chains into one matrix
results_matrix <- as.matrix(results3)

# Compute rate ratios for all betas
RR <- exp(results_matrix[, grep("beta", colnames(results_matrix))])

# Summarise
apply(RR, 2, function(x) c(
  mean   = mean(x),
  median = median(x),
  lower  = quantile(x, 0.025),
  upper  = quantile(x, 0.975)
))


# task 5 ----
# save the coefficient estimates as a vector
coef <- summary(results3)[[1]][,1]

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

# from mcmc samples compute the claims per age group
claims_u25   <- exp(log(100) - log_Holders_mean + results_matrix[,"beta0"])
claims_25_29 <- exp(log(100) - log_Holders_mean + results_matrix[,"beta0"]
                    + results_matrix[,"beta4"])
claims_30_35 <- exp(log(100) - log_Holders_mean + results_matrix[,"beta0"]
                    + results_matrix[,"beta5"])
claims_o35   <- exp(log(100) - log_Holders_mean + results_matrix[,"beta0"]
                    + results_matrix[,"beta6"])


# summary measures
round(apply(cbind(claims_u25, claims_25_29, claims_30_35, claims_o35), 2,
      function(x) c(mean   = mean(x),
                    median = median(x),
                    var = var(x),
                    lower  = quantile(x, 0.025),
                    upper  = quantile(x, 0.975)
)), 3)
# TODO  mean here different from the predicted values. because exp non-linear?

# density plots
plot(density(claims_u25), col=1, lwd=2,
     xlim = c(0, 24), ylim = c(0, 0.7),
     main="Posterior predictive distributions per age group",
     xlab="Number of claims")
lines(density(claims_25_29), col=2, lwd=2)
lines(density(claims_30_35), col=3, lwd=2)
lines(density(claims_o35), col=4, lwd=2)

legend("topright",
       legend=c("<25","25-29","30-35",">35"),
       col=1:4, lwd=2)


# task 6 ----
# Estimate the posterior probability that the claim rate for policyholders
# of age 25-29 is higher than the claim rate for policyholders of age 30-35:
# P(mu_25_29 > mu_30_35 | data):
sum(claims_25_29 > claims_30_35)/length(claims_25_29)
# or directlly over the mcmc samples and not the transformed values
mean(results_matrix[,"beta4"] > results_matrix[,"beta5"])

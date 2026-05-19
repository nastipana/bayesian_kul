load("mcmc_resultsUpdated.Rdata")

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





# compute posterior predictive distribution by calculating size and probabilty
# and sampling from negative binomial distribution
r <- results_matrix[,"r"]

p_u25 <- r/(r + claims_u25)
p_25_29 <- r/(r + claims_25_29)
p_30_35 <- r/(r + claims_30_35)
p_o35 <- r/(r + claims_o35)

ppd_u25   <- rnbinom(length(p_u25),   size=r, prob=p_u25)
ppd_25_29 <- rnbinom(length(p_25_29), size=r, prob=p_25_29)
ppd_30_35 <- rnbinom(length(p_30_35), size=r, prob=p_30_35)
ppd_o35   <- rnbinom(length(p_o35),   size=r, prob=p_o35)


# density plots
plot(density(ppd_u25), col=1, lwd=2,
     xlim = c(0, 30), ylim = c(0, 0.3),
     main="Posterior predictive distributions per age group",
     xlab="Number of claims")
lines(density(ppd_25_29), col=2, lwd=2)
lines(density(ppd_30_35), col=3, lwd=2)
lines(density(ppd_o35), col=4, lwd=2)

legend("topright",
       legend=c("<25","25-29","30-35",">35"),
       col=1:4, lwd=2)


# task 6 ----
sum(claims_25_29 > claims_30_35)/length(claims_25_29)





# Task 7 ---- 
# posterior samples are already in matrixform results_matrix
# claim rate per 100 policyholders - holding youngest age group, car group 1 constant
rate_d1 <- 100 * exp(-log_Holders_mean + results_matrix[, "beta0"])

rate_d2 <- 100 * exp(-log_Holders_mean + results_matrix[, "beta0"] +
                       results_matrix[, "beta1"])

rate_d3 <- 100 * exp(-log_Holders_mean + results_matrix[, "beta0"] +
                       results_matrix[, "beta2"])

rate_d4 <- 100 * exp(-log_Holders_mean + results_matrix[, "beta0"] +
                       results_matrix[, "beta3"])

# summary measures 
rate_summary <- round(apply(cbind(rate_d1, rate_d2, rate_d3, rate_d4), 2, function(x) c(
                          mean   = mean(x),
                          median = median(x),
                          var = var(x),
                          lower  = quantile(x, 0.025),
                          upper  = quantile(x, 0.975)
            )), 3)
rate_summary

# ggmcmc caterpillar plot (cannot follow slide methodology since we computed district claim 
#rates afterwards - not directly sampled from jags)
rates_matrix <- cbind(
  District1 = rate_d1,
  District2 = rate_d2,
  District3 = rate_d3,
  District4 = rate_d4
)

# convert to mcmc object
rates_mcmc <- as.mcmc(rates_matrix)
# convert to ggmcmc format
out_ggs <- ggs(rates_mcmc)

ggs_caterpillar(out_ggs) + 
  ggplot2::labs(
    x = "Claim rate per 100 policyholders (HPD)",
    y = "District",
    title= "Posterior Claim Rates per 100 policyfolders"
  )


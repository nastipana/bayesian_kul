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

# claim predicitons
round(apply(cbind(claims_u25, claims_25_29, claims_30_35, claims_o35),
            2, mean), 3)



# compute posterior predictive distribution by calculating size (r) and using the
# claims per age group to sample from a negative binomial distribution
r <- results_matrix[,"r"]

ppd_u25   <- rnbinom(length(claims_u25),   size=r, mu=claims_u25)
ppd_25_29 <- rnbinom(length(claims_25_29), size=r, mu=claims_25_29)
ppd_30_35 <- rnbinom(length(claims_30_35), size=r, mu=claims_30_35)
ppd_o35   <- rnbinom(length(claims_o35),   size=r, mu=claims_o35)


# summary measures
round(apply(cbind(ppd_u25, ppd_25_29, ppd_30_35, ppd_o35), 2,
            function(x) c(mean   = mean(x),
                          median = median(x),
                          var = var(x),
                          lower  = quantile(x, 0.025),
                          upper  = quantile(x, 0.975)
)), 3)


# to obtain PPD plots, transform data in a way that only probabilities for 
# integers are displayed
ppd <- list(ppd_u25, ppd_25_29, ppd_30_35, ppd_o35)
ppd_tab <- list(0)
ppd_prob <- list(0)
ppd_claim <- list(0)
for(i in 1:4){
  ppd_tab[[i]] <- table(ppd[[i]]) # as table
  ppd_prob[[i]] <- ppd_tab[[i]]/sum(ppd_tab[[i]]) # as probabilites
  ppd_claim[[i]] <- as.numeric(names(ppd_prob[[i]])) # number of claims
}

# density plot
plot(ppd_claim[[1]], ppd_prob[[1]], type = "h", lwd =3,
     xlab = "Number of claims",
     ylab = "Probability", ylim = c(0, 0.13),
     main = "Posterior predictive distributions per age group")
lines(ppd_claim[[2]]+0.2, ppd_prob[[2]], type = "h", 
      col=3,#adjustcolor(2, alpha.f=0.9), 
      lwd=3)
lines(ppd_claim[[3]]+0.4, ppd_prob[[3]], type = "h",
      col=2,#adjustcolor(3, alpha.f=0.7),
      lwd=3)
lines(ppd_claim[[4]]+0.6, ppd_prob[[4]], type = "h", 
      col=5,#adjustcolor(4, alpha.f=0.5), 
      lwd=3)

axis(2, at=seq(0, 0.13, by=0.02))

legend("topright",
       legend=c("<25","25-29","30-35",">35"),
       col=c(1,3,2,5), lwd=2)



# task 6 ----
sum(claims_25_29 > claims_30_35)/length(claims_25_29)



# task 7 ---- 
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
    title= "Posterior Claim Rates per 100 policyholders"
)


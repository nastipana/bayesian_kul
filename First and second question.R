#install packages
install.packages("readr")
install.packages("coda")
install.packages("runjags")
install.packages("MCMCvis")
install.packages("ggmcmc")
install.packages("basicMCMCplots")
install.packages("rjags")

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
results <- coda.samples(
  model = jags_model,
  variable.names = parameters,
  n.iter = 10000,
  thin = 1
)

#diagnostics
traceplot(results)
gelman.diag(results)
gelman.plot(results, ask = FALSE)
effectiveSize(results)
acfplot(results)
summary(results)



#burn-in
update(jags_model, 1000)

#collect samples
results <- coda.samples(
  model = jags_model,
  variable.names = parameters,
  n.iter = 10000,
  thin = 3
)

#diagnostics
traceplot(results)
gelman.diag(results)
gelman.plot(results, ask = FALSE)
effectiveSize(results)
acfplot(results)
summary(results)



#burn-in
update(jags_model, 5000)

#collect samples
results <- coda.samples(
  model = jags_model,
  variable.names = parameters,
  n.iter = 50000,
  thin = 1
)

#diagnostics
traceplot(results)
gelman.diag(results)
gelman.plot(results, ask = FALSE)
effectiveSize(results)
densplot(results[, "r"])
acfplot(results)
summary(results)
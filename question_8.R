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

#____________________ Compare differznt priors for the dispersion parameter r of the negative binomial 

# (1) Original uninformative Gamma 

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
writeLines(model_string, "model_r_gamma001.txt")

# (same for all 3) initial values but with seed incorporated 
set.seed(1110) # since the runif() are generated before jags starts, so would also become random
my.inits.seeded <- list(
  list(beta0 = 0, beta1 = 0, beta2 = 0, beta3 = 0,
       beta4 = 0, beta5 = 0, beta6 = 0,
       beta7 = 0, beta8 = 0, beta9 = 0,
       r = runif(1, 0.5, 5),
       .RNG.name = "base::Mersenne-Twister",
       .RNG.seed = 1001),
  
  list(beta0 = 0.5, beta1 = 0, beta2 = 0, beta3 = 0,
       beta4 = 0, beta5 = 0, beta6 = 0,
       beta7 = 0, beta8 = 0, beta9 = 0,
       r = runif(1, 0.5, 5),
       .RNG.name = "base::Mersenne-Twister",
       .RNG.seed = 1010),
  
  list(beta0 = -0.5, beta1 = 0, beta2 = 0, beta3 = 0,
       beta4 = 0, beta5 = 0, beta6 = 0,
       beta7 = 0, beta8 = 0, beta9 = 0,
       r = runif(1, 0.5, 5),
       .RNG.name = "base::Mersenne-Twister",
       .RNG.seed = 1011)
)

parameters <- c("beta0", "beta1", "beta2", "beta3", "beta4",
                "beta5", "beta6", "beta7", "beta8", "beta9", "r")

#

jags_model_gamma001 <- jags.model(
  file = "model_r_gamma001.txt",
  data = jags_data,
  inits = my.inits.seeded,
  n.chains = 3
)

update(jags_model_gamma001, 5000)

results_gamma001 <- coda.samples(
  model = jags_model_gamma001,
  variable.names = parameters,
  n.iter = 50000,
  thin = 1
)


# (2) Prior but with more information 
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

  r ~ dgamma(1, 1)
}"
writeLines(model_string, "model_r_gamma11.txt")

jags_model_gamma11 <- jags.model(
  file = "model_r_gamma11.txt",
  data = jags_data,
  inits = my.inits.seeded,
  n.chains = 3
)

update(jags_model_gamma11, 5000)

results_gamma11 <- coda.samples(
  model = jags_model_gamma11,
  variable.names = parameters,
  n.iter = 50000,
  thin = 1
)

# (3) Log-normal prior for the dispersion parameter r 

# Need new initial values since r is now deterministic (<- instead of ~), therefrore jags doesnt allow us to set an initial value for r. We do so for log_r

set.seed(1110) # since the runif() are generated before jags starts, so would also become random
my.inits.seeded <- list(
  list(beta0 = 0, beta1 = 0, beta2 = 0, beta3 = 0,
       beta4 = 0, beta5 = 0, beta6 = 0,
       beta7 = 0, beta8 = 0, beta9 = 0,
       log_r = runif(1, 0.5, 5),
       .RNG.name = "base::Mersenne-Twister",
       .RNG.seed = 1001),
  
  list(beta0 = 0.5, beta1 = 0, beta2 = 0, beta3 = 0,
       beta4 = 0, beta5 = 0, beta6 = 0,
       beta7 = 0, beta8 = 0, beta9 = 0,
       log_r = runif(1, 0.5, 5),
       .RNG.name = "base::Mersenne-Twister",
       .RNG.seed = 1010),
  
  list(beta0 = -0.5, beta1 = 0, beta2 = 0, beta3 = 0,
       beta4 = 0, beta5 = 0, beta6 = 0,
       beta7 = 0, beta8 = 0, beta9 = 0,
       log_r = runif(1, 0.5, 5),
       .RNG.name = "base::Mersenne-Twister",
       .RNG.seed = 1011)
)


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

  log_r ~ dnorm(0, 0.01)
  r <- exp(log_r)
}"
writeLines(model_string, "model_r_lognormal100.txt")

jags_model_lognormal100 <- jags.model(
  file = "model_r_lognormal100.txt",
  data = jags_data,
  inits = my.inits.seeded,
  n.chains = 3
)

update(jags_model_lognormal100, 5000)

results_lognormal100 <- coda.samples(
  model = jags_model_lognormal100,
  variable.names = parameters,
  n.iter = 50000,
  thin = 1
)


# (4) result log normal with smaller var (higher precision) 

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

  log_r ~ dnorm(0, 1)
  r <- exp(log_r)
}"
writeLines(model_string, "model_r_lognormal1.txt")

jags_model_lognormal1 <- jags.model(
  file = "model_r_lognormal1.txt",
  data = jags_data,
  inits = my.inits.seeded,
  n.chains = 3
)

update(jags_model_lognormal1, 5000)

results_lognormal1 <- coda.samples(
  model = jags_model_lognormal1,
  variable.names = parameters,
  n.iter = 50000,
  thin = 1
)


# Comparison of the 3
summary(results_gamma001)
summary(results_gamma11)
summary(results_lognormal100)
summary(results_lognormal1)




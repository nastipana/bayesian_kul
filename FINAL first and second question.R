#before this you j-guys have to install the pdf document i put on github
#install what the professor asked us
install.packages(c("readr","coda","runjags","MCMCvis","ggmcmc","basicMCMCplots"))

#now we load the libraries
library(readr)
library(coda)
library(runjags)
library(MCMCvis)
library(ggmcmc)
library(basicMCMCplots)

# R package that lets R talk to JAGS
install.packages("rjags")
library(rjags)

#This is the dataset that is already in R studio
library(MASS)
data(Insurance)
head(Insurance)
#just check how many for each
levels(Insurance$District)
levels(Insurance$Age)
levels(Insurance$Group)

#make numbers 
District <- as.numeric(Insurance$District)
Age <- as.numeric(Insurance$Age)
Group <- as.numeric(Insurance$Group)
#extract the data so that R knows
Claims <- Insurance$Claims
Holders <- Insurance$Holders
N <- nrow(Insurance)

#here we put it together
jags_data <- list(
  Claims = Claims,
  Holders = Holders,
  District = District,
  Age = Age,
  Group = Group,
  N = N
)

#i wrote the text so that jags can actually understand because it cannot read R only text or something
model_string <- "model {
  for (i in 1:N) {
    Claims[i] ~ dnegbin(p[i], r)
    p[i] <- r / (r + mu[i])
    log(mu[i]) <- log(Holders[i]) + beta0
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

#mhhh idk about this, these are the initial values
my.inits <- function() {
  list(
    beta0 = rnorm(1, 0, 1),
    beta1 = rnorm(1, 0, 1),
    beta2 = rnorm(1, 0, 1),
    beta3 = rnorm(1, 0, 1),
    beta4 = rnorm(1, 0, 1),
    beta5 = rnorm(1, 0, 1),
    beta6 = rnorm(1, 0, 1),
    beta7 = rnorm(1, 0, 1),
    beta8 = rnorm(1, 0, 1),
    beta9 = rnorm(1, 0, 1),
    r = runif(1, 0.5, 5)
  )
}

#these are the parameters to monitor
parameters <- c("beta0", "beta1", "beta2", "beta3", "beta4",
                "beta5", "beta6", "beta7", "beta8", "beta9", "r")

library(rjags)

#this is like the previous model bug and here we say how many chains and what te initial values are
jags_model <- jags.model(
  file = "bay_kul_model.txt",
  data = jags_data,
  inits = my.inits,
  n.chains = 3
)

#this is like the part where we like try it and throw it away
# burn-in
update(jags_model, 5000)

#here we collect the actual samples we use
#We thin because consecutive MCMC samples are correlated so now we keep every 10th
#I initially did thin = 1 but there was too much autocorrelation
results <- coda.samples(
  model = jags_model,
  variable.names = parameters,
  n.iter = 50000,
  thin = 10
)

library(coda)

#these are all different plots idk which ones to include or not
#i eventually included all of them in our word document
traceplot(results)

gelman.diag(results)
gelman.plot(results, ask = FALSE)

effectiveSize(results)

densplot(results[, "r"])

acfplot(results)

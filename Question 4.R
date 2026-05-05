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
library(ggplot2)
library(fdrtool)
library(patchwork)
rm(list = ls())

u_grid <- seq(0, 1, by = 0.001)
u_grid <- u_grid[1:(length(u_grid) - 1)]
data = read.csv("exponential_data_sample_1e+06_rate_1.csv")
data <- data[[1]]
Q_true <- qexp(u_grid)
#plot(u_grid,Q_true)






#Quantile function of MLE Estimator
Quantile_grenander <- function(u_grid,data) {
  e <- ecdf(data)
  g <- grenander(e)
  g_cdf <- g$F
  y <- environment(g_cdf)$x
  Q <- quantile(y, u_grid, type = 1)  # quantile function of Grenander
  return(Q)
  
}

L2dist <- function(u_grid,Q_true,Q_gren){
  n = length(u_grid)
  return(sqrt(sum((Q_gren - Q_true)^2)/n))
}

Convergence_Grenander <- function(u_grid,Q_true,data,start,end,step){
  n_points <- ((end - start) %/% step) + 1
  dists <- numeric(n_points)
  
  i <- 1
  num_samples <- start
  
  while (num_samples <= end) {
    samples <- data[1:num_samples]
    Q_gren <- Quantile_grenander(u_grid, samples)
    dists[i] <- L2dist(u_grid, Q_true, Q_gren)
    i <- i + 1
    num_samples <- num_samples + step
  }
  
  return(dists)
}

#generate data points if needed
generate_data <- function(n, lambda, seed){
  set.seed(seed)
  data <- rexp(n,rate = lambda)
  filename <- paste0("exponential_data_sample_", n,"_rate_", lambda, "_seed_", seed, ".csv")
  write.csv(data, file = filename, row.names = FALSE)
}


#simulation with given data points and fixed number of samples
data = read.csv("exponential_data_sample_1e+06_rate_1.csv")
data <- data[[1]]
start = 49000
end = 900000
step = 10000
dists = Convergence_Grenander(u_grid,Q_true,data,start,end,step)
x = seq(start, end, by = step)
#df <- data.frame(x = x, dists = dists)
#write.csv(df, file = "Data_Grenander.csv", row.names = FALSE)

plot(log10(x),log10(dists),
     main = "Convergence of Maximum Likelihood Estimator (Log-Log Plot)",
     xlab = "log10(Number of Samples)",
     ylab = "log10(L2 Distance)",)










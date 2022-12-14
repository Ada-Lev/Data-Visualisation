# Poisson Regression

#--------------------------------------------------------------#
# Libraries
#--------------------------------------------------------------#

library(ggplot2)
library(mvtnorm)

set.seed(12345)

## ggplot2 common theme
dejavu_layer <- list(
  theme_bw(),
  theme(
    text = element_text(family = "Bitter",
                        face = "bold",
                        size = 14),
    title = element_text(size = 18),
    legend.text = element_text(size = 14),
    axis.text = element_text(size = 13,
                             family = "DejaVuSans",
                             face = "plain"),
    axis.title = element_text(size = 14)
  )
)

data <- read.table("eBayNumberOfBidderData.dat", header = TRUE)
names(data)[1] <- "target"
X <- as.matrix(data[, -1])
y <- as.matrix(data[, 1])

# Bayesian analysis of the Poisson regression

# Prior: beta ~ N(0, 100*inv((t(X)*X))), where X is the n x p covariate matrix

Sigma <- 100 * solve(t(X) %*% X)
mean = rep(0, 9)
betas_init = rep(0, 9)

# Posterior is assumed multivariate normal: N(beta_mode, inv(J_x(beta_mode))))

# LogPosterior:
logPost <- function(betas, mean, Sigma, X, y){
  logPrior <- dmvnorm(x = betas, mean = mean, sigma = Sigma, log = TRUE)
  # unknown parameters in regression are betas.
  # This is why we write prior for them
  linPred <- X %*% betas
  logLik <- sum(linPred * y - exp(linPred)) #LogLik for the Poisson Model
  return(logPrior + logLik)
}

# Minimize the logLikelihood of the posterior
mode_optim <- optim(betas_init,
                    logPost,
                    gr = NULL,
                    mean, Sigma, X, y,
                    method = c("BFGS"),
                    control = list(fnscale = -1),
                    hessian=TRUE)

# Name the coefficient by covariates
names(mode_optim$par) <- names(as.data.frame(X))

# Compute approximate standard deviations.
approxPostStd <- sqrt(diag(-solve(mode_optim$hessian)))
# Name the coefficient by covariates
names(approxPostStd) <- names(as.data.frame(X))

post_mean <- mode_optim$par
print('The posterior mode is')
print(post_mean)
print('The approximate posterior standard deviation is')
print(approxPostStd)
post_cov_mat <- -solve(mode_optim$hessian)
print('The posterior covariance matrix is')
print(post_cov_mat)

# Repeat the analysis using the Metropolis Hastings algorithm

N <- 1000
random_walk_metropolis <- function(logPost, c = 1){
  theta <- matrix(NA, ncol = 9, nrow = N)
  theta[1, ] <- rep(0, 9)
  accept_rate <- 1
  for (i in 2:N){
    proposed <- as.vector(rmvnorm(1, mean = theta[i-1, ],
                                  sigma = c*post_cov_mat))
    post_prev <-logPost(betas = theta[i-1, ], mean, Sigma, X, y)
    post_new <- logPost(betas = proposed, mean, Sigma, X, y)
    accept_pr <- min(1, exp(post_new - post_prev))
    u <- runif(1)
    if (u <= accept_pr){
      theta[i, ] <- proposed
      accept_rate <- accept_rate + 1
    }
    else{
      theta[i, ] <- theta[i-1, ]
    }
  }
  print("Acceptance rate is")
  print(accept_rate/N)
  return(theta)
}

theta <- random_walk_metropolis(logPost)

df_plot <- data.frame(x = 1:N, VerifyId = theta[, 3], Sealed = theta[, 4],
                      Logbook = theta[, 8], MinBidShare = theta[, 9])

plot <- ggplot(df_plot, aes(x = x)) +
  dejavu_layer +
  labs(title = "MCMC of influential features") +
  geom_line(aes(y = VerifyId, color = "1"), size = 1) +
  geom_line(aes(y = Sealed, color = "2"), size = 1) +
  geom_line(aes(y = Logbook, color = "3"), size = 1) +
  geom_line(aes(y = MinBidShare, color = "4"), size = 1) +
  xlab("Iteration #") + ylab("Parameter Value") +
  scale_color_manual(values = c("royalblue", "orange", "darkgreen", "darkgray"),
                     labels = c("#1", "#2", "#3", "#4"),
                     name = "Features")

print(plot)

#ggsave("MCMC.png", dpi = 400)


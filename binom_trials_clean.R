# libraries
library(ggplot2)
library(gridExtra)

# Set seed
set.seed(12345)

# Given:
s = 13
n = 50
f = n - s
alpha_0 = 5
beta_0 = 5

# Beta Posterior distribution
beta_posterior <- function(n, s = 13, f = 37){
  alpha_0 = 5
  beta_0 = 5
  rbeta(n = n, shape1 = alpha_0 + s, shape2 = beta_0 + f)
}

# True expected value and standard deviation
expected_value <- (alpha_0 + s)/ ((alpha_0 + s) + (beta_0 + f))

variance <- (alpha_0 + s) * (beta_0 + f)/(((alpha_0 + s) + (beta_0 + f))**2*
                                            (alpha_0 + s + beta_0 + f + 1))
std <- variance**0.5

# Theta values generated from beta posterior for different n
df <- data.frame(theta_10 = beta_posterior(n = 10),
                 theta_100 = beta_posterior(n = 100), 
                 theta_1000 = beta_posterior(n = 1000),
                 theta_10000 = beta_posterior(n = 10000))

# Plots for different n values 

# n = 10
pl1 <- ggplot(df) +
  geom_histogram(aes(x = theta_10, y = ..density.., ), 
                 fill = "lightgrey", bins = 100, color = "black", alpha = 0.2) +
  geom_vline(aes(xintercept = mean(theta_10), color = "estimated"), size = 1) +
  geom_vline(aes(xintercept = expected_value, color = "true"), size = 1) +
  scale_color_manual(name = "mean", values = c(estimated = "red", true = "blue")) +
  labs(title = "n = 10", x = "θ", y = "p(θ | y)") +
  theme_minimal() + theme(legend.position = "none")

# n = 100
pl2 <- ggplot(df) +
  geom_histogram(aes(x = theta_100, y = ..density.., ), 
                 fill = "lightgrey", bins = 100, color = "black", alpha = 0.2) +
  geom_vline(aes(xintercept = mean(theta_100), color = "estimated"), size = 1) +
  geom_vline(aes(xintercept = expected_value, color = "true"), size = 1) +
  scale_color_manual(name = "mean", values = c(estimated = "red", true = "blue")) +
  labs(title = "n = 100", x = "θ", y = "p(θ | y)") +
  theme_minimal() + theme(legend.position = "none")

# n = 1000
pl3 <- ggplot(df) +
  geom_histogram(aes(x = theta_1000, y = ..density.., ), 
                 fill = "lightgrey", bins = 100, color = "black", alpha = 0.2) +
  geom_vline(aes(xintercept = mean(theta_1000), color = "estimated"), size = 1) +
  geom_vline(aes(xintercept = expected_value, color = "true"), size = 1) +
  scale_color_manual(name = "mean", values = c(estimated = "red", true = "blue")) +
  labs(title = "n = 1000", x = "θ", y = "p(θ | y)") +
  theme_minimal() + theme(legend.position = "none")

# n = 10000
pl4 <- ggplot(df) +
  geom_histogram(aes(x = theta_10000, y = ..density.., ), 
                 fill = "lightgrey", bins = 100, color = "black", alpha = 0.2) +
  geom_vline(aes(xintercept = mean(theta_10000), color = "estimated"), size = 1) +
  geom_vline(aes(xintercept = expected_value, color = "true"), size = 1) +
  scale_color_manual(name = "mean", values = c(estimated = "red", true = "blue")) +
  labs(title = "n = 10000", x = "θ", y = "p(θ | y)") +
  theme_minimal()

# Extract the legend
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

shared_legend <- g_legend(pl4)

g <- grid.arrange(arrangeGrob(pl1, pl2, pl3, pl4 + theme(legend.position = "none"), ncol = 2),
             top = textGrob("Posterior ~ Beta(α = 18, β = 42)",
                            gp = gpar(fontsize = 18, fontface = "bold")),
             shared_legend,
             heights = c(10, 0),
             widths = c(8.5, 1.5))

ggsave("binom.png", g, dpi = 400)


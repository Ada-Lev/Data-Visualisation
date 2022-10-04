# libraries
library(ggplot2)
library(gridExtra)
library(grid)

# Set seed
set.seed(12345)

# Given:
s <- 13
n <- 50
f <- n - s
alpha_0 <- 5
beta_0 <- 5

# Beta Posterior distribution
beta_posterior <- function(n){
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


## ggplot2 common theme
dejavu_layer <- list(
  theme_minimal(),
  theme(
    text = element_text(family = "Bitter",
                        face = "bold",
                        size = 13),
    title = element_text(size = 13),
    legend.text = element_text(size = 13),
    axis.text = element_text(size = 12,
                             family = "DejaVuSans",
                             face = "plain"),
    axis.title = element_text(size = 13,
                              face = "plain")
  )
)

# Plots for different n values

plot_fun <- function(col_name, title){
  pl <- ggplot(df) +
    dejavu_layer +
    geom_histogram(aes_string(x = col_name, y = "..density.."),
                   fill = "white",
                   bins = 100,
                   color = "darkgrey",
                   alpha = 0.3) +
    geom_vline(aes(xintercept = mean(df[[col_name]]), color = "Estim."),
               size = 1.5) +
    geom_vline(aes(xintercept = expected_value, color = "True"),
               size = 1.5) +
    scale_color_manual(name = "Mean",
                       values = c(Estim. = "orange", True = "royalblue")) +
    labs(title = title,
         x = "θ",
         y = "p(θ | y)")

  return(pl)
}

pl1 <- plot_fun("theta_10", "n = 10")
pl2 <- plot_fun("theta_100", "n = 100")
pl3 <- plot_fun("theta_1000", "n = 1000")
pl4 <- plot_fun("theta_10000", "n = 10000")

# Extract the legend
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

shared_legend <- g_legend(pl4)

g <- grid.arrange(arrangeGrob(pl1 + theme(legend.position = "none"),
                              pl2 + theme(legend.position = "none"),
                              pl3 + theme(legend.position = "none"),
                              pl4 + theme(legend.position = "none"),
                              ncol = 2),
             top = textGrob("Posterior ~ Beta(α = 18, β = 42)",
                            gp = gpar(fontsize = 18,
                                      fontface = "bold",
                                      fontfamily = "Bitter")),
             shared_legend,
             heights = c(10, 0),
             widths = c(8.5, 1.5))

#ggsave("binom.png", g, dpi = 400)


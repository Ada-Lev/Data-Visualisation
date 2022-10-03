#--------------------------------------------------------------#
# Libraries
#--------------------------------------------------------------#

library(ggplot2)

set.seed(12345)

## ggplot2 common theme
dejavu_layer <- list(
  theme_bw(),
  theme(
    text = element_text(family = "Bitter",
                        face = "bold",
                        size = 14),
    title = element_text(size = 15),
    legend.text = element_text(size = 14),
    axis.text = element_text(size = 13,
                             family = "DejaVuSans",
                             face = "plain"),
    axis.title = element_text(size = 14)
  )
)

f <- function(n, fun, funName){
  #Required for later plot drawing
  x <- c()
  a0 <- c()
  a1 <- c()
  a2 <- c()

  #Interval partition
  lenInt <- 1/n
  largestPoint <- 0
  lowestPoint <- 0

  for (point in 1:n){
    #Three points allocation
    largestPoint <- largestPoint + lenInt
    lowestPoint <- largestPoint - lenInt
    middlePoint <- (largestPoint + lowestPoint)/2

    #data to insert into optim
    data <- data.frame(x = c(lowestPoint,
                             middlePoint,
                             largestPoint),
                       y = c(fun(lowestPoint),
                             fun(middlePoint),
                             fun(largestPoint)))
    #print(data)
    optimized <- optim(fn = squaredError,
                       par = c(0, 0, 0),
                       data = data,
                       method = "BFGS")

    #these vectors are required for final plotting
    x <- c(x, middlePoint)
    a0 <- c(a0, optimized$par[1])
    a1 <- c(a1, optimized$par[2])
    a2 <- c(a2, optimized$par[3])
  }

  #The final data.frame for plotting
  dfPlot <- cbind.data.frame(x = x, a0 = a0, a1 = a1, a2 = a2)

  plot <- ggplot(dfPlot, aes(x = x)) +
    dejavu_layer +
    stat_function(fun = fun,
                  aes(color = "1"),
                  n = 1000,
                  size = 1.2) +
    stat_function(fun = function(x) dfPlot$a0 + dfPlot$a1 * x + dfPlot$a2 * x^2,
                  aes(color = "2"),
                  n = 1000,
                  size = 1.2,
                  linetype = "dashed") +
    scale_color_manual(name = NULL,
                      labels = c("\u2212x\u00B7sin(10\u00B7\u03C0\u00B7x)",
                                           "Interpolated"),
                      values = c("black", "cyan")) +
    labs(title = paste0("Parabolically Interpolated ",
                        "\u2212x\u00B7sin(10\u00B7\u03C0\u00B7x)"),
         x = "x",
         y = "y")

  print(plot)
  #ggsave("parabolic.png", dpi = 400)
}

#To minimize
squaredError <- function(data, par) {
  MSE <- with(data, sum(par[1] + par[2]*x + par[3]*x*x - y)^2)
  return(MSE)
}

f(n = 1000,
  fun = function(x) -x*sin(10*pi*x),
  funName = "-x*sin(10*pi*x)")

#As we can see, the interpolated function coincides with the real function.


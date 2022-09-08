library(ggplot2)

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
  
  for (k in 1:n){
    #Theree points allocation
    largestPoint <- largestPoint + lenInt
    lowestPoint <- largestPoint - lenInt
    middlePoint <- (largestPoint + lowestPoint)/2
    
    #data to insert into optim
    data <- data.frame(x = c(lowestPoint, middlePoint, largestPoint), 
                       y = c(fun(lowestPoint), fun(middlePoint), fun(largestPoint)))
    #print(data)
    optimized <- optim(fn = squaredError, par = c(0, 0, 0), data = data, 
                       method = "BFGS") #The TA said to use BFGS
    
    #these vectors are required for final plotting
    x <- c(x, middlePoint)
    a0 <- c(a0, optimized$par[1])
    a1 <- c(a1, optimized$par[2])
    a2 <- c(a2, optimized$par[3])
  }
  
  #The final data.frame for plotting
  dfPlot <- cbind.data.frame(x = x, a0 = a0, a1 = a1, a2 = a2)
  
  plot <- ggplot(dfPlot, aes(x = x)) + 
    theme_bw() + 
    stat_function(fun = fun, aes(color = "1"), n = 1000, size = 1.2) +
    stat_function(fun = function(x) dfPlot$a0 + dfPlot$a1 * x + dfPlot$a2 * x^2, 
                  aes(color = "2"), n = 1000,
                  size = 1.2,
                  linetype = "dashed") +
    scale_color_manual(name = NULL, 
                      labels = c("\u2212x\u00B7sin(10\u00B7\u03C0\u00B7x)", 
                                           "interpolated"), 
                      values = c("black", "cyan")) + 
    labs(title = paste0("Parabolically interpolated ", 
                        "\u2212x\u00B7sin(10\u00B7\u03C0\u00B7x)")) +
    xlab("x") +
    ylab("y") +
    theme(title = element_text(size = 12, face = 'bold'),
          axis.title = element_text(size = 10, face = "plain"))
  print(plot)
  ggsave("parabolic.png", dpi = 400)
}

#To minimize
squaredError <- function(data, par) {
  MSE <- with(data, sum(par[1] + par[2]*x + par[3]*x*x - y)^2)
  #print(MSE)
  return(MSE)
}

f(n = 1000, fun = function(x){
  result <- -x*sin(10*pi*x)
  return(result)
}, funName = "-x*sin(10*pi*x)")

#As we can see, the interpolated function coincides with the real functions.


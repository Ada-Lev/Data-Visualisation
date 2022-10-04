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
    title = element_text(size = 18),
    legend.text = element_text(size = 14),
    axis.text = element_text(size = 13,
                             family = "DejaVuSans",
                             face = "plain"),
    axis.title = element_text(size = 14)
  )
)

data <- read.csv("communities.csv")
n <- dim(data)[1]

# Rename the target variable ViolentCrimesPerPop as target
names(data)[names(data) == "ViolentCrimesPerPop"] <- "target"


# Divide the data into training and test sets
data <- as.data.frame(scale(data))
id = sample(1:n, floor(n * 0.5))
train = data[id, ]
test = data[-id, ]

# Vectors to store training and test errors
error_tr <- c()
error_test <- c()

# Calculate the MSE cost
cost <- function(par, data){
  tr_cost <- mean((train$target - par %*% t(as.matrix(train[-101])))^2)
  te_cost <- mean((test$target - par %*% t(as.matrix(train[-101])))^2)
  error_tr <<- c(error_tr, tr_cost)
  error_test <<- c(error_test, te_cost)
  return(tr_cost)
}

# Use gradient descent algorithm to minimize the cost
opt <- optim(par = rep(0, 100),
             fn = cost,
             method = "BFGS",
             control = list(maxit = 12))

# Plots:
df <- cbind.data.frame(x = 1:length(error_tr),
                       error_tr = error_tr,
                       error_test = error_test)

plot <- ggplot(df, aes(x = x)) +
  dejavu_layer +
  geom_point(aes(y = error_tr, colour = "1")) +
  geom_point(aes(y = error_test, colour = "2")) +
  xlim(c(501, length(error_tr))) +
  ylim(c(0, error_test[400])) +
  geom_vline(xintercept = 1230) +
  annotate(x = 1230,
           y = 2.5,
           label = "k = 1230",
           vjust = 2,
           geom = "label",
           family = "DejavuSans",
           size = 4,
           fill = "#e0e0e0") +
  labs(title = "Implicit Regularisation",
       x = "Iteration #",
       y = "MSE") +
  scale_color_manual(name = "MSE",
                     labels = c("Train", "Test"),
                     values = c("#388e3c", "#1976d2")) +
  guides(colour = guide_legend(override.aes = list(size = 4)))
  # Legend symbol size

print(plot)

#ggsave("implicit.png", dpi = 400)

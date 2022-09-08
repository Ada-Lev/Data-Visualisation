#Implicit regularization

library(ggplot2)

data <- read.csv("communities.csv")

n <- dim(data)[1]

# Rename the target variable ViolentCrimesPerPop as target
names(data)[names(data) == "ViolentCrimesPerPop"] <- "target"


# Divide the data
data <- as.data.frame(scale(data))
set.seed(12345) 
id = sample(1:n, floor(n * 0.5)) 
train = data[id, ]
#print(dim(train)) #997 entries

test = data[-id, ]
#print(dim(test)) #997 entries

error_tr <- c()
error_test <- c()

cost <- function(par, data){
  tr <- mean((train$target - par %*% t(as.matrix(train[-101])))^2)
  error_tr <<- c(error_tr, tr)
  error_test <<- c(error_test, 
                   mean((test$target - par %*% t(as.matrix(train[-101])))^2))
  return(tr)
}

opt <- optim(par = rep(0, 100), fn = cost, method = "BFGS", 
             control = list(maxit = 12)) 

# Plots:
df <- cbind.data.frame(x = 1:length(error_tr), error_tr = error_tr,
                       error_test = error_test)
plot <- ggplot(df, aes(x = x)) + 
  theme_bw() +
  geom_point(aes(y = error_tr, colour = "1")) +
  geom_point(aes(y = error_test, colour = "2")) +
  xlab("# iteration") +
  ylab("MSE") +
  xlim(c(501, length(error_tr))) +
  ylim(c(0, error_test[400])) +
  geom_vline(xintercept = 1230) +
  annotate(x = 1230, y = 2.5, label = "k = 1230", 
                    vjust = 2, geom = "label") +
  labs(title = "Test and training errors") +
  scale_color_manual(name = NULL, 
                     labels = c("Train MSE", "Test MSE"), 
                     values = c("cyan", "blue")) +
  theme(title = element_text(size = 12, face = 'bold'),
        axis.title = element_text(size = 10, face = "plain"))
print(plot)

ggsave("implicit.png", dpi = 400)

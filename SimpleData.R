#----setup-------------------------------------------------------------

rm(list = ls()) 


library(glmnet)


#----generate data-----------------------------------------------------
n = 50
d = 8
X = matrix(rnorm(n * d), n, d)
beta = c(3, 2, 0, 1.5, rep(0, d - 4))
eps = rnorm(n)
Y = X %*% beta + eps


# dataframe for complete data
data1 <- cbind(X,Y) %>% 
  as_tibble() 
colnames(data1)<- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "Y")

x <- model.matrix(Y~., data1)[,-1]
y <- data1$Y


# dataframe for missing data
X[sample.int(nrow(X), floor(nrow(X) * .2)), sample.int(ncol(X), floor(ncol(X) * .3))] <- NA

data2 <- cbind(X,Y) %>% 
  as_tibble() 
colnames(data2)<- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "Y")

data2 <- na.omit(data2)

x <- model.matrix(Y~., data2)[,-1]
y <- data2$Y


#----Imputation--------------------------------------------------------
# generate imputed dataframe
# with mean
X[sample.int(nrow(X), floor(nrow(X) * .2)), sample.int(ncol(X), floor(ncol(X) * .3))] <- NA

data2 <- cbind(X,Y) %>% 
  as_tibble() 
colnames(data2)<- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "Y")

data2$V1[is.na(data2$V1)] <- mean(data2$V1, na.rm = TRUE)
data2$V6[is.na(data2$V6)] <- mean(data2$V6, na.rm = TRUE)


x <- model.matrix(Y~., data2)[,-1]
y <- data2$V9

#----lasso plot--------------------------------------------------------

# estimate lasso
lambda <- 10^seq(10, -2, length = 100)
set.seed(123) 
lasso=glmnet(x,y, alpha = 1, family = "gaussian")

# plot coefficient path
plot(lasso)

# plot MSE with different lambda
cvlassofit<-cv.glmnet(x,y, alpha = 1, family = "gaussian" )
plot(cvlassofit)

# Add line to indicate best lambda
lambda_min <- cvlassofit$lambda.min
estimates <- as.vector(coef(lasso, s = lambda_min, exact = TRUE))
norm. <- sum(abs(estimates))
plot(lasso, xlim = range(0, norm., as.vector(lasso$beta)))
abline(v = norm., col = "red")







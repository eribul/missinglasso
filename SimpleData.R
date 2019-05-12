#----setup-------------------------------------------------------------

# clean memory
rm(list = ls())

# load packages
library(MASS)
library(Matrix)
library(lattice)
library(igraph)
library(flare)

library(foreach)
library(glmnet)
library(tibble)

library(tidyr)


#----generate data 1-----------------------------------------------------
# read the table with nested simulated dataframes
df_imp <- readRDS("C:/Users/Kokchun/Desktop/temp/df_imp.rds")

# extract data dataframes
# pick p=50, n=1000
# data1 is zero missing proportion
# data2 is 0.3 mp & mean imputed
# data3 is 0.3 mp & median imputed
data1 <- unnest(df_imp[50,5])
data2 <- unnest(df_imp[55,5])
data3 <- unnest(df_imp[56,5])

# convert dataframes to matrices, suitable as inputs of models
# for data1
x1 <- model.matrix(Y~., data1)[,-1]
y1 <- data1$Y
# for data2
x2 <- model.matrix(Y~., data2)[,-1]
y2 <- data2$Y
# for data3
x3 <- model.matrix(Y~., data3)[,-1]
y3 <- data3$Y


#----generate data 2---------------------------------------------------------
# # Alternatively, generate own data
# n = 50
# d = 100
# X = matrix(rnorm(n * d), n, d)
# beta = c(3,2,0,1.5,rep(0,d-4))
# eps = rnorm(n)
# Y = X %*% beta + eps
# 
# # dataframe for complete data
# data1 <- cbind(X,Y) %>%
#   as_tibble()
# #colnames(data1)<- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "Y")
# 
# x4 <- X
# y4 <- Y
# 
# # dataframe for missing data
# X[sample.int(nrow(X), floor(nrow(X) * .3)), sample.int(ncol(X), floor(ncol(X) * .3))] <- NA
# 
# data2 <- cbind(X,Y) %>%
#   as_tibble()
# #colnames(data2)<- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "Y")
# 
# data2 <- na.omit(data2)
# 
# x5 <- model.matrix(V21~., data2)[,-1]
# y5 <- data2$V21
# 
# # imputed with mean
# X[sample.int(nrow(X), floor(nrow(X) * .2)), sample.int(ncol(X), floor(ncol(X) * .3))] <- NA
# 
# data2 <- cbind(X,Y) %>%
#   as_tibble()
# colnames(data2)<- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "Y")
# 
# data2$V1[is.na(data2$V1)] <- mean(data2$V1, na.rm = TRUE)
# data2$V6[is.na(data2$V6)] <- mean(data2$V6, na.rm = TRUE)
# 
# x <- model.matrix(Y~., data2)[,-1]
# y <- data2$V9


#----lasso plot with glmnet--------------------------------------------------------
# estimate lasso
set.seed(123)
lasso1 = glmnet(x3, y3, alpha = 1, family = "gaussian")

# plot coefficient path
plot(lasso1, xvar = "lambda", label = TRUE)

# plot MSE with different lambda
cvlassofit <- cv.glmnet(x1,y1, alpha = 1, family = "gaussian")
plot(cvlassofit)

# Add line to indicate best lambda
lambda_min <- cvlassofit$lambda.min
estimates <- as.vector(coef(lasso, s = lambda_min, exact = TRUE))
norm. <- sum(abs(estimates))
plot(lasso, xlim = range(0, norm., as.vector(lasso$beta)), xvar = "lambda", label = TRUE)
abline(v = norm., col = "red")


#----Lasso plot with flare------------------------------------------
nlamb = 500
ratio = 0.3

lasso2 = slim(X=x3,Y=y3,nlambda=nlamb,lambda.min.ratio=ratio,method="lasso")
matplot(log(lasso2$lambda), t(lasso2$beta), type = "l", main = "Regularization Path", 
        xlab = "log(lambda)", ylab = "Coefficient")

#----Dantzig plot with flare----------------------------------------
dz = slim(X=x1,Y=y1,nlambda=nlamb,lambda.min.ratio=ratio,method="dantzig")
#plot(dz)
matplot(log(dz$lambda), t(dz$beta), type = "l", main = "Regularization Path", 
        xlab = "log(lambda)", ylab = "Coefficient")
plot(dz)

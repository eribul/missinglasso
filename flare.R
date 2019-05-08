library(flare)


## generate data
n = 50
d = 100
X = matrix(rnorm(n * d), n, d)
beta = c(3, 2, 0, 1.5, rep(0, d - 4))
eps = rnorm(n)
Y = X %*% beta + eps

# MIssing data
X[sample.int(nrow(X), 5), sample.int(ncol(X), 10)] <- NA

## Regression with "dantzig", general "lq" and "lasso" respectively
out <- slim(X, Y, method = "dantzig")

## Display results
print(out)
plot(out)
coef(out)

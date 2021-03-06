library(flare)

# Enligt k�llkoden f�r slim:
fix(slim)
# Ser jag att man helt enkelt bara ignorerar cases med NA.
# Tycker allts� inte detta verkar hanteras p� ett vettigt s�tt.

## generate data
n = 50
d = 100
X = matrix(rnorm(n * d), n, d)
beta = c(3, 2, 0, 1.5, rep(0, d - 4))
eps = rnorm(n)
Y = X %*% beta + eps

# MIssing data
X[sample.int(nrow(X), floor(nrow(X) * 1)),
  sample.int(ncol(X), floor(ncol(X) * 1))] <- NA

## Regression with "dantzig"
out <- slim(X, Y, method = "dantzig")

## Display results
print(out)
plot(out)
coef(out)


library(tidyverse)
library(tidymodels)
library(furrr)

set.seed(123)
memory.limit(1e9)
future::plan(multiprocess)


# Synthetic data ----------------------------------------------------------


#' tibble with random data
#' @param rows number of rows
#' @param cols number of columns
#' @param related proportion of variables related to outcome
#' @return data frame with specified number of rows and columns
rdata <- function(cols = 1e3, rows = 1e3, related = 1) {

  # Random functions chosen randomly to generate random data
  # First name of functions (for presentation), then functions themselves
  funs_s  <- sample(c("rnorm", "rexp", "runif"), cols, replace = TRUE)
  funs    <- lapply(funs_s, get)

  rel <- seq_len(cols) <= cols * related

  X <- sapply(funs, exec, rows)
  Xy <- X[,  rel, drop = FALSE]
  Xe <- X[, !rel, drop = FALSE]
  b <- rnorm(ncol(Xy))
  Y <- Xy %*% b + rnorm(rows) * 10 # linear comb with random coefs

  out <- cbind(Y, Xy, Xe)
  colnames(out) <- c(
    "Y",
    paste0("Xy", seq_len(ncol(Xy))),
    if (ncol(Xe) > 0) paste0("Xe", seq_len(ncol(Xe)))
  )
  out
}

#' Remove some data points at random.
#' @param x matrix
#' @param prop proportion of values set to NA
miss <- function(x, prop = 0) {
  X <- x[, -1]
  Y <- x[, 1]
  lx <- length(X)
  X[sample.int(lx, floor(lx * prop))] <- NA
  cbind(Y, X)
}

# Data object -------------------------------------------------------------
df_all <-
  crossing(
    p = seq(10, 50, 1e1), # no of parameters
    n = 100, #seq(10, 50, 1e1)  # no of cases
  ) %>%
  mutate(data = future_map2(p, n, rdata)) %>% # Slumpdata med p cols och n rows
  crossing(missing = seq(0, .5, .1)) %>% # andel som sätts till missing
  mutate(
    data = future_map2(data, missing, miss), # ta bort missing andel från resp
    data = map(data, as_tibble)
  )


# Impute ------------------------------------------------------------------

# KOlla vilka imputeringsmetoder som finns implementerade
ls("package:recipes", pattern = "impute$")

df_imp <-
  df_all %>%
  crossing(
    impute_nm = c(
      "step_bagimpute",
      # "step_knnimpute",    # leder till minnesproblem/krasch
      # "step_lowerimpute",  # för censurerad data
      "step_meanimpute",
      "step_medianimpute",
      # "step_modeimpute",   # Ej för numeriska data
      "step_rollimpute"
    )
  ) %>%
  mutate(
    imp_fun  = map(impute_nm, get),
    rec      = future_map(data, function(x) recipe(Y ~ ., x)),
    imp_rec  = future_map2(rec, imp_fun, ~ .x %>% .y(all_predictors())),
    imp_prep = future_map2(imp_rec, data, prep),
    imp_data = future_map(imp_prep, juice)
  ) %>%
  select(p, n, missing, impute_nm, imp_data)

saveRDS(df_imp, "df_imp.rds") # cache
plan(sequential) # Close workers to free memory



# Lasso -------------------------------------------------------------------

# inkludera bootstrap/CV för model assessment utifrån prediktion?
# rsample::vfold_cv()
# paranip::predict.model_fit()
# yardstick::metric

lasso <- function(dat) {
  linear_reg(mixture = 1) %>%
  set_engine("glmnet") %>%
  fit(Y ~ ., data = dat)
}

df_lasso <-
  df_imp %>%
  mutate(
    lasso = future_map(imp_data, lasso)
  )

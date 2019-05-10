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

  X  <- sapply(funs, exec, rows)
  Xy <- X[,  rel, drop = FALSE]
  Xe <- X[, !rel, drop = FALSE]
  b  <- rnorm(ncol(Xy))
  Y  <- Xy %*% b + rnorm(rows) * 10 # linear comb with random coefs

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
    n = 1000, #seq(10, 50, 1e1)  # no of cases
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

reci <- function(x) {
  recipe(Y ~ ., x) %>%
  step_center(all_predictors()) %>%
  step_scale(all_predictors())
}

df_imp <-
  df_all %>%
  crossing(
    impute_nm = c(
      # "step_bagimpute",    # Funkar men långsamt!
      # "step_knnimpute",    # leder till minnesproblem/krasch
      # "step_lowerimpute",  # för censurerad data
      "step_meanimpute",
      "step_medianimpute",
      # "step_modeimpute",   # Ej för numeriska data
      # "step_rollimpute"    # för tidsbunden data!
      "step_naomit"
    )
  ) %>%
  mutate(
    imp_fun  = map(impute_nm, get),
    rec      = future_map(data, reci),
    imp_rec  = future_map2(rec, imp_fun, ~ .x %>% .y(all_predictors())),
    imp_prep = future_map2(imp_rec, data, prep),
    imp_data = future_map(imp_prep, juice)
  ) %>%
  select(p, n, missing, impute_nm, imp_data)

saveRDS(df_imp, "df_imp.rds") # cache
plan(sequential) # Close workers to free memory


# Lasso -------------------------------------------------------------------

# Fit Lasso regression to data
lasso <- function(dat, penalty) {
  linear_reg(mixture = 1, penalty = penalty) %>%
  set_engine("glmnet") %>%
  fit(Y ~ ., data = dat)
}

# Fins best penalty for lasso using CV
best_lambda <- function(data) {
  glmnet::cv.glmnet(as.matrix(select(data, -Y)), data$Y)$lambda.min
}

# Observed and predicted values to compare
obspred <- function(fit, new_data) {
  bind_cols(truth = new_data$Y, predict(fit, new_data))
}

# Assessment metrics to evaluate prediction power
metr <- function(data) {
  metrics(data, truth, .pred)
}

# Summarise metrics from CV
metr_sum <- function(x) {
  group_by(x, .metric) %>%
  summarise(
    median = median(.estimate, na.rm = TRUE),
    ci_low = quantile(.estimate, .025, na.rm = TRUE),
    ci_hi  = quantile(.estimate, .925, na.rm = TRUE)
  ) %>%
  ungroup()
}

df_lasso <-
  df_imp %>%
  mutate(
    lambda   = future_map_dbl(imp_data, possibly(best_lambda, NA)),
    cv       = map(imp_data, possibly(vfold_cv, NULL)),
    df_train = map(cv, ~ map(.$splits, analysis)),
    df_test  = map(cv, ~ map(.$splits, assessment)),
    fit      = future_map2(
                 df_train, lambda, ~ map(.x, possibly(lasso, NULL), .y)),
    obspred  = future_map2(fit, df_test, map2, possibly(pred, NULL)),
    metr     = future_map(obspred, map, possibly(metr, NULL)),
    metr     = map(metr, possibly(bind_rows, NULL)),
    metr_sum = map(metr, possibly(metr_sum,  tibble()))
  )


# Figures -----------------------------------------------------------------

# Figure data
df_fig <-
  df_lasso %>%
  select(p, n, missing, impute_nm, lambda, metr_sum) %>%
  filter(!is.null(metr_sum)) %>%
  unnest(metr_sum) %>%
  mutate(Imputation = gsub("(^step_)|(impute$)", "", impute_nm))

# Make figure
df_fig %>%
  ggplot(aes(missing, median, label = lambda)) +
  facet_grid(.metric ~ p, scales = "free") +
  geom_line(aes(color = Imputation)) +
  geom_ribbon(
    aes(ymin = ci_low, ymax = ci_hi, fill = Imputation), alpha = .3) +
  theme_light() +
  theme(
    legend.position = "bottom"
  ) +
  scale_x_continuous(minor_breaks = NULL) +
  labs(
    x     = "Model assessment",
    y     = "Proportion of missing data before imputation",
    title = "Missing data decrease predictive power"
  )

ggsave("results.png", height = 20, width = 30, units = "cm")

# Suggestions for the plot
#
# - Add Bag impute
# - Try to extend missingness to 1 or at least higher than 0.5
# - Focus on RMSE and use facet rows for proportion of unrelated features?
# - Skip CI to make figure clearer (at lest if bag imputation overlap as well)

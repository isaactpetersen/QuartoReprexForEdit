# Load libraries
library("mice")

# Impute data
imp <- mice::mice(
  nhanes,
  maxit = 2,
  m = 2)

# Fit regression model to multiply imputed data
fit <- with(
  data = imp,
  exp = lm(bmi ~ hyp + chl))

# Pool results across models
fit_pooled <- mice::pool(fit)

# Summarize pooled results
fit_pooled

summary(fit_pooled)

# Session Info
sessionInfo()

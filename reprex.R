# Load libraries
library("mice")
library("rmarkdown")
library("knitr")

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

rmarkdown::paged_table(fit_pooled)

knitr::kable(fit_pooled)

summary(fit_pooled)

rmarkdown::paged_table(summary(fit_pooled))

knitr::kable(summary(fit_pooled))

# Session Info
sessionInfo()

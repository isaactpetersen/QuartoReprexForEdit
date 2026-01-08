# Load libraries
library("nlme")
library("mice")
library("lme4")
library("broom.mixed")

# Import data
data(Oxboys, package = "nlme")

# Prepare data & add missing values
Oxboys_addNA <- data.frame(Oxboys)

Oxboys_addNA$Subject <- as.integer(Oxboys_addNA$Subject)
Oxboys_addNA$Occasion <- as.integer(Oxboys_addNA$Occasion)

set.seed(52242)
Oxboys_addNA[sample(1:nrow(Oxboys_addNA), 25), "height"] <- NA

dataToImpute <- Oxboys_addNA

head(dataToImpute)

# Imputation settings
numImputations <- 5

varsToImpute <- c("height")

pred <- make.predictorMatrix(dataToImpute)
pred[1:nrow(pred), 1:ncol(pred)] <- 0
pred[varsToImpute, "Subject"] <- (-2) # cluster variable
pred[varsToImpute, "Occasion"] <- 1 # fixed effect predictor
pred[varsToImpute, "age"] <- 2 # random effect predictor
pred[varsToImpute, varsToImpute] <- 1 # fixed effect predictor

diag(pred) <- 0
pred

meth <- make.method(dataToImpute)
meth[1:length(meth)] <- ""
meth[varsToImpute] <- "2l.norm"

# Multiply impute data
mi_mice <- mice(
  as.data.frame(dataToImpute),
  method = meth,
  predictorMatrix = pred,
  m = numImputations,
  maxit = 5,
  seed = 52242)

# Summary and diagnostics
mi_mice

mi_mice$loggedEvents

# Mixed model
fit_lmer <- with(
  data = mi_mice,
  expr = lme4::lmer(height ~ age + Occasion + (1|Subject)))

# Model results
fit_lmer
summary(fit_lmer)

# Pooled results
mice::pool(fit_lmer)

# Session Info
sessionInfo()

library(brms)
library(lme4)

########################################################
#BRMS wrapper
########################################################
#Wrapper for brm models such that it saves the full model the first time it is run, otherwise it loads it from disk
run_model <- function(expr, modelName, path='brmsModels/', reuse = TRUE) {
  path <- paste0(path,'/', modelName, ".brm")
  if (reuse) {
    fit <- suppressWarnings(try(readRDS(path), silent = TRUE))
  }
  if (is(fit, "try-error")) {
    fit <- eval(expr)
    saveRDS(fit, file = path)
  }
  fit
}

########################################################
#This function extracts components of the mixed model and constructs the covariance matrix. From https://stackoverflow.com/a/45655597/905101
########################################################
rescov <- function(model, data) {
  var.d <- crossprod(getME(model,"Lambdat"))
  Zt <- getME(model,"Zt")
  vr <- sigma(model)^2
  var.b <- vr*(t(Zt) %*% var.d %*% Zt)
  sI <- vr * Diagonal(nrow(data))
  var.y <- var.b + sI
  invisible(var.y)
}



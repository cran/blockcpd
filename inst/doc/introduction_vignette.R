## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------

## ----eval = FALSE-------------------------------------------------------------
#  devtools::install_github("https://github.com/Lucas-Prates/blockcpd")

## -----------------------------------------------------------------------------
library(blockcpd)
set.seed(42)
parameters = list(scale = c(1, 2, 3, 1))

# nrow = number of signals
# ncol = number of variables or observations per signal
sim_df <- rcpd(nrow = 20, ncol = 200, 
               family = "exponential", parameters = parameters,
               changepoints = c(50, 110, 180)) 

## ---- echo=FALSE, fig.width=8, fig.height = 7---------------------------------
old_par_config = par(mfrow =  c(2, 2))
plot(sim_df$data_matrix[1, ], ylab = "Signal", main = "Signal 1")
abline(v = sim_df$changepoints, col = 'red', lty = 'dashed')
plot(sim_df$data_matrix[2, ], ylab = "Signal", main = "Signal 2")
abline(v = sim_df$changepoints, col = 'red', lty = 'dashed')
plot(sim_df$data_matrix[3, ], ylab = "Signal", main = "Signal 3")
abline(v = sim_df$changepoints, col = 'red', lty = 'dashed')
plot(sim_df$data_matrix[4, ], ylab = "Signal", main = "Signal 4")
abline(v = sim_df$changepoints, col = 'red', lty = 'dashed')
par(old_par_config)

## -----------------------------------------------------------------------------
  seg_model = fit_blockcpd(sim_df$data_matrix, family = "exponential", 
                           lambda = 1.73)

## ---- fig.width=8, fig.height = 4---------------------------------------------
  plot(seg_model, parameter = "scale")

## ---- echo=FALSE, fig.width=8, fig.height = 4---------------------------------
  mean_signal = apply(sim_df$data_matrix, 2, mean)
  blockcpd_obj = seg_model
  ncp = blockcpd_obj$ncp
  changepoints = blockcpd_obj$changepoints
  parameter_vec = blockcpd_obj$parameters[["scale"]]
  sf = stepfun((1:200)[changepoints], parameter_vec)
  plot(sf, xlim = c(1, 200), do.points = F, xaxs = "i", verticals = F,
       xlab = "Index", ylab = "Scale (Average)",
       ylim = c(0, max(mean_signal) + 1),
       main = paste("Block plot along with 'average signal'"))
  points(mean_signal, col = 'blue')
  abline(v = seg_model$changepoints, col = "red", lty = "dashed")

## ---- fig.width=8, fig.height = 4---------------------------------------------
  model_args = list(family = "exponential") # do not include lambda!

  # search space for lambda
  lambda_left = 0
  lambda_right = 5
  step = "automatic" # can also be set to any numeric value
  
  # uses the data and passed arguments to fit the curve and suggested values
  frv = select_frv(sim_df$data_matrix, lambda_left = lambda_left, step = step,
                   lambda_right = lambda_right, 
                   model_args = model_args)
  
  # plots the curve if the user prefers to perform graphical inspection
  plot(frv)

## ---- fig.width=8, fig.height = 4---------------------------------------------
  seg_model = fit_blockcpd(sim_df$data_matrix, family = "exponential", 
                           lambda = frv$suggested_lambda, 
                           bootstrap = TRUE, bootstrap_samples = 200L)
  confidence_plot(seg_model, scale = "percentage")


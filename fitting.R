##########################################
# Fitting the model                      #
# -------------------------------------- #
# Written by Ladislas Nalborczyk.        #
# E-mail: ladislas.nalborczyk@gmail.com  #
# Last updated on March 7, 2023          #
##########################################

library(tidyverse)
library(optimx)

# importing the data-generating model
source(file = "model.R")

# simulating some data and computing the loss (error)
loss_function <- function (
        par, data,
        nsims = 100, nsamples = 2000,
        exec_threshold = 1, imag_threshold = 0.5, iti = 2
        ) {
    
    # retrieving the parameters
    # amplitude_activ <- par[[1]]
    # peak_time_activ <- par[[2]]
    # curvature_activ <- par[[3]]
    # amplitude_inhib <- par[[4]]
    # peak_time_inhib <- par[[5]]
    # curvature_inhib <- par[[6]]
    # amplitude_inhib_prev <- par[[7]]
    # peak_time_inhib_prev <- par[[8]]
    # curvature_inhib_prev <- par[[9]]
    
    # simulating data from the data-generating model
    results <- model(
        nsims = nsims, nsamples = nsamples,
        exec_threshold = exec_threshold, imag_threshold = imag_threshold, iti = iti,
        amplitude_activ = par[[1]],
        peak_time_activ = par[[2]],
        curvature_activ = par[[3]],
        amplitude_inhib = par[[4]],
        peak_time_inhib = par[[5]],
        curvature_inhib = par[[6]],
        amplitude_inhib_prev = par[[7]],
        peak_time_inhib_prev = par[[8]],
        curvature_inhib_prev = par[[9]]
        )
    
    # retrieving distribution of simulated RTs
    predicted_rt <- results %>%
        dplyr::select(sim, rt = onset_imag) %>%
        # group_by(sim) %>%
        distinct() %>%
        # ungroup() %>%
        pull(rt)
    
    # retrieving distribution of simulated MTs
    predicted_mt <- results %>%
        dplyr::select(sim, mt = mt_imag) %>%
        # group_by(sim) %>%
        distinct() %>%
        # ungroup() %>%
        pull(mt)
    
    # computing the RMSE (combining RTs and MTs predictions)
    prediction_error <- sqrt(
        mean((data$reaction_time - predicted_rt)^2) +
        mean((data$movement_time - predicted_mt)^2)
        )
    
    # returning the prediction error
    return (prediction_error)
    
}

model_fitting <- function (data, method = c("nlminb", "optim", "optimx") ) {
    
    if (method == "nlminb") {
        
        fit <- stats::nlminb(
            start = c(1.5, 0.5, 0.4, 1.5, 0.5, 0.6, 1.5, 0.5, 0.6),
            objective = loss_function,
            data = data,
            lower = c(0, 0, 0, 0, 0, 0, 0, 0, 0),
            upper = c(3, 3, 3, 3, 3, 3, 3, 3, 3)
            )
        
        } else if (method == "optim") {
        
            fit <- stats::optim(
                par = c(1.5, 0.5, 0.4, 1.5, 0.5, 0.6, 1.5, 0.5, 0.6),
                fn = loss_function,
                data = data,
                method = "Nelder-Mead"
                )
        
        } else if (method == "optimx") {
            
            fit <- optimx::optimx(
                par = c(1.5, 0.5, 0.4, 1.5, 0.5, 0.6, 1.5, 0.5, 0.6),
                fn = loss_function,
                data = data
                )
            
        }
    
    return (fit)
    
}

######################################################################
# Fitting the model
# ---------------------------------------------------------------
# Written by Ladislas Nalborczyk
# E-mail: ladislas.nalborczyk@gmail.com
# Last updated on March 7, 2023
##############################################################

library(tidyverse)
library(optimx)

# defining the model
model <- function (
        par, data,
        nsims = 100, nsamples = 2000,
        exec_threshold = 1, imag_threshold = 0.5, iti = 2
        ) {
    
    # defining the activation/inhibition lognormal base function
    activation_inhibition_function <- function (time, amplitude, peak_time, curvature) {
        
        activ <- amplitude * exp(-(log(time) - peak_time)^2 / (2 * curvature^2) )
        
        return (activ)
        
    }
    
    # retrieving the parameters
    amplitude_activ <- par[[1]]
    peak_time_activ <- par[[2]]
    curvature_activ <- par[[3]]
    amplitude_inhib <- par[[4]]
    peak_time_inhib <- par[[5]]
    curvature_inhib <- par[[6]]
    amplitude_inhib_prev <- par[[7]]
    peak_time_inhib_prev <- par[[8]]
    curvature_inhib_prev <- par[[9]]
    
    # initialising the results dataframe
    results <- data.frame(
        sim = rep(1:nsims, each = nsamples),
        sample = rep(1:nsamples, nsims),
        time = rep(1:nsamples, nsims) / 1e3,
        exec_threshold = exec_threshold,
        imag_threshold = imag_threshold,
        activation = numeric(length = nsims * nsamples),
        inhibition = numeric(length = nsims * nsamples),
        inhibition_previous = numeric(length = nsims * nsamples)
        )
    
    for (i in 1:nsims) { # for each simulated experiment
        
        # adding some variability in the other parameters
        amplitude_activ_sim <- rnorm(n = 1, mean = amplitude_activ, sd = 0.1)
        peak_time_activ_sim <- rnorm(n = 1, mean = peak_time_activ, sd = 0.1)
        curvature_activ_sim <- rnorm(n = 1, mean = curvature_activ, sd = 0.01)
        
        amplitude_inhib_sim <- rnorm(n = 1, mean = amplitude_inhib, sd = 0.1)
        peak_time_inhib_sim <- rnorm(n = 1, mean = peak_time_inhib, sd = 0.1)
        curvature_inhib_sim <- rnorm(n = 1, mean = curvature_inhib, sd = 0.01)
        
        amplitude_inhib_prev_sim <- rnorm(n = 1, mean = amplitude_inhib_prev, sd = 0.1)
        peak_time_inhib_prev_sim <- rnorm(n = 1, mean = peak_time_inhib_prev, sd = 0.1)
        curvature_inhib_prev_sim <- rnorm(n = 1, mean = curvature_inhib_prev, sd = 0.01)
        
        # storing activation values for this simulation
        results$activation[results$sim == i] <- activation_inhibition_function(
            time = seq.int(from = 0, to = 5, length.out = nsamples),
            amplitude = amplitude_activ_sim,
            peak_time = peak_time_activ_sim,
            curvature = curvature_activ_sim
            )
        
        # storing inhibition values for this simulation
        results$inhibition[results$sim == i] <- activation_inhibition_function(
            time = seq.int(from = 0, to = 5, length.out = nsamples),
            amplitude = amplitude_inhib_sim,
            peak_time = peak_time_inhib_sim,
            curvature = curvature_inhib_sim
            )
        
        # storing inhibition values from previous trial
        results$inhibition_previous[results$sim == i] <- activation_inhibition_function(
            time = iti + seq.int(from = 0, to = 5, length.out = nsamples),
            amplitude = amplitude_inhib_prev_sim,
            peak_time = peak_time_inhib_prev_sim,
            curvature = curvature_inhib_prev_sim
            )
        
    }
    
    # computing the activation/inhibition balance and
    # implied distributions of RTs and MTs
    results <- results %>%
        group_by(sim) %>%
        mutate(balance = activation / (inhibition + inhibition_previous) ) %>%
        mutate(onset_exec = which(balance > exec_threshold) %>% first() ) %>%
        mutate(offset_exec = which(balance > exec_threshold) %>% last() ) %>%
        mutate(mt_exec = offset_exec - onset_exec) %>%
        mutate(onset_imag = which(balance > imag_threshold) %>% first() ) %>%
        mutate(offset_imag = which(balance > imag_threshold) %>% last() ) %>%
        mutate(mt_imag = offset_imag - onset_imag) %>%
        # convert from ms to seconds
        mutate(
            onset_exec = onset_exec / 1e3,
            offset_exec = offset_exec / 1e3,
            onset_imag = onset_imag / 1e3,
            offset_imag = offset_imag / 1e3,
            mt_exec = mt_exec / 1e3,
            mt_imag = mt_imag / 1e3
            ) %>%
        ungroup()
    
    # retrieving distribution of simulated RTs
    predicted_rt <- results %>%
        dplyr::select(sim, rt = onset_imag) %>%
        group_by(sim) %>%
        distinct() %>%
        ungroup() %>%
        pull(rt)
    
    # retrieving distribution of simulated MTs
    predicted_mt <- results %>%
        dplyr::select(sim, mt = mt_imag) %>%
        group_by(sim) %>%
        distinct() %>%
        ungroup() %>%
        pull(mt)
    
    # computing the RMSE (combining RTs and MTs predictions)
    prediction_error <- sqrt(
        mean(data$reaction_time - predicted_rt)^2 +
        mean(data$movement_time - predicted_mt)^2
        )
    
    # returning the prediction error
    return (prediction_error)
    
}

model_fitting <- function (data, method = c("nlminb", "optim", "optimx") ) {
    
    if (method == "nlminb") {
        
        fit <- stats::nlminb(
            start = c(1.5, 0.5, 0.4, 1.5, 0.5, 0.6, 1.5, 0.5, 0.6),
            objective = model,
            data = data,
            lower = c(0, 0, 0, 0, 0, 0, 0, 0, 0),
            upper = c(3, 3, 3, 3, 3, 3, 3, 3, 3)
            )
        
        } else if (method == "optim") {
        
            fit <- stats::optim(
                par = c(1.5, 0.5, 0.4, 1.5, 0.5, 0.6, 1.5, 0.5, 0.6),
                fn = model,
                data = data,
                method = "Nelder-Mead",
                lower = c(0, 0, 0, 0, 0, 0, 0, 0, 0),
                upper = c(3, 3, 3, 3, 3, 3, 3, 3, 3)
                )
        
        } else if (method == "optimx") {
            
            fit <- optimx::optimx(
                par = c(1.5, 0.5, 0.4, 1.5, 0.5, 0.6, 1.5, 0.5, 0.6),
                fn = model,
                data = data,
                lower = c(0, 0, 0, 0, 0, 0, 0, 0, 0),
                upper = c(3, 3, 3, 3, 3, 3, 3, 3, 3)
                )
            
        }
    
    return (fit)
    
}

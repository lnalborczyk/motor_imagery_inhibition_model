#############################################
# Fitting the model                         #
# Comparing various optimisation algorithms #
# ----------------------------------------- #
# Written by Ladislas Nalborczyk            #
# E-mail: ladislas.nalborczyk@gmail.com     #
# Last updated on March 9, 2023             #
#############################################

library(DEoptim) # global optimisation by differential evolution
library(optimx) # various optimisation methods
library(GenSA) # generalised simulated annealing
library(pso) # particle swarm optimization

# simulating some data and computing the prediction error
loss_function <- function (
        par = c(1, 1, 1), data,
        nsims = 100, nsamples = 2000,
        exec_threshold = 1, imag_threshold = 0.5, iti = 2
        ) {
    
    # retrieving parameter values
    amplitude_activ <- par[[1]]
    peak_time_activ <- par[[2]]
    curvature_activ <- par[[3]]
    amplitude_inhib <- 2 # par[[4]]
    peak_time_inhib <- 0.5 # par[[5]]
    curvature_inhib <- 0.6 # par[[6]]
    amplitude_inhib_prev <- 2 # par[[7]]
    peak_time_inhib_prev <- 0.5 # par[[8]]
    curvature_inhib_prev <- 0.6 # par[[9]]
    
    # simulating some data from the data-generating model
    results <- model(
        nsims = nsims, nsamples = nsamples,
        exec_threshold = exec_threshold, imag_threshold = imag_threshold, iti = iti,
        amplitude_activ = amplitude_activ,
        peak_time_activ = peak_time_activ,
        curvature_activ = curvature_activ,
        amplitude_inhib = amplitude_inhib,
        peak_time_inhib = peak_time_inhib,
        curvature_inhib = curvature_inhib,
        amplitude_inhib_prev = amplitude_inhib_prev,
        peak_time_inhib_prev = peak_time_inhib_prev,
        curvature_inhib_prev = curvature_inhib_prev
        )
    
    # retrieving distribution of simulated RTs
    predicted_rt <- results %>%
        dplyr::select(sim, rt = onset_imag) %>%
        distinct() %>%
        pull(rt) %>%
        # replacing NAs with 0s? or with a very large value?
        replace_na(1e6)
    
    # retrieving distribution of simulated MTs
    predicted_mt <- results %>%
        dplyr::select(sim, mt = mt_imag) %>%
        distinct() %>%
        pull(mt) %>%
        # replacing NAs with 0s? or with a very large value?
        replace_na(1e6)
    
    # computing the RMSE (combining RTs and MTs predictions)
    # not such a good idea, should rather use the distribution's shape...
    # prediction_error <- sqrt(
    #     mean((data$reaction_time - predicted_rt)^2) +
    #     mean((data$movement_time - predicted_mt)^2)
    #     )
    
    # quantile-based loss function (e.g., Ratcliff & Smith, 2004)
    find_quantiles_props <- function(x, quants) {
        
        quants2 <- c(0, quants, Inf) %>% as.numeric()
        quants_props <- numeric(length = length(quants) + 1)
        
        for (i in 1:length(quants_props) ) {
            
            quants_props[i] <- mean(quants2[i] < x & x <= quants2[i + 1])
            
        }
        
        return (quants_props)
        
    }
    
    # checking the function
    # find_quantiles_props(x = data$reaction_time, quants = observed_rt_quantiles) %>% sum()
    
    observed_rt_quantiles <- quantile(x = data$reaction_time, probs = c(0.1, 0.3, 0.5, 0.7, 0.9) )
    observed_mt_quantiles <- quantile(x = data$movement_time, probs = c(0.1, 0.3, 0.5, 0.7, 0.9) )
    
    observed_rt_quantiles_props <- find_quantiles_props(x = data$reaction_time, quants = observed_rt_quantiles)
    observed_mt_quantiles_props <- find_quantiles_props(x = data$movement_time, quants = observed_mt_quantiles)
    
    predicted_rt_quantiles_props <- find_quantiles_props(x = predicted_rt, quants = observed_rt_quantiles)
    predicted_mt_quantiles_props <- find_quantiles_props(x = predicted_mt, quants = observed_mt_quantiles)
    
    # applies a small correction when prop = 0 to avoid negative or Inf g-square
    predicted_rt_quantiles_props <- ifelse(
        test = predicted_rt_quantiles_props == 0,
        yes = predicted_rt_quantiles_props + 0.001,
        no = predicted_rt_quantiles_props
        )
    
    # makes sure proportions sum to 1
    predicted_rt_quantiles_props <- predicted_rt_quantiles_props / sum(predicted_rt_quantiles_props)
    
    # applies a small correction when prop = 0 to avoid negative or Inf g-square
    predicted_mt_quantiles_props <- ifelse(
        test = predicted_mt_quantiles_props == 0,
        yes = predicted_mt_quantiles_props + 0.001,
        no = predicted_mt_quantiles_props
        )
    
    # makes sure proportions sum to 1
    predicted_mt_quantiles_props <- predicted_mt_quantiles_props / sum(predicted_mt_quantiles_props)
    
    # computes the prediction error (combined for RTs and MTs)
    prediction_error <- 2 * (
        # error for RTs
        sum(observed_rt_quantiles_props * log(observed_rt_quantiles_props / predicted_rt_quantiles_props) ) +
            # error for MTs
            sum(observed_mt_quantiles_props * log(observed_mt_quantiles_props / predicted_mt_quantiles_props) )
        )
    
    # adding constraints?
    # for instance that balance should not be above 1 in imagery trials...
    # if (x[1] + x[2]^2 >= 2) prediction_error <- Inf # or 1e6
        
    # returns the prediction error
    return (prediction_error)
    
}

# fitting the model
# see https://cran.r-project.org/web/views/Optimization.html
model_fitting <- function (
        data,
        method = c(
            "SANN", "GenSA", "pso", "DEoptim", "Nelder-Mead", "BFGS",
            "L-BFGS-B", "bobyqa", "nlminb", "all_methods"
            ),
        maxit = 1e2
        ) {
    
    if (method == "SANN") {
        
        fit <- stats::optim(
            par = c(1, 1, 1), # 0.5, 0.5, 0.5, 0.5, 0.5, 0.5),
            fn = loss_function,
            data = data,
            method = method,
            control = list(maxit = maxit, trace = 2)
            )
        
        } else if (method == "GenSA") {
            
            fit <- GenSA::GenSA(
                fn = loss_function,
                data = data,
                par = c(1, 1, 1),
                lower = c(0, 0, 0),
                upper = c(5, 5, 5),
                control = list(maxit = maxit, verbose = TRUE)
                )
        
        } else if (method == "pso") {
            
            fit <- pso::psoptim(
                fn = loss_function,
                data = df,
                par = c(1, 1, 1),
                lower = c(0, 0, 0),
                upper = c(5, 5, 5),
                control = list(maxit = maxit, trace = 2)
                )
            
        } else if (method == "DEoptim") {
            
            fit <- DEoptim::DEoptim(
                fn = loss_function,
                #par = c(1, 1, 1),
                lower = c(0, 0, 0),
                upper = c(5, 5, 5),
                control = DEoptim.control(itermax = maxit, trace = 2),
                data = df
                )
            
        } else if (method %in% c("Nelder-Mead", "BFGS", "L-BFGS-B", "bobyqa", "nlminb") ) {
            
            fit <- optimx::optimx(
                par = c(1, 1, 1), # , 0.5, 0.5, 0.5), # 0.5, 0.5, 0.5),
                fn = loss_function,
                data = data,
                method = method,
                lower = c(0, 0, 0), #, 0, 0, 0), # 0, 0, 0),
                upper = c(5, 5, 5), #, 3, 3, 3), # 3, 3, 3),
                control = list(maxit = maxit, trace = 2)
                )
            
        } else if (method == "all_methods") {
            
            fit <- optimx::optimx(
                par = c(1, 1, 1), #, 1, 1, 1), # 1, 1, 1),
                fn = loss_function,
                data = data,
                lower = c(0, 0, 0), #, 0, 0, 0), # 0, 0, 0),
                upper = c(5, 5, 5), #, 3, 3, 3), # 3, 3, 3),
                control = list(trace = 2, all.methods = TRUE)
                )
            
        }
    
    return (fit)
    
}

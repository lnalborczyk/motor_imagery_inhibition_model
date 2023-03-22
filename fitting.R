#############################################
# Fitting the model                         #
# Comparing various optimisation algorithms #
# ----------------------------------------- #
# Written by Ladislas Nalborczyk            #
# E-mail: ladislas.nalborczyk@gmail.com     #
# Last updated on March 20, 2023            #
#############################################

library(DEoptim) # global optimisation by differential evolution
library(optimx) # various optimisation methods
library(GenSA) # generalised simulated annealing
library(pso) # particle swarm optimisation

# simulating some data and computing the prediction error
loss_function <- function (
        par = c(1, 1, 1, 1), data,
        nsims, nsamples = 2000,
        exec_threshold = 1, imag_threshold = 0.5, iti = 2
        ) {
    
    # how many trials should we simulate? by default nrow(data)
    nsims <- as.numeric(nrow(data) )
    
    # retrieving parameter values
    amplitude_activ <- par[[1]]
    peak_time_activ <- par[[2]]
    curvature_activ <- 0.4
    # amplitude_inhib is expressed in % of amplitude_activ
    # peak_time_inhib is expressed in % of peak_time_activ
    amplitude_inhib <- par[[3]] * amplitude_activ
    peak_time_inhib <- par[[4]] * peak_time_activ
    curvature_inhib <- 0.6
    amplitude_inhib_prev <- 0.5
    peak_time_inhib_prev <- 0.5
    curvature_inhib_prev <- 0.6
    
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
    
    # adding some constraints
    # amplitude_inhib should be >= amplitude_activ in imagined trials...
    # amplitude_activ should be >= amplitude_inhib in executed trials...
    if (unique(data$action_mode) == "imagined" & amplitude_activ > amplitude_inhib) {

        prediction_error <- 1e6
        return (prediction_error)

    } else if (unique(data$action_mode) == "executed" & amplitude_inhib > amplitude_activ) {

        prediction_error <- 1e6
        return (prediction_error)

    }
    
    # retrieving distribution of simulated RTs
    if (unique(data$action_mode) == "imagined") {
        
        predicted_rt <- results %>%
            dplyr::select(sim, rt = onset_imag) %>%
            distinct() %>%
            pull(rt) %>%
            # replacing NAs with a very large value
            replace_na(1e6)
        
        # retrieving distribution of simulated MTs
        predicted_mt <- results %>%
            dplyr::select(sim, mt = mt_imag) %>%
            distinct() %>%
            pull(mt) %>%
            # replacing NAs with a very large value
            replace_na(1e6)
        
    } else if (unique(data$action_mode) == "executed") {
        
        predicted_rt <- results %>%
            dplyr::select(sim, rt = onset_exec) %>%
            distinct() %>%
            pull(rt) %>%
            # replacing NAs with a very large value
            replace_na(1e6)
        
        # retrieving distribution of simulated MTs
        predicted_mt <- results %>%
            dplyr::select(sim, mt = mt_exec) %>%
            distinct() %>%
            pull(mt) %>%
            # replacing NAs with a very large value
            replace_na(1e6)
        
    } else {
        
        warning ("Action-mode should be one of 'imagined' or 'executed'...")
        
    }
    
    # computes the proportion of observations within quantiles
    find_quantiles_props <- function(x, quants) {
        
        quants2 <- c(0, quants, Inf) %>% as.numeric()
        
        # first option
        # quants_props <- numeric(length = length(quants) + 1)
        
        # for (i in 1:length(quants_props) ) {
        #     
        #     quants_props[i] <- mean(quants2[i] < x & x <= quants2[i + 1])
        #     # quants_props[i] <- mean(quants2[i] <= x & x < quants2[i + 1])
        #     
        # }
        
        # second option (similar results but probably faster)
        quants_props <- as.numeric(table(cut(x, quants2) ) ) / length(x)
        
        return (quants_props)
        
    }
    
    # computes observed RT quantiles
    observed_rt_quantiles <- quantile(x = data$reaction_time, probs = c(0.1, 0.3, 0.5, 0.7, 0.9), na.rm = TRUE)
    
    # computes observed MT quantiles
    observed_mt_quantiles <- quantile(x = data$movement_time, probs = c(0.1, 0.3, 0.5, 0.7, 0.9), na.rm = TRUE)
    
    # computes observed proportion of data in RT quantiles
    observed_rt_quantiles_props <- find_quantiles_props(x = data$reaction_time, quants = observed_rt_quantiles)
    
    # computes observed proportion of data in MT quantiles
    observed_mt_quantiles_props <- find_quantiles_props(x = data$movement_time, quants = observed_mt_quantiles)
    
    # computes predicted proportion of data in RT quantiles
    predicted_rt_quantiles_props <- find_quantiles_props(x = predicted_rt, quants = observed_rt_quantiles)
    
    # computes predicted proportion of data in MT quantiles
    predicted_mt_quantiles_props <- find_quantiles_props(x = predicted_mt, quants = observed_mt_quantiles)
    
    # applies a small correction when prop = 0 to avoid negative or Inf g-square
    predicted_rt_quantiles_props <- ifelse(
        test = predicted_rt_quantiles_props == 0,
        yes = 0.0001,
        no = predicted_rt_quantiles_props
        )
    
    # makes sure proportions sum to 1
    predicted_rt_quantiles_props <- predicted_rt_quantiles_props / sum(predicted_rt_quantiles_props)
    
    # applies a small correction when prop = 0 to avoid negative or Inf g-square
    predicted_mt_quantiles_props <- ifelse(
        test = predicted_mt_quantiles_props == 0,
        yes = 0.0001,
        no = predicted_mt_quantiles_props
        )
    
    # makes sure proportions sum to 1
    predicted_mt_quantiles_props <- predicted_mt_quantiles_props / sum(predicted_mt_quantiles_props)
    
    # computes the G^2 prediction error (combined for RTs and MTs)
    # see Ratcliff & Smith (2004, doi:10.1037/0033-295X.111.2.333) or Servant et al. (2019, doi:10.1152/jn.00507.2018.)
    prediction_error <- 2 * (
        # error for RTs
        sum(observed_rt_quantiles_props * log(observed_rt_quantiles_props / predicted_rt_quantiles_props) ) +
            # error for MTs
            sum(observed_mt_quantiles_props * log(observed_mt_quantiles_props / predicted_mt_quantiles_props) )
    )
    
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
            par = c(1, 1, 1, 1),
            fn = loss_function,
            data = data,
            method = method,
            control = list(maxit = maxit, trace = 2)
            )
        
        } else if (method == "GenSA") {
            
            fit <- GenSA::GenSA(
                fn = loss_function,
                data = data,
                par = c(1, 1, 1, 1),
                lower = c(0, 0, 0, 0),
                upper = c(2, 2, 2, 2),
                control = list(maxit = maxit, verbose = TRUE)
                )
        
        } else if (method == "pso") {
            
            fit <- pso::psoptim(
                fn = loss_function,
                data = df,
                par = c(1, 1, 1, 1),
                lower = c(0, 0, 0, 0),
                upper = c(2, 2, 2, 2),
                control = list(maxit = maxit, trace = 2, trace.stats = TRUE)
                )
            
        } else if (method == "DEoptim") {
            
            fit <- DEoptim::DEoptim(
                fn = loss_function,
                lower = c(0, 0, 0, 0),
                upper = c(2, 2, 2, 2),
                control = DEoptim.control(
                    itermax = maxit, trace = 2,
                    # defines the differential evolution strategy (defaults to 2)
                    # strategy = 6,
                    # c controls the speed of the crossover adaptation (defaults to 0)
                    # VTR = 0, c = 0.5,
                    # using all available cores
                    parallelType = "parallel",
                    packages = c("DEoptim", "tidyverse"),
                    parVar = c("model", "loss_function")
                    ),
                data = df
                )
            
        } else if (method %in% c("Nelder-Mead", "BFGS", "L-BFGS-B", "bobyqa", "nlminb") ) {
            
            fit <- optimx::optimx(
                par = c(1, 1, 1, 1),
                fn = loss_function,
                data = data,
                method = method,
                lower = c(0, 0, 0, 0),
                upper = c(2, 2, 2, 2),
                control = list(maxit = maxit, trace = 2)
                )
            
        } else if (method == "all_methods") {
            
            fit <- optimx::optimx(
                par = c(1, 1, 1, 1),
                fn = loss_function,
                data = data,
                lower = c(0, 0, 0, 0),
                upper = c(2, 2, 2, 2),
                control = list(maxit = maxit, trace = 2, all.methods = TRUE)
                )
            
        }
    
    return (fit)
    
}

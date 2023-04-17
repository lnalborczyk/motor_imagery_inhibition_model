#############################################
# Fitting the model                         #
# ----------------------------------------- #
# Written by Ladislas Nalborczyk            #
# E-mail: ladislas.nalborczyk@gmail.com     #
# Last updated on April 11, 2023            #
#############################################

library(optimParallel) # parallelised simplex (L-BFGS-B)
library(hydroPSO) # parallelised particle swarm optimisation
library(DEoptim) # global optimisation by differential evolution
library(optimx) # various optimisation methods
library(GenSA) # generalised simulated annealing
library(pso) # particle swarm optimisation

# simulating some data and computing the prediction error
loss_function <- function (
        par = c(1, 1, 1, 1, 1), data,
        nsims = NULL, nsamples = 3000,
        exec_threshold = 1, imag_threshold = 0.5,
        error_function = c("g2", "rmse", "sse", "wsse")
        ) {
    
    # how many trials should we simulate? if null, by default nrow(data)
    if (is.null(nsims) ) nsims <- as.numeric(nrow(data) )
    
    # defines imagery threshold relative to execution threshold
    imag_threshold <- imag_threshold * exec_threshold
    
    # setting an arbitrary value for the amplitude of the activation function
    amplitude_activ <- 1.5
    
    # setting a value for the ratio amplitude_activ / amplitude_inhib
    amplitude_inhib <- amplitude_activ / par[[1]]
    
    # retrieving parameter values for the activation function
    peak_time_activ <- par[[2]]
    curvature_activ <- par[[3]]
    
    # retrieving parameter values for the inhibition function
    peak_time_inhib <- par[[4]]
    curvature_inhib <- par[[5]]
    
    ############################################################################
    # adding some constraints
    # ------------------------------------------------------------------------
    # balance max should not be above exec_threshold in imagined trials
    # balance max should not be above 2 * exec_threshold in executed trials
    # curvature_activ should be lower than curvature_inhib 
    # balance peak time can not be smaller than the shortest RT
    # imagery threshold cannot be higher than execution threshold
    #########################################################################

    # computing the peak time (mode) of the balance function
    balance_peak_time <- exp(
        (peak_time_activ * curvature_inhib^2 - peak_time_inhib * curvature_activ^2) /
            (curvature_inhib^2 - curvature_activ^2) )
    
    # computing the maximum value of the balance function
    balance_max <- (amplitude_activ / amplitude_inhib) *
        exp(-(log(balance_peak_time) - peak_time_activ)^2 / (2 * curvature_activ^2) +
                (log(balance_peak_time) - peak_time_inhib)^2 / (2 * curvature_inhib^2) )
    
    if (unique(data$action_mode) == "imagined" & !is.na(balance_max) & balance_max > exec_threshold) {

        prediction_error <- 1e9
        return (prediction_error)

    } else if (unique(data$action_mode) == "executed" & !is.na(balance_max) & balance_max > 2 * exec_threshold) {

        prediction_error <- 1e9
        return (prediction_error)
    
    } else if (curvature_activ >= curvature_inhib) {
        
        prediction_error <- 1e9
        return (prediction_error)
    
    } else if (!is.na(balance_peak_time) & balance_peak_time < min(data$reaction_time) ) {

        prediction_error <- 1e9
        return (prediction_error)

    } else if (imag_threshold > exec_threshold) {
        
        prediction_error <- 1e9
        return (prediction_error)
        
    }
    
    # simulating some data from the data-generating model
    results <- model(
        nsims = nsims, nsamples = nsamples,
        exec_threshold = exec_threshold, imag_threshold = imag_threshold,
        amplitude_activ = amplitude_activ,
        peak_time_activ = peak_time_activ,
        curvature_activ = curvature_activ,
        amplitude_inhib = amplitude_inhib,
        peak_time_inhib = peak_time_inhib,
        curvature_inhib = curvature_inhib
        )
    
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
    
    if (error_function == "g2") {
    
        # computes the proportion of observations within quantiles
        find_quantiles_props <- function (x, quants) {
            
            quants2 <- c(0, quants, Inf) %>% as.numeric()
            quants_props <- as.numeric(table(cut(x, quants2) ) ) / length(x)
            
            return (quants_props)
            
        }
        
        # what quantiles should we look at?
        quantile_probs <- c(0.1, 0.3, 0.5, 0.7, 0.9)
        
        # computes observed RT quantiles
        observed_rt_quantiles <- quantile(x = data$reaction_time, probs = quantile_probs, na.rm = TRUE)
        
        # computes observed MT quantiles
        observed_mt_quantiles <- quantile(x = data$movement_time, probs = quantile_probs, na.rm = TRUE)
        
        # computes observed proportion of data in RT quantiles
        observed_rt_quantiles_props <- find_quantiles_props(
            x = data$reaction_time, quants = observed_rt_quantiles
            )
        
        # computes observed proportion of data in MT quantiles
        observed_mt_quantiles_props <- find_quantiles_props(
            x = data$movement_time, quants = observed_mt_quantiles
            )
        
        # computes predicted proportion of data in RT quantiles
        predicted_rt_quantiles_props <- find_quantiles_props(
            x = predicted_rt, quants = observed_rt_quantiles
            )
        
        # computes predicted proportion of data in MT quantiles
        predicted_mt_quantiles_props <- find_quantiles_props(
            x = predicted_mt, quants = observed_mt_quantiles
            )
        
        # applies a small correction when prop = 0 to avoid negative or Inf g-square
        predicted_rt_quantiles_props <- ifelse(
            test = predicted_rt_quantiles_props == 0,
            yes = 0.0001,
            no = predicted_rt_quantiles_props
            )
        
        # makes sure proportions sum to 1
        predicted_rt_quantiles_props <- predicted_rt_quantiles_props /
            sum(predicted_rt_quantiles_props)
        
        # applies a small correction when prop = 0 to avoid negative or Inf g-square
        predicted_mt_quantiles_props <- ifelse(
            test = predicted_mt_quantiles_props == 0,
            yes = 0.0001,
            no = predicted_mt_quantiles_props
            )
        
        # makes sure proportions sum to 1
        predicted_mt_quantiles_props <- predicted_mt_quantiles_props /
            sum(predicted_mt_quantiles_props)
        
        # computes the G^2 prediction error (except it is not multiplied by 2)
        # which is the error for RTs plus the error for MTs
        # see Ratcliff & Smith (2004, doi:10.1037/0033-295X.111.2.333) or
        # Servant et al. (2019, doi:10.1152/jn.00507.2018)
        prediction_error <- sum(observed_rt_quantiles_props * log(observed_rt_quantiles_props / predicted_rt_quantiles_props) ) +
            sum(observed_mt_quantiles_props * log(observed_mt_quantiles_props / predicted_mt_quantiles_props) )
        
    } else if (error_function == "rmse") {
        
        # or RMSE as in Ulrich et al. (2016)
        observed_rt_quantiles <- quantile(x = data$reaction_time, probs = seq(0, 1, 0.1), na.rm = TRUE)
        observed_mt_quantiles <- quantile(x = data$movement_time, probs = seq(0, 1, 0.1), na.rm = TRUE)
        predicted_rt_quantiles <- quantile(x = predicted_rt, probs = seq(0, 1, 0.1), na.rm = TRUE)
        predicted_mt_quantiles <- quantile(x = predicted_mt, probs = seq(0, 1, 0.1), na.rm = TRUE)
        
        # computing the weighted RMSE
        prediction_error <- sqrt((sum(predicted_rt_quantiles - observed_rt_quantiles)^2 +
                sum(predicted_mt_quantiles - observed_mt_quantiles)^2) /
                    (length(predicted_rt_quantiles) + length(predicted_mt_quantiles) ) )
    
    } else if (error_function == "sse") {
        
        # or raw SSE as in Ractliff & Smith (2004)
        observed_rt_quantiles <- quantile(x = data$reaction_time, probs = c(0.1, 0.3, 0.5, 0.7, 0.9), na.rm = TRUE)
        observed_mt_quantiles <- quantile(x = data$movement_time, probs = c(0.1, 0.3, 0.5, 0.7, 0.9), na.rm = TRUE)
        predicted_rt_quantiles <- quantile(x = predicted_rt, probs = c(0.1, 0.3, 0.5, 0.7, 0.9), na.rm = TRUE)
        predicted_mt_quantiles <- quantile(x = predicted_mt, probs = c(0.1, 0.3, 0.5, 0.7, 0.9), na.rm = TRUE)
        
        # computing the weighted SSE
        prediction_error <- sum((predicted_rt_quantiles - observed_rt_quantiles)^2) +
            sum((predicted_mt_quantiles - observed_mt_quantiles)^2)
        
    } else if (error_function == "wsse") {
        
        # or weighted SSE as in Ractliff & Smith (2004)
        observed_rt_quantiles <- quantile(x = data$reaction_time, probs = c(0.1, 0.3, 0.5, 0.7, 0.9), na.rm = TRUE)
        observed_mt_quantiles <- quantile(x = data$movement_time, probs = c(0.1, 0.3, 0.5, 0.7, 0.9), na.rm = TRUE)
        predicted_rt_quantiles <- quantile(x = predicted_rt, probs = c(0.1, 0.3, 0.5, 0.7, 0.9), na.rm = TRUE)
        predicted_mt_quantiles <- quantile(x = predicted_mt, probs = c(0.1, 0.3, 0.5, 0.7, 0.9), na.rm = TRUE)
        
        # quantile weights (cf. Ractliff & Smith, 2004)
        quantile_weights <- c(2, 2, 1, 1, 0.5)
        
        # computing the weighted SSE
        prediction_error <- sum(quantile_weights * (predicted_rt_quantiles - observed_rt_quantiles)^2) +
            sum(quantile_weights * (predicted_mt_quantiles - observed_mt_quantiles)^2)
        
    }
    
    # returns the prediction error
    return (prediction_error)

}

# fitting the model
model_fitting <- function (
        par, data,
        nsims = NULL,
        error_function,
        method = c(
            "SANN", "GenSA", "pso", "DEoptim",
            "Nelder-Mead", "BFGS", "L-BFGS-B", "bobyqa", "nlminb",
            "all_methods", "optimParallel"
            ),
        maxit = 1e2
        ) {
    
    if (method == "SANN") {
        
        fit <- stats::optim(
            par = par,
            fn = loss_function,
            data = data,
            nsims = nsims,
            error_function = error_function,
            method = method,
            control = list(maxit = maxit, trace = 2)
            )
        
        } else if (method == "GenSA") {
            
            fit <- GenSA::GenSA(
                fn = loss_function,
                data = data,
                nsims = nsims,
                error_function = error_function,
                par = par,
                # lower = rep(0, length(par) ),
                # upper = rep(2, length(par) ),
                lower = c(0, -1, 0, -1, 0),
                upper = c(3, 1, 3, 1, 3),
                control = list(maxit = maxit, verbose = TRUE)
                )
        
        } else if (method == "pso") {
            
            fit <- pso::psoptim(
                fn = loss_function,
                data = data,
                par = par,
                nsims = nsims,
                error_function = error_function,
                # lower = rep(0, length(par) ),
                # upper = rep(2, length(par) ),
                lower = c(0, -1, 0, -1, 0),
                upper = c(3, 1, 3, 1, 3),
                control = list(maxit = maxit, trace = 2, trace.stats = TRUE)
                )
            
        } else if (method == "hydroPSO") {
            
            fit <- hydroPSO::hydroPSO(
                fn = loss_function,
                data = data,
                nsims = nsims,
                error_function = error_function,
                par = par,
                # lower = rep(0, length(par) ),
                # upper = rep(2, length(par) ),
                lower = c(0, -1, 0, -1, 0),
                upper = c(3, 1, 3, 1, 3),
                control = list(
                    maxit = maxit,
                    verbose = TRUE,
                    # using all available cores
                    parallel = "parallel"
                    )
                )
            
        } else if (method == "DEoptim") {
            
            fit <- DEoptim::DEoptim(
                fn = loss_function,
                data = data,
                nsims = nsims,
                error_function = error_function,
                # lower = rep(0, length(par) ),
                # upper = rep(2, length(par) ),
                lower = c(0, -1, 0, -1, 0),
                upper = c(3, 1, 3, 1, 3),
                control = DEoptim.control(
                    itermax = maxit, trace = TRUE,
                    # defines the differential evolution strategy (defaults to 2)
                    # strategy = 6,
                    # value to reach (defaults to -Inf)
                    # VTR = 0,
                    # number of population members (by default 10*length(lower) )
                    # NP = 100,
                    # c controls the speed of the crossover adaptation
                    # when strategy = 6 (defaults to 0)
                    # c = 0.4,
                    # using all available cores
                    parallelType = "parallel",
                    packages = c("DEoptim", "tidyverse"),
                    parVar = c("model", "loss_function")
                    )
                )
            
        } else if (method %in% c("Nelder-Mead", "BFGS", "L-BFGS-B", "bobyqa", "nlminb") ) {
            
            fit <- optimx::optimx(
                par = par,
                fn = loss_function,
                data = data,
                nsims = nsims,
                error_function = error_function,
                method = method,
                # lower = rep(0, length(par) ),
                # upper = rep(2, length(par) ),
                lower = c(0, -1, 0, -1, 0),
                upper = c(3, 1, 3, 1, 3),
                control = list(maxit = maxit, trace = 6)
                )
            
        } else if (method == "all_methods") {
            
            fit <- optimx::optimx(
                par = par,
                fn = loss_function,
                data = data,
                nsims = nsims,
                error_function = error_function,
                # lower = rep(0, length(par) ),
                # upper = rep(2, length(par) ),
                lower = c(0, -1, 0, -1, 0),
                upper = c(3, 1, 3, 1, 3),
                control = list(maxit = maxit, trace = 2, all.methods = TRUE)
                )
            
        } else if (method == "optimParallel") {
            
            # using half of all available cores by default
            cl <- makeCluster(detectCores() / 2)
            
            # defining this as the default cluster
            setDefaultCluster(cl = cl)
            
            # loading the tidyverse package on each cluster
            # clusterEvalQ(cl, library(optimParallel) )
            clusterEvalQ(cl, library(tidyverse) )
            
            # loading model and loss function
            clusterExport(cl, c("model", "loss_function") )
            
            # parallel optimisation
            fit <- optimParallel::optimParallel(
                par = par,
                fn = loss_function,
                data = data,
                nsims = nsims,
                error_function = error_function,
                # lower = rep(0, length(par) ),
                # upper = rep(2, length(par) ),
                lower = c(0, -1, 0, -1, 0),
                upper = c(3, 1, 3, 1, 3),
                verbose = TRUE
                )
            
            # stopping the cluster
            stopCluster(cl)
            
        }
    
    return (fit)
    
}

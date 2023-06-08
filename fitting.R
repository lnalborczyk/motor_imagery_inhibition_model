#############################################
# Fitting the model                         #
# ----------------------------------------- #
# Written by Ladislas Nalborczyk            #
# E-mail: ladislas.nalborczyk@gmail.com     #
# Last updated on June 7, 2023              #
#############################################

library(optimParallel) # parallelised simplex (L-BFGS-B)
library(hydroPSO) # parallelised particle swarm optimisation
library(DEoptim) # global optimisation by differential evolution
library(optimx) # various optimisation methods
library(GenSA) # generalised simulated annealing
library(pso) # particle swarm optimisation
library(tgp) # latin hypercube sampling

# simulating some data and computing the prediction error
loss_function <- function (
    par, data,
    nsims = NULL, nsamples = 3000,
    # threshold modulation model ("tmm") or parallel inhibition model ("pim")
    model_version = "tmm",
    exec_threshold = 1, imag_threshold = 0.5,
    error_function = c("g2", "rmse", "sse", "wsse", "ks")
    ) {
    
    # how many trials should we simulate? if null, by default nrow(data)
    if (is.null(nsims) ) nsims <- as.numeric(nrow(data) )
    
    # what version of the model?
    if (model_version == "tmm") {
        
        # setting an arbitrary value for the amplitude of the activation function
        amplitude_activ <- par[[1]]
        
        # retrieving parameter values for the activation function
        peak_time_activ <- log(par[[2]])
        curvature_activ <- par[[3]]
        
        # retrieving parameter values for the execution threshold
        exec_threshold <- par[[4]] * amplitude_activ
        
        # defining imagery threshold relative to execution threshold
        imag_threshold <- imag_threshold * exec_threshold
        
        # the relative imagery threshold could be learned as well
        # imag_threshold <- par[[4]] * exec_threshold
        
        ################################################################################
        # adding some constraints
        # ----------------------------------------------------------------------------
        # predicted RTs/MTs should be valid (not a NaN)
        # imagery threshold cannot be higher than execution threshold
        # balance max should not be above exec_threshold in imagined trials
        # balance max should not be above 4 * exec_threshold in executed trials
        # balance value at the end of the trial should be below 0.25
        ##########################################################################
        
        # defining a function to compute the predicted RT and MT (quadratic formula)
        onset_offset <- function (alpha, mu, sigma, thresh) {
            
            onset <-  exp(mu - sqrt(-2 * sigma^2 * log(thresh / alpha) ) )
            offset <- exp(mu + sqrt(-2 * sigma^2 * log(thresh / alpha) ) )
            
            return (c(onset, offset) )
            
        }
        
        # defining the activation/inhibition rescaled lognormal function
        activation_function <- function (
        exec_threshold = 1, imag_threshold = 0.5,
        amplitude = 1.5, peak_time = 0, curvature = 0.4
        ) {
            
            # adding some variability in the other parameters
            # variability is currently fixed but could also be estimated
            amplitude_sim <- rnorm(n = 1, mean = amplitude, sd = 0.01)
            peak_time_sim <- rnorm(n = 1, mean = peak_time, sd = 0.01)
            curvature_sim <- rnorm(n = 1, mean = curvature, sd = 0.01)
            exec_threshold_sim <- rnorm(n = 1, mean = exec_threshold, sd = 0.01)
            
            # no variability in the motor imagery threshold
            # imag_threshold_sim <- rnorm(n = 1, mean = imag_threshold, sd = 0.01)
            imag_threshold_sim <- imag_threshold
            
            # computing the predicted RT and MT in imagery
            onset_offset_imag <- onset_offset(
                alpha = amplitude_sim,
                mu = peak_time_sim,
                sigma = curvature_sim,
                thresh = imag_threshold_sim
                )
            
            onset_imag <- min(onset_offset_imag)
            mt_imag <- max(onset_offset_imag) - min(onset_offset_imag)
            
            # computing the predicted RT and MT in execution
            onset_offset_exec <- onset_offset(
                alpha = amplitude_sim,
                mu = peak_time_sim,
                sigma = curvature_sim,
                thresh = exec_threshold_sim
                )
            
            onset_exec <- min(onset_offset_exec)
            mt_exec <- max(onset_offset_exec) - min(onset_offset_exec)
            
            # returning it
            return (data.frame(onset_imag, mt_imag, onset_exec, mt_exec) )
            
        }
        
        # computing the predicted RT and MT
        predicted_rt_mt <- suppressWarnings(activation_function(
            amplitude = amplitude_activ,
            peak_time = peak_time_activ,
            curvature = curvature_activ,
            exec_threshold = exec_threshold,
            imag_threshold = imag_threshold
            ) )
        
        if (unique(data$action_mode) == "imagined") {
            
            predicted_rt_mt <- predicted_rt_mt[1:2]
            
        } else if (unique(data$action_mode) == "executed") {
            
            predicted_rt_mt <- predicted_rt_mt[3:4]
            
        }
        
        # computing the balance output at the end of the trial (i.e., when time = 3)
        balance_end_of_trial <- amplitude_activ * exp(-(log(nsamples / 1e3) - peak_time_activ)^2 / (2 * curvature_activ^2) )
        
        # coding the constraints (penalising by setting the prediction error to +Inf)
        if (any(is.na(predicted_rt_mt) ) ) {
            
            prediction_error <- Inf
            return (prediction_error)
            
        } else if (imag_threshold >= exec_threshold) {
            
            prediction_error <- Inf
            return (prediction_error)
            
        } else if (unique(data$action_mode) == "imagined" & !is.na(peak_time_activ) & peak_time_activ >= exec_threshold) {
            
            prediction_error <- Inf
            return (prediction_error)
            
        } else if (unique(data$action_mode) == "executed" & !is.na(peak_time_activ) & peak_time_activ >= 4 * exec_threshold) {
            
            prediction_error <- Inf
            return (prediction_error)
            
        } else if (balance_end_of_trial >= 0.5 * imag_threshold) {
            
            prediction_error <- Inf
            return (prediction_error)
            
        }
        
        # simulating some data
        results <- data.frame(
            sim = rep(1:nsims, each = nsamples),
            exec_threshold = exec_threshold,
            imag_threshold = imag_threshold
            ) %>%
            group_by(sim) %>%
            do(
                suppressWarnings(
                    activation_function(
                        amplitude = amplitude_activ,
                        peak_time = peak_time_activ,
                        curvature = curvature_activ,
                        exec_threshold = exec_threshold,
                        imag_threshold = imag_threshold
                        )
                    )
                ) %>%
            ungroup()
        
    } else if (model_version == "pim") {
        
        # defines imagery threshold relative to execution threshold
        imag_threshold <- imag_threshold * exec_threshold
        
        # setting an arbitrary value for the amplitude of the activation function
        amplitude_activ <- 1.5
        
        # retrieving parameter values for the activation function
        peak_time_activ <- log(par[[2]])
        curvature_activ <- par[[3]]
        
        # fixing curvature_activ to 1/sqrt(6)?
        # https://www.mdpi.com/1099-4300/19/2/56
        # https://en.wikipedia.org/wiki/Log-normal_distribution#cite_note-Bloetscher-45
        # curvature_activ <- 1 / sqrt(6)
        
        # setting a value for the ratio amplitude_activ / amplitude_inhib
        amplitude_inhib <- amplitude_activ / par[[1]]
        
        # retrieving parameter values for the inhibition function
        # peak_time_inhib <- log(par[[3]] * par[[2]])
        peak_time_inhib <- log(par[[2]])
        curvature_inhib <- par[[4]] * par[[3]]
        
        ############################################################################
        # adding some constraints
        # -------------------------------------------------------------------------
        # predicted RTs/MTs should be valid (not a NaN)
        # curvature_activ should be lower than curvature_inhib
        # imagery threshold cannot be higher than execution threshold
        # balance max should not be above exec_threshold in imagined trials
        # balance max should not be above 4 * exec_threshold in executed trials
        # balance value at the end of the trial should be below 0.25
        ##########################################################################
        
        # defining a function to compute the predicted RT and MT (quadratic formula)
        onset_offset <- function (alpha_f, alpha_g, mu_f, mu_g, sigma_f, sigma_g, thresh) {
            
            a <- sigma_g^2 - sigma_f^2
            b <- 2 * (sigma_f^2 * mu_g - sigma_g^2 * mu_f)
            c <- sigma_f^2 * mu_g^2 - sigma_g^2 * mu_f^2 - 2 * sigma_f^2 * sigma_g^2 * (log(alpha_f / alpha_g) - log(thresh) )
            onset <- exp((-b - sqrt(b^2 - 4 * a * c) ) / (2 * a) )
            offset <- exp((-b + sqrt(b^2 - 4 * a * c) ) / (2 * a) )
            
            return (c(onset, offset) )
            
        }
        
        # defining the balance function
        # basically a ratio of two rescaled lognormal functions
        balance_function <- function (
        exec_threshold = 1, imag_threshold = 0.5,
        amplitude_activ = 1.5, peak_time_activ = 0, curvature_activ = 0.4,
        amplitude_inhib = 1.5, peak_time_inhib = 0, curvature_inhib = 0.6
        ) {
            
            # adding some variability in the other parameters
            # variability is currently fixed but could also be estimated
            amplitude_activ_sim <- rnorm(n = 1, mean = amplitude_activ, sd = 0.01)
            peak_time_activ_sim <- rnorm(n = 1, mean = peak_time_activ, sd = 0.01)
            curvature_activ_sim <- rnorm(n = 1, mean = curvature_activ, sd = 0.01)
            
            amplitude_inhib_sim <- rnorm(n = 1, mean = amplitude_inhib, sd = 0.01)
            peak_time_inhib_sim <- rnorm(n = 1, mean = peak_time_inhib, sd = 0.01)
            curvature_inhib_sim <- rnorm(n = 1, mean = curvature_inhib, sd = 0.01)
                
            # in this model, there is no variation in the thresholds
            # exec_threshold_sim <- exec_threshold
            # imag_threshold_sim <- imag_threshold
            
            # computing the predicted RT and MT in imagery
            onset_offset_imag <- onset_offset(
                alpha_f = amplitude_activ_sim, alpha_g = amplitude_inhib_sim,
                mu_f = peak_time_activ_sim, mu_g = peak_time_inhib_sim,
                sigma_f = curvature_activ_sim, sigma_g = curvature_inhib_sim,
                thresh = imag_threshold
                )
            
            onset_imag <- min(onset_offset_imag)
            mt_imag <- max(onset_offset_imag) - min(onset_offset_imag)
            
            # computing the predicted RT and MT in execution
            onset_offset_exec <- onset_offset(
                alpha_f = amplitude_activ_sim, alpha_g = amplitude_inhib_sim,
                mu_f = peak_time_activ_sim, mu_g = peak_time_inhib_sim,
                sigma_f = curvature_activ_sim, sigma_g = curvature_inhib_sim,
                thresh = exec_threshold
                )
            
            onset_exec <- min(onset_offset_exec)
            mt_exec <- max(onset_offset_exec) - min(onset_offset_exec)
            
            # returning it
            return (data.frame(onset_imag, mt_imag, onset_exec, mt_exec) )
            
        }
        
        # computing the predicted RT and MT
        predicted_rt_mt <- suppressWarnings(balance_function(
            exec_threshold = exec_threshold,
            imag_threshold = imag_threshold,
            amplitude_activ = amplitude_activ,
            peak_time_activ = peak_time_activ,
            curvature_activ = curvature_activ,
            amplitude_inhib = amplitude_inhib,
            peak_time_inhib = peak_time_inhib,
            curvature_inhib = curvature_inhib
            ) )
        
        if (unique(data$action_mode) == "imagined") {
            
            predicted_rt_mt <- predicted_rt_mt[1:2]
            
        } else if (unique(data$action_mode) == "executed") {
            
            predicted_rt_mt <- predicted_rt_mt[3:4]
            
        }
        
        # computing the peak time (mode) of the balance function
        balance_peak_time <- exp(
            (peak_time_activ * curvature_inhib^2 - peak_time_inhib * curvature_activ^2) /
                (curvature_inhib^2 - curvature_activ^2) )
        
        # computing the maximum value of the balance function
        balance_max <- (amplitude_activ / amplitude_inhib) *
            exp(-(log(balance_peak_time) - peak_time_activ)^2 / (2 * curvature_activ^2) +
                    (log(balance_peak_time) - peak_time_inhib)^2 / (2 * curvature_inhib^2) )
        
        # computing the balance output at the end of the trial (i.e., when time = 3)
        balance_end_of_trial <- (amplitude_activ / amplitude_inhib) *
            exp(-(log(nsamples / 1e3) - peak_time_activ)^2 / (2 * curvature_activ^2) +
                    (log(nsamples / 1e3) - peak_time_inhib)^2 / (2 * curvature_inhib^2) )
        
        # coding the constraints (penalising by setting the prediction error to +Inf)
        if (any(is.na(predicted_rt_mt) ) ) {
            
            prediction_error <- Inf
            return (prediction_error)
            
        } else if (curvature_activ >= curvature_inhib) {
            
            prediction_error <- Inf
            return (prediction_error)
            
        } else if (imag_threshold >= exec_threshold) {
            
            prediction_error <- Inf
            return (prediction_error)
            
        } else if (unique(data$action_mode) == "imagined" & !is.na(balance_max) & balance_max >= exec_threshold) {
            
            prediction_error <- Inf
            return (prediction_error)
            
        } else if (unique(data$action_mode) == "executed" & !is.na(balance_max) & balance_max >= 4 * exec_threshold) {
            
            prediction_error <- Inf
            return (prediction_error)
            
        } else if (balance_end_of_trial >= 0.25) {
            
            prediction_error <- Inf
            return (prediction_error)
            
        }
        
        # simulating some data
        results <- data.frame(
            sim = rep(1:nsims, each = nsamples),
            exec_threshold = exec_threshold,
            imag_threshold = imag_threshold
            ) %>%
            group_by(sim) %>%
            do(
                suppressWarnings(
                    balance_function(
                        exec_threshold = exec_threshold,
                        imag_threshold = imag_threshold,
                        amplitude_activ = amplitude_activ,
                        peak_time_activ = peak_time_activ,
                        curvature_activ = curvature_activ,
                        amplitude_inhib = amplitude_inhib,
                        peak_time_inhib = peak_time_inhib,
                        curvature_inhib = curvature_inhib
                        )
                    )
                ) %>%
            ungroup()
        
    }
    
    # retrieving distribution of simulated RTs
    if (unique(data$action_mode) == "imagined") {
        
        # retrieving distribution of simulated RTs
        predicted_rt <- replace_na(data = results$onset_imag, replace = 1e6)
        
        # retrieving distribution of simulated MTs
        predicted_mt <- replace_na(data = results$mt_imag, replace = 1e6)
        
    } else if (unique(data$action_mode) == "executed") {
        
        # retrieving distribution of simulated RTs
        predicted_rt <- replace_na(data = results$onset_exec, replace = 1e6)
        
        # retrieving distribution of simulated MTs
        predicted_mt <- replace_na(data = results$mt_exec, replace = 1e6)
        
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
            yes = 1e-6,
            no = predicted_rt_quantiles_props
            )
        
        # makes sure proportions sum to 1
        predicted_rt_quantiles_props <- predicted_rt_quantiles_props /
            sum(predicted_rt_quantiles_props)
        
        # applies a small correction when prop = 0 to avoid negative or Inf g-square
        predicted_mt_quantiles_props <- ifelse(
            test = predicted_mt_quantiles_props == 0,
            yes = 1e-6,
            no = predicted_mt_quantiles_props
            )
        
        # makes sure proportions sum to 1
        predicted_mt_quantiles_props <- predicted_mt_quantiles_props /
            sum(predicted_mt_quantiles_props)
        
        # computes the G^2 prediction error
        # which is the error for RTs plus the error for MTs
        # see Ratcliff & Smith (2004, doi:10.1037/0033-295X.111.2.333) or
        # Servant et al. (2019, doi:10.1152/jn.00507.2018)
        prediction_error <- 2 * sum(observed_rt_quantiles_props * log(observed_rt_quantiles_props / predicted_rt_quantiles_props) ) +
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
        
        # or weighted SSE as in Ratcliff & Smith (2004)
        observed_rt_quantiles <- quantile(x = data$reaction_time, probs = c(0.1, 0.3, 0.5, 0.7, 0.9), na.rm = TRUE)
        observed_mt_quantiles <- quantile(x = data$movement_time, probs = c(0.1, 0.3, 0.5, 0.7, 0.9), na.rm = TRUE)
        predicted_rt_quantiles <- quantile(x = predicted_rt, probs = c(0.1, 0.3, 0.5, 0.7, 0.9), na.rm = TRUE)
        predicted_mt_quantiles <- quantile(x = predicted_mt, probs = c(0.1, 0.3, 0.5, 0.7, 0.9), na.rm = TRUE)
        
        # quantile weights (cf. Ratcliff & Smith, 2004)
        quantile_weights <- c(2, 2, 1, 1, 0.5)
        
        # computing the weighted SSE
        prediction_error <- sum(quantile_weights * (predicted_rt_quantiles - observed_rt_quantiles)^2) +
            sum(quantile_weights * (predicted_mt_quantiles - observed_mt_quantiles)^2)
     
    } else if (error_function == "ks") {
        
        # computes the KS statistic
        prediction_error <- as.numeric(ks.test(x = predicted_rt, y = data$reaction_time)$statistic) +
            as.numeric(ks.test(x = predicted_mt, y = data$movement_time)$statistic)
        
    }
    
    # if NA, replacing it by +Inf
    if (is.na(prediction_error) ) prediction_error <- Inf
    
    # returning the prediction error
    return (prediction_error)

}

# fitting the model
model_fitting <- function (
        par, data,
        nsims = NULL,
        par_names = NULL,
        lower_bounds, upper_bounds,
        nstudies = 200, initial_pop_while = FALSE,
        error_function, model_version,
        method = c(
            "SANN", "GenSA", "pso", "hydroPSO", "DEoptim",
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
                lower = lower_bounds,
                upper = upper_bounds,
                control = list(maxit = maxit, verbose = TRUE)
                )
        
        } else if (method == "pso") {
            
            fit <- pso::psoptim(
                fn = loss_function,
                data = data,
                par = par,
                nsims = nsims,
                error_function = error_function,
                lower = lower_bounds,
                upper = upper_bounds,
                control = list(maxit = maxit, trace = 2, trace.stats = TRUE)
                )
            
        } else if (method == "hydroPSO") {
            
            fit <- hydroPSO::hydroPSO(
                fn = loss_function,
                data = data,
                nsims = nsims,
                error_function = error_function,
                par = par,
                lower = lower_bounds,
                upper = upper_bounds,
                control = list(
                    maxit = maxit,
                    verbose = TRUE,
                    # using all available cores
                    parallel = "parallel"
                    )
                )
            
        } else if (method == "DEoptim") {
            
            if (initial_pop_while == TRUE) {
                
                # function for generating plausible starting values
                source (file = paste0("utils/hypercube_sampling_while_", model_version, ".R") )
            
                # generating plausible starting values
                lhs_initial_pop <- generating_initial_pop(
                    nstudies = nstudies,
                    action_mode = unique(data$action_mode),
                    par_names = par_names,
                    lower_bounds = lower_bounds, upper_bounds = upper_bounds
                    )
                
            } else {
                
                # initialising an empty dataframe
                lhs_initial_pop_df <- data.frame(matrix(data = NA, nrow = nstudies, ncol = length(lower_bounds) ) )
                
                # populating it with hypercube samples
                for (i in 1:length(lower_bounds) ){
                    
                    lhs_initial_pop_df[, i] <- lhs(n = nstudies, rect = c(lower_bounds[i], upper_bounds[i]) )[, 1]
                    
                }
                
                # lhs_initial_pop <- as.matrix(data.frame(
                #     lhs(n = nstudies, rect = c(lower_bounds[1], upper_bounds[1]) )[, 1],
                #     lhs(n = nstudies, rect = c(lower_bounds[2], upper_bounds[2]) )[, 1],
                #     lhs(n = nstudies, rect = c(lower_bounds[3], upper_bounds[3]) )[, 1],
                #     lhs(n = nstudies, rect = c(lower_bounds[4], upper_bounds[4]) )[, 1]
                #     ) )
                
                # converting to a matrix (as requested by deoptim)
                lhs_initial_pop <- as.matrix(lhs_initial_pop_df)
                
            }
            
            # starting the optimisation
            fit <- DEoptim::DEoptim(
                fn = loss_function,
                data = data,
                nsims = nsims,
                error_function = error_function,
                model_version = model_version,
                lower = lower_bounds,
                upper = upper_bounds,
                control = DEoptim.control(
                    # maximum number of iterations
                    itermax = maxit,
                    # printing progress iteration
                    trace = TRUE,
                    # printing progress every 10 iterations
                    # trace = 10,
                    # defines the differential evolution strategy (defaults to 2)
                    # 1: DE / rand / 1 / bin (classical strategy)
                    # 2: DE / local-to-best / 1 / bin (default)
                    # 3: DE / best / 1 / bin with jitter
                    # 4: DE / rand / 1 / bin with per-vector-dither
                    # 5: DE / rand / 1 / bin with per-generation-dither
                    # 6: DE / current-to-p-best / 1
                    strategy = 3,
                    # value to reach (defaults to -Inf)
                    VTR = 0,
                    # number of population members (by default 10*length(lower) )
                    # NP = 200,
                    NP = nrow(lhs_initial_pop),
                    # F is the mutation constant (defaults to 0.8)
                    F = 0.9,
                    # crossover probability (recombination) (defaults to 0.5)
                    CR = 0.95,
                    # c controls the speed of the crossover adaptation
                    # when strategy = 6 (defaults to 0)
                    # c = 0.1,
                    # proportion of best solutions to use in the mutation
                    # when strategy = 6 (defaults to 0.2)
                    # p = 0.1,
                    # defining the initial population using lhs
                    initialpop = lhs_initial_pop,
                    # when to stop optimisation
                    reltol = 1e-6,
                    # number of iteration after which to stop the optimisation
                    # if there is no improvement
                    steptol = 1000,
                    # using all available cores
                    parallelType = "parallel",
                    packages = c("DEoptim", "tidyverse", "lhs"),
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
                lower = lower_bounds,
                upper = upper_bounds,
                control = list(maxit = maxit, trace = 6)
                )
            
        } else if (method == "all_methods") {
            
            fit <- optimx::optimx(
                par = par,
                fn = loss_function,
                data = data,
                nsims = nsims,
                error_function = error_function,
                lower = lower_bounds,
                upper = upper_bounds,
                control = list(maxit = maxit, trace = 2, all.methods = TRUE)
                )
            
        } else if (method == "optimParallel") {
            
            # using half of all available cores by default
            # cl <- makeCluster(detectCores() / 2)
            
            # using all available cores by default
            cl <- makeCluster(detectCores() )
            
            # defining this as the default cluster
            setDefaultCluster(cl = cl)
            
            # loading the tidyverse package on each cluster
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
                lower = lower_bounds,
                upper = upper_bounds,
                verbose = TRUE
                )
            
            # stopping the cluster
            stopCluster(cl)
            
        }
    
    return (fit)
    
}

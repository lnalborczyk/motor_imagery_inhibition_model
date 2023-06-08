##############################################
# Parameter recovery study                   #
# PIM with 4 free pars                       #
# ------------------------------------------ #
# Written by Ladislas Nalborczyk             #
# E-mail: ladislas.nalborczyk@gmail.com      #
# Last updated on June 5, 2023               #
##############################################

library(DEoptim) # global optimisation by differential evolution
library(dplyr) # data wrangling
library(tidyr) # data wrangling
library(tgp) # latin hypercube sampling

# get the number of available cores
ncores <- 8
cat(ncores, "cores will be used.")
cl <- makeCluster(ncores)
parallel::clusterEvalQ(cl = cl, expr = {library(tgp); library(DEoptim); library(dplyr); library(tidyr)})

# model function
model <- function (
        nsims = 100, nsamples = 3000,
        exec_threshold = 1, imag_threshold = 0.5,
        amplitude_activ = 1.5, peak_time_activ = 0, curvature_activ = 0.4
        ) {
    
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
        imag_threshold_sim <- rnorm(n = 1, mean = imag_threshold, sd = 0.01)
        
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
    
    # computing the activation/inhibition balance
    # and the implied/predicted distributions of RTs and MTs 
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
    
    # returning the results
    return (results)
    
}

# simulating some data and computing the prediction error
loss_function <- function (
        par, data,
        nsims = NULL, nsamples = 3000,
        exec_threshold = 1, imag_threshold = 0.5
        ) {
    
    # how many trials should we simulate? if null, by default nrow(data)
    if (is.null(nsims) ) nsims <- as.numeric(nrow(data) )
    
    # defines execution threshold
    exec_threshold <- par[[1]]
    
    # defines imagery threshold relative to execution threshold
    imag_threshold <- imag_threshold * exec_threshold
    
    # retrieving parameter values for the activation function
    amplitude_activ <- par[[2]]
    peak_time_activ <- log(par[[3]])
    curvature_activ <- par[[4]]
    
    # setting a value for the ratio amplitude_activ / amplitude_inhib
    # amplitude_inhib <- amplitude_activ / par[[1]]
    
    # retrieving parameter values for the inhibition function
    # peak_time_inhib <- log(par[[3]] * par[[2]])
    # curvature_inhib <- par[[5]] * par[[4]]
    
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
        imag_threshold_sim <- rnorm(n = 1, mean = imag_threshold, sd = 0.01)
        
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
    
    # retrieving distribution of simulated RTs
    if (unique(data$action_mode) == "imagined") {
        
        predicted_rt <- replace_na(data = results$onset_imag, replace = 1e6)
        predicted_mt <- replace_na(data = results$mt_imag, replace = 1e6)
        
    } else if (unique(data$action_mode) == "executed") {
        
        predicted_rt <- replace_na(data = results$onset_exec, replace = 1e6)
        predicted_mt <- replace_na(data = results$mt_exec, replace = 1e6)
        
    } else {
        
        warning ("Action-mode should be one of 'imagined' or 'executed'...")
        
    }
        
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
    
    # if NA, replacing it by Inf
    if (is.na(prediction_error) ) prediction_error <- Inf
    
    # returning the prediction error
    return (prediction_error)
    
}

# fitting the model
model_fitting <- function (
        par, data,
        nsims = NULL,
        lower_bounds, upper_bounds,
        nstudies = 200,
        initial_pop_while = FALSE,
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
            lower = c(0.25, -1, 0.1, 1),
            upper = c(4, 0.5, 0.6, 2),
            control = list(maxit = maxit, verbose = TRUE)
            )
        
    } else if (method == "pso") {
        
        fit <- pso::psoptim(
            fn = loss_function,
            data = data,
            par = par,
            nsims = nsims,
            error_function = error_function,
            lower = c(0.25, -1, 0.1, 1),
            upper = c(4, 0.5, 0.6, 2),
            control = list(maxit = maxit, trace = 2, trace.stats = TRUE)
            )
        
    } else if (method == "hydroPSO") {
        
        fit <- hydroPSO::hydroPSO(
            fn = loss_function,
            data = data,
            nsims = nsims,
            error_function = error_function,
            par = par,
            lower = c(0.25, -1, 0.1, 1),
            upper = c(4, 0.5, 0.6, 2),
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
            # source (file = "scripts/hypercube_sampling_while_4pars.R")
            source (file = "parameter_recovery/code/hypercube_sampling_while_4pars.R")
            
            # generating plausible starting values
            lhs_initial_pop <- generating_initial_pop(
                nstudies = nstudies,
                action_mode = unique(data$action_mode),
                lower_bounds = lower_bounds, upper_bounds = upper_bounds
                )
            
        } else {
            
            lhs_initial_pop = as.matrix(data.frame(
                lhs(n = nstudies, rect = c(lower_bounds[1], upper_bounds[1]) )[, 1],
                lhs(n = nstudies, rect = c(lower_bounds[2], upper_bounds[2]) )[, 1],
                lhs(n = nstudies, rect = c(lower_bounds[3], upper_bounds[3]) )[, 1],
                lhs(n = nstudies, rect = c(lower_bounds[4], upper_bounds[4]) )[, 1]
                ) )
            
        }
        
        # starting the optimisation
        fit <- DEoptim::DEoptim(
            fn = loss_function,
            data = data,
            nsims = nsims,
            # error_function = error_function,
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
                # steptol = 500,
                # using all available cores
                parallelType = "parallel",
                packages = c("DEoptim", "dplyr", "tidyr", "tgp"),
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
            lower = c(0.25, -1, 0.1, 1),
            upper = c(4, 0.5, 0.6, 2),
            control = list(maxit = maxit, trace = 6)
            )
        
    } else if (method == "all_methods") {
        
        fit <- optimx::optimx(
            par = par,
            fn = loss_function,
            data = data,
            nsims = nsims,
            error_function = error_function,
            lower = c(0.25, -1, 0.1, 1),
            upper = c(4, 0.5, 0.6, 2),
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
            lower = c(0.25, -1, 0.1, 1),
            upper = c(4, 0.5, 0.6, 2),
            verbose = TRUE
            )
        
        # stopping the cluster
        stopCluster(cl)
        
    }
    
    return (fit)
    
}

# number of parameter sets to generate
nstudies <- 50

# action mode ("executed" or "imagined")
# if action_mode == "imagined", we should change bounds on amplitude_ratio
action_mode <- "executed"

# bounds on parameters
lower_bounds <- c(0, 1.5, 0.5, 0.1)
upper_bounds <- c(1.5, 3, 1.5, 0.6)

# function for generating plausible starting values
# source (file = "scripts/hypercube_sampling_while_4pars.R")
source (file = "parameter_recovery/code/hypercube_sampling_while_4pars.R")

# generating plausible starting values
lhs_pars <- generating_initial_pop(
    nstudies = nstudies,
    action_mode = action_mode,
    lower_bounds = lower_bounds, upper_bounds = upper_bounds
    )

# names of free parameters
par_names <- c("exec_threshold", "amplitude_activ", "peak_time_activ", "curvature_activ")

# setting columns names
# colnames(lhs_pars) <- par_names

# varying the number of observed (and simulated) trials (as in White et al., 2019)
nobs <- c(50, 100, 200, 500)

# initialise results dataframe
par_recov_results <- data.frame(lhs_pars) %>%
    slice(1:nstudies) %>%
    # select(amplitude_ratio:curvature_inhib) %>%
    pivot_longer(cols = everything(), names_to = "parameters", values_to = "true_pars") %>%
    mutate(parameters = factor(x = parameters, levels = par_names) ) %>%
    mutate(study = rep(1:nstudies, each = length(par_names) ) ) %>%
    crossing(nobs) %>%
    # adding a simulation id
    group_by(study, nobs) %>%
    mutate(study_id = cur_group_id() ) %>%
    ungroup() %>%
    # initialising empty vectors for parameter values
    mutate(estimated_pars = 0, final_error = 0) %>%
    arrange(study_id)

# recording and printing when simulations started
start <- Sys.time()
print(paste0("Simulation started at ", start) )

# running the simulations
for (i in 1:max(par_recov_results$study_id) ) {
    
    # retrieving true parameter values
    true_pars <- par_recov_results$true_pars[par_recov_results$study_id == i]
    
    # printing progress
    cat(
        "\nStudy", i, "/", max(par_recov_results$study_id),
        "started.\nTrue parameters:", true_pars, "\n"
        )
    
    # simulating some data
    temp_df <- model(
        nsims = unique(par_recov_results$nobs[par_recov_results$study_id == i]),
        nsamples = 3000,
        exec_threshold = true_pars[1],
        imag_threshold = 0.5,
        amplitude_activ = true_pars[2],
        peak_time_activ = log(true_pars[3]),
        curvature_activ = true_pars[4]
        ) %>%
        mutate(action_mode = action_mode) %>%
        # keeping only the relevant columns
        dplyr::select(
            sim,
            reaction_time = paste0("onset_", substr(unique(.$action_mode), 1, 4) ),
            movement_time = paste0("mt_", substr(unique(.$action_mode), 1, 4) ),
            action_mode
            ) %>%
        distinct() %>%
        dplyr::select(-sim)
    
    # plotting RT and MT distributions
    # hist(temp_df$reaction_time)
    # hist(temp_df$movement_time)
    
    # fitting the model
    temp_fitting_results <- model_fitting(
        data = temp_df,
        nsims = 500,
        method = "DEoptim",
        lower_bounds = lower_bounds,
        upper_bounds = upper_bounds,
        nstudies = 200,
        initial_pop_while = TRUE,
        maxit = 3000
        )
    
    # storing final parameter estimates
    par_recov_results$estimated_pars[par_recov_results$study_id == i] <-
        as.numeric(temp_fitting_results$optim$bestmem)
    
    # storing final error value
    par_recov_results$final_error[par_recov_results$study_id == i] <-
        temp_fitting_results$optim$bestval
    
    # printing progress
    cat(
        "\nStudy", i, "/", max(par_recov_results$study_id),
        "done.\nTrue parameters:", true_pars,
        "\nEstimated parameters round:",
        as.numeric(temp_fitting_results$optim$bestmem), "\n"
        )
    
}

# printing when simulation ended
end <- Sys.time()
print(paste0("Simulation finished at ", end) )

# printing total duration of simulations
cat("Duration: ")
print(end - start)

# printing final result
print(data.frame(par_recov_results) )

# saving simulation results
save(
    par_recov_results,
    file = "3pars_50to500obs_500sims_DEoptim_2000iter_g2_lhs_while.Rdata"
    )

# stopping the cluster
stopCluster(cl)

##############################################
# Parameter recovery study                   #
# PIM with 4 free pars                       #
# ------------------------------------------ #
# Written by Ladislas Nalborczyk             #
# E-mail: ladislas.nalborczyk@gmail.com      #
# Last updated on June 12, 2023              #
##############################################

library(DEoptim) # global optimisation by differential evolution
library(dplyr) # data wrangling
library(tidyr) # data wrangling
library(tgp) # latin hypercube sampling

# get the number of available cores
ncores <- 32
cat(ncores, "cores will be used.")
cl <- makeCluster(ncores)
parallel::clusterEvalQ(cl = cl, expr = {library(tgp); library(DEoptim); library(dplyr); library(tidyr)})

# model function
model <- function (
        nsims = 100, nsamples = 3000,
        exec_threshold = 1, imag_threshold = 0.5,
        amplitude_activ = 1.5, peak_time_activ = 0, curvature_activ = 0.4,
        amplitude_inhib = 1.5, peak_time_inhib = 0, curvature_inhib = 0.6
        ) {
    
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
    # the mode of the function is given by:
    # exp((mu_f * sigma_g^2 - mu_g * sigma_f^2) / (sigma_g^2 - sigma_f^2) )
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
                balance_function(
                    amplitude_activ = amplitude_activ,
                    peak_time_activ = peak_time_activ,
                    curvature_activ = curvature_activ,
                    amplitude_inhib = amplitude_inhib,
                    peak_time_inhib = peak_time_inhib,
                    curvature_inhib = curvature_inhib,
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
    
    # defines the imagery threshold relative to the execution threshold
    imag_threshold <- imag_threshold * exec_threshold
    
    # setting an arbitrary value for the amplitude of the activation function
    amplitude_activ <- 1.5
    
    # retrieving parameter values for the activation function
    peak_time_activ <- log(par[[2]])
    curvature_activ <- par[[3]]
    
    # setting a value for the ratio amplitude_activ / amplitude_inhib
    amplitude_inhib <- amplitude_activ / par[[1]]
    
    # retrieving parameter values for the inhibition function
    peak_time_inhib <- log(par[[2]])
    curvature_inhib <- par[[4]] * par[[3]]
    
    ################################################################################
    # adding some constraints
    # ----------------------------------------------------------------------------
    # predicted RTs/MTs should be valid (not a NaN)
    # curvature_activ should be lower than curvature_inhib
    # imagery threshold cannot be higher than execution threshold
    # balance max should not be above exec_threshold in imagined trials
    # balance max should not be above 4 * exec_threshold in executed trials
    # balance value at the end of the trial should be below half imag_threshold
    ############################################################################
    
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
        
        # computing the predicted RT and MT in imagery
        # onset_imag <- (2 * (curvature_inhib_sim^2 * peak_time_activ_sim - curvature_activ_sim^2 * peak_time_inhib_sim) - sqrt((2 * (curvature_inhib_sim^2 * peak_time_activ_sim - curvature_activ_sim^2 * peak_time_inhib_sim) )^2 - 4 * (curvature_inhib_sim^2 - curvature_activ_sim^2) * (curvature_inhib_sim^2 * peak_time_activ_sim^2 - curvature_activ_sim^2 * peak_time_inhib_sim^2 - 2 * (log(amplitude_activ_sim / amplitude_inhib_sim) - log(imag_threshold) ) * curvature_activ_sim^2 * curvature_inhib_sim^2) ) ) / (2 * (curvature_inhib_sim^2 - curvature_activ_sim^2))
        # offset_imag <- (2 * (curvature_inhib_sim^2 * peak_time_activ_sim - curvature_activ_sim^2 * peak_time_inhib_sim) + sqrt((2 * (curvature_inhib_sim^2 * peak_time_activ_sim - curvature_activ_sim^2 * peak_time_inhib_sim) )^2 - 4 * (curvature_inhib_sim^2 - curvature_activ_sim^2) * (curvature_inhib_sim^2 * peak_time_activ_sim^2 - curvature_activ_sim^2 * peak_time_inhib_sim^2 - 2 * (log(amplitude_activ_sim / amplitude_inhib_sim) - log(imag_threshold) ) * curvature_activ_sim^2 * curvature_inhib_sim^2) ) ) / (2 * (curvature_inhib_sim^2 - curvature_activ_sim^2))
        onset_offset_imag <- onset_offset(
            alpha_f = amplitude_activ_sim, alpha_g = amplitude_inhib_sim,
            mu_f = peak_time_activ_sim, mu_g = peak_time_inhib_sim,
            sigma_f = curvature_activ_sim, sigma_g = curvature_inhib_sim,
            thresh = imag_threshold
            )
        
        onset_imag <- min(onset_offset_imag)
        mt_imag <- max(onset_offset_imag) - min(onset_offset_imag)
        
        # computing the predicted RT and MT in execution
        # onset_exec <- (2 * (curvature_inhib_sim^2 * peak_time_activ_sim - curvature_activ_sim^2 * peak_time_inhib_sim) - sqrt((2 * (curvature_inhib_sim^2 * peak_time_activ_sim - curvature_activ_sim^2 * peak_time_inhib_sim) )^2 - 4 * (curvature_inhib_sim^2 - curvature_activ_sim^2) * (curvature_inhib_sim^2 * peak_time_activ_sim^2 - curvature_activ_sim^2 * peak_time_inhib_sim^2 - 2 * (log(amplitude_activ_sim / amplitude_inhib_sim) - log(exec_threshold) ) * curvature_activ_sim^2 * curvature_inhib_sim^2) ) ) / (2 * (curvature_inhib_sim^2 - curvature_activ_sim^2))
        # offset_exec <- (2 * (curvature_inhib_sim^2 * peak_time_activ_sim - curvature_activ_sim^2 * peak_time_inhib_sim) + sqrt((2 * (curvature_inhib_sim^2 * peak_time_activ_sim - curvature_activ_sim^2 * peak_time_inhib_sim) )^2 - 4 * (curvature_inhib_sim^2 - curvature_activ_sim^2) * (curvature_inhib_sim^2 * peak_time_activ_sim^2 - curvature_activ_sim^2 * peak_time_inhib_sim^2 - 2 * (log(amplitude_activ_sim / amplitude_inhib_sim) - log(exec_threshold) ) * curvature_activ_sim^2 * curvature_inhib_sim^2) ) ) / (2 * (curvature_inhib_sim^2 - curvature_activ_sim^2))
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
        amplitude_activ = amplitude_activ,
        peak_time_activ = peak_time_activ,
        curvature_activ = curvature_activ,
        amplitude_inhib = amplitude_inhib,
        peak_time_inhib = peak_time_inhib,
        curvature_inhib = curvature_inhib,
        exec_threshold = exec_threshold,
        imag_threshold = imag_threshold
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
        exp(-(log(3) - peak_time_activ)^2 / (2 * curvature_activ^2) +
                (log(3) - peak_time_inhib)^2 / (2 * curvature_inhib^2) )
    
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
                    amplitude_activ = amplitude_activ,
                    peak_time_activ = peak_time_activ,
                    curvature_activ = curvature_activ,
                    amplitude_inhib = amplitude_inhib,
                    peak_time_inhib = peak_time_inhib,
                    curvature_inhib = curvature_inhib,
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
        par_names,
        lower_bounds, upper_bounds,
        nstudies = 200,
        initial_pop_while = FALSE,
        maxit = 1e2
        ) {
        
    if (initial_pop_while == TRUE) {
        
        # function for generating plausible starting values
        source (file = "scripts/hypercube_sampling_while_pim.R")
        
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
        
        # converting to a matrix (as requested by deoptim)
        lhs_initial_pop <- as.matrix(lhs_initial_pop_df)
        
    }
    
    # starting the optimisation
    fit <- DEoptim::DEoptim(
        fn = loss_function,
        data = data,
        nsims = nsims,
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
    
    return (fit)
    
}

# number of parameter sets to generate
nstudies <- 30

# action mode ("executed" or "imagined")
# if action_mode == "imagined", we should change bounds on amplitude_ratio
action_mode <- "executed"

# bounds on parameters
lower_bounds <- c(1, 0.5, 0.1, 1)
upper_bounds <- c(2, 1.5, 0.6, 2)

# function for generating plausible starting values
source (file = "scripts/hypercube_sampling_while_pim.R")

# names of free parameters
par_names <- c("amplitude_ratio", "peak_time", "curvature_activ", "curvature_inhib")

# generating plausible starting values
lhs_pars <- generating_initial_pop(
    nstudies = nstudies,
    action_mode = action_mode,
    par_names = par_names,
    lower_bounds = lower_bounds,
    upper_bounds = upper_bounds
    )

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
        exec_threshold = 1,
        imag_threshold = 0.5,
        amplitude_activ = 1.5,
        peak_time_activ = log(true_pars[2]),
        curvature_activ = true_pars[3],
        amplitude_inhib = 1.5 / true_pars[1],
        peak_time_inhib = log(true_pars[2]),
        curvature_inhib = true_pars[4] * true_pars[3]
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
        lower_bounds = lower_bounds,
        upper_bounds = upper_bounds,
        par_names = par_names,
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
    file = "4pars_pim_50to500obs_500sims_DEoptim_3000iter_g2_lhs_while.Rdata"
    )

# stopping the cluster
stopCluster(cl)

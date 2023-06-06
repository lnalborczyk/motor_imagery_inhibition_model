##############################################
# Latin hypercube sampling                   #
# to determine initial parameter values      #
# with realistic constraints                 #
# ------------------------------------------ #
# Written by Ladislas Nalborczyk             #
# E-mail: ladislas.nalborczyk@gmail.com      #
# Last updated on May 30, 2023               #
##############################################

generating_initial_pop <- function (
        nstudies, action_mode,
        par_names = c("exec_threshold", "peak_time_activ", "curvature_activ"),
        lower_bounds, upper_bounds
        ) {
    
    # initialising the result_nrow variable
    result_nrow <- 0
    
    # while we do not have the requested number of starting values
    while (result_nrow < nstudies) {
        
        # generating nstudies parameter values
        lhs_pars <- data.frame(
            tgp::lhs(n = nstudies, rect = c(lower_bounds[1], upper_bounds[1]) )[, 1],
            tgp::lhs(n = nstudies, rect = c(lower_bounds[2], upper_bounds[2]) )[, 1],
            tgp::lhs(n = nstudies, rect = c(lower_bounds[3], upper_bounds[3]) )[, 1]
            # tgp::lhs(n = nstudies, rect = c(lower_bounds[4], upper_bounds[4]) )[, 1],
            # tgp::lhs(n = nstudies, rect = c(lower_bounds[5], upper_bounds[5]) )[, 1]
            )
        
        # setting columns names
        colnames(lhs_pars) <- par_names
        
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
        predicted_rt_mt <- lhs_pars %>%
            rowwise() %>%
            do(
                suppressWarnings(
                    activation_function(
                        exec_threshold = .$exec_threshold,
                        amplitude = 1.5, # .$amplitude_activ,
                        peak_time = log(.$peak_time_activ),
                        curvature = .$curvature_activ
                        )
                    )
                )
        
        if (action_mode == "imagined") {
            
            predicted_rt_mt <- predicted_rt_mt[, 1:2]
            
        } else if (action_mode == "executed") {
            
            predicted_rt_mt <- predicted_rt_mt[, 3:4]
            
        }
        
        # adding some constraints
        # to restrict parameter values to realistic RT/MT data (e.g., Evans, 2020)
        # predicted RT/MT should be valid (not a NaN)
        # predicted RT should be be between 0.2 and 1 seconds
        # predicted MT should be be between 0.2 and 2 seconds
        # balance at the end of the trial should come back to zero
        final_par_values <- bind_cols(lhs_pars, predicted_rt_mt) %>%
            rowwise() %>%
            mutate(
                balance_end_of_trial = 1.5 *
                    exp(-(log(3) - peak_time_activ)^2 / (2 * curvature_activ^2) )
                ) %>%
            # data.frame()
            mutate(
                included = case_when(
                    any(is.na(pick(everything() ) ) ) ~ FALSE,
                    pick(4) < 0.1 ~ FALSE,
                    pick(4) > 2 ~ FALSE,
                    pick(5) < 0.1 ~ FALSE,
                    pick(5) > 2 ~ FALSE,
                    balance_end_of_trial > 0.25 ~ FALSE,
                    .default = TRUE
                    )
                )
        
        # reshaping the final matrix
        temp_lhs_initial_pop <- final_par_values %>%
            filter(included) %>%
            dplyr::select(1:length(par_names) ) %>%
            data.frame() %>%
            as.matrix()
        
        # rbinding results
        if (!exists("lhs_initial_pop") ) {
            
            lhs_initial_pop <- temp_lhs_initial_pop
            
            } else {
                
                lhs_initial_pop <- rbind(lhs_initial_pop, temp_lhs_initial_pop)
                
            }
        
        # updating the result_nrow variable
        result_nrow <- nrow(lhs_initial_pop)
        
        # printing progress
        print(result_nrow)
    
    }
    
    # once we have the requested number of starting values, returning the final matrix
    return (lhs_initial_pop)
    
}

# plotting good and bad parameter combinations
# library(plotly)
# plot_ly(
#     x = final_par_values$amplitude_activ,
#     y = final_par_values$peak_time_activ,
#     z = final_par_values$curvature_activ,
#     type = "scatter3d",
#     mode = "markers",
#     # color = final_par_values$curvature_activ,
#     colors = "RdBu",
#     symbol = lhs_pars$included,
#     symbols = c("cross", "circle")
#     )

# number of parameter sets included (or not)
# table(final_par_values$included)

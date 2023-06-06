##############################################
# Latin hypercube sampling                   #
# to determine initial parameter values      #
# with realistic constraints                 #
# ------------------------------------------ #
# Written by Ladislas Nalborczyk             #
# E-mail: ladislas.nalborczyk@gmail.com      #
# Last updated on May 18, 2023               #
##############################################

library(plotly)
library(tgp)

# simulation parameters
nstudies <- 500
action_mode <- "executed"
parameters <- c("amplitude_ratio", "peak_time_activ", "peak_time_inhib", "curvature_inhib")

# generating nobs parameter values
lhs_pars <- data.frame(
    lhs(n = nstudies, rect = c(1, 2) )[, 1],
    lhs(n = nstudies, rect = c(0.5, 1.5) )[, 1],
    lhs(n = nstudies, rect = c(0.5, 2) )[, 1],
    lhs(n = nstudies, rect = c(1, 3) )[, 1]
    )

# setting columns names
colnames(lhs_pars) <- parameters

# relative curvature_inhib
# lhs_pars <- lhs_pars %>% mutate(curvature_inhib = curvature_inhib * curvature_activ)

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
predicted_rt_mt <- lhs_pars %>%
    rowwise() %>%
    do(
        suppressWarnings(
            balance_function(
                amplitude_activ = 1.5,
                peak_time_activ = log(.$peak_time_activ),
                curvature_activ = 1 / sqrt(6),
                amplitude_inhib = 1.5 / .$amplitude_ratio,
                peak_time_inhib = .$peak_time_inhib * log(.$peak_time_activ),
                curvature_inhib = .$curvature_inhib * (1 / sqrt(6) )
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
# balance_end_of_trial <- (amplitude_activ / amplitude_inhib) *
#     exp(-(log(3) - peak_time_activ)^2 / (2 * curvature_activ^2) +
#             (log(3) - peak_time_inhib)^2 / (2 * curvature_inhib^2) )

final_par_values <- bind_cols(lhs_pars, predicted_rt_mt) %>%
    rowwise() %>%
    mutate(
        balance_end_of_trial = (amplitude_ratio) *
               exp(-(log(3) - log(peak_time_activ) )^2 / (2 * (1 / sqrt(6) )^2) +
                       (log(3) - peak_time_inhib * log(peak_time_activ) )^2 / (2 * (curvature_inhib * (1 / sqrt(6) ) )^2) )
        ) %>%
    mutate(
        included = case_when(
            any(is.na(pick(everything() ) ) ) ~ FALSE,
            # onset_imag < 0.1 | onset_exec < 0.1 ~ FALSE,
            # mt_imag < 0.1 | mt_exec < 0.1 ~ FALSE,
            # onset_imag > 1 | onset_exec > 1 ~ FALSE,
            # mt_imag > 2 | mt_exec > 2 ~ FALSE,
            pick(5) < 0.2 ~ FALSE,
            pick(5) > 1 ~ FALSE,
            pick(6) < 0.2 ~ FALSE,
            pick(6) > 2 ~ FALSE,
            balance_end_of_trial > 0.2 ~ FALSE,
            .default = TRUE
            )
        ) %>%
    data.frame()

# plotting good and bad parameter combinations
# plot_ly(
#     x = final_par_values$amplitude_ratio,
#     y = final_par_values$peak_time_activ,
#     z = final_par_values$peak_time_inhib,
#     type = "scatter3d",
#     mode = "markers",
#     color = final_par_values$curvature_inhib,
#     colors = "RdBu",
#     symbol = final_par_values$included,
#     symbols = c("cross", "circle")
#     )

# number of parameter sets included (or not)
table(final_par_values$included)

# correlation between pars conditioned on inclusion?
# final_par_values %>%
#     filter(included) %>%
#     select(1:4) %>%
#     ggpairs() +
#     theme_bw(base_size = 12, base_family = "Open Sans")

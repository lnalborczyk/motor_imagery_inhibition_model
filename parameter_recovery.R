##############################################
# Parameter recovery study                   #
# ------------------------------------------ #
# Written by Ladislas Nalborczyk             #
# E-mail: ladislas.nalborczyk@gmail.com      #
# Last updated on March 28, 2023             #
##############################################

# importing the data-generating model
source(file = "model.R")

# importing the model fitting routines
source(file = "fitting.R")

# true parameter values in EE sequences
true_pars <- c(1.5, 0.5, 0.5)

# simulating data
df <- model(
    nsims = 100, nsamples = 2000,
    exec_threshold = 1, imag_threshold = 0.5,
    amplitude_activ = 1.5,
    peak_time_activ = true_pars[2],
    curvature_activ = 0.4,
    amplitude_inhib = 1.5 / true_pars[1],
    peak_time_inhib = true_pars[3],
    curvature_inhib = 0.6
    ) %>%
    # was the action executed or imagined?
    mutate(
        action_mode = ifelse(
            test = true_pars[1] >= 1,
            yes = "executed", no = "imagined"
            )
        ) %>%
    # keeping only the relevant columns
    dplyr::select(
        sim,
        reaction_time = paste0("onset_", substr(unique(.$action_mode), 1, 4) ),
        movement_time = paste0("mt_", substr(unique(.$action_mode), 1, 4) ),
        action_mode
        ) %>%
    distinct() %>%
    dplyr::select(-sim)

# plotting the distributions of RTs and MTs
df %>%
    pivot_longer(cols = reaction_time:movement_time) %>%
    ggplot(aes(x = value, colour = name, fill = name) ) +
    geom_density(color = "white", alpha = 0.6, show.legend = FALSE) +
    theme_bw(base_size = 12, base_family = "Open Sans") +
    scale_fill_manual(values = met.brewer(name = "Johnson", n = 2) ) +
    scale_colour_manual(values = met.brewer(name = "Johnson", n = 2) ) +
    labs(x = "Reaction/Movement time (in seconds)", y = "Density")

# fitting the model using all methods available in optimx::optimx() (does not work)
# fitting_results <- model_fitting(
#     data = df, par = c(1, 1, 1, 1, 1),
#     method = "all_methods", maxit = 100
#     )

# fitting the model using simulated annealing (works better but slow)
# fitting_results <- model_fitting(
#     data = df, par = c(1, 1, 1, 1, 1),
#     method = "SANN", maxit = 200
#     )

# fitting the model using generalised simulated annealing (works much better but slow)
# fitting_results <- model_fitting(
#     data = df, par = c(1, 1, 1, 1, 1),
#     method = "GenSA", maxit = 200
#     )

# fitting the model using particle swarm optimisation
# works well but quite slow (error around 0.01 for 1e3 iterations)
# fitting_results <- model_fitting(
#     par = c(1, 1, 1, 1, 1), data = df,
#     method = "pso", maxit = 1e3
#     )

# fitting the model using a parallelised particle swarm optimisation
# works well and slightly faster (error around 0.01 for 1e3 iterations)
# fitting_results <- model_fitting(
#     par = c(1, 1, 1, 1, 1), data = df,
#     method = "hydroPSO", maxit = 1e3
#     )

# fitting the model using differential evolution
# seems to work best in short periods of time
# (error around 0.1 for 200 iterations and 0.01 for 1e3 iterations)
fitting_results <- model_fitting(
    par = c(1, 1, 1), data = df,
    method = "DEoptim", maxit = 500
    )

# maybe try polishing the estimated parameters with a second simplex run?
# fitting_results2 <- model_fitting(
#     par = as.numeric(fitting_results$optim$bestmem),
#     data = df,
#     method = "all_methods", maxit = 20
#     )

# plotting the optimisation results (for DEoptim only)
# plot(x = fitting_results, plot.type = "bestmemit", type = "b", col = "steelblue")
# plot(x = fitting_results, plot.type = "bestvalit", type = "b", col = "steelblue")

# getting a summary of the optimisation results
# fitting_results$value
# fitting_results$par
summary(fitting_results)

# retrieving estimated parameter values
# estimated_pars <- as.numeric(fitting_results$par)
estimated_pars <- as.numeric(fitting_results$optim$bestmem)

# plotting true parameter versus estimated parameter values
data.frame(
    parameter = c("amplitude_ratio", "peak_time_activ", "peak_time_inhib"),
    true_pars = true_pars,
    estimated_pars = estimated_pars
    ) %>%
    mutate(
        parameter = factor(
            x = parameter,
            levels = c("amplitude_ratio", "peak_time_activ", "peak_time_inhib")
            )
        ) %>%
    pivot_longer(cols = true_pars:estimated_pars) %>%
    ggplot(aes(x = parameter, y = value, colour = name) ) +
    geom_hline(yintercept = 1, lty = 2) +
    geom_line(aes(group = name), size = 0.5, show.legend = FALSE) +
    geom_point(aes(shape = name), size = 3, show.legend = FALSE) +
    geom_label(
        data = . %>% filter(parameter == "peak_time_inhib"),
        aes(label = name),
        nudge_x = 0.3,
        show.legend = FALSE
        ) +
    ylim(c(0, 2) ) +
    theme_bw(base_size = 12, base_family = "Open Sans") +
    scale_fill_manual(values =  met.brewer(name = "Johnson", n = 2) ) +
    scale_colour_manual(values = met.brewer(name = "Johnson", n = 2) ) +
    labs(x = "Parameter", y = "Parameter value")

# plotting data simulated using the estimated parameters
model(
    nsims = 1e3, nsamples = 2000,
    exec_threshold = 1, imag_threshold = 0.5,
    amplitude_activ = 1.5,
    peak_time_activ = estimated_pars[2],
    curvature_activ = 0.4,
    amplitude_inhib = 1.5 / estimated_pars[1],
    peak_time_inhib = estimated_pars[3],
    curvature_inhib = 0.6
    ) %>%
    # was the action executed or imagined?
    mutate(
        action_mode = ifelse(
            test = true_pars[1] >= 1,
            yes = "executed", no = "imagined"
            )
        ) %>%
    # keeping only the relevant columns
    dplyr::select(
        sim,
        reaction_time = paste0("onset_", substr(unique(.$action_mode), 1, 4) ),
        movement_time = paste0("mt_", substr(unique(.$action_mode), 1, 4) ),
        action_mode
        ) %>%
    distinct() %>%
    dplyr::select(-sim) %>%
    pivot_longer(cols = reaction_time:movement_time) %>%
    ggplot(aes(x = value, colour = name, fill = name) ) +
    geom_density(
        data = df %>% pivot_longer(cols = reaction_time:movement_time),
        color = "white",
        position = "identity",
        alpha = 0.5, show.legend = FALSE
        ) +
    geom_density(linewidth = 1, fill = NA, show.legend = FALSE) +
    theme_bw(base_size = 12, base_family = "Open Sans") +
    scale_fill_manual(values =  met.brewer(name = "Johnson", n = 2) ) +
    scale_colour_manual(values = met.brewer(name = "Johnson", n = 2) ) +
    labs(
        title = "Observed and simulated distributions of RTs/MTs",
        subtitle = "Distributions of RTs and MTs in executed-executed sequences",
        x = "Reaction/Movement time (in seconds)", y = "Density"
        )

# saving the plot
ggsave(
    filename = "figures/predictive_checks.png",
    width = 12, height = 8, dpi = 300,
    device = "png"
    )

############################################
# full parameter recovery study
#######################################

# importing the data-generating model
source(file = "model.R")

# importing the model fitting routines
source(file = "fitting.R")

# number of simulated "studies" to run
nstudies <- 20

# number of free parameters
npars <- 3

# should also vary N (as in White et al., 2019)
# nobs <- c(50, 100, 200, 500)

# initialise results dataframe
par_recov_results <- data.frame(
    study = rep(1:nstudies, each = npars),
    parameter = rep(
        c("amplitude_ratio", "peak_time_activ", "peak_time_inhib"),
        times = nstudies
        ),
    true_pars = rep(0, times = nstudies),
    estimated_pars = rep(0, times = nstudies),
    final_error = rep(0, times = npars * nstudies)
    )

# running the simulations
for (i in 1:nstudies) {
    
    # true parameter values
    true_pars <- c(
        rnorm(n = 1, mean = 1.5, sd = 0.1),
        rnorm(n = 1, mean = 0.5, sd = 0.1),
        rnorm(n = 1, mean = 0.5, sd = 0.1)
        )
    
    # simulating some data
    temp_df <- model(
        nsims = 100, nsamples = 2000,
        exec_threshold = 1, imag_threshold = 0.5,
        amplitude_activ = 1.5,
        peak_time_activ = true_pars[2],
        curvature_activ = 0.4,
        amplitude_inhib = 1.5 / true_pars[1],
        peak_time_inhib = true_pars[3],
        curvature_inhib = 0.6
        ) %>%
        # was the action executed or imagined?
        mutate(
            action_mode = ifelse(
                test = true_pars[1] >= 1,
                yes = "executed", no = "imagined"
                )
            ) %>%
        # keeping only the relevant columns
        dplyr::select(
            sim,
            reaction_time = paste0("onset_", substr(unique(.$action_mode), 1, 4) ),
            movement_time = paste0("mt_", substr(unique(.$action_mode), 1, 4) ),
            action_mode
            ) %>%
        distinct() %>%
        dplyr::select(-sim)
    
    # fitting the model
    temp_fitting_results <- model_fitting(
        par = c(1, 1, 1), data = temp_df,
        method = "DEoptim", maxit = 200
        )
    
    # storing the true parameter values
    par_recov_results$true_pars[par_recov_results$study == i] <- true_pars
    
    # storing the best parameter estimates
    par_recov_results$estimated_pars[par_recov_results$study == i] <-
        as.numeric(temp_fitting_results$optim$bestmem)
    
    # storing the final error value
    par_recov_results$final_error[par_recov_results$study == i] <-
        temp_fitting_results$optim$bestval
    
    # printing progress
    cat("Study", i, "done.")
    
}

# plotting final error values
hist(par_recov_results$final_error)
unique(par_recov_results$final_error)

# plotting the correlation between true and estimated parameter values
# plot(par_recov_results$true_pars, par_recov_results$estimated_pars)
# cor.test(par_recov_results$true_pars, par_recov_results$estimated_pars)
# the quality of the recovery was considered poor if correlation coefficients
# between original and recovered parameters were below .5, fair if .5<r<.75,
# good if.75<r<.9, and excellent if r>.9
par_recov_results %>%
    # group_by(study) %>%
    # mutate(parameter = c("amplitude_ratio", "peak_time_activ", "peak_time_inhib") ) %>%
    # ungroup() %>%
    ggplot(aes(x = true_pars, y = estimated_pars) ) +
    geom_abline(intercept = 0, slope = 1) +
    geom_point(size = 3, pch = 21, color = "white", fill = "black") +
    geom_smooth(method = "lm") +
    ggpubr::stat_cor(
        aes(label = ..r.label..),
        geom = "label", fill = "#F6F6FF"
        ) +
    theme_bw(base_size = 12, base_family = "Open Sans") +
    facet_wrap(~parameter, scales = "free", ncol = 3) +
    labs(x = "True parameter value", y = "Estimated parameter value")

# saving the plot
ggsave(
    filename = "private/figures/parameter_recovery_3pars_100trials_200optim.png",
    width = 12, height = 8, dpi = 300,
    device = "png"
    )

# computing the goodness-of-recovery statistic (cf. White et al., 2019, 10.3758/s13423-017-1271-2)
par_recov_results %>%
    group_by(study) %>%
    mutate(parameter = 1:5) %>%
    ungroup() %>%
    group_by(parameter) %>%
    summarise(eta = sum(abs(estimated_pars - true_pars) / (max(true_pars) - min(true_pars) ) ) ) %>%
    ungroup() %>%
    data.frame()

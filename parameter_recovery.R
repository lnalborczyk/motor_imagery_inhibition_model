##############################################
# Parameter recovery study                   #
# ------------------------------------------ #
# Written by Ladislas Nalborczyk             #
# E-mail: ladislas.nalborczyk@gmail.com      #
# Last updated on June 20, 2023              #
##############################################

# concordance correlation coefficient (CCC)
library(DescTools)

# correlation matrix
library(GGally)

# importing the data-generating model
source(file = "model.R")

# importing the model fitting routines
source(file = "fitting.R")

# true parameter values in executed trials
# true_pars <- c(1.1, 0.5, 0.3, 1.25)
# true_pars <- c(1.1, 0.8, 0.3)

# importing script to generate plausible parameter values
# source (file = "utils/hypercube_sampling_while_tmm.R")
source (file = "utils/hypercube_sampling_while_pim.R")

# generating plausible parameter values
lhs_initial_pars <- generating_initial_pop(
    nstudies = 1,
    action_mode = "executed",
    # TMM
    # par_names = c("amplitude_activ", "peak_time_activ", "curvature_activ", "exec_threshold"),
    # lower_bounds = c(0, 0.5, 0, 0),
    # upper_bounds = c(2, 1.5, 1, 1)
    # PIM
    par_names = c("amplitude_ratio", "peak_time", "curvature_activ", "curvature_inhib"),
    lower_bounds = c(1, 0.5, 0.1, 1),
    upper_bounds = c(2, 1.5, 0.6, 2)
    )

true_pars <- as.numeric(lhs_initial_pars)

# plotting the corresponding balance function with execution and imagery thresholds
# z <- function (
#         time = 0,
#         activation_amplitude = 1.5, activation_peak_time = log(true_pars[2]), activation_curvature = true_pars[4], 
#         inhibition_amplitude = 1.5 / true_pars[1], inhibition_peak_time = log(true_pars[3] * true_pars[2]), inhibition_curvature = true_pars[5] * true_pars[4]
#         ) {
#     
#     balance_output <- (activation_amplitude / inhibition_amplitude) *
#         exp(-(log(time) - activation_peak_time)^2 / (2 * activation_curvature^2) + (log(time) - inhibition_peak_time)^2 / (2 * inhibition_curvature^2) )
#     
#     return (balance_output)
#     
# }
# 
# curve(
#     expr = z,
#     from = 0, 3,
#     main = "Balance function",
#     xlab = "Time (in seconds)",
#     ylab = "Balance value (arbitrary units)"
#     )
# 
# abline(h = 0.5 * true_pars[1], lty = 2)
# abline(h = true_pars[1], lty = 2)

# simulating data (TMM)
df <- model(
    nsims = 200, nsamples = 3000,
    exec_threshold = true_pars[4] * true_pars[1],
    imag_threshold = 0.5 * true_pars[4] * true_pars[1],
    amplitude_activ = true_pars[1],
    peak_time_activ = log(true_pars[2]),
    curvature_activ = true_pars[3],
    model_version = "tmm",
    full_output = FALSE
    ) %>%
    # was the action executed or imagined?
    mutate(
        action_mode = ifelse(
            test = true_pars[4] < 1,
            yes = "executed", no = "imagined"
            )
        ) %>%
    # mutate(action_mode = "executed") %>%
    # keeping only the relevant columns
    dplyr::select(
        sim,
        reaction_time = paste0("onset_", substr(unique(.$action_mode), 1, 4) ),
        movement_time = paste0("mt_", substr(unique(.$action_mode), 1, 4) ),
        action_mode
        ) %>%
    distinct() %>%
    dplyr::select(-sim)

# simulating data (PIM)
df <- model(
    nsims = 200, nsamples = 3000,
    exec_threshold = 1,
    imag_threshold = 0.5,
    amplitude_activ = 1.5,
    peak_time_activ = log(true_pars[2]),
    curvature_activ = true_pars[3],
    amplitude_inhib = 1.5 / true_pars[1],
    peak_time_inhib = log(true_pars[2]),
    curvature_inhib = true_pars[4] * true_pars[3],
    model_version = "pim",
    full_output = FALSE
    ) %>%
    # was the action executed or imagined?
    mutate(
        action_mode = ifelse(
            test = true_pars[1] >= 1,
            yes = "executed", no = "imagined"
            )
        ) %>%
    # mutate(action_mode = "executed") %>%
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
    geom_density(
        color = "white", alpha = 0.8,
        show.legend = TRUE
        ) +
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

# fitting the TMM using differential evolution
# seems to work best in short periods of time
# g2 and sse seem to work great (but not rmse)
# (error around 0.05 in 100 iterations and around 0.01 in 1000 iterations)
# with CR = F = 0.9, now around 0.001 in 1000 iterations...
fitting_results <- model_fitting(
    data = df,
    nsims = 200,
    error_function = "g2",
    method = "DEoptim",
    model_version = "tmm",
    par_names = c("amplitude_activ", "peak_time_activ", "curvature_activ", "exec_threshold"),
    lower_bounds = c(0, 0.5, 0, 0),
    upper_bounds = c(2, 1.5, 1, 1),
    nstudies = 200,
    initial_pop_while = TRUE,
    maxit = 100
    )

# optimisation summary
summary(fitting_results)

# fitting the PIM using differential evolution
fitting_results <- model_fitting(
    data = df,
    nsims = 200,
    error_function = "g2",
    method = "DEoptim",
    model_version = "pim",
    par_names = c("amplitude_ratio", "peak_time", "curvature_activ", "curvature_inhib"),
    lower_bounds = c(1, 0.5, 0.1, 1),
    upper_bounds = c(2, 1.5, 0.6, 2),
    nstudies = 200,
    initial_pop_while = TRUE,
    maxit = 100
    )

# polishing the estimated parameters with a second run
# fitting_results2 <- model_fitting(
#     data = df,
#     nsims = 500,
#     error_function = "g2",
#     method = "DEoptim",
#     lower_bounds = apply(X = rbind(0.75 * as.numeric(fitting_results$optim$bestmem), c(1, 0.5, 0.5, 0.1, 1) ), MARGIN = 2, FUN = max),
#     upper_bounds = apply(X = rbind(1.25 * as.numeric(fitting_results$optim$bestmem), c(2, 1.5, 2, 0.5, 2) ), MARGIN = 2, FUN = min),
#     nstudies = 200,
#     initial_pop_while = FALSE,
#     maxit = 1000
#     )

# optimisation summary
# summary(fitting_results2)

# plotting the optimisation results (for DEoptim only)
# plot(x = fitting_results, plot.type = "bestmemit", type = "b", col = "steelblue")
# plot(x = fitting_results, plot.type = "bestvalit", type = "b", col = "steelblue")

# plotting optimisation paths in parameter space
optimisation_results <- data.frame(fitting_results$member$bestmemit) %>%
    mutate(iteration = 1:n() )

# plotting in 2D
optimisation_results %>%
    distinct() %>%
    ggplot(aes(x = par1, y = par4, color = iteration) ) +
    geom_point(show.legend = FALSE) +
    geom_path(show.legend = FALSE) +
    theme_bw(base_size = 10, base_family = "Open Sans")

# plotting in 3D
plot_ly(
    data = distinct(optimisation_results),
    x = ~par1, y = ~par2, z = ~par4
    ) %>%
    add_trace(type = "scatter3d", mode = "markers+lines", color = ~iteration)

# getting a summary of the optimisation results
summary(fitting_results)

# retrieving estimated parameter values
estimated_pars <- as.numeric(fitting_results$optim$bestmem)

# plotting true parameter versus estimated parameter values
data.frame(
    parameter = c(
        "amplitude_ratio", "peak_time",
        "curvature_activ", "curvature_inhib"
        ),
    true_pars = true_pars,
    estimated_pars = estimated_pars
    ) %>%
    mutate(
        parameter = factor(
            x = parameter,
            levels = c(
                "amplitude_ratio", "peak_time",
                "curvature_activ", "curvature_inhib"
                )
            )
        ) %>%
    pivot_longer(cols = true_pars:estimated_pars) %>%
    ggplot(aes(x = parameter, y = value, colour = name) ) +
    geom_hline(yintercept = 1, lty = 2) +
    geom_line(aes(group = name), linewidth = 0.5, show.legend = FALSE) +
    geom_point(aes(shape = name), size = 3, show.legend = FALSE) +
    geom_label(
        data = . %>% filter(parameter == "peak_time"),
        aes(label = name),
        nudge_x = 0.5,
        show.legend = FALSE
        ) +
    # ylim(c(0, 2) ) +
    theme_bw(base_size = 12, base_family = "Open Sans") +
    scale_fill_manual(values =  met.brewer(name = "Johnson", n = 2) ) +
    scale_colour_manual(values = met.brewer(name = "Johnson", n = 2) ) +
    labs(x = "Parameter", y = "Parameter value")

# plotting data simulated using the estimated parameters
model(
    nsims = 500, nsamples = 3000,
    exec_threshold = 1,
    imag_threshold = 0.5,
    amplitude_activ = 1.5,
    peak_time_activ = log(true_pars[2]),
    curvature_activ = true_pars[3],
    amplitude_inhib = 1.5 / true_pars[1],
    peak_time_inhib = log(true_pars[2]),
    curvature_inhib = true_pars[4] * true_pars[3],
    model_version = "pim"
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
    # removing NAs or aberrant simulated data
    na.omit() %>%
    filter(reaction_time < 2 & movement_time < 2) %>%
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
        x = "Reaction/Movement time (in seconds)", y = "Probability density"
        )

# plotting the implied activation/inhibition/balance functions
par_names <- c(
    "amplitude_ratio", "peak_time",
    "curvature_activ", "rel_curvature_inhib"
    )

parameters_estimates_summary <- paste(as.vector(rbind(
    paste0(par_names, ": "),
    paste0(as.character(round(estimated_pars, 3) ), "\n")
    ) ), collapse = "") %>% str_sub(end = -2)

model(
    nsims = 500, nsamples = 2000,
    exec_threshold = 1, imag_threshold = 0.5,
    amplitude_activ = 1.5,
    peak_time_activ = log(true_pars[2]),
    curvature_activ = true_pars[3],
    amplitude_inhib = 1.5 / true_pars[1],
    peak_time_inhib = log(true_pars[2]),
    curvature_inhib = true_pars[4] * true_pars[3],
    model_version = "pim",
    full_output = TRUE
    ) %>%
    pivot_longer(cols = activation:balance) %>%
    ggplot(
        aes(
            x = time, y = value,
            group = interaction(sim, name),
            colour = name
            )
        ) +
    geom_hline(yintercept = 1, linetype = 2) +
    geom_hline(yintercept = 0.5, linetype = 2) +
    # plotting average
    stat_summary(
        aes(group = name, colour = name),
        fun = "median", geom = "line",
        linewidth = 1, alpha = 1,
        show.legend = TRUE
        ) +
    # displaying estimated parameter values
    annotate(
        geom = "label",
        x = Inf, y = Inf,
        hjust = 1, vjust = 1,
        label = parameters_estimates_summary,
        family = "Courier"
        ) +
    theme_bw(base_size = 12, base_family = "Open Sans") +
    scale_fill_manual(values =  met.brewer(name = "Hiroshige", n = 3) ) +
    scale_colour_manual(values = met.brewer(name = "Hiroshige", n = 3) ) +
    labs(
        title = "Latent activation, inhibition, and balance functions",
        x = "Time within a trial (in seconds)",
        y = "Activation/inhibition (a.u.)",
        colour = "",
        fill = ""
        )

############################################
# full parameter recovery study
#######################################

# importing the data-generating model
source(file = "model.R")

# importing the model fitting routines
source(file = "fitting.R")

# number of parameter sets to generate
nstudies <- 50

# action mode ("executed" or "imagined")
# if action_mode == "imagined", should change bounds on amplitude_ratio
action_mode <- "executed"

# free parameters
parameters <- c(
    "amplitude_ratio", "peak_time",
    "curvature_activ", "curvature_inhib"
    )

# importing the function generating parameter values with constraints
source (file = "hypercube_sampling.R")

# number of included (or not) parameter sets
table(final_par_values$included)

# varying the number of observed trials (as in White et al., 2019)
# nobs <- c(50, 100, 200, 500)
nobs <- 200

# initialise results dataframe
# par_recov_results <- crossing(
#     study = rep(1:nstudies, each = length(parameters) ),
#     nobs, parameters
#     ) %>%
#     # adding a simulation id
#     group_by(study, nobs) %>%
#     mutate(study_id = cur_group_id() ) %>%
#     ungroup() %>%
#     # initialising empty vectors for parameter values
#     mutate(
#         true_pars = 0,
#         starting_values = 0,
#         estimated_pars = 0,
#         final_error = 0
#         )

par_recov_results <- final_par_values %>%
    filter(included == TRUE) %>%
    select(amplitude_ratio:curvature_inhib) %>%
    pivot_longer(cols = everything(), names_to = "parameters", values_to = "true_pars") %>%
    mutate(nobs = nobs) %>%
    mutate(study = rep(1:sum(final_par_values$included), each = 4) ) %>%
    # adding a simulation id
    group_by(study, nobs) %>%
    mutate(study_id = cur_group_id() ) %>%
    ungroup() %>%
    # initialising empty vectors for parameter values
    mutate(
        # true_pars = 0,
        starting_values = 0,
        estimated_pars = 0,
        final_error = 0
        )

# recording and printing when simulations started
start <- Sys.time()
print(paste0("Simulation started at ", start) )

# running the simulations
for (i in 1:max(par_recov_results$study_id) ) {
    
    # printing progress
    cat("Study", i, "started.\n\n")
    
    # generating true parameter values
    # true_pars <- c(
    #     lhs(n = 1, rect = c(0, 1) )[1],
    #     lhs(n = 1, rect = c(-1, 1) )[1],
    #     lhs(n = 1, rect = c(-1, 1) )[1],
    #     lhs(n = 1, rect = c(0, 1) )[1]
    #     )
    # true_pars <- c(
    #     runif(n = 1, min = 0.5, max = 1.5),
    #     runif(n = 1, min = -0.5, max = 0.5),
    #     runif(n = 1, min = 0.1, max = 0.4),
    #     runif(n = 1, min = -0.5, max = 0.5),
    #     runif(n = 1, min = 0.4, max = 0.8)
    #     )
    
    # retrieving true parameter values
    true_pars <- par_recov_results$true_pars[par_recov_results$study_id == i]
    
    # simulating some data
    temp_df <- model(
        nsims = unique(par_recov_results$nobs[par_recov_results$study_id == i]),
        nsamples = 3000,
        exec_threshold = 1,
        imag_threshold = 0.5,
        amplitude_activ = 1.5,
        peak_time_activ = true_pars[2],
        curvature_activ = true_pars[3],
        amplitude_inhib = 1.5 / true_pars[1],
        peak_time_inhib = true_pars[2],
        curvature_inhib = true_pars[4] * true_pars[3]
        ) %>%
        # was the action executed or imagined?
        # mutate(
        #     action_mode = ifelse(
        #         test = true_pars[1] >= 1,
        #         yes = "executed", no = "imagined"
        #         )
        #     ) %>%
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
    
    # generating starting values
    starting_values <- c(
        runif(n = 1, min = 1, max = 2),
        runif(n = 1, min = -0.6, max = 0.4),
        runif(n = 1, min = 0.1, max = 0.5),
        runif(n = 1, min = 1.1, max = 2)
        )
    
    # printing progress
    cat(
        "\nStudy", i, "started.\nTrue parameters:", true_pars,
        "\nStarting values:", starting_values, "\n\n"
        )
    
    # fitting the model
    temp_fitting_results <- model_fitting(
        par = starting_values,
        data = temp_df,
        nsims = 500,
        error_function = "g2",
        method = "DEoptim",
        maxit = 1000
        )
    
    # storing true parameter values
    # par_recov_results$true_pars[par_recov_results$study_id == i] <- true_pars
    
    # storing starting parameter values
    par_recov_results$starting_values[par_recov_results$study_id == i] <- starting_values
    
    # storing final parameter estimates
    par_recov_results$estimated_pars[par_recov_results$study_id == i] <-
        as.numeric(temp_fitting_results$optim$bestmem)
    
    # storing final error value
    par_recov_results$final_error[par_recov_results$study_id == i] <-
        temp_fitting_results$optim$bestval
    
    # printing progress
    cat(
        "\nStudy", i, "done.\nTrue parameters:", true_pars,
        "\nEstimated parameters:",
        as.numeric(temp_fitting_results$optim$bestmem),
        "\nStarting values:", starting_values, "\n\n"
        )
    
}

# printing when simulation ended
end <- Sys.time()
print(paste0("Simulation finished at ", end) )

# printing total duration of simulations
cat("Duration: ")
print(end - start)

# saving simulation results
save(
    par_recov_results,
    file = "parameter_recovery/4pars_200obs_500_sims_DEoptim_1000iter_g2_lhs.Rdata"
    )

# loading it
# load(file = "parameter_recovery/5pars_100_obs_1e3_DEoptim_g2.Rdata")
load(file = "parameter_recovery/results/4pars_pim_50to500obs_500sims_DEoptim_3000iter_g2_lhs_while.Rdata")

# plotting final error values
# par_recov_results2 <- par_recov_results
# par_recov_results <- par_recov_results %>% filter(final_error != 0)
hist(par_recov_results$final_error)
unique(par_recov_results$final_error)

# equally-sized ggplot2 axes in facet_wrap()
source(file = "utils/facet_wrap_equal.R")

# plotting the correlation between true and estimated parameter values
# the quality of the recovery was considered poor if correlation coefficients
# between original and recovered parameters were below .5, fair if .5<r<.75,
# good if.75<r<.9, and excellent if r>.9
par_recov_results %>%
    # filter(final_error != 0) %>%
    ggplot(aes(x = true_pars, y = estimated_pars) ) +
    geom_abline(intercept = 0, slope = 1, lty = 2) +
    geom_point(
        # aes(fill = as.factor(study) ),
        size = 3, pch = 21,
        alpha = 0.8,
        color = "white",
        fill = "steelblue",
        show.legend = TRUE
        ) +
    # geom_smooth(method = "lm", color = "black") +
    ggpubr::stat_cor(
        aes(label = ..r.label..),
        geom = "label", fill = "#F6F6FF"
        ) +
    # coord_fixed(xlim = c(0, 2), ylim = c(0, 2) ) +
    # coord_fixed() +
    theme_bw(base_size = 12, base_family = "Montserrat") +
    # facet_wrap(~parameters,  ncol = 3) +
    # facet_wrap_equal(nobs~parameters, scales = "free", ncol = 3) +
    facet_grid_equal(nobs ~ parameters) +
    # facet_grid(nobs ~ parameters) +
    labs(x = "True parameter value", y = "Estimated parameter value")

# saving the plot
ggsave(
    filename = "parameter_recovery/figures/4pars_pim_50to500obs_500sims_DEoptim_3000iter_g2_lhs_while.png",
    width = 12, height = 8, dpi = 300,
    device = "png"
    )

# computing the goodness-of-recovery statistic (cf. White et al., 2019, 10.3758/s13423-017-1271-2)
# the normalised RMSE and the R-squared (cf. Ghaderi-Kangavari et al., 2023)
par_recov_results %>%
    # filter(final_error != 0) %>%
    group_by(parameters, nobs) %>%
    summarise(
        eta = sum(abs(estimated_pars - true_pars) / (max(true_pars) - min(true_pars) ) ),
        ccc = CCC(x = estimated_pars, y = true_pars)$rho.c$est %>% round(., 3),
        nrmse = sqrt(mean(estimated_pars - true_pars)^2) / (max(true_pars) - min(true_pars) ),
        pearson_cor = cor(x = estimated_pars, y = true_pars, method = "pearson"),
        r2 = cor(x = estimated_pars, y = true_pars, method = "pearson")^2
        ) %>%
    ungroup() %>%
    mutate(across(eta:r2, ~ round(., 3) ) ) %>%
    data.frame()

# correlation matrix between estimated parameters
par_recov_results %>%
    # filter(final_error != 0) %>%
    # filter(nobs == 500) %>%
    select(study, nobs, parameters, estimated_pars) %>%
    pivot_wider(names_from = parameters, values_from = estimated_pars) %>%
    # head()
    ggpairs(columns = 3:6) +
    theme_bw(base_size = 12, base_family = "Open Sans")

# saving the plot
ggsave(
    filename = "parameter_recovery/figures/4pars_pim_50to500obs_500sims_DEoptim_3000iter_g2_lhs_while_correlation_matrix.png",
    width = 12, height = 8, dpi = 300,
    device = "png"
    )

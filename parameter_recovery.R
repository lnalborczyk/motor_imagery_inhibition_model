##############################################
# Parameter recovery study                   #
# ------------------------------------------ #
# Written by Ladislas Nalborczyk             #
# E-mail: ladislas.nalborczyk@gmail.com      #
# Last updated on March 26, 2023             #
##############################################

# importing the data-generating model
source(file = "model.R")

# importing the model fitting routines
source(file = "fitting.R")

# true parameter values in EE sequences
true_pars <- c(0.5, 0.4, 0.75, 1, 1.5)

# simulating data
df <- model(
    nsims = 200, nsamples = 2000,
    exec_threshold = 1, imag_threshold = 0.5,
    amplitude_activ = 1.5,
    peak_time_activ = true_pars[1],
    curvature_activ = true_pars[2],
    amplitude_inhib = true_pars[3] * 1.5,
    peak_time_inhib = true_pars[4] * true_pars[1],
    curvature_inhib = true_pars[5] * true_pars[2]
    ) %>%
    # was the action executed or imagined?
    mutate(
        action_mode = ifelse(
            test = true_pars[3] >= 1,
            yes = "imagined", no = "executed"
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
    scale_fill_manual(values =  met.brewer(name = "Johnson", n = 2) ) +
    scale_colour_manual(values = met.brewer(name = "Johnson", n = 2) ) +
    labs(x = "Reaction/Movement time (in seconds)", y = "Density")

# fitting the model using all methods available in optimx::optimx() (does not work)
# fitting_results <- model_fitting(data = df, method = "all_methods", maxit = 200)

# fitting the model using simulated annealing (works better but slow)
# fitting_results <- model_fitting(data = df, method = "SANN", maxit = 200)

# fitting the model using generalised simulated annealing (works much better but slow)
# fitting_results <- model_fitting(data = df, method = "GenSA", maxit = 200)

# fitting the model using particle swarm optimisation
# works well but quite slow (error around 0.01 for 1e3 iterations)
fitting_results <- model_fitting(
    par = c(1, 1, 1, 1, 1), data = df,
    method = "pso", maxit = 1e3
    )

# fitting the model using a parallelised particle swarm optimisation
# works well and slightly faster (error around 0.02 for 1e3 iterations) 
# fitting_results <- model_fitting(
#     par = c(1, 1, 1, 1, 1), data = df,
#     method = "hydroPSO", maxit = 1e3
#     )

# fitting the model using differential evolution
# seems to work best in short periods of time (error around 0.01 for 1e3 iterations)
# fitting_results <- model_fitting(
#     par = c(1, 1, 1, 1, 1), data = df,
#     method = "DEoptim", maxit = 1e3
#     )

# plotting the optimisation results (for DEoptim only)
# plot(x = fitting_results, plot.type = "bestmemit", type = "b", col = "steelblue")
# plot(x = fitting_results, plot.type = "bestvalit", type = "b", col = "steelblue")

# getting a summary of the optimisation results
# summary(fitting_results)
fitting_results$value
fitting_results$par

# retrieving estimated parameter values
# estimated_pars <- fitting_results$optim$bestmem
estimated_pars <- fitting_results$par

# plotting true parameter versus estimated parameter values
data.frame(
    parameter = c(
        # "amplitude_activ",
        "peak_time_activ", "curvature_activ",
        "amplitude_inhib", "peak_time_inhib", "curvature_inhib"
        ),
    true_pars = true_pars,
    estimated_pars = estimated_pars
    ) %>%
    mutate(
        parameter = factor(
            x = parameter,
            levels = c(
                # "amplitude_activ",
                "peak_time_activ", "curvature_activ",
                "amplitude_inhib", "peak_time_inhib", "curvature_inhib"
                )
            )
        ) %>%
    pivot_longer(cols = true_pars:estimated_pars) %>%
    ggplot(aes(x = parameter, y = value, colour = name) ) +
    geom_point(size = 2, alpha = 0.8) +
    theme_bw(base_size = 12, base_family = "Open Sans") +
    scale_fill_manual(values =  met.brewer(name = "Johnson", n = 2) ) +
    scale_colour_manual(values = met.brewer(name = "Johnson", n = 2) ) +
    labs(x = "Parameter", y = "Parameter value")

# plotting data simulated using the estimated parameters
model(
    nsims = 1e3, nsamples = 2000,
    exec_threshold = 1, imag_threshold = 0.5,
    amplitude_activ = 1.5,
    peak_time_activ = estimated_pars[1],
    curvature_activ = estimated_pars[2],
    amplitude_inhib = estimated_pars[3] * 1.5,
    peak_time_inhib = estimated_pars[4] * estimated_pars[1],
    curvature_inhib = estimated_pars[5] * estimated_pars[2]
    ) %>%
    # was the action executed or imagined?
    mutate(
        action_mode = ifelse(test = true_pars[3] >= 1, yes = "imagined", no = "executed")
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
    geom_density(size = 1, fill = NA, show.legend = FALSE) +
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

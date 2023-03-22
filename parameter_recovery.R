##############################################
# Parameter recovery study                   #
# ------------------------------------------ #
# Written by Ladislas Nalborczyk             #
# E-mail: ladislas.nalborczyk@gmail.com      #
# Last updated on March 22, 2023             #
##############################################

# importing the data-generating model
source(file = "model.R")

# importing the model fitting routines
source(file = "fitting.R")

# true parameter values in EI sequences
true_pars <- c(1.5, 0.5, 1.25, 0.75)

# simulating data
df <- model(
    nsims = 100, nsamples = 2000,
    exec_threshold = 1, imag_threshold = 0.5, iti = 2,
    amplitude_activ = true_pars[1],
    peak_time_activ = true_pars[2],
    curvature_activ = 0.4,
    amplitude_inhib = true_pars[3] * true_pars[1],
    peak_time_inhib = true_pars[4] * true_pars[2],
    curvature_inhib = 0.6,
    amplitude_inhib_prev = 0.5,
    peak_time_inhib_prev = 0.5,
    curvature_inhib_prev = 0.6
    ) %>%
    # was the action executed or imagined?
    mutate(action_mode = ifelse(test = true_pars[3] >= 1, yes = "imagined", no = "executed") ) %>%
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
    geom_density(alpha = 0.5, show.legend = FALSE) +
    theme_bw(base_size = 12, base_family = "Open Sans") +
    scale_fill_manual(values =  met.brewer(name = "Johnson", n = 2) ) +
    scale_colour_manual(values = met.brewer(name = "Johnson", n = 2) ) +
    labs(x = "Reaction/Movement time (in seconds)", y = "Density")

# fitting the model using all methods available in optimx::optimx() (does not work)
# fitting_results <- model_fitting(data = df, method = "all_methods", maxit = 1e2)

# fitting the model using simulated annealing (works better but slow)
# fitting_results <- model_fitting(data = df, method = "SANN", maxit = 1e2)

# fitting the model using generalised simulated annealing (works much better but slow)
# fitting_results <- model_fitting(data = df, method = "GenSA", maxit = 1e2)

# fitting the model using particle swarm optimisation
# works very well with 1e3 iterations but quite slow...
# fitting_results <- model_fitting(data = df, method = "pso", maxit = 1e3)

# fitting the model using differential evolution
# seems to work best in short periods of time...
fitting_results <- model_fitting(data = df, method = "DEoptim", maxit = 1e3)

# plotting the optimisation results
# plot(x = fitting_results, plot.type = "bestmemit", type = "b", col = "steelblue")
# plot(x = fitting_results, plot.type = "bestvalit", type = "b", col = "steelblue")

# getting a summary of the optimisation results
summary(fitting_results)
fitting_results$value
fitting_results$par

##############################################
# Parameter recovery study                   #
# ------------------------------------------ #
# Written by Ladislas Nalborczyk             #
# E-mail: ladislas.nalborczyk@gmail.com      #
# Last updated on March 9, 2023              #
##############################################

# importing the data-generating model
source(file = "model.R")

# importing the model fitting routines
source(file = "fitting.R")

# simulating some data in EI sequences
df <- model(
    nsims = 100, nsamples = 2000,
    exec_threshold = 1, imag_threshold = 0.5, iti = 2,
    amplitude_activ = 1.5, peak_time_activ = 0.5, curvature_activ = 0.4,
    amplitude_inhib = 1.5, peak_time_inhib = 0.5, curvature_inhib = 0.6,
    amplitude_inhib_prev = 0.5, peak_time_inhib_prev = 0.5, curvature_inhib_prev = 0.6
    ) %>%
    # keeping only imagery RTs and MTs (for testing purposes)
    dplyr::select(sim, reaction_time = onset_imag, movement_time = mt_imag) %>%
    distinct() %>%
    dplyr::select(-sim) %>%
    # specifying the current action mode (i.e., executed or imagined)
    mutate(action_mode = "imagined")

# plotting the distributions of RTs and MTs
hist(x = df$reaction_time, breaks = "FD")
hist(x = df$movement_time, breaks = "FD")

# fitting the model using all methods available in optimx::optimx()
# fitting_results <- model_fitting(data = df, method = "all_methods", maxit = 1e2)
# fitting_results

# fitting the model using simulated annealing (works better but slow)
# fitting_results <- model_fitting(data = df, method = "SANN", maxit = 1e2)
# fitting_results

# fitting the model using generalised simulated annealing (works much better but slow)
# fitting_results <- model_fitting(data = df, method = "GenSA", maxit = 1e2)
# fitting_results$value
# fitting_results$par

# fitting the model using differential evolution (seems to work best with 3+ pars)
fitting_results <- model_fitting(data = df, method = "DEoptim", maxit = 1e2)
fitting_results$optim

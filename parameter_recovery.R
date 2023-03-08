#########################################
# Parameter recovery study              #
# ------------------------------------- #
# Written by Ladislas Nalborczyk        #
# E-mail: ladislas.nalborczyk@gmail.com #
# Last updated on March 8, 2023         #
#########################################

# importing the data-generating model
source(file = "model.R")

# importing the model fitting routines
source(file = "fitting.R")

# simulating some data in II sequences
df <- model(
    nsims = 100, nsamples = 2000,
    exec_threshold = 1, imag_threshold = 0.5, iti = 2,
    amplitude_activ = 2, peak_time_activ = 0.5, curvature_activ = 0.4,
    amplitude_inhib = 2, peak_time_inhib = 0.5, curvature_inhib = 0.6,
    amplitude_inhib_prev = 2, peak_time_inhib_prev = 0.5, curvature_inhib_prev = 0.6
    ) %>%
    # keeping only imagery RTs and MTs (for testing purposes)
    dplyr::select(sim, reaction_time = onset_imag, movement_time = mt_imag) %>%
    distinct() %>%
    dplyr::select(-sim)

# plotting the distributions of RTs and MTs
hist(x = df$reaction_time, breaks = "FD")
hist(x = df$movement_time, breaks = "FD")

# fitting the model using the "bobyqa" optimiser
# fitting_results <- model_fitting(data = df, method = "bobyqa")

# fitting the model using all available methods in optimx::optimx()
fitting_results <- model_fitting(data = df, method = "all_methods")
fitting_results

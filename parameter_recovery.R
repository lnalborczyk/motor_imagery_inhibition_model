#########################################
# Parameter recovery study              #
# ------------------------------------- #
# Written by Ladislas Nalborczyk        #
# E-mail: ladislas.nalborczyk@gmail.com #
# Last updated on March 7, 2023         #
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
    amplitude_inhib_prev = 0.5, peak_time_inhib_prev = 0.5, curvature_inhib_prev = 0.6
    ) %>%
    # keeping only imagery RTs and MTs (for testing purposes)
    dplyr::select(sim, reaction_time = onset_imag, movement_time = mt_imag) %>%
    distinct() %>%
    dplyr::select(-sim)

# plotting the distributions of RTs and MTs
hist(x = df$reaction_time, breaks = "FD")
hist(x = df$movement_time, breaks = "FD")

# fitting the model
# maybe see also the fitdistr package...
# https://stackoverflow.com/questions/29897756/how-to-define-your-own-distribution-for-fitdistr-function-in-r-with-the-help-of
fitting_results_optim <- model_fitting(data = df, method = "optim")
fitting_results_optim # 0 is successful convergence, 1 when maxit is reached...

fitting_results_nlminb <- model_fitting(data = df, method = "nlminb")
fitting_results_nlminb # X-convergence (3)... + NAs produced in rnorm...

fitting_results_optimx <- model_fitting(data = df, method = "optimx")
fitting_results_optimx # maxit reached for Nelder-Mead...

# fitting_results <- model_fitting(data = df, method = "bobyqa")
fitting_results <- model_fitting(data = df, method = "Nelder-Mead")
fitting_results
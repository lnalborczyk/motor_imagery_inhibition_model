##############################################
# Fitting the model to actual data           #
# ------------------------------------------ #
# Written by Ladislas Nalborczyk             #
# E-mail: ladislas.nalborczyk@gmail.com      #
# Last updated on March 24, 2023             #
##############################################

library(tidyverse)
library(MetBrewer)

#############################################################################
# Experiment 1 from Bart et al. (2020)
# Group 1 is hand, group 2 is foot
# Other variables explained at https://osf.io/xtpfg
######################################################################

# importing data for IE sequences in the hand group
df_IE <- read.csv(file = "private/data/bart_2020/dataset_exp1.csv") %>%
    # keeping only mixed blocks (for comparison purposes)
    select(participant = ID, group, contains(match = "mixed") ) %>%
    # reshaping the RT/MT information
    pivot_longer(cols = 3:34) %>%
    separate(col = name, sep = "_", into = c("name", "global", "hand_rep", "target_rep", "block_type") ) %>%
    # keeping only the hand group
    filter(group == 1) %>%
    # keeping only the IE sequences
    filter(global == "IE") %>%
    # adding the action mode of current trial
    mutate(action_mode = substr(global, 2, 2) ) %>%
    # keeping only the relevant columns
    select(participant, action_mode, name, value) %>%
    # renaming the variables
    mutate(action_mode = ifelse(test = action_mode == "I", yes = "imagined", no = "executed") ) %>%
    mutate(name = ifelse(test = name == "RT", yes = "reaction_time", no = "movement_time") ) %>%
    # converting RTs/MTs from ms to seconds
    mutate(value = value / 1e3) %>%
    # pivoting the RTs/MTs
    group_by(participant, name) %>%
    mutate(id = 1:n() ) %>%
    ungroup() %>%
    pivot_wider(names_from = name, values_from = value) %>%
    # removing the participant column (for now)
    select(reaction_time, movement_time, action_mode)

# plotting the distributions of RTs and MTs
df_IE %>%
    pivot_longer(cols = reaction_time:movement_time) %>%
    ggplot(aes(x = value, colour = name, fill = name) ) +
    geom_density(alpha = 0.5, show.legend = FALSE) +
    theme_bw(base_size = 12, base_family = "Open Sans") +
    scale_fill_manual(values =  met.brewer(name = "Johnson", n = 2) ) +
    scale_colour_manual(values = met.brewer(name = "Johnson", n = 2) ) +
    labs(x = "Reaction/Movement time (in seconds)", y = "Density")

# importing data for EE sequences in the hand group
df_EE <- read.csv(file = "private/data/bart_2020/dataset_exp1.csv") %>%
    # keeping only mixed blocks (for comparison purposes)
    select(participant = ID, group, contains(match = "mixed") ) %>%
    # reshaping the RT/MT information
    pivot_longer(cols = 3:34) %>%
    separate(col = name, sep = "_", into = c("name", "global", "hand_rep", "target_rep", "block_type") ) %>%
    # keeping only the hand group
    filter(group == 1) %>%
    # keeping only the IE sequences
    filter(global == "EE") %>%
    # adding the action mode of current trial
    mutate(action_mode = substr(global, 2, 2) ) %>%
    # keeping only the relevant columns
    select(participant, action_mode, name, value) %>%
    # renaming the variables
    mutate(action_mode = ifelse(test = action_mode == "I", yes = "imagined", no = "executed") ) %>%
    mutate(name = ifelse(test = name == "RT", yes = "reaction_time", no = "movement_time") ) %>%
    # converting RTs/MTs from ms to seconds
    mutate(value = value / 1e3) %>%
    # pivoting the RTs/MTs
    group_by(participant, name) %>%
    mutate(id = 1:n() ) %>%
    ungroup() %>%
    pivot_wider(names_from = name, values_from = value) %>%
    # removing the participant column (for now)
    select(reaction_time, movement_time, action_mode)

# plotting the distributions of RTs and MTs
df_EE %>%
    pivot_longer(cols = reaction_time:movement_time) %>%
    ggplot(aes(x = value, colour = name, fill = name) ) +
    geom_density(alpha = 0.5, show.legend = FALSE) +
    theme_bw(base_size = 12, base_family = "Open Sans") +
    scale_fill_manual(values =  met.brewer(name = "Johnson", n = 2) ) +
    scale_colour_manual(values = met.brewer(name = "Johnson", n = 2) ) +
    labs(x = "Reaction/Movement time (in seconds)", y = "Density")

##############################################################################
# Fitting the model
# 6 parameters are activation_amplitude,
# activation_peak_time, activation_curvature,
# inhibition_amplitude (% of activation_amplitude),
# inhibition_peak_time (% of activation_peak_time), and
# inhibition_curvature (% of activation_curvature)
#######################################################################

# importing the data-generating model
source(file = "model.R")

# importing the model fitting routines
source(file = "fitting.R")

# fitting the model using differential evolution
# fitting_results_IE <- model_fitting(
#     par = c(1, 1, 1), data = df_IE,
#     method = "DEoptim", maxit = 200
#     )
# 
# fitting_results_EE <- model_fitting(
#     par = c(1, 1, 1), data = df_EE,
#     method = "DEoptim", maxit = 200
#     )

# getting a summary of the optimisation results
# summary(fitting_results_IE)
# summary(fitting_results_EE)

# best parameter estimates in IE sequences are 0.977 0.95393 1.04441 (bestvalit around 1.63206)
# best parameter estimates in EE sequences are 0.979 0.95512 1.05559 (bestvalit around 1.69417)

# retrieving the estimated parameters in IE sequences
# estimated_pars_IE <- fitting_results_IE$optim$bestmem %>% as.numeric()
# estimated_pars_EE <- fitting_results_EE$optim$bestmem %>% as.numeric()

# fitting the model using particle swarm optimisation
fitting_results_IE <- model_fitting(
    par = c(1, 1, 1, 1, 1, 1), data = df_IE,
    method = "pso", maxit = 1e4
    )

fitting_results_EE <- model_fitting(
    par = c(1, 1, 1, 1, 1, 1), data = df_EE,
    method = "pso", maxit = 1e4
    )

# best parameter estimates in IE sequences are 0.9742326 0.9495626 1.0414963 (bestvalit around 1.497536)
# best parameter estimates in EE sequences are 0.09136413 0.99111854 0.84127333 (bestvalit around 0.0516)

# retrieving the estimated parameters
estimated_pars_IE <- fitting_results_IE$par
estimated_pars_EE <- fitting_results_EE$par

# simulating data with the estimated parameters (kind of ppc)
sim_IE <- model(
    nsims = nrow(df_IE), nsamples = 2000,
    exec_threshold = 1, imag_threshold = 0.5,
    amplitude_activ = estimated_pars_IE[1],
    peak_time_activ = estimated_pars_IE[2],
    curvature_activ = 0.4,
    amplitude_inhib = estimated_pars_IE[3] * estimated_pars_IE[1],
    peak_time_inhib = estimated_pars_IE[4] * estimated_pars_IE[2],
    curvature_inhib = 0.6
    ) %>%
    # was the action executed or imagined?
    mutate(action_mode = ifelse(
        test = estimated_pars_IE[3] >= 1,
        yes = "imagined", no = "executed"
        ) ) %>%
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
sim_IE %>%
    pivot_longer(cols = reaction_time:movement_time) %>%
    ggplot(aes(x = value, colour = name, fill = name) ) +
    geom_density(alpha = 0.5, show.legend = FALSE) +
    theme_bw(base_size = 12, base_family = "Open Sans") +
    coord_cartesian(xlim = c(0.25, 1.25) ) +
    scale_fill_manual(values =  met.brewer(name = "Johnson", n = 2) ) +
    scale_colour_manual(values = met.brewer(name = "Johnson", n = 2) ) +
    labs(x = "Reaction/Movement time (in seconds)", y = "Density")

# simulating data with the estimated parameters (kind of ppc)
sim_EE <- model(
    nsims = nrow(df_EE), nsamples = 2000,
    exec_threshold = 1, imag_threshold = 0.5,
    amplitude_activ = estimated_pars_EE[1],
    peak_time_activ = estimated_pars_EE[2],
    curvature_activ = 0.4,
    amplitude_inhib = estimated_pars_EE[3] * estimated_pars_EE[1],
    peak_time_inhib = estimated_pars_EE[4] * estimated_pars_EE[2],
    curvature_inhib = 0.6
    ) %>%
    # was the action executed or imagined?
    mutate(action_mode = ifelse(
        test = estimated_pars_EE[3] >= 1,
        yes = "imagined", no = "executed"
        ) ) %>%
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
sim_EE %>%
    pivot_longer(cols = reaction_time:movement_time) %>%
    ggplot(aes(x = value, colour = name, fill = name) ) +
    geom_density(alpha = 0.5, show.legend = FALSE) +
    theme_bw(base_size = 12, base_family = "Open Sans") +
    coord_cartesian(xlim = c(0.25, 1.25) ) +
    scale_fill_manual(values =  met.brewer(name = "Johnson", n = 2) ) +
    scale_colour_manual(values = met.brewer(name = "Johnson", n = 2) ) +
    labs(x = "Reaction/Movement time (in seconds)", y = "Density")

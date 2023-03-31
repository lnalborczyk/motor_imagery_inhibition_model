##############################################
# Fitting the model to actual data           #
# ------------------------------------------ #
# Written by Ladislas Nalborczyk             #
# E-mail: ladislas.nalborczyk@gmail.com      #
# Last updated on March 31, 2023             #
##############################################

library(patchwork)
library(tidyverse)
library(MetBrewer)

############################################################################
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
    geom_density(color = "white", alpha = 0.6, show.legend = FALSE) +
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
    geom_density(color = "white", alpha = 0.6, show.legend = FALSE) +
    theme_bw(base_size = 12, base_family = "Open Sans") +
    scale_fill_manual(values =  met.brewer(name = "Johnson", n = 2) ) +
    scale_colour_manual(values = met.brewer(name = "Johnson", n = 2) ) +
    labs(x = "Reaction/Movement time (in seconds)", y = "Density")

# importing data for II sequences in the hand group
df_II <- read.csv(file = "private/data/bart_2020/dataset_exp1.csv") %>%
    # keeping only mixed blocks (for comparison purposes)
    select(participant = ID, group, contains(match = "mixed") ) %>%
    # reshaping the RT/MT information
    pivot_longer(cols = 3:34) %>%
    separate(col = name, sep = "_", into = c("name", "global", "hand_rep", "target_rep", "block_type") ) %>%
    # keeping only the hand group
    filter(group == 1) %>%
    # keeping only the II sequences
    filter(global == "II") %>%
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
df_II %>%
    pivot_longer(cols = reaction_time:movement_time) %>%
    ggplot(aes(x = value, colour = name, fill = name) ) +
    geom_density(color = "white", alpha = 0.6, show.legend = FALSE) +
    theme_bw(base_size = 12, base_family = "Open Sans") +
    scale_fill_manual(values =  met.brewer(name = "Johnson", n = 2) ) +
    scale_colour_manual(values = met.brewer(name = "Johnson", n = 2) ) +
    labs(x = "Reaction/Movement time (in seconds)", y = "Density")

# importing data for EI sequences in the hand group
df_EI <- read.csv(file = "private/data/bart_2020/dataset_exp1.csv") %>%
    # keeping only mixed blocks (for comparison purposes)
    select(participant = ID, group, contains(match = "mixed") ) %>%
    # reshaping the RT/MT information
    pivot_longer(cols = 3:34) %>%
    separate(col = name, sep = "_", into = c("name", "global", "hand_rep", "target_rep", "block_type") ) %>%
    # keeping only the hand group
    filter(group == 1) %>%
    # keeping only the EI sequences
    filter(global == "EI") %>%
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
df_EI %>%
    pivot_longer(cols = reaction_time:movement_time) %>%
    ggplot(aes(x = value, colour = name, fill = name) ) +
    geom_density(color = "white", alpha = 0.6, show.legend = FALSE) +
    theme_bw(base_size = 12, base_family = "Open Sans") +
    scale_fill_manual(values =  met.brewer(name = "Johnson", n = 2) ) +
    scale_colour_manual(values = met.brewer(name = "Johnson", n = 2) ) +
    labs(x = "Reaction/Movement time (in seconds)", y = "Density")

##############################################################################
# Fitting the model to each condition
# 3 free parameters are the activation/inhibition amplitude ratio,
# activation_peak_time, and inhibition_peak_time
############################################################################

# importing the data-generating model
source(file = "model.R")

# importing the model fitting routines
source(file = "fitting.R")

# fitting the model using differential evolution
fitting_results_IE <- model_fitting(
    par = c(1, 1, 1),
    data = df_IE, nsims = 500,
    error_function = "wsse",
    method = "DEoptim", maxit = 500
    )

fitting_results_EE <- model_fitting(
    par = c(1, 1, 1),
    data = df_EE, nsims = 500,
    error_function = "wsse",
    method = "DEoptim", maxit = 500
    )

fitting_results_II <- model_fitting(
    par = c(1, 1, 1),
    data = df_II, nsims = 500,
    error_function = "wsse",
    method = "DEoptim", maxit = 500
    )

fitting_results_EI <- model_fitting(
    par = c(1, 1, 1),
    data = df_EI, nsims = 500,
    error_function = "wsse",
    method = "DEoptim", maxit = 500
    )

# getting a summary of the optimisation results
summary(fitting_results_IE)
summary(fitting_results_EE)
summary(fitting_results_II)
summary(fitting_results_EI)

# best parameter estimates in IE sequences are
# 1.3414 0.71388 0.60102 (bestvalit around 0.00396, using sse)
# best parameter estimates in EE sequences are
# 1.35698 0.68533 0.55379 (bestvalit around 0.0064, using sse)

# best parameter estimates in IE sequences are
# 0.71413 1.22771 1.75257 (bestvalit around 0.11502, using g2)
# best parameter estimates in EE sequences are
# 0.71929 1.21016 1.73923 (bestvalit around 0.07155, using g2)

# retrieving estimated parameters
# estimated_pars_IE <- fitting_results_IE$optim$bestmem %>% as.numeric()
# estimated_pars_EE <- fitting_results_EE$optim$bestmem %>% as.numeric()

# fitting the model using particle swarm optimisation
# fitting_results_IE_pso <- model_fitting(
#     par = c(1, 1, 1), data = df_IE,
#     method = "pso", maxit = 2000
#     )
# 
# fitting_results_EE_pso <- model_fitting(
#     par = c(1, 1, 1), data = df_EE,
#     method = "pso", maxit = 2000
#     )

# best parameter estimates in IE sequences are
# 0.5501751 1.2858752 1.8942179 (bestvalit around 0.2502)
# best parameter estimates in EE sequences are
# 0.47419 1.32958 1.98809  (bestvalit around 0.21093)

# best parameter estimates in IE sequences are
# 0.5477616132 0.2876862846 0.9583299498 0.0005916149 1.8014553519 (bestvalit around 0.05069868)
# best parameter estimates in EE sequences are
# 1.00445849 0.07692108 0.04314579 1.55153960 1.90340393 (bestvalit around 0.1070052)

# retrieving the estimated parameters
# estimated_pars_IE <- as.numeric(fitting_results_IE$par)
# estimated_pars_EE <- as.numeric(fitting_results_EE$par)
estimated_pars_IE <- as.numeric(fitting_results_IE$optim$bestmem)
estimated_pars_EE <- as.numeric(fitting_results_EE$optim$bestmem)
estimated_pars_II <- as.numeric(fitting_results_II$optim$bestmem)
estimated_pars_EI <- as.numeric(fitting_results_EI$optim$bestmem)

################################################
# predictive checks
#########################################

# simulating implied distribution of RTs and MTs using the estimated parameters
sim_IE <- model(
    nsims = 1000, nsamples = 2000,
    exec_threshold = 1, imag_threshold = 0.5,
    amplitude_activ = 1.5,
    peak_time_activ = estimated_pars_IE[2],
    curvature_activ = 0.4,
    amplitude_inhib = 1.5 / estimated_pars_IE[1],
    peak_time_inhib = estimated_pars_IE[3],
    curvature_inhib = 0.6
    ) %>%
    # was the action executed or imagined?
    mutate(action_mode = "executed") %>%
    # mutate(action_mode = ifelse(
    #     test = estimated_pars_IE[1] >= 1,
    #     yes = "executed", no = "imagined"
    #     ) ) %>%
    # keeping only the relevant columns
    dplyr::select(
        sim,
        reaction_time = paste0("onset_", substr(unique(.$action_mode), 1, 4) ),
        movement_time = paste0("mt_", substr(unique(.$action_mode), 1, 4) ),
        action_mode
        ) %>%
    distinct() %>%
    dplyr::select(-sim)

# simulating data with the estimated parameters
sim_EE <- model(
    nsims = 1000, nsamples = 2000,
    exec_threshold = 1, imag_threshold = 0.5,
    amplitude_activ = 1.5,
    peak_time_activ = estimated_pars_EE[2],
    curvature_activ = 0.4,
    amplitude_inhib = 1.5 / estimated_pars_EE[1],
    peak_time_inhib = estimated_pars_EE[3],
    curvature_inhib = 0.6
    ) %>%
    # was the action executed or imagined?
    mutate(action_mode = "executed") %>%
    # mutate(action_mode = ifelse(
    #     test = estimated_pars_EE[3] >= 1,
    #     yes = "imagined", no = "executed"
    #     ) ) %>%
    # keeping only the relevant columns
    dplyr::select(
        sim,
        reaction_time = paste0("onset_", substr(unique(.$action_mode), 1, 4) ),
        movement_time = paste0("mt_", substr(unique(.$action_mode), 1, 4) ),
        action_mode
    ) %>%
    distinct() %>%
    dplyr::select(-sim)

# simulating data with the estimated parameters
sim_II <- model(
    nsims = 1000, nsamples = 2000,
    exec_threshold = 1, imag_threshold = 0.5,
    amplitude_activ = 1.5,
    peak_time_activ = estimated_pars_II[2],
    curvature_activ = 0.4,
    amplitude_inhib = 1.5 / estimated_pars_II[1],
    peak_time_inhib = estimated_pars_II[3],
    curvature_inhib = 0.6
    ) %>%
    # was the action executed or imagined?
    mutate(action_mode = "imagined") %>%
    # mutate(action_mode = ifelse(
    #     test = estimated_pars_EE[3] >= 1,
    #     yes = "imagined", no = "executed"
    #     ) ) %>%
    # keeping only the relevant columns
    dplyr::select(
        sim,
        reaction_time = paste0("onset_", substr(unique(.$action_mode), 1, 4) ),
        movement_time = paste0("mt_", substr(unique(.$action_mode), 1, 4) ),
        action_mode
        ) %>%
    distinct() %>%
    dplyr::select(-sim)

# simulating data with the estimated parameters
sim_EI <- model(
    nsims = 1000, nsamples = 2000,
    exec_threshold = 1, imag_threshold = 0.5,
    amplitude_activ = 1.5,
    peak_time_activ = estimated_pars_EI[2],
    curvature_activ = 0.4,
    amplitude_inhib = 1.5 / estimated_pars_EI[1],
    peak_time_inhib = estimated_pars_EI[3],
    curvature_inhib = 0.6
    ) %>%
    # was the action executed or imagined?
    mutate(action_mode = "imagined") %>%
    # mutate(action_mode = ifelse(
    #     test = estimated_pars_EE[3] >= 1,
    #     yes = "imagined", no = "executed"
    #     ) ) %>%
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
p1 <- sim_IE %>%
    pivot_longer(cols = reaction_time:movement_time) %>%
    ggplot(aes(x = value, colour = name, fill = name) ) +
    geom_density(
        data = df_IE %>% pivot_longer(cols = reaction_time:movement_time),
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
        subtitle = "Distributions of RTs and MTs in imagined-executed sequences",
        x = "Reaction/Movement time (in seconds)", y = "Density"
        )

# plotting the distributions of RTs and MTs
p2 <- sim_EE %>%
    pivot_longer(cols = reaction_time:movement_time) %>%
    ggplot(aes(x = value, colour = name, fill = name) ) +
    geom_density(
        data = df_EE %>% pivot_longer(cols = reaction_time:movement_time),
        color = "white",
        position = "identity",
        alpha = 0.5, show.legend = FALSE
        ) +
    geom_density(size = 1, fill = NA, show.legend = FALSE) +
    theme_bw(base_size = 12, base_family = "Open Sans") +
    # coord_cartesian(xlim = c(0.25, 1.25) ) +
    scale_fill_manual(values =  met.brewer(name = "Johnson", n = 2) ) +
    scale_colour_manual(values = met.brewer(name = "Johnson", n = 2) ) +
    labs(
        title = "Observed and simulated distributions of RTs/MTs",
        subtitle = "Distributions of RTs and MTs in executed-executed sequences",
        x = "Reaction/Movement time (in seconds)", y = "Density"
        )

# plotting the distributions of RTs and MTs
p2 <- sim_II %>%
    pivot_longer(cols = reaction_time:movement_time) %>%
    ggplot(aes(x = value, colour = name, fill = name) ) +
    geom_density(
        data = df_II %>% pivot_longer(cols = reaction_time:movement_time),
        color = "white",
        position = "identity",
        alpha = 0.5, show.legend = FALSE
        ) +
    geom_density(size = 1, fill = NA, show.legend = FALSE) +
    theme_bw(base_size = 12, base_family = "Open Sans") +
    # coord_cartesian(xlim = c(0.25, 1.25) ) +
    scale_fill_manual(values =  met.brewer(name = "Johnson", n = 2) ) +
    scale_colour_manual(values = met.brewer(name = "Johnson", n = 2) ) +
    labs(
        title = "Observed and simulated distributions of RTs/MTs",
        subtitle = "Distributions of RTs and MTs in imagined-imagined sequences",
        x = "Reaction/Movement time (in seconds)", y = "Density"
        )

# plotting the distributions of RTs and MTs
p4 <- sim_EI %>%
    pivot_longer(cols = reaction_time:movement_time) %>%
    ggplot(aes(x = value, colour = name, fill = name) ) +
    geom_density(
        data = df_EI %>% pivot_longer(cols = reaction_time:movement_time),
        color = "white",
        position = "identity",
        alpha = 0.5, show.legend = FALSE
        ) +
    geom_density(size = 1, fill = NA, show.legend = FALSE) +
    theme_bw(base_size = 12, base_family = "Open Sans") +
    # coord_cartesian(xlim = c(0.25, 1.25) ) +
    scale_fill_manual(values =  met.brewer(name = "Johnson", n = 2) ) +
    scale_colour_manual(values = met.brewer(name = "Johnson", n = 2) ) +
    labs(
        title = "Observed and simulated distributions of RTs/MTs",
        subtitle = "Distributions of RTs and MTs in executed-imagined sequences",
        x = "Reaction/Movement time (in seconds)", y = "Density"
        )

# combining all plots
(p1 + p2) / (p3 + p4)

# saving the plot
ggsave(
    filename = "figures/predictive_checks_bart_et_al_2020.png",
    width = 12, height = 8, dpi = 300,
    device = "png"
    )

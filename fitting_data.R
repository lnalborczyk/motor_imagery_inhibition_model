##############################################
# Fitting the model to actual data           #
# ------------------------------------------ #
# Written by Ladislas Nalborczyk             #
# E-mail: ladislas.nalborczyk@gmail.com      #
# Last updated on April 05, 2023             #
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
# 5 free parameters are the activation/inhibition amplitude ratio,
# activation_peak_time, activation_curvature,
# inhibition_peak_time, and inhibition_curvature
############################################################################

# importing the data-generating model
source(file = "model.R")

# importing the model fitting routines
source(file = "fitting.R")

# fitting the model using differential evolution
fitting_results_IE <- model_fitting(
    par = c(1, 1, 1, 1, 1),
    data = df_IE,
    nsims = 1e3,
    error_function = "g2",
    method = "DEoptim",
    maxit = 500
    )

fitting_results_EE <- model_fitting(
    par = c(1, 1, 1, 1, 1),
    data = df_EE,
    nsims = 1e3,
    error_function = "g2",
    method = "DEoptim",
    maxit = 500
    )

fitting_results_II <- model_fitting(
    par = c(1, 1, 1, 1, 1),
    data = df_II,
    nsims = 1e3,
    error_function = "g2",
    method = "DEoptim",
    maxit = 500
    )

fitting_results_EI <- model_fitting(
    par = c(1, 1, 1, 1, 1),
    data = df_EI,
    nsims = 1e3,
    error_function = "g2",
    method = "DEoptim",
    maxit = 500
    )

# getting a summary of the optimisation results
summary(fitting_results_IE)
summary(fitting_results_EE)
summary(fitting_results_II)
summary(fitting_results_EI)

# using wsse
# best parameter estimates in IE sequences are
# 1.34781 0.70156 0.57385 (bestvalit around 0.00398)
# best parameter estimates in EE sequences are
# 1.36582 0.64592 0.49884 (bestvalit around 0.00635)
# best parameter estimates in II sequences are
# 0.31171 0.40644 -0.20384 (bestvalit around 0.02315)
# best parameter estimates in EI sequences are
# 0.41025 0.50511 0.00185 (bestvalit around 0.01594)

# using g2
# best parameter estimates in IE sequences are
# 0.78142 1.2121 1.70929 (bestvalit around 0.10678)
# best parameter estimates in EE sequences are
# 0.76605 1.1994 1.71286 (bestvalit around 0.06786)
# best parameter estimates in IE sequences are
# 0.08034 0.11802 -0.82256 (bestvalit around 0.0936)
# best parameter estimates in EE sequences are
# 0.12722 0.20348 -0.63707 (bestvalit around 0.12335)

# fitting the model using particle swarm optimisation
fitting_results_IE_pso <- model_fitting(
    par = c(1, 1, 1),
    data = df_IE,
    # nsims = 500,
    error_function = "g2",
    method = "pso", maxit = 2000
    )

fitting_results_EE_pso <- model_fitting(
    par = c(1, 1, 1),
    data = df_EE,
    # nsims = 500,
    error_function = "g2",
    method = "pso", maxit = 2000
    )

fitting_results_II_pso <- model_fitting(
    par = c(1, 1, 1),
    data = df_II,
    # nsims = 500,
    error_function = "g2",
    method = "pso", maxit = 2000
    )

fitting_results_EI_pso <- model_fitting(
    par = c(1, 1, 1),
    data = df_EI,
    # nsims = 500,
    error_function = "g2",
    method = "pso", maxit = 2000
    )

# using wsse
# best parameter estimates in IE sequences are
# 1.3416345 0.6961862 0.5544498 (bestvalit around 0.002970687)
# best parameter estimates in EE sequences are
# 1.3310512 0.6231945 0.4340981 (bestvalit around 0.005490737)
# best parameter estimates in II sequences are
# 0.42559993 0.52572246 0.02623149 (bestvalit around 0.02618343)
# best parameter estimates in EI sequences are
# 0.3310483 0.4530563 -0.1149336 (bestvalit around 0.01360734)

# using g2
# best parameter estimates in IE sequences are
# 0.71413 1.22771 1.75257 (bestvalit around 0.11502)
# best parameter estimates in EE sequences are
# 0.71929 1.21016 1.73923 (bestvalit around 0.07155)
# best parameter estimates in IE sequences are
# 0.71413 1.22771 1.75257 (bestvalit around 0.11502)
# best parameter estimates in EE sequences are
# 0.71929 1.21016 1.73923 (bestvalit around 0.07155)

# # retrieving the estimated parameters
estimated_pars_IE <- as.numeric(fitting_results_IE$optim$bestmem)
estimated_pars_EE <- as.numeric(fitting_results_EE$optim$bestmem)
estimated_pars_II <- as.numeric(fitting_results_II$optim$bestmem)
estimated_pars_EI <- as.numeric(fitting_results_EI$optim$bestmem)

# retrieving the estimated parameters
estimated_pars_IE <- as.numeric(fitting_results_IE_pso$par)
estimated_pars_EE <- as.numeric(fitting_results_EE_pso$par)
estimated_pars_II <- as.numeric(fitting_results_II_pso$par)
estimated_pars_EI <- as.numeric(fitting_results_EI_pso$par)

################################################
# predictive checks
#########################################

# simulating implied distribution of RTs and MTs using the estimated parameters
sim_IE <- model(
    nsims = 1000, nsamples = 2000,
    exec_threshold = 1, imag_threshold = 0.5,
    amplitude_activ = 1.5,
    peak_time_activ = estimated_pars_IE[2],
    curvature_activ = estimated_pars_IE[3],
    amplitude_inhib = 1.5 / estimated_pars_IE[1],
    peak_time_inhib = estimated_pars_IE[4],
    curvature_inhib = estimated_pars_IE[5]
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

# plotting the implied balance function in IE trials
model(
    nsims = 1000, nsamples = 2000,
    exec_threshold = 1, imag_threshold = 0.5,
    amplitude_activ = 1.5,
    peak_time_activ = estimated_pars_IE[2],
    curvature_activ = estimated_pars_IE[3],
    amplitude_inhib = 1.5 / estimated_pars_IE[1],
    peak_time_inhib = estimated_pars_IE[4],
    curvature_inhib = estimated_pars_IE[5]
    ) %>%
    # pivot_longer(cols = activation:balance) %>%
    pivot_longer(cols = balance) %>%
    # group_by(time, name) %>%
    # summarise(value = median(value) ) %>%
    # ungroup() %>%
    ggplot(
        aes(
            x = time, y = value,
            group = interaction(sim, name),
            colour = name
            )
        ) +
    geom_hline(yintercept = 1, linetype = 2) +
    geom_hline(yintercept = 0.5, linetype = 2) +
    # plotting some individual simulations
    # geom_line(
    #     data = . %>% filter(sim %in% unique(sim)[1:100]),
    #     size = 0.5, alpha = 0.1, show.legend = FALSE
    #     ) +
    # plotting average
    stat_summary(
        aes(group = name, colour = name),
        fun = "median", geom = "line",
        linewidth = 1, alpha = 1,
        show.legend = TRUE
        ) +
    # geom_line(linewidth = 1, show.legend = TRUE) +
    theme_bw(base_size = 12, base_family = "Open Sans") +
    scale_fill_manual(values =  met.brewer(name = "Hiroshige", n = 3) ) +
    scale_colour_manual(values = met.brewer(name = "Hiroshige", n = 3) ) +
    labs(
        title = "Simulating activation/inhibition patterns",
        # subtitle = "Balance function is defined as activation_current / inhibition_current",
        x = "Time within a trial (in seconds)",
        y = "Activation/inhibition (a.u.)",
        colour = "",
        fill = ""
        )

# plotting the distributions of RTs and MTs
sim_IE %>%
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
    coord_cartesian(xlim = c(0, 2) ) +
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
    coord_cartesian(xlim = c(0, 2) ) +
    scale_fill_manual(values =  met.brewer(name = "Johnson", n = 2) ) +
    scale_colour_manual(values = met.brewer(name = "Johnson", n = 2) ) +
    labs(
        title = "Observed and simulated distributions of RTs/MTs",
        subtitle = "Distributions of RTs and MTs in executed-executed sequences",
        x = "Reaction/Movement time (in seconds)", y = "Density"
        )

# plotting the distributions of RTs and MTs
p3 <- sim_II %>%
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
    coord_cartesian(xlim = c(0, 2) ) +
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
    coord_cartesian(xlim = c(0, 2) ) +
    scale_fill_manual(values =  met.brewer(name = "Johnson", n = 2) ) +
    scale_colour_manual(values = met.brewer(name = "Johnson", n = 2) ) +
    labs(
        title = "Observed and simulated distributions of RTs/MTs",
        subtitle = "Distributions of RTs and MTs in executed-imagined sequences",
        x = "Reaction/Movement time (in seconds)", y = "Density"
        )

# combining all plots
(p1 + p3) / (p2 + p4)

# saving the plot
ggsave(
    filename = "figures/predictive_checks_bart_et_al_2020_pso_g2_2000_iter.png",
    width = 12, height = 8, dpi = 300,
    device = "png"
    )

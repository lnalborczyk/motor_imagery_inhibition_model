##############################################
# Fitting the model to actual data           #
# ------------------------------------------ #
# Written by Ladislas Nalborczyk             #
# E-mail: ladislas.nalborczyk@gmail.com      #
# Last updated on April 12, 2023             #
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
    par = c(1, 0, 0.5, 0, 0.5),
    data = df_IE,
    nsims = 1000,
    error_function = "g2",
    method = "DEoptim",
    maxit = 2000
    )

fitting_results_EE <- model_fitting(
    par = c(1, 0, 0.5, 0, 0.5),
    data = df_EE,
    nsims = 1000,
    error_function = "g2",
    method = "DEoptim",
    maxit = 2000
    )

fitting_results_II <- model_fitting(
    par = c(1, 0, 0.5, 0, 0.5),
    data = df_II,
    nsims = 1000,
    error_function = "g2",
    method = "DEoptim",
    maxit = 2000
    )

fitting_results_EI <- model_fitting(
    par = c(1, 0, 0.5, 0, 0.5),
    data = df_EI,
    nsims = 1000,
    error_function = "g2",
    method = "DEoptim",
    maxit = 2000
    )

# getting a summary of the optimisation results
summary(fitting_results_IE)
summary(fitting_results_EE)
summary(fitting_results_II)
summary(fitting_results_EI)

# using g2, 1000 simulated trials and 1000 iterations
# best parameter estimates in IE sequences are
# 1.0186 -0.10192 1.52759 -0.0785 2.03995 (bestvalit around 0.04736)
# best parameter estimates in EE sequences are
# 1.00645 0.10474 1.65506 0.36639 2.32876 (bestvalit around 0.04928)
# best parameter estimates in II sequences are
# 0.50226 -0.31411 1.72672 -0.46571 2.09779 (bestvalit around 0.03967)
# best parameter estimates in EI sequences are
# 0.50326 -0.27066 1.82591 -0.43791 2.35464 (bestvalit around 0.00886)

# retrieving the estimated parameters
estimated_pars_IE <- as.numeric(fitting_results_IE$optim$bestmem)
estimated_pars_EE <- as.numeric(fitting_results_EE$optim$bestmem)
estimated_pars_II <- as.numeric(fitting_results_II$optim$bestmem)
estimated_pars_EI <- as.numeric(fitting_results_EI$optim$bestmem)

# fitting the model using particle swarm optimisation
fitting_results_IE_pso <- model_fitting(
    # par = c(1, 1, 1, 1, 1),
    par = estimated_pars_IE,
    data = df_IE,
    nsims = 1000,
    error_function = "g2",
    method = "pso",
    maxit = 2000
    )

fitting_results_EE_pso <- model_fitting(
    # par = c(1, 1, 1, 1, 1),
    par = estimated_pars_EE,
    data = df_EE,
    nsims = 1000,
    error_function = "g2",
    method = "pso",
    maxit = 2000
    )

fitting_results_II_pso <- model_fitting(
    # par = c(1, 1, 1, 1, 1),
    par = estimated_pars_II,
    data = df_II,
    nsims = 1000,
    error_function = "g2",
    method = "pso",
    maxit = 2000
    )

fitting_results_EI_pso <- model_fitting(
    # par = c(1, 1, 1, 1, 1),
    par = estimated_pars_EI,
    data = df_EI,
    nsims = 1000,
    error_function = "g2",
    method = "pso",
    maxit = 2000
    )

# using g2, 1000 simulated trials and 1000 iterations
# best parameter estimates in IE sequences are
# 1.97701555 0.06284588 0.06263408 0.99966417 0.15406468 (bestvalit around 0.06487346)
# best parameter estimates in EE sequences are
# 1.62723715 0.04837623 0.05899386 0.99993729 0.14760247 (bestvalit around 0.05658119)
# best parameter estimates in II sequences are
# 1.96395391 0.22935939 0.04865418 0.98947350 0.09911938 (bestvalit around 0.360675)
# best parameter estimates in EI sequences are
# 0.5033786 -0.2723716 1.8453853 -0.4490106 2.3700998 (bestvalit around 0.003638892)

# retrieving the estimated parameters
estimated_pars_IE_pso <- as.numeric(fitting_results_IE_pso$par)
estimated_pars_EE_pso <- as.numeric(fitting_results_EE_pso$par)
estimated_pars_II_pso <- as.numeric(fitting_results_II_pso$par)
estimated_pars_EI_pso <- as.numeric(fitting_results_EI_pso$par)

# polishing the estimated parameters with an additional simplex run
# fitting_results_EI_polished <- model_fitting(
#     par = as.numeric(fitting_results_EI_pso$par),
#     data = df_II,
#     nsims = 1e4,
#     error_function = "g2",
#     method = "optimParallel",
#     maxit = 50
#     )

# putting everything in a table and exporting it
par_names <- c(
    "amplitude_ratio", "peak_time_activ", "curvature_activ",
    "peak_time_inhib", "curvature_inhib"
    )

data.frame(
    par_names = par_names,
    IE = estimated_pars_IE,
    EE = estimated_pars_EE,
    II = estimated_pars_II,
    EI = estimated_pars_EI
    ) %>%
    pivot_longer(cols = IE:EI, names_to = "condition") %>%
    mutate(error_value = rep(c(
        fitting_results_II_pso$value, fitting_results_II_pso$value,
        fitting_results_II_pso$value, fitting_results_EI_pso$value
        ), 5) ) %>%
    data.frame() %>%
    mutate(across(value:error_value, ~ round(.x, 6) ) ) %>%
    write.csv(file = "fitting_results/parameter_estimates_bart_et_al_2020.csv", row.names = FALSE)

################################################
# predictive checks
#########################################

# simulating implied distribution of RTs and MTs using the estimated parameters
sim_IE <- model(
    nsims = 1000, nsamples = 3000,
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
    curvature_activ = estimated_pars_EE[3],
    amplitude_inhib = 1.5 / estimated_pars_EE[1],
    peak_time_inhib = estimated_pars_EE[4],
    curvature_inhib = estimated_pars_EE[5]
    ) %>%
    # was the action executed or imagined?
    mutate(action_mode = "executed") %>%
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
    curvature_activ = estimated_pars_II[3],
    amplitude_inhib = 1.5 / estimated_pars_II[1],
    peak_time_inhib = estimated_pars_II[4],
    curvature_inhib = estimated_pars_II[5]
    ) %>%
    # was the action executed or imagined?
    mutate(action_mode = "imagined") %>%
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
    peak_time_activ = estimated_pars_EI_pso[2],
    curvature_activ = estimated_pars_EI_pso[3],
    amplitude_inhib = 1.5 / estimated_pars_EI_pso[1],
    peak_time_inhib = estimated_pars_EI_pso[4],
    curvature_inhib = estimated_pars_EI_pso[5]
    ) %>%
    # was the action executed or imagined?
    mutate(action_mode = "imagined") %>%
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
    filename = "fitting_results/predictive_checks_bart_et_al_2020.png",
    width = 12, height = 8, dpi = 300,
    device = "png"
    )

#################################################################
# plotting the implied balance functions per condition
##########################################################

parameters_estimates_summary <- paste(as.vector(rbind(
    paste0(par_names, ": "),
    paste0(as.character(round(estimated_pars_II, 3) ), "\n")
    ) ), collapse = "") %>% str_sub(end = -2)

model(
    nsims = 1e2, nsamples = 3000,
    exec_threshold = 1, imag_threshold = 0.5,
    amplitude_activ = 1.5,
    peak_time_activ = estimated_pars_EI_pso[2],
    curvature_activ = estimated_pars_EI_pso[3],
    amplitude_inhib = 1.5 / estimated_pars_EI_pso[1],
    peak_time_inhib = estimated_pars_EI_pso[4],
    curvature_inhib = estimated_pars_EI_pso[5],
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
        x = -Inf, y = Inf,
        hjust = -0.1, vjust = 1.1,
        label = parameters_estimates_summary,
        family = "Courier"
        ) +
    theme_bw(base_size = 12, base_family = "Open Sans") +
    scale_fill_manual(values =  met.brewer(name = "Hiroshige", n = 3) ) +
    scale_colour_manual(values = met.brewer(name = "Hiroshige", n = 3) ) +
    labs(
        title = "Activation/inhibition patterns in imagined-executed sequences",
        x = "Time within a trial (in seconds)",
        y = "Activation/inhibition (a.u.)",
        colour = "",
        fill = ""
        )

# combining all plots
(p5 + p6) / (p7 + p8)

# saving the plot
ggsave(
    filename = "fitting_results/balance_function_per_condition_bart_et_al_2020.png",
    width = 12, height = 8, dpi = 300,
    device = "png"
    )

##############################################
# Fitting the model to actual data           #
# ------------------------------------------ #
# Written by Ladislas Nalborczyk             #
# E-mail: ladislas.nalborczyk@gmail.com      #
# Last updated on May 23, 2023               #
##############################################

library(geomtextpath)
library(DescTools)
library(patchwork)
library(tidyverse)
library(MetBrewer)
library(plotly)

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

# correlation between RTs and MTs?
# df_rt_mt <- read.csv(file = "private/data/bart_2020/dataset_exp1.csv") %>%
#     # keeping only mixed blocks (for comparison purposes)
#     select(participant = ID, group, contains(match = "mixed") ) %>%
#     # reshaping the RT/MT information
#     pivot_longer(cols = 3:34) %>%
#     separate(col = name, sep = "_", into = c("name", "global", "hand_rep", "target_rep", "block_type") ) %>%
#     # keeping only the hand group
#     filter(group == 1) %>%
#     # keeping only the IE sequences
#     # filter(global == "IE") %>%
#     # adding the action mode of current trial
#     mutate(action_mode = substr(global, 2, 2) ) %>%
#     # keeping only the relevant columns
#     select(participant, action_mode, name, value) %>%
#     # renaming the variables
#     mutate(action_mode = ifelse(test = action_mode == "I", yes = "imagined", no = "executed") ) %>%
#     mutate(name = ifelse(test = name == "RT", yes = "reaction_time", no = "movement_time") ) %>%
#     # converting RTs/MTs from ms to seconds
#     mutate(value = value / 1e3) %>%
#     # pivoting the RTs/MTs
#     group_by(participant, name) %>%
#     mutate(id = 1:n() ) %>%
#     ungroup() %>%
#     pivot_wider(names_from = name, values_from = value)
# 
# df_rt_mt %>%
#     ggplot(aes(x = reaction_time, y = movement_time) ) +
#     geom_point() +
#     geom_smooth(method = "lm", colour = "black") +
#     facet_wrap(~participant, scales = "free") +
#     theme_bw(base_size = 10, base_family = "Open Sans") +
#     labs(
#         title = "Relation between reaction times and movement times",
#         x = "Reaction time (in seconds)",
#         y = "Movemnt time (in seconds)"
#         )

# saving the plot
# ggsave(
#     filename = "fitting_results/rt_mt.png",
#     width = 12, height = 8, dpi = 300,
#     device = "png"
#     )
# 
# df_rt_mt %>%
#     ggplot(aes(x = sort(reaction_time), y = sort(movement_time) ) ) +
#     geom_point() +
#     geom_smooth(method = "lm", colour = "black") +
#     facet_wrap(~participant, scales = "free") +
#     theme_bw(base_size = 10, base_family = "Open Sans") +
#     labs(
#         title = "Relation between sorted reaction times and sorted movement times",
#         x = "Reaction time (in seconds)",
#         y = "Movemnt time (in seconds)"
#         )
# 
# saving the plot
# ggsave(
#     filename = "fitting_results/sorted_rt_mt.png",
#     width = 12, height = 8, dpi = 300,
#     device = "png"
#     )
# 
# # plotting ACF by participants
# df_rt_mt %>%
#     group_by(participant) %>%
#     summarise(
#         autocor_rt = acf(x = reaction_time, plot = FALSE)$acf,
#         autocor_mt = acf(x = movement_time, plot = FALSE)$acf
#         ) %>%
#     mutate(trial = 1:n() ) %>%
#     ungroup() %>%
#     pivot_longer(cols = autocor_rt:autocor_mt, names_to = "name") %>%
#     # data.frame() %>% head()
#     ggplot(aes(x = trial, y = value, colour = name) ) +
#     geom_hline(yintercept = 0, linetype = 2) +
#     geom_line(size = 0.75, alpha = 1, show.legend = FALSE) +
#     facet_wrap(~participant) +
#     theme_bw(base_size = 10, base_family = "Open Sans") +
#     labs(x = "Lag (in number of trials)", y = "Auto-correlation")
# 
# saving the plot
# ggsave(
#     filename = "fitting_results/acf_plot.png",
#     width = 12, height = 8, dpi = 300,
#     device = "png"
#     )

##############################################################################
# Fitting the model to each condition
# 4 free parameters are the activation/inhibition amplitude ratio,
# the activation and inhibition peak_time,
# activation_curvature, and inhibition_curvature
############################################################################

# importing the data-generating model
source(file = "model.R")

# importing the model fitting routines
source(file = "fitting.R")

# fitting the model using differential evolution
fitting_results_IE <- model_fitting(
    data = df_IE,
    nsims = 200,
    error_function = "g2",
    method = "DEoptim",
    lower_bounds = c(1, 0.5, 0.5, 0.1, 1),
    upper_bounds = c(2, 1.5, 2, 0.5, 1.5),
    maxit = 2000
    )

# polishing the estimated parameters with a second run
fitting_results_IE2 <- model_fitting(
    data = df_IE,
    nsims = 500,
    error_function = "g2",
    method = "DEoptim",
    lower_bounds = apply(X = rbind(0.75 * as.numeric(fitting_results_IE$optim$bestmem), c(1, 0.5, 0.5, 0.1, 1) ), MARGIN = 2, FUN = max),
    upper_bounds = apply(X = rbind(1.25 * as.numeric(fitting_results_IE$optim$bestmem), c(2, 1.5, 2, 0.5, 1.5) ), MARGIN = 2, FUN = min),
    maxit = 1000
    )

fitting_results_EE <- model_fitting(
    data = df_EE,
    nsims = 200,
    error_function = "g2",
    method = "DEoptim",
    lower_bounds = c(0.25, 0.5, 0.5, 1),
    upper_bounds = c(4, 1.5, 2.0, 3),
    maxit = 1000
    )

fitting_results_II <- model_fitting(
    data = df_II,
    nsims = 200,
    error_function = "g2",
    method = "DEoptim",
    lower_bounds = c(0, 0.5, 0.5, 0.1, 1),
    upper_bounds = c(1, 1.5, 2, 0.5, 2),
    maxit = 200
    )

fitting_results_EI <- model_fitting(
    data = df_EI,
    nsims = 200,
    error_function = "g2",
    method = "DEoptim",
    lower_bounds = c(0.25, 0.5, 0.5, 1),
    upper_bounds = c(4, 1.5, 2.0, 3),
    maxit = 1000
    )

# getting a summary of the optimisation results
summary(fitting_results_IE)
summary(fitting_results_EE)
summary(fitting_results_II)
summary(fitting_results_EI)

# plotting optimisation paths in parameter space
optimisation_results <- data.frame(fitting_results_IE$member$bestmemit) %>%
    mutate(iteration = 1:n() )

# plotting in 2D
optimisation_results %>%
    # distinct() %>%
    ggplot(aes(x = par1, y = par2, color = iteration) ) +
    geom_point(show.legend = FALSE) +
    geom_path(show.legend = FALSE) +
    theme_bw(base_size = 10, base_family = "Open Sans")

# plotting in 3D
plot_ly(
    data = distinct(optimisation_results),
    x = ~par1, y = ~par2, z = ~par3
    ) %>%
    add_trace(type = "scatter3d", mode = "markers+lines", color = ~iteration)

# retrieving the estimated parameters
estimated_pars_IE <- as.numeric(fitting_results_IE$optim$bestmem)
estimated_pars_EE <- as.numeric(fitting_results_EE$optim$bestmem)
estimated_pars_II <- as.numeric(fitting_results_II$optim$bestmem)
estimated_pars_EI <- as.numeric(fitting_results_EI$optim$bestmem)

# polishing the estimated parameters with an additional simplex run
fitting_results_IE_polished <- model_fitting(
    par = as.numeric(estimated_pars_IE),
    data = df_IE,
    nsims = 1e4,
    error_function = "g2",
    method = "optimParallel",
    maxit = 50
    )

# putting everything in a table
par_names <- c(
    "amplitude_ratio", "peak_time_activ", "peak_time_inhib", "curvature_inhib"
    )

estimates_summary <- data.frame(
    par_names = par_names,
    IE = estimated_pars_IE,
    EE = estimated_pars_EE,
    II = estimated_pars_II,
    EI = estimated_pars_EI
    ) %>%
    pivot_longer(cols = IE:EI, names_to = "condition") %>%
    mutate(error_value = rep(c(
        fitting_results_IE$optim$bestval, fitting_results_EE$optim$bestval,
        fitting_results_II$optim$bestval, fitting_results_EI$optim$bestval
        ), 4) ) %>%
    data.frame() %>%
    mutate(across(value:error_value, ~ round(.x, 6) ) )

# exporting it as a csv file
write.csv(
    x = estimates_summary,
    file = "fitting_results/bart_et_al_2020/parameter_estimates_bart_et_al_2020_4pars_fixed_curvature_activ.csv",
    row.names = FALSE
    )

################################################
# predictive checks
#########################################

# simulating implied distribution of RTs and MTs using the estimated parameters
sim_IE <- model(
    nsims = 1000, nsamples = 3000,
    exec_threshold = 1,
    imag_threshold = 0.5,
    amplitude_activ = 1.5,
    # peak_time_activ = estimated_pars_IE[2],
    # curvature_activ = estimated_pars_IE[3],
    # amplitude_inhib = 1.5 / estimated_pars_IE[1],
    # peak_time_inhib = estimated_pars_IE[4],
    # curvature_inhib = estimated_pars_IE[5]
    peak_time_activ = log(estimated_pars_IE[2]),
    curvature_activ = estimated_pars_IE[4],
    amplitude_inhib = 1.5 / estimated_pars_IE[1],
    peak_time_inhib = log(estimated_pars_IE[3] * estimated_pars_IE[2]),
    curvature_inhib = estimated_pars_IE[5] * estimated_pars_IE[4]
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
    nsims = 1000, nsamples = 3000,
    exec_threshold = 1,
    imag_threshold = 0.5,
    amplitude_activ = 1.5,
    peak_time_activ = log(estimated_pars_EE[2]),
    curvature_activ = 0.2,
    amplitude_inhib = 1.5 / estimated_pars_EE[1],
    peak_time_inhib = log(estimated_pars_EE[3] * estimated_pars_EE[2]),
    curvature_inhib = estimated_pars_EE[4] * 0.2
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
    nsims = 1000, nsamples = 3000,
    exec_threshold = 1,
    imag_threshold = 0.5,
    peak_time_activ = log(estimated_pars_II[2]),
    curvature_activ = 0.2,
    amplitude_inhib = 1.5 / estimated_pars_II[1],
    peak_time_inhib = log(estimated_pars_II[3] * estimated_pars_II[2]),
    curvature_inhib = estimated_pars_II[4] * 0.2
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
    nsims = 1000, nsamples = 3000,
    exec_threshold = 1,
    imag_threshold = 0.5,
    amplitude_activ = 1.5,
    peak_time_activ = log(estimated_pars_EI[2]),
    curvature_activ = 0.2,
    amplitude_inhib = 1.5 / estimated_pars_EI[1],
    peak_time_inhib = log(estimated_pars_EI[3] * estimated_pars_EI[2]),
    curvature_inhib = estimated_pars_EI[4] * 0.2
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
sim_IE %>%
    # removing NAs or aberrant simulated data
    na.omit() %>%
    filter(reaction_time < 3 & movement_time < 3) %>%
    # number of remaining trials
    # nrow()
    # or removing data using the same rule as in Bart et al. (2020)
    # that is, below or above 3 SDs
    mutate(
        sd3_rt_neg = mean(x = reaction_time) - 3 * sd(x = reaction_time, na.rm = TRUE),
        sd3_rt_pos = mean(x = reaction_time) + 3 * sd(x = reaction_time, na.rm = TRUE),
        sd3_mt_neg = mean(x = movement_time) - 3 * sd(x = movement_time, na.rm = TRUE),
        sd3_mt_pos = mean(x = movement_time) + 3 * sd(x = movement_time, na.rm = TRUE)
        ) %>%
    mutate(included = case_when(
        between(reaction_time, sd3_rt_neg, sd3_rt_pos) & between(movement_time, sd3_mt_neg, sd3_mt_pos) ~ TRUE,
        .default = FALSE
        ) ) %>%
    filter(included == TRUE) %>%
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
    # removing NAs or aberrant simulated data
    na.omit() %>%
    filter(reaction_time < 3 & movement_time < 3) %>%
    # number of remaining trials
    # nrow()
    # or removing data using the same rule as in Bart et al. (2020)
    # that is, below or above 3 SDs
    mutate(
        sd3_rt_neg = mean(x = reaction_time) - 3 * sd(x = reaction_time, na.rm = TRUE),
        sd3_rt_pos = mean(x = reaction_time) + 3 * sd(x = reaction_time, na.rm = TRUE),
        sd3_mt_neg = mean(x = movement_time) - 3 * sd(x = movement_time, na.rm = TRUE),
        sd3_mt_pos = mean(x = movement_time) + 3 * sd(x = movement_time, na.rm = TRUE)
        ) %>%
    mutate(included = case_when(
        between(reaction_time, sd3_rt_neg, sd3_rt_pos) & between(movement_time, sd3_mt_neg, sd3_mt_pos) ~ TRUE,
        .default = FALSE
        ) ) %>%
    filter(included == TRUE) %>%
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
    # removing NAs or aberrant simulated data
    na.omit() %>%
    filter(reaction_time < 3 & movement_time < 3) %>%
    # number of remaining trials
    # nrow()
    # or removing data using the same rule as in Bart et al. (2020)
    # that is, below or above 3 SDs
    mutate(
        sd3_rt_neg = mean(x = reaction_time) - 3 * sd(x = reaction_time, na.rm = TRUE),
        sd3_rt_pos = mean(x = reaction_time) + 3 * sd(x = reaction_time, na.rm = TRUE),
        sd3_mt_neg = mean(x = movement_time) - 3 * sd(x = movement_time, na.rm = TRUE),
        sd3_mt_pos = mean(x = movement_time) + 3 * sd(x = movement_time, na.rm = TRUE)
        ) %>%
    mutate(included = case_when(
        between(reaction_time, sd3_rt_neg, sd3_rt_pos) & between(movement_time, sd3_mt_neg, sd3_mt_pos) ~ TRUE,
        .default = FALSE
        ) ) %>%
    filter(included == TRUE) %>%
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
    # removing NAs or aberrant simulated data
    na.omit() %>%
    filter(reaction_time < 3 & movement_time < 3) %>%
    # number of remaining trials
    # nrow()
    # or removing data using the same rule as in Bart et al. (2020)
    # that is, below or above 3 SDs
    mutate(
        sd3_rt_neg = mean(x = reaction_time) - 3 * sd(x = reaction_time, na.rm = TRUE),
        sd3_rt_pos = mean(x = reaction_time) + 3 * sd(x = reaction_time, na.rm = TRUE),
        sd3_mt_neg = mean(x = movement_time) - 3 * sd(x = movement_time, na.rm = TRUE),
        sd3_mt_pos = mean(x = movement_time) + 3 * sd(x = movement_time, na.rm = TRUE)
        ) %>%
    mutate(included = case_when(
        between(reaction_time, sd3_rt_neg, sd3_rt_pos) & between(movement_time, sd3_mt_neg, sd3_mt_pos) ~ TRUE,
        .default = FALSE
        ) ) %>%
    filter(included == TRUE) %>%
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
(p1 + p2) / (p3 + p4)

# saving the plot
ggsave(
    filename = "fitting_results/bart_et_al_2020/predictive_checks_bart_et_al_2020_4pars_fixed_curvature_activ.png",
    width = 12, height = 8, dpi = 300,
    device = "png"
    )

##############################################################################
# checking observed and predicted (simulated) quantiles
# cf http://singmann.org/wiener-model-analysis-with-brms-part-ii/
##########################################################################  

# what quantiles should we look at?
quantile_probs <- seq(0.1, 0.9, 0.1)

qq_ie <- bind_rows(
    df_IE %>% mutate(type = "observed"),
    sim_IE %>% mutate(type = "simulated") %>% na.omit() %>% filter(reaction_time < 3 & movement_time < 3)
    ) %>%
    pivot_longer(names_to = "measure", cols = reaction_time:movement_time) %>%
    group_by(type, measure) %>%
    reframe(enframe(quantile(x = value, probs = quantile_probs, na.rm = TRUE, names = TRUE) ) ) %>%
    ungroup() %>%
    ggplot(
        aes(
            x = name, y = value,
            group = interaction(type, measure),
            colour = measure, fill = measure,
            shape = type
            )
        ) +
    geom_line(alpha = 0.5, linetype = 3) +
    geom_point(
        size = 4,
        alpha = 0.9,
        show.legend = TRUE
        ) +
    theme_bw(base_size = 12, base_family = "Open Sans") +
    scale_fill_manual(values =  met.brewer(name = "Johnson", n = 2) ) +
    scale_colour_manual(values = met.brewer(name = "Johnson", n = 2) ) +
    labs(
        title = "Imagined-executed sequences",
        x = "Percentile", y = "Time (in seconds)"
        )

qq_ee <- bind_rows(
    df_EE %>% mutate(type = "observed"),
    sim_EE %>% mutate(type = "simulated") %>% na.omit() %>% filter(reaction_time < 3 & movement_time < 3)
    ) %>%
    pivot_longer(names_to = "measure", cols = reaction_time:movement_time) %>%
    group_by(type, measure) %>%
    reframe(enframe(quantile(x = value, probs = quantile_probs, na.rm = TRUE, names = TRUE) ) ) %>%
    ungroup() %>%
    ggplot(
        aes(
            x = name, y = value,
            group = interaction(type, measure),
            colour = measure, fill = measure,
            shape = type
            )
        ) +
    geom_line(alpha = 0.5, linetype = 3) +
    geom_point(
        size = 4,
        alpha = 0.9,
        show.legend = TRUE
        ) +
    theme_bw(base_size = 12, base_family = "Open Sans") +
    scale_fill_manual(values =  met.brewer(name = "Johnson", n = 2) ) +
    scale_colour_manual(values = met.brewer(name = "Johnson", n = 2) ) +
    labs(
        title = "Executed-executed sequences",
        x = "Percentile", y = "Time (in seconds)"
        )

qq_ii <- bind_rows(
    df_II %>% mutate(type = "observed"),
    sim_II %>% mutate(type = "simulated") %>% na.omit() %>% filter(reaction_time < 3 & movement_time < 3)
    ) %>%
    pivot_longer(names_to = "measure", cols = reaction_time:movement_time) %>%
    group_by(type, measure) %>%
    reframe(enframe(quantile(x = value, probs = quantile_probs, na.rm = TRUE, names = TRUE) ) ) %>%
    ungroup() %>%
    ggplot(
        aes(
            x = name, y = value,
            group = interaction(type, measure),
            colour = measure, fill = measure,
            shape = type
            )
        ) +
    geom_line(alpha = 0.5, linetype = 3) +
    geom_point(
        size = 4,
        alpha = 0.9,
        show.legend = TRUE
        ) +
    theme_bw(base_size = 12, base_family = "Open Sans") +
    scale_fill_manual(values =  met.brewer(name = "Johnson", n = 2) ) +
    scale_colour_manual(values = met.brewer(name = "Johnson", n = 2) ) +
    labs(
        title = "Imagined-imagined sequences",
        x = "Percentile", y = "Time (in seconds)"
        )

qq_ei <- bind_rows(
    df_EI %>% mutate(type = "observed"),
    sim_EI %>% mutate(type = "simulated") %>% na.omit() %>% filter(reaction_time < 3 & movement_time < 3)
    ) %>%
    pivot_longer(names_to = "measure", cols = reaction_time:movement_time) %>%
    group_by(type, measure) %>%
    reframe(enframe(quantile(x = value, probs = quantile_probs, na.rm = TRUE, names = TRUE) ) ) %>%
    ungroup() %>%
    ggplot(
        aes(
            x = name, y = value,
            group = interaction(type, measure),
            colour = measure, fill = measure,
            shape = type
            )
        ) +
    geom_line(alpha = 0.5, linetype = 3) +
    geom_point(
        size = 4,
        alpha = 0.9,
        show.legend = TRUE
        ) +
    theme_bw(base_size = 12, base_family = "Open Sans") +
    scale_fill_manual(values =  met.brewer(name = "Johnson", n = 2) ) +
    scale_colour_manual(values = met.brewer(name = "Johnson", n = 2) ) +
    labs(
        title = "Executed-imagined sequences",
        x = "Percentile", y = "Time (in seconds)"
        )

# combining all plots
(qq_ie + qq_ee) / (qq_ii + qq_ei) +
    plot_layout(guides = "collect") & theme(legend.position = "bottom")

# saving the plot
ggsave(
    filename = "fitting_results/bart_et_al_2020/quantile_plot_bart_et_al_2020_4pars_fixed_curvature_activ.png",
    width = 16, height = 10, dpi = 300,
    device = "png"
    )

#################################################################
# plotting the implied balance functions per condition
###########################################################

parameters_estimates_summary_IE <- paste(as.vector(rbind(
    paste0(par_names, ": "),
    paste0(as.character(round(estimated_pars_IE, 3) ), "\n")
    ) ), collapse = "") %>% str_sub(end = -2)

p5 <- model(
    nsims = 500, nsamples = 3000,
    exec_threshold = 1, imag_threshold = 0.5,
    amplitude_activ = 1.5,
    peak_time_activ = log(estimated_pars_IE[2]),
    curvature_activ = 0.2,
    amplitude_inhib = 1.5 / estimated_pars_IE[1],
    peak_time_inhib = log(estimated_pars_IE[3] * estimated_pars_IE[2]),
    curvature_inhib = estimated_pars_IE[4] * 0.2,
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
        label = parameters_estimates_summary_IE,
        family = "Courier"
        ) +
    theme_bw(base_size = 12, base_family = "Open Sans") +
    scale_fill_manual(values =  met.brewer(name = "Hiroshige", n = 3) ) +
    scale_colour_manual(values = met.brewer(name = "Hiroshige", n = 3) ) +
    labs(
        title = "Imagined-executed sequences",
        x = "Time within a trial (in seconds)",
        y = "Activation/inhibition (a.u.)",
        colour = "",
        fill = ""
        )

parameters_estimates_summary_EE <- paste(as.vector(rbind(
    paste0(par_names, ": "),
    paste0(as.character(round(estimated_pars_EE, 3) ), "\n")
    ) ), collapse = "") %>% str_sub(end = -2)

p6 <- model(
    nsims = 500, nsamples = 3000,
    exec_threshold = 1, imag_threshold = 0.5,
    amplitude_activ = 1.5,
    peak_time_activ = log(estimated_pars_EE[2]),
    curvature_activ = 0.2,
    amplitude_inhib = 1.5 / estimated_pars_EE[1],
    peak_time_inhib = log(estimated_pars_EE[3] * estimated_pars_EE[2]),
    curvature_inhib = estimated_pars_EE[4] * 0.2,
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
        label = parameters_estimates_summary_EE,
        family = "Courier"
        ) +
    theme_bw(base_size = 12, base_family = "Open Sans") +
    scale_fill_manual(values =  met.brewer(name = "Hiroshige", n = 3) ) +
    scale_colour_manual(values = met.brewer(name = "Hiroshige", n = 3) ) +
    labs(
        title = "Executed-executed sequences",
        x = "Time within a trial (in seconds)",
        y = "Activation/inhibition (a.u.)",
        colour = "",
        fill = ""
        )

parameters_estimates_summary_II <- paste(as.vector(rbind(
    paste0(par_names, ": "),
    paste0(as.character(round(estimated_pars_II, 3) ), "\n")
    ) ), collapse = "") %>% str_sub(end = -2)

p7 <- model(
    nsims = 500, nsamples = 3000,
    exec_threshold = 1, imag_threshold = 0.5,
    amplitude_activ = 1.5,
    peak_time_activ = log(estimated_pars_II[2]),
    curvature_activ = 0.2,
    amplitude_inhib = 1.5 / estimated_pars_II[1],
    peak_time_inhib = log(estimated_pars_II[3] * estimated_pars_II[2]),
    curvature_inhib = estimated_pars_II[4] * 0.2,
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
        label = parameters_estimates_summary_II,
        family = "Courier"
        ) +
    theme_bw(base_size = 12, base_family = "Open Sans") +
    scale_fill_manual(values =  met.brewer(name = "Hiroshige", n = 3) ) +
    scale_colour_manual(values = met.brewer(name = "Hiroshige", n = 3) ) +
    labs(
        title = "Imagined-imagined sequences",
        x = "Time within a trial (in seconds)",
        y = "Activation/inhibition (a.u.)",
        colour = "",
        fill = ""
        )

parameters_estimates_summary_EI <- paste(as.vector(rbind(
    paste0(par_names, ": "),
    paste0(as.character(round(estimated_pars_EI, 3) ), "\n")
    ) ), collapse = "") %>% str_sub(end = -2)

p8 <- model(
    nsims = 500, nsamples = 3000,
    exec_threshold = 1, imag_threshold = 0.5,
    amplitude_activ = 1.5,
    peak_time_activ = log(estimated_pars_EI[2]),
    curvature_activ = 0.2,
    amplitude_inhib = 1.5 / estimated_pars_EI[1],
    peak_time_inhib = log(estimated_pars_EI[3] * estimated_pars_EI[2]),
    curvature_inhib = estimated_pars_EI[4] * 0.2,
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
    # plotting the motor execution and motor imagery thresholds
    geom_hline(yintercept = 1, linetype = 2) +
    geom_hline(yintercept = 0.5, linetype = 2) +
    # geom_texthline(
    #     yintercept = 1, linetype = 2,
    #     hjust = 0.9,
    #     label = "Motor execution threshold"
    #     ) +
    # geom_texthline(
    #     yintercept = 0.5, linetype = 2,
    #     hjust = 0.9,
    #     label = "Motor imagery threshold"
    #     ) +
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
        label = parameters_estimates_summary_EI,
        family = "Courier"
        ) +
    theme_bw(base_size = 12, base_family = "Open Sans") +
    scale_fill_manual(values =  met.brewer(name = "Hiroshige", n = 3) ) +
    scale_colour_manual(values = met.brewer(name = "Hiroshige", n = 3) ) +
    labs(
        title = "Executed-imagined sequences",
        x = "Time within a trial (in seconds)",
        y = "Activation/inhibition (a.u.)",
        colour = "",
        fill = ""
        )

# combining all plots
(p5 + p6) / (p7 + p8) +
    plot_layout(guides = "collect") & theme(legend.position = "bottom")

# saving the plot
ggsave(
    filename = "fitting_results/bart_et_al_2020/balance_function_per_condition_bart_et_al_2020_4pars_fixed_curvature_activ.png",
    width = 16, height = 10, dpi = 300,
    device = "png"
    )

# PPCS: median RT and median MT
# defining a function to compute the predicted RT and MT (quadratic formula)
onset_offset <- function (alpha_f, alpha_g, mu_f, mu_g, sigma_f, sigma_g, thresh) {
    
    a <- sigma_g^2 - sigma_f^2
    b <- 2 * (sigma_f^2 * mu_g - sigma_g^2 * mu_f)
    c <- sigma_f^2 * mu_g^2 - sigma_g^2 * mu_f^2 - 2 * sigma_f^2 * sigma_g^2 * (log(alpha_f / alpha_g) - log(thresh) )
    onset <- exp((-b - sqrt(b^2 - 4 * a * c) ) / (2 * a) )
    offset <- exp((-b + sqrt(b^2 - 4 * a * c) ) / (2 * a) )
    
    return (c(onset, offset) )
    
}

onset_offset_imag_IE <- onset_offset(
    alpha_f = 1.5, alpha_g = 1.5 / estimated_pars_IE[1],
    mu_f = log(estimated_pars_IE[2]), mu_g = log(estimated_pars_IE[3] * estimated_pars_IE[2]),
    sigma_f = 0.2, sigma_g = estimated_pars_IE[4] * 0.2,
    thresh = 0.5
    )

c(min(onset_offset_imag_IE), max(onset_offset_imag_IE) - min(onset_offset_imag_IE) )
median(df_IE$reaction_time)
median(df_IE$movement_time)

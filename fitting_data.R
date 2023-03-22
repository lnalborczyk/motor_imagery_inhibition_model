##############################################
# Fitting the model to actual data           #
# ------------------------------------------ #
# Written by Ladislas Nalborczyk             #
# E-mail: ladislas.nalborczyk@gmail.com      #
# Last updated on March 22, 2023             #
##############################################

library(tidyverse)
library(MetBrewer)

#############################################################################
# Experiment 1 from Bart et al. (2020)
# Group 1 is hand, group 2 is foot
# Other variables explained at https://osf.io/xtpfg
######################################################################

# importing data for IE sequences in the hand group
df_II <- read.csv(file = "private/data_bart_2020/dataset_exp1.csv") %>%
    # keeping only mixed blocks (for comparison purposes)
    select(participant = ID, group, contains(match = "mixed") ) %>%
    # reshaping the RT/MT information
    pivot_longer(cols = 3:34) %>%
    separate(col = name, sep = "_", into = c("name", "global", "hand_rep", "target_rep", "block_type") ) %>%
    # keeping only the hand group
    filter(group == 1) %>%
    # keeping only the IE sequences
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
    select(reaction_time, movement_time, action_mode) %>% slice(1:100)

# plotting the distributions of RTs and MTs
df_II %>%
    pivot_longer(cols = reaction_time:movement_time) %>%
    ggplot(aes(x = value, colour = name, fill = name) ) +
    geom_density(alpha = 0.5, show.legend = FALSE) +
    theme_bw(base_size = 12, base_family = "Open Sans") +
    scale_fill_manual(values =  met.brewer(name = "Johnson", n = 2) ) +
    scale_colour_manual(values = met.brewer(name = "Johnson", n = 2) ) +
    labs(x = "Reaction/Movement time (in seconds)", y = "Density")

# importing the data-generating model
source(file = "model.R")

# importing the model fitting routines
source(file = "fitting.R")

# fitting the model using particle swarm optimisation
# works very well with 1e3 iterations but quite slow...
# fitting_results <- model_fitting(data = df_II, method = "pso", maxit = 1e3)

# fitting the model using differential evolution
# seems to work best in short periods of time...
fitting_results <- model_fitting(data = df_II, method = "DEoptim", maxit = 200)

# plotting the optimisation results
# plot(x = fitting_results, plot.type = "bestmemit", type = "b", col = "steelblue")
# plot(x = fitting_results, plot.type = "bestvalit", type = "b", col = "steelblue")

# getting a summary of the optimisation results
summary(fitting_results)
fitting_results$value
fitting_results$par

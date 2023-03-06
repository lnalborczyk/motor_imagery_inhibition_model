######################################################################
# A simplistic model of motor inhibition during motor imagery
# Generates patterns of activation/inhibition and
# distributions of RTs and MTs
# ---------------------------------------------------------------
# Written by Ladislas Nalborczyk
# E-mail: ladislas.nalborczyk@gmail.com
# Last updated on March 6, 2023
##############################################################

library(tidyverse)
library(patchwork)

#############################################################################
# General function - Parameters
# ----------------------------------------------------------------------
# nsims: number of random walks (number of simulations/experiments)
# nsamples: number of times evidence is being sampled
# amplitude_activ: amplitude of the activation function
# peak_time_activ = 0.5
# curvature_activ = 0.8
# amplitude_inhib: amplitude of the inhibition function
# peak_time_inhib = 0.5
# curvature_inhib = 1.2
# amplitude_inhib_prev: amplitude of the inhibition function (in the previous trial)
# peak_time_inhib_prev = 0.5
# curvature_inhib_prev = 1.2
# exec_threshold: motor execution threshold
# threshold_prop: motor imagery threshold
# iti: inter-trial time interval (arbitrary units)
#################################################################

# nsims = 1e2; nsamples = 1e3;
# amplitude = 1; peak_time = 1; curvature = 0.5;
# amplitude_activ = 1; peak_time_activ = 1; curvature_activ = 0.5;
# threshold = 0.5; threshold_prop = 0.8; iti = 2;
# i = 1;

model <- function (
        nsims = 1e2, nsamples = 1e3,
        amplitude_activ = 1.5, peak_time_activ = 0.5, curvature_activ = 0.8,
        amplitude_inhib = 1.5, peak_time_inhib = 0.5, curvature_inhib = 1.2,
        amplitude_inhib_prev = 1.5, peak_time_inhib_prev = 0.5, curvature_inhib_prev = 1.2,
        exec_threshold = 1, imag_threshold = 0.5, iti = 10
        ) {
    
    # defining the activation/inhibition function
    activation_inhibition_function <- function (
        time = 0, amplitude = 1.5, peak_time = 0.5, curvature = 0.8
        ) {
        
        activ <- amplitude * exp(-(log(time) - peak_time)^2 / (2 * curvature^2) )
        
        return (activ)
        
    }
    
    # plotting the activation/inhibition function
    # curve(
    #     expr = activation_inhibition_function,
    #     from = 0, 20,
    #     main = "Facilitation curve",
    #     xlab = "Time (in seconds)",
    #     ylab = "Facilitation (arbitrary units)"
    #     )
    
    # initialising the results dataframe
    results <- data.frame(
        sim = rep(1:nsims, each = nsamples),
        sample = rep(1:nsamples, nsims),
        exec_threshold = numeric(length = nsims * nsamples),
        imag_threshold = numeric(length = nsims * nsamples),
        activation = numeric(length = nsims * nsamples),
        inhibition = numeric(length = nsims * nsamples),
        inhibition_previous = numeric(length = nsims * nsamples)
        )
    
    for (i in 1:nsims) { # for each simulated experiment
        
        # adding some variability in threshold (or not)
        threshold_sim <- rnorm(n = 1, mean = threshold, sd = 0)
        
        # adding some variability in the other parameters
        amplitude_activ_sim <- rnorm(n = 1, mean = amplitude_activ, sd = 0.1)
        peak_time_activ_sim <- rnorm(n = 1, mean = peak_time_activ, sd = 0.1)
        curvature_activ_sim <- rnorm(n = 1, mean = curvature_activ, sd = 0.01)
        
        amplitude_inhib_sim <- rnorm(n = 1, mean = amplitude_inhib, sd = 0.1)
        peak_time_inhib_sim <- rnorm(n = 1, mean = peak_time_inhib, sd = 0.1)
        curvature_inhib_sim <- rnorm(n = 1, mean = curvature_inhib, sd = 0.01)
        
        amplitude_inhib_prev_sim <- rnorm(n = 1, mean = amplitude_inhib_prev, sd = 0.1)
        peak_time_inhib_prev_sim <- rnorm(n = 1, mean = peak_time_inhib_prev, sd = 0.1)
        curvature_inhib_prev_sim <- rnorm(n = 1, mean = curvature_inhib_prev, sd = 0.01)
        
        # storing execution and imagery thresholds
        results$exec_threshold[results$sim == i] <- threshold_sim
        results$imag_threshold[results$sim == i] <- threshold_prop * threshold_sim
        
        # storing activation values for this simulation
        results$activation[results$sim == i] <- activation_inhibition_function(
            time = 1:nsamples / 100,
            amplitude = amplitude_activ_sim,
            peak_time = peak_time_activ_sim,
            curvature = curvature_activ_sim
            )
        
        # storing inhibition values for this simulation
        results$inhibition[results$sim == i] <- activation_inhibition_function(
            time = 1:nsamples / 100,
            amplitude = amplitude_inhib_sim,
            peak_time = peak_time_inhib_sim,
            curvature = curvature_inhib_sim
            )
        
        # storing inhibition values from previous trial
        results$inhibition_previous[results$sim == i] <- activation_inhibition_function(
            time = iti + 1:nsamples / 100,
            amplitude = amplitude_inhib_prev_sim,
            peak_time = peak_time_inhib_prev_sim,
            curvature = curvature_inhib_prev_sim
            )
        
    }
    
    # returning the results
    return (results)
    
}

###########################################################################
# Example: simulating patterns of activation/inhibition and
# distributions of RTs and MTs
###################################################################

simulations_II <- simulating_rt(
    nsims = 1000, nsamples = 2000,
    amplitude_activ = 1.5, peak_time_activ = 0.5, curvature_activ = 0.8,
    # amplitude_inhib = 1 for imagined current trials and 0.5 for executed trials
    amplitude_inhib = 1.5, peak_time_inhib = 0.5, curvature_inhib = 1.2,
    # amplitude_inhib_prev = 1 for imagined previous trials and 0.5 for executed trials
    amplitude_inhib_prev = 1.5, peak_time_inhib_prev = 0.5, curvature_inhib_prev = 1.2,
    threshold = 1, threshold_prop = 0.25, iti = 10
    )

simulations_EI <- simulating_rt(
    nsims = 1000, nsamples = 2000,
    amplitude_activ = 1.5, peak_time_activ = 0.5, curvature_activ = 0.8,
    # amplitude_inhib = 1 for imagined current trials and 0.5 for executed trials
    amplitude_inhib = 1.5, peak_time_inhib = 0.5, curvature_inhib = 1.2,
    # amplitude_inhib_prev = 1 for imagined previous trials and 0.5 for executed trials
    amplitude_inhib_prev = 0.5, peak_time_inhib_prev = 0.5, curvature_inhib_prev = 1.2,
    threshold = 1, threshold_prop = 0.25, iti = 10
    )

simulations_IE <- simulating_rt(
    nsims = 1000, nsamples = 2000,
    amplitude_activ = 1.5, peak_time_activ = 0.5, curvature_activ = 0.8,
    # amplitude_inhib = 1 for imagined current trials and 0.5 for executed trials
    amplitude_inhib = 0.5, peak_time_inhib = 0.5, curvature_inhib = 1.2,
    # amplitude_inhib_prev = 1 for imagined previous trials and 0.5 for executed trials
    amplitude_inhib_prev = 1.5, peak_time_inhib_prev = 0.5, curvature_inhib_prev = 1.2,
    threshold = 1, threshold_prop = 0.25, iti = 10
    )

simulations_EE <- simulating_rt(
    nsims = 1000, nsamples = 2000,
    amplitude_activ = 1.5, peak_time_activ = 0.5, curvature_activ = 0.8,
    # amplitude_inhib = 1 for imagined current trials and 0.5 for executed trials
    amplitude_inhib = 0.5, peak_time_inhib = 0.5, curvature_inhib = 1.2,
    # amplitude_inhib_prev = 1 for imagined previous trials and 0.5 for executed trials
    amplitude_inhib_prev = 0.5, peak_time_inhib_prev = 0.5, curvature_inhib_prev = 1.2,
    threshold = 1, threshold_prop = 0.25, iti = 10
    )

p1 <- simulations_EE %>%
    # mutate(balance = activation / (inhibition * inhibition_previous) ) %>%
    # mutate(balance = inhibition * inhibition_previous) %>%
    # mutate(balance = activation / inhibition) %>%
    # mutate(balance = activation - inhibition - inhibition_previous) %>%
    # mutate(balance = activation / (inhibition + inhibition_previous) ) %>%
    # mutate(balance = activation / max(inhibition, inhibition_previous) ) %>%
    # mutate(balance = activation / inhibition_previous) %>%
    # mutate(balance = activation * (inhibition + inhibition_previous) ) %>%
    # mutate(balance = exp(activation - inhibition - inhibition_previous) ) %>%
    # mutate(balance = (activation - 2*inhibition - 2*inhibition_previous)^2) %>%
    # activation function that seems to work for II trials
    # mutate(total_inhibition = (inhibition + inhibition_previous)^2) %>%
    # mutate(balance = activation / (inhibition + inhibition_previous)^2) %>%
    # activation function that seems to work for EI trials
    mutate(balance = activation / (inhibition + inhibition_previous) ) %>%
    # reshaping the data
    # select(-total_inhibition)
    pivot_longer(cols = activation:balance) %>%
    ggplot(
        aes(
            x = sample, y = value,
            group = interaction(sim, name),
            colour = name
            )
        ) +
    geom_hline(yintercept = 1, linetype = 2) +
    geom_hline(yintercept = 0.25 * 1, linetype = 2) +
    # geom_texthline(yintercept = 1, linetype = 2, label = "execution threshold") +
    # geom_texthline(yintercept = 0.8 * 1, linetype = 2, label = "imagery threshold") +
    # plotting individual simulations
    geom_line(size = 0.1, alpha = 0.1, show.legend = FALSE) +
    # plotting average
    stat_summary(
        aes(group = name, colour = name),
        fun = "median", geom = "line",
        linewidth = 1, alpha = 1,
        show.legend = TRUE
        ) +
    theme_bw(base_size = 12, base_family = "Open Sans") +
    # scale_fill_manual(values = c("grey70", met.brewer(name = "Johnson", n = 3) ) ) +
    # scale_colour_manual(values = c("grey70", met.brewer(name = "Johnson", n = 3) ) ) +
    scale_fill_manual(values =  met.brewer(name = "Hiroshige", n = 4) ) +
    scale_colour_manual(values = met.brewer(name = "Hiroshige", n = 4) ) +
    labs(
        title = "Simulating activation/inhibition patterns",
        # subtitle = "Net balance is simply defined as the amount of activation minus the amount\nof inhibition in the current trial and the amount of residual inhibition from the previous trial.",
        x = "Time within a trial (in ms)",
        y = "Activation/inhibition (arbitrary units)",
        colour = "",
        fill = ""
        )

p2 <- simulations_EE %>%
    # mutate(balance = activation - inhibition - inhibition_previous) %>%
    # mutate(balance = activation - inhibition) %>%
    # mutate(balance = activation / (inhibition + inhibition_previous) ) %>%
    # mutate(balance = activation * inhibition * inhibition_previous) %>%
    # mutate(balance = activation * (1 / (inhibition + inhibition_previous)^2) ) %>%
    # activation function that seems to work for II trials
    # mutate(balance = activation / (inhibition + inhibition_previous)^2) %>%
    # activation function that seems to work for EI trials
    mutate(balance = activation / (inhibition + inhibition_previous) ) %>%
    # select(-inhibition_previous) %>%
    # pivot_longer(cols = activation:balance) %>%
    group_by(sim) %>%
    # mutate(onset = which(balance > imag_threshold) %>% first() ) %>%
    # mutate(offset = which(balance > imag_threshold) %>% last() ) %>%
    mutate(onset = which(balance > exec_threshold) %>% first() ) %>%
    mutate(offset = which(balance > exec_threshold) %>% last() ) %>%
    ungroup() %>%
    # head(20)
    select(sim, onset, offset) %>%
    distinct() %>%
    # data.frame()
    # head()
    group_by(sim) %>%
    mutate(mt = offset - onset) %>%
    ungroup() %>%
    # head()
    # ggplot(aes(x = mt) ) +
    select(-offset) %>%
    mutate(
        onset_median = median(onset),
        mt_median = median(mt)
        ) %>%
    pivot_longer(cols = onset:mt) %>%
    # head()
    ggplot(aes(x = value, group = name, colour = name, fill = name) ) +
    # geom_vline(yintercept = onset_median, linetype = 3) +
    # geom_vline(yintercept = mt_median, linetype = 3) +
    # geom_line(alpha = 0.2) +
    geom_density(
        alpha = 0.5,
        # colour = met.brewer(name = "Hiroshige", n = 4)[2],
        # fill = met.brewer(name = "Hiroshige", n = 4)[2],
        show.legend = FALSE
        ) +
    # geom_density(
    #     # aes(x = mt),
    #     alpha = 0.5,
    #     colour = met.brewer(name = "Hiroshige", n = 4)[1],
    #     fill = met.brewer(name = "Hiroshige", n = 4)[1],
    #     show.legend = FALSE
    #     ) +
    # geom_x_median(aes(group = name)) +
    geom_x_mean(show.legend = FALSE) +
    geom_x_mean_label(fill = "white", show.legend = FALSE) +
    # geom_y_median(aes(color = name) ) +
    # aes(color = name)
    # geom_boxplot() +
    theme_bw(base_size = 12, base_family = "Open Sans") +
    # coord_cartesian(xlim = c(0, 500) ) +
    # scale_fill_manual(values =  met.brewer(name = "Hiroshige", n = 4) ) +
    # scale_colour_manual(values = met.brewer(name = "Hiroshige", n = 4) ) +
    scale_fill_manual(values =  met.brewer(name = "Johnson", n = 2) ) +
    scale_colour_manual(values = met.brewer(name = "Johnson", n = 2) ) +
    labs(
        title = "Simulating the distribution of RTs and MTs",
        # subtitle = "Reaction time can be defined as the time between stimulus onset and when the balance crosses the execution or imagery threshold.",
        x = "Reaction/Movement time (in ms)",
        y = "Probability density"
        )

p1 + p2 + plot_layout(guides = "collect") & theme(legend.position = "bottom")

# saving the plot
# ggsave(
#     filename = "figures/RT_MT_EE_new_balance_function.png",
#     width = 16, height = 8, dpi = 300,
#     device = "png"
#     )

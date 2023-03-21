###############################################################
# A simplistic model of motor inhibition during motor imagery #
# Generates patterns of activation/inhibition and             #
# distributions of RTs and MTs                                #
# ----------------------------------------------------------- #
# Written by Ladislas Nalborczyk                              #
# E-mail: ladislas.nalborczyk@gmail.com                       #
# Last updated on March 9, 2023                               #
###############################################################

library(tidyverse)
library(patchwork)
library(MetBrewer)

#############################################################################
# General function - Parameters
# For alternative parametrisations of the lognormal distribution,
# see https://en.wikipedia.org/wiki/Log-normal_distribution
# ----------------------------------------------------------------------
# nsims: number of random walks (number of simulations/trials)
# nsamples: number of samples (time steps) within a trial
# exec_threshold: motor execution threshold
# threshold_prop: motor imagery threshold
# iti: inter-trial time interval (in seconds)
# amplitude_activ: amplitude of the activation function
# peak_time_activ: peak time of the activation function
# curvature_activ: curvature of the activation function
# amplitude_inhib: amplitude of the inhibition function
# peak_time_inhib: peak time of the inhibition function
# curvature_inhib: curvature of the inhibition function
# amplitude_inhib_prev: amplitude of the inhibition function (in the previous trial)
# peak_time_inhib_prev: peak time of the inhibition function (in the previous trial)
# curvature_inhib_prev: curvature of the inhibition function (in the previous trial)
#################################################################

model <- function (
        nsims = 1e2, nsamples = 1e3,
        exec_threshold = 1, imag_threshold = 0.5, iti = 2,
        amplitude_activ = 1.5, peak_time_activ = 0.5, curvature_activ = 0.8,
        amplitude_inhib = 1.5, peak_time_inhib = 0.5, curvature_inhib = 1.2,
        amplitude_inhib_prev = 1.5, peak_time_inhib_prev = 0.5, curvature_inhib_prev = 1.2
        ) {
    
    # defining the activation/inhibition function
    # basically an unnormalised lognormal distribution
    # e.g., https://www.cell.com/neuron/pdf/S0896-6273(11)00879-8.pdf
    # https://en.wikipedia.org/wiki/Log-normal_distribution
    # if two independent, log-normal variables are multiplied [divided],
    # the product [ratio] is again log-normal, with parameters mu = mu_1 + mu_2
    # [mu = mu1 − mu2] and sigma = sigma_1 + sigma_2 [sigma = sigma_1 - sigma_2]
    activation_inhibition_function <- function (
        time = 0, amplitude = 1.5, peak_time = 0.5, curvature = 0.6
        ) {
        
        activ <- amplitude * exp(-(log(time) - peak_time)^2 / (2 * curvature^2) )
        
        return (activ)
        
    }
    
    # plotting the activation/inhibition function
    # curve(
    #     expr = activation_inhibition_function,
    #     from = 0, 2,
    #     main = "Facilitation curve",
    #     xlab = "Time (in seconds)",
    #     ylab = "Facilitation (arbitrary units)"
    #     )
    
    # initialising the results dataframe
    results <- data.frame(
        sim = rep(1:nsims, each = nsamples),
        sample = rep(1:nsamples, nsims),
        time = rep(1:nsamples, nsims) / 1e3,
        exec_threshold = exec_threshold,
        imag_threshold = imag_threshold,
        activation = numeric(length = nsims * nsamples),
        inhibition = numeric(length = nsims * nsamples),
        inhibition_previous = numeric(length = nsims * nsamples)
        )
    
    for (i in 1:nsims) { # for each simulated experiment
        
        # adding some variability in threshold (or not)
        # threshold_sim <- rnorm(n = 1, mean = threshold, sd = 0)
        # threshold_sim <- rnorm(n = 1, mean = threshold, sd = 0)
        
        # storing execution and imagery thresholds
        # results$exec_threshold[results$sim == i] <- exec_threshold
        # results$imag_threshold[results$sim == i] <- imag_threshold
        
        # adding some variability in the other parameters
        amplitude_activ_sim <- rnorm(n = 1, mean = amplitude_activ, sd = 0.01)
        peak_time_activ_sim <- rnorm(n = 1, mean = peak_time_activ, sd = 0.01)
        curvature_activ_sim <- rnorm(n = 1, mean = curvature_activ, sd = 0.01)
        
        amplitude_inhib_sim <- rnorm(n = 1, mean = amplitude_inhib, sd = 0.01)
        peak_time_inhib_sim <- rnorm(n = 1, mean = peak_time_inhib, sd = 0.01)
        curvature_inhib_sim <- rnorm(n = 1, mean = curvature_inhib, sd = 0.01)
        
        amplitude_inhib_prev_sim <- rnorm(n = 1, mean = amplitude_inhib_prev, sd = 0.01)
        peak_time_inhib_prev_sim <- rnorm(n = 1, mean = peak_time_inhib_prev, sd = 0.01)
        curvature_inhib_prev_sim <- rnorm(n = 1, mean = curvature_inhib_prev, sd = 0.01)
        
        # storing activation values for this simulation
        results$activation[results$sim == i] <- activation_inhibition_function(
            time = seq.int(from = 0, to = 5, length.out = nsamples),
            amplitude = amplitude_activ_sim,
            peak_time = peak_time_activ_sim,
            curvature = curvature_activ_sim
            )
        
        # storing inhibition values for this simulation
        results$inhibition[results$sim == i] <- activation_inhibition_function(
            time = seq.int(from = 0, to = 5, length.out = nsamples),
            amplitude = amplitude_inhib_sim,
            peak_time = peak_time_inhib_sim,
            curvature = curvature_inhib_sim
            )
        
        # storing inhibition values from previous trial
        results$inhibition_previous[results$sim == i] <- activation_inhibition_function(
            time = iti + seq.int(from = 0, to = 5, length.out = nsamples),
            amplitude = amplitude_inhib_prev_sim,
            peak_time = peak_time_inhib_prev_sim,
            curvature = curvature_inhib_prev_sim
            )
        
    }
    
    # computing the activation/inhibition balance and
    # implied distributions of RTs and MTs
    results <- results %>%
        group_by(sim) %>%
        mutate(balance = activation / (inhibition + inhibition_previous) ) %>%
        mutate(onset_exec = which(balance > exec_threshold) %>% first() ) %>%
        mutate(offset_exec = which(balance > exec_threshold) %>% last() ) %>%
        mutate(mt_exec = offset_exec - onset_exec) %>%
        mutate(onset_imag = which(balance > imag_threshold) %>% first() ) %>%
        mutate(offset_imag = which(balance > imag_threshold) %>% last() ) %>%
        mutate(mt_imag = offset_imag - onset_imag) %>%
        # convert from ms to seconds
        mutate(
            onset_exec = onset_exec / 1e3,
            offset_exec = offset_exec / 1e3,
            onset_imag = onset_imag / 1e3,
            offset_imag = offset_imag / 1e3,
            mt_exec = mt_exec / 1e3,
            mt_imag = mt_imag / 1e3
            ) %>%
        ungroup()
    
    # returning the results
    return (results)
    
}

###########################################################################
# Example: simulating patterns of activation/inhibition and
# distributions of RTs and MTs (in seconds)
###################################################################

# simulation_results <- model(
#     nsims = 1000, nsamples = 2000,
#     exec_threshold = 1, imag_threshold = 0.5, iti = 2,
#     amplitude_activ = 1.5, peak_time_activ = 0.25, curvature_activ = 0.4,
#     amplitude_inhib = 1.5, peak_time_inhib = 0.25, curvature_inhib = 0.6,
#     amplitude_inhib_prev = 1.5, peak_time_inhib_prev = 0.25, curvature_inhib_prev = 0.6
#     )
# 
# p1 <- simulation_results %>%
#     pivot_longer(cols = activation:balance) %>%
#     ggplot(
#         aes(
#             x = time, y = value,
#             group = interaction(sim, name),
#             colour = name
#             )
#         ) +
#     geom_hline(yintercept = 1, linetype = 2) +
#     geom_hline(yintercept = 0.5, linetype = 2) +
#     # plotting individual simulations
#     geom_line(size = 0.1, alpha = 0.1, show.legend = FALSE) +
#     # plotting average
#     stat_summary(
#         aes(group = name, colour = name),
#         fun = "median", geom = "line",
#         linewidth = 1, alpha = 1,
#         show.legend = TRUE
#         ) +
#     theme_bw(base_size = 12, base_family = "Open Sans") +
#     scale_fill_manual(values =  met.brewer(name = "Hiroshige", n = 4) ) +
#     scale_colour_manual(values = met.brewer(name = "Hiroshige", n = 4) ) +
#     labs(
#         title = "Simulating activation/inhibition patterns",
#         subtitle = "Net balance is defined as activation_current / (inhibition_current + inhibition_previous)",
#         x = "Time within a trial (in seconds)",
#         y = "Activation/inhibition (a.u.)",
#         colour = "",
#         fill = ""
#         )
# 
# p2 <- simulation_results %>%
#     mutate(
#         exec_rt_median = median(onset_exec),
#         imag_rt_median = median(onset_imag),
#         exec_mt_median = median(mt_exec),
#         imag_mt_median = median(mt_imag),
#         ) %>%
#     pivot_longer(cols = c(onset_imag, mt_imag) ) %>%
#     ggplot(aes(x = value, group = name, colour = name, fill = name) ) +
#     geom_density(
#         alpha = 0.5,
#         adjust = 2,
#         show.legend = FALSE
#         ) +
#     geom_label(
#         data = . %>% summarise(m = unique(imag_rt_median) ),
#         aes(x = m, y = 0, label = round(m, 3) ),
#         position = position_nudge(y = 0.01),
#         size = 4,
#         inherit.aes = FALSE
#         ) +
#     geom_label(
#         data = . %>% summarise(m = unique(imag_mt_median) ),
#         aes(x = m, y = 0, label = round(m, 3) ),
#         position = position_nudge(y = 0.01),
#         size = 4,
#         inherit.aes = FALSE
#         ) +
#     theme_bw(base_size = 12, base_family = "Open Sans") +
#     scale_fill_manual(values =  met.brewer(name = "Johnson", n = 2) ) +
#     scale_colour_manual(values = met.brewer(name = "Johnson", n = 2) ) +
#     labs(
#         title = "Simulating the implied distribution of RTs and MTs",
#         x = "Reaction/Movement time (in seconds)",
#         y = "Probability density"
#         )

# combining the plots
# p1 + p2 + plot_layout(guides = "collect") & theme(legend.position = "bottom")

# saving the plot
# ggsave(
#     filename = "model_output.png",
#     width = 16, height = 8, dpi = 300,
#     device = "png"
#     )

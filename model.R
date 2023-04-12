###############################################################
# A simplistic model of motor inhibition during motor imagery #
# Generates distributions of RTs and MTs according to the     #
# balance between excitatory and inhibitory inputs            #
# ----------------------------------------------------------- #
# Written by Ladislas Nalborczyk                              #
# E-mail: ladislas.nalborczyk@gmail.com                       #
# Last updated on April 12, 2023                              #
###############################################################

library(geomtextpath)
library(tidyverse)
library(patchwork)
library(MetBrewer)

################################################################################
# General function - Parameters
# ----------------------------------------------------------------------------
# nsims: number of simulations (observations/trials)
# nsamples: number of samples (time steps) within a trial
# exec_threshold: motor execution threshold
# threshold_prop: motor imagery threshold
# amplitude_activ: amplitude of the activation function
# peak_time_activ: peak time of the activation function
# curvature_activ: curvature of the activation function
# amplitude_inhib: amplitude of the inhibition function
# peak_time_inhib: peak time of the inhibition function
# curvature_inhib: curvature of the inhibition function
# full_output: boolean indicating whether activ/inhib curves should be returned
################################################################################

model <- function (
        nsims = 100, nsamples = 3000,
        exec_threshold = 1, imag_threshold = 0.5,
        amplitude_activ = 1.5, peak_time_activ = 0, curvature_activ = 0.2,
        amplitude_inhib = 1.5, peak_time_inhib = 0, curvature_inhib = 0.4,
        full_output = FALSE
        ) {
    
    # defining the activation/inhibition rescaled lognormal function
    # could also use a rescaled gamma (or inverse gaussian)?
    # https://www.sciencedirect.com/science/article/abs/pii/S0010028515000195?via%3Dihub
    # peak time (in seconds) is given by exp(peak_time)
    activation_inhibition_function <- function (
        time = 0, amplitude = 1.5, peak_time = 0, curvature = 0.4
        ) {

        # adding some variability in the other parameters
        # variability is currently fixed but could also be estimated
        amplitude_sim <- rnorm(n = 1, mean = amplitude, sd = 0.05)
        peak_time_sim <- rnorm(n = 1, mean = peak_time, sd = 0.05)
        curvature_sim <- rnorm(n = 1, mean = curvature, sd = 0.01)

        activ_inhib <- amplitude_sim *
            exp(-(log(time) - peak_time_sim)^2 / (2 * curvature_sim^2) )

        return (activ_inhib)

    }
    
    # or directly computing the balance (ratio) between activation and
    # inhibition in the current trial (activation/inhibition)
    # basically a ratio of two rescaled lognormal distribution
    # the mode of the function is given by:
    # exp((mu_f * sigma_g^2 - mu_g * sigma_f^2) / (sigma_g^2 - sigma_f^2) )
    balance_funtion <- function (
        time = 0,
        amplitude_activ = 1.5, peak_time_activ = 0, curvature_activ = 0.2,
        amplitude_inhib = 1.5, peak_time_inhib = 0, curvature_inhib = 0.4
        ) {
        
        # adding some variability in the other parameters
        # variability is currently fixed but could also be estimated
        amplitude_activ_sim <- rnorm(n = 1, mean = amplitude_activ, sd = 0.01)
        peak_time_activ_sim <- rnorm(n = 1, mean = peak_time_activ, sd = 0.01)
        curvature_activ_sim <- rnorm(n = 1, mean = curvature_activ, sd = 0.01)
        
        amplitude_inhib_sim <- rnorm(n = 1, mean = amplitude_inhib, sd = 0.01)
        peak_time_inhib_sim <- rnorm(n = 1, mean = peak_time_inhib, sd = 0.01)
        curvature_inhib_sim <- rnorm(n = 1, mean = curvature_inhib, sd = 0.01)
        
        # computing the balance (ratio)
        balance_output <- (amplitude_activ_sim / amplitude_inhib_sim) *
            exp(-(log(time) - peak_time_activ_sim)^2 / (2 * curvature_activ_sim^2) +
                    (log(time) - peak_time_inhib_sim)^2 / (2 * curvature_inhib_sim^2) )
        
        # when time = 0, balance should be 0 as well (starting point)
        balance_output <- ifelse(test = time == 0, yes = 0, no = balance_output)
        
        # returning it
        return (balance_output)
        
    }
    
    # computing the activation/inhibition balance and
    # implied distributions of RTs and MTs per simulation
    results <- data.frame(
        sim = rep(1:nsims, each = nsamples),
        sample = rep(1:nsamples, nsims),
        time = rep(1:nsamples, nsims) / 1e3,
        exec_threshold = exec_threshold,
        imag_threshold = imag_threshold
        ) %>%
        group_by(sim) %>%
        # if full_output = TRUE, returns activation and inhibition functions
        # in addition to the balance function
        {if (full_output)
                mutate(
                    ., activation = activation_inhibition_function(
                        time = time,
                        amplitude = amplitude_activ,
                        peak_time = peak_time_activ,
                        curvature = curvature_activ
                        )
                    ) %>%
                mutate(
                    ., inhibition = activation_inhibition_function(
                        time = time,
                        amplitude = amplitude_inhib,
                        peak_time = peak_time_inhib,
                        curvature = curvature_inhib
                        )
                    )
            else . } %>%
        # mutate(balance = activation / inhibition) %>%
        mutate(
            balance = balance_funtion(
                time = time,
                amplitude_activ = amplitude_activ,
                peak_time_activ = peak_time_activ,
                curvature_activ = curvature_activ,
                amplitude_inhib = amplitude_inhib,
                peak_time_inhib = peak_time_inhib,
                curvature_inhib = curvature_inhib
                )
            ) %>%
        # numerically finding the balance's onset (RT) and offset
        mutate(onset_exec = which(balance > exec_threshold) %>% first() ) %>%
        mutate(offset_exec = which(balance > exec_threshold) %>% last() ) %>%
        # MT is defined as offset minus onset
        mutate(mt_exec = offset_exec - onset_exec) %>%
        mutate(onset_imag = which(balance > imag_threshold) %>% first() ) %>%
        mutate(offset_imag = which(balance > imag_threshold) %>% last() ) %>%
        mutate(mt_imag = offset_imag - onset_imag) %>%
        # convert from ms to seconds
        mutate(across(onset_exec:mt_imag, ~ . / 1e3) ) %>%
        ungroup()
    
    # returning the results
    return (results)
    
}

###########################################################################
# Example: simulating patterns of activation/inhibition and
# distributions of RTs and MTs (in seconds)
###################################################################

# simulation_results <- model(
#     nsims = 1e2, nsamples = 3000,
#     exec_threshold = 1, imag_threshold = 0.5,
#     amplitude_activ = 1.5, peak_time_activ = -0.5, curvature_activ = 0.3,
#     amplitude_inhib = 1.75, peak_time_inhib = -0.5, curvature_inhib = 0.5
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
#     # plotting the motor execution and motor imagery thresholds
#     geom_texthline(
#         yintercept = 1, linetype = 2,
#         hjust = 0.9,
#         label = "Motor execution threshold"
#         ) +
#     geom_texthline(
#         yintercept = 0.5, linetype = 2,
#         hjust = 0.9,
#         label = "Motor imagery threshold"
#         ) +
#     # plotting some individual simulations
#     geom_line(
#         data = . %>% filter(sim %in% unique(sim)[1:100]),
#         linewidth = 0.5, alpha = 0.1, show.legend = FALSE
#         ) +
#     # plotting average
#     stat_summary(
#         aes(group = name, colour = name),
#         fun = "median", geom = "line",
#         linewidth = 1, alpha = 1,
#         show.legend = TRUE
#         ) +
#     theme_bw(base_size = 12, base_family = "Open Sans") +
#     scale_fill_manual(values =  met.brewer(name = "Hiroshige", n = 3) ) +
#     scale_colour_manual(values = met.brewer(name = "Hiroshige", n = 3) ) +
#     labs(
#         title = "Simulating activation/inhibition patterns",
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
#     ggplot(
#         aes(
#             x = value, group = name,
#             colour = name, fill = name
#             )
#         ) +
#     geom_density(
#         color = "white",
#         alpha = 0.6,
#         adjust = 5,
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
#     scale_fill_manual(values =  met.brewer(name = "Johnson", n = 5)[4:5]) +
#     scale_colour_manual(values = met.brewer(name = "Johnson", n = 5)[4:5]) +
#     labs(
#         title = "Simulating the implied distributions of RTs and MTs",
#         x = "Reaction/Movement time (in seconds)",
#         y = "Probability density"
#         )

# combining the plots
# p1 + p2 + plot_layout(guides = "collect") & theme(legend.position = "bottom")

# saving the plot
# ggsave(
#     filename = "figures/model_output.png",
#     width = 16, height = 8, dpi = 300,
#     device = "png"
#     )

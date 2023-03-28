###############################################################
# A simplistic model of motor inhibition during motor imagery #
# Generates distributions of RTs and MTs according to the     #
# balance between excitatory and inhibitory inputs            #
# ----------------------------------------------------------- #
# Written by Ladislas Nalborczyk                              #
# E-mail: ladislas.nalborczyk@gmail.com                       #
# Last updated on March 28, 2023                              #
###############################################################

library(tidyverse)
library(patchwork)
library(MetBrewer)

#############################################################################
# General function - Parameters
# ----------------------------------------------------------------------
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
#################################################################

# for testing purposes
# nsims = 100; nsamples = 2000;
# exec_threshold = 1; imag_threshold = 0.5;
# amplitude_activ = 1.5; peak_time_activ = 0.5; curvature_activ = 0.4;
# amplitude_inhib = 1.5; peak_time_inhib = 0.5; curvature_inhib = 0.6;

model <- function (
        nsims = 100, nsamples = 2000,
        exec_threshold = 1, imag_threshold = 0.5,
        amplitude_activ = 1.5, peak_time_activ = 0.5, curvature_activ = 0.4,
        amplitude_inhib = 1.5, peak_time_inhib = 0.5, curvature_inhib = 0.6
        ) {
    
    # defining the activation/inhibition rescaled lognormal function
    # could also use a rescaled gamma (or inverse gaussian)?
    # https://www.sciencedirect.com/science/article/abs/pii/S0010028515000195?via%3Dihub
    # peak time (in seconds) is given by exp(peak_time)
    # activation_inhibition_function <- function (
    #     time = 0, amplitude = 1.5, peak_time = 0.5, curvature = 0.6
    #     ) {
    #     
    #     activ_inhib <- amplitude * exp(-(log(time) - peak_time)^2 / (2 * curvature^2) )
    #     
    #     return (activ_inhib)
    #     
    # }
    
    # or directly computing the balance (ratio) between activation and
    # inhibition in the current trial (activation/inhibition)
    # basically a ratio of two rescaled lognormal distribution
    # the mode of the function is given by exp((mu_f * sigma_g^2 - mu_g * sigma_f^2) / (sigma_g^2 - sigma_f^2) )
    balance_funtion <- function (
        time = 0,
        amplitude_activ = 1.5, peak_time_activ = 0.5, curvature_activ = 0.4, 
        amplitude_inhib = 1.5, peak_time_inhib = 0.5, curvature_inhib = 0.6
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
    
    # taking log of peak_time so that it scales more naturally in seconds
    # peak_time_activ <- log(ifelse(peak_time_activ == 0, peak_time_activ + 1e-6, peak_time_activ) )
    # peak_time_inhib <- log(ifelse(peak_time_inhib == 0, peak_time_inhib + 1e-6, peak_time_inhib) )
    
    # defining the time scaling factor so that predictions scale more naturally in seconds
    timescale <- 5
    
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
        # mutate(
        #     activation = activation_inhibition_function(
        #         time = seq.int(from = 0, to = timescale, length.out = nsamples),
        #         amplitude = amplitude_activ,
        #         peak_time = peak_time_activ,
        #         curvature = curvature_activ
        #         )
        #     ) %>%
        # mutate(
        #     inhibition = activation_inhibition_function(
        #         time = seq.int(from = 0, to = timescale, length.out = nsamples),
        #         amplitude = amplitude_inhib,
        #         peak_time = peak_time_inhib,
        #         curvature = curvature_inhib
        #         )
        #     ) %>%
        # mutate(balance = activation / inhibition) %>%
        mutate(
            balance = balance_funtion(
                time = seq.int(from = 0, to = timescale, length.out = nsamples),
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
#     nsims = 200, nsamples = 2000,
#     exec_threshold = 1, imag_threshold = 0.5,
#     amplitude_activ = 1.5, peak_time_activ = 0.5, curvature_activ = 0.4,
#     amplitude_inhib = 1.5, peak_time_inhib = 0.5, curvature_inhib = 0.6
#     )
# 
# p1 <- simulation_results %>%
#     pivot_longer(cols = balance) %>%
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
#     geom_line(size = 0.5, alpha = 0.1, show.legend = FALSE) +
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
#         subtitle = "Balance function is defined as activation_current / inhibition_current",
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
#         color = "white",
#         alpha = 0.6,
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
#     filename = "figures/model_output.png",
#     width = 16, height = 8, dpi = 300,
#     device = "png"
#     )

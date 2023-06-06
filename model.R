###############################################################
# A simplistic model of motor inhibition during motor imagery #
# Generates distributions of RTs and MTs according to the     #
# balance between excitatory and inhibitory inputs            #
# ----------------------------------------------------------- #
# Written by Ladislas Nalborczyk                              #
# E-mail: ladislas.nalborczyk@gmail.com                       #
# Last updated on June 5, 2023                                #
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
# model_version: threshold modulation model ("tmm") or parallel inhibition model ("pim")
# full_output: boolean indicating whether activ/inhib curves should be returned
################################################################################

# nsims = 100; nsamples = 3000;
# exec_threshold = 1; imag_threshold = 0.5;
# amplitude_activ = 1.5; peak_time_activ = 0; curvature_activ = 0.4;
# amplitude_inhib = 1.5; peak_time_inhib = 0; curvature_inhib = 0.6;
# model_version = "tmm"; full_output = FALSE;

model <- function (
        nsims = 100, nsamples = 3000,
        exec_threshold = 1, imag_threshold = 0.5,
        amplitude_activ = 1.5, peak_time_activ = 0, curvature_activ = 0.4,
        amplitude_inhib = 1.5, peak_time_inhib = 0, curvature_inhib = 0.6,
        model_version = c("tmm", "pim"),
        full_output = FALSE
        ) {
    
    # if full_output = TRUE, returns the full activation, inhibition,
    # and balance functions
    if (full_output == TRUE) {
    
        # defining the activation/inhibition rescaled lognormal function
        activation_inhibition_function <- function (
            time = 0, amplitude = 1.5, peak_time = 0, curvature = 0.4
            ) {
    
            # adding some variability in the other parameters
            # variability is currently fixed but could also be estimated
            amplitude_sim <- rnorm(n = 1, mean = amplitude, sd = 0.01)
            peak_time_sim <- rnorm(n = 1, mean = peak_time, sd = 0.01)
            curvature_sim <- rnorm(n = 1, mean = curvature, sd = 0.01)
    
            # computing the activation/inhibition value
            activ_inhib <- amplitude_sim * exp(-(log(time) - peak_time_sim)^2 / (2 * curvature_sim^2) )
    
            # returning it
            return (activ_inhib)
    
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
            mutate(
                activation = activation_inhibition_function(
                    time = time,
                    amplitude = amplitude_activ,
                    peak_time = peak_time_activ,
                    curvature = curvature_activ
                    )
                ) %>%
            mutate(
                inhibition = activation_inhibition_function(
                    time = time,
                    amplitude = amplitude_inhib,
                    peak_time = peak_time_inhib,
                    curvature = curvature_inhib
                    )
                ) %>%
            {if (model_version == "pim") mutate(., balance = activation / inhibition) else mutate(., balance = activation)} %>%
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
        
    } else {
        
        # defining a function to compute the predicted RT and MT (quadratic formula)
        onset_offset <- function (alpha_f, alpha_g, mu_f, mu_g, sigma_f, sigma_g, thresh) {
            
            a <- sigma_g^2 - sigma_f^2
            b <- 2 * (sigma_f^2 * mu_g - sigma_g^2 * mu_f)
            c <- sigma_f^2 * mu_g^2 - sigma_g^2 * mu_f^2 - 2 * sigma_f^2 * sigma_g^2 * (log(alpha_f / alpha_g) - log(thresh) )
            onset <- exp((-b - sqrt(b^2 - 4 * a * c) ) / (2 * a) )
            offset <- exp((-b + sqrt(b^2 - 4 * a * c) ) / (2 * a) )

            return (c(onset, offset) )
            
        }
        
        # defining the balance function
        # basically a ratio of two rescaled lognormal functions
        # why ratio? see https://www.cell.com/neuron/pdf/S0896-6273(11)00879-8.pdf
        # the mode of the function is given by:
        # exp((mu_f * sigma_g^2 - mu_g * sigma_f^2) / (sigma_g^2 - sigma_f^2) )
        balance_function <- function (
            exec_threshold = 1, imag_threshold = 0.5,
            amplitude_activ = 1.5, peak_time_activ = 0, curvature_activ = 0.4,
            amplitude_inhib = 1.5, peak_time_inhib = 0, curvature_inhib = 0.6
            ) {
            
            # adding some variability in the other parameters
            # variability is currently fixed but could also be estimated
            amplitude_activ_sim <- rnorm(n = 1, mean = amplitude_activ, sd = 0.01)
            peak_time_activ_sim <- rnorm(n = 1, mean = peak_time_activ, sd = 0.01)
            curvature_activ_sim <- rnorm(n = 1, mean = curvature_activ, sd = 0.01)
            
            amplitude_inhib_sim <- rnorm(n = 1, mean = amplitude_inhib, sd = 0.01)
            peak_time_inhib_sim <- rnorm(n = 1, mean = peak_time_inhib, sd = 0.01)
            curvature_inhib_sim <- rnorm(n = 1, mean = curvature_inhib, sd = 0.01)
            
            if (model_version == "tmm") {
                
                exec_threshold_sim <- rnorm(n = 1, mean = exec_threshold, sd = 0.01)
                imag_threshold_sim <- rnorm(n = 1, mean = imag_threshold, sd = 0.01)
                
            } else if (model_version == "pim") {
             
                exec_threshold_sim <- exec_threshold
                imag_threshold_sim <- imag_threshold
                
            }
            
            # computing the predicted RT and MT in imagery
            # onset_imag <- (2 * (curvature_inhib_sim^2 * peak_time_activ_sim - curvature_activ_sim^2 * peak_time_inhib_sim) - sqrt((2 * (curvature_inhib_sim^2 * peak_time_activ_sim - curvature_activ_sim^2 * peak_time_inhib_sim) )^2 - 4 * (curvature_inhib_sim^2 - curvature_activ_sim^2) * (curvature_inhib_sim^2 * peak_time_activ_sim^2 - curvature_activ_sim^2 * peak_time_inhib_sim^2 - 2 * (log(amplitude_activ_sim / amplitude_inhib_sim) - log(imag_threshold) ) * curvature_activ_sim^2 * curvature_inhib_sim^2) ) ) / (2 * (curvature_inhib_sim^2 - curvature_activ_sim^2))
            # offset_imag <- (2 * (curvature_inhib_sim^2 * peak_time_activ_sim - curvature_activ_sim^2 * peak_time_inhib_sim) + sqrt((2 * (curvature_inhib_sim^2 * peak_time_activ_sim - curvature_activ_sim^2 * peak_time_inhib_sim) )^2 - 4 * (curvature_inhib_sim^2 - curvature_activ_sim^2) * (curvature_inhib_sim^2 * peak_time_activ_sim^2 - curvature_activ_sim^2 * peak_time_inhib_sim^2 - 2 * (log(amplitude_activ_sim / amplitude_inhib_sim) - log(imag_threshold) ) * curvature_activ_sim^2 * curvature_inhib_sim^2) ) ) / (2 * (curvature_inhib_sim^2 - curvature_activ_sim^2))
            onset_offset_imag <- onset_offset(
                alpha_f = amplitude_activ_sim, alpha_g = amplitude_inhib_sim,
                mu_f = peak_time_activ_sim, mu_g = peak_time_inhib_sim,
                sigma_f = curvature_activ_sim, sigma_g = curvature_inhib_sim,
                thresh = imag_threshold_sim
                )
            
            onset_imag <- min(onset_offset_imag)
            mt_imag <- max(onset_offset_imag) - min(onset_offset_imag)
            
            # computing the predicted RT and MT in execution
            # onset_exec <- (2 * (curvature_inhib_sim^2 * peak_time_activ_sim - curvature_activ_sim^2 * peak_time_inhib_sim) - sqrt((2 * (curvature_inhib_sim^2 * peak_time_activ_sim - curvature_activ_sim^2 * peak_time_inhib_sim) )^2 - 4 * (curvature_inhib_sim^2 - curvature_activ_sim^2) * (curvature_inhib_sim^2 * peak_time_activ_sim^2 - curvature_activ_sim^2 * peak_time_inhib_sim^2 - 2 * (log(amplitude_activ_sim / amplitude_inhib_sim) - log(exec_threshold) ) * curvature_activ_sim^2 * curvature_inhib_sim^2) ) ) / (2 * (curvature_inhib_sim^2 - curvature_activ_sim^2))
            # offset_exec <- (2 * (curvature_inhib_sim^2 * peak_time_activ_sim - curvature_activ_sim^2 * peak_time_inhib_sim) + sqrt((2 * (curvature_inhib_sim^2 * peak_time_activ_sim - curvature_activ_sim^2 * peak_time_inhib_sim) )^2 - 4 * (curvature_inhib_sim^2 - curvature_activ_sim^2) * (curvature_inhib_sim^2 * peak_time_activ_sim^2 - curvature_activ_sim^2 * peak_time_inhib_sim^2 - 2 * (log(amplitude_activ_sim / amplitude_inhib_sim) - log(exec_threshold) ) * curvature_activ_sim^2 * curvature_inhib_sim^2) ) ) / (2 * (curvature_inhib_sim^2 - curvature_activ_sim^2))
            onset_offset_exec <- onset_offset(
                alpha_f = amplitude_activ_sim, alpha_g = amplitude_inhib_sim,
                mu_f = peak_time_activ_sim, mu_g = peak_time_inhib_sim,
                sigma_f = curvature_activ_sim, sigma_g = curvature_inhib_sim,
                thresh = exec_threshold_sim
                )
            
            onset_exec <- min(onset_offset_exec)
            mt_exec <- max(onset_offset_exec) - min(onset_offset_exec)
            
            # returning it
            return (data.frame(onset_imag, mt_imag, onset_exec, mt_exec) )
            
        }
        
        # computing the activation/inhibition balance
        # and the implied/predicted distributions of RTs and MTs 
        results <- data.frame(
            sim = rep(1:nsims, each = nsamples),
            exec_threshold = exec_threshold,
            imag_threshold = imag_threshold
            ) %>%
            group_by(sim) %>%
            do(
                suppressWarnings(
                    balance_function(
                        amplitude_activ = amplitude_activ,
                        peak_time_activ = peak_time_activ,
                        curvature_activ = curvature_activ,
                        amplitude_inhib = amplitude_inhib,
                        peak_time_inhib = peak_time_inhib,
                        curvature_inhib = curvature_inhib,
                        exec_threshold = exec_threshold,
                        imag_threshold = imag_threshold
                        )
                    )
                ) %>%
            ungroup()
    
    }
    
    # returning the results
    return (results)
    
}

###########################################################################
# Example: simulating patterns of activation/inhibition and
# distributions of RTs and MTs (in seconds)
###################################################################

# simulation_results <- model(
#     nsims = 100, nsamples = 2000,
#     exec_threshold = 1, imag_threshold = 0.5,
#     amplitude_activ = 1.5, peak_time_activ = log(0.5), curvature_activ = 0.26,
#     amplitude_inhib = 1.75, peak_time_inhib = log(0.5), curvature_inhib = 0.5,
#     model_version = "tmm", full_output = TRUE
#     )
# 
# p1 <- simulation_results %>%
#     # pivot_longer(cols = activation:balance) %>%
#     pivot_longer(cols = activation) %>%
#     ggplot(
#         aes(
#             x = time, y = value,
#             group = interaction(sim, name),
#             colour = name
#             )
#         ) +
#     # plotting the motor execution and motor imagery thresholds
#     geom_labelhline(
#         yintercept = 1, linetype = 2,
#         hjust = 0.9,
#         label = "Motor execution threshold"
#         ) +
#     geom_labelhline(
#         yintercept = 0.5, linetype = 2,
#         hjust = 0.9,
#         label = "Motor imagery threshold"
#         ) +
#     # plotting some individual simulations
#     geom_line(
#         data = . %>% filter(sim %in% unique(sim)[1:50]),
#         linewidth = 0.5, alpha = 0.5, colour = "grey",
#         show.legend = FALSE
#         ) +
#     # plotting average
#     stat_summary(
#         aes(group = name, colour = name),
#         fun = "median", geom = "line",
#         colour = "black",
#         linewidth = 1, alpha = 1,
#         show.legend = TRUE
#         ) +
#     theme_bw(base_size = 12, base_family = "Open Sans") +
#     # scale_fill_manual(values =  met.brewer(name = "Hiroshige", n = 3) ) +
#     # scale_colour_manual(values = met.brewer(name = "Hiroshige", n = 3) ) +
#     labs(
#         # title = "Simulating activation/inhibition patterns",
#         title = "Simulating activation patterns",
#         x = "Time within a trial (in seconds)",
#         # y = "Activation/inhibition (a.u.)",
#         y = "Activation (a.u.)",
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
#     filename = "figures/model_output_tmm.png",
#     width = 16, height = 8, dpi = 300,
#     device = "png"
#     )

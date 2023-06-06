######################################################
# Shiny app to explore the model of motor inhibition #
# during motor imagery                               #
# -------------------------------------------------- #
# Written by Ladislas Nalborczyk                     #
# E-mail: ladislas.nalborczyk@gmail.com              #
# Last update: May 18, 2023                          #
######################################################

library(shinyhelper)
library(tidyverse)
library(patchwork)
library(MetBrewer)
library(shiny)
library(bslib)

# deploying the app
# source(file = "shiny_app/deploying.R")

# user interface
ui <- navbarPage(
        
        # defines a title
        title = "Modelling the inhibitory mechanisms involved in motor imagery",
    
        # sets a theme
        # theme = bs_theme(),
        theme = bs_theme(bootswatch = "united"),
        
        tabPanel(

            title = "Threshold modulation model",
            
            fluidPage(
                
                theme = bs_theme(bootswatch = "united"),
                
                HTML(paste(
                "Short summary: A large body of behavioural, electrophysiological, and neuroimaging
                empirical evidence suggests that the motor system is involved during
                motor imagery. This raises the 'problem of inhibition of execution':
                Given the role of the motor system in providing the multisensory content
                of motor imagery, how is it possible for motor imagery not to lead
                to motor execution? It has been proposed that this may be achieved
                by modulating (e.g., upregulating) the execution threshold.
                Alternatively, this may be achieved by parallel inhibitory processes
                preventing execution during motor imagery. We propose a model of
                the interplay between excitatory and inhibitory processes during
                motor imagery to disentangle the respective predictions of these
                propositions. Activation and inhibition curves are modelled as a
                scaled lognormal function (basically an unnormalised and rescaled
                lognormal distribution), which is assumed to reflect the distribution
                of firing rates across a population of excitatory and inhibitory
                neurons (which is often found to be approximately lognormal, see
                for instance <a href='https://www.nature.com/articles/nrn3687'>
                this paper</a> or <a href='https://www.cell.com/neuron/fulltext/S0896-6273(11)00879-8'>
                this one</a>). The overall model structure
                is adapted from horse-race models of activation/inhibition patterns
                during response inhibition (e.g., <a href='https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0169320'>
                this paper</a>). One important difference
                with these models, however, is that in the present model,
                the 'race' does not stop at the execution threshold, but the
                competition (the balance) between activation and inhibition is
                modelled throughout the entire trial to account for both reaction
                times (i.e., the time it takes to prepare and initiate execution/imagery)
                and movement times (i.e., the time it takes to execute/imagine an action).
                Code available via <a href='https://github.com/lnalborczyk/motor_imagery_inhibition_model'>
                Github</a>."
                ) ),
                
                br(),
                br(),
                
                sidebarLayout(
                    sidebarPanel(
                        width = 4,
                        sliderInput(
                            inputId = "alpha",
                            label = "Amplitude (maximum value)",
                            pre = "<i>&alpha;</i> = ",
                            min = 0, max = 2, value = 1.5, step = 0.01
                            ),
                        sliderInput(
                            inputId = "beta",
                            label = "Peak time (log(time) at maximum)",
                            pre = "<i>&beta;</i> = ",
                            min = -1, max = 1, value = 0, step = 0.01
                            ),
                        sliderInput(
                            inputId = "lambda",
                            label = "Width or curvature",
                            pre = "<i>&lambda;</i> = ",
                            min = 0, max = 2, value = 0.2, step = 0.01
                            ),
                        sliderInput(
                            inputId = "exec_threshold_tmm",
                            label = "Motor execution threshold",
                            min = 0, max = 2, step = 0.01, value = 1
                            ),
                        sliderInput(
                            inputId = "imag_threshold_tmm",
                            label = "Motor imagery threshold (relative to exec_threshold) (usually fixed)",
                            min = 0, max = 1, step = 0.01, value = 0.5
                            )
                        ),
            
                    # showing a plot of the generated distribution
                    mainPanel(plotOutput(outputId = "distPlot1", width = "80%", height = "1000px") ),
                    
                    )
                ),
            
            # includes a footer
            hr(),
            HTML(
                paste0(
                    "Written by <a href='https://lnalborczyk.github.io'>
                    Ladislas Nalborczyk</a>. Last update: ", format(Sys.time(), '%d %B, %Y'), "."
                    )
                )
            
            ),
        
        tabPanel(
            
            title = "Parallel inhibition model",
            
            fluidPage(
                
                theme = bs_theme(bootswatch = "united"),
                
                sidebarLayout(
                    sidebarPanel(
                        width = 4,
                        sliderInput(
                            inputId = "activation_alpha",
                            label = "Amplitude of the activation curve (usually fixed)",
                            pre = "<i>&alpha;</i> = ",
                            min = 0, max = 2, value = 1.5, step = 0.01
                            ),
                        sliderInput(
                            inputId = "activation_beta",
                            label = "Peak time of the activation curve",
                            pre = "<i>&beta;</i> = ",
                            min = -1, max = 1, value = 0, step = 0.01
                            ),
                        sliderInput(
                            inputId = "activation_lambda",
                            label = "Curvature of the activation curve (usually fixed)",
                            pre = "<i>&lambda;</i> = ",
                            min = 0, max = 2, value = 0.2, step = 0.01
                            ),
                        sliderInput(
                            inputId = "inhibition_alpha",
                            label = "Amplitude of the inhibition curve",
                            pre = "<i>&alpha;</i> = ",
                            min = 0, max = 2, value = 1.5, step = 0.01
                            ),
                        sliderInput(
                            inputId = "inhibition_beta",
                            label = "Peak time of the inhibition curve",
                            pre = "<i>&beta;</i> = ",
                            min = -1, max = 1, value = 0, step = 0.01
                            ),
                        sliderInput(
                            inputId = "inhibition_lambda",
                            label = "Curvature of the inhibition curve",
                            pre = "<i>&lambda;</i> = ",
                            min = 0, max = 2, value = 0.4, step = 0.01
                            ),
                        sliderInput(
                            inputId = "exec_threshold",
                            label = "Motor execution threshold (usually fixed)",
                            min = 0, max = 2, step = 0.01, value = 1
                            ),
                        sliderInput(
                            inputId = "imag_threshold",
                            label = "Motor imagery threshold (relative to exec_threshold) (usually fixed)",
                            min = 0, max = 1, step = 0.01, value = 0.5
                            )
                        ),
                    
                    # showing a plot of the generated distribution
                    mainPanel(plotOutput(outputId = "distPlot2", width = "80%", height = "1000px") ),
                    
                )
                
            ),
            
            # includes a footer
            hr(),
            HTML(
                paste0(
                    "Written by <a href='https://lnalborczyk.github.io'>
                    Ladislas Nalborczyk</a>. Last update: ", format(Sys.time(), '%d %B, %Y'), "."
                    )
                
                )
            
            )
        
        )

# server logic
server <- function (input, output) {
    
    # interactive theme customisation
    # bs_themer()
    
    # defining the activation function
    activation <- function (time = 0, amplitude = 1.5, peak_time = 0.5, curvature = 0.8) {
        
        activ <- amplitude * exp(-(log(time) - peak_time)^2 / (2 * curvature^2) )
        
        return (activ)
        
    }
    
    # defining the inhibition function
    inhibition <- function (time = 0, amplitude = 2, peak_time = 0.5, curvature = 1.2) {
        
        inhib <- amplitude * exp(-(log(time) - peak_time)^2 / (2 * curvature^2) )
        
        return (inhib)
        
    }
    
    # computing the balance function (activation/inhibition)
    balance <- function (
        time = 0,
        activation_amplitude = 1.5, activation_peak_time = 0.5, activation_curvature = 0.8, 
        inhibition_amplitude = 1.5, inhibition_peak_time = 0.5, inhibition_curvature = 1.2
        ) {
        
        # computing the balance
        balance_output <- activation(time = time, amplitude = activation_amplitude, peak_time = activation_peak_time, curvature = activation_curvature) /
            (inhibition(time = time, amplitude = inhibition_amplitude, peak_time = inhibition_peak_time, curvature = inhibition_curvature)
            )
        
        return (balance_output)
        
    }

    # plotting results for the threshold modulation model
    output$distPlot1 <- renderPlot({
        
        # plotting it
        p1 <- data.frame(x = c(0, 3) ) %>%
            ggplot(aes(x = x) ) +
            geom_hline(yintercept = input$exec_threshold_tmm, linetype = 2) +
            geom_hline(yintercept = input$imag_threshold_tmm * input$exec_threshold_tmm, linetype = 2) +
            stat_function(
                fun = activation,
                color = "darkgreen", linewidth = 1,
                args = list(
                    amplitude = input$alpha,
                    peak_time = input$beta,
                    curvature = input$lambda
                    )
                ) +
            theme_bw(base_size = 14, base_family = "Open Sans") +
            scale_colour_manual(values = met.brewer(name = "Johnson", n = 3) ) +
            labs(
                title = "Simulating activation patterns",
                x = "Time within a trial (in seconds)",
                y = "Activation (a.u.)",
                colour = "",
                fill = ""
                )
        
        p2 <- data.frame(x = seq(0, 3, 0.01) ) %>%
            mutate(
                activation = activation(
                    time = x,
                    amplitude = input$alpha,
                    peak_time = input$beta,
                    curvature = input$lambda
                    ),
                exec_threshold = input$exec_threshold_tmm,
                imag_threshold = input$imag_threshold_tmm
                ) %>%
            mutate(balance = activation) %>%
            mutate(onset = x[which(balance > imag_threshold) %>% first()]) %>%
            mutate(offset = x[which(balance > imag_threshold) %>% last()]) %>%
            mutate(mt = offset - onset) %>%
            ggplot(aes(x = x) ) +
            geom_hline(yintercept = input$imag_threshold_tmm * input$exec_threshold_tmm, linetype = 2) +
            geom_hline(yintercept = input$exec_threshold_tmm, linetype = 2) +
            geom_line(aes(y = balance), col = "darkgreen", linewidth = 1) +
            geom_segment(
                aes(x = 0, y = input$imag_threshold_tmm, xend = unique(onset), yend = input$imag_threshold_tmm),
                arrow = arrow(length = unit(x = 0.3, units = "cm"), ends = "both", type = "closed")
                ) +
            geom_segment(
                aes(x = unique(onset), y = input$imag_threshold_tmm, xend = unique(offset), yend = input$imag_threshold_tmm),
                arrow = arrow(length = unit(x = 0.3, units = "cm"), ends = "both", type = "closed")
                ) +
            geom_text(
                aes(x = unique(onset) + unique(mt) / 2, y = input$imag_threshold_tmm + 0.1, label = unique(mt) )
                ) +
            geom_text(
                aes(x = unique(onset) / 2, y = input$imag_threshold_tmm + 0.1, label = unique(onset) )
                ) +
            coord_cartesian(ylim = c(0, 2) ) +
            theme_bw(base_size = 14, base_family = "Open Sans") +
            scale_colour_manual(values = met.brewer(name = "Johnson", n = 3) ) +
            labs(
                title = "Simulating average RTs and MTs in imagined trials",
                x = "Time within a trial (in seconds)",
                y = "Activation (a.u.)",
                colour = "",
                fill = ""
                )
        
        p3 <- data.frame(x = seq(0, 3, 0.01) ) %>%
            mutate(
                activation = activation(
                    time = x,
                    amplitude = input$alpha,
                    peak_time = input$beta,
                    curvature = input$lambda
                    ),
                exec_threshold = input$exec_threshold_tmm,
                imag_threshold = input$imag_threshold_tmm
                ) %>%
            mutate(balance = activation) %>%
            mutate(onset = x[which(balance > exec_threshold) %>% first()]) %>%
            mutate(offset = x[which(balance > exec_threshold) %>% last()]) %>%
            mutate(mt = offset - onset) %>%
            ggplot(aes(x = x) ) +
            geom_hline(yintercept = input$imag_threshold_tmm * input$exec_threshold_tmm, linetype = 2) +
            geom_hline(yintercept = input$exec_threshold_tmm, linetype = 2) +
            geom_line(aes(y = balance), col = "darkgreen", linewidth = 1) +
            geom_segment(
                aes(x = 0, y = input$exec_threshold_tmm, xend = unique(onset), yend = input$exec_threshold_tmm),
                arrow = arrow(length = unit(x = 0.3, units = "cm"), ends = "both", type = "closed")
                ) +
            geom_segment(
                aes(x = unique(onset), y = input$exec_threshold_tmm, xend = unique(offset), yend = input$exec_threshold_tmm),
                arrow = arrow(length = unit(x = 0.3, units = "cm"), ends = "both", type = "closed")
                ) +
            geom_text(
                aes(x = unique(onset) + unique(mt) / 2, y = input$exec_threshold_tmm + 0.1, label = unique(mt) )
                ) +
            geom_text(
                aes(x = unique(onset) / 2, y = input$exec_threshold_tmm + 0.1, label = unique(onset) )
                ) +
            coord_cartesian(ylim = c(0, 2) ) +
            theme_bw(base_size = 14, base_family = "Open Sans") +
            scale_colour_manual(values = met.brewer(name = "Johnson", n = 3) ) +
            labs(
                title = "Simulating average RTs and MTs in executed trials",
                x = "Time within a trial (in seconds)",
                y = "Activation (a.u.)",
                colour = "",
                fill = ""
                )
        
        # combining the plots
        p1 / p2 / p3
        
    })
    
    # plotting results for the parallel inhibition model
    output$distPlot2 <- renderPlot({
        
        # plotting it
        p1 <- data.frame(x = c(0, 3) ) %>%
            ggplot(aes(x = x) ) +
            geom_hline(yintercept = input$exec_threshold, linetype = 2) +
            geom_hline(yintercept = input$imag_threshold * input$exec_threshold, linetype = 2) +
            stat_function(
                fun = activation,
                color = "darkgreen", linewidth = 1,
                args = list(
                    amplitude = input$activation_alpha,
                    peak_time = input$activation_beta,
                    curvature = input$activation_lambda
                    )
                ) +
            stat_function(
                fun = inhibition,
                color = "orangered", linewidth = 1,
                args = list(
                    amplitude = input$inhibition_alpha,
                    peak_time = input$inhibition_beta,
                    curvature = input$inhibition_lambda
                    )
                ) +
            stat_function(
                fun = balance,
                color = "steelblue", linewidth = 1,
                args = list(
                    activation_amplitude = input$activation_alpha,
                    activation_peak_time = input$activation_beta,
                    activation_curvature = input$activation_lambda,
                    inhibition_amplitude = input$inhibition_alpha,
                    inhibition_peak_time = input$inhibition_beta,
                    inhibition_curvature = input$inhibition_lambda
                    )
                ) +
            theme_bw(base_size = 14, base_family = "Open Sans") +
            scale_colour_manual(values = met.brewer(name = "Johnson", n = 3) ) +
            labs(
                title = "Simulating activation/inhibition patterns",
                subtitle = "The balance function is defined as activation/inhibition",
                x = "Time within a trial (in seconds)",
                y = "Activation/inhibition (a.u.)",
                colour = "",
                fill = ""
                )
        
        p2 <- data.frame(x = seq(0, 3, 0.01) ) %>%
            mutate(
                activation = activation(
                    time = x,
                    amplitude = input$activation_alpha,
                    peak_time = input$activation_beta,
                    curvature = input$activation_lambda
                    ),
                inhibition = inhibition(
                    time = x,
                    amplitude = input$inhibition_alpha,
                    peak_time = input$inhibition_beta,
                    curvature = input$inhibition_lambda
                    ),
                exec_threshold = input$exec_threshold,
                imag_threshold = input$imag_threshold * input$exec_threshold
                ) %>%
            mutate(balance = activation / inhibition) %>%
            mutate(onset = x[which(balance > imag_threshold) %>% first()]) %>%
            mutate(offset = x[which(balance > imag_threshold) %>% last()]) %>%
            mutate(mt = offset - onset) %>%
            ggplot(aes(x = x) ) +
            geom_hline(yintercept = input$imag_threshold * input$exec_threshold, linetype = 2) +
            geom_hline(yintercept = input$exec_threshold, linetype = 2) +
            geom_line(aes(y = balance), col = "steelblue", linewidth = 1) +
            geom_segment(
                aes(x = 0, y = input$imag_threshold * input$exec_threshold, xend = unique(onset), yend = input$imag_threshold * input$exec_threshold),
                arrow = arrow(length = unit(x = 0.3, units = "cm"), ends = "both", type = "closed")
                ) +
            geom_segment(
                aes(x = unique(onset), y = input$imag_threshold * input$exec_threshold, xend = unique(offset), yend = input$imag_threshold * input$exec_threshold),
                arrow = arrow(length = unit(x = 0.3, units = "cm"), ends = "both", type = "closed")
                ) +
            geom_text(
                aes(x = unique(onset) + unique(mt) / 2, y = input$imag_threshold * input$exec_threshold + 0.1, label = unique(mt) )
                ) +
            geom_text(
                aes(x = unique(onset) / 2, y = input$imag_threshold * input$exec_threshold+ 0.1, label = unique(onset) )
                ) +
            coord_cartesian(ylim = c(0, 2) ) +
            theme_bw(base_size = 14, base_family = "Open Sans") +
            scale_colour_manual(values = met.brewer(name = "Johnson", n = 3) ) +
            labs(
                title = "Simulating average RTs and MTs in imagined trials",
                x = "Time within a trial (in seconds)",
                y = "Activation/inhibition (a.u.)",
                colour = "",
                fill = ""
                )
        
        p3 <- data.frame(x = seq(0, 3, 0.01) ) %>%
            mutate(
                activation = activation(
                    time = x,
                    amplitude = input$activation_alpha,
                    peak_time = input$activation_beta,
                    curvature = input$activation_lambda
                    ),
                inhibition = inhibition(
                    time = x,
                    amplitude = input$inhibition_alpha,
                    peak_time = input$inhibition_beta,
                    curvature = input$inhibition_lambda
                    ),
                exec_threshold = input$exec_threshold,
                imag_threshold = input$imag_threshold * input$exec_threshold
                ) %>%
            mutate(balance = activation / inhibition) %>%
            mutate(onset = x[which(balance > exec_threshold) %>% first()]) %>%
            mutate(offset = x[which(balance > exec_threshold) %>% last()]) %>%
            mutate(mt = offset - onset) %>%
            ggplot(aes(x = x) ) +
            geom_hline(yintercept = input$imag_threshold * input$exec_threshold, linetype = 2) +
            geom_hline(yintercept = input$exec_threshold, linetype = 2) +
            geom_line(aes(y = balance), col = "steelblue", linewidth = 1) +
            geom_segment(
                aes(x = 0, y = input$exec_threshold, xend = unique(onset), yend = input$exec_threshold),
                arrow = arrow(length = unit(x = 0.3, units = "cm"), ends = "both", type = "closed")
                ) +
            geom_segment(
                aes(x = unique(onset), y = input$exec_threshold, xend = unique(offset), yend = input$exec_threshold),
                arrow = arrow(length = unit(x = 0.3, units = "cm"), ends = "both", type = "closed")
                ) +
            geom_text(
                aes(x = unique(onset) + unique(mt) / 2, y = input$exec_threshold + 0.1, label = unique(mt) )
                ) +
            geom_text(
                aes(x = unique(onset) / 2, y = input$exec_threshold + 0.1, label = unique(onset) )
                ) +
            coord_cartesian(ylim = c(0, 2) ) +
            theme_bw(base_size = 14, base_family = "Open Sans") +
            scale_colour_manual(values = met.brewer(name = "Johnson", n = 3) ) +
            labs(
                title = "Simulating average RTs and MTs in executed trials",
                x = "Time within a trial (in seconds)",
                y = "Activation/inhibition (a.u.)",
                colour = "",
                fill = ""
                )
        
        # combining the plots
        p1 / p2 / p3
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

####################################################################
# Shiny app to explore the model of activation/inhibition patterns #
# during motor imagery and action-mode switching paradigms         #
# ---------------------------------------------------------------- #
# Written by Ladislas Nalborczyk                                   #
# E-mail: ladislas.nalborczyk@gmail.com                            #
# Last update: March 28, 2023                                      #
####################################################################

library(shinyhelper)
library(tidyverse)
library(patchwork)
library(MetBrewer)
library(shiny)
library(bslib)

# user interface
ui <- fluidPage(

    # defines a title
    titlePanel(title = "Simulating activation/inhibition patterns"),
    
    # sets a theme
    # theme = bs_theme(),
    theme = bs_theme(bootswatch = "united"),
    
    # Sidebar panel with slider inputs
    sidebarLayout(
        # Sliders
        sidebarPanel(
            width = 4,
            sliderInput(
                inputId = "activation_alpha",
                label = "Amplitude of the activation curve",
                pre = "<i>&alpha;</i> = ",
                min = 0, max = 2, value = 1.5, step = 0.01
                ),
            sliderInput(
                inputId = "activation_beta",
                label = "Peak time of the activation curve",
                pre = "<i>&beta;</i> = ",
                min = 0, max = 2, value = 0.5, step = 0.01
                ),
            sliderInput(
                inputId = "activation_lambda",
                label = "Curvature of the activation curve",
                pre = "<i>&lambda;</i> = ",
                min = 0, max = 2, value = 0.4, step = 0.01
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
                min = 0, max = 2, value = 0.5, step = 0.01
                ),
            sliderInput(
                inputId = "inhibition_lambda",
                label = "Curvature of the inhibition curve",
                pre = "<i>&lambda;</i> = ",
                min = 0, max = 2, value = 0.6, step = 0.01
                ),
            sliderInput(
                inputId = "inhibition_previous_alpha",
                label = "Amplitude of the inhibition curve (in the previous trial)",
                pre = "<i>&alpha;</i> = ",
                min = 0, max = 2, value = 1.5, step = 0.01
                ),
            sliderInput(
                inputId = "inhibition_previous_beta",
                label = "Peak time of the inhibition curve (in the previous trial)",
                pre = "<i>&beta;</i> = ",
                min = 0, max = 2, value = 1, step = 0.01
                ),
            sliderInput(
                inputId = "inhibition_previous_lambda",
                label = "Curvature of the inhibition curve (in the previous trial)",
                pre = "<i>&lambda;</i> = ",
                min = 0, max = 2, value = 0.6, step = 0.01
                ),
            sliderInput(
                inputId = "delay",
                label = "Pick a delay between two successive trials (in seconds)",
                # pre = "<i>&lambda;</i> = ",
                min = 0, max = 10, step = 0.1, value = 5
                ),
            sliderInput(
                inputId = "exec_threshold",
                label = "Pick a value for the execution threshold",
                # pre = "<i>&lambda;</i> = ",
                min = 0, max = 2, step = 0.01, value = 1
                ),
            sliderInput(
                inputId = "imag_threshold",
                label = "Pick a value for the imagery threshold",
                # pre = "<i>&lambda;</i> = ",
                min = 0, max = 2, step = 0.01, value = 0.5
                )
            ),

        # Show a plot of the generated distribution
        mainPanel(plotOutput(outputId = "distPlot", width = "80%", height = "1000px") )
        
        )
    
    )

# server logic
server <- function(input, output) {
    
    # interactive theme customisation
    # bs_themer()

    output$distPlot <- renderPlot({
        
        # defining the time scaling factor
        timescale <- 5
        
        # defining the activation function for the current trial
        activation <- function (time = 0, amplitude = 1.5, peak_time = 0.5, curvature = 0.8) {
            
            activ <- amplitude * exp(-(log(time * timescale) - peak_time)^2 / (2 * curvature^2) )
            
            return (activ)
            
        }
        
        # defining the activation function for the previous trials
        activation_previous <- function (time = 0, amplitude = 1.5, peak_time = 0.5, curvature = 0.8, delay = 10) {
            
            activ <- amplitude * exp(-(log(delay + time * timescale) - peak_time)^2 / (2 * curvature^2) )
            
            return (activ)
            
        }
        
        # defining the inhibition function for the current trial
        inhibition <- function (time = 0, amplitude = 2, peak_time = 0.5, curvature = 1.2) {
            
            inhib <- amplitude * exp(-(log(time * timescale) - peak_time)^2 / (2 * curvature^2) )
            
            return (inhib)
            
        }
        
        inhibition_previous <- function (time = 0, amplitude = 2, peak_time = 0.5, curvature = 1.2, delay = 10) {
            
            inhib <- amplitude * exp(-(log(delay + time * timescale) - peak_time)^2 / (2 * curvature^2) )
            
            return (inhib)
            
        }
        
        # computing the balance (activation/inhibition)
        balance <- function (
            time = 0, delay = 10,
            activation_amplitude = 1.5, activation_peak_time = 0.5, activation_curvature = 0.8, 
            inhibition_amplitude = 1.5, inhibition_peak_time = 0.5, inhibition_curvature = 1.2,
            inhibition_previous_amplitude = 1.5, inhibition_previous_peak_time = 0.5, inhibition_previous_curvature = 1.2
            ) {
            
            # computing the balance
            balance_output <- activation(time = time, amplitude = activation_amplitude, peak_time = activation_peak_time, curvature = activation_curvature) /
                (inhibition(time = time, amplitude = inhibition_amplitude, peak_time = inhibition_peak_time, curvature = inhibition_curvature)
                     # inhibition_previous(time = time, amplitude = inhibition_previous_amplitude, peak_time = inhibition_previous_peak_time, curvature = inhibition_previous_curvature, delay = delay)
                     # balance_previous(time = time, inhibition_previous_amplitude = inhibition_previous_amplitude, inhibition_previous_peak_time = inhibition_previous_peak_time, inhibition_previous_curvature = inhibition_previous_curvature, delay = delay)
                 )
            
            return (balance_output)
            
        }
        
        # computing the balance (activation/inhibition) from the previous trial
        balance_previous <- function (
            time = 0, delay = 10,
            activation_amplitude = 1.5, activation_peak_time = 0.5, activation_curvature = 0.4, 
            inhibition_amplitude = 1.5, inhibition_peak_time = 0.5, inhibition_curvature = 0.6,
            inhibition_previous_amplitude = 1.5, inhibition_previous_peak_time = 0.5, inhibition_previous_curvature = 0.6
            ) {
            
            # computing the balance
            balance_output <- activation_previous(time = time, amplitude = activation_amplitude, peak_time = activation_peak_time, curvature = activation_curvature, delay = delay) /
                inhibition_previous(time = time, amplitude = inhibition_previous_amplitude, peak_time = inhibition_previous_peak_time, curvature = inhibition_previous_curvature, delay = delay)
            
            return (balance_output)
            
        }
        
        # computing the amplitude of inhibition_previous at t = 0
        # t0_inhibition_previous <- inhibition_previous(
        #     time = 0,
        #     amplitude = input$inhibition_previous_alpha,
        #     peak_time = input$inhibition_previous_beta,
        #     curvature = input$inhibition_previous_lambda,
        #     delay = input$delay
        #     )
            
        # plotting it
        p1 <- data.frame(x = c(0, 2) ) %>%
            ggplot(aes(x = x) ) +
            geom_hline(yintercept = input$exec_threshold, linetype = 2) +
            geom_hline(yintercept = input$imag_threshold, linetype = 2) +
            stat_function(
                fun = activation,
                color = "darkgreen", linewidth = 1,
                args = list(
                    amplitude = input$activation_alpha,
                    peak_time = input$activation_beta,
                    # peak_time = log(ifelse(input$activation_beta == 0, input$activation_beta + 1e-6, input$activation_beta) ),
                    curvature = input$activation_lambda
                    )
                ) +
            stat_function(
                fun = inhibition,
                color = "orangered", linewidth = 1,
                args = list(
                    amplitude = input$inhibition_alpha,
                    peak_time = input$inhibition_beta,
                    # peak_time = log(ifelse(input$inhibition_beta == 0, input$inhibition_beta + 1e-6, input$inhibition_beta) ),
                    curvature = input$inhibition_lambda
                    )
                ) +
            stat_function(
                fun = inhibition_previous,
                color = "orangered", linewidth = 1, lty = 2,
                args = list(
                    amplitude = input$inhibition_previous_alpha,
                    peak_time = input$inhibition_previous_beta,
                    # peak_time = log(ifelse(input$inhibition_previous_beta == 0, input$inhibition_previous_beta + 1e-6, input$inhibition_previous_beta) ),
                    curvature = input$inhibition_previous_lambda,
                    delay = input$delay
                    )
                ) +
            stat_function(
                fun = balance,
                color = "steelblue", linewidth = 1,
                args = list(
                    activation_amplitude = input$activation_alpha,
                    activation_peak_time = input$activation_beta,
                    # activation_peak_time = log(ifelse(input$activation_peak_time == 0, input$activation_peak_time + 1e-6, input$activation_peak_time) ),
                    activation_curvature = input$activation_lambda,
                    inhibition_amplitude = input$inhibition_alpha,
                    inhibition_peak_time = input$inhibition_beta,
                    # inhibition_peak_time = log(ifelse(input$inhibition_peak_time == 0, input$inhibition_peak_time + 1e-6, input$inhibition_peak_time) ),
                    inhibition_curvature = input$inhibition_lambda,
                    inhibition_previous_amplitude = input$inhibition_previous_alpha,
                    inhibition_previous_peak_time = input$inhibition_previous_beta,
                    # inhibition_previous_peak_time = log(ifelse(input$inhibition_previous_peak_time == 0, input$inhibition_previous_peak_time + 1e-6, input$inhibition_previous_peak_time) ),
                    inhibition_previous_curvature = input$inhibition_previous_lambda,
                    delay = input$delay
                    )
                ) +
            # stat_function(
            #     fun = balance_previous,
            #     color = "steelblue", linewidth = 1, lty = 2,
            #     args = list(
            #         # activation_amplitude = input$activation_alpha,
            #         # activation_peak_time = input$activation_beta,
            #         # activation_curvature = input$activation_lambda,
            #         # inhibition_amplitude = input$inhibition_alpha,
            #         # inhibition_peak_time = input$inhibition_beta,
            #         # inhibition_curvature = input$inhibition_lambda,
            #         inhibition_previous_amplitude = input$inhibition_previous_alpha,
            #         inhibition_previous_peak_time = input$inhibition_previous_beta,
            #         inhibition_previous_curvature = input$inhibition_previous_lambda,
            #         delay = input$delay
            #         )
            #     ) +
            theme_bw(base_size = 14, base_family = "Open Sans") +
            scale_colour_manual(values = met.brewer(name = "Johnson", n = 3) ) +
            labs(
                title = "Simulating activation/inhibition patterns",
                subtitle = "Activation/inhibition balance is defined as activation_current / (inhibition_current + inhibition_previous)",
                x = "Time within a trial (in seconds)",
                y = "Activation/inhibition (a.u.)",
                colour = "",
                fill = ""
                )
        
        p2 <- data.frame(x = seq(0, 2, 0.01) ) %>%
            mutate(
                activation = activation(
                    time = x,
                    amplitude = input$activation_alpha,
                    peak_time = input$activation_beta,
                    # peak_time = log(ifelse(input$activation_beta == 0, input$activation_beta + 1e-6, input$activation_beta) ),
                    curvature = input$activation_lambda
                    ),
                inhibition = inhibition(
                    time = x,
                    amplitude = input$inhibition_alpha,
                    peak_time = input$inhibition_beta,
                    # peak_time = log(ifelse(input$inhibition_beta == 0, input$inhibition_beta + 1e-6, input$inhibition_beta) ),
                    curvature = input$inhibition_lambda
                    ),
                inhibition_previous = inhibition_previous(
                    time = x,
                    amplitude = input$inhibition_previous_alpha,
                    peak_time = input$inhibition_previous_beta,
                    # peak_time = log(ifelse(input$inhibition_previous_beta == 0, input$inhibition_previous_beta + 1e-6, input$inhibition_previous_beta) ),
                    curvature = input$inhibition_previous_lambda,
                    delay = input$delay
                    ),
                exec_threshold = input$exec_threshold,
                imag_threshold = input$imag_threshold
                ) %>%
            # mutate(balance = activation / (inhibition + inhibition_previous) ) %>%
            mutate(balance = activation / inhibition) %>%
            mutate(onset = x[which(balance > imag_threshold) %>% first()]) %>%
            mutate(offset = x[which(balance > imag_threshold) %>% last()]) %>%
            mutate(mt = offset - onset) %>%
            ggplot(aes(x = x) ) +
            geom_hline(yintercept = input$imag_threshold, linetype = 2) +
            geom_line(aes(y = balance), col = "steelblue", linewidth = 1) +
            geom_segment(
                aes(x = 0, y = input$imag_threshold, xend = unique(onset), yend = input$imag_threshold),
                arrow = arrow(length = unit(x = 0.3, units = "cm"), ends = "both", type = "closed")
                ) +
            geom_segment(
                aes(x = unique(onset), y = input$imag_threshold, xend = unique(offset), yend = input$imag_threshold),
                arrow = arrow(length = unit(x = 0.3, units = "cm"), ends = "both", type = "closed")
                ) +
            geom_text(
                aes(x = unique(onset) + unique(mt) / 2, y = input$imag_threshold + 0.1, label = unique(mt) )
                ) +
            geom_text(
                aes(x = unique(onset) / 2, y = input$imag_threshold + 0.1, label = unique(onset) )
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
        
        p3 <- data.frame(x = seq(0, 2, 0.01) ) %>%
            mutate(
                activation = activation(
                    time = x,
                    amplitude = input$activation_alpha,
                    peak_time = input$activation_beta,
                    # peak_time = log(ifelse(input$activation_beta == 0, input$activation_beta + 1e-6, input$activation_beta) ),
                    curvature = input$activation_lambda
                    ),
                inhibition = inhibition(
                    time = x,
                    amplitude = input$inhibition_alpha,
                    peak_time = input$inhibition_beta,
                    # peak_time = log(ifelse(input$inhibition_beta == 0, input$inhibition_beta + 1e-6, input$inhibition_beta) ),
                    curvature = input$inhibition_lambda
                    ),
                inhibition_previous = inhibition_previous(
                    time = x,
                    amplitude = input$inhibition_previous_alpha,
                    peak_time = input$inhibition_previous_beta,
                    # peak_time = log(ifelse(input$inhibition_previous_beta == 0, input$inhibition_previous_beta + 1e-6, input$inhibition_previous_beta) ),
                    curvature = input$inhibition_previous_lambda,
                    delay = input$delay
                    ),
                exec_threshold = input$exec_threshold,
                imag_threshold = input$imag_threshold
                ) %>%
            # mutate(balance = activation / (inhibition + inhibition_previous) ) %>%
            mutate(balance = activation / inhibition) %>%
            mutate(onset = x[which(balance > exec_threshold) %>% first()]) %>%
            mutate(offset = x[which(balance > exec_threshold) %>% last()]) %>%
            mutate(mt = offset - onset) %>%
            ggplot(aes(x = x) ) +
            geom_hline(yintercept = input$imag_threshold, linetype = 2) +
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

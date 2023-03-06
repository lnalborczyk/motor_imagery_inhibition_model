#########################################################################
# Modelling the impact of activation/inhibition patterns on RTs and MTs #
# during motor imagery and action-mode switching paradigms              #
# OSF project: xxxx                                                     #
# --------------------------------------------------------------------- #
# Written by Ladislas Nalborczyk                                        #
# E-mail: ladislas.nalborczyk@gmail.com                                 #
# Last update: March 6, 2023                                            #
#########################################################################

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
    theme = bs_theme(version = 4, bootswatch = "minty"),

    # fluidRow(p("Important note: this application is meant to facilitate the creation of R scripts to automate sequential testing procedures. More specifically, it automatically writes around 90% of the code the user would have to write to use such a procedure. However, it is almost certain that the produced R code will not work immediately. It will require some minor tweakings from the user, such as checking the local path, making sure that the scripts and the data are in the same repository, adapting the data import step to specific properties of the data under consideration, and so on. For more information, please have a look at our tutorial paper.") ),
    # fluidRow(h3("Step 1: Create an automation") ),
    
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
                min = 0, max = 2, value = 0.5, step = 0.01
                ),
            sliderInput(
                inputId = "inhibition_previous_lambda",
                label = "Curvature of the inhibition curve (in the previous trial)",
                pre = "<i>&lambda;</i> = ",
                min = 0, max = 2, value = 0.6, step = 0.01
                ),
            sliderInput(
                inputId = "delay",
                label = "Pick a delay between two successive trials",
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
    
    # bs_themer()

    output$distPlot <- renderPlot({
        
        # defining the activation function for the current trial
        # https://en.wikipedia.org/wiki/Gaussian_function
        activation <- function (time = 0, amplitude = 1.5, peak_time = 0.5, curvature = 0.8) {
            
            activ <- amplitude * exp(-(log(time) - peak_time)^2 / (2 * curvature^2) )
            
            return (activ)
            
        }
        
        # defining the activation function for the previous trials
        activation_previous <- function (time = 0, amplitude = 1.5, peak_time = 0.5, curvature = 0.8, delay = 10) {
            
            activ <- amplitude * exp(-(log(delay + time) - peak_time)^2 / (2 * curvature^2) )
            
            return (activ)
            
        }
        
        # defining the inhibition function for the current trial
        inhibition <- function (time = 0, amplitude = 2, peak_time = 0.5, curvature = 1.2) {
            
            inhib <- amplitude * exp(-(log(time) - peak_time)^2 / (2 * curvature^2) )
            
            return (inhib)
            
        }
        
        inhibition_previous <- function (time = 0, amplitude = 2, peak_time = 0.5, curvature = 1.2, delay = 10) {
            
            inhib <- amplitude * exp(-(log(delay + time) - peak_time)^2 / (2 * curvature^2) )
            
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
                (inhibition(time = time, amplitude = inhibition_amplitude, peak_time = inhibition_peak_time, curvature = inhibition_curvature) +
                     inhibition_previous(time = time, amplitude = inhibition_previous_amplitude, peak_time = inhibition_previous_peak_time, curvature = inhibition_previous_curvature, delay = delay) )
            
            return (balance_output)
            
        }
        
        # computing the balance (activation/inhibition) from the previous trial
        balance_previous <- function (
            time = 0, delay = 10,
            activation_amplitude = 1.5, activation_peak_time = 0.5, activation_curvature = 0.8, 
            inhibition_amplitude = 1.5, inhibition_peak_time = 0.5, inhibition_curvature = 1.2,
            inhibition_previous_amplitude = 1.5, inhibition_previous_peak_time = 0.5, inhibition_previous_curvature = 1.2
            ) {
            
            # computing the balance
            balance_output <- activation_previous(time = time, amplitude = activation_amplitude, peak_time = activation_peak_time, curvature = activation_curvature) /
                inhibition_previous(time = time, amplitude = inhibition_amplitude, peak_time = inhibition_peak_time, curvature = inhibition_curvature)
            
            return (balance_output)
            
        }
        
        # computing the amplitude of inhibition_previous at t = 0
        t0_inhibition_previous <- inhibition_previous(
            time = 0,
            amplitude = input$inhibition_previous_alpha,
            peak_time = input$inhibition_previous_beta,
            curvature = input$inhibition_previous_lambda,
            delay = input$delay
            )
            
        # plotting it
        p1 <- data.frame(x = c(0, 10) ) %>%
            ggplot(aes(x = x) ) +
            geom_hline(yintercept = input$exec_threshold, linetype = 2) +
            geom_hline(yintercept = input$imag_threshold, linetype = 2) +
            stat_function(
                fun = activation,
                color = "darkgreen", linewidth = 1,
                args = list(
                    amplitude = input$activation_alpha,
                    peak_time = input$activation_beta,
                    # peak_time = t0_inhibition_previous,
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
                fun = inhibition_previous,
                color = "orangered", linewidth = 1, lty = 2,
                args = list(
                    amplitude = input$inhibition_previous_alpha,
                    peak_time = input$inhibition_previous_beta,
                    curvature = input$inhibition_previous_lambda,
                    delay = input$delay
                    )
                ) +
            # stat_function(
            #     fun = inhibition_previous, color = "orangered", linewidth = 1, lty = 2,
            #     args = list(amplitude = 1.5)
            #     ) +
            stat_function(
                fun = balance,
                color = "steelblue", linewidth = 1, lty = 2,
                args = list(
                    activation_amplitude = input$activation_alpha,
                    activation_peak_time = input$activation_beta,
                    # activation_peak_time = t0_inhibition_previous,
                    activation_curvature = input$activation_lambda,
                    inhibition_amplitude = input$inhibition_alpha,
                    inhibition_peak_time = input$inhibition_beta,
                    inhibition_curvature = input$inhibition_lambda,
                    inhibition_previous_amplitude = input$inhibition_previous_alpha,
                    inhibition_previous_peak_time = input$inhibition_previous_beta,
                    inhibition_previous_curvature = input$inhibition_previous_lambda,
                    delay = input$delay
                    )
                ) +
            theme_bw(base_size = 14, base_family = "Open Sans") +
            scale_colour_manual(values = met.brewer(name = "Johnson", n = 3) ) +
            labs(
                title = "Simulating activation/inhibition patterns",
                subtitle = "Activation/inhibition balance is defined as activation_current / (inhibition_current + inhibition_previous)",
                x = "Time within a trial (a.u.)",
                y = "Activation/inhibition (a.u.)",
                colour = "",
                fill = ""
                )
        
        p2 <- data.frame(x = seq(0, 10, 0.01) ) %>%
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
                inhibition_previous = inhibition_previous(
                    time = x,
                    amplitude = input$inhibition_previous_alpha,
                    peak_time = input$inhibition_previous_beta,
                    curvature = input$inhibition_previous_lambda,
                    delay = input$delay
                    ),
                exec_threshold = input$exec_threshold,
                imag_threshold = input$imag_threshold
                ) %>%
            mutate(balance = activation / (inhibition + inhibition_previous) ) %>%
            # mutate(balance_square = activation / (inhibition + inhibition_previous)^2) %>%
            mutate(onset = x[which(balance > imag_threshold) %>% first()]) %>%
            mutate(offset = x[which(balance > imag_threshold) %>% last()]) %>%
            mutate(mt = offset - onset) %>%
            ggplot(aes(x = x) ) +
            geom_hline(yintercept = input$imag_threshold, linetype = 2) +
            # geom_hline(yintercept = input$exec_threshold, linetype = 2) +
            geom_line(aes(y = balance), col = "steelblue", linewidth = 1, lty = 2) +
            geom_segment(
                aes(x = 0, y = input$imag_threshold, xend = unique(onset), yend = input$imag_threshold),
                arrow = arrow(length = unit(x = 0.3, units = "cm"), ends = "both", type = "closed")
                ) +
            geom_segment(
                aes(x = unique(onset), y = input$imag_threshold, xend = unique(offset), yend = input$imag_threshold),
                arrow = arrow(length = unit(x = 0.3, units = "cm"), ends = "both", type = "closed")
                ) +
            geom_text(
                aes(x = (unique(onset) + unique(mt) ) / 2, y = input$imag_threshold + 0.1, label = unique(mt) )
                ) +
            geom_text(
                aes(x = unique(onset) / 2, y = input$imag_threshold + 0.1, label = unique(onset) )
                ) +
            # coord_cartesian(xlim = c(0, 20), ylim = c(0, 1) ) +
            theme_bw(base_size = 14, base_family = "Open Sans") +
            scale_colour_manual(values = met.brewer(name = "Johnson", n = 3) ) +
            labs(
                title = "Simulating average RTs and MTs in imagined trials",
                # subtitle = "Activation/inhibition balance is defined as activation_current / (inhibition_current + inhibition_previous)^2",
                x = "Time within a trial (a.u.)",
                y = "Activation/inhibition (a.u.)",
                colour = "",
                fill = ""
                )
        
        p3 <- data.frame(x = seq(0, 10, 0.01) ) %>%
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
                inhibition_previous = inhibition_previous(
                    time = x,
                    amplitude = input$inhibition_previous_alpha,
                    peak_time = input$inhibition_previous_beta,
                    curvature = input$inhibition_previous_lambda,
                    delay = input$delay
                    ),
                exec_threshold = input$exec_threshold,
                imag_threshold = input$imag_threshold
                ) %>%
            mutate(balance = activation / (inhibition + inhibition_previous) ) %>%
            mutate(onset = x[which(balance > exec_threshold) %>% first()]) %>%
            mutate(offset = x[which(balance > exec_threshold) %>% last()]) %>%
            mutate(mt = offset - onset) %>%
            ggplot(aes(x = x) ) +
            geom_hline(yintercept = input$imag_threshold, linetype = 2) +
            geom_hline(yintercept = input$exec_threshold, linetype = 2) +
            geom_line(aes(y = balance), col = "steelblue", linewidth = 1, lty = 2) +
            geom_segment(
                aes(x = 0, y = input$exec_threshold, xend = unique(onset), yend = input$exec_threshold),
                arrow = arrow(length = unit(x = 0.3, units = "cm"), ends = "both", type = "closed")
                ) +
            geom_segment(
                aes(x = unique(onset), y = input$exec_threshold, xend = unique(offset), yend = input$exec_threshold),
                arrow = arrow(length = unit(x = 0.3, units = "cm"), ends = "both", type = "closed")
                ) +
            geom_text(
                aes(x = (unique(onset) + unique(mt) ) / 2, y = input$exec_threshold + 0.1, label = unique(mt) )
                ) +
            geom_text(
                aes(x = unique(onset) / 2, y = input$exec_threshold + 0.1, label = unique(onset) )
                ) +
            # coord_cartesian(xlim = c(0, 20), ylim = c(0, 1) ) +
            theme_bw(base_size = 14, base_family = "Open Sans") +
            scale_colour_manual(values = met.brewer(name = "Johnson", n = 3) ) +
            labs(
                title = "Simulating average RTs and MTs in executed trials",
                # subtitle = "Activation/inhibition balance is defined as activation_current / (inhibition_current + inhibition_previous)^2",
                x = "Time within a trial (a.u.)",
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

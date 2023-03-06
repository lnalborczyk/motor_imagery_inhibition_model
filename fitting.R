######################################################################
# Fitting the model
# ---------------------------------------------------------------
# Written by Ladislas Nalborczyk
# E-mail: ladislas.nalborczyk@gmail.com
# Last updated on March 6, 2023
##############################################################

loss_function <- function (model, data) {
    
    # to be defined
    
}

model_fitting <- function (data, method = c("nlminb", "optim") ) {
    
    if (method == "nlminb") {
        
        fit <- stats::nlminb(
            start = c(1, 1, 0, 0.5, 0),
            objective = model,
            rt = rt, resp = resp,
            lower = c(0.01, -Inf, 0, 0, 0),
            upper = c(Inf, Inf, Inf, 1, Inf)
            )
        
        } else if (method == "optim") {
        
            fit <- stats::optim(
                par = c(1, 1, 0, 0.5, 0),
                fn = model,
                rt = rt, resp = resp,
                method = "Nelder-Mead"
                )
        
        }
    
    return (fit)
    
}

# importing the model
# source(file = "model.R")

# importing the data
# data(some_data)

# fitting the model (pars are a, v, t0, w, sv)
# model_fitting(data = some_data, method = "nlminb")$par

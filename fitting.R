######################################################################
# Fitting the model
# ---------------------------------------------------------------
# Written by Ladislas Nalborczyk
# E-mail: ladislas.nalborczyk@gmail.com
# Last updated on March 6, 2023
##############################################################

model_fitting <- function (rt, resp, method = c("nlminb", "optim") ) {
    
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
# data(self_produced_speech)

# reshaping the data
# df <- self_produced_speech |>
#   # removing the last block
#   filter(block < 7) |>
#   # keeping only the relevant columns
#   select(participant, choice = response, RT) |>
#   mutate(choice = ifelse(test = choice == "stim1", yes = 1, no = 2) )

# fitting the model (pars are a, v, t0, w, sv)
# ddm_fitting(rt = df_ln$RT, resp = df_ln$choice, method = "nlminb")$par

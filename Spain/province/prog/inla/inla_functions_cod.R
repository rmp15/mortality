# functions to enable age group and sex to be selected

# INLA models
#source('../models/INLA/03_spatiotemporal/inla_models_cod.R')

inla.function.climate <- function(){

    # INLA model
    system.time(mod <-
    inla(formula = fml,
    family = "poisson",
    data = dat.inla,
    E = pop_men,
    control.compute = list(config=TRUE, dic=TRUE),
    control.predictor = list(link = 1),
    ))

    return(mod)
}

# functions to enable age group and sex to be selected with rough run to improve speed
inla.function.climate.fast <- function() {

    # INLA model rough
    system.time(mod.rough <-
    inla(formula = fml,
    family = "poisson",
    data = dat.inla,
    E = pop_men,
    control.compute = list(dic=TRUE),
    control.predictor = list(link = 1),
    num.threads = 30, # NEED TO FIX
    control.inla = list(diagonal=10000, int.strategy='eb',strategy='gaussian'),
    ))
    
    # INLA model proper
    system.time(mod <-
    inla(formula = fml,
    family = "poisson",
    data = dat.inla,
    E = pop_men,
    control.compute = list(config=TRUE, dic=TRUE),
    control.predictor = list(link = 1),
    num.threads = 30, # NEED TO FIX
    control.inla=list(diagonal=0),
    control.mode = list(result = mod.rough, restart = TRUE),
    ))

    return(mod)
}

# functions to enable age group and sex to be selected with faster AR1 structure in addition to rough run
inla.function.climate.faster <- function() {
    
    # INLA model rough
    system.time(mod.rough <-
    inla(formula = fml,
    family = "poisson",
    data = dat.inla,
    E = pop_men,
    control.compute = list(dic=TRUE),
    control.predictor = list(link = 1),
    control.inla = list(diagonal=10000, int.strategy='eb',strategy='gaussian'),
    #verbose=TRUE
    ))
    
    # INLA model proper
    system.time(mod <-
    inla(formula = fml,
    family = "poisson",
    data = dat.inla,
    E = pop_men,
    control.compute = list(config=TRUE, dic=TRUE),
    control.predictor = list(link = 1),
    control.inla=list(diagonal=0),
    control.mode = list(result = mod.rough, restart = TRUE),
    #verbose=TRUE
    ))

    return(mod)
}
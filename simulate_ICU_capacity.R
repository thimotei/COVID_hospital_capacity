library(deSolve)

source("simulate_epidemic.R")

# if the maximum days and timestep aren't sourced from simulating an epidemic

#maxDays <- 100
#timeStep <- 0.1
#timeValues <- seq(0, max_days, timeStep) # days

# the incidence data must be interpolated to be used as a time-varying input in the dynamical system
incidenceFun <- approxfun(incidenceDF$time, incidenceDF$incidence, method = "constant")

# we have built in stochasicity if we think it is necessary further down the line. The system is deterministic
# by default for simplicity

#incidenceStochFun <- approxfun(incidenceStochDF$time, incidenceStochDF$incidence, method = "constant")

# defining the system of ODEs describing the dynamics ofthe demands on a hospital during a COVID-19 epidemic
icuCapacityEquations <- function(time, variables, parameters, input) {
  with(as.list(c(variables, parameters)), {
    import <- input(time) # this is a time-varying input, where the incidence case curve is fed into the system
    dW     <- b1*import - mu1*W - nu1*W - b2*W
    dIcu   <- b2*W - mu2*Icu - nu2*Icu
    dD     <- mu1*W + mu2*Icu
    dR     <- nu1*W + nu2*Icu
    return(list(c(dW, dIcu, dD, dR)))
  })
}

# defining the parameters - each is described with its own comment
parametersValuesIcu <- c(
  b1  = 0.25, # rate (per unit time) of hospitalisation from confirmation
  b2  = 0.1, # rate (per unit time) onset of severe symptoms from hospitalisation
  mu1 = 0.015, # rate (per unit time) proportion of individuals that die (in the ward)
  mu2 = 0.01,  # rate (per unit time) proportion of individuals that die (in the ICU)
  nu1 = 0.01,
  nu2 = 0.02
)

initialValuesIcu <- c(
  W   = 0,  
  Icu = 0,  
  D   = 0,
  R   = 0
)



IcuSimulation <- ode(
  y = initialValuesIcu,
  times = timeValues,
  func = icuCapacityEquations,
  parms = parametersValuesIcu,
  input = incidenceFun
)

IcuSimulation <- as.data.frame(IcuSimulation)
IcuSimulation <- data.frame(time = IcuSimulation$time,
                            W    = IcuSimulation$W,
                            Icu  = IcuSimulation$Icu,
                            D    = IcuSimulation$D,
                            R    = IcuSimulation$R)

# adding stochasticity, by assuming the waiting time between each event is exponentially distributed,
# meaning each variable is now a Poisson process

#W_stoch   <- sapply(Icu_values_1$W,   function(lambda) rpois(1,lambda))
#Icu_stoch <- sapply(Icu_values_1$Icu, function(lambda) rpois(1,lambda))
#D_stoch   <- sapply(Icu_values_1$D,   function(lambda) rpois(1,lambda))
#R_stoch   <- sapply(Icu_values_1$R,   function(lambda) rpois(1,lambda))




#Icu_day_means <- matrix(data = NA, nrow = max_days, ncol = 4)
#for(i in 0:(max_days - 1))
#{
#  Icu_day_means[i + 1,] <- as.vector(colMeans(Icu_values_1[((i)*100):((i + 1)*100),2:5]))
#}

#Icu_day_meansDF <- data.frame(time = 0:(max_days - 1), 
#                              W = Icu_day_means[,1],
#                              Icu = Icu_day_means[,2],
#                              D = Icu_day_means[,3],
#                              R = Icu_day_means[,4])


# to make simple trajectory plots of cases and hospital capacity
source("plots.R")


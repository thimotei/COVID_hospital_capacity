library(deSolve)


sirEquations <- function(time, variables, parameters) {
  with(as.list(c(variables, parameters)), {
    dS <- -r0*gamma*(S*I)/n
    dI <- r0*gamma*(S*I)/n - gamma*I
    dR <- gamma*I
    return(list(c(dS, dI, dR)))
  })
}

meanInfectiousPeriod <- 3 # days
r0Global    <- 3
gammaGlobal <- 1/meanInfectiousPeriod
nGlobal     <- 1000


parametersValues <- c(
  r0 = r0Global,        # average number of secondary infectious (/person)
  gamma  = gammaGlobal, # infectious contact rate (/person/day)
  n = 1000
)



initialValues <- c(
  S = 999,  # number of susceptibles at time = 0
  I =   1,  # number of infectious at time = 0
  R =   0   # number of recovered (and immune) at time = 0
)

maxDays <- 30
timeStep <- 0.1
timeValues <- seq(0, maxDays, timeStep) # days


sirValues <- ode(
  y = initialValues,
  times = timeValues,
  func = sirEquations,
  parms = parametersValues 
)



sirValues <- as.data.frame(sirValues)
incidenceDF <- data.frame(time = sirValues$time, 
                          incidence = ((sirValues$S*sirValues$I)/nGlobal)*gammaGlobal*r0Global)



#incidenceStoch <- sapply(incidenceDF$incidence, function(lambda) rpois(1,lambda))
#incidenceStochDF <- data.frame(time = sir_values_1$time, 
#                          incidence = incidenceStoch)


#with(incidenceDF, {
#  #plotting the time series of susceptibles:
#  plot(time, incidence, type = "l", col = "blue",
#       xlab = "time (days)", ylab = "incidence of cases")
#})


#prevalenceStoch <- sapply(prevalenceDF$prevalence, function(lambda) rpois(1,lambda))

#prevalenceStochMean <- NA
#for(i in 0:(max_days - 1))
#{
#  prevalenceStochMean[i + 1] <- as.vector(mean(prevalenceStoch[((i)*(1/timeStep)):((i + 1)*(1/timeStep))]))
#}


# plots

colourCycle = c("#e41a1c", 
                "#377eb8",
                "#4daf4a",
                "#984ea3",
                "#ff7f00",
                "#ffff33",
                "#a65628",
                "#f781bf")


layout(matrix(c(1,
                2), nrow=2, byrow=TRUE))


par(mar=c(3,4,1,3),mgp=c(2,0.6,0))
with(incidenceDF, {
  # plotting the time series of susceptibles:
  plot(time, incidence, type = "l", col = colourCycle[1],
       xlab = "time (days)", ylab = "cases (incidence)")
})
grid(ny = NULL, nx = 0, col = rgb(0.9,0.9,0.9), lty = "solid")


par(mar=c(3,4,1,3),mgp=c(2,0.6,0))
with(IcuSimulation, {
  # plotting the time series of susceptibles:
  plot(time, W, type = "l", col = colourCycle[2],
       xlab = "time (days)", ylab = "number of people", main = "A")
  # adding the time series of infectious:
  lines(time, Icu, col = colourCycle[3])
  # adding the time series of recovered:
  lines(time, D, col = colourCycle[4])
  # adding incidence
  lines(time, R, col = colourCycle[5])
})
grid(ny = NULL, nx = 0, col = rgb(0.9,0.9,0.9), lty = "solid")
legend("topleft",
       legend=c("No. on the ward",
                "No. in ICU",
                "No. dead",
                "No. recovered"),
       col=c(colourCycle[2], 
             colourCycle[3],
             colourCycle[4],
             colourCycle[5]),
       lty=1:1, 
       cex=1)

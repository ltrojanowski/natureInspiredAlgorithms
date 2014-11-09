#defincja równania vdp

vdp <- function (t, y, mu) {
  list(c(
    y[2],
    mu * (1-y[1]^2) * y[2] - y[1]
  ))
}

library(deSolve)
library(rbenchmark)

yinit <- c(y1 = 2, y2 = 0)

stiff <- ode(y = yinit, func = vdp,
             times = 0:3000, parms = 1000)

#nonstiff <- 
benchmark(ode(y = yinit, func = vdp,
                times = seq(0, 30, by = 0.01),
                parms = 1))
head(stiff, n = 3)

#head(nonstiff, n = 3)

#plot(nonstiff)#, type = "l", lwd = 2, ylab = "y", main = "IVP ODE, nonstiff")
#plot(stiff)

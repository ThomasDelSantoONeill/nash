### Performance benchmark of LV vs round-robin method (Figure 4 in F&F
###   manuscript). For this particular case we utilised the following
###   matrices for each model as input for the 'nash()' function. Because
###   of the long computation times of the round-robin method we run the
###   following code (EXCEPT THAT FOR THE SIMPLE MODEL) in parallel utilising
###   Queen Mary's Apocrita HPC facility, supported by QMUL Research-IT
###   (http://doi.org/10.5281/zenodo.438045).
SMFmat <- readRDS("F&F_Scripts&Data/SMFmat.rds")
BSFmat <- readRDS("F&F_Scripts&Data/BSFmat.rds")
NSFmat <- readRDS("F&F_Scripts&Data/NSFmat.rds")
### SIMPLE MODEL
  # Load libraries.
  library(deSolve) # ODE solver library
  library(nash)
  # Initial conditions and parameters.
  y <- c(b1 = 0.02, b2 = 0.001)
  parameters <- c(r1 = 1, r2 = 1,
                  a11 = 1, a12 = 0.5,
                  a21 = 0.25, a22 = 1)
  time <- 1:100
  # Numerical fudge to avoid biomasses becoming negative.
  inv <- 1e-5
  # Model formulation.
  HQLV <- function(par, avg.window = 10) {
    derivs <- function(time, y, parameters) {
      with(as.list(c(y, parameters)), {
        db1.dt = b1 * (r1 - a11*b1 - a12*b2^2) - par[1]*b1 + inv
        db2.dt = b2 * (r2 - a22*b2 - a21*b1^2) - par[2]*b2 + inv
        return(list(c(db1.dt, db2.dt)))
      })
    }
    # Default integrator in deSolve
    simulation <- ode(y = y, times = time, func = derivs ,
                      parms = c(parameters , par))
    # Yield computation
    yields <- array(dim = c(nrow(simulation), length(par)))
    for (i in 1:nrow(simulation)) {
      yields[i,] <- simulation[i,-1] * par
    }
    return(colMeans(tail(yields , n = avg.window)))
  }
  # Execution of nash for different innitial values of F
  fn.eval <- array(dim = c(n,length(par)))
  for (i in 1:nrow(SMFmat)) {
    print(i)
    fn.eval[i,1] <- nash(par = as.numeric(SMFmat[i,]), fn = HQLV,
                         conv.criterion = 0.01, progress = FALSE)$counts
    fn.eval[i,2] <- nash(par = as.numeric(SMFmat[i,]), fn = HQLV,
                         method = "round-robin", conv.criterion = 0.01,
                         progress = FALSE)$counts
  }
  ### If you do not want to compile the code you can load the data via:
  ###   'SMdata <- readRDS("SM-Fnash-counts.rds")'
  SMdata <- readRDS("F&F_Scripts&Data/SM_fn.counts_data.rds")
### BALTIC SEA MODEL
  library(Rpath)
  load("F&F_Scripts&Data/BalticSeaModel.RData")
  parmat <- readRDS("F&F_Scripts&Data/BSFmat.rds")
  spp = c("AdCod", "AdHerring", "AdSprat", "AdFlounder")
  par <- Rsim.model$fishing$ForcedFRate[sim.years,spp]
  ### NOTE: WHAT FOLLOWS WILL ONLY RUN IN QMUL'S COMPUTER CLUSTER TO RUN IN SERIES
  ###   ON YOUR MACHINE SUBSTITUTE ACCORDINGLY. BE AWARE THAT THE EXECUTION MIGHT
  ###   TAKE A LONG TIME AND THEREFORE THE RESULTING DATA IS INCLUDED IN THE
  ###   F&F_Scripts&Data AS: "BS_fn.counts_data.rds".
  for(i in as.integer(Sys.getenv("SGE_TASK_ID"))) {
    nash.eq.LV <- nash(par = as.numeric(parmat[i,]), fn = fn_rpath, aged.str = TRUE,
                       data.years = 10, rsim.mod = Rsim.model,
                       IDnames = c("JuvCod", "AdCod", "JuvHerring", "AdHerring",
                                   "JuvSprat", "AdSprat", "JuvFlounder",
                                   "AdFlounder"),
                       method = "LV", yield.curves = FALSE,
                       rpath.params = Rpath.parameters, avg.window = 250,
                       simul.years = sim.years, integration.method = "RK4",
                       track = FALSE, conv.criterion = 0.01)
    saveRDS(nash.eq.LV, paste("BS-NE-Fmsy-LV_",i,".rds",sep = ""))
    nash.eq.RR <- nash(par = as.numeric(parmat[i,]), fn = fn_rpath, aged.str = TRUE,
                       data.years = 10, rsim.mod = Rsim.model,
                       IDnames = c("JuvCod", "AdCod", "JuvHerring", "AdHerring",
                                   "JuvSprat", "AdSprat", "JuvFlounder",
                                   "AdFlounder"),
                       method = "round-robin", yield.curves = FALSE,
                       rpath.params = Rpath.parameters, avg.window = 250,
                       simul.years = sim.years, integration.method = "RK4",
                       track = FALSE, conv.criterion = 0.01)
    saveRDS(nash.eq.RR, paste("BS-NE-Fmsy-RR_",i,".rds",sep = ""))
  }
  BSdata <- readRDS("F&F_Scripts&Data/BS_fn.counts_data.rds")
### NORTH SEA MODEL
  load("F&F_Scripts&Data/NorthSeaModel.RData")
  parmat <- readRDS("F&F_Scripts&Data/NSFmat.rds")
  spp = c("AduCod", "AduWhiting", "AduHaddock", "AduSaithe", "AduHerring",
          "NorwayPout", "Sandeels", "Plaice", "Sole")
  par <- Rsim.model$fishing$ForcedFRate[sim.years,spp]
  ### NOTE: WHAT FOLLOWS WILL ONLY RUN IN QMUL'S COMPUTER CLUSTER TO RUN IN SERIES
  ###   ON YOUR MACHINE SUBSTITUTE ACCORDINGLY. BE AWARE THAT THE EXECUTION MIGHT
  ###   TAKE A LONG TIME AND THEREFORE THE RESULTING DATA IS INCLUDED IN THE
  ###   F&F_Scripts&Data AS: "NS_fn.counts_data.rds".
  for(i in as.integer(Sys.getenv("SGE_TASK_ID"))) {
    nash.eq.LV <- nash(par = as.numeric(parmat[i,]), fn = fn_rpath, aged.str = TRUE,
                       data.years = 23, rsim.mod = Rsim.model,
                       IDnames = c("JuvCod", "AduCod", "JuvWhiting", "AduWhiting",
                                   "JuvHaddock", "AduHaddock", "JuvSaithe",
                                   "AduSaithe", "JuvHerring", "AduHerring",
                                   "NorwayPout", "Sandeels", "Plaice", "Sole"),
                       method = "LV", yield.curves = FALSE,
                       rpath.params = Rpath.parameters, avg.window = 500,
                       simul.years = sim.years, integration.method = "AB",
                       track = FALSE, conv.criterion = 0.01)
    saveRDS(nash.eq.LV, paste("NS-NE-Fmsy-LV_",i,".rds",sep = ""))
    nash.eq.RR <- nash(par = as.numeric(parmat[i,]), fn = fn_rpath, aged.str = TRUE,
                       data.years = 23, rsim.mod = Rsim.model,
                       IDnames = c("JuvCod", "AduCod", "JuvWhiting", "AduWhiting",
                                   "JuvHaddock", "AduHaddock", "JuvSaithe",
                                   "AduSaithe", "JuvHerring", "AduHerring",
                                   "NorwayPout", "Sandeels", "Plaice", "Sole"),
                       method = "round-robin", yield.curves = FALSE,
                       rpath.params = Rpath.parameters, avg.window = 500,
                       simul.years = sim.years, integration.method = "AB",
                       track = FALSE, conv.criterion = 0.01)
    saveRDS(nash.eq.RR, paste("NS-NE-Fmsy-RR_",i,".rds",sep = ""))
  }
  NSdata <- readRDS("F&F_Scripts&Data/NS_fn.counts_data.rds")

### Plotting routine
  performance <- data.frame(Simple.Model = c(colMeans(SMdata)),
                            BalticSea.Model = c(colMeans(BSdata)),
                            NorthSea.Model = c(colMeans(NSdata)))
  colnames(performance) <- c("Simple Model", "Baltic Sea Model",
                             "North Sea Model")
  rownames(performance) <- c("LV", "round-robin")
  performance <- as.matrix(performance)
  barplot(performance, beside = TRUE, ylim = c(0,10000),
          legend = rownames(performance),
          args.legend = list(x = "topright", bty = "n",
                             title = "Method",
                             title.adj = 0.1),
          ylab = "Function Evaluations (no.)", cex.lab = 1, cex.names = 1,
          cex.axis = 1, border = TRUE, col = c("gray45","gray85"))
  # width.error <- 0.025
  segments(x0 = 1.5, y0 = mean(na.omit(SMdata[,1]))-sd(na.omit(SMdata[,1])),
           x1 = 1.5, y1 = mean(na.omit(SMdata[,1]))+sd(na.omit(SMdata[,1])), lwd = 2)
  # segments(x0 = 1.5-width.error, y0 = mean(SMdata[,1])+sd(SMdata[,1]),
  #          x1 = 1.5+width.error, y1 = mean(SMdata[,1])+sd(SMdata[,1]), lwd = 2)
  # segments(x0 = 1.5-width.error, y0 = mean(SMdata[,1])-sd(SMdata[,1]),
  #          x1 = 1.5+width.error, y1 = mean(SMdata[,1])-sd(SMdata[,1]), lwd = 2)

  segments(x0 = 2.5, y0 = mean(na.omit(SMdata[,2]))-sd(na.omit(SMdata[,2])),
           x1 = 2.5, y1 = mean(na.omit(SMdata[,2]))+sd(na.omit(SMdata[,2])), lwd = 2)
  # segments(x0 = 2.5-width.error, y0 = mean(SMdata[,2])+sd(SMdata[,2]),
  #          x1 = 2.5+width.error, y1 = mean(SMdata[,2])+sd(SMdata[,2]), lwd = 2)
  # segments(x0 = 2.5-width.error, y0 = mean(SMdata[,2])-sd(SMdata[,2]),
  #          x1 = 2.5+width.error, y1 = mean(SMdata[,2])-sd(SMdata[,2]), lwd = 2)

  segments(x0 = 4.5, y0 = mean(na.omit(BSdata[,1]))-sd(na.omit(BSdata[,1])),
           x1 = 4.5, y1 = mean(na.omit(BSdata[,1]))+sd(na.omit(BSdata[,1])),
           lwd = 2)
  # segments(x0 = 4.5-width.error, y0 = mean(na.omit(BSdata[,1]))+sd(na.omit(BSdata[,1])),
  #          x1 = 4.5+width.error, y1 = mean(na.omit(BSdata[,1]))+sd(na.omit(BSdata[,1])),
  #          lwd = 2)
  # segments(x0 = 4.5-width.error, y0 = mean(na.omit(BSdata[,1]))-sd(na.omit(BSdata[,1])),
  #          x1 = 4.5+width.error, y1 = mean(na.omit(BSdata[,1]))-sd(na.omit(BSdata[,1])),
  #          lwd = 2)

  segments(x0 = 5.5, y0 = mean(na.omit(BSdata[,2]))-sd(na.omit(BSdata[,2])),
           x1 = 5.5, y1 = mean(na.omit(BSdata[,2]))+sd(na.omit(BSdata[,2])),
           lwd = 2)
  # segments(x0 = 5.5-width.error, y0 = mean(na.omit(BSdata[,2]))+sd(na.omit(BSdata[,2])),
  #          x1 = 5.5+width.error, y1 = mean(na.omit(BSdata[,2]))+sd(na.omit(BSdata[,2])),
  #          lwd = 2)
  # segments(x0 = 5.5-width.error, y0 = mean(na.omit(BSdata[,2]))-sd(na.omit(BSdata[,2])),
  #          x1 = 5.5+width.error, y1 = mean(na.omit(BSdata[,2]))-sd(na.omit(BSdata[,2])),
  #          lwd = 2)

  segments(x0 = 7.5, y0 = mean(na.omit(NSdata[,1]))-sd(na.omit(NSdata[,1])),
           x1 = 7.5, y1 = mean(na.omit(NSdata[,1]))+sd(na.omit(NSdata[,1])),
           lwd = 2)
  # segments(x0 = 7.5-width.error, y0 = mean(na.omit(NSdata[,1]))+sd(na.omit(NSdata[,1])),
  #          x1 = 7.5+width.error, y1 = mean(na.omit(NSdata[,1]))+sd(na.omit(NSdata[,1])),
  #          lwd = 2)
  # segments(x0 = 7.5-width.error, y0 = mean(na.omit(NSdata[,1]))-sd(na.omit(NSdata[,1])),
  #          x1 = 7.5+width.error, y1 = mean(na.omit(NSdata[,1]))-sd(na.omit(NSdata[,1])),
  #          lwd = 2)
  segments(x0 = 8.5, y0 = mean(na.omit(NSdata[,2]))-sd(na.omit(NSdata[,2])),
           x1 = 8.5, y1 = mean(na.omit(NSdata[,2]))+sd(na.omit(NSdata[,2])),
           lwd = 2)
  box()
  grid()
  ### At this point we decided to make an inset bar plot to "zoom-in" the simple
  ###   and Baltic Sea models but first some data analysis to evaluate the
  ###   improvements in performance
  ratioperf <- data.frame(SM = SMdata[,2]/SMdata[,1],
                          BS = BSdata[,2]/BSdata[,1],
                          NS = NSdata[,2]/NSdata[,1])
  ### Mean performance improvement and sd around estimate
  mean(apply(ratioperf,1,mean))
  sd(apply(ratioperf,1,mean))

  ### Plot for inset
  barplot(performance[,-3], beside = TRUE, ylim = c(0,400),
          legend = FALSE,
          ylab = "Function Evaluations (no.)", cex.lab = 1, cex.names = 1,
          cex.axis = 1, border = TRUE, col = c("gray45","gray85"))
  segments(x0 = 1.5, y0 = mean(na.omit(SMdata[,1]))-sd(na.omit(SMdata[,1])),
           x1 = 1.5, y1 = mean(na.omit(SMdata[,1]))+sd(na.omit(SMdata[,1])), lwd = 2)
  segments(x0 = 2.5, y0 = mean(na.omit(SMdata[,2]))-sd(na.omit(SMdata[,2])),
           x1 = 2.5, y1 = mean(na.omit(SMdata[,2]))+sd(na.omit(SMdata[,2])), lwd = 2)
  segments(x0 = 4.5, y0 = mean(na.omit(BSdata[,1]))-sd(na.omit(BSdata[,1])),
           x1 = 4.5, y1 = mean(na.omit(BSdata[,1]))+sd(na.omit(BSdata[,1])),
           lwd = 2)
  segments(x0 = 5.5, y0 = mean(na.omit(BSdata[,2]))-sd(na.omit(BSdata[,2])),
           x1 = 5.5, y1 = mean(na.omit(BSdata[,2]))+sd(na.omit(BSdata[,2])),
           lwd = 2)
  box()
  grid()
  ### Figure 4 in F&F manuscript was later processed to make it nice for
  ###   publication.

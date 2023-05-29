### Computing Nash equilibria for the three benchmark models.
### NOTE to reviewers:
###   For both Rpath models the NE computation might take a long time. Please
###   'readRDS()' the appropriate data 'nash.eq.<<method>>.<<model>>' where
###   method is either LV or RR and BS for Baltic Sea model or NS for the North
###   Sea model.
  # SIMPLE MODEL (Listing 1 in F&F manuscript)
  # Load libraries .
  library(Rpath)
  library (deSolve) # ODE solver library
  library (nash)
  # Initial conditions and parameters.
  y <- c (b1 = 0.02, b2 = 0.001)
  parameters <- c (r1 = 1, r2 = 1,
                   a11 = 1, a12 = 0.5,
                   a21 = 0.25, a22 = 1)
  time <- 1:100
  # Numerical fudge to avoid biomasses becoming negative.
  inv <- 1e-5
  # Model formulation.
  HQLV <- function(par, avg.window = 10) {
    derivs <- function(time, y, parameters) {
      with(as.list(c(y, parameters)), {
        db1.dt = b1 * (r1 - a11 * b1 - a12 * b2^2) - par[1] * b1 + inv
        db2.dt = b2 * (r2 - a22 * b2 - a21 * b1^2) - par[2] * b2 + inv
        return(list(c(db1.dt, db2.dt)))
      })
    }
    # Default integrator in deSolve
    simulation <- ode(y = y, times = time, func = derivs,
                      parms = c(parameters, par))
    # Yield computation
    yields <- array(dim = c(nrow(simulation), length(par)))
    for( i in 1:nrow(simulation)) {
      yields[i,] <- simulation[i, -1] * par
    }
    return(colMeans(tail(yields, n = avg.window)))
  }
  # Execution of nash
  nash(par = c(0.2, 0.3), fn = HQLV)
  # BALTIC SEA AND NORTH SEA MODELS (Listing 2 in F&F manuscript)
  load("F&F_Scripts&Data/BalticSeaModel.RData")
  spp = c("AdCod", "AdHerring", "AdSprat", "AdFlounder")
  par <- as.numeric(tail(Rsim.model$fishing$ForcedFRate[,spp], n = 1))
  # LV method
  nash.eq.LV.BS <- nash(par = par, fn = fn_rpath, aged.str = TRUE,
                        data.years = 10, rsim.mod = Rsim.model,
                        IDnames = c("JuvCod", "AdCod", "JuvHerring",
                                    "AdHerring", "JuvSprat", "AdSprat",
                                    "JuvFlounder", "AdFlounder"),
                        method = "LV", yield.curves = TRUE,
                        rpath.params = Rpath.parameters, avg.window = 250,
                        simul.years = 500, integration.method = "AB",
                        conv.criterion = 0.01,
                        F.increase = 0.01)
  saveRDS(nash.eq.LV.BS, "nash.eq.LV.BS.rds")

  load("F&F_Scripts&Data/NorthSeaModel.RData")
  spp = c("AduCod", "AduWhiting", "AduHaddock", "AduSaithe", "AduHerring",
          "NorwayPout", "Sandeels", "Plaice", "Sole")
  par <- Rsim.model$fishing$ForcedFRate[sim.years,spp]
  nash.eq.LV.NS <- nash(par = par, fn = fn_rpath, aged.str = TRUE,
                        data.years = 23, rsim.mod = Rsim.model,
                        IDnames = c("JuvCod", "AduCod", "JuvWhiting",
                                    "AduWhiting", "JuvHaddock", "AduHaddock",
                                    "JuvSaithe", "AduSaithe", "JuvHerring",
                                    "AduHerring", "NorwayPout", "Sandeels",
                                    "Plaice", "Sole"),
                        method = "LV", yield.curves = TRUE,
                        rpath.params = Rpath.parameters, avg.window = 500,
                        simul.years = 1000, integration.method = "AB",
                        conv.criterion = 0.01)
  saveRDS(nash.eq.LV.NS, "nash.eq.LV.NS.rds")

### Plotting libraries
library(ggplot2)
library(extrafont)
library(reshape2)
loadfonts(device = "win")
# Personalised theme
theme_tjdso <- theme(
  text = element_text(family = "Consolas", size = 20),
  panel.background = element_blank(),
  panel.border = element_rect(fill = FALSE),
  panel.grid.major = element_line(
    linetype = 3,
    colour = "grey",
    size = 0.25
  ),
  panel.grid.minor = element_line(
    linetype = 3,
    colour = "grey",
    size = 0.1
  ),
  strip.background = element_blank(),
  strip.text = element_text(size = 20),
  strip.placement = "outside",
  panel.spacing.x = unit(5, "mm"),
  axis.ticks.length = unit(-2, "mm"),
  axis.text.x.top = element_blank(),
  axis.text.y.right = element_blank(),
  axis.title.x.top = element_blank(),
  axis.title.y.right = element_blank(),
  axis.title = element_text(size = 18),
  axis.text.x.bottom = element_text(margin = margin(4, 0, 0, 0, "mm")),
  axis.text.y.left = element_text(margin = margin(0, 4, 0, 0, "mm"))
)
### Diagnostics plotting routine (Figure_2 in F&F manuscript).
###   If the above was not run, load appropriate data
###   (e.g. 'readRDS("F&F_Scripts&Data/nash.eq.LV.BS.rds")') and execute
###   the ggplot2 code.
nash.eq.LV.BS <- readRDS("F&F_Scripts&Data/nash.eq.LV.BS.rds")
sppname <- c("Cod", "Herring", "Sprat", "Flounder")
# Transform data into something ggplot2 reads
Fvec <- c()
Yvec <- c()
sppvec <- c()
for (i in 1:length(nash.eq.LV.BS$YieldEQ)) {
  Fvec <- append(Fvec, nash.eq.LV.BS$YieldEQ[[i]][,1])
  Yvec <- append(Yvec, nash.eq.LV.BS$YieldEQ[[i]][,2])
  sppvec <- append(sppvec, rep(sppname[i], nrow(nash.eq.LV.BS$YieldEQ[[i]])))
}
Yeq <- data.frame("Spp"=sppvec,
                  "Fval"=Fvec,
                  "Yield"=Yvec)
Fnash <- data.frame("Spp"=sppname,
                    "Fnash"=as.numeric(nash.eq.LV.BS$par))
lab <- round(Fnash$Fnash, digits = 3)
round(Fnash$Fnash, digits = 2)
lab <- c(expression(paste("F"["Nash"], " = ", 0.21)),
         expression(paste("F"["Nash"], " = ", 0.27)),
         expression(paste("F"["Nash"], " = ", 0.59)),
         expression(paste("F"["Nash"], " = ", 0.31)))
Fnash$Labs <- as.character(lab)
# Plot
ggplot(data = Yeq, aes(x = Fval, y = Yield)) +
  geom_line(size = 1.5) +
  geom_vline(data = Fnash, aes(xintercept = Fnash),
             linetype = "dashed", size = 1.25) +
  geom_text(data = Fnash, aes(x = 0.2, y =0,
                              label = Labs),
            family = "Consolas", parse = TRUE, size = 5) +
  facet_wrap(~Spp, scales = "free") +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.01),
                     sec.axis = dup_axis()) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01),
                     sec.axis = dup_axis()) +
  theme_tjdso +
  labs(y = bquote(paste("Yield (",Kg^3, " x ",Km^-2, ")")),
       x = bquote(paste("Fishing Mortality (",yr^-1,")")))

### Diagnostics plotting routine (Figure_3 in F&F manuscript).
###   If the above was not run, load appropriate data
###   (e.g. 'readRDS("F&F_Scripts&Data/nash.eq.LV.NS.rds")') and execute
###   the ggplot2 code.
nash.eq.LV.NS <- readRDS("F&F_Scripts&Data/nash.eq.LV.NS.rds")
sppname <- c("Cod", "Whiting", "Haddock", "Saithe", "Herring", "NorwayPout",
             "Sandeels", "Plaice", "Sole")
# Transform data into something ggplot2 reads
Fvec <- c()
Yvec <- c()
sppvec <- c()
for (i in 1:length(nash.eq.LV.NS$YieldEQ)) {
  Fvec <- append(Fvec, nash.eq.LV.NS$YieldEQ[[i]][,1])
  Yvec <- append(Yvec, nash.eq.LV.NS$YieldEQ[[i]][,2])
  sppvec <- append(sppvec, rep(sppname[i], nrow(nash.eq.LV.NS$YieldEQ[[i]])))
}
Yeq <- data.frame("Spp"=sppvec,
                  "Fval"=Fvec,
                  "Yield"=Yvec)
Fnash <- data.frame("Spp"=sppname,
                    "Fnash"=as.numeric(nash.eq.LV.NS$par))
lab <- round(Fnash$Fnash, digits = 3)
lab <- c(expression(paste("F"["Nash"], " = ", 0.418)),
         expression(paste("F"["Nash"], " = ", 0.337)),
         expression(paste("F"["Nash"], " = ", 0.335)),
         expression(paste("F"["Nash"], " = ", 0.188)),
         expression(paste("F"["Nash"], " = ", 0.217)),
         expression(paste("F"["Nash"], " = ", 0.388)),
         expression(paste("F"["Nash"], " = ", 0.184)),
         expression(paste("F"["Nash"], " = ", 0.526)),
         expression(paste("F"["Nash"], " = ", 0.118)))
Fnash$Labs <- as.character(lab)
# Plot
ggplot(data = Yeq, aes(x = Fval, y = Yield)) +
  geom_line(size = 1.5) +
  geom_vline(data = Fnash, aes(xintercept = Fnash),
             linetype = "dashed", size = 1.25) +
  geom_text(data = Fnash, aes(x = Fnash*1.5, y =0,
                              label = Labs),
            family = "Consolas", parse = TRUE, size = 5) +
  facet_wrap(~Spp, scales = "free") +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.01),
                     sec.axis = dup_axis()) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01),
                     sec.axis = dup_axis()) +
  theme_tjdso +
  labs(y = bquote(paste("Yield (",Kg^3, " x ",Km^-2, ")")),
       x = bquote(paste("Fishing Mortality (",yr^-1,")")))

### Bounding the search space for the Nash equilibrium by subjecting the
###   ecosystem to conservation constraints (Figure 2 in F&F manuscript).
###   To generate the equilibrium yield curves under a constraint of biodiversity
###   conservation the 'rsense' branch of the nash package is required. You can
###   install this branch by running the following command:
###   remotes::install_github("ThomasDelSantoONeill/nash@rsense")
library(Rpath)
library(nash)
load("F&F_Scripts&Data/BalticSeaModel.RData")
nash.eq.LV.BS <- readRDS("F&F_Scripts&Data/BS-NE-Fmsy-LV.rds")
spp = c("AdCod", "AdHerring", "AdSprat", "AdFlounder")
par <- as.numeric(tail(Rsim.model$fishing$ForcedFRate[,spp], n = 1))
model.area <- 240669
### Target for Cod biomass = 5e4 (kg^3)
blim <- c((as.numeric(nash.eq.LV.BS$value/nash.eq.LV.BS$par)[1]+(100000/model.area)), 0, 0, 0)
### Now nash will return a matrix for the 'par' values computed as well as a
###   new 'Bnash' matrix (i.e. the biomass state at Fnash). IF you do not whish
###   to run this please run
###   'readRDS("F&F_Scripts&Data/nash.eq.LV.BS.CC.rds")'
nash.eq.LV.BS.CC <- nash(par = par, fn = fn_rpath, aged.str = TRUE,
                         data.years = 10, rsim.mod = Rsim.model,
                         IDnames = c("JuvCod", "AdCod", "JuvHerring", "AdHerring",
                                     "JuvSprat", "AdSprat", "JuvFlounder",
                                     "AdFlounder"),
                         method = "LV", yield.curves = FALSE,
                         rpath.params = Rpath.parameters, avg.window = 250,
                         simul.years = sim.years, integration.method = "RK4",
                         Bcons = blim, track = TRUE, conv.criterion = 0.01)
### If TRUE all went well!
tail(na.omit(nash.eq.LV.BS.CC$Bnash), n = 1)>=blim
# saveRDS(nash.eq.LV.BS.CC, "nash.eq.LV.BS.CC.rds")

###   You should now have access to the 'mse_btarget()' function which works
###   exactly like 'fn_rpath()' but computing long term biomasses instead of
###   yields.

###   This routine uses the function 'target_F()' below as a root-finding
###   algorithm; whereby for a given biomass state, 'target_F()' finds the
###   harvesting rates (Fs) that generate the observed biomass trajectories.

###   The script below was run in parallel utilising Queen Mary's Apocrita HPC
###   facility, supported by QMUL Research-IT
###   (http://doi.org/10.5281/zenodo.438045). All but the focal species for
###   which the equilibrium yield curves is to be computed are fixed at
###   Fnash. Then for each F value from 0 to 2xFnash ('length.out = 50') of the
###   focal species, we run 'mse_btarget()' to get biomasses.
###   The script then substitutes the output Cod value for the Cod biomass
###   constraint and then the root finder is executed within the 'optim()'
###   function to estimate the Fs that produce this biomass state. Then the
###   script calculates the long-term yield (via 'fn_rpath()') for these new
###   set of Fs.

targetspp <- c(
  "JuvCod",
  "AdCod",
  "JuvHerring",
  "AdHerring",
  "JuvSprat",
  "AdSprat",
  "JuvFlounder",
  "AdFlounder"
)
# Root finder (set up specifically for the Baltic Sea model)
target_F <-
  function(f_rates,
           b_target,
           avg.window,
           rsim.scene,
           verbose = F,
           species_list) {

    b_actual <- mse_btarget(
      par = f_rates,
      simul.years = 500,
      aged.str = TRUE,
      data.years = 10,
      rsim.mod = rsim.scene,
      rpath.params = Rpath.parameters,
      avg.window = avg.window,
      integration.method = "RK4",
      IDnames = species_list
    )[c(seq(2,length(species_list),2))]
    # This is the output to minimize
    output <- sum(log(as.numeric(b_actual) / b_target) ^ 2)
    # verbose=TRUE returns the full run time series (default False)
    # otherwise just return the function value
    if (verbose) {
      return(res)
    } else{
      return(output)
    }
  }
Yieldeq <- list()
par <- as.numeric(tail(na.omit(nash.eq.LV.BS.CC$par), n = 1))
### NOTE: WHAT FOLLOWS WILL ONLY RUN IN QMUL'S COMPUTER CLUSTER TO RUN IN SERIES
###   ON YOUR MACHINE SUBSTITUTE ACCORDINGLY. BE AWARE THAT THE EXECUTION MIGHT
###   TAKE A LONG TIME AND THEREFORE THE RESULTING DATA IS INCLUDED IN THE
###   F&F_Scripts&Data AS: "CCYield_Spp_<<number>>.rds" WHERE <<number>>
###   TAKES VALUES FROM 2-4.
for (i in as.integer(Sys.getenv("SGE_TASK_ID"))) {
  print(i)
  Harv <- par
  Fvec <- seq(0,par[i]*2,length.out = 50)
  outyield <- array(dim = c(length(Fvec),2))
  for (j in 1:length(Fvec)) {
    Harv[i] <- Fvec[j]
    biomasstarget <- mse_btarget(
      par = Harv,
      simul.years = 500,
      aged.str = TRUE,
      data.years = 10,
      rsim.mod = Rsim.model,
      rpath.params = Rpath.parameters,
      avg.window = 250,
      integration.method = "RK4",
      IDnames = c(
        "JuvCod",
        "AdCod",
        "JuvHerring",
        "AdHerring",
        "JuvSprat",
        "AdSprat",
        "JuvFlounder",
        "AdFlounder"
      )
    )[seq(2,8,2)]
    biomasstarget[1] <- as.numeric(tail(na.omit(nash.eq.LV.BS.CC$Bnash[,1]), n=1))
    harv2 <- optim(
      par = Harv,
      fn = target_F,
      b_target = as.numeric(biomasstarget),
      avg.window = 250,
      rsim.scene = Rsim.model,
      species_list = targetspp,
      method="L-BFGS-B",
      lower = rep(0,length(par)),
      control = list(
        fnscale = 1,
        factr = 1e7,
        pgtol = 1e-8
      )
    )
    print(harv2$par)
    Harv[1] <- as.numeric(harv2$par)[1]
    Yieldspp <- fn_rpath(
      par = Harv,
      simul.years = 500,
      aged.str = TRUE,
      data.years = 10,
      rsim.mod = Rsim.model,
      rpath.params = Rpath.parameters,
      avg.window = 250,
      integration.method = "RK4",
      IDnames = c(
        "JuvCod",
        "AdCod",
        "JuvHerring",
        "AdHerring",
        "JuvSprat",
        "AdSprat",
        "JuvFlounder",
        "AdFlounder"
      )
    )
    print(Yieldspp)
    outyield[j,] <- c(Fvec[j], as.numeric(Yieldspp)[i])
  }
  Yieldeq[[i]] <- outyield
}
saveRDS(Yieldeq, paste("CCYield_Spp_",i,".rds",sep = ""))
### Load data for plotting (Figure 2 in EAP manuscript)
BSCCdataY2 <- readRDS("F&F_Scripts&Data/CCYield_Spp_2.rds")
BSCCdataY3 <- readRDS("F&F_Scripts&Data/CCYield_Spp_3.rds")
BSCCdataY4 <- readRDS("F&F_Scripts&Data/CCYield_Spp_4.rds")
nash.eq.LV.BS.CC <- readRDS("F&F_Scripts&Data/BS-CCNE-Fmsy.rds")
# Plotting -----------------------------------------------------------------
library(ggplot2)
library(extrafont)
library(reshape2)
loadfonts(device = "win")
# Baltic Sea
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
                    "Fnash"=as.numeric(t(tail(na.omit(nash.eq.LV.BS$par), n = 1))))
# Code is kept at the conservation constraint biomass so populate a matrix with
#   NA for plotting purposes.
BSCCdataY2[[1]] <- matrix(rep(NA,100),ncol = 2,nrow = 50)
BSCCdataY2[[3]] <- BSCCdataY3[[3]]
BSCCdataY2[[4]] <- BSCCdataY4[[4]]

Fvec.cons5 <- c()
Yvec.cons5 <- c()
sppvec.cons5 <- c()
for (i in 1:length(BSCCdataY2)) {
  Fvec.cons5 <- append(Fvec.cons5, BSCCdataY2[[i]][,1])
  Yvec.cons5 <- append(Yvec.cons5, BSCCdataY2[[i]][,2])
  sppvec.cons5 <- append(sppvec.cons5, rep(sppname[i], nrow(BSCCdataY2[[i]])))
}
Yeq.cons5 <- data.frame("Spp"=sppvec.cons5,
                        "Fval"=Fvec.cons5,
                        "Yield"=Yvec.cons5)
Fnash.cons5 <- data.frame("Spp"=sppname,
                          "Fnash.cons5"=as.numeric(t(tail(na.omit(nash.eq.LV.BS.CC$par), n = 1))))
Fnash.cons5$Yield <- c(nash.eq.LV.BS.CC$value[1], rep(NA,3))
lab <- round(Fnash.cons5$Fnash, digits = 3)
round(Fnash.cons5$Fnash, digits = 3)
lab <- c(expression(paste("CC-F"["Nash"], " = ", 0.150)),
         expression(paste("CC-F"["Nash"], " = ", 0.392)),
         expression(paste("CC-F"["Nash"], " = ", 0.660)),
         expression(paste("CC-F"["Nash"], " = ", 0.309)))
Fnash.cons5$Labs <- as.character(lab)

lab <- round(Fnash$Fnash, digits = 3)
round(Fnash$Fnash, digits = 3)
lab <- c(expression(paste("F"["Nash"], " = ", 0.205)),
         expression(paste("F"["Nash"], " = ", 0.277)),
         expression(paste("F"["Nash"], " = ", 0.606)),
         expression(paste("F"["Nash"], " = ", 0.309)))
Fnash$Labs <- as.character(lab)


# Personalised theme
theme_tjdso <- theme(text = element_text(family = "Consolas", size = 20),
                     panel.background = element_blank(),
                     panel.border = element_rect(fill = FALSE),
                     panel.grid.major = element_line(linetype = 3,
                                                     colour = "grey",
                                                     size = 0.25),
                     panel.grid.minor = element_line(linetype = 3,
                                                     colour = "grey",
                                                     size = 0.1),
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
                     axis.text.x.bottom = element_text(margin = margin(4,0,
                                                                       0,0,
                                                                       "mm")),
                     axis.text.y.left = element_text(margin = margin(0,4,
                                                                     0,0,
                                                                     "mm")))
# Plot
ggplot(data = Yeq, aes(x = Fval, y = Yield)) +
  geom_line(size = 1.5) +
  geom_vline(data = Fnash, aes(xintercept = Fnash),
             linetype = "dashed", size = 1.25) +
  geom_text(data = Fnash, aes(x = 0.3, y =0,
                              label = Labs),
            family = "Consolas", parse = TRUE, size = 5) +
  facet_wrap(~Spp, scales = "free") +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.01),
                     sec.axis = dup_axis()) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01),
                     sec.axis = dup_axis()) +
  theme_tjdso +
  labs(y = bquote(paste("Yield (",Kg^3, " x ",Km^-2, ")")),
       x = bquote(paste("Fishing Mortality (",yr^-1,")"))) +
  # geom_line(data = Yeq.cons, aes(x = Fval, y = Yield), size = 1.5, col = "blue") +
  geom_line(data = Yeq.cons5, aes(x = Fval, y = Yield), size = 1.5, col = "red") +
  # geom_vline(data = Fnash.cons, aes(xintercept = Fnash.cons),
  #            linetype = "dashed", size = 1.25, col = "blue") +
  geom_vline(data = Fnash.cons5, aes(xintercept = Fnash.cons5),
             linetype = "dashed", size = 1.25, col = "red") +
  geom_point(data = Fnash.cons5, aes(x = Fnash.cons5, y = Yield),
             col = 2, size = 4) +
  geom_text(data = Fnash.cons5, aes(x = 0.3, y =0.2,
                              label = Labs),
            family = "Consolas", parse = TRUE, size = 5)
### As you can see there are points in all 4 frames. This is a ggplot2 issue
###   and as a result we further processed this figure by erasing these points
###   for Flounder, Herring ans Sprat (making it nicer for publication).

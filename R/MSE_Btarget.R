#'Computes long-term equilibrium yields for \code{\link{Rpath-package}} models
#'
#'Viable \code{fn} to be used as input for \code{\link{nash}} when the
#' \code{\link{Rpath-package}} is used as operating ecological model
#' \insertCite{@see Lucey2020 for details}{nash}. \code{fn_rpath} takes the
#' harvesting rates as the numeric type vector \code{par} returning simulated
#' yields at equilibrium.
#'
#'@param par Numeric vector of harvesting rates of length equal to the number of
#' harvested species.
#'@param simul.years Desired simulation time.
#'@param aged.str Logical TRUE/FALSE if multistanza functional groups are
#' included in the model.
#'@param data.years Numeric vector indicating the years worth of data used to
#' parameterise the \code{Rpath} model.
#'@param IDnames Character vector with the names of the species for which
#' Nash Equilibrium harvesting rates are computed. Note that these names must
#' coincide with the ones used during the construction of the \code{Rpath}
#' model.
#'@param rsim.mod \code{Rpath}'s \code{rsim.scenario} object.
#'@param rpath.params \code{Rpath}'s \code{rpath.parameters} object.
#'@param avg.window Numeric type vector indicating the time window used to
#' average equilibrium yields.
#'@param integration.method Numerical integration routine used to solve the
#' \code{rsim.mod} object. Character vector with values (i) `\code{RK4}` or
#' (ii) `\code{AB}`.
#'
#'@details The \code{avg.window} argument becomes useful in case the dynamics
#' of the model reaches a steady state (\emph{e.g.} a limit cycle) rather than
#' a stable point attractor.
#'
#' The numerical integration methods implemented in the
#' \code{\link{Rpath-package}} are the 4th order Runge-Kutta (\code{RK4}) and
#' the two-step Adams-Bashforth (\code{AB}) method. The trade-off between both
#' methods is accuracy and speed, with \code{RK4} being more accurate but
#' slower than the \code{AB} method
#' \insertCite{@see Lucey2020 for details}{nash}.
#'
#' \code{fn_rpath} works for aged structure models in which multistanza species
#' are partition into adults and juveniles. Then a fixed harvesting rate ratio
#' between adults and juveniles is computed and retained when the function
#' \code{\link{nash}} is called.
#'
#'@return The function \code{fn_rpath} returns an atomic vector of real
#' double-precision long-term yields.
#'
#'@references
#'\insertRef{Lucey2020}{nash}
#'
#'@export
mse_btarget <- function(par,
                        simul.years = 100,
                        aged.str = TRUE,
                        data.years,
                        IDnames,
                        rsim.mod,
                        rpath.params,
                        avg.window = 10,
                        integration.method = "RK4",
                        verbose = FALSE) {
  ### LOCAL VARIABLES
  sppname <- IDnames
  harvesting <- par
  simul.years <- simul.years

  ### ADJUST SCENARIO if Adams-Bashforth method is used
  if (integration.method == "AB") {
    # Setting integration flags
    rsim.mod$params$NoIntegrate <-
      ifelse(
        rsim.mod$params$MzeroMort * rsim.mod$params$B_BaseRef > 24,
        0,
        rsim.mod$params$spnum
      )
  }
  # NOTE: Ensuring effort = 0 to play only with harvesting rates.
  fleet_name <-
    as.character(colnames(rsim.mod$fishing$ForcedEffort))
  for (i in 1:length(fleet_name)) {
    rsim.mod <- adjust.fishing(
      rsim.mod,
      parameter = "ForcedEffort",
      group = fleet_name[i],
      sim.year = 1:simul.years,
      sim.month = 0,
      value = 0
    )
  }

  ### AGE-STRUCTURED MODELS
  if (aged.str == TRUE) {
    n.aged.str <- rsim.mod$stanzas$Nsplit
    stanza.names <- rpath.params$stanzas$stindiv$Group
    stanza.names <- sppname[sppname %in% stanza.names]
    juvname <- stanza.names[seq(1, length(stanza.names), 2)]
    adname <- stanza.names[seq(2, length(stanza.names), 2)]
    JuvFProp <-
      as.numeric((
        rsim.mod$fishing$ForcedFRate[data.years, juvname] /
          rsim.mod$fishing$ForcedFRate[data.years, adname]
      ))
    for (i in 1:length(juvname)) {
      rsim.mod <- adjust.fishing(
        Rsim.scenario = rsim.mod,
        parameter = "ForcedFRate",
        group = juvname[i],
        sim.year = (data.years + 1):simul.years,
        sim.month = 0,
        value = harvesting[i] * JuvFProp[i]
      )
      rsim.mod <- adjust.fishing(
        Rsim.scenario = rsim.mod,
        parameter = "ForcedFRate",
        group = adname[i],
        sim.year = (data.years + 1):simul.years,
        sim.month = 0,
        value = harvesting[i]
      )
    }
    if ((length(sppname) > length(stanza.names)) == TRUE) {
      non.aged.groups <- sppname[!sppname %in% stanza.names]
      elements <- n.aged.str + (1:length(non.aged.groups))
      for (i in 1:length(elements)) {
        element <- elements[i]
        rsim.mod <- adjust.fishing(
          Rsim.scenario = rsim.mod,
          parameter = "ForcedFRate",
          group = non.aged.groups[i],
          sim.year = (data.years + 1):simul.years,
          sim.month = 0,
          value = harvesting[element]
        )
      }
      # Run simulation and compute biomass
      rsim.simul <- rsim.run(rsim.mod,
                             method = integration.method,
                             years = 1:simul.years)
      biomass <- array(dim = c(nrow(rsim.simul$annual_Biomass),
                               length(sppname)))
      for (i in 1:nrow(rsim.simul$annual_Biomass)) {
        adB <- rsim.simul$annual_Biomass[i, adname]
        juvB <- rsim.simul$annual_Biomass[i, juvname]
        non.aged.B <- rsim.simul$annual_Biomass[i, non.aged.groups]
        biomass[i, ] <-
          as.numeric(c(c(rbind(juvB, adB)), non.aged.B))/harvesting
      }
    } else if ((length(sppname) > length(stanza.names)) == FALSE) {
      # Run simulation and compute biomass
      rsim.simul <- rsim.run(rsim.mod,
                             method = integration.method,
                             years = 1:simul.years)
      biomass <- array(dim = c(nrow(rsim.simul$annual_Biomass),
                               length(sppname)))
      for (i in 1:nrow(rsim.simul$annual_Biomass)) {
        adB <- rsim.simul$annual_Biomass[i, adname]
        juvB <- rsim.simul$annual_Biomass[i, juvname]
        biomass[i, ] <- c(rbind(juvB, adB))/harvesting
      }
    }
  } else if (aged.str == FALSE) {
    for (i in 1:length(sppname)) {
      rsim.mod <- adjust.fishing(
        Rsim.scenario = rsim.mod,
        parameter = "ForcedFRate",
        group = sppname[i],
        sim.year = (data.years + 1):simul.years,
        sim.month = 0,
        value = harvesting[i]
      )
    }
    # Run simulation and compute biomass
    rsim.simul <- rsim.run(rsim.mod,
                           method = integration.method,
                           years = 1:simul.years)
    biomass <-
      array(dim = c(nrow(rsim.simul$annual_Biomass), length(sppname)))
    for (i in 1:nrow(rsim.simul$annual_Biomass)) {
      biomass[i, ] <- rsim.simul$annual_Biomass[i, sppname]/harvesting
    }
  }
  names <- c()
  for (i in 1:ncol(biomass)) {
    names <- append(names, paste("Spp", i))
  }
  colnames(biomass) <- names
  if (verbose) {
    return(biomass)
  } else{
    outlist <- colMeans(tail(biomass, n = avg.window))
    return(outlist)
  }
}

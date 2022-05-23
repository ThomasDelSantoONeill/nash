MSE_Btarget <- function(par,
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
    stanza.names <- IDnames[IDnames %in% stanza.names]
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
    if ((length(IDnames) > length(stanza.names)) == TRUE) {
      non.aged.groups <- IDnames[!IDnames %in% stanza.names]
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
          as.numeric(c(c(rbind(juvB, adB)), non.aged.biomass))
      }
    } else if ((length(IDnames) > length(stanza.names)) == FALSE) {
      # Run simulation and compute biomass
      rsim.simul <- rsim.run(rsim.mod,
                             method = integration.method,
                             years = 1:simul.years)
      biomass <- array(dim = c(nrow(rsim.simul$annual_Biomass),
                               length(sppname)))
      for (i in 1:nrow(rsim.simul$annual_Biomass)) {
        adB <- rsim.simul$annual_Biomass[i, adname]
        juvB <- rsim.simul$annual_Biomass[i, juvname]
        biomass[i, ] <- c(rbind(juvB, adB))
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
      biomass[i, ] <- rsim.simul$annual_Biomass[i, sppname]
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

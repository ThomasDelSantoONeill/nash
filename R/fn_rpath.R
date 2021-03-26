#' Computes long-term equilibrium yields for \code{Rpath} models
#'
#' Description
#'
#' @param par
#' @param simul.years
#' @param aged.str
#' @param data.years
#' @param IDnames
#' @param rsim.mod
#' @param rpath.params
#' @param avg.window
#' @param integration.method
#'
#' @return
#' @export
fn_rpath <- function(par, simul.years = 100, aged.str = TRUE, data.years,
                     IDnames, rsim.mod, rpath.params, avg.window = 10,
                     integration.method = "RK4") {
  # VALIDATOR
  if (!is.vector(par)) {
    stop("`par` is not a vector.")
  }
  if (length(par) < 1) {
    stop("`par` must be a vector of length >= 1")
  }
  if (!is.logical(aged.str)) {
    stop("`aged.str` must be a logical TRUE/FALSE argument.")
  }
  if (length(data.years) > 1) {
    stop("`data.years` must be a single numeric value.")
  }
  if (aged.str == TRUE && !is.list(rpath.params)) {
    stop("For aged structure models, the Rpath's balanced parameter object needs
         to be provided.")
  }
  if (aged.str == FALSE && length(par) < length(IDnames)) {
    stop("The length of `par` and the length of `IDnames` must be equal for
         non age-structured models.")
  }
  if (!is.list(rsim.mod)) {
    stop("`rsim.mod` must be an rsim object created via Rpath's
         `rsim.scenario()` function.")
  }
  if (simul.years != 100 & nrow(rsim.mod$fishing$ForcedFRate) != simul.years) {
    stop("If a longer simulation is desired, you will need to create a new
         `Rsim` scenario where the argument `years` is set to the required
         simulation time. Then, pass `par` with the new `Rsim.model` setting
         `simul.years` equal to the number of years within `rsim.scenario()`
         function.")
  }
  if (length(avg.window) > 1) {
    stop("`avg.window` must be a single numeric value.")
  }
  if (!integration.method%in%list("RK4", "AB")){
    stop("Sorry but the integration method introduced is not implemented")
  }
  if (!("Rpath" %in% .packages(all.available = TRUE))) {
    stop("`fn_Rpath` works only in conjunction with the Rpath package.")
  }
  # LOCAL VARIABLES
  sppname <- IDnames
  harvesting <- par
  simul.years <- simul.years
  # ADJUST SCENARIO
  if (integration.method == "AB") {
    # Setting integration flags
    NoIntegrateSpp <- as.vector(rsim.mod$params$spname[rsim.mod$params$PBopt > 24])
    for (i in 1:length(NoIntegrateSpp)) {
      name <- NoIntegrateSpp[i]
      rsim.mod <- adjust.scenario(rsim.mod, parameter = "NoIntegrate",
                                  group = name, value = 0)
    }
  }
  # NOTE: Ensure effort is set to 0 to play with harvesting rates.
  fleet_name <- as.character(colnames(rsim.mod$fishing$ForcedEffort))
  for (i in 1:length(fleet_name)) {
    rsim.mod <- adjust.fishing(rsim.mod, parameter = "ForcedEffort",
                               group = fleet_name[i],
                               sim.year = 1:simul.years, sim.month = 0,
                               value = 0)
  }
  # AGE-STRUCTURED MODELS
  if (aged.str == TRUE) {
    n.aged.str <- rsim.mod$stanzas$Nsplit
    stanza.names <- rpath.params$stanzas$stindiv$Group
    juvname <- stanza.names[seq(1, n.aged.str*2, 2)]
    adname <- stanza.names[seq(2, n.aged.str*2, 2)]
    JuvFProp <- as.numeric((rsim.mod$fishing$ForcedFRate[data.years,juvname]/
                              rsim.mod$fishing$ForcedFRate[data.years,adname]))
    if (all.equal(IDnames[IDnames%in%stanza.names], stanza.names) == FALSE) {
      stop("`IDnames` levels for stanza groups must be labeled as in the model
           parameterisation.")
    }
    for (i in 1:length(juvname)) {
      rsim.mod <- adjust.fishing(Rsim.scenario = rsim.mod,
                                 parameter = "ForcedFRate",
                                 group = juvname[i],
                                 sim.year = (data.years+1):simul.years,
                                 sim.month = 0,
                                 value = harvesting[i]*JuvFProp[i])
      rsim.mod <- adjust.fishing(Rsim.scenario = rsim.mod,
                                 parameter = "ForcedFRate",
                                 group = adname[i],
                                 sim.year = (data.years+1):simul.years,
                                 sim.month = 0, value = harvesting[i])
    }
    if ((length(IDnames) > length(stanza.names)) == TRUE) {
      non.aged.groups <- IDnames[!IDnames%in%stanza.names]
      elements <- n.aged.str + (1:length(non.aged.groups))
      for (i in 1:length(elements)) {
        element <- elements[i]
        rsim.mod <- adjust.fishing(Rsim.scenario = rsim.mod,
                                   parameter = "ForcedFRate",
                                   group = non.aged.groups[i],
                                   sim.year = (data.years+1):simul.years,
                                   sim.month = 0, value = harvesting[element])
      }
      # RUN SIMULATION & COMPUTE YIELDS
      rsim.simul <- rsim.run(rsim.mod, method = integration.method,
                             years = 1:simul.years)
      yields <- array(dim = c(nrow(rsim.simul$annual_Biomass),
                              length(harvesting)))
      for (i in 1:nrow(rsim.simul$annual_Biomass)) {
        adY <- rsim.simul$annual_Biomass[i, adname] * harvesting[1:n.aged.str]
        juvY <- rsim.simul$annual_Biomass[i, juvname] * harvesting[1:n.aged.str] * JuvFProp
        yield <- adY + juvY
        non.aged.yield <- rsim.simul$annual_Biomass[i, non.aged.groups] * harvesting[-c(1:n.aged.str)]
        yields[i,] <- append(yield, non.aged.yield)
      }
    } else if ((length(IDnames) > length(stanza.names)) == FALSE) {
      # RUN SIMULATION & COMPUTE YIELDS
      rsim.simul <- rsim.run(rsim.mod, method = integration.method,
                             years = 1:simul.years)
      yields <- array(dim = c(nrow(rsim.simul$annual_Biomass),
                              length(harvesting)))
      for (i in 1:nrow(rsim.simul$annual_Biomass)) {
        adY <- rsim.simul$annual_Biomass[i, adname] * harvesting[1:n.aged.str]
        juvY <- rsim.simul$annual_Biomass[i, juvname] * harvesting[1:n.aged.str] * JuvFProp
        yields[i,] <- adY + juvY
      }
    }
  } else if (aged.str == FALSE) {
    for (i in 1:length(sppname)) {
      rsim.mod <- adjust.fishing(Rsim.scenario = rsim.mod,
                                 parameter = "ForcedFRate",
                                 group = sppname[i],
                                 sim.year = (data.years+1):simul.years,
                                 sim.month = 0, value = harvesting[i])
    }
    # RUN SIMULATION & COMPUTE YIELDS
    rsim.simul <- rsim.run(rsim.mod, method = integration.method,
                           years = 1:simul.years)
    yields <- array(dim = c(nrow(rsim.simul$annual_Biomass), length(adname)))
    for (i in 1:nrow(rsim.simul$annual_Biomass)) {
      yields[i,] <- rsim.simul$annual_Biomass[i,sppname] * harvesting
    }
  }
  names <- c()
  for (i in 1:ncol(yields)) {
    names <- append(names, paste("Spp",i))
  }
  colnames(yields) <- names
  return(colMeans(tail(yields, n = avg.window)))
}

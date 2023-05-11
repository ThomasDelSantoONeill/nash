library(Rpath)
library(nash)
#### RPATH ####
# Parameters
groups <- c("GreySeal", "FishFeedingBirds", "JuvCod", "AdCod", "JuvHerring",
            "AdHerring", "JuvSprat", "AdSprat", "JuvFlounder", "AdFlounder",
            "SaduriaEntemon", "MytilusSp", "MacomaBalthica",
            "OtherMacrozoobenthos", "Meiobenthos", "Mysids", "OtherZooplankton",
            "PseudocalanusSp", "AcartiaSp", "TemoraSp", "Phytoplankton",
            "Detritus", "ACT0018", "ACT1824", "ACT2440", "PAS0012", "PAS1218",
            "PAS1840", "PEL0018", "PEL1824", "PEL2440", "PEL40OO")
types <- c(rep(0, 20), 1, 2, rep(3, 10))
stgroups <- c(rep(NA, 2), rep("Cod", 2), rep("Herring", 2), rep("Sprat", 2),
              rep("Flatfish", 2), rep(NA, 22))
Rpath.parameters <- create.rpath.params(group = groups, type = types,
                                        stgroup = stgroups)
biomass <- c(0.006, 0.002, 0.3537279, 0.33, 1.021176, 2.72, 4.691991, 4.48,
             0.3816071, 0.463, 2, 10, 45, 11.385, 6.8, NA, 4.006379, 1.930314,
             3.026768, 2.270881, 7.05, 1644.81, rep(NA, 10))
pb <- c(0.1, 0.1, 1.092, 0.885, 0.948, 0.79, 1.605, 1.07, 1.143, 0.762, 1.3,
        1.75, 0.4, 2, 6.17, 5, 30, 7, 25, 25, 200, rep(NA, 11))
qb <- c(16.28, 130, 7.649284, 3.81, 5.627138, 3, 8.705635, 4.63, 9.07962, 4.21,
        5, 8.73, 2, 10, 31.17, 15, 100, 27, 83, 83, rep(NA, 12))
Rpath.parameters$model[, Biomass := biomass]
Rpath.parameters$model[, PB := pb]
Rpath.parameters$model[, QB := qb]
Rpath.parameters$model[Group %in% "Mysids", EE := 0.7499999]
Rpath.parameters$model[, BioAcc := c(0.06, rep(0, 21), rep(NA, 10))]
Rpath.parameters$model[, Unassim := c(0.15, rep(0.2, 2), 0.17, rep(0.2, 16),
                                      rep(0, 2), rep(NA, 10))]
Rpath.parameters$model$DetInput <- c(rep(NA, 21), 0, rep(NA, 10))
Rpath.parameters$model[, Detritus := c(rep(1, 21), 0, rep(1, 10))]
# Fishery
ACT0018.l <- c(rep(0, 3), 0.04655146, 0.000964227, 0.01997571, 0.02041084,
               0.02239085, 0.000439406, 0.03830102, rep(0, 12), rep(NA,10))
ACT1824.l <- c(rep(0, 3), 0.0408974, 0.000690529, 0.01430556, 0.02132261,
               0.02339106, 0.000151976, 0.01324718, rep(0, 12), rep(NA, 10))
ACT2440.l <- c(rep(0, 3), 0.04313556, 0.000743249, 0.01539775, 0.0166325,
               0.01824598, 0.000713593, 0.06220077, rep(0, 12), rep(NA, 10))
PAS0012.l <- c(rep(0, 3), 0.05106486, 0.000447274, 0.01346155, rep(0, 3),
               0.04880729, rep(0, 12), rep(NA, 10))
PAS1218.l <- c(rep(0, 3), 0.03198545, 0, 6.7811e-07, rep(0, 3), 0.01100713,
               rep(0, 12), rep(NA, 10))
PAS1840.l <- c(rep(0, 3), 0.03337823, rep(0, 5), 0.004351989, rep(0, 12),
               rep(NA, 10))
PEL0018.l <- c(rep(0, 2), 6.74852e-05, 0.002968224, 1.83938e-05, 0.000833394,
               0.003225667, 0.01538038, 0, 9.76619e-06, rep(0, 12), rep(NA, 10))
PEL1824.l <- c(rep(0, 2), 9.13016e-05, 0.005086678, 8.18015e-05, 0.003706335,
               0.003422847, 0.01582962, 0, 0.000312952, rep(0, 12), rep(NA, 10))
PEL2440.l <- c(rep(0, 2), 0.000157981, 0.02342807, 0.005151592, 0.2334139,
               0.2251564, 0.8507619, 0, 0.002712224, rep(0, 12), rep(NA, 10))
PEL40OO.l <- c(rep(0, 2), 6.16847e-08, 0.000304323, 0.01543218, 0.06277447,
               0.02362915, 0.1140002, rep(0, 14), rep(NA, 10))
Rpath.parameters$model[, ACT0018 := ACT0018.l]
Rpath.parameters$model[, ACT1824 := ACT1824.l]
Rpath.parameters$model[, ACT2440 := ACT2440.l]
Rpath.parameters$model[, PAS0012 := PAS0012.l]
Rpath.parameters$model[, PAS1218 := PAS1218.l]
Rpath.parameters$model[, PAS1840 := PAS1840.l]
Rpath.parameters$model[, PEL0018 := PEL0018.l]
Rpath.parameters$model[, PEL1824 := PEL1824.l]
Rpath.parameters$model[, PEL2440 := PEL2440.l]
Rpath.parameters$model[, PEL40OO := PEL40OO.l]
# Discards
ACT0018.d <- c(rep(0, 2), 0.000424188, 0.001106558, rep(0, 4), 0.00260477,
               0.02900947, rep(0, 12), rep(NA, 10))
ACT1824.d <- c(rep(0, 2), 0.000376657, 0.001076847, rep(0, 4), 0.00078248,
               0.008714528, rep(0, 12), rep(NA, 10))
ACT2440.d <- c(rep(0, 2), 0.000248775, 0.001118913, rep(0, 4), 0.003622325,
               0.04034203, rep(0, 12), rep(NA, 10))
PAS0012.d <- c(rep(0, 2), 5.55097E-05, 0.000497353, rep(0, 5), 0.01116253,
               rep(0, 12), rep(NA, 10))
PAS1218.d <- c(rep(0, 2), 4.63214E-05, 0.000415099, rep(0, 5), 0.002194796,
               rep(0, 12), rep(NA, 10))
PAS1840.d <- c(rep(0, 2), 7.67498E-05, 0.000687742, rep(0, 5), 0.000615467,
               rep(0, 12), rep(NA, 10))
PEL0018.d <- c(rep(0, 2), 3.84098E-05, 3.34277E-05, rep(0, 18), rep(NA, 10))
PEL1824.d <- c(rep(0, 2), 6.19798E-05, 7.36702E-05, rep(0, 18), rep(NA, 10))
PEL2440.d <- c(rep(0, 2), 6.93338E-05, 0.000195279, rep(0, 5), 3.4375E-06,
               rep(0, 12), rep(NA, 10))
PEL40OO.d <- c(rep(0, 2), 1.08246E-06, 8.25504E-06, rep(0, 18), rep(NA, 10))
Rpath.parameters$model[, ACT0018.disc := ACT0018.d]
Rpath.parameters$model[, ACT1824.disc := ACT1824.d]
Rpath.parameters$model[, ACT2440.disc := ACT2440.d]
Rpath.parameters$model[, PAS0012.disc := PAS0012.d]
Rpath.parameters$model[, PAS1218.disc := PAS1218.d]
Rpath.parameters$model[, PAS1840.disc := PAS1840.d]
Rpath.parameters$model[, PEL0018.disc := PEL0018.d]
Rpath.parameters$model[, PEL1824.disc := PEL1824.d]
Rpath.parameters$model[, PEL2440.disc := PEL2440.d]
Rpath.parameters$model[, PEL40OO.disc := PEL40OO.d]
# Stanzas
Rpath.parameters$stanzas$stgroups[, VBGF_Ksp := c(0.230000004172325,
                                                  0.430000007152557,
                                                  0.509999990463257,
                                                  0.200000002980232)]
Rpath.parameters$stanzas$stgroups[, Wmat := c(0.129999995231628,
                                              0.379999995231628,
                                              0.259999990463257,
                                              0.100000001490116)]
Rpath.parameters$stanzas$stindiv[, First := c(0, 36, 0, 24, 0, 24, 0, 36)]
Rpath.parameters$stanzas$stindiv[, Last := c(35, 400, 23, 400, 23, 400, 35,
                                             400)]
Rpath.parameters$stanzas$stindiv[, Z := c(1.092, 0.885, 0.948, 0.79,
                                          1.605, 1.07, 1.143, 0.762)]
Rpath.parameters$stanzas$stindiv[, Leading := c(rep(c(F, T), 4))]
Rpath.parameters <- rpath.stanzas(Rpath.parameters)
# Diet
GreySeal.diet <- c(rep(0, 2), 0.07827354, 0.03178926, 0.002591124, 0.375759, 0,
                   0.2067591, 0, 0.03093355, rep(0, 12), 0.2738944)
FishFeedingBirds.diet <- c(rep(0, 6), 0.02, 0.93, rep(0, 14), 0.05)
JuvCod.diet <- c(rep(0, 4), 0.05209047, 0.07938787, 0.1764397, 0.2520024,
                 rep(0, 2), 0.2036188, rep(0, 2), 0.01193309, 0, 0.2145273,
                 0, 0.01000043, rep(0, 5))
AdCod.diet <- c(rep(0, 2), 0.01580723, 0, 0.04505872, 0.4874896, 0.1508785,
                0.1815, 0.03000606, 0, 0.0565, 0, 0.0002534152, 0.002240333,
                0, 0.03045089, rep(0, 7))
JuvHerring.diet <- c(rep(0, 15), 0.1, 0.2, 0.1, 0.1, 0.5, rep(0, 3))
AdHerring.diet <- c(rep(0, 6), 0.01, rep(0, 6), 0.1, 0, 0.25, 0.15, 0.1, 0.02,
                    0.37, rep(0, 3))
JuvSprat.diet <- c(rep(0, 16), 0.39, 0.15, 0.21, 0.25, rep(0, 3))
AdSprat.diet <- c(rep(0, 16), 0.33, 0.17, 0.09, 0.41, rep(0, 3))
JuvFlounder.diet <- c(rep(0, 13), 0.6, 0.1, 0.3, rep(0, 7))
AdFlounder.diet <- c(rep(0, 10), 0.08255491, 0.4532953, 0.3394456, 0.1247042,
                     rep(0, 9))
SaduriaEntemon.diet <- c(rep(0, 13), 0.59761, 0.003984, 0.398406, rep(0, 7))
MytilusSp.diet <- c(rep(0, 20), 0.8, 0.2, 0)
MacomaBalthica.diet <- c(rep(0, 20), 0.4, 0.6, 0)
OtherMacrozoobenthos.diet <- c(rep(0, 13), 0.1, 0.3, rep(0, 6), 0.6, 0)
Meiobenthos.diet <- c(rep(0, 14), 0.02, rep(0, 6 ), 0.98, 0)
Mysids.diet <- c(rep(0, 16), 0.05, 0.05, 0.1, 0.2, 0.3, 0.2, 0.1)
OtherZooplankton.diet <- c(rep(0, 20), 1, rep(0, 2))
PseudocalanusSp.diet <- c(rep(0, 20), 1, rep(0, 2))
AcartiaSp.diet <- c(rep(0, 20), 1, rep(0, 2))
TemoraSp.diet <- c(rep(0, 20), 1, rep(0, 2))
Phytoplankton.diet <- c(rep(NA, 23))
Rpath.parameters$diet[, GreySeal := GreySeal.diet]
Rpath.parameters$diet[, FishFeedingBirds := FishFeedingBirds.diet]
Rpath.parameters$diet[, JuvCod := JuvCod.diet]
Rpath.parameters$diet[, AdCod := AdCod.diet]
Rpath.parameters$diet[, JuvHerring := JuvHerring.diet]
Rpath.parameters$diet[, AdHerring := AdHerring.diet]
Rpath.parameters$diet[, JuvSprat := JuvSprat.diet]
Rpath.parameters$diet[, AdSprat := AdSprat.diet]
Rpath.parameters$diet[, JuvFlounder := JuvFlounder.diet]
Rpath.parameters$diet[, AdFlounder := AdFlounder.diet]
Rpath.parameters$diet[, SaduriaEntemon := SaduriaEntemon.diet]
Rpath.parameters$diet[, MytilusSp := MytilusSp.diet]
Rpath.parameters$diet[, MacomaBalthica := MacomaBalthica.diet]
Rpath.parameters$diet[, OtherMacrozoobenthos := OtherMacrozoobenthos.diet]
Rpath.parameters$diet[, Meiobenthos := Meiobenthos.diet]
Rpath.parameters$diet[, Mysids := Mysids.diet]
Rpath.parameters$diet[, OtherZooplankton := OtherZooplankton.diet]
Rpath.parameters$diet[, PseudocalanusSp := PseudocalanusSp.diet]
Rpath.parameters$diet[, AcartiaSp := AcartiaSp.diet]
Rpath.parameters$diet[, TemoraSp := TemoraSp.diet]
Rpath.parameters$diet[, Phytoplankton := Phytoplankton.diet]
check.rpath.params(Rpath.parameters)
# Rpath model
Rpath.model <- rpath(Rpath.parameters, eco.name = 'Baltic Sea Fishery')
#### RSIM ####
sim.years <- 500
Rsim.model <- rsim.scenario(Rpath.model, Rpath.parameters, years = 1:sim.years)
# VVs
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "JuvHerring", groupto = "JuvCod",
                              value = 1)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "AdHerring", groupto = "JuvCod",
                              value = 1)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "AdSprat", groupto = "JuvCod",
                              value = 1.015069)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "SaduriaEntemon", groupto = "JuvCod",
                              value = 1E+10)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "JuvHerring", groupto = "AdCod",
                              value = 1)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "AdHerring", groupto = "AdCod",
                              value = 1)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "JuvSprat", groupto = "AdCod",
                              value = 1E+10)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "AdSprat", groupto = "AdCod",
                              value = 454847.7)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "OtherZooplankton",
                              groupto = "JuvHerring",
                              value = 1603350)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "PseudocalanusSp",
                              groupto = "JuvHerring",
                              value = 1.000007)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "TemoraSp",
                              groupto = "JuvHerring",
                              value = 159664.6)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "Mysids",
                              groupto = "AdHerring",
                              value = 1E+10)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "PseudocalanusSp",
                              groupto = "AdHerring",
                              value = 608776.8)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "TemoraSp",
                              groupto = "AdHerring",
                              value = 3.465625E+07)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "TemoraSp",
                              groupto = "JuvSprat",
                              value = 1)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "OtherMacrozoobenthos",
                              groupto = "SaduriaEntemon",
                              value = 1E+10)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "Mysids",
                              groupto = "SaduriaEntemon",
                              value = 1E+10)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "OtherZooplankton",
                              groupto = "Mysids",
                              value = 1)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "AcartiaSp",
                              groupto = "Mysids",
                              value = 1)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "Phytoplankton",
                              groupto = "Mysids",
                              value = 1)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "Detritus",
                              groupto = "Mysids",
                              value = 1)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "Phytoplankton",
                              groupto = "OtherZooplankton",
                              value = 1E+10)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "Phytoplankton",
                              groupto = "PseudocalanusSp",
                              value = 20.51294)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "Phytoplankton",
                              groupto = "AcartiaSp",
                              value = 2.105472)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "Phytoplankton",
                              groupto = "TemoraSp",
                              value = 1)
# # Effort
# Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedEffort",
#                              group = "ACT0018", sim.year = 1:sim.years,
#                              sim.month = 0,
#                              value = c(1, 0.801506, 0.8285093, 0.523856,
#                                        0.495454, 0.5060846, 0.5728487,
#                                        0.6282578, 0.6998034,
#                                        rep(0.6429319, sim.years)))
# Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedEffort",
#                              group = "ACT1824", sim.year = 1:sim.years,
#                              sim.month = 0,
#                              value = c(1, 0.8626296, 0.9740052, 0.617117,
#                                        0.5097772, 0.4445865, 0.5331612,
#                                        0.5614181, 0.6945872,
#                                        rep(0.65593, sim.years)))
# Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedEffort",
#                              group = "ACT2440", sim.year = 1:sim.years,
#                              sim.month = 0,
#                              value = c(1, 0.7602245, 0.8028268, 0.4842178,
#                                        0.4352735, 0.3822806, 0.4145548,
#                                        0.5021464, 0.534105,
#                                        rep(0.4101246, sim.years)))
# Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedEffort",
#                              group = "PAS0012", sim.year = 1:sim.years,
#                              sim.month = 0,
#                              value = c(1, 0.9044925, 0.8223968,
#                                        0.7229398, 0.656688, 0.64854,
#                                        0.5843078, 0.572095, 0.6155943,
#                                        rep(0.5987266, sim.years)))
# Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedEffort",
#                              group = "PAS1218", sim.year = 1:sim.years,
#                              sim.month = 0,
#                              value = c(1, 0.7475984, 0.716076,
#                                        0.4678181, 0.3006322, 0.2879224,
#                                        0.2564141, 0.1931863, 0.2553373,
#                                        rep(0.1801596, sim.years)))
# Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedEffort",
#                              group = "PAS1840",
#                              sim.year = 1:sim.years, sim.month = 0,
#                              value = c(1, 0.4276912, 0.2924765,
#                                        0.2499503, 0.2238991, 0.1944442,
#                                        0.1917399, 0.1724458, 0.1227148,
#                                        rep(0.07997237, sim.years)))
# Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedEffort",
#                              group = "PEL0018",
#                              sim.year = 1:sim.years, sim.month = 0,
#                              value = c(1, 0.7950546, 0.6204393,
#                                        0.3762521, 0.3291099, 0.6593255,
#                                        0.6238568, 0.8577416, 1.412984,
#                                        rep(1.587065, sim.years)))
# Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedEffort",
#                              group = "PEL1824",
#                              sim.year = 1:sim.years, sim.month = 0,
#                              value = c(1, 1.135785, 1.081941,
#                                        1.081769, 0.6270754, 1.325389,
#                                        1.192106, 1.638697, 1.920583,
#                                        rep(2.601135, sim.years)))
# Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedEffort",
#                              group = "PEL2440",
#                              sim.year = 1:sim.years, sim.month = 0,
#                              value = c(1, 0.8242539, 0.7275792,
#                                        0.7363582, 0.5667459, 0.5695906,
#                                        0.490158, 0.4687528, 0.3750999,
#                                        rep(0.3864659, sim.years)))
# Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedEffort",
#                              group = "PEL40OO",
#                              sim.year = 1:sim.years, sim.month = 0,
#                              value = c(1, 0.9930536, 1.005373,
#                                        1.09185, 1.13515, 1.186169,
#                                        1.039734, 0.9533734, 0.6396784,
#                                        rep(0.6923758, sim.years)))
gearname <-
  colnames(Rsim.model$fishing$ForcedEffort[, 2:ncol(Rsim.model$fishing$ForcedEffort)])
for (i in 1:length(gearname)) {
  Rsim.model <<- adjust.fishing(Rsim.model, parameter = "ForcedEffort",
                                group = gearname[i], sim.year = 1:sim.years,
                                sim.month = 0, value = 0)
}
# Frate
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "JuvCod", sim.year = 1:sim.years,
                             sim.month = 0,
                             value = c(0.004850723, 0.00401009, 0.004006988,
                                       0.002912952, 0.002372227, 0.002672406,
                                       0.002734797, 0.003128057, 0.00360948,
                                       rep(0.00374565, sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "AdCod", sim.year = 1:sim.years,
                             sim.month = 0,
                             value = c(0.8606467, 0.6712403, 0.6557999,
                                       0.4778628, 0.4044484, 0.399199,
                                       0.4027945, 0.4235261, 0.465305,
                                       rep(0.4342084, sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "JuvHerring", sim.year = 1:sim.years,
                             sim.month = 0,
                             value = c(0.02304128, 0.02156023, 0.0213471,
                                       0.02188938, 0.02148676, 0.02225782,
                                       0.01975114, 0.01850788, 0.01352732,
                                       rep(0.01426122, sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "AdHerring", sim.year = 1:sim.years,
                             sim.month = 0,
                             value = c(0.1337755, 0.1146456, 0.107126,
                                       0.1033898, 0.08782185, 0.08969103,
                                       0.08012354, 0.0779647, 0.06486452,
                                       rep(0.06663042, sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "JuvSprat", sim.year = 1:sim.years,
                             sim.month = 0,
                             value = c(0.06687993, 0.05603178, 0.05206996,
                                       0.04868216, 0.039612, 0.04030403,
                                       0.03644056, 0.036145, 0.03168812,
                                       rep(0.03225251, sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "AdSprat", sim.year = 1:sim.years,
                             sim.month = 0,
                             value = c(0.2366071, 0.2001461, 0.1822011,
                                       0.1805462, 0.1467681, 0.1517046,
                                       0.1332288, 0.1301285, 0.1084464,
                                       rep(0.1139571, sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "JuvFlounder", sim.year = 1:sim.years,
                             sim.month = 0,
                             value = c(0.02178824, 0.01714403, 0.01811623,
                                       0.01119189, 0.01014636, 0.009469409,
                                       0.01058561, 0.01209205, 0.01335201,
                                       rep(0.01139497, sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "AdFlounder", sim.year = 1:sim.years,
                             sim.month = 0,
                             value = c(0.5896167, 0.4744886, 0.4795426,
                                       0.3273892, 0.2923977, 0.2778762,
                                       0.2891278, 0.3145123, 0.344834,
                                       rep(0.3030173, sim.years)))

# Egg production
Rsim.model <- adjust.forcing(Rsim.model, parameter = "ForcedRecs",
                             group = "AdHerring", sim.year = 1:sim.years,
                             sim.month = 0,
                             value = c(1, 1.077727, 1.193518, 1.06507, 1.121166,
                                       1.051862, 1.008342, 1.094405, 1.137283,
                                       1.173944, rep(1.180972, sim.years)))
Rsim.model <- adjust.forcing(Rsim.model, parameter = "ForcedRecs",
                             group = "AdSprat", sim.year = 1:sim.years,
                             sim.month = 0,
                             value = c(1, 1.077727, 1.193518, 1.06507, 1.121166,
                                       1.051862, 1.008342, 1.094405, 1.137283,
                                       1.173944, rep(1.180972, sim.years)))
Rsim.model <- adjust.forcing(Rsim.model, parameter = "ForcedRecs",
                             group = "AdCod", sim.year = 1:sim.years,
                             sim.month = 0,
                             value = c(1, 0.5492537, 2.021642, 1.013134,
                                       1.727612, 0.9067164, 1.278358, 0.7835821,
                                       0.9004478, 0.8977277,
                                       rep(0.9011803, sim.years)))

# Forcing functions
Rsim.model <- adjust.forcing(Rsim.model, parameter = "ForcedSearch",
                             group = "OtherMacrozoobenthos",
                             sim.year = 1:sim.years, sim.month = 0,
                             value = c(1, 0.5258236, 0.7198213, 0.561654,
                                       0.5543391, 0.8611185, 0.6287283,
                                       0.3989885, 0.8400382,
                                       rep(0.8400382, sim.years)))
Rsim.model <- adjust.forcing(Rsim.model, parameter = "ForcedSearch",
                             group = "Mysids",
                             sim.year = 1:sim.years, sim.month = 0,
                             value = c(1, 0.5258236, 0.7198213, 0.561654,
                                       0.5543391, 0.8611185, 0.6287283,
                                       0.3989885, 0.8400382,
                                       rep(0.8400382, sim.years)))
Rsim.model <- adjust.forcing(Rsim.model, parameter = "ForcedSearch",
                             group = "AcartiaSp", sim.year = 1:sim.years,
                             sim.month = 0,
                             value = c(1, 0.8247877, 0.8192357, 1.455585,
                                       1.365937, 1.247071, 0.6618223, 0.5600914,
                                       1.009634, rep(0.8255835, sim.years)))
Rsim.model <- adjust.forcing(Rsim.model, parameter = "ForcedSearch",
                             group = "TemoraSp", sim.year = 1:sim.years,
                             sim.month = 0,
                             value = c(1, 0.8247877, 0.8192357, 1.455585,
                                       1.365937, 1.247071, 0.6618223, 0.5600914,
                                       1.009634, rep(0.8255835, sim.years)))
Rsim.model <- adjust.forcing(Rsim.model, parameter = "ForcedPrey",
                             group = "Phytoplankton", sim.year = 1:sim.years,
                             sim.month = 0,
                             value = c(1, 0.9074827, 0.9369552, 1.107766,
                                       1.04327, 1.052554, 0.8505842, 0.9891393,
                                       1.037044, rep(0.9645875, sim.years)))

# Running the model
Rsim.run <- rsim.run(Rsim.model, method = "RK4", years = 1:sim.years)

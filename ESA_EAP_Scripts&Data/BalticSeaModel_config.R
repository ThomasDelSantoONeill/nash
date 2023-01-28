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
biomass <- c(0.006, 0.002, 0.3424508, 0.33, 1.003478, 2.32929, 3.724431,
             2.390462, 0.4216416, 0.463, 2, 10, 45, 11.385, 6.8, NA, 4.006379,
             1.930314, 3.026768, 2.270881, 7.05, 1644.81, rep(NA, 10))
pb <- c(0.1, 0.1, 1.062, 0.885, 1.175847, 0.7838983, 1.8645, 1.243, 1.184425,
        0.7896167, 1.3, 1.75, 0.4, 2, 6.17, 5, 20, 7, 20, 20, 200, rep(NA, 11))
qb <- c(16.28, 130, 7.594003, 3.81, 5.810652, 3, 8.901528, 4.63, 9.106646, 4.21,
        5, 8.73, 2, 10, 31.17, 15, 100, 27, 83, 83, rep(NA, 12))
Rpath.parameters$model[, Biomass := biomass]
Rpath.parameters$model[, PB := pb]
Rpath.parameters$model[, QB := qb]
Rpath.parameters$model[Group %in% "Mysids", EE := 0.7499999]
Rpath.parameters$model[, BioAcc := c(0.00036, rep(0, 21), rep(NA, 10))]
Rpath.parameters$model[, Unassim := c(rep(0.2, 3), 0.17, rep(0.2, 16),
                                      rep(0, 2), rep(NA, 10))]
Rpath.parameters$model$DetInput <- c(rep(NA, 21), 0, rep(NA, 10))
Rpath.parameters$model[, Detritus := c(rep(1, 21), 0, rep(1, 10))]
# Fishery
ACT0018.l <- c(rep(0, 3), 0.04591162, 0.00082648, 0.01712203, 0.01854215,
               0.02033977, 0.000439406, 0.03830102, rep(0, 12), rep(NA, 10))
ACT1824.l <- c(rep(0, 3), 0.04033527, 0.000591882, 0.01226191, 0.01937044,
               0.02124836, 0.000151976, 0.01324718, rep(0, 12), rep(NA, 10))
ACT2440.l <- c(rep(0, 3), 0.04254267, 0.000637071, 0.01319807, 0.01510973,
               0.01657458, 0.000713593, 0.06220077, rep(0, 12), rep(NA, 10))
PAS0012.l <- c(rep(0, 3), 0.05036297, 0.000383378, 0.01153847, rep(0, 3),
               0.04880729, rep(0, 12), rep(NA, 10))
PAS1218.l <- c(rep(0, 3), 0.031545810, 0, 5.81237E-07, rep(0, 3), 0.01100713,
               rep(0, 12), rep(NA, 10))
PAS1840.l <- c(rep(0, 3), 0.03291944, rep(0, 5), 0.004351989, rep(0, 12),
               rep(NA, 10))
PEL0018.l <- c(rep(0, 2), 0.003049315, 0.002927426, 1.57661E-05, 0.000714338,
               0.002930345, 0.01397149, 0, 9.76619E-06, rep(0, 12), rep(NA, 10))
PEL1824.l <- c(rep(0, 2), 0.004125458, 0.005016762, 7.01156E-05, 0.003176858,
               0.003109472, 0.01437957, 0, 0.000312952, rep(0, 12), rep(NA, 10))
PEL2440.l <- c(rep(0, 2), 0.007138357, 0.02310606, 0.004415651, 0.2000691,
               0.2045425, 0.7728292, 0, 0.002712224, rep(0, 12), rep(NA, 10))
PEL40OO.l <- c(rep(0, 2), 2.78722E-06, 0.00030014, 0.01322758, 0.05380669,
               0.02146581, 0.1035574, rep(0, 14), rep(NA, 10))
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
ACT0018.d <- c(rep(0, 2), 0.000952843, 0.000507916, rep(0, 4), 0.00260477,
               0.02900947, rep(0, 12), rep(NA, 10))
ACT1824.d <- c(rep(0, 2), 0.000846075, 0.000494278, rep(0, 4), 0.00078248,
               0.008714528, rep(0, 12), rep(NA, 10))
ACT2440.d <- c(rep(0, 2), 0.000558817, 0.000513587, rep(0, 4), 0.003622325,
               0.04034203, rep(0, 12), rep(NA, 10))
PAS0012.d <- c(rep(0, 2), 0.00012469, 0.000228288, rep(0, 5), 0.01116253,
               rep(0, 12), rep(NA, 10))
PAS1218.d <- c(rep(0, 2), 0.000104051, 0.000190533, rep(0, 5), 0.002194796,
               rep(0, 12), rep(NA, 10))
PAS1840.d <- c(rep(0, 2), 0.000172401, 0.000315677, rep(0, 5), 0.000615467,
               rep(0, 12), rep(NA, 10))
PEL0018.d <- c(rep(0, 2), 8.6279E-05, 1.53435E-05, rep(0, 18), rep(NA, 10))
PEL1824.d <- c(rep(0, 2), 0.000139224, 3.3815E-05, rep(0, 18), rep(NA, 10))
PEL2440.d <- c(rep(0, 2), 0.000155743, 8.96341E-05, rep(0, 5), 3.4375E-06,
               rep(0, 12), rep(NA, 10))
PEL40OO.d <- c(rep(0, 2), 2.4315E-06, 3.78911E-06, rep(0, 18), rep(NA, 10))
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
Rpath.parameters$stanzas$stgroups[, VBGF_Ksp := c((0.230), (0.430), (0.510),
                                                  (0.200))]
Rpath.parameters$stanzas$stgroups[, Wmat := c(0.130, 0.380, 0.260, 0.100)]
Rpath.parameters$stanzas$stindiv[, First := c(0, 36, 0, 24, 0, 24, 0, 36)]
Rpath.parameters$stanzas$stindiv[, Last := c(35, 400, 23, 400, 23, 400, 35,
                                             400)]
Rpath.parameters$stanzas$stindiv[, Z := c(1.062, 0.885, 1.175847, 0.7838983,
                                          1.8645, 1.243, 1.184425, 0.7896167)]
Rpath.parameters$stanzas$stindiv[, Leading := c(rep(c(F, T), 4))]
Rpath.parameters <- rpath.stanzas(Rpath.parameters)
# Diet
GreySeal.diet <- c(rep(0, 2), 0.07827354, 0.03178926, 0.002591124, 0.375759, 0,
                   0.2067591, 0, 0.03093355, rep(0, 12), 0.2738944)
FishFeedingBirds.diet <- c(rep(0, 6), 0.02, 0.93, rep(0, 14), 0.05)
JuvCod.diet <- c(rep(0, 4), 0.07206677, 0.05935177, 0.2998774, 0.1259485,
                 rep(0, 2), 0.1188794, rep(0, 2), 0.009366813, 0, 0.1705682,
                 0, 0.009995912, rep(0, 4), 0.1339452)
AdCod.diet <- c(rep(0, 2), 0.01499608, 0, 0.04502488, 0.4198902, 0.1199686,
                0.2779273, 0.02999822, 0, 0.05098667, 0, 0.0002131353,
                0.001266966, 0, 0.0397279, rep(0, 7))
JuvHerring.diet <- c(rep(0, 15), 0.1, 0.2, 0.1, 0.1, 0.5, rep(0, 3))
AdHerring.diet <- c(rep(0, 6), 0.01, 0, rep(0, 5), 0.1, 0, 0.25, 0.15, 0.1,
                    0.02, 0.37, rep(0, 3))
JuvSprat.diet <- c(rep(0, 15), 0.0005, 0.389875, 0.149875, 0.209875, 0.249875,
                   rep(0, 3))
AdSprat.diet <- c(rep(0, 15), 0.001, 0.32975, 0.16975, 0.08975, 0.40975,
                  rep(0, 3))
JuvFlounder.diet <- c(rep(0, 13), 0.6, 0.1, 0.3, rep(0, 7))
AdFlounder.diet <- c(rep(0, 10), 0.08255491, 0.4532953, 0.3394456,
                     0.1247042, rep(0, 9))
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
                              group = "JuvSprat", groupto = "JuvCod",
                              value = 1E+10)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "AdHerring", groupto = "AdCod",
                              value = 1+1e-6)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "JuvSprat", groupto = "AdCod",
                              value = 2.054679E+08)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "AdSprat", groupto = "AdCod",
                              value = 17.36081)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "TemoraSp", groupto = "JuvHerring",
                              value = 7.870935E+07)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "TemoraSp", groupto = "AdHerring",
                              value = 16127.11)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "Mysids", groupto = "JuvSprat",
                              value = 40)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "OtherZooplankton", groupto = "JuvSprat",
                              value = 40)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "PseudocalanusSp", groupto = "JuvSprat",
                              value = 40)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "AcartiaSp", groupto = "JuvSprat",
                              value = 40)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "TemoraSp", groupto = "JuvSprat",
                              value = 40)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "Mysids", groupto = "AdSprat",
                              value = 5)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "OtherZooplankton", groupto = "AdSprat",
                              value = 5)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "PseudocalanusSp", groupto = "AdSprat",
                              value = 5)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "AcartiaSp", groupto = "AdSprat",
                              value = 5)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "TemoraSp", groupto = "AdSprat",
                              value = 5)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "OtherMacrozoobenthos",
                              groupto = "SaduriaEntemon", value = 22508.95)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "Mysids", groupto = "SaduriaEntemon",
                              value = 22791.77)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "AcartiaSp", groupto = "Mysids",
                              value = 1+1e-6)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "TemoraSp", groupto = "Mysids",
                              value = 1E+10)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "Phytoplankton", groupto = "Mysids",
                              value = 1+1e-6)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "Detritus", groupto = "Mysids",
                              value = 1+1e-6)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "Phytoplankton", groupto = "AcartiaSp",
                              value = 2.300091)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "Phytoplankton", groupto = "TemoraSp",
                              value = 1+1e-6)
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
                             value = c(0.05098097, 0.04536942, 0.04135337,
                                       0.0369213, 0.02670299, 0.0382447,
                                       0.03498893, 0.04256835, 0.04975314,
                                       rep(0.05955861,sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "AdCod", sim.year = 1:sim.years,
                             sim.month = 0,
                             value = c(0.840488, 0.6556228, 0.6403595,
                                       0.4668901, 0.3951078, 0.3900644,
                                       0.3934016, 0.4135607, 0.4543019,
                                       rep(0.4240381, sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "JuvHerring", sim.year = 1:sim.years,
                             sim.month = 0,
                             value = c(0.02009802, 0.01880615, 0.01862025,
                                       0.01909326, 0.01874207, 0.01941464,
                                       0.01722815, 0.01614371, 0.01179936,
                                       rep(0.01243951, sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "AdHerring", sim.year = 1:sim.years,
                             sim.month = 0,
                             value = c(0.1338983, 0.1147509, 0.1072243,
                                       0.1034847, 0.08790248, 0.08977338,
                                       0.08019711, 0.07803628, 0.06492407,
                                       rep(0.0666916, sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "JuvSprat", sim.year = 1:sim.years,
                             sim.month = 0,
                             value = c(0.07654068, 0.06412552, 0.05959143,
                                       0.05571426, 0.04533393, 0.04612591,
                                       0.04170437, 0.04136612, 0.03626544,
                                       rep(0.03691136, sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "AdSprat", sim.year = 1:sim.years,
                             sim.month = 0,
                             value = c(0.4028093, 0.3407367, 0.3101864,
                                       0.3073691, 0.2498638, 0.2582679,
                                       0.226814, 0.2215359, 0.1846235,
                                       rep(0.1940051, sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "JuvFlounder", sim.year = 1:sim.years,
                             sim.month = 0,
                             value = c(0.01971947, 0.01551622, 0.01639611,
                                       0.01012923, 0.009182973, 0.008570299,
                                       0.009580513, 0.01094392, 0.01208425,
                                       rep(0.01031303, sim.years)))
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
                                       0.3989885, rep(0.8400382, sim.years)))
Rsim.model <- adjust.forcing(Rsim.model, parameter = "ForcedSearch",
                             group = "Mysids", sim.year = 1:sim.years,
                             sim.month = 0,
                             value = c(1, 0.5258236, 0.7198213, 0.561654,
                                       0.5543391, 0.8611185, 0.6287283,
                                       0.3989885, rep(0.8400382, sim.years)))
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

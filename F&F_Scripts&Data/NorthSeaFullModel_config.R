library(Rpath)
library(nash)
#### RPATH ####
# Parameters
groups <- c("BaleenWhales", "ToothedWhales", "Seals", "SurfaceFeedingSeabirds",
            "JuvenileSharks", "Spurdog", "LargePiscivorousSharks",
            "SmallSharks", "JuvenileRays", "StarryRayOthers",
            "ThornbackAndSpottedRay", "SkateCuckooRay", "JuvCod", "AduCod",
            "JuvWhiting", "AduWhiting", "JuvHaddock", "AduHaddock", "JuvSaithe",
            "AduSaithe", "Hake", "BlueWhiting", "NorwayPout",
            "OtherLargeGadoids", "OtherSmallGadoids", "Monkfish", "Gurnards",
            "JuvHerring", "AduHerring", "Sprat", "Mackerel", "HorseMackerel",
            "Sandeels", "Plaice", "Dab", "LongRoughDab", "Flounder", "Sole",
            "LemonSole", "Witch", "Turbot", "Megrim", "Halibut", "Dragonets",
            "CatfishWolfFish", "LargeDemersalFish", "SmallDemersalFish",
            "MiscellaneousFilterfeedingPelagicFish", "SquidAndCuttlefish",
            "FishLarvae", "CarnivorousZooplankton",
            "HerbivorousAndOmnivorousZooplanktonCopepods",
            "GelatinousZooplankton", "LargeCrabs", "Nephrops",
            "EpifaunalMacrobenthosMobileGrazers", "InfaunalMacrobenthos",
            "Shrimp", "SmallMobileEpifaunaSwarmingCrustaceans",
            "SmallInfaunaPolychaetes", "SessileEpifauna", "Meiofauna",
            "BenthicMicrofloraInclBacteriaProtozoa",
            "PlanktonicMicrofloraInclBacteriaProtozoa", "DivingSeabirds",
            "Phytoplankton", "DetritusDOMWaterColumn", "DetritusPOMSediment",
            "Discards", "DemersalTrawlAndDemSeine", "BeamTrawl",
            "IndustrialTrawl", "PelagicTrawl", "DriftAndFixedNets",
            "NephropsTrawl", "GearsUsingHooks", "ShrimpTrawlers", "Dredges",
            "Pots", "Other")
types <- c(rep(0, 65), 1, rep(2, 3), rep(3, 11))
stgroups <- c(rep(NA, 12), rep("Cod", 2), rep("Whiting", 2), rep("Haddock", 2),
              rep("Saithe", 2), rep(NA, 7), rep("Herring", 2), rep(NA, 51))
Rpath.parameters <- create.rpath.params(group = groups, type = types,
                                        stgroup = stgroups)
biomass <- c(0.067, 0.017, 0.008, 0.002, 0.001, 0.13, 0.001, 0.002, 0.268, 0.39,
             0.066, 0.05, 0.1001798, 0.13, 0.02666127, 0.43, 0.007704244, 0.12,
             0.1159751, 0.21, 0.014, 0.23, 1.31, rep(NA,2), 0.042, 0.18,
             0.1432551, 2.68, 0.579, 0.75, 0.75, 1.85, 0.58, 2.8, 0.35, 0.25,
             0.135, 0.14, 0.082, 0.027, 0.034, 0.033, 0.045, 0.014, rep(NA, 3),
             0.06, rep(NA, 2), 16, 0.066, 1.2, 0.98, 78, 136, 0.074, 30, 150,
             105, NA, 0.105, 1.44, 0.004, 7.5, 25, 25, 1e-06, rep(NA, 11))
pb <- c(0.02, 0.02, 0.09, 0.237, 0.5, 0.48, 0.44, 0.51, 0.66, 0.66, 0.78, 0.35,
        1.79, 1.19, 2.36, 0.89, 2, 1.14, 1, 0.88, 0.82, 2.5, 2.2, 1, 1.8, 0.7,
        0.82, 1.31, 0.8, 2.28, 0.6, 0.58, 2.28, 0.85, 0.672, 0.7, 1.1, 0.8,
        0.864, 0.9, 0.86, 0.72, 0.16, 1.44, 0.48, 0.55, 1.42, 4, 4.5, 4, 4, 9.2,
        2.9, 0.55, 0.37, 0.3883938, 1, 11, 1.36, 0.9, 0.26, 35, 9470, 571, 0.45,
        286.6667, rep(NA, 14))
qb <- c(9.9, 17.63, 26.842, 77.28, 2.5, 2, 1.6, 2.96, 1.7, 1.7, 2.3, 1.8,
        8.01195, 3.5, 17.40203, 5.46, 7.685435, 2.35, 8.51089, 3.600001, 2.2,
        9.06, 5.05, 2.5, 4, 1.7, 3.2, 11.53677, 4.34, 5.28, 1.73, 3.5, 5.24,
        3.42, 4, 4, 3.2, 3.1, 4.32, 3, 2.1, 3.1, 3.14, 6.9, 1.7, 2.54, 3.7,
        10.19, 15, 20, NA, 30, rep(NA, 5), 22, rep(NA,3), 125, rep(NA, 2),
        86.97, rep(NA, 15))
Rpath.parameters$model[, Biomass := biomass]
Rpath.parameters$model[, PB := pb]
Rpath.parameters$model[, QB := qb]
Rpath.parameters$model[Group %in% "OtherLargeGadoids", EE := 0.95]
Rpath.parameters$model[Group %in% c("OtherSmallGadoids", "SmallDemersalFish",
                                    "MiscellaneousFilterfeedingPelagicFish",
                                    "FishLarvae", "CarnivorousZooplankton",
                                    "Meiofauna"), EE := 0.99]
Rpath.parameters$model[Group %in% "LargeDemersalFish", EE := 0.90]
Rpath.parameters$model[Group %in% "CarnivorousZooplankton", ProdCons := 0.32]
Rpath.parameters$model[Group %in% "GelatinousZooplankton", ProdCons := 0.45]
Rpath.parameters$model[Group %in% c("LargeCrabs", "Nephrops",
                                    "EpifaunalMacrobenthosMobileGrazers",
                                    "SessileEpifauna"), ProdCons := 0.2]
Rpath.parameters$model[Group %in% c("InfaunalMacrobenthos",
                                    "SmallInfaunaPolychaetes"), ProdCons := 0.3]
Rpath.parameters$model[Group %in% "SmallMobileEpifaunaSwarmingCrustaceans",
                       ProdCons := 0.35]
Rpath.parameters$model[Group %in% c("BenthicMicrofloraInclBacteriaProtozoa",
                                    "PlanktonicMicrofloraInclBacteriaProtozoa"),
                       ProdCons := 0.5]
Rpath.parameters$model[Group %in% c("BaleenWhales", "ToothedWhales", "Seals",
                                    "SurfaceFeedingSeabirds", "JuvenileSharks",
                                    "Spurdog", "LargePiscivorousSharks",
                                    "SmallSharks", "JuvenileRays",
                                    "StarryRayOthers", "ThornbackAndSpottedRay",
                                    "SkateCuckooRay", "JuvCod", "AduCod",
                                    "JuvWhiting", "AduWhiting", "JuvHaddock",
                                    "AduHaddock", "JuvSaithe", "AduSaithe",
                                    "Hake", "BlueWhiting", "NorwayPout",
                                    "OtherLargeGadoids", "OtherSmallGadoids",
                                    "Monkfish", "Gurnards", "JuvHerring",
                                    "AduHerring", "Sprat", "Mackerel",
                                    "HorseMackerel", "Sandeels", "Plaice",
                                    "Dab", "LongRoughDab", "Flounder", "Sole",
                                    "LemonSole", "Witch", "Turbot", "Megrim",
                                    "Halibut", "Dragonets", "CatfishWolfFish",
                                    "LargeDemersalFish", "SmallDemersalFish",
                                    "MiscellaneousFilterfeedingPelagicFish",
                                    "SquidAndCuttlefish", "FishLarvae",
                                    "CarnivorousZooplankton",
                                    "HerbivorousAndOmnivorousZooplanktonCopepods",
                                    "GelatinousZooplankton", "LargeCrabs",
                                    "Nephrops",
                                    "EpifaunalMacrobenthosMobileGrazers",
                                    "InfaunalMacrobenthos", "Shrimp",
                                    "SmallMobileEpifaunaSwarmingCrustaceans",
                                    "SmallInfaunaPolychaetes",
                                    "SessileEpifauna", "Meiofauna",
                                    "BenthicMicrofloraInclBacteriaProtozoa",
                                    "PlanktonicMicrofloraInclBacteriaProtozoa",
                                    "DivingSeabirds", "Phytoplankton",
                                    "DetritusDOMWaterColumn",
                                    "DetritusPOMSediment", "Discards"),
                       BioAcc := 0]
Rpath.parameters$model[Group %in% c("BaleenWhales", "ToothedWhales", "Seals",
                                    "SurfaceFeedingSeabirds", "JuvenileSharks",
                                    "Spurdog", "LargePiscivorousSharks",
                                    "SmallSharks", "JuvenileRays",
                                    "StarryRayOthers", "ThornbackAndSpottedRay",
                                    "SkateCuckooRay", "JuvCod", "AduCod",
                                    "JuvWhiting", "AduWhiting", "JuvHaddock",
                                    "AduHaddock", "JuvSaithe", "AduSaithe",
                                    "Hake", "BlueWhiting", "NorwayPout",
                                    "OtherLargeGadoids", "OtherSmallGadoids",
                                    "Monkfish", "Gurnards", "JuvHerring",
                                    "AduHerring", "Sprat", "Mackerel",
                                    "HorseMackerel", "Sandeels", "Plaice",
                                    "Dab", "LongRoughDab", "Flounder", "Sole",
                                    "LemonSole", "Witch", "Turbot", "Megrim",
                                    "Halibut", "Dragonets", "CatfishWolfFish",
                                    "LargeDemersalFish", "SmallDemersalFish",
                                    "MiscellaneousFilterfeedingPelagicFish",
                                    "SquidAndCuttlefish", "FishLarvae",
                                    "CarnivorousZooplankton",
                                    "GelatinousZooplankton", "LargeCrabs",
                                    "Nephrops",
                                    "EpifaunalMacrobenthosMobileGrazers",
                                    "InfaunalMacrobenthos", "Shrimp",
                                    "SmallMobileEpifaunaSwarmingCrustaceans",
                                    "SmallInfaunaPolychaetes",
                                    "SessileEpifauna", "Meiofauna",
                                    "DivingSeabirds"), Unassim := 0.2]
Rpath.parameters$model[Group %in% "HerbivorousAndOmnivorousZooplanktonCopepods",
                       Unassim := 0.38]
Rpath.parameters$model[Group %in% c("BenthicMicrofloraInclBacteriaProtozoa",
                                    "PlanktonicMicrofloraInclBacteriaProtozoa"),
                       Unassim := 0.3]
Rpath.parameters$model[Group %in% c("Phytoplankton", "DetritusDOMWaterColumn",
                                    "DetritusPOMSediment", "Discards"),
                       Unassim := 0]
Rpath.parameters$model$DetInput <- c(rep(NA, 66), rep(0, 3), rep(NA, 11))
Rpath.parameters$model[Group %in% c("BaleenWhales", "ToothedWhales", "Seals",
                                    "SurfaceFeedingSeabirds", "JuvenileSharks",
                                    "Spurdog", "LargePiscivorousSharks",
                                    "SmallSharks", "JuvenileRays",
                                    "StarryRayOthers", "ThornbackAndSpottedRay",
                                    "SkateCuckooRay", "JuvCod", "AduCod",
                                    "JuvWhiting", "AduWhiting", "JuvHaddock",
                                    "AduHaddock", "JuvSaithe", "AduSaithe",
                                    "Hake", "BlueWhiting", "NorwayPout",
                                    "OtherLargeGadoids", "OtherSmallGadoids",
                                    "Monkfish", "Gurnards", "JuvHerring",
                                    "AduHerring", "Sprat", "Mackerel",
                                    "HorseMackerel", "Sandeels", "Plaice",
                                    "Dab", "LongRoughDab", "Flounder", "Sole",
                                    "LemonSole", "Witch", "Turbot", "Megrim",
                                    "Halibut", "Dragonets", "CatfishWolfFish",
                                    "LargeDemersalFish", "SmallDemersalFish",
                                    "MiscellaneousFilterfeedingPelagicFish",
                                    "SquidAndCuttlefish", "Nephrops",
                                    "EpifaunalMacrobenthosMobileGrazers",
                                    "InfaunalMacrobenthos", "Shrimp",
                                    "SmallMobileEpifaunaSwarmingCrustaceans",
                                    "SmallInfaunaPolychaetes",
                                    "SessileEpifauna", "Meiofauna",
                                    "DivingSeabirds"),
                       DetritusDOMWaterColumn := 0.3]
Rpath.parameters$model[Group %in% c("FishLarvae", "CarnivorousZooplankton",
                                    "HerbivorousAndOmnivorousZooplanktonCopepods",
                                    "GelatinousZooplankton"),
                       DetritusDOMWaterColumn := 0.6]
Rpath.parameters$model[Group %in% "LargeCrabs",
                       DetritusDOMWaterColumn := 0.2]
Rpath.parameters$model[Group %in% c("BenthicMicrofloraInclBacteriaProtozoa",
                                    "PlanktonicMicrofloraInclBacteriaProtozoa"),
                       DetritusDOMWaterColumn := 0.5]
Rpath.parameters$model[Group %in% "Phytoplankton",
                       DetritusDOMWaterColumn := 0.45]
Rpath.parameters$model[Group %in% c("DetritusDOMWaterColumn",
                                    "DetritusPOMSediment", "Discards"),
                       DetritusDOMWaterColumn := 0]
Rpath.parameters$model[Group %in% c("DemersalTrawlAndDemSeine", "BeamTrawl",
                                    "IndustrialTrawl", "PelagicTrawl",
                                    "DriftAndFixedNets", "NephropsTrawl",
                                    "GearsUsingHooks", "ShrimpTrawlers",
                                    "Dredges", "Pots", "Other"),
                       DetritusDOMWaterColumn := 0]
Rpath.parameters$model[Group %in% c("BaleenWhales", "ToothedWhales", "Seals",
                                    "SurfaceFeedingSeabirds", "JuvenileSharks",
                                    "Spurdog", "LargePiscivorousSharks",
                                    "SmallSharks", "JuvenileRays",
                                    "StarryRayOthers", "ThornbackAndSpottedRay",
                                    "SkateCuckooRay", "JuvCod", "AduCod",
                                    "JuvWhiting", "AduWhiting", "JuvHaddock",
                                    "AduHaddock", "JuvSaithe", "AduSaithe",
                                    "Hake", "BlueWhiting", "NorwayPout",
                                    "OtherLargeGadoids", "OtherSmallGadoids",
                                    "Monkfish", "Gurnards", "JuvHerring",
                                    "AduHerring", "Sprat", "Mackerel",
                                    "HorseMackerel", "Sandeels", "Plaice",
                                    "Dab", "LongRoughDab", "Flounder", "Sole",
                                    "LemonSole", "Witch", "Turbot", "Megrim",
                                    "Halibut", "Dragonets", "CatfishWolfFish",
                                    "LargeDemersalFish", "SmallDemersalFish",
                                    "MiscellaneousFilterfeedingPelagicFish",
                                    "SquidAndCuttlefish", "Nephrops",
                                    "EpifaunalMacrobenthosMobileGrazers",
                                    "InfaunalMacrobenthos", "Shrimp",
                                    "SmallMobileEpifaunaSwarmingCrustaceans",
                                    "SmallInfaunaPolychaetes",
                                    "SessileEpifauna", "Meiofauna",
                                    "DivingSeabirds"),
                       DetritusPOMSediment := 0.7]
Rpath.parameters$model[Group %in% c("FishLarvae", "CarnivorousZooplankton",
                                    "HerbivorousAndOmnivorousZooplanktonCopepods",
                                    "GelatinousZooplankton"),
                       DetritusPOMSediment := 0.4]
Rpath.parameters$model[Group %in% "LargeCrabs",
                       DetritusPOMSediment := 0.8]
Rpath.parameters$model[Group %in% c("BenthicMicrofloraInclBacteriaProtozoa",
                                    "PlanktonicMicrofloraInclBacteriaProtozoa"),
                       DetritusPOMSediment := 0.5]
Rpath.parameters$model[Group %in% "Phytoplankton",
                       DetritusPOMSediment := 0.55]
Rpath.parameters$model[Group %in% c("DetritusDOMWaterColumn",
                                    "DetritusPOMSediment", "Discards"),
                       DetritusPOMSediment := 0]
Rpath.parameters$model[Group %in% c("DemersalTrawlAndDemSeine", "BeamTrawl",
                                    "IndustrialTrawl", "PelagicTrawl",
                                    "DriftAndFixedNets", "NephropsTrawl",
                                    "GearsUsingHooks", "ShrimpTrawlers",
                                    "Dredges", "Pots", "Other"),
                       DetritusPOMSediment := 0]
Rpath.parameters$model[, Discards := c(rep(0, 69), rep(1, 11))]
#Landings
DemTrwal.land <- c(rep(0, 5), 0.002853826, 1.61612E-05, 6.9682E-08,
                   0, 0.003235913, 0.002662631, 2.34101E-05, 0.03, 0.1041975,
                   0.003074729, 0.1092998, 0.001762671, 0.08517189, 0.04552222,
                   0.1147963, 0.004977133, 7.57943E-06, 0, 0.03461567,
                   0.003844701, 0.01988857, 0.001587582, 0, 7.61782E-05,
                   1.2714E-07, 0.001521828, 0.002362751, 9.47507E-05, 0.1158071,
                   0.008702274, 1.36623E-06, 0, 0.00073, 0.009700906,
                   0.01260026, 0.001017524, 0.003450463, 0.001256885,
                   0, 0.005266989, 0.00317015, 0.04240225, 0, 4.7518E-05,
                   rep(0, 4), 0.000534952, 0.005311305, 0.000125163, 0,
                   1.27469E-05, rep(0, 11), rep(NA, 11))
BeamTrwal.land <- c(rep(0, 5), 0.000108376, 1.23729E-07, rep(0, 2), 0.002978352,
                    0.002450699, 6.53384E-06, 0.0018, 0.006552235, 0.000110648,
                    0.003933301, 4.3537E-06, 0.00021037, 2.2384E-06, 5.64472E-06,
                    2.43125E-05, 1.18097E-06, 0,	 0.000217164, 0, 0.000680806,
                    0.005903429, 0, 1.01256E-06, 0, 3.08442E-05, 9.53491E-05, 0,
                    0.1373555, 0.01224303, 3.29043E-09, 0.06336, 0.05,
                    0.003271517, 1.4182E-05, 0.002883521, 2.06385E-06,
                    8.98286E-07, 0, 8.42967E-06, 0.000844637, 0.1039312, 0,
                    0.002025608, rep(0,  4), 0.004035845, 0.000286481,
                    0.000455548, 0, 0.000385635, rep(0, 11), rep(NA, 11))
IndusTrwal.land <- c(rep(0, 5), 0.000143917, 2.46712E-07, 4.6391E-06, 0,
                     3.48245E-05, 2.86549E-05, 1.74541E-06,	 0.000167273,
                     0.000580177, 1.00244E-05, 0.000356347, 3.73357E-06,
                     0.000180406, 0.000293247, 0.0007395, 1.14155E-05,
                     0.00227606, 0.2699929, 0.000127188, 0, 9.92653E-05,
                     rep(0, 2), 0.05240672, 0.15, 0.00918311, 2.83554E-05, 0.75,
                     0.000161342, 1.97623E-05, 2.23074E-09, 0, 9.906E-05,
                     1.99087E-05, 0.000441012, 3.07836E-06, 6.69646E-06,
                     1.77533E-05, 0, 1.92181E-06, 1.07893E-05, 0.000255006,
                     0.009657317, rep(0,  5), 2.14311E-06, 8.75106E-05,
                     8.7513E-06, 0, 0.009032811, rep(0, 11), rep(NA, 11))
PelTrwal.land <- c(rep(0, 5), 4.08836E-05, 2.34101E-05, 2.19843E-06, rep(0,  3),
                   2.39866E-05, 1.78108E-05, 6.17756E-05,	 6.1138E-05,
                   0.00217332, 7.56862E-07, 3.65714E-05, 0.000107123,
                   0.000270139, 3.79176E-05, 0.05867125, 0.002804139,
                   4.08954E-05, 0.01182882, 3.39484E-07, rep(0, 2), 0.8008611,
                   0.000372095, 0.2, 0.075, 0.1, 8.69528E-05, 0.000218173,
                   3.8283E-06, 0, 0.000101, 7.22042E-06, 3.89385E-06,
                   2.35092E-06, 1.36295E-08, rep(0, 2), 2.6806E-08, 0.00105018,
                   0.000134994,	0.000558762, rep(0, 5), 3.79089E-06, 3.4765E-07,
                   8.54844E-06, 0, 0.000533971, rep(0, 11), rep(NA, 11))
DriFixNets.land <- c(rep(0, 5), 0.001200038, 5.85574E-05, 3.35868E-05, 0,
                     6.72056E-05, 5.52993E-05, 1.58995E-06, 0.000922723,
                     0.003200404, 2.74904E-06, 9.77224E-05, 5.64513E-07,
                     2.72771E-05, 2.59703E-05, 6.54909E-05, 7.78357E-05,
                     2.18401E-05, 4.21338E-07, 0.000520877, 0, 0.001048654,
                     6.47252E-07, 0, 0.000197455, 4.16757E-06, 0.000210318,
                     3.89716E-06, 1.98556E-09, 0.003299252, 0.000302294,
                     1.73212E-08, 0, 0.0041, 8.74881E-05, 7.18032E-06,
                     0.000160238, 1.361E-06, 4.15467E-06, 0, 2.15383E-05,
                     0.000507869, 0.007575414, 4.63691E-07, 1.27375E-06,
                     rep(0,4), 0.000357838, 2.20989E-06, 2.09492E-05, 0,
                     5.64742E-06, rep(0, 11), rep(NA, 11))
NephTrwal.land <- c(rep(0, 5), 0.001689229, 0.000145782, 3.29635E-05, 0,
                    0.000471284, 0.00038779, 6.37936E-06, 0.000800898,
                    0.002777863, 0.0008865, 0.0315131, 4.41837E-05, 0.002134945,
                    0.000411278, 0.001037147, 7.04942E-05, 1.00901E-05,
                    9.61902E-06, 0.000566028, 1.79983E-05, 0.000741084,
                    0.000522996, 0, 0.001136149, 1.38999E-06, 0.006299297,
                    0.001422844, 2.09827E-05, 0.006183171, 0.001402784,
                    2.36721E-08, 0, 0.0012, 0.000662904, 0.003257636,
                    0.000133446, 1.74708E-05, 4.80635E-05, 0, 2.54385E-05,
                    0.001317723, 0.009300798, 3.31944E-06, 7.62581E-05,
                    rep(0, 4), 0.000193941, 0.0132548, 0.000157412, 0,
                    3.86333E-05, rep(0, 11), rep(NA, 11))
Hooks.land <- c(rep(0,5), 4.10245E-06, 2.64534E-05, 1.33883E-07, rep(0, 3),
                6.7955E-08, 0, 0.000235009, 8.84465E-08,	 3.14408E-06,
                2.9778E-07, 1.43887E-05, 1.12153E-05, 2.82824E-05, 7.93459E-05,
                rep(0, 2), 0.000225534, 0, 7.8519E-06, rep(0, 2), 3.09655E-07,
                6.33251E-08, 0.000804855, 1.81276E-08, 1.14047E-05, 1.3422E-06,
                4.10254E-08, 7.57396E-11, 0, 1.86513E-06, 2.44241E-07, 0,
                2.54212E-07, 0, 1.97713E-07, 0, 4.23871E-07, 0.00027866,
                1.33488E-05, rep(0, 6), 1.84914E-05, 8.77745E-07, 7.1681E-07,
                0, 1.73493E-07, rep(0, 11), rep(NA, 11))
ShrTrwal.land <- c(rep(0, 9), 1.88495E-07, 1.55101E-07, 5.46338E-10, 0,
                   4.45137E-05,6.90765E-07, 2.45552E-05, 5.13927E-08,
                   2.48328E-06, 1.20001E-06, 3.02614E-06, 1.27107E-10,
                   rep(0, 2), 2.59938E-09, rep(0,2), 2.99253E-06, 0,
                   4.21016E-08, 0, 8.69089E-07, 5.47972E-05, 0.000203852,
                   7.4591E-05, 0.000170959, rep(0, 2), 3.9178E-05, 2.53003E-07,
                   1.47238E-08, 1.76899E-06, rep(0, 4), 3.66086E-06,
                   0.005471857, 0, 7.572E-07, rep(0, 4), 8.81764E-06,
                   6.05081E-10, 1.66099E-07, 0, 0.068, rep(0, 11), rep(NA, 11))
Dredge.land <- c(rep(0, 9), 8.1584E-08, 6.71304E-08, 7.01511E-09, 0, 2.30915E-06,
                 4.45937E-08, 1.58521E-06, 2.98076E-08, 1.4403E-06, 4.27379E-08,
                 1.07775E-07, rep(0, 3), 6.46907E-08, 0, 9.83967E-06,
                 1.79614E-08, rep(0, 2), 1.56045E-06, 7.22706E-06, 0,
                 8.40661E-06, 6.18888E-06, 5.07096E-07, 2.12659E-09, 0,
                 1.26659E-05, 9.07168E-07, 0, 3.23756E-06, 1.13761E-08,
                 2.15474E-09, rep(0, 2), 7.97072E-07, 0.000217828, 0,
                 2.39097E-06, rep(0, 4), 2.68322E-06,	1.60983E-06, 0.005744705,
                 0, 1.58402E-06, rep(0, 11), rep(NA, 11))
Pot.land <- c(rep(0, 5), 6.76065E-05, 3.63329E-07, 9.54724E-07, 0, 9.43282E-08,
              7.76168E-08, 1.24347E-08, 0, 2.14791E-05, 1.72058E-07, 6.11626E-06,
              3.18991E-08, 1.54136E-06, 6.48939E-08, 1.63647E-07, 3.31326E-08,
              rep(0, 2), 2.86118E-06, 0, 1.74708E-07, rep(0, 2), 1.31441E-06,
              1.49621E-07, 2.65544E-05, 1.13993E-08, 5.51185E-09, 6.7742E-06,
              1.09838E-06, 2.8709E-10, 0, 9.55866E-06, 1.76406E-07, 1.09704E-07,
              3.79153E-07, 5.53269E-08, 9.04262E-09, 0,1.26472E-07, 6.13519E-06,
              1.40671E-05, 4.13962E-08, rep(0, 5), 0.006222396, 0.000105637,
              1.03265E-05, 0, 1.09048E-06, rep(0, 11), rep(NA, 11))
Other.land <- c(rep(0, 5), 0.002663633, 1.29188E-06, 0.000188234, 0, 2.06262E-06,
                1.6972E-06, 4.66721E-06, 0.000189342, 0.000656721, 2.76421E-10,
                0.07, 3.24629E-07, 1.5686E-05, 0.000153725, 0.000387659,
                5.99688E-06, 0, 0.06, 0, 0.01259993, 1.26035E-05, rep(0, 2),
                0.001329465, 2.29841E-05, 0.00015471, 1.57579E-08, 0.0003051, 0,
                5.07217E-05,	1.64657E-08, rep(0, 2), 1.56085E-05, 1.10744E-06,
                0.007, 5.62001E-08, 2.96177E-07, 0, 1.21545E-06, 4.73364E-07,
                1.34082E-05, 0, 3.21269E-07, rep(0, 4), 7.97214E-05, 5.32429E-07,
                0.00011504, rep(0, 13), rep(NA, 11))
Rpath.parameters$model[, DemersalTrawlAndDemSeine := DemTrwal.land]
Rpath.parameters$model[, BeamTrawl := BeamTrwal.land]
Rpath.parameters$model[, IndustrialTrawl := IndusTrwal.land]
Rpath.parameters$model[, PelagicTrawl := PelTrwal.land]
Rpath.parameters$model[, DriftAndFixedNets := DriFixNets.land]
Rpath.parameters$model[, NephropsTrawl := NephTrwal.land]
Rpath.parameters$model[, GearsUsingHooks := Hooks.land]
Rpath.parameters$model[, ShrimpTrawlers :=  ShrTrwal.land]
Rpath.parameters$model[, Dredges := Dredge.land]
Rpath.parameters$model[, Pots := Pot.land]
Rpath.parameters$model[, Other := Other.land]
#Discards
DemTrwal.d <- c(rep(0, 5), 5.06718E-07, rep(0, 3), 0.003111471, 0.002875586,
                0.003111471, 0.05, 0.004727384, 0.000660449, 0.009234448,
                0.001666666, 0.005895701, 0.02684426, 2.12736E-07, 3.77563E-06,
                rep(0, 2), 4.14996E-07, 0, 5.40373E-06, 0, 0.000466441,
                7.03557E-05, 0, 0.000129631, 0.008985599, 0, 0.01420333,
                0.01493082, 0.000308498, 0,	2.6691E-06, 0.000196156, 4.18395E-09,
                1.69632E-06, 7.51739E-08, 2.6895E-06, 0, 6.48066E-05,
                0.001197834, 0.000719857, rep(0, 6), 0.001751058, 8.05812E-07,
                rep(0, 2), 2.34919E-07, rep(0, 11), rep(NA, 11))
BeamTrwal.d <- c(rep(0, 9), 0.000168984, 0.000156173, 0.000168984, 0.0019,
                 0.000145395, 0.000620671, 0.008678269, 2.68194E-06, 9.48715E-06,
                 5.39399E-07, 4.27464E-12, 1.06707E-10, rep(0, 2), 1.0912E-10, 0,
                 1.0679E-05, rep(0, 7), 0.07917948, 0.079301, rep(0, 2),
                 0.005127981, 0.000682288, 4.65486E-13, 5.20357E-05, rep(0, 4),
                 7.73096E-05, 0.000703873, rep(0, 6), 0.000102423, 3.37388E-07,
                 2.17421E-07, rep(0, 13), rep(NA, 11))
IndusTrwal.d <- c(rep(0, 5), 4.00615E-07, rep(0, 3), 2.83885E-05, 2.62363E-05,
                  0.000503669, 0.0005, 4.51617E-05, 0.000217787, 0.003045107,
                  1.59815E-05, 5.65333E-05, 4.1076E-05, 3.2552E-10, 2.75284E-09,
                  0.000389518, 0, 1.10056E-11, rep(0, 3), 0.002450352,
                  0.000369599, 0, 3.49904E-06, 6.23571E-08, 0, 4.92028E-06,
                  3.6383E-06, 9.66499E-05, rep(0, 2),	3.03451E-06, 5.50228E-09,
                  5.09064E-07, 0, 1.4645E-06, rep(0, 2), 6.4495E-07, 0.000193362,
                  rep(0, 7), 2.02046E-08, rep(0, 2), 0.000146561, rep(0, 11),
                  rep(NA, 11))
PelTrwal.d <- c(rep(0, 11), 7.27037E-05, 1.03766E-06, 7.84726E-08, 1.45708E-08,
                2.0373E-07, 2.75453E-07, 9.74395E-07, rep(0, 3), 7.27037E-05,
                0.00105, rep(0, 2), 4.33143E-10, 0, 0.01843836, 0.002781151,
                6.34E-07, 0.0004149, 0.00046465, 0, 2.48112E-08, 4.29769E-09,
                rep(0, 3), 1.68974E-09, 1.60362E-13, rep(0, 29), rep(NA, 11))
DriFixNets.d <- c(rep(0, 5), 1.22131E-10, rep(0, 3), 5.21884E-06, 4.82319E-06,
                  5.21884E-06, 0.0018, 0.000138638, 1.89622E-05, 0.000265131,
                  1.53603E-07, 5.43358E-07, 2.70846E-06, 2.1464E-11, 3.6225E-11,
                  rep(0, 2), 7.85617E-10, 0, 1.3385E-10, 0, 7.12422E-09,
                  1.07458E-09, 0, 2.47109E-06, 6.52264E-09, 0, 6.89203E-05,
                  6.16174E-05, 1.08711E-08, 0, 1.46547E-05, 2.64214E-08,
                  3.61043E-13, 1.41731E-06, rep(0, 5), 8.04387E-06, rep(0, 6),
                  0.000387453, rep(0, 15), rep(NA, 11))
NephTrwal.d <- c(rep(0, 5), 5.80257E-07, rep(0, 3), 0.002731318, 0.002524253,
                 0.002825415, 0.01, 0.000848823, 0.000907536, 0.01268923,
                 0.000418711, 0.001481157, 0.000745727, 5.90975E-09, 2.48624E-08,
                 rep(0, 2), 8.10164E-10, 0, 2.26067E-07, 0, 0.000128908,
                 1.94438E-05, 0, 7.45413E-07, 0.000264078, 0, 0.01917295,
                 0.01912735, 0.000603797, 0, 1.35321E-05, 4.12613E-05,
                 3.86762E-09, 2.26558E-07, 6.12829E-11, 3.69346E-06, 0,
                 1.01146E-06, 0.00030228, 0.000392139, rep(0, 6), 0.000639601,
                 3.25226E-06, rep(0, 14), rep(NA, 11))
Hooks.d <- c(rep(0, 13), 3.03628E-07, rep(0, 2), 1.01085E-09, 3.57579E-09,
             4.03392E-07, 3.19681E-12, rep(0, 37),	1.23379E-10, rep(0, 11),
             rep(NA, 11))
ShrTrwal.d <- c(rep(0, 13), 9.35504E-08, 1.07502E-05, 0.00015031, rep(0, 11),
                8.0764E-05, 1.2182E-05, 0, 8.79546E-08, 9.32825E-07, 0,
                0.005422184, 0.001105026, rep(0, 2), 0.000293606, 4.00481E-07,
                rep(0, 18), 0.034, rep(0, 11), rep(NA, 11))
Dredge.d <- c(rep(0, 69), rep(NA, 11))
Pot.d <- c(rep(0, 69), rep(NA, 11))
Othr.d <- c(rep(0, 34), 0.02,	rep(0, 34), rep(NA, 11))
Rpath.parameters$model[, DemersalTrawlAndDemSeine.disc := DemTrwal.d]
Rpath.parameters$model[, BeamTrawl.disc := BeamTrwal.d]
Rpath.parameters$model[, IndustrialTrawl.disc := IndusTrwal.d]
Rpath.parameters$model[, PelagicTrawl.disc := PelTrwal.d]
Rpath.parameters$model[, DriftAndFixedNets.disc := DriFixNets.d]
Rpath.parameters$model[, NephropsTrawl.disc := NephTrwal.d]
Rpath.parameters$model[, GearsUsingHooks.disc := Hooks.d]
Rpath.parameters$model[, ShrimpTrawlers.disc := ShrTrwal.d]
Rpath.parameters$model[, Dredges.disc := Dredge.d]
Rpath.parameters$model[, Pots.disc := Pot.d]
Rpath.parameters$model[, Other.disc := Othr.d]
#Stanzas
Rpath.parameters$stanzas$stgroups[, VBGF_Ksp := c(0.23, 0.32, 0.19, 0.07, 0.46)]
Rpath.parameters$stanzas$stgroups[, Wmat := c(0.20, 0.38, 0.15, 0.030, 0.52)]
Rpath.parameters$stanzas$stindiv[, First := c(0, 24, 0, 12, 0, 12, 0, 36, 0, 12)]
Rpath.parameters$stanzas$stindiv[, Last := c(23, 400, 11, 400, 11, 400, 35, 400,
                                             11, 400)]
Rpath.parameters$stanzas$stindiv[, Z := c(1.79, 1.19, 2.36, 0.89, 2, 1.14, 1,
                                          0.88, 1.31, 0.8)]
Rpath.parameters$stanzas$stindiv[, Leading := rep(c(F, T), 5)]
Rpath.parameters <- rpath.stanzas(Rpath.parameters)
#Diet
Baleen.Whales.diet <- c(rep(0, 14), 0.002035905, rep(0, 7), 0.005319953,
                        rep(0, 4), 0.0801999, 0.09999987, 0.01033299, 0.09596378,
                        0, 0.4999994, rep(0, 14), 0.02578131, 0.02578131, 0,
                        0.05156263, 0, 0.103023, rep(0, 17))
Tooth.Whale.diet <- c(rep(0, 12), 0.02489587, 0, 0.02069656, 0.06368943,
                      0.001399768, 0.003699386, 0.05559077, 0.01259791,
                      0.00559907, 0.002499585, 0.2237629, 0.01449759, 0.04729215,
                      rep(0, 2), 0.001099817, 0.003299452, 0.1043827, 0.01959675,
                      0.01229796, 0.2730547, rep(0, 2), 6.598904E-05, rep(0, 7),
                      9.99834E-05, rep(0, 2), 0.0002999502, 0, 0.1095818,
                      rep(0, 21))
Seals.diet <- c(rep(0, 12), 0.03160289, 0.06360582, 0.004300393, 0.06890629,
                0.0003000274, 0.01180108, 0.003300302, 0.03130286, rep(0, 3),
                0.06970637, 0.02780254, 0.01040095, 0, 8.600786E-06, 0.006100557,
                0.002800256, 0, 0.01240113, 0.3013275, 0.06150562, 0.04940452,
                0.01060097, 0.01140104, 0.03670335, 0.02400219, 0.02500229,
                0.01060097, 0.011001, 0, 0.01060097, rep(0, 2), 0.1035095,
                rep(0, 23))
Surf.Birds.diet <- c(rep(0, 12), 0.005338055, 0, 0.01313983, 0, 0.005748673,
                     rep(0, 5), 0.01313983, 0.002258407, rep(0, 4), 0.04763187,
                     0.04373097, 0.005953984, 0,0.206131, 0.002669027, rep(0, 3),
                     0.0002053091, rep(0, 7), 0.01416637, 0.002566372,
                     0.002566372, 0.003798231, 0, 0.01437169, 0.0052354, 0,
                     0.02976991, 0, 0.0001026546, 0.01580885, 0.01211328, 0,
                     0.005132745, 0.02361062, rep(0, 7), 0.4593273, 0.0654)
Juv.Sharks.diet <- c(rep(0, 14), 0.00040004, 0.01920192, rep(0, 5), 0.00350035,
                     0.07310731, 0, 0.00260026, rep(0, 2), 0.00050005, 0.06260626,
                     0.02310231, 0.01370137, 0, 0.01440144, 0, 0.006700669,
                     rep(0, 8), 0.0010001, rep(0, 2), 0.010001, 0.00370037,
                     0.01230123, 0, 0.01990199, 0, 0.02690269, 0.02810281,
                     0.00740074, 0.3341334, 0.05970597, 0.07810781, 0.06910691,
                     0.1207121, 0.008600861, 0.00050005, rep(0, 8))
Spurdog.diet <- c(rep(0, 5), 0.02104882, rep(0, 8), 0.002473237, 0.1188206,
                  0.0002546907, 0.006104158, rep(0, 3), 0.0214698, 0.09892946,
                  0, 0.1317656, rep(0, 2), 0.001768101, 0.1873345, 0.04083471,
                  0.08187991, 0.01936492, 0.03862458, 0.04746509, 0.006840867,
                  rep(0, 4), 0.006840867, rep(0, 3), 0.02694249, rep(0, 2),
                  0.01062965, 0, 0.0231537, 0, 0.005577937,	0, 0.03757215,
                  0.01399747, 0.0107349, 0.009366726, 0.008314284, 0.02031211,
                  0, 0.001578662, rep(0, 10))
Pisc.Sharks.diet <- c(rep(0, 4), 0.06071214, 0.01010202, 0, 0.05061012,
                      0.06071214, rep(0, 13), 0.1013203, 0, 0.009801961, 0,
                      0.04880976, 0.00090018, 0.1018204, 0, 0.1542308,
                      rep(0, 2), 0.06161232, 0.06161232, rep(0, 8), 0.05431086,
                      rep(0, 2), 0.01080216, 0, 0.2052411, rep(0, 6),
                      0.00740148, rep(0, 14))
Small.Sharks.diet <- c(rep(0, 14), 0.00010001, 0.00590059, rep(0, 5), 0.0740074,
                       0.02110211, 0, 0.01030103, 0, 0.00040004, 0, 0.00540054,
                       0.01380138, 0.00470047, 0, 0.01110111, 0.0020002,
                       0.00590059, 0.00210021, 0, 0.0020002, rep(0, 5),
                       0.01660166, rep(0, 2), 0.00250025, 0.00410041, 0.0260026,
                       0, 0.00120012, rep(0, 2), 0.1142114, 0.01880188,
                       0.3033303, 0.1820182, 0.0370037, 0.04130413, 0.07530753,
                       0.00870087, 0.01010101, rep(0, 8))
Juv.Rays.diet <- c(rep(0, 12), 0.002, rep(0, 9), 0.0106, 0, 0.015, rep(0, 4),
                   0.0001, rep(0, 2), 0.3959, 0, 0.0029, 0.0012, rep(0, 10),
                   0.0009, 0, 0.0005, 0, 0.0082, rep(0, 2), 0.096, 0, 0.0657,
                   0.0032, 0.0526, 0.2394, 0.1016, 0, 0.0042, rep(0, 8))
Star.Ray.Othr.diet <- c(rep(0, 8), 0.00019998, rep(0, 3), 0.01, 0, 0.00279972,
                        0.00449955, 0.00059994, rep(0, 5), 0.1105889, 0,
                        0.04689531, 0, 0.00039996, 0.00029997, 0.00839916,
                        0.00019998, rep(0, 2), 0.4, 0, 0.02769723, 0.01949805,
                        0, 0.00129987, 0, 9.999E-05, rep(0, 2), 0.00019998,
                        0.00549945, 0, 0.00019998, 0.00939906, 0, 0.00169983, 0,
                        0.00019998, rep(0, 2), 0.06439357, 0, 0.1586, 0.0019998,
                        0.01079892, 0.08129187, 0.03219678, rep(0 , 10))
Thornback.SppotedRay.diet <- c(rep(0,14), 0.00160016, 0, 0.00010001, rep(0, 5),
                               0.00140014, 0, 0.03270327, rep(0, 2), 0.00320032,
                               0, 0.01920192, rep(0, 2), 0.03630363, 0.00920092,
                               rep(0, 3), 0.00450045, rep(0, 8), 0.06960697, 0,
                               0.0050005, 0, 0.00160016, rep(0, 2), 0.3827383, 0,
                               0.2056206, 0.0020002, 0.1166117, 0.09650965,
                               0.01210121, rep(0, 10))
Skate.Cucko.diet <- c(rep(0, 14), 0.001441025, 0, 0.0002682884, rep(0, 3),
                      0.0009372521, 0.001757348, 0.05471209, 0, 0.06677921,
                      0.0009372521, 0.002811756, 0.01359016, rep(0, 4),
                      0.3901312, 0, 0.002928913, 0.001874504, 0, 0.02788325, 0,
                      0.0009372521, rep(0, 3), 0.08048653, 0, 0.001757348,
                      0.01253575, 0, 0.009255365, 0, 0.003983322, rep(0, 2),
                      0.0638503, 0.001757348, 0.02143964, 0.0007029391,
                      0.09852863, 0.08622719, 0.05248612, rep(0, 10))
Juv.Cod.diet <- c(rep(0, 12), 9.980984E-05, 0, 0.002000197, 0, 0.001100108,
                  rep(0, 5), 0.11, 0, 0.03700365, 0, 0.002100207, 0.050705,
                  0.002100207, 0.03060302, 0, 0.000801079, 0.12, 2.100207E-05,
                  0.00639063, 0.02270224, 0, 0.003790374, 0, 9.980984E-05,
                  rep(0, 3), 0.005290521, rep(0, 2), 0.07040694, 9.980984E-05,
                  0.001, 0, 0.005290521, 0.12, 0, 0.04020396, 0.002010198,
                  0.1297128, 0.1155, 0.045, 0.02460242, 0.05030496, 0.001100108,
                  rep(0, 9))
Adult.Cod.diet <- c(rep(0, 5), 0.00010004, rep(0, 2), 0.00010004, rep(0, 3),
                    0.01, 0.008003201, 0.00420168, 0.07, 0.0009003601,
                    0.02110844, rep(0, 3), 0.0205082, 0.07362945, 0, 0.02260904,
                    0, 0.006402561, 0.00220088, 0.06152461, 0.0145058, 0.0135054,
                    0.00310124, 0.05362145, 0.01920768, 0.21, 0.05602241,
                    0.00360144, 0.01880752, 0, 0.00120048, rep(0, 3), 0.01, 0,
                    0.00010004, 0.01580632, 0, 0.002400961, 0, 0.0010004, 0,
                    0.00010004, 0.09, 0.05, 0.0985, 0.02601041,	0.009003601,
                    0.00140056, 0.0007002801, 0.00010004, rep(0, 9))
Juv.Whiting.diet <- c(rep(0, 12), 0.00379962, 0, 0.0005999401, 0, 0.00039996,
                      rep(0, 5), 0.1371863, 0, 0.01309869, rep(0, 2), 0.00029997,
                      0, 0.01449855, 0, 0.00159984, 0.3088691, 0, 0.00029997,
                      0.00429957, 0, 0.00179982, rep(0, 8), 0.02789721,
                      0.00049995, 0.02129787, 0, 0.1020898, 0.07159284, 0,
                      0.02039796, 0, 0.1020898, 0.08169183, 0.03059694,
                      0.02179782, 0.03329667, rep(0, 10))
Adult.Whiting.diet <- c(rep(0, 12), 0.00460046, 0, 0.003, 0.00170017, 0.00070007,
                        0, 0.00020002, rep(0, 2), 0.004, 0.2248205, 0,
                        0.02070207, 0, 0.00030003, 0.0174, 0, 0.21, 0, 0.01110111,
                        0.25, 0, 0.03, 0.00670067, 0, 0.00340034, rep(0, 5),
                        0.00040004, 0, 0.00040004, 0.0140014, 0.00310031,
                        0.00840084, 0, 0.05070508, rep(0, 2), 0.01580158,
                        0.00060006,	0.02380238, 0.03960396, 0.0384, 0.0030003,
                        0.013, 0.00020002, rep(0, 9))
Juv.Haddock.diet <- c(rep(0, 22), 0.01020104, 0, 0.001200122, 0, 9.90101E-05,
                      rep(0, 5), 0.1285131, rep(0, 2), 0.0206021, 0,
                      0.0003000306, rep(0, 5), 9.90101E-05, rep(0, 2),
                      0.002100214, 0, 0.01310134, 0, 0.1003102, 0.05020512, 0,
                      0.004100418, 0.0002000204, 0.1130115, 0.2510256,
                      0.03950403, 0.04070415, 0.2193224, 0.00520053,
                      0.0002000204, rep(0, 8))
Adult.Haddock.diet <- c(rep(0, 12), 0.0001000062, rep(0, 3), 0.000500031,
                        rep(0, 5), 0.09080563, 0.003900242, 0.003800236,
                        rep(0, 2), 0.001300081, 0, 0.001000062, rep(0, 2),
                        0.2955183, 3.800236E-05, 0.0002000124, 0.003300204, 0,
                        0.0002000124, 0, 0.0001000062, rep(0, 3), 0.000500031,
                        0, 0.0001000062, 0.001200074, 0.0003000186, 0.002500155,
                        0, 0.05940368, rep(0, 2), 0.02020125, 0.05030312,
                        0.2014125, 0.1511094, 0.008800546, 0.009400583,
                        0.0854053, 0.007300453, 0.001300081, rep(0, 8))
Juv.Saithe.diet <- c(rep(0, 14), 0.0005999898, 0, 9.999829E-05, rep(0, 5),
                     0.2387959, 0, 0.02089964, rep(0, 2), 1.699971E-05, 0,
                     0.115398, rep(0, 2), 0.009499838, rep(0, 14), 0.01059982,
                     0.00649989, 0, 0.3370943, 0.232496, rep(0, 3), 0.0001999966,
                     0.007299876, 0.005199912, 0.01419976, 0.001099981,
                     rep(0, 10))
Adult.Saithe.diet <- c(rep(0, 14), 0.0003548272, 0.02088472, 0.0006968678,
                       0.002131094, rep(0, 3), 0.0362286, 0.3428931, 0,
                       0.03857281, 0, 0.0003196642, 0.004155634, 0.202454,
                       0.06041652, 0.004049079, 0, 0.0214175, 0, 0.0006393283,
                       0.0004262188, rep(0, 9), 0.0001054892, 0.0001054892,
                       0.01097514, 0.002557313, 0, 0.132341, 0.1102841, rep(0, 3),
                       0.0002131094, 0, 0.001278657, 0.006073619, 0.0004262188,
                       rep(0, 10))
Hake.diet <- c(rep(0, 14), 0.00259974, rep(0, 5), 0.00739926, 0.3256674,
               0.1085891, 0, 0.0489951, 0, 0.0029997,	0.00229977, 0, 0.1085891,
               0.1731827, 0.1891811, rep(0, 14), 0.01679832, 0.01109889,
               0.00119988, rep(0, 4), 0.00019998, 0, 0.00109989, 0, 9.999E-05,
               rep(0, 12))
Blue.whiting.diet <- c(rep(0, 22), 0.094, rep(0, 6), 0.0102, rep(0, 17), 0.0102,
                       rep(0, 2), 0.6051, 0.1311, rep(0, 3), 0.0483, 0, 0.1009,
                       0, 0.0002, rep(0, 10))
Norway.pout.diet <- c(rep(0, 50), 0.2857, 0.4762, rep(0, 4), 0.1905, 0, 0.0476,
                      rep(0, 11))
Other.large.gadoids.diet <- c(rep(0, 12), 0.0273, 0, 0.0046, 0, 0.0002,
                              rep(0, 2), 0.0271, 0, 0.0412, 0.1106, 0.0262,
                              0.0145, rep(0, 2), 0.0297, 0.1561, rep(0, 3),
                              0.3822, rep(0, 2), 0.0169, rep(0, 10), 0.007, 0,
                              0.0508, 0, 0.0111, 0.0199, 0, 0.0115, rep(0, 3),
                              0.0425, 0.0086, 0.012, rep(0, 10))
Other.small.gadoids.diet <- c(rep(0, 14), 0.0002990114, rep(0, 7), 0.0003990152,
                              0, 0.015, rep(0, 4), 0.003890149, rep(0, 2),
                              0.000998038, 0, 0.004990189, rep(0, 8),
                              0.004990189, rep(0, 2), 0.01380053, 0,
                              0.0007980303, 0, 0.1002, 0.2996114, 0, 0.01, 0.015,
                              0.1397049, 0.1793, 0.000998038, 0.03, 0.18,
                              rep(0, 10))
Monkfish.diet <- c(rep(0, 8), 0.00759924, rep(0, 3), 0.01149885, 0.01149885,
                   0.009799019, 0.1170883, 0.00279972, 0.0658934, 0.00369963, 0,
                   0.00759924, 0, 0.1320868, rep(0, 2), 0.00369963, 0, 0.00989901,
                   0.1189881, 0.00369963, 0.0679932, 0, 0.169983, 0.00369963,
                   0.03019698, 0.04519548, rep(0, 2), 0.01129887, 0.00369963, 0,
                   0.00369963, 0, 0.00369963, rep(0, 2), 0.04149585, 0.00369963,
                   0.09059094, rep(0, 5), 0.00759924, 0.01129887, rep(0, 14))
Gurnards.diet <- c(rep(0, 8), 0.0003000198, rep(0, 3), 0.01, 0, 0.008000528, 0,
                   0.0003000198, 0, 0.0001000066, rep(0, 2), 0.0001000066, 0.1409,
                   0, 0.07520496, 0, 0.0001000066, 0.0001000066, 0, 0.01560103,
                   0.0008000528, 0.001300086, 0.4, 3.400225E-05, 0.00940062,
                   0.01660109, rep(0, 3), 0.0001000066, rep(0, 3), 0.02350155,
                   rep(0, 2), 0.01890125, 0.0002000132, 0.01640108, 0, 0.01050069,
                   rep(0, 2), 0.06740445, 0.0008000528, 0.01000066, 0.002900191,
                   0.02980197, 0.1402093, 0.0004000264, rep(0, 10))
Juv.Herring.diet <- c(rep(0, 50), 0.2733, 0.6748, rep(0, 6), 0.0519, rep(0, 11))
Adu.Herring.diet <- c(rep(0, 50), 0.3093, 0.6701, rep(0, 6), 0.0206, rep(0, 11))
Sprat.diet <- c(rep(0, 51),0.8,rep(0, 11),0.1,0,0.1,rep(0, 4))
Mackerel.diet <- c(rep(0, 12), 9.999915E-05, 0, 8.399929E-06, rep(0, 6),
                   0.0004999958, 0.009399921, 0,0.0113999, rep(0, 2),
                   0.0009999915, 0, 0.0603995, 0, 0.006299947, 0.2653978,
                   rep(0, 13), 0.0001999983, 0.0001999983, 0.01699986, 0,
                   0.1715986, 0.114499, rep(0, 3), 0.004499962, 9.999915E-05,
                   0.0001999983, 0.03199973, 0.120399, 0.002599978, rep(0, 2),
                   0.1821985, rep(0, 6))
Horse.mackerel.diet <- c(rep(0, 12), 0.00560056, 0, 0.00280028, 0, 0.00090009, 0,
                         0.00110011, rep(0, 2), 0.01680168, 0.2235224, 0,
                         0.0340034, 0, 0.00490049, 0.0040004, 0, 0.00610061,
                         rep(0, 2), 0.04230423, rep(0, 13), 0.00670067, rep(0, 2),
                         0.1192119, 0.4369437, rep(0, 3), 0.03350335, 0,
                         0.0140014, rep(0, 6), 0.04760476, rep(0, 6))
Sandeels.diet <- c(rep(0, 50), 0.0631, 0.6104, rep(0, 3), 0.0008, 0.0056, 0,
                   0.0155, 0.1875, 0, 0.0478, 0, 0.0476, 0, 0.0217, rep(0, 4))
Plaice.diet <- c(rep(0, 27), 0.00020002, rep(0, 4), 0.00750075, rep(0, 19),
                 0.00340034, 0.01590159, 0, 0.1032103, 0.1743, 0.01490149,
                 0.1932193, 0.469947, 0.0140014, 0.00340034, rep(0, 8))
Dab.diet <- c(rep(0, 51), 0.0125, 0, 0.01, 0, 0.2582, 0.2397, 0.001, 0.3288,
              0.1379, 0.011, 0.0009, rep(0, 8))
Long.rough.dab.diet <- c(rep(0, 14), 0.00039996, rep(0, 7), 0.00539946, 0,
                         0.02659734, rep(0, 7), 0.02329767, 0, 0.03789621,
                         rep(0, 11), 0.03759624, 0, 0.0029997, rep(0, 4), 0.03,
                         0.01389861, 0.2104789, 0.2847, 0.04059594, 0.00329967,
                         0.2828717, rep(0, 10))
Flounder.diet <- c(rep(0, 22), 0.0440044, rep(0, 11), 0.06060606, 0.0440044,
                   rep(0, 2), 0.0440044, rep(0, 7), 0.01010101, rep(0, 3),
                   0.05160516, 0, 0.07130713, 0.04890489, 0.1220122, 0.2240224,
                   0.07130713, 0.00660066, 0.1526153, 0, 0.04890489, rep(0, 9))
Sole.diet <- c(rep(0, 46), 0.0128, rep(0, 8), 0.0128, 0.1154, rep(0, 2), 0.4231,
               0, 0.4359, rep(0, 8))
Lemon.sole.diet <- c(rep(0, 46), 0.0068, rep(0, 8), 0.0411, 0.0993, rep(0, 2),
                     0.6576, 0.1952, rep(0, 9))
Witch.diet <- c(rep(0, 46), 0.0082, rep(0, 8), 0.2473, 0.0349, rep(0, 2), 0.6852,
                0.0244, rep(0, 9))
Turbot.diet <- c(rep(0, 12), 0.05759424, 0, 0.01129887, rep(0, 7), 0.1141886, 0,
                 0.0339966, rep(0, 2), 0.00139986, 0, 0.1710829, rep(0, 2),
                 0.1815818, 0, 0.0689931, rep(0, 8), 0.07149285, rep(0, 2),
                 0.1134887, rep(0, 8), 0.06809319, 0.05669433, 0.05009499,
                 rep(0, 12))
Megrim.diet <- c(rep(0, 21), 0.05409459, 0.239576, 0, 0.2271773, 0, 0.00619938,
                 rep(0, 2), 0.3376662, rep(0, 2), 0.01839816, rep(0, 10),
                 0.03069693, rep(0, 2), 0.00619938, 0.02449755, rep(0, 3),
                 0.00279972, 0, 0.02769723, 0, 0.00279972, 0, 0.02079792,
                 0.00139986, rep(0, 11))
Halibut.diet <- c(rep(0, 12), 0.01050004, 0, 0.0509543, 0, 0.01000004, rep(0, 5),
                  0.2034112, 0, 0.03024777, rep(0, 4), 0.1017056, rep(0, 2),
                  0.4070254, rep(0, 15), 0.08160807, rep(0, 4), 0.09967554, 0,
                  0.003654093, 0, 0.001218031, rep(0, 12))
Dragonets.diet <- c(rep(0, 51), 0.0045, 0, 0.008, 0, 0.0925, 0.1375, 0.0805,
                    0.2205, 0.439, 0, 0.0175, rep(0, 8))
Catfish.Wolffish.diet <- c(rep(0, 50), 0.0153, rep(0, 2), 0.051, 0, 0.7829,
                           0.1131, 0.0367, 0, 0.001, rep(0, 10))
Large.demersal.fish.diet <- c(rep(0, 21), 0.0083, rep(0, 2), 0.0258, rep(0, 4),
                              0.0005, 0, 0.0061, 6.00E-05, 0, 0.0002, rep(0, 8),
                              0.0005, 0, 0.0015, 0.0658, 0.0056, 4.00E-05, 0,
                              0.6774, 0.0765, 0.0008, rep(0, 2), 0.0127, 0.0255,
                              0.0617, 0.0039, 0.0212, 0.0051, 0.0008, rep(0, 8))
Small.demersal.fish.diet <- c(rep(0, 22), 0.07870173, rep(0, 3), 0.0101009,
                              3.300294E-05, 0, 0.001300116, rep(0, 2), 0.2500055,
                              0, 0.0371033, rep(0, 2), 0.0005000445, rep(0, 5),
                              0.005300472, rep(0, 2), 0.03650325, 0, 0.02550227,
                              0, 0.0002000178, 0.09110811, 0, 0.03, 0, 0.1339119,
                              0.0797, 0.02010179, 0.1314117, 0.0393035, 0,
                              0.0292026, rep(0, 8))
Miscellaneous.filterfeeding.pelagics.diet <- c(rep(0, 47), 0.0531, 0.0056, 0,
                                               0.1489, 0.6949, 0.0332, rep(0, 2),
                                               0.0266, 0, 0.0144, 0.0122, 0.0111,
                                               rep(0, 10))
Squid.Cuttlefish.diet <- c(rep(0, 12), 0.01069893, 0, 0.00089991, 0, 0.00039996,
                           0, 0.01069893, rep(0, 2), 0.01059894, 0.01059894,
                           rep(0, 4), 0.00089991, 0, 0.01069893, 0, 0.01069893,
                           0.04269573, 0.00539946, 0.00539946, 0.00539946,
                           0.00539946, 0.00539946, 0.00539946, 0.00539946,
                           rep(0, 6), 0.00539946, 0.02119788, 0.05329467,
                           0.01029897, 0.2182782, 0.2890711, rep(0, 5),
                           0.02069793, 0.1605839, 0.02119788, rep(0, 5),
                           0.05329467, rep(0, 4))
Fish.larvae.diet <- c(rep(0, 51), 0.8, rep(0, 13), 0.2, rep(0, 4))
Carnivorous.zooplankton.diet <- c(rep(0, 49), 0.0245, 0.0509, 0.7223, rep(0, 6),
                                  0.1005, rep(0, 4), 0.1018, rep(0, 6))
HerbivorousOmnivorous.zooplankton.diet <- c(rep(0, 63), 0.05, 0, 0.9, 0.05,
                                            rep(0, 3))
Gelatinous.zooplankton.diet <- c(rep(0, 48), 0.0498, 0.224, 0.1989, 0.1989,
                                 rep(0, 5),
                                 0.0498, 0.0796, rep(0, 4), 0.0995, 0, 0.0995,
                                 rep(0, 4))
Large.crabs.diet <- c(rep(0, 53), 0.00100998, 0.0197996, 0.2887942, 0.2950941,
                      0.00100998, 0.0197996, 0.1042979, 0.07049859, 0,
                      0.04949901, 0.04949901, rep(0, 4), 0.100698, 0)
Nephrops.diet <- c(rep(0, 56), 0.25, 0, 0.15, 0.2, 0.1, 0, 0.1, 0.05, 0, 0.1,
                   0.03, 0.02, rep(0, 2))
Epifaunal.macrobenthos.diet <- c(rep(0, 55), 0.05, 0.2, 0, 0.05, 0.25, rep(0, 2),
                                 0.1, 0.1, 0, 0.05, 0.05, 0.15, rep(0, 2))
Infaunal.macrobenthos.diet <- c(rep(0, 58), 0.03, 0.11, 0, 0.05, 0.3, 0.11,
                                rep(0, 2), 0.1, 0.3, rep(0, 2))
Shrimp.diet <- c(rep(0, 50), 0.05, 0.1, rep(0, 5), 0.01, 0.05, 0.1, 0, 0.1,
                 0.15, 0.12, rep(0, 2), 0.1, 0.17, 0.05, 0)
Small.mobile.epifauna.diet <- c(rep(0, 58), 0.075, rep(0, 2), 0.15, 0.2, 0.175,
                                rep(0, 2), 0.1, 0.3, rep(0, 2))
Small.infauna.diet <- c(rep(0, 59), 0.05, 0, 0.15, 0.4, 0.05, rep(0, 2), 0.15,
                        0.2, rep(0, 2))
Sessile.epifauna.diet <- c(rep(0, 63), 0.7, 0, 0.1, 0, 0.2, rep(0, 2))
Meiofauna.diet <- c(rep(0, 61), 0.05, 0.75, rep(0, 4), 0.2, rep(0, 2))
Benthic.microflora.diet <- c(rep(0, 62), 0.1, 0.1, rep(0, 2), 0.2, 0.6,
                             rep(0, 2))
Planktonic.microflora.diet <- c(rep(0, 62), 0.02, 0.1, rep(0, 2), 0.65, 0.23,
                                rep(0, 2))
Diving.seabirds.diet <- c(rep(0, 12), 0.00229954, 0, 0.0179964, 0, 0.0006998599,
                          rep(0, 5), 0.00019996, 0.00119976, 0.0004999, 0,
                          9.999999E-05, 0, 0.05898819, 0.05438911, 0.02009598, 0,
                          0.3325334, 0.00029994, 9.999999E-05, rep(0, 10),
                          9.999999E-05, 0.00079984, 0, 9.999999E-05, 0,
                          9.999999E-05, 0.0005998799, 0, 0.01039792, rep(0, 2),
                          0.3593282, rep(0, 2), 0.0004999, 0.1065787, rep(0, 7),
                          0.03209357, 0)
Rpath.parameters$diet[, BaleenWhales := Baleen.Whales.diet]
Rpath.parameters$diet[, ToothedWhales := Tooth.Whale.diet]
Rpath.parameters$diet[, Seals := Seals.diet]
Rpath.parameters$diet[, SurfaceFeedingSeabirds := Surf.Birds.diet]
Rpath.parameters$diet[, JuvenileSharks := Juv.Sharks.diet]
Rpath.parameters$diet[, Spurdog := Spurdog.diet]
Rpath.parameters$diet[, LargePiscivorousSharks := Pisc.Sharks.diet]
Rpath.parameters$diet[, SmallSharks := Small.Sharks.diet]
Rpath.parameters$diet[, JuvenileRays := Juv.Rays.diet]
Rpath.parameters$diet[, StarryRayOthers := Star.Ray.Othr.diet]
Rpath.parameters$diet[, ThornbackAndSpottedRay := Thornback.SppotedRay.diet]
Rpath.parameters$diet[, SkateCuckooRay := Skate.Cucko.diet]
Rpath.parameters$diet[, JuvCod := Juv.Cod.diet]
Rpath.parameters$diet[, AduCod := Adult.Cod.diet]
Rpath.parameters$diet[, JuvWhiting := Juv.Whiting.diet]
Rpath.parameters$diet[, AduWhiting := Adult.Whiting.diet]
Rpath.parameters$diet[, JuvHaddock := Juv.Haddock.diet]
Rpath.parameters$diet[, AduHaddock := Adult.Haddock.diet]
Rpath.parameters$diet[, JuvSaithe := Juv.Saithe.diet]
Rpath.parameters$diet[, AduSaithe := Adult.Saithe.diet]
Rpath.parameters$diet[, Hake := Hake.diet]
Rpath.parameters$diet[, BlueWhiting := Blue.whiting.diet]
Rpath.parameters$diet[, NorwayPout := Norway.pout.diet]
Rpath.parameters$diet[, OtherLargeGadoids := Other.large.gadoids.diet]
Rpath.parameters$diet[, OtherSmallGadoids := Other.small.gadoids.diet]
Rpath.parameters$diet[, Monkfish := Monkfish.diet]
Rpath.parameters$diet[, Gurnards := Gurnards.diet]
Rpath.parameters$diet[, JuvHerring := Juv.Herring.diet]
Rpath.parameters$diet[, AduHerring := Adu.Herring.diet]
Rpath.parameters$diet[, Sprat := Sprat.diet]
Rpath.parameters$diet[, Mackerel := Mackerel.diet]
Rpath.parameters$diet[, HorseMackerel := Horse.mackerel.diet]
Rpath.parameters$diet[, Sandeels := Sandeels.diet]
Rpath.parameters$diet[, Plaice := Plaice.diet]
Rpath.parameters$diet[, Dab := Dab.diet]
Rpath.parameters$diet[, LongRoughDab := Long.rough.dab.diet]
Rpath.parameters$diet[, Flounder := Flounder.diet]
Rpath.parameters$diet[, Sole := Sole.diet]
Rpath.parameters$diet[, LemonSole := Lemon.sole.diet]
Rpath.parameters$diet[, Witch := Witch.diet]
Rpath.parameters$diet[, Turbot := Turbot.diet]
Rpath.parameters$diet[, Megrim := Megrim.diet]
Rpath.parameters$diet[, Halibut := Halibut.diet]
Rpath.parameters$diet[, Dragonets := Dragonets.diet]
Rpath.parameters$diet[, CatfishWolfFish := Catfish.Wolffish.diet]
Rpath.parameters$diet[, LargeDemersalFish := Large.demersal.fish.diet]
Rpath.parameters$diet[, SmallDemersalFish := Small.demersal.fish.diet]
Rpath.parameters$diet[, MiscellaneousFilterfeedingPelagicFish :=
                        Miscellaneous.filterfeeding.pelagics.diet]
Rpath.parameters$diet[, SquidAndCuttlefish := Squid.Cuttlefish.diet]
Rpath.parameters$diet[, FishLarvae := Fish.larvae.diet]
Rpath.parameters$diet[, CarnivorousZooplankton := Carnivorous.zooplankton.diet]
Rpath.parameters$diet[, HerbivorousAndOmnivorousZooplanktonCopepods :=
                        HerbivorousOmnivorous.zooplankton.diet]
Rpath.parameters$diet[, GelatinousZooplankton := Gelatinous.zooplankton.diet]
Rpath.parameters$diet[, LargeCrabs := Large.crabs.diet]
Rpath.parameters$diet[, Nephrops := Nephrops.diet]
Rpath.parameters$diet[, EpifaunalMacrobenthosMobileGrazers :=
                        Epifaunal.macrobenthos.diet]
Rpath.parameters$diet[, InfaunalMacrobenthos := Infaunal.macrobenthos.diet]
Rpath.parameters$diet[, Shrimp := Shrimp.diet]
Rpath.parameters$diet[, SmallMobileEpifaunaSwarmingCrustaceans :=
                        Small.mobile.epifauna.diet]
Rpath.parameters$diet[, SmallInfaunaPolychaetes := Small.infauna.diet]
Rpath.parameters$diet[, SessileEpifauna := Sessile.epifauna.diet]
Rpath.parameters$diet[, Meiofauna := Meiofauna.diet]
Rpath.parameters$diet[, BenthicMicrofloraInclBacteriaProtozoa :=
                        Benthic.microflora.diet]
Rpath.parameters$diet[, PlanktonicMicrofloraInclBacteriaProtozoa :=
                        Planktonic.microflora.diet]
Rpath.parameters$diet[, DivingSeabirds := Diving.seabirds.diet]
#Rpath model
check.rpath.params(Rpath.parameters)
Rpath.model <- rpath(Rpath.parameters, eco.name = 'North Sea Fishery')

#### RSIM ####
sim.years <- 1000
Rsim.model <- rsim.scenario(Rpath.model, Rpath.parameters, years = 1:sim.years)
# VVs
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "all", groupto = "StarryRayOthers",
                              value = 1.31)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "all", groupto = "AduCod",
                              value = 5.00)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "all", groupto = "JuvWhiting",
                              value = 3.00)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "all", groupto = "AduWhiting",
                              value = 5.00)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "all", groupto = "JuvHaddock",
                              value = 2.50)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "all", groupto = "AduHaddock",
                              value = 4.00)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "all", groupto = "AduSaithe",
                              value = 2.10)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "all", groupto = "Hake",
                              value = 3.00)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "all", groupto = "BlueWhiting",
                              value = 4.00)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "all", groupto = "NorwayPout",
                              value = 3.32)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "all", groupto = "AduHerring",
                              value = 10.00)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "all", groupto = "Sprat",
                              value = 5.00)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "all", groupto = "Mackerel",
                              value = 4.00)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "all", groupto = "HorseMackerel",
                              value = 5.50)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "all", groupto = "Sandeels",
                              value = 5.00)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "all", groupto = "Plaice",
                              value = 4.00)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "all", groupto = "Sole",
                              value = 10.00)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "all", groupto = "Turbot",
                              value = 3.35)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "all", groupto = "CarnivorousZooplankton",
                              value = 1.10)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "all",
                              groupto = "HerbivorousAndOmnivorousZooplanktonCopepods",
                              value = 100.00)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "all", groupto = "Nephrops",
                              value = 10.00)
Rsim.model <- adjust.scenario(Rsim.model, parameter = "VV",
                              group = "all", groupto = "Shrimp",
                              value = 2.45)
# # Effort
# Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedEffort",
#                              group = "DemersalTrawlAndDemSeine",
#                              sim.year = 1:sim.years, sim.month = 0,
#                              value = c(1, 0.9661622, 0.9008364, 0.8920089,
#                                        0.8183876, 0.7724553, 0.7771879,
#                                        0.7195613, 0.5357047, 0.4764302,
#                                        0.4432984, 0.5151287, 0.2882786,
#                                        0.2315721, 0.2316745, 0.2358405,
#                                        0.2092922, 0.2306971, 0.2287765,
#                                        0.2059996, 0.1929958, 0.1910192,
#                                        rep(0.1910192, sim.years)))
# Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedEffort",
#                              group = "BeamTrawl",
#                              sim.year = 1:sim.years, sim.month = 0,
#                              value = c(1, 1.158772, 1.163457, 1.107756,
#                                        0.9692869, 0.8068714, 0.7719938,
#                                        0.7235815, 0.707444, 0.6371682, 0.6135205,
#                                        0.6017498, 0.5790833, 0.5652371, 0.548806,
#                                        0.4926404, 0.4628968, 0.3431652, 0.342384,
#                                        0.3304436, 0.2886084, 0.2645107,
#                                        rep(0.2645107, sim.years)))
# Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedEffort",
#                              group = "IndustrialTrawl",
#                              sim.year = 1:sim.years, sim.month = 0,
#                              value = c(1, 0.928718, 0.8205128, 0.7497436,
#                                        0.8605128, 0.9307692, 0.988718, 1.027179,
#                                        0.9620513, 0.8538461, 0.9104788, 0.8312972,
#                                        0.8049601, 0.7809468, 0.5620207, 0.4959471,
#                                        0.3193114, 0.4176928, 0.45337, 0.490015,
#                                        0.4671607, 0.2897705,
#                                        rep(0.2897705, sim.years)))
# Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedEffort",
#                              group = "PelagicTrawl",
#                              sim.year = 1:sim.years, sim.month = 0,
#                              value = c(1, 1.167721, 1.081822, 0.8723464, 0.640658,
#                                        0.5754253, 0.555743, 0.3885843, 0.5480107,
#                                        0.6083228, 0.6137543, 0.8861227, 0.8492573,
#                                        0.864426, 0.7450742, 0.6373039, 0.5757004,
#                                        0.3831036, 0.4163267, 0.3881139, 0.3729359,
#                                        0.5383003, rep(0.5383003, sim.years)))
# Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedEffort",
#                              group = "DriftAndFixedNets",
#                              sim.year = 1:sim.years, sim.month = 0,
#                              value = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1.175745,
#                                        1.254202, 1.332659, 1.326172, 1.401343,
#                                        1.704298, 1.59184, 1.349401, 1.386385,
#                                        1.195318, 1.25025, 1.185508,
#                                        rep(1.185508, sim.years)))
# Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedEffort",
#                              group = "NephropsTrawl",
#                              sim.year = 1:sim.years, sim.month = 0,
#                              value = c(1, 0.7685972, 0.9619048, 0.9554697,
#                                        0.8190476, 0.7467182, 0.8190476, 0.7081081,
#                                        0.7788932, 0.8478764, 1.113683, 1.463047,
#                                        1.812411, 1.836008, 1.64243, 1.656385,
#                                        1.618668, 1.507889, 1.41245, 1.225774,
#                                        1.14383, 1.028296,
#                                        rep(1.028296, sim.years)))
# Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedEffort",
#                              group = "GearsUsingHooks",
#                              sim.year = 1:sim.years, sim.month = 0,
#                              value = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1.179123,
#                                        1.175461, 1.171798, 1.079823, 1.090337,
#                                        1.041603, 1.344547, 1.650768, 2.065099,
#                                        1.896589, 1.656842, 1.56463,
#                                        rep(1.56463, sim.years)))
# Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedEffort",
#                              group = "ShrimpTrawlers",
#                              sim.year = 1:sim.years, sim.month = 0,
#                              value = c(1, 1.058674, 1.142857, 0.9464286, 1.020408,
#                                        0.9897959, 0.9566327, 0.9311224, 0.8852041,
#                                        0.8443878, 0.8689169, 0.8638875, 0.9404085,
#                                        0.9218259, 0.9020348, 0.8828997, 0.9419596,
#                                        0.9143829, 0.9605693, 0.8544816, 0.6161624,
#                                        0.8567501, rep(0.8567501, sim.years)))
# Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedEffort",
#                              group = "Dredges",
#                              sim.year = 1:sim.years, sim.month = 0,
#                              value = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.8929281,
#                                        1.168011, 1.661234, 1.84122, 2.519476,
#                                        1.403339, 1.64566, 1.410239, 1.64145,
#                                        1.86502, 1.905645, 1.606653,
#                                        rep(1.606653, sim.years)))
# Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedEffort",
#                              group = "Pots",
#                              sim.year = 1:sim.years, sim.month = 0,
#                              value = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1.093526,
#                                        1.540655, 1.970986, 2.014524, 2.027584,
#                                        3.037664, 3.119561, 3.011612, 3.247285,
#                                        3.673495, 3.339094, 3.451751,
#                                        rep(3.451751, sim.years)))
# Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedEffort",
#                              group = "Other",
#                              sim.year = 1:sim.years, sim.month = 0,
#                              value = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0.2498454,
#                                        0.4016317, 1.135766, 1.150056, 0.7931818,
#                                        0.6674658, 0.6453203, 0.9188693, 1.274428,
#                                        0.977429, 1.08173, 1.024884,
#                                        rep(1.024884, sim.years)))
gearname <-
  colnames(Rsim.model$fishing$ForcedEffort[, 2:ncol(Rsim.model$fishing$ForcedEffort)])
for (i in 1:length(gearname)) {
  Rsim.model <<- adjust.fishing(Rsim.model, parameter = "ForcedEffort",
                                group = gearname[i],
                                sim.year = 1:sim.years, sim.month = 0,
                                value = 0)
}
# Frate
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "Spurdog",
                             sim.year = 1:sim.years, sim.month = 0,
                             value = c(0.06748538, 0.06384049, 0.06477566,
                                       0.06430732, 0.06085224, 0.05882563,
                                       0.05989879, 0.05714124, 0.0539889,
                                       0.05342437, 0.0425032, 0.05267617,
                                       0.06816681, 0.06744768, 0.0580283,
                                       0.05889159, 0.05613846, 0.05843773,
                                       0.06496666, 0.05442911, 0.05547611,
                                       0.05205923, rep(0.05205923 ,sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "LargePiscivorousSharks",
                             sim.year = 1:sim.years, sim.month = 0,
                             value = c(0.2723897, 0.242037, 0.267125, 0.261116,
                                       0.2346246, 0.2218081, 0.2319781,
                                       0.2109641, 0.2220261, 0.2325012,
                                       0.2849482, 0.3482512, 0.4002458,
                                       0.4003381, 0.3734932, 0.3897044,
                                       0.3837173, 0.3576325, 0.3581458,
                                       0.3140359, 0.2984014, 0.2790882,
                                       rep(0.2790882, sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "SmallSharks",
                             sim.year = 1:sim.years, sim.month = 0,
                             value = c(0.1313901, 0.127594, 0.1304323,
                                       0.1299316, 0.1276828, 0.1265803,
                                       0.1278854, 0.1259604, 0.1271448,
                                       0.128095, 0.06501783, 0.08671062,
                                       0.1629765, 0.164575, 0.1284269,
                                       0.1221199, 0.1171064, 0.136941,
                                       0.1697127, 0.13572, 0.1448626,
                                       0.1363388, rep(0.1363388, sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "StarryRayOthers",
                             sim.year = 1:sim.years, sim.month = 0,
                             value = c(0.03291125, 0.03173007, 0.03227459,
                                       0.03161699, 0.02819904, 0.02555818,
                                       0.02595708, 0.0237237, 0.02117184,
                                       0.02018891, 0.02167945, 0.02562513,
                                       0.02463348, 0.02378762, 0.02204378,
                                       0.0218178, 0.02078643, 0.01923115,
                                       0.01842452, 0.01639349, 0.01517819,
                                       0.01396182, rep(0.01396182 ,sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "ThornbackAndSpottedRay",
                             sim.year = 1:sim.years, sim.month = 0,
                             value = c(0.1693052, 0.1624679, 0.1656106,
                                       0.1623265, 0.1447526, 0.1313503,
                                       0.1336093, 0.1219985, 0.1090022,
                                       0.1042062, 0.1124078, 0.1333952,
                                       0.1289486, 0.1246592, 0.1153556,
                                       0.1143203, 0.1090042, 0.1010509,
                                       0.09672111, 0.08595101, 0.07962468,
                                       0.07320186, rep(0.07320186, sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "SkateCuckooRay",
                             sim.year = 1:sim.years, sim.month = 0,
                             value = c(0.1351172, 0.1200511, 0.1256601,
                                       0.1234262, 0.1112695, 0.1043071,
                                       0.1091255, 0.09912488, 0.09119981,
                                       0.09016653, 0.1035976, 0.1275977,
                                       0.1328236, 0.130343, 0.1168615,
                                       0.1168692, 0.1110426, 0.1063052,
                                       0.1012405, 0.08945999, 0.08361328,
                                       0.07537381, rep(0.07537381, sim.years)))

Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "JuvCod",
                             sim.year = 1:sim.years, sim.month = 0,
                             value = c(0.9792305, 0.9326808, 0.9007916,
                                       0.8904805, 0.8125607, 0.7625397,
                                       0.7732114, 0.7136685, 0.5734791,
                                       0.5302772, 0.5353407, 0.6318769,
                                       0.4908896, 0.4473322, 0.4258243,
                                       0.4361171, 0.4054652, 0.4007396,
                                       0.3908083, 0.3465365, 0.3273072,
                                       0.309365, rep(0.309365, sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "AduCod",
                             sim.year = 1:sim.years, sim.month = 0,
                             value = c(0.9556606, 0.9287899, 0.8791562,
                                       0.868203, 0.7960251, 0.74745,
                                       0.7518943, 0.6981175, 0.5449572,
                                       0.4930902, 0.4728722, 0.5447987,
                                       0.3689823, 0.3210486, 0.3139321,
                                       0.3217477, 0.2936258, 0.2983609,
                                       0.2977913, 0.2663719, 0.2522852,
                                       0.2433686, rep(0.2433686, sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "JuvWhiting",
                             sim.year = 1:sim.years, sim.month = 0,
                             value = c(0.2469109, 0.2307551, 0.2336538,
                                       0.2292868, 0.2064414, 0.1911218,
                                       0.196131, 0.1791988, 0.1575509,
                                       0.1511571, 0.1644039, 0.1976662,
                                       0.1885628, 0.181643, 0.1660903,
                                       0.1655036, 0.1567155, 0.1491653,
                                       0.142887, 0.1268569, 0.118083,
                                       0.1082848, rep(0.1082848,sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "AduWhiting",
                             sim.year = 1:sim.years, sim.month = 0,
                             value = c(0.5848178, 0.5566671, 0.5574121,
                                       0.5509851, 0.5123409, 0.487694,
                                       0.4957563, 0.4664957, 0.4228888,
                                       0.4110115, 0.3070264, 0.3879266,
                                       0.4798621, 0.4684505, 0.3877251,
                                       0.3673856, 0.3499375, 0.3850587,
                                       0.4330846, 0.358866, 0.3622956,
                                       0.3393892, rep(0.3393892, sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "JuvHaddock",
                             sim.year = 1:sim.years, sim.month = 0,
                             value = c(0.5090012, 0.4800213, 0.4622742,
                                       0.4576969, 0.4168561, 0.3920874,
                                       0.3986533, 0.3663684, 0.2886222,
                                       0.2660493, 0.2673879, 0.3201917,
                                       0.2401556, 0.2162571, 0.2040756,
                                       0.2065544, 0.1919856, 0.1949733,
                                       0.1885159, 0.1672055, 0.1563924,
                                       0.148106, rep(0.148106, sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "AduHaddock",
                             sim.year = 1:sim.years, sim.month = 0,
                             value = c(0.7936782, 0.7612296, 0.717249,
                                       0.7100445, 0.6499566, 0.6127393,
                                       0.6185541, 0.5714129, 0.4339085,
                                       0.3906797, 0.3735808, 0.4385741,
                                       0.2769679, 0.2345683, 0.2282929,
                                       0.231655, 0.209961, 0.222793,
                                       0.2186504, 0.1956841, 0.1832086,
                                       0.1778537, rep(0.1778537, sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "JuvSaithe",
                             sim.year = 1:sim.years, sim.month = 0,
                             value = c(0.639474, 0.6160051, 0.5767811,
                                       0.5708078, 0.5236112, 0.4943666,
                                       0.4981889, 0.4610792, 0.3470206,
                                       0.3104643, 0.2916774, 0.3402272,
                                       0.2030448, 0.1678488, 0.1647864,
                                       0.1671371, 0.1496023, 0.1622891,
                                       0.1607947, 0.1443407, 0.1354533,
                                       0.13261, rep(0.13261, sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "AduSaithe",
                             sim.year = 1:sim.years, sim.month = 0,
                             value = c(0.5587321, 0.5390616, 0.5038157,
                                       0.4984353, 0.4576058, 0.4322984,
                                       0.43542, 0.4032893, 0.3031082,
                                       0.2707406, 0.2528422, 0.2942095,
                                       0.1731674, 0.1422317, 0.1397728,
                                       0.1416021, 0.126167, 0.1378863,
                                       0.1372581, 0.123346, 0.1159053,
                                       0.1137075, rep(0.1137075, sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "Hake",
                             sim.year = 1:sim.years, sim.month = 0,
                             value = c(0.3777348, 0.3652023, 0.3426217,
                                       0.3387268, 0.3110691, 0.2939616,
                                       0.2959431, 0.2743765, 0.2096713,
                                       0.1888833, 0.1801255, 0.2085752,
                                       0.1301967, 0.1095869, 0.1084426,
                                       0.1109084, 0.101993, 0.1089056,
                                       0.110566, 0.09931138, 0.09313026,
                                       0.09119977, rep(0.09119977, sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "BlueWhiting",
                             sim.year = 1:sim.years, sim.month = 0,
                             value = c(0.2671749, 0.3091757, 0.2859887,
                                       0.2316658, 0.1737652, 0.1579129,
                                       0.1535606, 0.1113056, 0.1512667,
                                       0.1654176, 0.1674883, 0.2361609,
                                       0.2264551, 0.2300495, 0.1970274,
                                       0.1687654, 0.1509709, 0.1028924,
                                       0.1117906, 0.1049824, 0.1008419,
                                       0.1410101, rep(0.1410101, sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "NorwayPout",
                             sim.year = 1:sim.years, sim.month = 0,
                             value = c(0.2548527, 0.2406531, 0.2181006,
                                       0.2028986, 0.2250457, 0.2393331,
                                       0.2512191, 0.2586533, 0.2457,
                                       0.2235767, 0.2009085, 0.1923451,
                                       0.2204356, 0.2161857, 0.1543668,
                                       0.1346741, 0.09707335, 0.1293113,
                                       0.1530466, 0.1469118, 0.1469334,
                                       0.108255, rep(0.108255, sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "OtherLargeGadoids",
                             sim.year = 1:sim.years, sim.month = 0,
                             value = c(0.5586793, 0.5391415, 0.5057876,
                                       0.5005744, 0.4597902, 0.4342544,
                                       0.4373889, 0.405544, 0.3081724,
                                       0.2767999, 0.2635401, 0.3054461,
                                       0.1881714, 0.157727, 0.1561777,
                                       0.1604348, 0.145639, 0.154859,
                                       0.1548378, 0.1390006, 0.130762,
                                       0.1275458, rep(0.1275458, sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "OtherSmallGadoids",
                             sim.year = 1:sim.years, sim.month = 0,
                             value = c(0.09810653, 0.1045207, 0.1001383,
                                       0.09142777, 0.08093411, 0.07764144,
                                       0.0769017, 0.06926981, 0.0733625,
                                       0.07505047, 0.0420717, 0.06085541,
                                       0.08841707, 0.08890909, 0.06840982,
                                       0.05855271, 0.0547019, 0.0590324,
                                       0.075899, 0.06144968, 0.06520583,
                                       0.06947155, rep(0.06947155, sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "Monkfish",
                             sim.year = 1:sim.years, sim.month = 0,
                             value = c(0.5358454, 0.5181801, 0.4904701,
                                       0.4850892, 0.4457896, 0.4202478,
                                       0.4233288, 0.3933674, 0.3071118,
                                       0.2788408, 0.2717551, 0.3136364,
                                       0.2142117, 0.1873511, 0.1851247,
                                       0.1935226, 0.1766735, 0.1771472,
                                       0.1757874, 0.1567567, 0.1497741,
                                       0.1442636, rep(0.1442636, sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "Gurnards",
                             sim.year = 1:sim.years, sim.month = 0,
                             value = c(0.04454258, 0.04877998, 0.04892053,
                                       0.0469939, 0.04140807, 0.03546557,
                                       0.03457304, 0.03215425, 0.0302083,
                                       0.02758043, 0.02728598, 0.02854879,
                                       0.0268213, 0.02593529, 0.02483486,
                                       0.02307076, 0.02175213, 0.01769087,
                                       0.01737194, 0.01623462, 0.01450601,
                                       0.0133663, rep(0.0133663, sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "JuvHerring",
                             sim.year = 1:sim.years, sim.month = 0,
                             value = c(0.1505345, 0.1706173, 0.1577191,
                                       0.1294017, 0.101155, 0.09372876,
                                       0.09224845, 0.07108948, 0.08993442,
                                       0.09569243, 0.09750535, 0.1317529,
                                       0.1261764, 0.1275441, 0.1082527,
                                       0.0932667, 0.08222932, 0.05907743,
                                       0.06389773, 0.0605913, 0.05799639,
                                       0.07627144, rep(0.07627144, sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "AduHerring",
                             sim.year = 1:sim.years, sim.month = 0,
                             value = c(0.3206203, 0.3694092, 0.3416002,
                                       0.2773877, 0.2100309, 0.1918195,
                                       0.1870899, 0.1376708, 0.1842152,
                                       0.2001962, 0.202694, 0.2830447,
                                       0.2719801, 0.2760694, 0.2357135,
                                       0.2020622, 0.1800743, 0.1243294,
                                       0.1351327, 0.1271508, 0.1221679,
                                       0.16818, rep(0.16818, sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "Sprat",
                             sim.year = 1:sim.years, sim.month = 0,
                             value = c(0.2597637, 0.2414043, 0.213317,
                                       0.1948481, 0.2233953, 0.2415543,
                                       0.2565545, 0.2664106, 0.2496408,
                                       0.2216474, 0.2362944, 0.2159647,
                                       0.2091498, 0.2029396, 0.1461339,
                                       0.1289414, 0.08314017, 0.1085118,
                                       0.1177909, 0.1272533, 0.1213271,
                                       0.07547374, rep(0.07547374, sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "Mackerel",
                             sim.year = 1:sim.years, sim.month = 0,
                             value = c(0.2917213, 0.3336549, 0.3108558,
                                       0.2539369, 0.1920683, 0.1747819,
                                       0.1708487, 0.1255909, 0.1675841,
                                       0.1828213, 0.1872149, 0.2621878,
                                       0.2546381, 0.2583757, 0.2221405,
                                       0.1926895, 0.1739817, 0.1231429,
                                       0.1321909, 0.1232028, 0.1179164,
                                       0.1588287, rep(0.1588287, sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "HorseMackerel",
                             sim.year = 1:sim.years, sim.month = 0,
                             value = c(0.1182445, 0.1341098, 0.1249158,
                                       0.1036661, 0.07892497, 0.07148334,
                                       0.06973249, 0.05178499, 0.06519572,
                                       0.0705065, 0.07115142, 0.1004252,
                                       0.09407137, 0.0947886, 0.08233415,
                                       0.07157531, 0.06488358, 0.04556451,
                                       0.04866857, 0.04505629, 0.0431244,
                                       0.05948126, rep(0.05948126, sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "Sandeels",
                             sim.year = 1:sim.years, sim.month = 0,
                             value = c(0.4598078, 0.4399778, 0.3914758,
                                       0.3514404, 0.383826, 0.4087757,
                                       0.4312019, 0.4377516, 0.4199524,
                                       0.3793389, 0.4024726, 0.3851279,
                                       0.372582, 0.3636647, 0.2683994,
                                       0.2357595, 0.1608246, 0.1903411,
                                       0.2066668, 0.2199339, 0.2098362,
                                       0.1468737, rep(0.1468737, sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "Plaice",
                             sim.year = 1:sim.years, sim.month = 0,
                             value = c(0.6569552, 0.6990901, 0.6954008,
                                       0.6704323, 0.5969682, 0.5225948,
                                       0.513496, 0.4773988, 0.4328262,
                                       0.3959099, 0.3925455, 0.4199598,
                                       0.3770991, 0.3600342, 0.3456358,
                                       0.3277537, 0.3089005, 0.2624824,
                                       0.2582618, 0.2384357, 0.2143674,
                                       0.2017529, rep(0.2017529, sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "Dab",
                             sim.year = 1:sim.years, sim.month = 0,
                             value = c(0.05630039, 0.05954825, 0.06059818,
                                       0.05854896, 0.05241673, 0.0461702,
                                       0.04558402, 0.04267707, 0.04110761,
                                       0.03880068, 0.03435993, 0.0382586,
                                       0.04346379, 0.04279954, 0.03827803,
                                       0.03570087, 0.03407532, 0.03142972,
                                       0.03326322, 0.02911018, 0.02767575,
                                       0.02572964, rep(0.02572964, sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "LongRoughDab",
                             sim.year = 1:sim.years, sim.month = 0,
                             value = c(0.00289776, 0.002450737, 0.002695577,
                                       0.002654825, 0.002382344, 0.002235582,
                                       0.002380342, 0.002146723, 0.002089827,
                                       0.002127138, 0.002572051, 0.003219497,
                                       0.003613752, 0.003597793, 0.003202155,
                                       0.003210505, 0.003072471, 0.002925358,
                                       0.002769243, 0.002436807, 0.002277456,
                                       0.002029199,
                                       rep(0.002029199, sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "Flounder",
                             sim.year = 1:sim.years, sim.month = 0,
                             value = c(0.25344, 0.2936792, 0.2948665,
                                       0.2807497, 0.2456561, 0.2044935,
                                       0.1956541, 0.1833845, 0.1792946,
                                       0.1614839, 0.1554906, 0.1525075,
                                       0.1467629, 0.1432537, 0.1390894,
                                       0.1248548, 0.1173166, 0.08697178,
                                       0.0867738, 0.08374762, 0.07314491,
                                       0.06703759, rep(0.06703759, sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "Sole",
                             sim.year = 1:sim.years, sim.month = 0,
                             value = c(0.4573761, 0.5201656, 0.5235257,
                                       0.4999813, 0.4419011, 0.3746057,
                                       0.3609851, 0.339746, 0.3327529,
                                       0.3042189, 0.3022335, 0.3035391,
                                       0.2988022, 0.2928212, 0.2864291,
                                       0.2726652, 0.2566111, 0.1992838,
                                       0.1994327, 0.1867246, 0.1698693,
                                       0.1575716, rep(0.1575716, sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "LemonSole",
                             sim.year = 1:sim.years, sim.month = 0,
                             value = c(0.1049307, 0.1058559, 0.1023207,
                                       0.1000678, 0.09027313, 0.08208337,
                                       0.08180506, 0.07580356, 0.06270377,
                                       0.05686092, 0.05522345, 0.06179553,
                                       0.04700459, 0.04271883, 0.04125788,
                                       0.04018923, 0.03718031, 0.03463879,
                                       0.03407397, 0.03104085, 0.02856767,
                                       0.0270981, rep(0.0270981, sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "Witch",
                             sim.year = 1:sim.years, sim.month = 0,
                             value = c(0.1990903, 0.1843498, 0.1814061,
                                       0.1793937, 0.1632221, 0.1536373,
                                       0.1575426, 0.1444709, 0.1186857,
                                       0.1117268, 0.1175016, 0.142013,
                                       0.1209042, 0.1129969, 0.1041381,
                                       0.1049886, 0.09844272, 0.09781247,
                                       0.09392749, 0.08318492, 0.07780617,
                                       0.07195598, rep(0.07195598, sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "Turbot",
                             sim.year = 1:sim.years, sim.month = 0,
                             value = c(0.4170994, 0.4319478, 0.4309319,
                                       0.4244702, 0.4059601, 0.3862113,
                                       0.3829598, 0.3749605, 0.3666182,
                                       0.3570697, 0.1611314, 0.2041666,
                                       0.3857329, 0.3858879, 0.2911148,
                                       0.2543159, 0.2434865, 0.3001646,
                                       0.3919835, 0.3107843, 0.3326871,
                                       0.3142664, rep(0.3142664, sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "Megrim",
                             sim.year = 1:sim.years, sim.month = 0,
                             value = c(0.102302, 0.09874463, 0.09219322,
                                       0.09127664, 0.0837483, 0.07905358,
                                       0.07958034, 0.07367957, 0.05504326,
                                       0.0490376, 0.04582739, 0.05328472,
                                       0.03044068, 0.0246922, 0.02456163,
                                       0.02498831, 0.0222337, 0.0243516,
                                       0.02411717, 0.02170881, 0.02034179,
                                       0.02004289, rep(0.02004289, sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "Halibut",
                             sim.year = 1:sim.years, sim.month = 0,
                             value = c(0.0404881, 0.03879643, 0.03654331,
                                       0.03615355, 0.03319028, 0.03136015,
                                       0.03168702, 0.02933456, 0.0223896,
                                       0.02017043, 0.01937153, 0.02262608,
                                       0.01451604, 0.01237304, 0.01195178,
                                       0.01212959, 0.01094094, 0.01161195,
                                       0.01142013, 0.01025139, 0.009618409,
                                       0.009248598,
                                       rep(0.009248598, sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "CatfishWolfFish",
                             sim.year = 1:sim.years, sim.month = 0,
                             value = c(0.3851377, 0.3718998, 0.3473739,
                                       0.3439562, 0.3155918, 0.2978738,
                                       0.2997998, 0.2776193, 0.2077143,
                                       0.1852133, 0.1733026, 0.201439,
                                       0.1158758, 0.09430142, 0.09401962,
                                       0.09605231, 0.08566303, 0.09320573,
                                       0.0924009, 0.08305062, 0.07799834,
                                       0.07688241, rep(0.07688241, sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "LargeDemersalFish",
                             sim.year = 1:sim.years, sim.month = 0,
                             value = c(0.348012, 0.3400446, 0.337703,
                                       0.3249301, 0.2887357, 0.2674895,
                                       0.2708851, 0.2450389, 0.2237365,
                                       0.2177837, 0.2340483, 0.2815065,
                                       0.263952, 0.2546198, 0.2381638,
                                       0.2389792, 0.2293358, 0.2120285,
                                       0.2123374, 0.1891667, 0.1778136,
                                       0.1737001, rep(0.1737001, sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "SmallDemersalFish",
                             sim.year = 1:sim.years, sim.month = 0,
                             value = c(0.4700815, 0.5063573, 0.5062131,
                                       0.4858948, 0.4349695, 0.380592,
                                       0.3726294, 0.3485671, 0.3233558,
                                       0.297281, 0.2976563, 0.3138127,
                                       0.2928325, 0.2824453, 0.2739395,
                                       0.2639406, 0.2497237, 0.2094083,
                                       0.2081134, 0.191627, 0.1734383,
                                       0.1651427, rep(0.1651427, sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "MiscellaneousFilterfeedingPelagicFish",
                             sim.year = 1:sim.years, sim.month = 0,
                             value = c(0.3549459, 0.3342656, 0.2963282,
                                       0.2685258, 0.3011666, 0.3234568,
                                       0.3425196, 0.352163, 0.3334207,
                                       0.2983063, 0.3174404, 0.2962102,
                                       0.2867033, 0.2789461, 0.2031795,
                                       0.1789345, 0.1184882, 0.1477315,
                                       0.1603326, 0.1720521, 0.164083,
                                       0.10778, rep(0.10778, sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "SquidAndCuttlefish",
                             sim.year = 1:sim.years, sim.month = 0,
                             value = c(0.03590212, 0.04094212, 0.0412953,
                                       0.03939718, 0.03449168, 0.02887982,
                                       0.02779761, 0.02597624, 0.02537522,
                                       0.02304291, 0.02255191, 0.02266883,
                                       0.02219419, 0.0217187, 0.0209445,
                                       0.01903041, 0.0179652, 0.01378579,
                                       0.01364908, 0.0129926, 0.01146612,
                                       0.01049362, rep(0.01049362, sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "LargeCrabs",
                             sim.year = 1:sim.years, sim.month = 0,
                             value = c(0.01195096, 0.01227413, 0.01230027,
                                       0.01208467, 0.01137215, 0.010674,
                                       0.01061278, 0.01025834, 0.009901656,
                                       0.009594003, 0.01018107, 0.01289855,
                                       0.01496126, 0.01504341, 0.01494394,
                                       0.02018144, 0.02036057, 0.0192229,
                                       0.0204263, 0.02228081, 0.02035647,
                                       0.02072941, rep(0.02072941, sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "Nephrops",
                             sim.year = 1:sim.years, sim.month = 0,
                             value = c(0.01944463, 0.01617081, 0.01842357,
                                       0.01826596, 0.01599058, 0.0147218,
                                       0.01572093, 0.01389692, 0.01384746,
                                       0.0144292, 0.0178538, 0.02300806,
                                       0.02654359, 0.02655416, 0.02391397,
                                       0.02421059, 0.02354115, 0.02212002,
                                       0.02084785, 0.01824425, 0.01701487,
                                       0.01542969, rep(0.01542969, sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "EpifaunalMacrobenthosMobileGrazers",
                             sim.year = 1:sim.years, sim.month = 0,
                             value = c(8.52E-05, 8.56E-05, 8.59E-05, 8.55E-05,
                                       8.43E-05, 8.32E-05, 8.31E-05, 8.25E-05,
                                       8.23E-05, 8.19E-05, 7.33E-05, 9.47E-05,
                                       0.000132344, 0.000145499, 0.000194424,
                                       0.000111937, 0.000129416, 0.000111505,
                                       0.000128911, 0.00014446, 0.000147138,
                                       0.000124652,
                                       rep(0.000124652, sim.years)))
Rsim.model <- adjust.fishing(Rsim.model, parameter = "ForcedFRate",
                             group = "Shrimp",
                             sim.year = 1:sim.years, sim.month = 0,
                             value = c(1.515663, 1.589607, 1.691714, 1.410376,
                                       1.523611, 1.488768, 1.44996, 1.418041,
                                       1.347741, 1.278153, 1.31905, 1.304413,
                                       1.406402, 1.377853, 1.322389, 1.286769,
                                       1.34564, 1.31774, 1.386026, 1.243971,
                                       0.9122694, 1.222883,
                                       rep(1.222883, sim.years)))
# Forcing
Rsim.model <- adjust.forcing(Rsim.model, parameter = "ForcedSearch",
                             group = "StarryRayOthers", sim.year = 1:sim.years,
                             sim.month = 0,
                             value = c(1, 0.9526911, 0.893743, 0.8233139,
                                       0.7245176, 0.580942, 0.4669409, 0.4233466,
                                       0.4130158, 0.4794402, 0.5109328, 0.4473176,
                                       0.3736314, 0.2928742, 0.2658394, 0.2967508,
                                       0.3685113, 0.4169871, 0.3968832, 0.3793588,
                                       0.3423307, 0.3766794,
                                       rep(0.4169099, sim.years)))
Rsim.model <- adjust.forcing(Rsim.model, parameter = "ForcedRecs",
                             group = "JuvCod", sim.year = 1:sim.years,
                             sim.month = 0,
                             value = c(1, 1.083742, 1.15678, 1.216492, 1.242375,
                                       1.215813, 0.9922753, 0.8756158, 0.647683,
                                       0.5025799, 0.4818873, 0.3828645, 0.3305077,
                                       0.3433338, 0.3930009, 0.4082056, 0.4080923,
                                       0.3893202, 0.3876509, 0.386555, 0.3910727,
                                       0.4358378, 0.4541524,
                                       rep(0.4542, sim.years)))
Rsim.model <- adjust.forcing(Rsim.model, parameter = "ForcedSearch",
                             group = "AduCod", sim.year = 1:sim.years,
                             sim.month = 0,
                             value = c(1, 0.9942778, 1.018283, 1.032825, 1.064811,
                                       1.057389, 0.9780535, 0.9137216, 0.791474,
                                       0.7257386, 0.6809118, 0.6039972, 0.5313129,
                                       0.501758, 0.523687, 0.5381916, 0.5249159,
                                       0.4985612, 0.5474494, 0.6565779, 0.7445346,
                                       0.8193123, rep(0.8890961, sim.years)))
Rsim.model <- adjust.forcing(Rsim.model, parameter = "ForcedRecs",
                             group = "JuvWhiting", sim.year = 1:sim.years,
                             sim.month = 0,
                             value = c(1, 0.9796521, 0.9491083, 0.9133191,
                                       0.8484561, 0.7270078, 0.7025896, 0.8295403,
                                       1.025536, 1.145007, 1.070517, 0.8187804,
                                       0.577073, 0.4203761, 0.3796214, 0.4251021,
                                       0.489237, 0.5334167, 0.5846202, 0.5861588,
                                       0.5172055, 0.4672309, 0.4829256,
                                       rep(0.4169099, sim.years)))
Rsim.model <- adjust.forcing(Rsim.model, parameter = "ForcedSearch",
                             group = "AduWhiting", sim.year = 1:sim.years,
                             sim.month = 0,
                             value = c(1, 0.9526911, 0.893743, 0.8233139,
                                       0.7245176, 0.580942, 0.4669409, 0.4233466,
                                       0.4130158, 0.4794402, 0.5109328, 0.4473176,
                                       0.3736314, 0.2928742, 0.2658394, 0.2967508,
                                       0.3685113, 0.4169871, 0.3968832, 0.3793588,
                                       0.3423307, 0.3766794,
                                       rep(0.4169099, sim.years)))
Rsim.model <- adjust.forcing(Rsim.model, parameter = "ForcedRecs",
                             group = "JuvHaddock",
                             sim.year = 1:sim.years, sim.month = 0,
                             value = c(1, 0.9374743, 0.8670852, 0.7802932,
                                       0.6977834, 0.5030927, 0.6657735, 1.146469,
                                       1.256752, 1.126605, 0.5695813, 0.1449049,
                                       0.1891164, 0.3527553, 0.4036039, 0.3847535,
                                       0.2974354, 0.2807451, 0.2732898, 0.2385394,
                                       0.1329279, 0.1334059, 0.1385682,
                                       rep(0.1385682, sim.years)))
Rsim.model <- adjust.forcing(Rsim.model, parameter = "ForcedRecs",
                             group = "JuvSaithe", sim.year = 1:sim.years,
                             sim.month = 0,
                             value = c(1, 1.074535, 1.145892, 1.212877, 1.291401,
                                       1.270914, 1.111098, 1.015819, 1.040898,
                                       1.258596, 1.406945, 1.405092, 1.301965,
                                       1.153255, 1.021811, 0.9505886, 0.7619206,
                                       0.6835842, 0.6886239, 0.6976185, 0.7578521,
                                       0.6492221, 0.5876831,
                                       rep(0.5877, sim.years)))
Rsim.model <- adjust.forcing(Rsim.model, parameter = "ForcedRecs",
                             group = "JuvHerring", sim.year = 1:sim.years,
                             sim.month = 0,
                             value = c(1, 1.015599, 1.030265, 1.043587, 0.9723554,
                                       0.8849775, 0.8712575, 0.9702657, 1.169757,
                                       1.420153, 1.343792, 1.107857, 0.8118114,
                                       0.6022916, 0.5845616, 0.6089359, 0.6520786,
                                       0.7193114, 0.7708102, 0.8015057, 0.8101279,
                                       0.9013488, rep(0.8884637, sim.years)))
Rsim.model <- adjust.forcing(Rsim.model, parameter = "ForcedSearch",
                             group = "Mackerel", sim.year = 1:sim.years,
                             sim.month = 0,
                             value = c(1, 0.8751226, 0.9242368, 0.9854343,
                                       1.058551, 1.161117, 1.310171, 1.428522,
                                       1.47378, 1.484505, 1.415546, 1.382852,
                                       1.448895, 1.525393, 1.609231, 1.637298,
                                       1.605207, 1.530708, 1.480382, 1.501253,
                                       1.519446, 1.537101,
                                       rep(1.541095, sim.years)))
Rsim.model$forcing$ForcedPrey[,67] <- c(1.030349, 1.030342, 1.030321, 1.030285,
                                        1.030235, 1.030173, 1.030097, 1.030008,
                                        1.029907, 1.029794, 1.029669, 1.029533,
                                        1.029385, 1.029227, 1.029058, 1.028879,
                                        1.02869,  1.028492, 1.028285, 1.028068,
                                        1.027843, 1.02761,  1.027369, 1.02712,
                                        1.026864, 1.026601, 1.026332, 1.026056,
                                        1.025774, 1.025486, 1.025194, 1.024896,
                                        1.024593, 1.024286, 1.023975, 1.023661,
                                        1.023342, 1.023021, 1.022697, 1.022371,
                                        1.022043, 1.021712, 1.021381, 1.021048,
                                        1.020714, 1.02038,  1.020045, 1.019711,
                                        1.019377, 1.019044, 1.018712, 1.018382,
                                        1.018053, 1.017727, 1.017402, 1.017081,
                                        1.016762, 1.016447, 1.016136, 1.015828,
                                        1.015525, 1.015227, 1.014933, 1.014645,
                                        1.014363, 1.014086, 1.013816, 1.013552,
                                        1.013295, 1.013046, 1.012804, 1.012569,
                                        1.012343, 1.012126, 1.011917, 1.011718,
                                        1.011528, 1.011348, 1.011178, 1.011018,
                                        1.010869, 1.010731, 1.010605, 1.010491,
                                        1.010388, 1.010298, 1.010221, 1.010156,
                                        1.010105, 1.010068, 1.010044, 1.010035,
                                        1.010041, 1.010061, 1.010096, 1.010145,
                                        1.010208, 1.010283, 1.010372, 1.010472,
                                        1.010585, 1.010708, 1.010843, 1.010988,
                                        1.011143, 1.011307, 1.01148,  1.011662,
                                        1.011852, 1.012049, 1.012254, 1.012466,
                                        1.012684, 1.012907, 1.013136, 1.01337,
                                        1.013608, 1.01385,  1.014096, 1.014344,
                                        1.014596, 1.014849, 1.015105, 1.015361,
                                        1.015618, 1.015876, 1.016133, 1.01639,
                                        1.016645, 1.0169,   1.017152, 1.017402,
                                        1.017648, 1.017892, 1.018132, 1.018367,
                                        1.018598, 1.018823, 1.019043, 1.019257,
                                        1.019464, 1.019665, 1.019857, 1.020042,
                                        1.020219, 1.020386, 1.020545, 1.020693,
                                        1.020831, 1.020959, 1.021076, 1.021181,
                                        1.021273, 1.021354, 1.021421, 1.021475,
                                        1.021515, 1.021541, 1.021552, 1.021548,
                                        1.021528, 1.021492, 1.021439, 1.021369,
                                        1.021282, 1.021176, 1.021052, 1.020909,
                                        1.020747, 1.020565, 1.020363, 1.02014,
                                        1.019896, 1.01963,  1.019342, 1.019031,
                                        1.018698, 1.018341, 1.017959, 1.017554,
                                        1.017124, 1.016668, 1.016187, 1.015679,
                                        1.015145, 1.014585, 1.013999, 1.013389,
                                        1.012755, 1.012098, 1.011418, 1.010717,
                                        1.009994, 1.009251, 1.008489, 1.007708,
                                        1.006908, 1.006091, 1.005256, 1.004406,
                                        1.003541, 1.00266,  1.001765, 1.000858,
                                        0.9999373, 0.9990049, 0.9980614,
                                        0.9971073, 0.9961433, 0.9951702,
                                        0.9941888, 0.9931997, 0.9922036,
                                        0.9912012, 0.9901931, 0.9891801,
                                        0.9881631, 0.9871425, 0.9861191,
                                        0.9850937, 0.9840668, 0.9830394,
                                        0.9820119, 0.9809852, 0.9799599,
                                        0.9789368, 0.9779165, 0.9768998,
                                        0.9758873, 0.9748798, 0.973878,
                                        0.9728824, 0.971894, 0.9709134,
                                        0.9699413, 0.9689782, 0.9680251,
                                        0.9670826, 0.9661514, 0.9652321,
                                        0.9643256, 0.9634326, 0.9625536,
                                        0.9616893, 0.9608406, 0.9600081,
                                        0.9591926, 0.9583946, 0.957615,
                                        0.9568544, 0.9561136, 0.9553931,
                                        0.9546939, 0.9540164, 0.9533615,
                                        0.9527298, 0.9521222, 0.9515392,
                                        0.9509814, 0.9504498, 0.949945,
                                        0.9494677, 0.9490184, 0.9485981,
                                        0.9482075, 0.9478471, 0.9475176,
                                        0.9472198, 0.9469544, 0.9467222,
                                        0.9465237, 0.9463598, 0.946231,
                                        0.9461382, 0.9460819,
                                        rep(0.946063, (sim.years*12)-(23*12)+1))
# Setting integration flags
Rsim.model$params$NoIntegrate <-
  ifelse(Rsim.model$params$MzeroMort*Rsim.model$params$B_BaseRef > 24,
         0, Rsim.model$params$spnum)
#Running Rsim
Rsim.run <- rsim.run(Rsim.model, method = 'AB', years = 1:sim.years)

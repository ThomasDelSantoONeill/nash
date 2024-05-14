set.seed(2023)
Fs <- c(0.2, 0.3)
inimax <- 1000
Fini <- array(dim = c(inimax, length(Fs)))
for (i in 1:ncol(Fini)) {
  Fini[, i] <-
    truncnorm::rtruncnorm(inimax,
                          a = 0,
                          mean = mean(Fs[i]),
                          sd = 0.1)
}
par(mfrow = c(1, 2))
for (i in 1:ncol(Fini)) {
  hist(Fini[, i])
  abline(v = Fs[i], lwd = 2)
}

load("F&F_Scripts&Data/BalticSeaModel.RData")
spp = c("AdCod", "AdHerring", "AdSprat", "AdFlounder")
Fs <- tail(Rsim.model$fishing$ForcedFRate[, spp], n = 250)
inimax <- 1000
Fini <- array(dim = c(inimax, ncol(Fs)))
for (i in 1:ncol(Fs)) {
  Fini[, i] <-
    truncnorm::rtruncnorm(inimax,
                          a = 0,
                          mean = mean(Fs[, i]),
                          sd = 0.1)
}
par(mfrow = c(2, 2))
for (i in 1:ncol(Fini)) {
  hist(Fini[, i])
  abline(v = colMeans(Fs)[i], lwd = 2)
}
saveRDS(Fini, "BS_Performance.rds")
dev.off()

load("F&F_Scripts&Data/NorthSeaModel.RData")
spp = c(
  "AduCod",
  "AduWhiting",
  "AduHaddock",
  "AduSaithe",
  "AduHerring",
  "NorwayPout",
  "Sandeels",
  "Plaice",
  "Sole"
)
Fs <- tail(Rsim.model$fishing$ForcedFRate[, spp], n = 500)
Fini <- array(dim = c(inimax, ncol(Fs)))
for (i in 1:ncol(Fs)) {
  Fini[, i] <-
    truncnorm::rtruncnorm(inimax,
                          a = 0,
                          mean = mean(Fs[, i]),
                          sd = 0.1)
}
par(mfrow = c(3, 3))
for (i in 1:ncol(Fini)) {
  hist(Fini[, i])
  abline(v = colMeans(Fs)[i], lwd = 2)
}
saveRDS(Fini, "NS_Performance.rds")

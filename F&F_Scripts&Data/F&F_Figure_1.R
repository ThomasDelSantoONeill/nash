### Plotting routine for food web visualisation of Rpath models (Figure 1
###   in F&F manuscript).
###   To visualise the North Sea model uncomment the following line:
###   load("F&F_Scripts&Data/NorthSeaModel.RData").
###   Figure 1 in EAP manuscript is different as it was further processed to
###   improve readability.
load("F&F_Scripts&Data/BalticSeaModel.RData")
# load("F&F_Scripts&Data/NorthSeaModel.RData")
## Libraries
library(data.table)
library(igraph)
library(ggraph)
library(tidygraph)
library(extrafont)
loadfonts(device="win")
## Script
# NODES
nodes <- data.table(spnum  = 1:Rpath.model$NUM_GROUPS,
                    spname = Rpath.model$Group,
                    sptype = Rpath.model$type)
nodes[sptype == 0, type.label := factor('Consumers', levels = c('Consumers',
                                                                'Producers',
                                                                'Detritus',
                                                                'Fleets'))]
nodes[sptype == 1, type.label := 'Producers']
nodes[sptype == 2, type.label := 'Detritus']
nodes[sptype == 3, type.label := 'Fleets']
nodes[, biomass := Rpath.model$Biomass]
nodes[, weight := abs(biomass - mean(biomass)) / sd(biomass)]
# LINKS
predprey <- data.table(from = row(Rpath.model$DC)[Rpath.model$DC > 0],
                       to   = col(Rpath.model$DC)[Rpath.model$DC > 0],
                       weight = Rpath.model$DC[Rpath.model$DC > 0])
# FISHERY
# Combine landings and discards
catch <- Rpath.model$Landings + Rpath.model$Discards
fishery <- data.table(from = row(catch)[catch > 0],
                      to   = col(catch)[catch > 0],
                      weight = catch[catch > 0])
# Need to fix these weights as they are not equal between predprey and fleet
# Transform fishery column number to their assigned number in spnum
fleet.num <- nodes[sptype == 3, spnum]
for(i in 1:ncol(catch)) fishery[to == i, to := fleet.num[i]][]
links <- rbindlist(list(predprey, fishery))
# NETWORK AESTHETIC PROPERTIES
net <- graph_from_data_frame(d=links, vertices=nodes, directed=T)
graph <- as_tbl_graph(net)
# Create layout
lay <- create_layout(net, 'mds')
# Set y value to Trophic Level
lay$y <- Rpath.model$TL
# Recenter on x-axis
# Trophic Level 1
ppnum <- which(lay$y < 2)
n <- length(ppnum)
lay$x[ppnum] <- order(lay$x[ppnum]) * 1/n - 0.5/n
# Trophic Levels 2+
TL.groups <- round((max(lay$y) - 2) / 0.5)
for(iTL in 1:TL.groups){
  if(iTL == 1) TL <- 2
  TLnum <- which(lay$y >= TL & lay$y < (TL + 0.5))
  n <- length(TLnum)
  lay$x[TLnum] <- order(lay$x[TLnum]) * 1/n - 0.5/n
  TL <- TL + 0.5
}
# Defining a theme
foodweb_theme <- theme(
  axis.line.x = element_blank(),
  axis.text.x = element_blank(),
  axis.ticks.x = element_blank(),
  axis.title.x = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.grid.major.x = element_blank(),
  axis.line.y = element_line(),
  axis.ticks.y = element_line(),
  axis.title.y = element_text(angle = 360,
                              margin = margin(
                                t = 0,
                                r = 10,
                                b = 0,
                                l = 0
                              )),
  axis.text.y.left = element_text(),
  panel.grid.major.y = element_line(linetype = 2,
                                    color = rgb(0, 0, 0, alpha = 0.5)),
  panel.grid.minor.y = element_blank(),
  panel.background = element_blank(),
  legend.key = element_blank(),
  legend.background = element_blank(),
  aspect.ratio = 1,
  plot.margin = unit(c(0, 3, 0, 0), "cm"),
  text = element_text(family = "Consolas")
)
# Defining node colours (c(consumers, detritus, fleets, producers))
node_colours <- c("#faebd7", "#be9b7b", "#aaaaaa", "#009688")
# Nice labels
nicelab <- nodes$spname
# Plotting
ggraph(lay) +
  geom_edge_link0(
    aes(edge_colour = links$weight),
    edge_width = links$weight * 5,
    lineend = "round"
  ) +
  scale_edge_color_gradient(
    name = "Link Strength",
    low = "#6495ED85",
    high = "#C1403D85",
    limits = c(0, 1.01),
    guide = guide_edge_colorbar(
      title = "Link Strength",
      title.position = "top",
      title.hjust = 0.5,
      ticks = TRUE,
      label = TRUE,
      frame.colour = "black",
      barwidth = 7.5,
      direction = "horizontal",
      position = c(0.9, 0.1)
    )
  ) +
  geom_node_point(
    aes(fill = lay$type.label),
    shape = 21,
    size = 10,
    stroke = 1,
    color = "#000000",
    alpha = 0.85
  ) +
  geom_node_label(
    aes(label = nicelab),
    repel = TRUE,
    label.padding = 0.75,
    label.size = NA,
    fill = NA,
    size = 4,
    family = "Consolas",
    fontface = "bold"
  ) +
  labs(y = "Trophic\nLevel") +
  foodweb_theme +
  scale_fill_manual(
    values = node_colours,
    guide = guide_legend(
      direction = "vertical",
      position = c(1, 1),
      title = element_blank()
    )
  )

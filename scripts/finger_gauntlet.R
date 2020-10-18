library(ggraph)
library(igraph)
library(tidyverse)
library(tidygraph)

#d <- finger_gauntlet[,]
d1 <- data.frame(from = "Finger Gauntlet", to=c("Left", "Right"), lr=c("Left", "Right"))

d2 <- data.frame(from = rep(d1$to, each = 4), 
                 to = c("L-LSLS", "L-SLLS", "L-LSSS", "L-SSSL",
                        "R-LSLS", "R-SLLS", "R-LSSS", "R-SSSL"),
                 lr = c(rep("Left", each = 4),
                          rep("Right", each = 4)) )

d3 <- data.frame(from = rep(d2$to, each = 5), 
                 to = c( paste(rep(d2$to, each = 5), 1:5, sep = "-")),
                 lr = c(rep("Left", each = 20),
                          rep("Right", each = 20))  )
data <- rbind(d1, d2, d3)
graph <- as_tbl_graph(data)

ggraph(graph, layout = 'dendrogram', circular = FALSE) + 
        geom_edge_elbow(aes(colour = lr)) +
        geom_node_label(aes( label=name, filter=name %in% unique(d1$from), fill = NULL), angle=0 , hjust=0.5, nudge_y = 0.5) +
        geom_node_label(aes( label=name, filter=name %in% unique(d1$to), fill = NULL), angle=0 , hjust=0.5, nudge_y = 0.5) +
        geom_node_label(aes( label=name, filter=name %in% unique(d2$to), fill = NULL), angle=0 , hjust=0.5, nudge_y = 0.5) +
        geom_node_text(aes( label=name, filter=leaf) , angle=0 , hjust=0.5, nudge_y = -0.05) +
        ylim(-.4, NA)


 # specific coloring---- 
library(ggraph)
library(igraph)
library(tidyverse)

#d <- finger_gauntlet[,]
d1 <- data.frame(from = "Finger Gauntlet", to=c("Left", "Right"),
                 lr = "")

d2 <- data.frame(from = rep(d1$to, each = 4), 
                 to = c("L-LSLS", "L-SLLS", "L-LSSS", "L-SSSL",
                        "R-LSLS", "R-SLLS", "R-LSSS", "R-SSSL"),
                 lr = "")

d3 <- data.frame(from = rep(d2$to, each = 5), 
                 to = c( paste(rep(d2$to, each = 5), 1:5, sep = "-")),
                 lr = c(rep("Left", each = 20),
                        rep("Right", each = 20))  )

data <- rbind(d1, d2, d3)
data$number <- c(1:length(data$to))
graph <- as_tbl_graph(data)


ggraph(graph, layout = 'dendrogram', circular = FALSE) + 
        geom_edge_elbow() +
        geom_edge_elbow(aes(colour = "red", filter= number =="50")) +
        geom_node_label(aes( label=name, filter=name %in% unique(d1$from), fill = NULL), angle=0 , hjust=0.5, nudge_y = 0.5) +
        geom_node_label(aes( label=name, filter=name %in% unique(d1$to), fill = NULL), angle=0 , hjust=0.5, nudge_y = 0.5) +
        geom_node_label(aes( label=name, filter=name %in% unique(d2$to), fill = NULL), angle=0 , hjust=0.5, nudge_y = 0.5) +
        geom_node_text(aes( label=name, filter=leaf) , angle=0 , hjust=0.5, nudge_y = -0.05) +
        ylim(-.4, NA)

ggraph(graph, layout = 'dendrogram', circular = TRUE) + 
        geom_edge_elbow(aes(width = 0.5)) +
        geom_edge_elbow(aes(colour = "red", filter= number =="50", width = 0.5)) +
        coord_fixed()

plot <- ggraph(graph, layout = 'dendrogram', circular = TRUE) + 
        geom_edge_elbow(aes(width = 0.5)) +
        geom_edge_elbow(aes(colour = "red", filter= number %in% c("45","50"), width = 0.5)) +
        coord_fixed()

svg("image.svg",width=10,height=5)
plot
dev.off()

library(RColorBrewer)
n <- 50
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
set.seed(503)
colors <- sample(col_vector, n)
colors_5000 <- rep(colors, each = 100)

plot <- ggraph(graph, layout = 'dendrogram', circular = TRUE) + 
        geom_edge_elbow(aes(width = 0.5), color = colors_5000) +
        coord_fixed() +
        theme_graph(background = NA) +
        theme(legend.position = "none")

plot <- ggraph(graph, layout = 'dendrogram', circular = TRUE) + 
        geom_edge_elbow(aes(width = 0.5), color = colors_5000) +
        coord_fixed() +
        theme_graph(background = "black") +
        theme(legend.position = "none")
plot
svg("image.svg",width=10,height=5)
plot
dev.off()

png("mandala.png")
plot
dev.off()

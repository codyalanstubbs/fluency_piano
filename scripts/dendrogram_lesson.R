# Libraries ----
library(ggraph)
library(igraph)
library(tidyverse)
theme_set(theme_void())

# data: edge list ----
d1 <- data.frame(from="origin", to=paste("group", seq(1,7), sep=""))
d2 <- data.frame(from=rep(d1$to, each=7), to=paste("subgroup", seq(1,49), sep="_"))
d3 <- data.frame(from=rep(d2$to, each=2), to=paste("subgroup", seq(1,98), sep="_"))
edges <- rbind(d1, d2, d3)

# We can add a second data frame with information for each node! ----
name <- unique(c(as.character(edges$from), as.character(edges$to)))

# Create a graph object
mygraph <- graph_from_data_frame( edges)

library(ggraph)
ggraph(mygraph, layout = 'dendrogram', circular = FALSE) + 
        geom_edge_elbow() +
        geom_node_text(aes( label=name, filter=leaf) , angle=90 , hjust=1, nudge_y = -0.01) +
        geom_node_label(aes( label=name, filter=name %in% unique(d1$to), fontface = "bold", fill = "black"), angle=0 , hjust=0.5, nudge_y = 0.5) +
        ylim(-.4, NA)
 # ----
library(ggraph)
library(tidygraph)

set_graph_style(plot_margin = margin(1,1,1,1))
graph <- as_tbl_graph(highschool)

# Not specifying the layout - defaults to "auto"
ggraph(graph) +
        geom_edge_link(aes(colour = factor(name))) +
        geom_node_point()

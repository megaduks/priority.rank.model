# the script is used to plot various networks produced by the Priority Rank Model

source('scripts/create.network.R')

# Erdos-Renyi random network
#
distance <- distance.random
g <- generate_rank_graph(distance, use_bin_ranking = FALSE)
title <- "Random network"

# draw the graph
p1.net <- ggnet2(g, size = 'degree', color = 'royalblue', edge.size = 0.5, edge.color = 'grey50', mode = 'kamadakawai')

# compute the degree distribution in the graph
d <- as.data.frame(table(igraph::degree(g)))
names(d) <- c("degree", "count")
d$degree <- as.numeric(d$degree)

# draw the histogram of the degree distribution
p1.dd <- ggplot(data = d, aes(x=degree,y=count)) + geom_bar(stat = "identity", aes(alpha = 0.1)) + geom_smooth(stat = 'identity')

p <- grid.arrange(p1.net, p1.dd, ncol=2, top=title)
ggsave(filename = 'figures/random.png', plot = p)

#
# Watts-Strogatz small world network
#
distance <- distance.attribute
g <- generate_rank_graph(distance, use_bin_ranking = FALSE)
title <- "Small world network"

# draw the graph
p2.net <- ggnet2(g, size = 'degree', color = 'firebrick', edge.size = 0.5, edge.color = 'grey50', mode = 'kamadakawai') 

# compute the degree distribution in the graph
d <- as.data.frame(table(igraph::degree(g)))
names(d) <- c("degree", "count")
d$degree <- as.numeric(d$degree)

# draw the histogram of the degree distribution
p2.dd <- ggplot(data = d, aes(x=degree,y=count)) + geom_bar(stat = "identity", aes(alpha = 0.1)) + geom_smooth(stat = 'identity')

p <- grid.arrange(p2.net, p2.dd, ncol=2, top=title)
ggsave(filename = 'figures/small.world.png', plot = p)

#
# Albert-Barabasi preferential attachment network
#
distance <- distance.degree
g <- generate_rank_graph(distance, use_bin_ranking = FALSE)
title <- "Preferential attachment network"

# draw the graph
p3.net <- ggnet2(g, size = 'degree', color = 'darkorchid', edge.size = 0.5, edge.color = 'grey50', mode = 'kamadakawai')

# compute the degree distribution in the graph
d <- as.data.frame(table(igraph::degree(g)))
names(d) <- c("degree", "count")
d$degree <- as.numeric(d$degree)

# draw the histogram of the degree distribution
p3.dd <- ggplot(data = d, aes(x=degree,y=count)) + geom_bar(stat = "identity", aes(alpha = 0.1)) + geom_smooth(stat = 'identity')

p <- grid.arrange(p3.net, p3.dd, ncol=2, top=title)
ggsave(filename = 'figures/preferential.attachment.png', plot = p)

#
# Cosine distance network
#
distance <- distance.cosine
g <- generate_rank_graph(distance, use_bin_ranking = FALSE)
title <- "Cosine distance network"

# draw the graph
p4.net <- ggnet2(g, size = 'degree', color = 'darkseagreen', edge.size = 0.5, edge.color = 'grey50', mode = 'kamadakawai')

# compute the degree distribution in the graph
d <- as.data.frame(table(igraph::degree(g)))
names(d) <- c("degree", "count")
d$degree <- as.numeric(d$degree)

# draw the histogram of the degree distribution
p4.dd <- ggplot(data = d, aes(x=degree,y=count)) + geom_bar(stat = "identity", aes(alpha = 0.1)) + geom_smooth(stat = 'identity')

p <- grid.arrange(p4.net, p4.dd, ncol=2, top=title)
ggsave(filename = 'figures/cosine.png', plot = p)

#
# Disassortative  network
#
distance <- distance.dissasortative
g <- generate_rank_graph(distance, use_bin_ranking = FALSE)
title <- "Disassortative network"

# draw the graph
p5.net <- ggnet2(g, size = 'degree', color = 'slateblue', edge.size = 0.5, edge.color = 'grey50', mode = 'fruchtermanreingold')

# compute the degree distribution in the graph
d <- as.data.frame(table(igraph::degree(g)))
names(d) <- c("degree", "count")
d$degree <- as.numeric(d$degree)

# draw the histogram of the degree distribution
p5.dd <- ggplot(data = d, aes(x=degree,y=count)) + geom_bar(stat = "identity", aes(alpha = 0.1)) + geom_smooth(stat = 'identity')

p <- grid.arrange(p5.net, p5.dd, ncol=2, top=title)
ggsave(filename = 'figures/disassortative.png', plot = p)

#
# Hierarchical  network
#
distance <- distance.hierarchical
g <- generate_rank_graph(distance, use_bin_ranking = FALSE)
title <- "Hierarchical network"

# draw the graph
p6.net <- ggnet2(g, size = 'degree', color = g$vertex.class, edge.size = 0.5, edge.color = 'grey50', mode = 'fruchtermanreingold')

# compute the degree distribution in the graph
d <- as.data.frame(table(igraph::degree(g)))
names(d) <- c("degree", "count")
d$degree <- as.numeric(d$degree)

# draw the histogram of the degree distribution
p6.dd <- ggplot(data = d, aes(x=degree,y=count)) + geom_bar(stat = "identity", aes(alpha = 0.1)) + geom_smooth(stat = 'identity')

p <- grid.arrange(p6.net, p6.dd, ncol=2, top=title)
ggsave(filename = 'figures/hierarchical.png', plot = p)


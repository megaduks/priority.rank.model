# the script is used to plot various networks produced by the Priority Rank Model

source('scripts/create.network.R')

#
# Erdos-Renyi random network
#
similarity <- similarity.random
g <- generate_rank_graph(similarity)
title <- "Random network"

# draw the graph
p1 <- ggnet2(g, size = 'degree', color = 'tomato', edge.size = 0.5, edge.color = 'grey50', mode = 'kamadakawai')

# compute the degree distribution in the graph
d <- as.data.frame(table(igraph::degree(g)))
names(d) <- c("degree", "count")

# draw the histogram of the degree distribution
p2 <- ggplot(data = d, aes(x=degree,y=count)) + geom_bar(stat = "identity")

p <- grid.arrange(p1, p2, ncol=2, top=title)
ggsave(filename = 'figures/random.png', plot = p)

#
# Watts-Strogatz small world network
#
similarity <- similarity.attribute
g <- generate_rank_graph(similarity)
title <- "Small world network"

# draw the graph
p1 <- ggnet2(g, size = 'degree', color = 'blueviolet', edge.size = 0.5, edge.color = 'grey50', mode = 'kamadakawai')

# compute the degree distribution in the graph
d <- as.data.frame(table(igraph::degree(g)))
names(d) <- c("degree", "count")

# draw the histogram of the degree distribution
p2 <- ggplot(data = d, aes(x=degree,y=count)) + geom_bar(stat = "identity")

p <- grid.arrange(p1, p2, ncol=2, top=title)
ggsave(filename = 'figures/small.world.png', plot = p)

#
# Albert-Barabasi preferential attachment network
#
similarity <- similarity.degree
g <- generate_rank_graph(similarity)
title <- "Preferential attachment network"

# draw the graph
p1 <- ggnet2(g, size = 'degree', color = 'steelblue', edge.size = 0.5, edge.color = 'grey50', mode = 'kamadakawai')

# compute the degree distribution in the graph
d <- as.data.frame(table(igraph::degree(g)))
names(d) <- c("degree", "count")

# draw the histogram of the degree distribution
p2 <- ggplot(data = d, aes(x=degree,y=count)) + geom_bar(stat = "identity")

p <- grid.arrange(p1, p2, ncol=2, top=title)
ggsave(filename = 'figures/preferential.attachment.png', plot = p)

#
# Cosine similarity network
#
similarity <- similarity.cosine
g <- generate_rank_graph(similarity)
title <- "Cosine similarity network"

# draw the graph
p1 <- ggnet2(g, size = 'degree', color = 'limegreen', edge.size = 0.5, edge.color = 'grey50', mode = 'kamadakawai')

# compute the degree distribution in the graph
d <- as.data.frame(table(igraph::degree(g)))
names(d) <- c("degree", "count")

# draw the histogram of the degree distribution
p2 <- ggplot(data = d, aes(x=degree,y=count)) + geom_bar(stat = "identity")

p <- grid.arrange(p1, p2, ncol=2, top=title)
ggsave(filename = 'figures/cosine.png', plot = p)

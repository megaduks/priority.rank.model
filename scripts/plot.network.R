# the script is used to plot various networks produced by the Priority Rank Model

source('scripts/create.network.R')

#
# Erdos-Renyi random network
#
similarity <- similarity.random
g <- generate_rank_graph(similarity, use_bin_ranking = TRUE)
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
similarity <- similarity.attribute
g <- generate_rank_graph(similarity, use_bin_ranking = FALSE)
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
similarity <- similarity.degree
g <- generate_rank_graph(similarity, use_bin_ranking = TRUE)
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
# Cosine similarity network
#
similarity <- similarity.cosine
g <- generate_rank_graph(similarity, use_bin_ranking = TRUE)
title <- "Cosine similarity network"

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


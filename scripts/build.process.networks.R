# the script is used to create artificial simple process networks for validation of the method

source('scripts/create.network.R')

# Simple process networks
#

distance <- distance.simple.process
g <- generate_graph(distance)
igraph::write_graph(g, file = 'network1.csv', format = 'pajek')
df <- data.frame(cbind(V(g), vertex.x, vertex.y, vertex.z))
names(df) <- c('vertex-id', 'vertex-x', 'vertex-y', 'vertex-z')
write.csv(df, file = 'vertices1.csv', quote = FALSE, row.names = FALSE)

distance <- distance.simple.process.with.random
g <- generate_graph(distance)
igraph::write_graph(g, file = 'network2.csv', format = 'pajek')
df <- data.frame(cbind(V(g), vertex.x, vertex.y, vertex.z))
names(df) <- c('vertex-id', 'vertex-x', 'vertex-y', 'vertex-z')
write.csv(df, file = 'vertices2.csv', quote = FALSE, row.names = FALSE)

distance <- distance.simple.process.with.sigmoid
g <- generate_graph(distance)
igraph::write_graph(g, file = 'network3.csv', format = 'pajek')
df <- data.frame(cbind(V(g), vertex.x, vertex.y, vertex.z))
names(df) <- c('vertex-id', 'vertex-x', 'vertex-y', 'vertex-z')
write.csv(df, file = 'vertices3.csv', quote = FALSE, row.names = FALSE)

distance <- distance.no.simple.process
g <- generate_graph(distance)
igraph::write_graph(g, file = 'network4.csv', format = 'pajek')
df <- data.frame(cbind(V(g), vertex.x, vertex.y, vertex.z))
names(df) <- c('vertex-id', 'vertex-x', 'vertex-y', 'vertex-z')
write.csv(df, file = 'vertices4.csv', quote = FALSE, row.names = FALSE)



distance <- distance.simple.process
g <- generate_graph(distance)
title <- "Simple process 1"
p.net <- ggnet2(g, size = 'degree', color = vertex.z, edge.size = 0.5, edge.color = 'grey80', mode = g$vertex.coords, palette = 'Set1')
d <- as.data.frame(table(igraph::degree(g)))
names(d) <- c("degree", "count")
p.dd <- ggplot(data = d, aes(x=degree,y=count)) + geom_bar(stat = "identity", aes(alpha = 0.1)) + geom_smooth(stat = 'identity')
p <- grid.arrange(p.net, p.dd, ncol=2, top=title)
ggsave(filename = 'figures/net1.png', plot = p)

distance <- distance.simple.process.with.random
g <- generate_graph(distance)
title <- "Simple process 2"
p.net <- ggnet2(g, size = 'degree', color = vertex.z, edge.size = 0.5, edge.color = 'grey80', mode = g$vertex.coords, palette = 'Set1')
d <- as.data.frame(table(igraph::degree(g)))
names(d) <- c("degree", "count")
p.dd <- ggplot(data = d, aes(x=degree,y=count)) + geom_bar(stat = "identity", aes(alpha = 0.1)) + geom_smooth(stat = 'identity')
p <- grid.arrange(p.net, p.dd, ncol=2, top=title)
ggsave(filename = 'figures/net2.png', plot = p)

distance <- distance.simple.process.with.sigmoid
g <- generate_graph(distance)
title <- "Simple process 3"
p.net <- ggnet2(g, size = 'degree', color = vertex.z, edge.size = 0.5, edge.color = 'grey80', mode = g$vertex.coords, palette = 'Set1')
d <- as.data.frame(table(igraph::degree(g)))
names(d) <- c("degree", "count")
p.dd <- ggplot(data = d, aes(x=degree,y=count)) + geom_bar(stat = "identity", aes(alpha = 0.1)) + geom_smooth(stat = 'identity')
p <- grid.arrange(p.net, p.dd, ncol=2, top=title)
ggsave(filename = 'figures/net3.png', plot = p)

distance <- distance.no.simple.process
g <- generate_graph(distance)
title <- "Simple process 4"
p.net <- ggnet2(g, size = 'degree', color = vertex.z, edge.size = 0.5, edge.color = 'grey80', mode = g$vertex.coords, palette = 'Set1')
d <- as.data.frame(table(igraph::degree(g)))
names(d) <- c("degree", "count")
p.dd <- ggplot(data = d, aes(x=degree,y=count)) + geom_bar(stat = "identity", aes(alpha = 0.1)) + geom_smooth(stat = 'identity')
p <- grid.arrange(p.net, p.dd, ncol=2, top=title)
ggsave(filename = 'figures/net4.png', plot = p)

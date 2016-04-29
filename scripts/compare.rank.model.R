library(reshape2)

source('scripts/create.network.R')

# Function to compute the averages and standard deviations of popular
# centrality measures. The function returns a data frame with the results
#
# graphs : a list of igraph objects representing graphs

compute.centrality.measures <- function(graphs) {
  
  # declare an empty data frame to store the results
  results <- data.frame(graph.type = character(), measure = character(), average = numeric(), std.dev = numeric())
  
  for (i in c('degree','closeness','betweenness','clustering')) {
    measures <- switch(i,
                       degree = lapply(graphs, igraph::degree, mode = 'all', normalized = TRUE),
                       closeness = lapply(graphs, igraph::closeness, mode = 'all', normalized = TRUE),
                       betweenness = lapply(graphs, igraph::betweenness, directed = FALSE, normalized = TRUE),
                       clustering = lapply(graphs, igraph::transitivity, type = 'local'))
    
    averages <- unlist(lapply(measures, mean, na.rm = TRUE))
    deviations <- unlist(lapply(measures, sd, na.rm = TRUE))
    results <- rbind(results, data.frame(graph.type, measure = i, average = averages, std.dev = deviations))
  }
  results
}

# number of test graphs
num.of.graphs <- 100

# size of each graph
num.of.vertices <- 100

# generate artificial random graphs using igraph package
g <- lapply(1:num.of.graphs, sample_gnp, n = num.of.vertices, p = 0.02)
graph.type <- 'random'

data <- compute.centrality.measures(g)

# generate graphs using Rank Model
sim <- similarity.random
g <- lapply(1:num.of.graphs, generate_rank_graph)
graph.type <- 'rank.random'

data <- rbind(data, compute.centrality.measures(g))

# generate artificial small world graphs using igraph package
g <- lapply(1:num.of.graphs, sample_smallworld, dim = 1, size = num.of.vertices, nei = 4, p = 0.01)
graph.type <- 'small.world'

data <- rbind(data, compute.centrality.measures(g))

# generate graphs using Rank Model
sim <- similarity.attribute
g <- lapply(1:num.of.graphs, generate_rank_graph)
graph.type <- 'rank.small.world'

data <- rbind(data, compute.centrality.measures(g))

# generate artificial preferential attachment graphs using igraph package
g <- lapply(1:num.of.graphs, sample_pa, n = num.of.vertices, m = 2, out.dist = NULL, out.seq = NULL, power = 2)
graph.type <- 'preferential.attachment'

data <- rbind(data, compute.centrality.measures(g))

# generate graphs using Rank Model
sim <- similarity.degree
g <- lapply(1:num.of.graphs, generate_rank_graph)
graph.type <- 'rank.preferential.attachment'

data <- rbind(data, compute.centrality.measures(g))

# save the results
write.csv(x = data, file = 'results/rank.model.comparison.csv')

# plot figures with comparisons
d <- data[data$graph.type %in% c('random','rank.random'),]
plot <- ggplot(d, aes(average, std.dev, colour = measure)) + geom_point(aes(shape = measure)) + facet_grid(. ~ graph.type)
ggsave(filename = 'figures/comparison.random.png', plot = plot)

d <- data[data$graph.type %in% c('small.world','rank.small.world'),]
plot <- ggplot(d, aes(average, std.dev, colour = measure)) + geom_point(aes(shape = measure)) + facet_grid(. ~ graph.type)
ggsave(filename = 'figures/comparison.small.world.png', plot = plot)

d <- data[data$graph.type %in% c('preferential.attachment','rank.preferential.attachment'),]
plot <- ggplot(d, aes(average, std.dev, colour = measure)) + geom_point(aes(shape = measure)) + facet_grid(. ~ graph.type)
ggsave(filename = 'figures/comparison.preferential.attachment.png', plot = plot)

library(igraph)
library(dplyr)
library(ggplot2)
library(GGally)
library(intergraph)
library(gridExtra)
library(lsa)

# simple functions which compute the similarity
# between each pair of nodes. The main aim of each 
# function is to establish the neighbourhood of
# each node

similarity.random <- function(x, y, graph) {
  
  # the first simple example returns a random 
  # similarity between any pair of nodes
  if (x == y)
    result <- 0
  else
    result <- abs(rnorm(1))
  
  result
}

similarity.degree <- function(x, y, graph) {
  
  # the function defines the similarity between vertices
  # only by the degree of the target node
  if (x == y)
    result <- 0
  else
    result <- runif(1, 0, log(igraph::degree(graph, y) + 2))
  
  result
}

similarity.attribute <- function(x, y, graph) {
  
  # the function defines the similarity between vertices 
  # as the distance between values of any vertex attribute
  #
  # a small constant prevents from infinite similarities
  if (x == y)
    result <- 0
  else
    result <- 1 / (abs(vertex.df[x,2] - vertex.df[y,2]) + 0.001)
  
  result
}

similarity.cosine <- function(x, y, graph) {
  
  # the function defines the similarity between vertices
  # as the cosine similarity between values of a vertex attribute
  # which contains a vector of numbers
  if (x == y)
    result <- 0
  else
    result <- cosine(vertex.vectors[[x]], vertex.vectors[[y]])
  
  result
}

similarity.dissasortative <- function(x, y, graph) {
  
  # the function defines the similarity between vertices
  # as the reciprocal of the distance between degrees
  
  if (x == y)
    result <- 0
  else
    # result <- (abs(vertex.degrees[x] - vertex.degrees[y]) / max(vertex.degrees[x], vertex.degrees[y]))
    result <- (max(vertex.degrees) - (vertex.degrees[x] / vertex.degrees[y]))
  
  result
}

similarity.dissasortative <- function(x, y, graph) {
  
  # the function defines the similarity between vertices
  # as the reciprocal of the distance between degrees
  
  if (x == y)
    result <- 0
  else
    # result <- (abs(vertex.degrees[x] - vertex.degrees[y]) / max(vertex.degrees[x], vertex.degrees[y]))
    result <- (max(vertex.degrees) - (vertex.degrees[x] / vertex.degrees[y]))
  
  result
}

# define the length of the ranking
n <- 10

# define the number of vertices in the graph
num.vertices <- 100

# define if equal number of outgoing edges should be used
use.equal.outdegree <- FALSE

# set the number of edges each new vertex creates
k <- 2

# create a given number of vertices
vertex.ids <- 1:num.vertices

# create a random attribute for each vertex
vertex.values <- runif(num.vertices)

# compute the probabilities of selecting each position of the ranking
probabilities <- 1 / 1:n / sum(1/1:n)

# simulate vertices using a data frame for easy import into igraph
vertex.df <- data.frame(vertex.ids, vertex.values)

# create a random vector for cosine similarity for each vertex
vertex.vectors <- numeric()

for (i in vertex.ids)
  vertex.vectors[i] <- list(c(1,sample(x = 0:1, size = 9, replace = TRUE)))

# create a vector of expected vertex degrees
vertex.degrees <- numeric()

for (i in vertex.ids)
  vertex.degrees[i] <- sample(1:10, 1)

ranking <- function(v, sim, g) {
  
  # apply the similarity() function between vertex v and all other vertices
  vertex.df$distances <- sapply(vertex.df$vertex.ids, sim, x = v, graph = g)
  
  neighbours <- vertex.df %>%
    filter(distances > 0) %>%
    arrange(distances) %>%
    top_n(n) %>%
    select(vertex.ids)
  
  
  # top_n() may return more values in the presence of ties, so in order to have
  # a correct sampling we need to crop the neighbourhood to exactly n positions
  neighbours <- neighbours[1:n,]
  neighbours
}

bin_ranking <- function(v, sim, b, nb, g) {
  
  # apply the similarity() function between vertex v and all other vertices
  vertex.df$distances <- sapply(vertex.df$vertex.ids, sim, x = v, graph = g)
  
  lower_bound <- ((max(vertex.df$distances) - min(vertex.df$distances)) / nb) * (b - 1) + min(vertex.df$distances)
  upper_bound <- ((max(vertex.df$distances) - min(vertex.df$distances)) / nb) * (b) + min(vertex.df$distances)
  
  neighbours <- vertex.df %>%
    filter(distances >= lower_bound) %>%
    filter(distances >= upper_bound) %>%
    select(vertex.ids)
  
  neighbours
}

generate_rank_graph <- function(similarity, use_bin_ranking = FALSE) {
  
  # number of bins when binned ranking is used
  num_bins = 10
  
  # create an empty directed graph with a given number of vertices
  g <- make_empty_graph(n = num.vertices, directed = TRUE)

  # loop over all vertices in the graph
  for (j in vertex.df$vertex.ids) {
  
    if (use_bin_ranking == TRUE) {
      
        friends <- numeric()
        
        for (n in 1:k) {
            # select the bin from which the target vertices 
            bin <- sample(1:num_bins, 1, prob = probabilities)
            vertex.neighbours <- bin_ranking(vertex.df[j,1], similarity, bin, num_bins, g)
            
            if (length(vertex.neighbours) > 0)
              friend <- sample(vertex.neighbours$vertex.ids, 1)
            else
              friend <- sample(vertex.df$vertex.ids, 1)
            
            friends <- c(friends, friend)
        }
    }
    else {
        # compute the ranking for a given vertex
        vertex.neighbours <- ranking(vertex.df[j,1], similarity, g)
      
        if (use.equal.outdegree == FALSE)
          k <- vertex.degrees[j]
        
        # select a neighbouring vertex using model probabilities
        friends <- sample(vertex.neighbours, k, replace = FALSE, prob = probabilities)
    }
    
    # add edges to selected vertices
    for (l in 1:k) 
      g <- g + edge(j,friends[l])
  }

  # remove duplicated edges and loops
  g <- simplify(g)
  g
} 


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
    result <- igraph::degree(graph, y, mode = "in") + 1
  
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

# define the length of the ranking
n <- 10

# define the number of vertices in the graph
num.vertices <- 36682

# set the number of edges each new vertex creates
k <- 5

# create a given number of vertices
vertex.ids <- 1:num.vertices

# create a random attribute for each vertex
vertex.values <- runif(num.vertices)

# compute the probabilities of selecting each position of the ranking
probabilities <- 1 / 1:n / sum(1/1:n)
#probabilities <- rep(1 / n, 10)

# simulate vertices using a data frame for easy import into igraph
vertex.df <- data.frame(vertex.ids, vertex.values)

# create a random vector for cosine similarity for each vertex
vertex.vectors <- numeric()

for (i in vertex.ids)
  vertex.vectors[i] <- list(c(1,sample(x = 0:1, size = 9, replace = TRUE)))

ranking <- function(v, sim, g) {
  
  # apply the similarity() function between vertex v and all other vertices
  vertex.df$distances <- sapply(vertex.df$vertex.ids, sim, x = v, graph = g)
  
  suppressMessages(neighbours <- vertex.df %>%
    filter(distances > 0) %>%
    arrange(distances) %>%
    top_n(n, distances) %>%
    select(vertex.ids))
  
  
  # top_n() may return more values in the presence of ties, so in order to have
  # a correct sampling we need to crop the neighbourhood to exactly n positions
  neighbours <- neighbours[1:n,]
  neighbours
}

generate_rank_graph <- function(similarity) {
  
  # create an empty directed graph with a given number of vertices
  g <- make_empty_graph(n = num.vertices, directed = TRUE)

  # loop over all vertices in the graph
  for (j in vertex.df$vertex.ids) {
  
    # compute the ranking for a given vertex
    vertex.neighbours <- ranking(vertex.df[j,1], similarity, g)
  
    # select a neighbouring vertex using model probabilities
    friends <- sample(vertex.neighbours, k, replace = FALSE, prob = probabilities)
  
    # add edges to selected vertices
    for (l in 1:k) 
      g <- g + edge(j,friends[l])
  }

  # remove duplicated edges and loops
  g <- simplify(g)
  g
} 


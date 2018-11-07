library(igraph)
library(dplyr)
library(ggplot2)
library(GGally)
library(intergraph)
library(gridExtra)
library(lsa)
library(sigmoid)

# simple functions which compute the distance
# between each pair of nodes. The main aim of each 
# function is to establish the neighbourhood of
# each node

distance.random <- function(x, y, graph) {
  
  # the first simple example returns a random 
  # distance between any pair of nodes
  if (x == y)
    result <- 0
  else
    result <- abs(rnorm(1))
  
  result
}

distance.degree <- function(x, y, graph) {
  
  # the function defines the distance between vertices
  # only by the degree of the target node
  if (x == y)
    result <- 0
  else
    result <- runif(1, 0, log(igraph::degree(graph, y) + 2))
  
  result
}

distance.attribute <- function(x, y, graph, edge.prob=0.05) {
  
  # the function defines the distance between vertices 
  # as the distance between values of any vertex attribute
  #
  # a small constant prevents from infinite distances
  if (x == y)
    result <- 0
  else
    result <- 1 / (abs(vertex.df[x,2] - vertex.df[y,2]) + 0.001)
    # result <- 1 / sqrt((vertex.x[x] - vertex.x[y])^2 + (vertex.y[x] - vertex.y[y])^2)
  
  if (runif(1) < edge.prob)
    result <- 0
  
  result
}

distance.cosine <- function(x, y, graph, edge.prob=0.5) {
  
  # the function defines the distance between vertices
  # as the cosine distance between values of a vertex attribute
  # which contains a vector of numbers
  if (x == y)
    result <- 0
  else
    result <- cosine(vertex.vectors[[x]], vertex.vectors[[y]])
    
  result
}

distance.dissasortative <- function(x, y, graph) {
  
  # the function defines the distance between vertices
  # as the reciprocal of the distance between their degrees
  
  if (x == y)
    result <- 0
  else
    result <- max(vertex.degrees) - (vertex.degrees[x] / vertex.degrees[y])
  
  result
}

distance.hierarchical <- function(x, y, graph, alpha=0.25) {
  
  # the function defines the distance between vertices
  # as the reciprocal of the linear combination of their euclidean 
  # distance in space and the difference of "levels"
  
  color.map <- list('steelblue'=1, 'seagreen'=2, 'tomato'=3)
  
  if (x == y)
    result <- 0
  else {
    distance <- sqrt((vertex.x[x] - vertex.x[y])^2 + (vertex.y[x] - vertex.y[y])^2)
    result <- 1 - (alpha * abs(as.numeric(color.map[vertex.class[x]]) - as.numeric(color.map[vertex.class[y]])) + (1 - alpha) * distance)
  }
  result
}

distance.simple.process <- function(x, y, graph) {
  
  # the function defines a simple deterministic process
  # where two vertices create edge if the proximity
  # between attribute values of these vertices is small
  alpha = 0.5
  
  if (x == y)
    result <- 0.001
  else {
    if (vertex.z[x] == vertex.z[y])
      result <- alpha * sqrt((vertex.x[x]-vertex.x[y])**2 + (vertex.y[x]-vertex.y[y])**2)
    else
      result <- abs(vertex.x[x]-vertex.x[y]) + abs(vertex.y[x]-vertex.y[y])
  }

  1/result
}

distance.simple.process.with.random <- function(x, y, graph) {
  
  # the function defines a simple process
  # where two vertices create edge if the proximity
  # between attribute values of these vertices is small
  # with a small stochastic element
  alpha = 0.5
  
  if (x == y)
    result <- 0.001
  else {
    if (vertex.z[x] == vertex.z[y])
      result <- alpha * sqrt((vertex.x[x]-vertex.x[y])**2 + (vertex.y[x]-vertex.y[y])**2)
    else
      result <- abs(vertex.x[x]-vertex.x[y]) + abs(vertex.y[x]-vertex.y[y])
  }
  
  result <- result + runif(1, min=-1, max=1) * 0.1  
  
  1/result
}

distance.simple.process.with.sigmoid <- function(x, y, graph) {
  
  # the function defines a simple process
  # where two vertices create edge if the proximity
  # between attribute values of these vertices is small
  # but the final distance goes through non-linearity
  alpha = 0.5
  
  if (x == y)
    result <- 0.001
  else {
    if (vertex.z[x] == vertex.z[y])
      result <- alpha * sqrt((vertex.x[x]-vertex.x[y])**2 + (vertex.y[x]-vertex.y[y])**2)
    else
      result <- abs(vertex.x[x]-vertex.x[y]) + abs(vertex.y[x]-vertex.y[y])
  }
  
  # transform result to the range [-10, 10]
  result <- (result/2) * 20 - 10
  # pass the result through sigmoid
  result <- 1 - sigmoid(result)
  
  result
}

distance.no.simple.process <- function(x, y, graph) {
  
  # the function defines a contradiction of a simple process
  # where two vertices create edge stochastically
  
  if (x == y)
    result <- 0.001
  else {
    if (runif(1) < 0.5)
      result <- sqrt((vertex.x[x]-vertex.x[y])**2 + (vertex.y[x]-vertex.y[y])**2)
    else
      result <- abs(vertex.x[x]-vertex.x[y]) + abs(vertex.y[x]-vertex.y[y])
  }

1/(runif(1, min = -1, max = 1) * result)
}

# define the length of the ranking
n <- 10

# define the number of vertices in the graph
num.vertices <- 100

# define if equal number of outgoing edges should be used
use.equal.outdegree <- FALSE

# set the number of edges each new vertex creates
k <- 4

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

# create a random assignment of vertices to one of two classes
vertex.class <- rep('steelblue', num.vertices)
vertex.class[ sample(num.vertices, num.vertices/2) ] <- 'seagreen'
vertex.class[ sample(num.vertices, num.vertices/4) ] <- 'tomato'

# create random coordinates for each vertex
vertex.x <- runif(num.vertices)
vertex.y <- runif(num.vertices)

# create random categorical attribute with small number of values
vertex.z <- sample(c('A','B'), size = num.vertices, replace = TRUE)

layout.geom <- function(){
  cbind(vertex.x, vertex.y)
}

for (i in vertex.ids)
  vertex.vectors[i] <- list(c(1,sample(x = 0:1, size = 9, replace = TRUE)))

# create a vector of expected vertex degrees
vertex.degrees <- numeric()

for (i in vertex.ids)
  vertex.degrees[i] <- sample(1:10, 1)

ranking <- function(v, dist, g) {
  
  # apply the distance() function between vertex v and all other vertices
  vertex.df$dst <- sapply(vertex.df$vertex.ids, dist, x = v, graph = g)
  
  neighbours <- vertex.df %>%
    filter(dst > 0) %>%
    arrange(dst) %>%
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

generate_graph <- function(distance) {
  
  # create an empty directed graph with a given number of vertices
  g <- make_empty_graph(n = num.vertices, directed = TRUE)
  
  # loop over all vertices in the graph
  for (j in vertex.df$vertex.ids) {
    
    # compute the ranking for a given vertex
    vertex.neighbours <- ranking(vertex.df[j,1], distance, g)
    
    # select a neighbouring vertex using model probabilities
    friends <- vertex.neighbours[1:k]
  
    # add edges to selected vertices
    for (l in 1:k)
      g <- g + edge(j,friends[l])
  }
    
  # remove duplicated edges and loops
  g <- simplify(g)
  g$vertex.class <- vertex.class
  g$vertex.coords <- cbind(vertex.x, vertex.y)
  
  g
} 

  
  
generate_rank_graph <- function(distance, use_bin_ranking = FALSE) {
  
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
            vertex.neighbours <- bin_ranking(vertex.df[j,1], distance, bin, num_bins, g)
            
            if (length(vertex.neighbours) > 0)
              friend <- sample(vertex.neighbours$vertex.ids, 1)
            else
              friend <- sample(vertex.df$vertex.ids, 1)
            
            friends <- c(friends, friend)
        }
    }
    else {
        # compute the ranking for a given vertex
        vertex.neighbours <- ranking(vertex.df[j,1], distance, g)
      
#        if (use.equal.outdegree == FALSE)
#          k <- vertex.degrees[j]
        
        # select a neighbouring vertex using model probabilities
        friends <- sample(vertex.neighbours, k, replace = FALSE, prob = probabilities)
    }
    
    # add edges to selected vertices
    for (l in 1:k)
      g <- g + edge(j,friends[l])
    
  }

  # remove duplicated edges and loops
  g <- simplify(g)
  g$vertex.class <- vertex.class
  g$vertex.coords <- cbind(vertex.x, vertex.y)
  
  g
} 


source(file = 'scripts/create.network.R')
library(fmsb)


normalizze <- function(x) {
  x <- (x - min(x))/(max(x) - min(x))
}

#num.vertices <- 1000

#g <- sample_pa(n = num.vertices, power = 2.5, directed = TRUE, m = 5)

data.file <- read.csv(file = 'data/gnutella.csv', header = FALSE, sep = '\t')
g <- graph_from_data_frame(data.file)
num.vertices <- vcount(g)


d <- data.frame(id = 1:num.vertices, degree = degree(g, mode = "in"))
b <- data.frame(id = 1:num.vertices, betweenness = betweenness(g, directed = TRUE))
c <- data.frame(id = 1:num.vertices, closeness = closeness(g, mode = "in"))
r <- data.frame(id = 1:num.vertices, random = runif(num.vertices))

#d2 <- data.frame(id = 1:num.vertices, degree2 = d$degree^2)
#b2 <- data.frame(id = 1:num.vertices, betweenness2 = b$betweenness^2)
#c2 <- data.frame(id = 1:num.vertices, closeness2 = c$closeness^2)

x <- d
x <- merge(x,b)
#x <- merge(x,c)
#x <- merge(x,r)

edges <- as.data.frame(get.edges(g, 1:ecount(g)))
names(edges) <- c('x','y')

nonedges <- merge(as.numeric(1:num.vertices), as.numeric(1:num.vertices)) %>% setdiff(edges)
names(nonedges) <- c('x','y')

D.existing <- x %>% 
  inner_join(edges, by = c("id" = "x"), copy = TRUE) %>% 
  inner_join(x, by = c("y" = "id"), copy = TRUE)
D.existing$response <- 0

D.nonexisting <- x %>%
  inner_join(nonedges, by = c("id" = "x"), copy = TRUE) %>%
  inner_join(x, by = c("y" = "id"), copy = TRUE)
D.nonexisting$response <- 1

D <- union(D.existing, D.nonexisting)

y <- D$response

# remove the response variable
D <- D[-length(D)]

# remove the id of the source node
D <- D[-c(1, (length(D)/2) + 1)]

# add a column with random numbers
D$random <- rnorm(1:num.vertices)

# normalize all features
D <- as.data.frame(lapply(D, normalizze))

D <- as.matrix(D)

beta <- solve(t(D) %*% D) %*% t(D) %*% y

dist <- D %*% beta
dim(dist) <- c(num.vertices, num.vertices)

similarity.reverse <- function(x,y,graph) {
  if (x == y)
    result <- 0
  else
    result <- 1 / dist[x,y]
  
  result
}

h <- generate_rank_graph(similarity.reverse)

amg <- as.vector(as_adjacency_matrix(g))
amh <- as.vector(as_adjacency_matrix(h))

true.positives  <- sum(which(amg==1) %in% which(amh==1))
true.negatives  <- sum(which(amg==0) %in% which(amh==0))
false.positives <- sum(which(amg==0) %in% which(amh==1))
false.negatives <- sum(which(amg==1) %in% which(amh==0))

confusion.matrix <- rbind(cbind(true.positives, false.negatives), cbind(false.positives, true.negatives))
rownames(confusion.matrix) <- c('edge', 'no edge')
colnames(confusion.matrix) <- c('edge', 'no edge')

results <- data.frame(k = numeric(0), cnt = numeric(0))

for (i in 1:num.vertices)
  for (j in 1:10) {
    a <- as.vector(neighbors(g, i))
    r <- ranking(i, similarity.reverse, g)[1:j]
    results <- rbind(results, data.frame(k=j, cnt = sum(a %in% r)))
}

print(beta)
print(confusion.matrix)

results %>%
  group_by(k) %>%
  summarize(prec.at.k = sum(cnt)/ecount(g))

print(ks.test(degree(g), degree(h)))
print(ks.test(betweenness(g), betweenness(h)))
print(ks.test(closeness(g), closeness(h)))

print(Kappa.test(confusion.matrix))

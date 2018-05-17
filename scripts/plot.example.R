library(igraph)

g <- make_empty_graph() %>%
  add_vertices(1, name='Alice', age=30, sex='F') %>%
  add_vertices(1, name='Bob', age=40, sex='M') %>%
  add_vertices(1, name='Cecil', age=25, sex='M') %>%
  add_vertices(1, name='Diane', age=20, sex='F') %>%
  add_vertices(1, name='Eve', age=35, sex='F') %>%
  add_edges(c(1,4,1,5,2,3,2,5,3,1,3,4,4,1,4,5,5,1,5,4))

l <- layout_with_dh(g)

g <- make_empty_graph() %>%
  add_vertices(1, name='Alice', age=30, sex='F') %>%
  add_vertices(1, name='Bob', age=40, sex='M') %>%
  add_vertices(1, name='Cecil', age=25, sex='M') %>%
  add_vertices(1, name='Diane', age=20, sex='F') %>%
  add_vertices(1, name='Eve', age=35, sex='F')

png(filename = "example.png", width=2400, height=1600, units="px")
par(mfrow=c(2,3), ps=12, cex=2, cex.main=1)

l <- layout_with_fr(g)

g <- g %>%
  add_edges(c(1,4,1,5), width=10, lty=1)

plot(g, layout=l, edge.width=E(g)$width, edge.lty=E(g)$lty, edge.curved=0.3, edge.arrow.size=2, vertex.label.family="Arial",
     vertex.label.font=2, vertex.label=V(g)$name, vertex.size=55, vertex.color=c("sandybrown","azure")[1+(V(g)$sex=="M")], main="1. Alice")
box()

g <- g %>%
  set_edge_attr('width', value=5) %>%
  set_edge_attr('lty', value=2) %>%
  add_edges(c(2,3,2,5), width=10, lty=1)

plot(g, layout=l, edge.width=E(g)$width, edge.lty=E(g)$lty, edge.curved=0.3, edge.arrow.size=2,vertex.label.family="Arial",
     vertex.label.font=2, vertex.label=V(g)$name, vertex.size=55, vertex.color=c("sandybrown","azure")[1+(V(g)$sex=="M")], main="2. Bob")
box()

g <- g %>%
  set_edge_attr('width', value=5) %>%
  set_edge_attr('lty', value=2) %>%
  add_edges(c(3,1,3,4), width=10, lty=1)

plot(g, layout=l, edge.width=E(g)$width, edge.lty=E(g)$lty, edge.curved=0.3,edge.arrow.size=2,vertex.label.family="Arial",
     vertex.label.font=2, vertex.label=V(g)$name, vertex.size=55, vertex.color=c("sandybrown","azure")[1+(V(g)$sex=="M")], main="3. Cecil")
box()

g <- g %>%
  set_edge_attr('width', value=5) %>%
  set_edge_attr('lty', value=2) %>%
  add_edges(c(4,1,4,5), width=10, lty=1)

plot(g, layout=l, edge.width=E(g)$width, edge.lty=E(g)$lty, edge.curved=0.3,edge.arrow.size=2,vertex.label.family="Arial",
     vertex.label.font=2, vertex.label=V(g)$name, vertex.size=55, vertex.color=c("sandybrown","azure")[1+(V(g)$sex=="M")], main="4. Diane")
box()

g <- g %>%
  set_edge_attr('width', value=5) %>%
  set_edge_attr('lty', value=2) %>%
  add_edges(c(5,1,5,4), width=10, lty=1)

plot(g, layout=l, edge.width=E(g)$width, edge.lty=E(g)$lty, edge.curved=0.3,edge.arrow.size=2,vertex.label.family="Arial",
     vertex.label.font=2, vertex.label=V(g)$name, vertex.size=55, vertex.color=c("sandybrown","azure")[1+(V(g)$sex=="M")], main="5. Eve")
box()

g <- g %>%
  set_edge_attr('width', value=10) %>%
  set_edge_attr('lty', value=1) 

plot(g, layout=l, edge.width=E(g)$width, edge.lty=E(g)$lty, edge.curved=0.3,edge.arrow.size=2,vertex.label.family="Arial",
     vertex.label.font=2, vertex.label=V(g)$name, vertex.size=55, vertex.color=c("sandybrown","azure")[1+(V(g)$sex=="M")], main="Final network")
box()

dev.off()


library(igraph)
library(dplyr)
library(magrittr)


load("union_characters.RData")
load("union_edges.RData")

#Plotting Network using igraph

g= graph.data.frame(union_edges, directed = TRUE, vertices = union_characters)

plot(g, layout = layout_with_fr(g),
     vertex.size = 5, 
     vertex.label = NA,
     vertex.label.cex = .4,
     vertex.label.color = "black",
     vertex.frame.color = "orange",
     edge.arrow.size = .3, 
     edge.color = 'light blue'
)

#number of nodes and edges

nrow(union_characters)
nrow(union_edges)

#edgelist

as_adj_edge_list(g)

#Simplify graph

g= simplify(g, remove.multiple = TRUE, remove.loops = TRUE)
plot(g, layout = layout_with_fr(g),
     vertex.size = 5, 
     vertex.label = NA,
     vertex.label.cex = .4,
     vertex.label.color = "black",
     vertex.frame.color = "orange",
     edge.arrow.size = .3, 
     edge.color = 'light blue')


#adjacency matrix

as_adjacency_matrix(g)

# Replace the vertex labels (auto label) of each node with the node names stored in the data table

plot(g, layout = layout_with_fr(g),
     vertex.size = 5, 
     vertex.label = V(g)$name,
     vertex.label.cex = .4,
     vertex.label.color = "black",
     vertex.frame.color = "orange",
     edge.arrow.size = .3, 
     edge.color = 'light blue'
)

#Generate node color based on the type of nodes. For example, different group of people belongs to different type of sports are visualized using different colors. Add legends to explain the meaning of colors.
plot(g, layout = layout_with_fr(g),
     vertex.size = 5, 
     vertex.label = V(g)$name,
     vertex.label.cex = .4,
     vertex.label.color = "black",
     vertex.color = V(g)$node_color,
     vertex.frame.color = V(g)$node_color,
     edge.arrow.size = .3, 
     edge.color = 'light blue'
)
#Set the node size based on the degree of the node.

degree = degree(g, mode="all")

plot(g, layout = layout_with_fr(g),
     vertex.size = degree, 
     #      vertex.label = NA,
     vertex.label = V(g)$name,
     vertex.label.cex = .4,
     vertex.label.color = "black",
     vertex.color = V(g)$node_color,
     vertex.frame.color = V(g)$node_color,
     edge.arrow.size = .3, 
     edge.color = 'light blue'
)


#Use 5 different type of layouts in igraph to plot the network and display it. Explain the changes in the network structure and compare your network based on the different layout algorithm that you have used.

# 1. circle
plot(g, layout = layout_in_circle(g),
     vertex.size = 5, 
     vertex.label = NA,
     vertex.label.cex = .4,
     vertex.label.color = "black",
     vertex.color = V(g)$node_color,
     vertex.frame.color = 'black',
     edge.arrow.size = .3, 
     edge.color = 'light blue',
     main = "Circle"
)

# 2. sphere
plot(g, layout = layout_on_sphere(g),
     vertex.size = 5, 
     vertex.label = NA,
     vertex.label.cex = .4,
     vertex.label.color = "black",
     vertex.color = V(g)$node_color,
     vertex.frame.color = 'black',
     edge.arrow.size = .3, 
     edge.color = 'light blue',
     main = "Sphere"
)

# 3. kamada kawai
plot(g, layout = layout.kamada.kawai(g),
     vertex.size = 5, 
     vertex.label = NA,
     vertex.label.cex = .4,
     vertex.label.color = "black",
     vertex.color = V(g)$node_color,
     vertex.frame.color = 'black',
     edge.arrow.size = .3, 
     edge.color = 'light blue',
     main = "Kamada Kawai"
)

# 4. random
plot(g, layout = layout_randomly(g),
     vertex.size = 5, 
     vertex.label = NA,
     vertex.label.cex = .4,
     vertex.label.color = "black",
     vertex.color = V(g)$node_color,
     vertex.frame.color = 'black',
     edge.arrow.size = .3, 
     edge.color = 'light blue',
     main = "Random"
)

# 5. Spring
plot(g, layout = layout.spring(g),
     vertex.size = 5, 
     vertex.label = NA,
     vertex.label.cex = .4,
     vertex.label.color = "black",
     vertex.color = V(g)$node_color,
     vertex.frame.color = 'black',
     edge.arrow.size = .3, 
     edge.color = 'light blue',
     main = "Spring"
)

# 6. best graph
plot(g, layout = layout_nicely(g),
     vertex.size = 5, 
     vertex.label = NA,
     vertex.label.cex = .4,
     vertex.label.color = "black",
     vertex.color = V(g)$node_color,
     vertex.frame.color = 'black',
     edge.arrow.size = .3, 
     edge.color = 'light blue',
     main = "Layout Nicely"
)


#Rank the nodes based on degree, betweenness, closeness and eigenvector centrality value and display it in a table.

# based on degree
degree = degree(g, mode = "all")
head(as.data.frame(sort(degree, decreasing = T)))

# based on betweeness
between = betweenness(g, directed = T, weights = NA)
head(as.data.frame(sort(between, decreasing = T)))

# based on closeness
close = closeness(g, mode = "all")
head(as.data.frame(sort(close, decreasing = T)))

# based on centrality
centrality = eigen_centrality(g, directed = T, weights = NA)$vector
head(as.data.frame(sort(centrality, decreasing = T)))


#Find the nodes with highest degree, betweenness centrality, closeness and eigenvector centrality values.

which.max(degree)

which.max(between)

which.max(close)

which.max(centrality)

#Find the hubs in the network and display each of it in a different color.

hs = hub_score(g, weights = NA)$vector
plot(g, layout = layout_with_fr(g),
     vertex.size = hs * 10, 
     vertex.label = NA,
     vertex.label.cex = .4,
     vertex.label.color = "black",
     vertex.frame.color = "orange",
     edge.arrow.size = .3, 
     edge.color = 'light blue'
)


#Calculate the average path length for both (undirected and directed network).

# directed
mean_distance(g, directed=T)

# undirected
mean_distance(g, directed=F)

#Using the undirected network, find all the shortest paths from one node to another and the length of all shortest paths in the graph

g.undir = as.undirected(g)

# shortest path
head(all_shortest_paths(g.undir, from = V(g.undir), to = V(g.undir), mode = c("all"))$res[])

# length of shortest path
head(distances(g.undir, v = V(g.undir), to = V(g.undir), mode = c("all")))

#Find the shortest path from the node with highest betweenness centrality (broker) to all other nodes. Color the path that has the longest shortest path from the broker to its destination node. Repeat the same for nodes with highest degree and eigenvector centrality values.

which.max(between)
shortest_paths(g.undir, from = V(g.undir)[name == 'Jon Snmow'], to = V(g.undir), mode = c("all"))$vpath

which.max(between)
head(shortest_paths(g.undir, from = V(g.undir)[name == 'Eddard Stark'], to = V(g.undir), mode = c("all"))$vpath)

which.max(degree)
head(shortest_paths(g.undir, from = V(g.undir)[name == 'Quellon Greyjoy'], to = V(g.undir), mode = c("all"))$vpath)

which.max(centrality)
head(shortest_paths(g.undir, from = V(g.undir)[name == 'Alys Arryn'], to = V(g.undir), mode = c("all"))$vpath)


#Identify the immediate neighbours of the node with highest degree centrality value. Set colors to plot the neighbours. Display the network and explain the neighbours with this important node.
neigh.nodes.degree = neighbors(g.undir, V(g.undir)[name == "Eddard Stark"], mode = "all")
neigh.nodes.degree

vcol2 = rep('gray', vcount(g.undir))
vcol2[neigh.nodes.degree] = "#ff9d00"

plot(g.undir, layout = layout_nicely(g.undir),
     vertex.size = 5, 
     #      vertex.label = V(g)$name,
     vertex.label.cex = .4,
     vertex.color = vcol2,
     vertex.label.color = 'black',
     vertex.frame.color = vcol2,
     edge.arrow.size = .3
)


#Identify the immediate neighbours of the node with highest eigenvector centrality value. Set colors to plot the neighbours. Display the network and explain the neighbours with this important node

neigh.nodes.centrality = neighbors(g.undir, V(g.undir)[name == "Alys Arryn"], mode = "all")

neigh.nodes.centrality

vcol3 = rep('gray', vcount(g.undir))
vcol3[neigh.nodes.centrality] = "pink"

plot(g.undir, layout = layout_nicely(g.undir),
     vertex.size = 5, 
     #      vertex.label = V(g)$name,
     vertex.label.cex = .4,
     vertex.color = vcol3,
     vertex.label.color = 'black',
     vertex.frame.color = vcol3,
     edge.arrow.size = .3
)


#Find cliques in the network and display it. How many cliques that you can find in the network?

cliques(g.undir)     
# max_cliques(g.undir)
clique_num(g.undir)

set.seed(0)
vcol = rep("grey80", vcount(g.undir))

vcol[unlist(largest_cliques(g.undir))] = "gold"

plot(as.undirected(g.undir), 
     #      vertex.label = V(g.undir)$name, 
     vertex.label = NA, 
     vertex.color = vcol,
     vertex.size = 5,
     vertex.label.cex = .5,
     vertex.label.color = 'black'
)

#Find a community detection algorithm in igraph. Explain how it works. Apply the community detection on your network and display the network. Each community must be in its own color.

ceb = cluster_edge_betweenness(g.undir)

plot(ceb, g.undir,
     layout = layout_nicely(g.undir),
     vertex.size = 5, 
     #      vertex.label = V(g)$name,
     vertex.label.cex = .4,
     vertex.label.color = 'black',
     edge.arrow.size = .3
     
) 


#Find the number of communities that occur.

length(ceb)

#Find its membership

membership(ceb)

#Find how modular the graph partitioning is. (High modularity for a partitioning reflects dense connections within communities and sparse connections across communities)

modularity(ceb)




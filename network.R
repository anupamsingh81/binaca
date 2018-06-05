library(tidyverse)
library(tidytext)
library(tidygraph)
library(ggraph)
library(igraph)

network=songcol %>% filter(year<1966) %>% 
  mutate(music_director= str_replace_all(music_director," ","")) %>% 
  mutate(singer= str_replace_all(singer," ","")) %>% 
  select(music_director,rank,year,singer) %>%
  filter(year>1953,year<1970) %>%  # can select year here
  unnest_tokens(word,music_director) %>% 
  #filter(word%in%toplyrics) %>% 
  # group_by(year) %>% 
  #count(word) %>% 
  #arrange(desc(n)) %>% 
  #arrange(year) %>% 
  rename(music_director=word) %>% 
  unnest_tokens(word,singer) %>% 
  rename(singer=word)

network1=network %>% group_by(singer) %>% count %>% # around 29 singers
  filter(n>1) #%>% # select only those who have at least sung 2 popular songs, the number goes down from 29 to 17
  


networkk = network %>% semi_join(network1,by="singer") # only network with node having greater than 1 frequency of connxn different from no. of edges
  

# can repeat analysis for both networkk and network

# nodelist

sources <- networkkk%>%
  distinct(music_director) %>%
  rename(label = music_director)

destinations <- networkkk%>%
  distinct(singer) %>%
  rename(label = singer)

node_list = full_join(sources,destinations)

nodes <- add_column(node_list, id = 1:nrow(node_list)) %>% 
  select(id, everything())


# edgelist

per_route <- networkkk %>%  
  group_by(music_director, singer) %>%
  summarise(weight = n()) %>% 
  ungroup()
per_route


edges <- per_route %>% 
  left_join(nodes, by = c("music_director" = "label")) %>% 
  rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("singer" = "label")) %>% 
  rename(to = id)

edges <- select(edges, from, to, weight)


edges1=edges %>% filter(weight>2) # select only people with greater than 4 collaboration

nodes

nodes1=edges %>% 
  filter(weight>2) %>%  # select only greater than 4 collaboration
  select(from,to) %>% 
  gather(key="link",value="id") %>% 
  distinct(id) %>% 
  left_join(nodes) # select only nodes with greater than 4 collaboration


routes_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = FALSE)

options(scipen=999)

library(igraph)
routes_igraph <- graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)
# STATS
routes_tidy %>%
  activate(nodes) %>%
  mutate(degree = centrality_degree(),closeness=centrality_closeness(),betweenness= centrality_betweenness(),
         authority=centrality_pagerank()) %>% as_tibble() %>% arrange(desc(authority)) %>% View()

edge_density(routes_igraph)



# 2 components here 34 and 57 (ram lal,kishori amonkar single collb,isolated)

?components

# Diameter

diameter(routes_igraph,directed = FALSE)

# A diameter of 11 suggests network not compact takes 11 degrees of separation


# clustering coefficients

transitivity(routes_igraph)


# transitivity is proportion of closed triangles(triads)/total no. of open+closed triangles..0.029 suggest poor clustering

# other newtwork properties

# network size
E(routes_igraph)

V(routes_igraph)

# 150 edges , 62 vertices

mean_distance(routes_igraph,directed = FALSE)

# Everyone can reach within average of 3 nodes


# Diameter

diameter(routes_igraph,directed = FALSE)

centr_degree(routes_igraph,mode = "in",normalized = T)

# implies 31.4% centralization along one node


edge_density(routes_igraph)
#8% edge density

degree(routes_igraph) # degree of each node

# mean degree
mean(degree(routes_igraph)) # degree of each node)
# mean degree 5 for network

# required for directed to symmetrize it, but can run even in undirected

net.sym = as.undirected(routes_igraph,mode="collapse",edge.attr.comb = list(weight="sum","ignore"))

#cliques

cliques(net.sym)

# length of all cliques
sapply(cliques(net.sym),length)

# largest_cliques

largest_cliques(net.sym)

# 13 cliques with three vertices

# color clique

vcol <- rep("grey80",vcount(net.sym))

vcol[unlist(largest_cliques(net.sym))] = "gold"

plot(as.undirected(net.sym),vertex.label=V(net.sym)$name,vertex.color=vcol)

ceb= cluster_edge_betweenness(routes_igraph) # calculate community partition


dendPlot(ceb,mode="hclust")


plot(ceb,routes_igraph)

length(ceb) # number of communities

membership(ceb) # membership of ech node


plot(ceb,routes_igraph)



routes_tidy



routes_tidy %>% 
  activate(edges) %>% 
  arrange(desc(weight))

ggraph(routes_tidy) + geom_edge_link() + geom_node_point() + theme_graph()


ggraph(routes_tidy, layout = "graphopt") + 
  geom_node_point() +
  geom_edge_link(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label), repel = TRUE) +
  labs(edge_width = "songs") +
  theme_graph()


routes_tidy1 <- tbl_graph(nodes = nodes, edges = edges1, directed = FALSE)


edges1
class(routes_tidy)

routes_tidy


routes_tidy1 %>% 
  activate(edges) %>% 
  arrange(desc(weight))

ggraph(routes_tidy1) + geom_edge_link() + geom_node_point() + theme_graph()


ggraph(routes_tidy1, layout = "graphopt") + 
  geom_node_point() +
  geom_edge_link(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label), repel = TRUE) +
  labs(edge_width = "Songs") +
  theme_graph()

set.seed(123)

# centrality
routes_tidy %>%
  activate(nodes) %>%
  mutate(centrality = centrality_authority()) %>% 
  ggraph(layout = "graphopt") + 
  geom_edge_link(width = 1, colour = "lightgray") +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  geom_node_text(aes(label = label), repel = TRUE)+
  scale_color_gradient(low = "yellow", high = "red")+
  theme_graph()

# show label only with degree say greater than 2
routes_tidy %>%
  activate(nodes) %>%
  mutate(centrality = centrality_degree()) %>% 
  filter(centrality>2) %>% 
  ggraph(layout = "graphopt") + 
  geom_edge_link(width = 1, colour = "lightgray") +
 geom_node_point(aes(size = centrality, colour = centrality)) +
  geom_node_text(aes(label = label), repel = TRUE)+
  scale_color_gradient(low = "yellow", high = "red")+
  theme_graph()


# clusters
set.seed(123)
routes_tidy1 %>%
  activate(nodes) %>%
  mutate(community = as.factor(group_infomap())) %>% 
  ggraph(layout = "graphopt") + 
  geom_edge_link(width = 1, colour = "lightgray") +
  geom_node_point(aes(colour = community), size = 4) +
  geom_node_text(aes(label = label), repel = TRUE)+
  theme_graph()

edges %>% count(to) %>% View()


centr
#igraph


library(igraph)
routes_igraph <- graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)

plot(routes_igraph, layout = layout_with_graphopt, edge.arrow.size = 0.2)

# plot only those with greater than 3 degree

plot(routes_igraph, vertex.label = ifelse(degree(routes_igraph) > 3, V(routes_igraph)$label, NA),layout = layout_with_graphopt, edge.arrow.size = 0.2)

#######SUBGRAPH################################

# nodes1$id only has nodes with edge weight >4

subgraph(routes_igraph,nodes1$id)

ig= induced_subgraph(routes_igraph, nodes1$id, impl = "auto")

plot(ig, layout = layout_with_graphopt, edge.arrow.size = 0.2)

ceb= cluster_edge_betweenness(ig) # calculate community partition


dendPlot(ceb,mode="hclust")


length(ceb) # number of communities


node2 = data.frame(id=as.numeric(names(membership(ceb))),component=as.vector(membership(ceb)))


node3 = inner_join(nodes,node2)


# membership of ech node

nodes %>% filter()



plot(ceb,routes_igraph)


#stats


edge_density(ig)

#Thus higher in this  subgraph 0.35 as compared to 0.029 as selected subset

#components
components(ig)

# only 1 component here c.f  2 components in original here 34 and 57 (ram lal,kishori amonkar single collb,isolated)

?components

# Diameter

diameter(ig,directed = FALSE)

# A diameter of 11 suggests network not compact takes 11 degrees of separation

#Diameter of 15 larger not connected network since we took out some edges

# clustering coefficients

transitivity(ig)

# transitivity is proportion of closed triangles(triads)/total no. of open+closed triangles..0.05 suggest poor clustering but better compared to original 0.029



# using tidygraph

routes_igraph_tidy <- as_tbl_graph(ig) # converting the subgraph to tidy

?as_tbl_graph
routes_igraph_tidy

ggraph(routes_igraph_tidy, layout = "graphopt") + 
  geom_node_point(color="green") +
  geom_edge_link(aes(width = weight,color="red"),alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label), repel = TRUE) +
  labs(edge_width = "Songs") +
  theme_graph()


# centrality
set.seed(123)

routes_igraph_tidy %>%
  activate(nodes) %>%
  mutate(centrality = centrality_authority()) %>% 
  ggraph(layout = "graphopt") + 
  geom_edge_link(aes(width = weight), colour = "lightgray") +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  geom_node_text(aes(label = label), repel = TRUE)+
  scale_color_gradient(low = "yellow", high = "red")+
  theme_graph()

set.seed(123)

routes_igraph_tidy %>%
  activate(nodes) %>%
 # activate(edges) %>% 
  mutate(centrality = centrality_pagerank()) %>% 
  ggraph(layout = "graphopt") + 
  geom_edge_link(aes(width = weight), colour = "lightgray") +
  geom_node_point(aes(size = centrality, colour = centrality)) +
  geom_node_text(aes(label = label), repel = TRUE)+
  scale_color_gradient(low = "yellow", high = "red")+
  theme_graph()

# clusters
set.seed(123)
routes_igraph_tidy %>%
  activate(nodes) %>%
  mutate(community = as.factor(group_infomap())) %>% 
  ggraph(layout = "graphopt") + 
  geom_edge_link(aes(width = weight), colour = "lightgray") +
  geom_node_point(aes(colour = community), size = 4) +
  geom_node_text(aes(label = label), repel = TRUE)+
  theme_graph()
?centrality_edge_betweenness()

?separate
##singer col2

# crateing dataframe of singer singer inrxn

snet1=songcol %>% filter(year<1966) %>% 
  mutate(music_director= str_replace_all(music_director," ","")) %>% 
  mutate(singer= str_replace_all(singer," ","")) %>% 
  select(music_director,rank,year,singer) %>% separate(singer,into=c("singer1","singer2"),sep=",") %>% 
  drop_na() %>% 
  select(rank,year,singer1,singer2,year) %>% rename(music_director=singer1,singer=singer2) %>% 
  mutate(music_director=tolower(music_director),singer=tolower(singer))


networkkk = full_join(networkk,snet1)


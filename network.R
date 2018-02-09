

network=songcol %>% mutate(music_director= str_replace_all(music_director," ","")) %>% 
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

?distinct
unique(network$singer)


# nodelist

sources <- network%>%
  distinct(music_director) %>%
  rename(label = music_director)

destinations <- network %>%
  distinct(singer) %>%
  rename(label = singer)

node_list = full_join(sources,destinations)

nodes <- add_column(node_list, id = 1:nrow(node_list)) %>% 
  select(id, everything())


# edgelist

per_route <- network %>%  
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

edges1=edges %>% filter(weight>5) # select only people with greater than 5 collaboration

nodes

nodes1=edges %>% 
  filter(weight>4) %>%  # select only greater than 4 collaboration
  select(from,to) %>% 
  gather(key="link",value="id") %>% 
  distinct(id) %>% 
  left_join(nodes) # select only nodes with greater than 5

library(tidygraph)
library(ggraph)

routes_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)

class(routes_tidy)

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
  labs(edge_width = "Letters") +
  theme_graph()


routes_tidy1 <- tbl_graph(nodes = nodes, edges = edges1, directed = TRUE)

edges1
class(routes_tidy)

routes_tidy


routes_tidy1 %>% 
  activate(edges) %>% 
  arrange(desc(weight))

ggraph(routes_tidy1) + geom_edge_link() + geom_node_point() + theme_graph()


ggraph(routes_tidy, layout = "graphopt") + 
  geom_node_point() +
  geom_edge_link(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label), repel = TRUE) +
  labs(edge_width = "Letters") +
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

library(igraph)
routes_igraph <- graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE)

plot(routes_igraph, layout = layout_with_graphopt, edge.arrow.size = 0.2)


#######SUBGRAPH################################

# nodes1$id only has nodes with edges >4

subgraph(routes_igraph,nodes1$id)

ig= induced_subgraph(routes_igraph, nodes1$id, impl = "auto")

plot(ig, layout = layout_with_graphopt, edge.arrow.size = 0.2)

# using tidygraph

routes_igraph_tidy <- as_tbl_graph(ig) # converting the subgraph to tidy


routes_igraph_tidy

ggraph(routes_igraph_tidy, layout = "graphopt") + 
  geom_node_point() +
  geom_edge_link(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label), repel = TRUE) +
  labs(edge_width = "Letters") +
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





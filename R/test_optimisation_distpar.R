library(igraph)
library(tictoc)


tax_assign <- function (x,total = 100){
  c(rep("A",x),rep("B",total-x))
}

estimation_transfer <- function (x, col1){
  x_A <- subgroup_graph(x,c(col1))
  plot(x_A)
  x_comp = components(x_A)

  num_T = 0
  denom_T = 0

  for (i in 1:x_comp$no){
    size = x_comp$csize[i]
    denom_T = denom_T + size
    if (size > 1){
      num = (size*(size-1))/2
      num_T = num_T + num
    }
  }
  dist_T = num_T/((denom_T*(denom_T-1))/2)
  print(dist_T)
}

new_graph <- function (reseau){
  reseau = set_color(reseau)
  E(reseau)$weight = rep(1,length(E(reseau)))
  plot(reseau, vertex.label=NA, vertex.size =10)
  return(reseau)
}


cluster_selection <- function(graph, nom_col1, nom_col2){
  if (components(graph)$no > 1){
    comp <- components(graph)
    clusters <- c()
    for (i in 1:comp$no){
      fact_comp <- factor(V(graph)$tax[which(components(graph)$membership == i)])
      #=== counter for the clusters that will compose the distance calculation graph
      if (comp$csize[i] > 5){
        clusters <- append(clusters,i)
      }else if (length(levels(fact_comp)) == 2){
        if (count(fact_comp)[which(count(fact_comp)$x == nom_col1),2] > count(fact_comp)[which(count(fact_comp)$x == nom_col2),2]){
          clusters <- append(clusters,i)
        }
      }

    }
    select_clust <- induced.subgraph(graph,comp$membership %in% clusters)
  }
  return(select_clust)
}

#===== Make the network
reseau  <- erdos.renyi.game(100, 0.05, "gnp", directed = FALSE, loops = FALSE)

V(reseau)$tax = tax_assign(40)
reseau = set_color(reseau)
V(reseau)$color
E(reseau)$weight = rep(1,length(E(reseau)))
plot(reseau, vertex.label=NA, vertex.size =10)

v1 = V(reseau)[which(V(reseau)$tax =="A")]
v2 = V(reseau)[which(V(reseau)$tax =="B")]
v.mix = c()
col1 = "A"
col2 = "B"
distance = "paths"

#===== Test the djikstra algorithm for one community
tic()
x.col1 <- lapply(v1,function(x) shortest_paths(reseau,x,v1[which(x == v1):length(v1)],"all")$vpath)
toc()

m_weight = mean(E(reseau)$weight)
max_weight = sum(E(reseau)$weight)


dist_par(reseau,col1,col2,"",distance,share_w = 0)
x.col1 = shortest_paths_graph(reseau,v1,v2,v.mix,col1,"paths","single",m_weight,max_weight)


#==== Test the whole program
tic()
Netfrac(reseau,"transfer")
toc()




#==== Estimation of the transfer/Spp distance
estimation_transfer(reseau,"A")
estimation_transfer(reseau,"B")

#==== Artificial transfer network
set.seed(66)
artificial_network <- function(number_nodes, connectivity){
  net_t1 <- erdos.renyi.game(number_nodes, connectivity, "gnp", directed = FALSE, loops = FALSE)
  net_t2 <- erdos.renyi.game(number_nodes, connectivity, "gnp", directed = FALSE, loops = FALSE)

  V(net_t1)$name <- sprintf("a%d", 1:number_nodes)
  V(net_t2)$name <- sprintf("b%d", 1:number_nodes)
  V(net_t1)$tax = "A"
  V(net_t2)$tax = "B"
  V(net_t1)$color = 1
  V(net_t2)$color = 2

  attrs <- rbind(as_data_frame(net_t1, "vertices"), as_data_frame(net_t2, "vertices")) %>% unique()
  el <- rbind(as_data_frame(net_t1), as_data_frame(net_t2))

  net_t <- graph_from_data_frame(el, directed = FALSE, vertices = attrs)
  net_t <- new_graph(net_t)

  net_t <- add.edges(net_t, c(sample(V(net_t1)$name,1),sample(V(net_t2)$name,1)))
  plot(net_t, vertex.label = NA, vertex.size= 10)
  return(net_t)
}

# net_t <- delete.vertices(net_t,c("a9","a1","a3"))
# plot(net_t)
# net_t <- add.vertices(net_t,3,name = c("a9","a1","a3"), tax="A", color=1)
# plot(net_t)
#
# net_t <- add.edges(net_t, c("a9","b5","a3","b7","a1","b8"))
# plot(net_t)





"""
element_next <- rand_adj[[1]][element]
element_next
rand_next <- which(adjacent_vertices(net_t,element_next)[[1]] == V(net_t)[rand])

net_t <- delete.edges(net_t,rand_edge)
plot(net_t, vertex.label = NA, vertex.size = 10)

element_next <- V(net_t)[element_next]
rand_edge <- E(net_t)[from(element_next)]
rand_edge <- rand_edge[-rand_next]


element

net_t <- delete.edges(net_t,rand_edge)
plot(net_t, vertex.label = NA, vertex.size = 10)

net_t <- add.edges(net_t, c(V(net_t)[element_next]$name,sample(V(net_t2)$name,1)))
plot(net_t, vertex.label = NA, vertex.size = 10)
"""


random_transfer <- function(graph, t_A = 1,t_B = 1){
  net_tt <- graph
  V(net_tt)$transfer <- "no"
  for (k in 1:(t_A+t_B)){
    if (k > t_A){
      rand <- sample(V(net_tt)[tax == "B"],1)
    }else{
      rand <- sample(V(net_tt)[tax == "A"],1)
    }
    rand_1 <- rand
    all_rand <- c()
    deg_rand <- 4
    
    while (igraph::degree(net_tt,rand) <= deg_rand){
      rand <- sample(V(net_tt),1)
      rand_1 <- rand
    }
    letter <- V(net_tt)$tax[rand_1]
    
    deg_rand <- 2
    number_nodes <- sample(1:5,1)
    for (i in 1:number_nodes) {
      degree1 <- ego(net_tt, order = 1, nodes = rand)
      print(degree1)
      all_rand <- append(all_rand, V(net_tt)$name[rand])
      rand <- sample(degree1[[1]][-1],1)
      
      a <- 0
      while (igraph::degree(net_tt,rand) <= deg_rand & a < 4){
        rand <- sample(degree1[[1]][-1],1)
        a = a+1
      }
      if (igraph::degree(net_tt,rand) <= deg_rand){
        max(igraph::degree(net_tt,degree1[[1]][-1]))
      }
    }
    V(net_tt)[all_rand]$transfer <- "yes"
    #find the edges between the transfers
    keep_edges <- net_tt %s% induced.subgraph(net_tt,all_rand)
    
    #make a new graph with adjacent nodes
    selegoG <- induced.subgraph(net_tt,unlist(ego(net_tt, order=1, nodes = all_rand)))
    #subtract the edges from new graph to original,isolating the transfers
    net_tt <- difference(net_tt,selegoG)
    #add again the keep_edges
    net_tt <- add.edges(net_tt, get.edgelist(keep_edges))
    
    #make link to other community
    other_comm <- sample(V(net_tt)[tax != letter],1)
    net_tt <- add.edges(net_tt, c(V(net_tt)[rand_1], other_comm))
  }
  #plot(net_tt, vertex.label = NA, vertex.size = 10)
  return(net_tt)
}

# make a list of the names of the nodes of interest
nodes_of_interest <- c("a35","b11")

# select the nodes having these names
selnodes <- V(net_t)[name %in% nodes_of_interest]
# get their network neighborhood
selegoV <- ego(net_t, order=1, nodes = "a22", mode = "all", mindist = 0)

# turn the returned list of igraph.vs objects into a graph
selegoG <- induced_subgraph(net_t,unlist(selegoV))

# plot the subgraph
plot(selegoG,vertex.label=V(selegoG)$name)


all_f1score <- rep(0, 100)
all_recall <- rep(0, 100)
all_precision <- rep(0, 100)
for(i in 1:100){
  transferTest <- random_transfer(net_t,3,3)
  truth <- V(transferTest)$transfer
  pred <- transfer2(transferTest)
  all_f1score[i] <- F1_Score(truth, pred, "yes")
  all_precision[i] <- Precision(truth, pred, "no")
  all_recall[i] <- Recall(truth, pred, "yes")
}

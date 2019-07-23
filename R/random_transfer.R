random_transfer <- function(graph, num_t = 1){
  net_tt <- graph
  V(net_tt)$transfer = "no"
  for (k in 1:num_t){
    rand <- sample(V(net_tt)$name,1)
    all_rand <- c()
    deg_rand <- 2

    while (igraph::degree(net_tt,rand) <= deg_rand){
      rand <- sample(V(graph)$name,1)
    }
    letter <- strsplit(rand,"")[[1]][1]
    previous <- ""


    number_nodes <- sample(1:5,1)
    for (i in 1:number_nodes) {
      all_rand <- append(all_rand,rand)
      rand <- V(net_tt)[rand]

      #make sure there it is not a dead end
      deg_rand <- n-i
      if (deg_rand > 2){
        deg_rand = 2
      }

      #=== find the edges and adjacent vertices of rand vertex
      rand_edge <- E(net_tt)[from(rand)]

      rand_adj <- adjacent_vertices(net_tt,rand)

      #=== randomly select another vertex and edge(s) to keep
      element <- sample(length(rand_edge),1)
      while (igraph::degree(net_tt,rand_adj[[1]][element]) <= deg_rand) {
        element <- sample(length(rand_edge),1)
      }
      element_next <- rand_adj[[1]][element]
      if(number_nodes == 1){
        cat("")
      }else if(i == number_nodes){
        keep_edge <- which(rand_adj[[1]] == V(net_tt)[previous])
        rand_edge <- rand_edge[-keep_edge]
      }else if(i == 1){
        keep_edge <- which(rand_adj[[1]] == V(net_tt)[element_next])
        rand_edge <- rand_edge[-keep_edge]
      }else{
        keep_edge <- which(rand_adj[[1]] == V(net_tt)[element_next] | rand_adj[[1]] == V(net_tt)[previous])
        rand_edge <- rand_edge[-keep_edge]
      }
      net_tt <- delete.edges(net_tt,rand_edge)
      V(net_tt)$transfer[rand] = "yes"
      previous <- rand
      rand <- V(net_tt)$name[element_next]
    }
    all_rand <- V(net_tt)[all_rand]

    other_comm <- sample(V(net_tt)$name[51:100],1)
    other_comm2 <- sample(V(net_tt)$name[1:50],1)
    if (number_nodes > 1){
      rand_1 <- sample(all_rand,2)
    }else{
      rand_1 <- previous
    }
    if(letter == "a"){
      net_tt <- add.edges(net_tt, c(V(net_tt)$name[rand_1[1]], other_comm))
      if (number_nodes > 1){
        net_tt <- add.edges(net_tt, c(V(net_tt)$name[rand_1[2]], other_comm))
      }

    }else{
      net_tt <- add.edges(net_tt, c(V(net_tt)$name[rand_1[1]], other_comm2))
      if (number_nodes > 1){
        net_tt <- add.edges(net_tt, c(V(net_tt)$name[rand_1[2]], other_comm2))
      }
    }
  }
  #plot(net_tt, vertex.label = NA, vertex.size = 10)
  return(net_tt)
}


all_transfer <- rep(0, 100)
for( i in 1:100){
  transferTest <- random_transfer(net_t,4)
  truth <- V(transferTest)$transfer
  pred <- transfer2(transferTest)
  all_transfer[i] <- F1_Score(truth, pred, "yes")
}

random_transfer <- function(graph, t_A = 1,t_B = 1){
  net_tt <- graph
  V(net_tt)$transfer <- "no"
  total = t_A + t_B
  for (k in 1:total){
    if (k <= t_A){
      letter <- "A"
    }else{
      letter <- "B"
    }
    rand <- sample(V(net_tt)[tax == letter & transfer == "no"],1)

    rand_1 <- rand
    all_rand <- c()
    deg_rand <- 1

    while (igraph::degree(net_tt,rand) <= deg_rand){
      rand <- sample(V(net_tt)[tax == letter & transfer == "no"],1)
      rand_1 <- rand
    }

    number_nodes <- sample(1:5,1)

    for (i in 1:number_nodes) {
      degree1 <- ego(net_tt, order = 1, nodes = rand)
      if (length(degree1[[1]]) == 1){
        break
      }
      all_rand <- append(all_rand, V(net_tt)$name[rand])
      rand <- sample(degree1[[1]][-1],1)

      a <- 0
      while (igraph::degree(net_tt,rand) <= deg_rand & a < 4){
        rand <- sample(degree1[[1]][-1],1)
        a = a+1
      }
      if (igraph::degree(net_tt,rand) <= deg_rand){
        rand <- V(net_tt)[names(which.max(igraph::degree(net_tt,degree1[[1]][-1])))]
      }
    }
    all_rand <- unique(all_rand)
    V(net_tt)[all_rand]$transfer <- "yes"
    #find the edges between the transfers
    keep_edges <- net_tt %s% induced.subgraph(net_tt,all_rand)

    #make a new graph with adjacent nodes
    egoG <- induced.subgraph(net_tt,unlist(ego(net_tt, order=1, nodes = all_rand)))

    #subtract the edges from new graph to original,isolating the transfers
    net_tt <- net_tt %m% egoG
    net_tt <- simplify(net_tt)

    #add again the keep_edges
    net_tt <- add.edges(net_tt, get.edgelist(keep_edges))

    #make link to other community
    other_comm <- sample(V(net_tt)[tax != letter],1)
    net_tt <- add.edges(net_tt, c(V(net_tt)[rand_1], other_comm))
    node_deg0 <- V(net_tt)[which(igraph::degree(net_tt,V(net_tt))== 0)]
    for(node in node_deg0){
      letter2 <- V(net_tt)$tax[node]
      if(V(net_tt)[node]$transfer == "no"){
        net_tt <- add_edges(net_tt,c(V(net_tt)[node],sample(V(net_tt)[tax == letter2 & transfer == "no"],1)))
      }else{
        net_tt <- add_edges(net_tt,c(V(net_tt)[node],sample(V(net_tt)[tax != letter2 & transfer == "no"],1)))
      }

    }

  }
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

all_f1score <- list()
all_recall <- list()
f1score <- rep(0, 100)
recall <- rep(0, 100)
percentages <- c()
percentages$A <- c(0,0,0,2,2,2,5,5,10)
percentages$B <- c(2,5,10,2,5,10,5,10,10)

set.seed(4864)
set.seed(666)
for(j in 1:length(percentages$A)){
  num_A = percentages$A[j]
  num_B = percentages$B[j]
  cat(num_A,num_B)
  for(i in 1:500){
    transferTest <- random_transfer(net_t1,num_A,num_B)
    truth <- V(transferTest)$transfer
    pred <- transfer2(transferTest)
    f1score[i] <- F1_Score(truth, pred, "yes")
    recall[i] <- Recall(truth, pred, "yes")
  }
  all_f1score[[j]] <- f1score
  all_recall[[j]] <- recall
}

names(all_f1score) <- c("0 and 10%","0 and 25%","0 and 50%","10 and 10%","10 and 25%","10 and 50%","25 and 25%","25 and 50%","50 and 50%")
boxplot(all_recall, xaxt="n", range = 1, las = 2, col = rgb(1,0.2,0.1,0.7), outline = FALSE)
text(seq_along(all_recall), par("usr")[3], labels = names(all_f1score), srt = 45, adj = 1.2, xpd = TRUE, cex = 1.3)
boxplot(all_f1score, xaxt="n", range = 1, las = 2, col = rgb(0,0.6,0.1,0.7), outline = FALSE)


for(i in 1:100){
  transferTest <- random_transfer(net_t1,2,5)
  truth <- noquote(V(transferTest)$transfer)
  pred <- transfer2(transferTest)
  f1score[i] <- F1_Score(truth, pred, "yes")
  recall[i] <- Recall(truth, pred, "yes")
  print(i)
}
all_f1score[[5]] <- f1score
all_recall[[5]] <- recall


transferTest <- random_transfer(net_t,10,10)
truth <- noquote(V(transferTest)$transfer)
pred <- transfer2(transferTest)
F1_Score(truth, pred, "yes")

set.seed(66)
net_tt <- random_transfer(net_t,10,10)
plot(net_tt)

#======================================
#= simulation for relationship between the connectivity in graphs and distance measure
#======================================

library(igraph)
source("R/main.R")

nvert = 50
results_conn10 = c()
results_conn25 = c()
results_conn50 = c()
results_conn80 = c()
results_Spp = c()
results_Spep = c()
results_Spelp = c()
results_Spinp = c()
results_transfer = c()
# for( i in 1:100){
#   net_conn10 = random.graph.game(nvert,0.1,"gnp")
#   net_conn25 = random.graph.game(nvert,0.25,"gnp")
#   net_conn50 = random.graph.game(nvert,0.50,"gnp")
#   net_conn80 = random.graph.game(nvert,0.80,"gnp")
#   V(net_conn10)$tax = sample(c("A","B","C"),nvert,T)
#   V(net_conn25)$tax = sample(c("A","B","C"),nvert,T)
#   V(net_conn50)$tax = sample(c("A","B","C"),nvert,T)
#   V(net_conn80)$tax = sample(c("A","B","C"),nvert,T)
#   V(net_conn10)$name = c(1:50)
#   V(net_conn25)$name = c(1:50)
#   V(net_conn50)$name = c(1:50)
#   V(net_conn80)$name = c(1:50)
#   E(net_conn10)$weight = rep(1,length(E(net_conn10)))
#   E(net_conn25)$weight = rep(1,length(E(net_conn25)))
#   E(net_conn50)$weight = rep(1,length(E(net_conn50)))
#   E(net_conn80)$weight = rep(1,length(E(net_conn80)))
#
#   r1 = Netfrac(net_conn10,"paths","all",4)
#   r2 = Netfrac(net_conn25,"paths","all",4)
#   r3 = Netfrac(net_conn50,"paths","all",4)
#   r4 = Netfrac(net_conn80,"paths","all",4)
#
#   results_conn10 = append(results_conn10,r1)
#   results_conn25 = append(results_conn10,r2)
#   results_conn50 = append(results_conn10,r3)
#   results_conn80 = append(results_conn10,r4)
#
# }
Some_test <- function(connectivity){
  for( i in 1:100){
    net_conn = random.graph.game(nvert,connectivity,"gnp")
    V(net_conn)$tax = sample(c("A","B","C"),nvert,T)
    V(net_conn)$name = sprintf("A%d",1:50)
    E(net_conn)$weight = rep(1,length(E(net_conn)))
    r1 = Netfrac(net_conn,"paths","single",4)
    results_Spp = append(results_Spp,r1[1])
    results_Spep = append(results_Spep,r1[2])
    results_Spelp = append(results_Spelp,r1[3])
    results_Spinp = append(results_Spinp,r1[4])
    results_transfer = append(results_transfer,r1[5])

  }

return(unlist(c(results_Spp,results_Spep,results_Spelp,results_Spinp,results_transfer)))
}
net_conn10 = net_conn10[net_conn10 != 0]


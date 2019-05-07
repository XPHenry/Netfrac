library(igraph)
library(SDDE)

CAT_env = load_network("HGT/CAT_edges.txt","HGT/CAT_nodes_env.txt","equal")
V(CAT_env)$tax

CAT_env_foo = c("red","green","blue","grey")
names(CAT_env_foo) = c("host","ubiquitous","water","unknown")
CAT_env_foo
CAT_env = set_color(CAT_env,CAT_env_foo)

plot(CAT_env,vertex.label=NA, vertex.size = 10)

TetA_env = load_network("HGT/TetA_edges.txt","HGT/TetA_nodes_env.txt","equal")
V(TetA_env)$tax

V(TetA_env)[7]$tax = "unknown"
V(TetA_env)[43]$tax = "unknown"
V(TetA_env)[44]$tax = "unknown"
V(TetA_env)[47]$tax = "unknown"

TetA_env_foo = c("red","green","blue","grey")
names(TetA_env_foo) = c("host","ubiquitous","water","unknown")
TetA_env_foo
TetA_env = set_color(TetA_env,TetA_env_foo)

plot(TetA_env, vertex.label=NA, vertex.size = 10)


#TetA with biggest cluster, instead of whole
cl <- components(TetA_net)
all_cluster = lapply(seq_along(cl$csize), function(x) V(TetA_net)$name[cl$membership %in% x])
clust = induced.subgraph(TetA_net,all_cluster[3])
TetA_env2 = clust


V(TetA_env2)


TetA_env2_tax = read.table("HGT/TetA_nodes_env2.txt", sep="\t")
TetA_env2_tax
V(TetA_env2)$tax = TetA_env2_tax$V2


TetA_env_foo = c("red","green","blue","grey")
names(TetA_env_foo) = c("host","ubiquitous","water","unknown")
TetA_env_foo
TetA_env2 = set_color(TetA_env2,TetA_env_foo)

plot(TetA_env2, vertex.label=NA, vertex.size = 10)
